open Mods
open Tools
open ExceptionDefn
open Graph

type variable = CONST of float | VAR of ((int -> float) -> (int -> float) -> float -> int -> float)
and action =
		BND of (port * port)
	| FREE of (port * bool) (*FREE(p,b) b=true if FREE is side-effect free*)
	| MOD of (port * int)
	| DEL of int
	| ADD of (int * int) (*(id in mixture, name_id)*)
and port = id * int
and id = FRESH of int | KEPT of int (*binding or modifying a port that has been added or kept from the lhs*)

module ActionSet = Set.Make(struct type t=action let compare = compare end) 

module IdMap = MapExt.Make (struct type t = id let compare = compare end)
module Id2Map = MapExt.Make (struct type t = id*int let compare = compare end)

type rule = {
	k_def : variable ; (*standard kinetic constant*)
	k_alt : variable option ; (*Possible unary kinetic rate*)
	over_sampling : float option ; (*Boosted kinetic rate for Bologna technique*)
	script : action list ;
	balance : (int * int * int) ;	(*#deleted,#preserved,#removed*)
	kappa: string ;
	lhs : Mixture.t ;
	rhs : Mixture.t ;
	refines: int option ; (*mixture id that is refined by lhs*)
	r_id : int ;
	added : IntSet.t;
	side_effect : bool ;
	modif_sites : Int2Set.t IdMap.t ;  
	pre_causal : int Id2Map.t ; (* INTERNAL_TESTED (8) | INTERNAL_MODIF (4) | LINK_TESTED (2) | LINK_MODIF (1) *)
	is_pert : bool ;
	cc_impact : (IntSet.t IntMap.t * IntSet.t IntMap.t * IntSet.t IntMap.t) option 
	}
	(*connect: cc_i(lhs) -> {cc_j(lhs),...} if cc_i and cc_j are connected by rule application*)
	(*disconnect: cc_i(rhs) -> {cc_j(rhs),...} if cc_i and cc_j are disconnected by rule application*)
	(*side_effect: ag_i -> {site_j,...} if one should check at runtime the id of the agent connected to (ag_i,site_j) and build its cc after rule application*)


let _INTERNAL_TESTED = 8
let _INTERNAL_MODIF = 4
let _LINK_TESTED = 2
let _LINK_MODIF = 1


let compute_causal lhs rhs script env = 
	let causal_map = (*adding tests for all sites mentionned in the left hand side --including existential site*) 
		IntMap.fold 
		(fun id ag causal_map ->
			Mixture.fold_interface 
			(fun site_id (int,lnk) causal_map ->
				let c =
					match int with
						| Some i -> _INTERNAL_TESTED
						| None -> 0
				in
				let c = 
					match lnk with
						| Node.WLD -> c
						| _ -> (c lor _LINK_TESTED)
				in
				Id2Map.add (KEPT id,site_id) c causal_map
			)
			ag causal_map
		) 
		(Mixture.agents lhs) Id2Map.empty
	in
	let add_causal p c map =
		let c' = try Id2Map.find p map with Not_found -> 0
		in
		Id2Map.add p (c lor c') map
	in
	List.fold_left
	(fun causal_map action ->
		match action with
			| BND (p1, p2) -> add_causal p2 _LINK_MODIF (add_causal p1 _LINK_MODIF causal_map)  
			| FREE (p1,side_effect_free) -> 
				if side_effect_free then 
					begin
						match p1 with
							| (KEPT id, site_id) ->
								begin 
									match Mixture.follow (id,site_id) lhs with
										| Some (id',site_id') -> let p2 = (KEPT id',site_id') in add_causal p2 _LINK_MODIF (add_causal p1 _LINK_MODIF causal_map)
										| None -> invalid_arg "Dynamics.Compute_causal"
								end 
							| (FRESH id, site_id) -> invalid_arg "Dynamics.Compute_causal"
					end
				else 
					add_causal p1 _LINK_MODIF causal_map
			| MOD (p,i) -> add_causal p _INTERNAL_MODIF causal_map
			| DEL ag_id -> 
				Mixture.fold_interface 
				(fun site_id (int,lnk) causal_map ->
					let causal_map = 
						match int with Some _ -> add_causal (KEPT ag_id,site_id) _INTERNAL_MODIF causal_map | None -> causal_map
					in
					add_causal (KEPT ag_id,site_id) _LINK_MODIF causal_map
				)
				(Mixture.agent_of_id ag_id lhs) causal_map
			| ADD (ag_id,name_id) -> (*Interface might be partial!*)
				let sign = Environment.get_sig name_id env in
				let arity = Signature.arity sign in
				let site_id = ref 0 in
				let p_causal = ref causal_map in
				while !site_id < arity do
					p_causal :=  
						begin
							match Environment.default_state name_id !site_id env with
								| None -> !p_causal
								| Some i -> add_causal (FRESH ag_id,!site_id) _INTERNAL_MODIF !p_causal
						end ;
					p_causal := add_causal (FRESH ag_id,!site_id) _LINK_MODIF !p_causal ;
					site_id := !site_id + 1 ;
				done ;
				!p_causal
	) causal_map script 
	
type perturbation = {precondition: boolean_variable ; effect : modification ; abort : boolean_variable option ; flag : string}
and modification = 
	INTRO of variable * Mixture.t 
	| DELETE of variable * Mixture.t 
	| UPDATE of int * variable 
	| SNAPSHOT of string option
	| STOP of string option
and boolean_variable = BCONST of bool | BVAR of ((int -> float) -> (int -> float) -> float -> int -> bool)

let string_of_pert pert env =
	match pert.effect with
		| INTRO (_,mix) -> Printf.sprintf "INTRO %s" (Mixture.to_kappa false mix env)
		| DELETE (_,mix) -> Printf.sprintf "DELETE %s" (Mixture.to_kappa false mix env)
		| UPDATE (r_id,_) -> Printf.sprintf "UPDATE rule[%d]" r_id
		| SNAPSHOT opt -> (match opt with None -> "SNAPSHOT" | Some s -> "SNAPSHOT("^s^")") 
		| STOP opt -> (match opt with None -> "STOP" | Some s -> "STOP("^s^")")

		
let diff m0 m1 label_opt env =
	let add_map id site_type map =
		let set = try IdMap.find id map with Not_found -> Int2Set.empty
		in
			IdMap.add id (Int2Set.add site_type set) map
	in
	let side_effect = ref false in
	let label = match label_opt with Some (_,pos) -> (string_of_pos pos) | None -> "" in
	let id_preserving ag1 ag2 = (*check whether ag2 can be the residual of ag1 for (same name)*)
		Mixture.name ag1 = Mixture.name ag2
	in
	let prefix, deleted, add_index =
		IntMap.fold
			(fun id ag0 (prefix, deleted, ind) ->
				if deleted = [] then
						try
							let ag1 = IntMap.find id (Mixture.agents m1) in
							if id_preserving ag0 ag1 then (id:: prefix, deleted, ind)
							else (prefix, id:: deleted, if id<ind then id else ind)
						with Not_found -> (prefix, id:: deleted, ind) (*id is bigger than the max id in m1 so id is deleted*)
				else (prefix,id::deleted,ind)
			)
			(Mixture.agents m0) ([],[], IntMap.size (Mixture.agents m0))
	in
	let added =
		IntMap.fold
			(fun id ag1 added ->
				if id < add_index then added else (id:: added)
			)	(Mixture.agents m1) []
	in
	let balance = (List.length deleted, List.length prefix, List.length added)
	and instructions = (*adding deletion instructions*)
		List.fold_left
			(fun inst id ->
				side_effect := true ;
				(DEL id):: inst
			)
			[] deleted
	in
	let instructions = (*adding creation instructions*) 
		List.fold_left
			(fun instructions id ->
				let ag = Mixture.agent_of_id id m1 in
				let name_id = Mixture.name ag in
				ADD(id, name_id)::instructions
			)
			instructions added
	in
	let instructions,modif_sites = (*adding connections of new agents if partner has a lower id*)
		List.fold_left
		(fun (inst,idmap) id ->
			let ag = Mixture.agent_of_id id m1 in
			let name_id = Mixture.name ag in
			let sign = Environment.get_sig name_id env in
			let modif_sites = 
				Signature.fold 
				(fun site_id idmap -> add_map (FRESH id) (site_id,0) (add_map (FRESH id) (site_id,1) idmap)
				) 
				sign idmap 
			in
			let inst = 
				Mixture.fold_interface 
					(fun site_id (int, lnk) inst ->
						let def_int = try Some (Environment.state_of_id (Mixture.name ag) site_id 0 env) with Not_found -> None
						in
						let inst =
							match (def_int, int) with
							| (None, None) -> inst
							| (Some i, None) -> inst (*DCDW: default will be assumed*)
							| (Some i, Some j) -> (MOD((FRESH id, site_id), j))::inst
							| (None, Some k) -> invalid_arg "Dynamics.diff: adding an agent that is not supposed to have an internal state"
						in
						match lnk with
						| Node.WLD -> invalid_arg "Dynamics.diff: adding an agent that is not fully described (wild card lnk)"
						| Node.FREE -> inst
						| Node.TYPE _ -> invalid_arg "Dynamics.diff: adding an agent that is not fully described (link type)"
						| Node.BND ->
								let opt = Mixture.follow (id, site_id) m1 in
								match opt with
								| None -> invalid_arg "Dynamics.diff: adding an agent that is not fully described (semi lnk)"
								| Some (i,x) ->
										let bnd_i =
											if List.mem i added then (FRESH i) else (KEPT i)
										in
										if i < id or (i = id && x < site_id) then 
											BND((bnd_i, x),(FRESH id, site_id)):: inst
										else 
											inst
					)
					ag inst
			in
				(inst,modif_sites)
			)
			(instructions,IdMap.empty) added
	in
	let instructions,modif_sites =
		List.fold_left
			(fun (inst,idmap) id -> (*adding link and internal state modifications for agents conserved by the rule*)
						let ag, ag' = (Mixture.agent_of_id id m0, Mixture.agent_of_id id m1) in
						let interface' = Mixture.interface ag' in
						(*folding on ag's interface: problem when a site is not mentionned at all in ag but is used in ag' --ie modif with no test*)
						let sign = Environment.get_sig (Mixture.name ag) env in
						let interface = Mixture.interface ag in
						let interface = Signature.fold (fun site_id interface -> if IntMap.mem site_id interface then interface else IntMap.add site_id (None,Node.WLD) interface) sign interface in
						IntMap.fold
							(fun site_id (int_state, lnk_state) (inst,idmap) ->
										let int_state', lnk_state' =
											try IntMap.find site_id interface' with
											| Not_found -> (None,Node.WLD) (*site is not mentioned in the right hand side*)
										in
										let inst,idmap =
											match (int_state, int_state') with
											| (Some i, Some j) -> 
												if i = j then (inst,idmap) 
												else 
													let inst = (MOD ((KEPT id, site_id), j))::inst
													and idmap = add_map (KEPT id) (site_id,0) idmap
													in
														(inst,idmap)
											| (Some _, None) -> invalid_arg "Dynamics.diff: agent not instanciated on the right"
											| (None, Some j) ->
													let site = Environment.site_of_id (Mixture.name ag) site_id env in
													let _ =
														warning
															(Printf.sprintf
																	"%s internal state of site '%s' of agent '%s' is modified although it is left unpecified in the left hand side"
																	label site (Environment.name (Mixture.name ag) env)
															)
													in
													let inst = (MOD ((KEPT id, site_id), j))::inst
													and idmap = add_map (KEPT id) (site_id,0) idmap
													in
													(inst,idmap)
											| (None, None) -> (inst,idmap)
										in
										match (lnk_state, lnk_state') with
										| (Node.BND, Node.FREE) | (Node.TYPE _, Node.FREE) -> (*connected -> disconnected*)
												let opt = Mixture.follow (id, site_id) m0 in
												begin
													match opt with
													| Some (id', site_id') -> (*generating a FREE instruction only for the smallest port*)
															let kept = List.exists (fun id -> id=id') prefix 
																(*try let _ = (Mixture.agent_of_id id' m1) in true with Not_found -> false*)
															in
															let idmap = 
																if kept then 
																	add_map (KEPT id) (site_id,1) (add_map (KEPT id') (site_id',1) idmap) (*bug if id' is deleted by the rule should not be added to modif_sites*)
																else
																	add_map (KEPT id) (site_id,1) idmap
															and inst =
																if id'< id or (id'= id && site_id'< site_id) then (FREE (((KEPT id), site_id),true)):: inst
																else inst
															in
															(inst,idmap)
													| None -> (*breaking a semi link so generate a FREE instruction*)
															let inst = (FREE (((KEPT id), site_id),false)):: inst
															and idmap = add_map (KEPT id) (site_id,1) idmap
															in
															side_effect := true ;
															let _ =
																warning
																(Printf.sprintf
																		"%s breaking a semi-link on site '%s' will induce a side effect"
																		label (Environment.site_of_id (Mixture.name ag) site_id env)
																)
															in
															(inst,idmap)
												end
										| (Node.BND, Node.BND) | (Node.TYPE _, Node.BND) -> (*connected -> connected*)
												begin
													let opt, opt' = (Mixture.follow (id, site_id) m0, Mixture.follow (id, site_id) m1) in
													if opt = opt' then (inst,idmap) (*no change to be made*)
													else
														match (opt, opt') with
														| (None, Some (id1', i1')) -> (*sub-case: semi-link -> connected*)
															(*warning*)
															let site = Environment.site_of_id (Mixture.name ag) site_id env in
															let _ =
																		warning
																			(Printf.sprintf
																					"%s link state of site '%s' of agent '%s' is changed although it is a semi-link in the left hand side"
																					label site (Environment.name (Mixture.name ag) env)
																			)
															in
															(*modified sites*)
															let id'' = if List.exists (fun id -> id=id1') prefix then (KEPT id1') else (FRESH id1')
															in
															let idmap' = add_map (KEPT id) (site_id,1) (add_map id'' (i1',1) idmap) in
															(*instruction*)
															if id1' < id or (id1'= id && i1'< site_id) then (*generating an instruction only for one of both sites*)
																begin
																	let inst = BND((KEPT id, site_id), (id'',i1')):: inst
																	in
																		side_effect := true ;
																		(inst,idmap')
																end
															else (inst,idmap')
															
														| (Some (id1, i1), Some (id1', i1')) -> (*sub-case: connected -> connected*)
																(*warning*)
																let site = Environment.site_of_id (Mixture.name ag) site_id env in
																let _ =
																	warning
																		(Printf.sprintf
																				"%s rule induces a link permutation on site '%s' of agent '%s'"
																				label site (Environment.name (Mixture.name ag) env)
																		)
																in
																(*modifed sites*)
																(*it might be that id1 is not preserved by the reaction!*)
																let idmap = if List.exists (fun id -> id=id1) prefix then add_map (KEPT id1) (i1,1) idmap else idmap
																in
																(*now id1' might be created by the reaction*)
																let id1'' = if List.exists (fun id -> id=id1') prefix then (KEPT id1') else (FRESH id1') in
																let idmap' = add_map id1'' (i1',1) idmap in
																(*instruction*)
																if id1'< id or (id1'= id && i1'< site_id) then
																	let inst = BND((KEPT id, site_id), (id1'', i1')):: inst
																	in
																		(inst,idmap')
																else 
																	(inst,idmap')
														| (Some (id1, i1), None) -> 
															(*sub-case: connected -> semi-link*) invalid_arg "Dynamics.diff: rhs has partial link state"
														| (None, None) -> (*sub-case: semi-link -> semi-link*) 
															(inst,idmap) (*nothing to be done*)
												end
										| (Node.FREE, Node.BND) -> (*free -> connected*)
												begin
													let opt' = Mixture.follow (id, site_id) m1 in
													match opt' with
													| None -> (*sub-case: free -> semi-link*) invalid_arg "Dynamics.diff: rhs creates a semi-link"
													| Some (id', i') -> (*sub-case: free -> connected*)
														(*no warning*)
														(*modif sites*)
														let id'' = if List.exists (fun id -> id=id') prefix then KEPT id' else FRESH id' in
														let idmap' = add_map (KEPT id) (site_id,1) (add_map id'' (i',1) idmap) 
														in
														if (id'< id) or (id'= id && i'< site_id) then 
															let inst = BND((KEPT id, site_id), (id'', i')):: inst 
															in
															(inst,idmap')
														else 
															(inst,idmap')
												end
										| (Node.FREE, Node.FREE) | (Node.WLD, Node.WLD) -> (*free -> free or wildcard -> wildcard*) (inst,idmap)
										| (Node.TYPE (sid,nme),Node.TYPE(sid',nme')) -> 
											if sid=sid' && nme=nme' then (inst,idmap)
											else invalid_arg "Dynamics.diff: rhs modifies a link type"
										| (Node.WLD, Node.FREE) ->  (*wildcard -> free*)
												let site = Environment.site_of_id (Mixture.name ag) site_id env in
												let _ =
													warning
														(Printf.sprintf
																"%s application of this rule will induce a null event when applied to an agent '%s' that is free on '%s'"
																label (Environment.name (Mixture.name ag) env) site
														)
												in
												let inst = (FREE ((KEPT id, site_id),false))::inst
												and idmap = add_map (KEPT id) (site_id,1) idmap
												in
												(side_effect := true ;
												(inst,idmap))
										| (Node.WLD, Node.BND) ->  (*wildcard -> connected*)
												let opt' = Mixture.follow (id, site_id) m1 in
												begin
													match opt' with
													| None -> invalid_arg "Dynamics.diff: rhs turns a wildcard into a semi link"
													| Some (id', i') ->
														(*warning*)
														let site = Environment.site_of_id (Mixture.name ag) site_id env in
														let _ =
																	warning
																		(Printf.sprintf
																				"%s site '%s' of agent '%s' is bound in the right hand side although it is unspecified in the left hand side"
																				label site (Environment.name (Mixture.name ag) env)
																		)
														in
														(*modif sites*)
														let id'' = if List.exists (fun id -> id=id') prefix then KEPT id' else FRESH id' in
														let idmap' = add_map (KEPT id) (site_id,1) (add_map id'' (i',1) idmap) in
														(*instruction*)
														if (id'< id) or (id'= id && i'< site_id) then
															let inst = BND((KEPT id, site_id), (id'', i')):: inst
															in
															(side_effect:= true;
															(inst,idmap'))
														else (inst,idmap')
												end
										| (_,_) -> (*connected,free -> wildcard*) invalid_arg "Dynamics.diff: rhs creates a wildcard"
							)
							interface (inst,idmap)
			)
			(instructions,modif_sites) prefix
	in
	(List.rev instructions, balance, added, modif_sites,!side_effect)

let rec superpose todo_list lhs rhs map already_done added env =
	match todo_list with
		| [] -> map
		| (lhs_id,rhs_id)::tl ->
			let lhs_ag = Mixture.agent_of_id lhs_id lhs
			and rhs_ag = Mixture.agent_of_id rhs_id rhs 
			in
			let todo_list,already_done = 
				if not (Mixture.name lhs_ag = Mixture.name rhs_ag) then raise False
				else
					Mixture.fold_interface 
					(fun site_id (int,lnk) (todo,already_done) ->
						let opt = Mixture.site_defined site_id rhs_ag (IntSet.mem rhs_id added) env in
						match opt with
							| None -> (todo,already_done) (*site_id is not in the agent in the rhs*)
							| Some (int',lnk') ->
								let compatible = 
									match (int,int') with
										| (Some i,Some i') -> i=i'
										| (None,_) -> true
										| (_,None) -> true
								in
									if not compatible then raise False
									else
										match (lnk,lnk') with
												| (Node.BND,Node.BND) -> 
													begin
														let opt = Mixture.follow (lhs_id,site_id) lhs 
														and opt' = Mixture.follow (rhs_id,site_id) rhs in
														match opt with
															| None -> (*semi-link*) (todo,already_done)
															| Some (lhs_id',site_id') ->
																match opt' with
																	| None -> (todo,already_done)
																	| Some (rhs_id',site_id'') -> 
																		if site_id'=site_id'' then 
																			if Int2Set.mem (lhs_id',rhs_id') already_done then (todo,already_done)
																			else ((lhs_id',rhs_id')::todo,Int2Set.add (lhs_id',rhs_id') already_done)
																		else raise False
													end
												| (Node.BND,Node.TYPE (site_j,name_id)) ->
													begin
														let opt = Mixture.follow (lhs_id,site_id) lhs in
														match opt with
															| None -> (todo,already_done) (*semi-link < link_type*)
															| Some (lhs_id',site_id') -> 
																let ag' = Mixture.agent_of_id lhs_id' lhs in
																let name = Mixture.name ag' in
																if (name = name_id) && (site_id' = site_j) then (todo,already_done) 
																else raise False
													end
												| (Node.TYPE (site_j,name_id),Node.BND) ->
													begin
														let opt = Mixture.follow (rhs_id,site_id) rhs in
														match opt with
															| None -> (todo,already_done) (*semi-link < link_type*)
															| Some (rhs_id',site_id') -> 
																let ag' = Mixture.agent_of_id rhs_id' rhs in
																let name = Mixture.name ag' in
																if (name = name_id) && (site_id' = site_j) then (todo,already_done) 
																else raise False
													end
												| (Node.TYPE (site_j,name_id),Node.TYPE (site_j',name_id')) ->
													if (name_id = name_id') && (site_j = site_j') then (todo,already_done) 
													else raise False
												| (Node.FREE,Node.FREE) | (Node.WLD,_) | (_,Node.WLD) -> (todo,already_done)
												| _ -> raise False
					) lhs_ag (tl,already_done)
				in
					superpose todo_list lhs rhs (IntMap.add lhs_id rhs_id map) already_done added env

let enable r mix env =
	
	let unify rhs lhs (root,modif_sites) glueings already_done =
		let root_ag = try Mixture.agent_of_id root rhs with Not_found -> invalid_arg (Printf.sprintf "Dynamics.enable: agent %d not found in %s" root (Mixture.to_kappa true rhs env)) in
		let name_id_root = Mixture.name root_ag in
		let candidates = (*agent id in lhs --all cc-- that have the name name_id_root*)
			let cpt = ref 0
			and candidates = ref IntSet.empty
			in
				while !cpt < Mixture.arity lhs do
					candidates := IntSet.union !candidates (Mixture.ids_of_name (name_id_root,!cpt) lhs) ;
					cpt := !cpt+1
				done ;
				!candidates
		in
			IntSet.fold
			(fun lhs_ag_id (glueings,already_done) ->
				if Int2Set.exists (*checking that lhs contains --ie tests-- a site that is modified by rhs*) 
				(fun (site_id,t) -> 
					let ag = Mixture.agent_of_id lhs_ag_id lhs in 
					let opt = Mixture.site_defined site_id ag false env in 
					match opt with
						| Some (int,lnk) -> 
							begin
								if t=0 (*int-modified*) then match int with Some _ -> true | None -> false
								else
									match lnk with Node.WLD -> false | _ -> true
							end 
						| None -> false
				) modif_sites
				then 
					let opt = try Some (superpose [(lhs_ag_id,root)] lhs rhs IntMap.empty Int2Set.empty r.added env) with False -> None 
					in
						match opt with
							| Some map -> (*map: id_mix -> id_rule*)
								begin
									let opt = IntMap.root map in
									match opt with
										| None -> invalid_arg "Dynamics.enable: empty map"
										| Some (i,j) -> 
											if Int2Set.mem (i,j) already_done then (glueings,already_done) 
											else
												let already_done = IntMap.fold (fun i j set -> Int2Set.add (i,j) set) map already_done in
												(map::glueings,already_done) 
								end
							| None -> (glueings,already_done)
				else (glueings,already_done)
			) candidates (glueings,already_done)
	in
	(*end sub function unify*)
	begin
		let idmap = r.modif_sites in
		let glueings,_ = 
			IdMap.fold
			(fun id set (glueings,already_done) -> 
				match id with
					| FRESH i | KEPT i -> unify r.rhs mix (i,set) glueings already_done
			) idmap ([],Int2Set.empty)
		in
		glueings
	end

let to_kappa r = r.kappa

let dump r env =
	let name = try Environment.rule_of_num r.r_id env with Not_found -> r.kappa in
	Debug.tag (Printf.sprintf "****Rule '%s' [%s]****" name r.kappa);
	IntMap.iter 
	(fun id ag -> 
		Mixture.fold_interface 
		(fun site_id _ _ -> 
			let c = Id2Map.find (KEPT id,site_id) r.pre_causal in
			Printf.printf "#%d.%d=%d\n" id site_id c
		) ag ()
	) (Mixture.agents r.lhs) ;
	let dump_script script =
		let id_of_port (id, s) =
			match id with
			| KEPT i -> (string_of_int i)^"."^(string_of_int s)
			| FRESH i -> (string_of_int ((fun (deleted, kept, _) -> deleted + kept + i) r.balance))^"."^(string_of_int s)
		in
		List.iter
			(fun action ->
						match action with
						| BND (p, p') -> Printf.printf "BND (#%s,#%s)\n" (id_of_port p) (id_of_port p')
						| FREE (p,b) -> if b then Printf.printf "FREE #%s\n" (id_of_port p) else Printf.printf "FREE* #%s\n" (id_of_port p)
						| MOD (p, i) -> Printf.printf "SET #%s to state %d\n" (id_of_port p) i
						| DEL i -> Printf.printf "DEL #%d\n" i
						| ADD (i, name) ->
								let sign = Environment.get_sig name env in
								Printf.printf "ADD %s%s with identifier #%d\n" (Environment.name name env) (Signature.to_string sign) ((fun (deleted, kept, _) -> deleted + kept + i) r.balance)
			)
			script
	in
	Printf.printf "Apply %s\n" (to_kappa r) ;
	dump_script r.script ;
	Printf.printf "if pattern %d is matched \n" (Mixture.get_id r.lhs) ;
	Printf.printf "Modif sites: %s" 
	(string_of_map 
		(fun id -> match id with FRESH i | KEPT i -> string_of_int i) 
		(string_of_set (fun (x,y) -> "("^(string_of_int x)^","^(string_of_int y)^")") Int2Set.fold)
		IdMap.fold
		r.modif_sites
	) ;
	print_newline() ;
	match r.cc_impact with
		| None -> Printf.printf "No CC impact\n"
		| Some (con,dis,se) ->
			IntMap.iter (fun cc_i cc_set -> Printf.printf "CC[%d] and CCs %s in the left hand side will merge\n" cc_i (Tools.string_of_set string_of_int IntSet.fold cc_set)) con ;
			IntMap.iter (fun cc_i cc_set -> Printf.printf "CC[%d] and CCs %s in the rhs are freshly disconnected  \n" cc_i (Tools.string_of_set string_of_int IntSet.fold cc_set)) dis ;
			IntMap.iter (fun id site_id_set -> Printf.printf "agent #%d might have side effect disconnection on sites %s\n" id (Tools.string_of_set string_of_int IntSet.fold site_id_set)) se 
			
			
