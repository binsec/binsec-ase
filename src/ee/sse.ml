(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Sse_types
open Sse_options

exception Halt

(* Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
    Kernel_functions.get_img ()
    |> Loader.Img.entry
    |> Virtual_address.create

module Etbl =
  Hashtbl.Make(
  struct
    type t = Dba.Expr.t ;;
    let hash = Hashtbl.hash ;;
    let equal = Dba.Expr.is_equal ;;
  end
  )

module type SSE_RUNNER = sig val start: unit -> unit end

module Env_make(W: WORKLIST): SSE_RUNNER =
struct
  module Stats = struct
    type t = {
      paths : int;
      after_max_reached : int;
      nb_snd_query : int;
      analysed_instructions : int;
      max_depth : int;
      max_depth_bcht : int;
      cut_early : int;
      asserts_unknown : int;
      asserts_failed : int;
      injection_locations : int;
      min_inj_per_path : int;
      max_inj_per_path : int;
      sum_inj_per_path : int;
      nb_inj_per_path : int;
      normal_ok : int;
      normal_ko : int;
      faulted_ok : int;
      faulted_ko : int;

      reached_models : int;
      assert_cut : int;
      
    }

    let empty = { 
      paths = 1; 
      after_max_reached = 0; 
      nb_snd_query = 0; 
      analysed_instructions = 1; 
      max_depth = 0; 
      max_depth_bcht = 0; 
      cut_early = 0; 
      asserts_unknown = 0; 
      asserts_failed = 0; 
      injection_locations = 0; 
      min_inj_per_path = 0;
      max_inj_per_path = 0;
      sum_inj_per_path = 0;
      nb_inj_per_path = 0;
      normal_ok = 0;
      normal_ko = 0;
      faulted_ok = 0;
      faulted_ko = 0;
      reached_models = 0; 
      assert_cut = 0;
    }
    let add_analysed_instructions s = { s with analysed_instructions = s.analysed_instructions + 1 }
    let add_path s = { s with paths = s.paths + 1 }
    let add_after_max_reached s = { s with after_max_reached = s.after_max_reached + 1 }
    let add_nb_snd_query s = { s with nb_snd_query = s.nb_snd_query + 1 }
    let update_max_depth s d = { s with max_depth = if d > s.max_depth then d else s.max_depth }
    let update_max_depth_bcht s d = { s with max_depth_bcht = if d > s.max_depth_bcht then d else s.max_depth_bcht }
    let add_cut_early s = { s with cut_early = s.cut_early + 1 }
    let add_assert_unknown s = { s with asserts_unknown = s.asserts_unknown + 1 }
    let add_assert_failed  s = { s with asserts_failed = s.asserts_failed + 1 }
    let add_injection_location  s = { s with injection_locations = s.injection_locations + 1 }
    let update_inj_per_path  s i = { s with 
      min_inj_per_path = min s.min_inj_per_path i;
      max_inj_per_path = max s.max_inj_per_path i;
      sum_inj_per_path = s.sum_inj_per_path + i;
      nb_inj_per_path = s.nb_inj_per_path + 1
     }
    let add_normal_ok  s = { s with normal_ok = s.normal_ok + 1 }
    let add_normal_ko  s = { s with normal_ko = s.normal_ko + 1 }
    let add_faulted_ok  s = { s with faulted_ok = s.faulted_ok + 1 }
    let add_faulted_ko  s = { s with faulted_ko = s.faulted_ko + 1 }
    let add_reached_models  s = { s with reached_models = s.reached_models + 1 }
    let add_assert_cut  s = { s with assert_cut = s.assert_cut + 1 }

    let pp ppf s =
      Format.fprintf ppf
        "@[<v 0>\
         @[<h>Paths %d@]@,\
         @[<h>Paths continuing after max reached %d@]@,\
         @[<h>Secondary queries (EDS only) %d@]@,\
         @[<h>Analysed instructions %d@]@,\
         @[<h>Max depth instruction %d@]@,\
         @[<h>Max depth branchments %d@]@,\
         @[<h>Cut early paths %d@]@,\
         @]"
         (* @[<h>Branch stats (OD only): normal ok | normal ko | faulted ok | faulted ko @]@,\
         @[%d | %d |%d |%d@]@,\ *)
        s.paths
        s.after_max_reached
        s.nb_snd_query
        s.analysed_instructions
        s.max_depth
        s.max_depth_bcht
        s.cut_early
        (* s.normal_ok 
        s.normal_ko 
        s.faulted_ok 
        s.faulted_ko  *)
    ;;

    let pp_fi ppf s =
      match Sse_options.FaultModel.get () with
      | "None" -> (
        Format.fprintf ppf
          "@[<v 0>\
           @[<h>NO FAULT INJECTION@]\
           @]"
      )
      | fm -> (
        Format.fprintf ppf
          "@[<v 0>\
           @[<h>Fault model %s@]@,\
           @[<h>Injection method %s@]@,\
           @[<h>Maximum number of faults %d@]@,\
           @[<h>Where number of faults checked: %s@]@,\
           @[<h>Where non_det constrains checked: %s@]@,\
           @[<h>At branch optim: %b@]@,\
           @[<h>Injection locations %d@]@,\
           @[<h>Min Injection locations per path %d@]@,\
           @[<h>Max Injection locations per path %d@]@,\
           @[<h>Avg Injection locations per path %d@]@,\
           @[<h>Models found %d @]@,\
           @[<h>Assert cut %d @]\
           @]"
          fm
          (Sse_options.InjectionMethod.get ())
          (Sse_options.MaxFaults.get ())
          (Sse_options.WhereCheckNbFaults.get ())
          (Sse_options.WhereCheckNonDetConstrain.get ())
          (Sse_options.AtBranchOptim.get ())
          s.injection_locations
          s.min_inj_per_path
          s.max_inj_per_path
          (if s.nb_inj_per_path != 0 then s.sum_inj_per_path / s.nb_inj_per_path else 0)
          s.reached_models
          s.assert_cut
      )
    ;;

    module R = struct
      let value = ref empty
      let add_analysed_instructions () = value := add_analysed_instructions !value
      let add_path  () = value := add_path !value
      let add_after_max_reached  () = value := add_after_max_reached !value
      let add_nb_snd_query  () = value := add_nb_snd_query !value
      let update_max_depth d = value := update_max_depth !value d
      let update_max_depth_bcht d = value := update_max_depth_bcht !value d
      let add_cut_early  () = value := add_cut_early !value
      let _add_assert_unknown () = value := add_assert_unknown !value
      let add_assert_failed  () = value := add_assert_failed !value
      let add_injection_location  () = value := add_injection_location !value
      let update_inj_per_path i = value := update_inj_per_path !value i
      let add_normal_ok  () = value := add_normal_ok !value
      let add_normal_ko  () = value := add_normal_ko !value
      let add_faulted_ok  () = value := add_faulted_ok !value
      let add_faulted_ko  () = value := add_faulted_ko !value
      let add_reached_models  () = value := add_reached_models !value
      let add_assert_cut  () = value := add_assert_cut !value

      let pp ppf () = pp ppf !value

      let pp_fi ppf () = pp_fi ppf !value
    end
    include R
  end

  module Env = struct
    type t = {
      entrypoint       : Virtual_address.t ;
      cfg              : C.t;
      mutable worklist : W.t;
      cut              : Dba.Expr.t list Virtual_address.Htbl.t;
      choice           : Directive.Choice.t Virtual_address.Htbl.t;
      assume           : Dba.Expr.t list Virtual_address.Htbl.t;
      reach            :
        (Directive.Count.t * Dba.Expr.t) Queue.t Virtual_address.Htbl.t;
      enumerate        : (int * Dba.Expr.t) Queue.t Virtual_address.Htbl.t;
      enumerations     : Bitvector.t list Etbl.t Virtual_address.Htbl.t;
    }

    let create entrypoint =
      let img = Kernel_functions.get_img () in
      let size = Array.fold_left (fun sum s ->
          if Loader.Section.has_flag Loader_types.Exec s then
            let { Loader_types.virt; _ } = Loader.Section.size s in
            sum + virt
          else sum)
          0 (Loader.Img.sections img) in
      {
        entrypoint;
        cfg          = C.create size;
        worklist     = W.empty;
        cut          = Virtual_address.Htbl.create 7;
        choice       = Virtual_address.Htbl.create 7;
        assume       = Virtual_address.Htbl.create 7;
        reach        = Virtual_address.Htbl.create 7;
        enumerate    = Virtual_address.Htbl.create 7;
        enumerations = Virtual_address.Htbl.create 7;
      }

    let llookup h v =
      try Virtual_address.Htbl.find h v
      with Not_found -> []
    let qlookup h v =
      try
        Virtual_address.Htbl.find h v
      with Not_found ->
        let q = Queue.create () in
        Virtual_address.Htbl.add h v q;
        q

    let add_cut e addr guard =
      Virtual_address.Htbl.replace e.cut addr (guard :: llookup e.cut addr)
    let add_choice e addr choice =
      Virtual_address.Htbl.replace e.choice addr choice
    let add_assume e addr pred =
      Virtual_address.Htbl.replace e.assume addr (pred :: llookup e.assume addr)
    let add_reach e addr count guard =
      Queue.add (count, guard) (qlookup e.reach addr)
    let add_enumerate e addr count expr =
      Queue.add (count, expr) (qlookup e.enumerate addr)

    (* Initialize goal table from cli specification *)
    let add_directive e a =
      Logger.debug ~level:2
        "Add action %a" Directive.pp a;
      let v = Directive.addr a in
      match Directive.directive a with
      | Cut g -> add_cut e v g
      | Choice c -> add_choice e v c
      | Assume p -> add_assume e v p
      | Reach (n, g) -> add_reach e v n g
      | Enumerate (n, x) -> add_enumerate e v n x

    let update_from_cli e =
      List.iter (add_directive e) (Directives.get ());
      Virtual_address.Set.iter
        (fun v -> add_reach e v Directive.Count.once Dba.Expr.one)
        (Sse_utils.get_goal_addresses ());
      Virtual_address.Set.iter
        (fun v -> add_cut e v Dba.Expr.one)
        (Sse_utils.get_avoid_addresses ())

    let choose e =
      let rec pick_one w =
        match W.pop w with
        | path_state, worklist when Path_state.may_lead_to_goal path_state ->
          Logger.debug "Selecting path #%d (among %d)"
            (Path_state.id path_state) (W.length w);
          e.worklist <- worklist;
          path_state
        | path_state, worklist ->
          Logger.debug "Discarding path #%d (among %d)"
            (Path_state.id path_state) (W.length w);
          Stats.add_cut_early ();
          Stats.update_max_depth (Path_state.get_depth path_state);
          pick_one worklist
        | exception Not_found ->
          Logger.info "Empty path worklist: halting ...";
          raise Halt
      in pick_one e.worklist

    let add_path e path_state =
      e.worklist <- W.push path_state e.worklist

    let decode e addr =
      match C.mem_vertex_a e.cfg addr with
      | None ->
        Logger.debug ~level:2 "Decoding %@ %a" Virtual_address.pp addr;
        let i = fst (Disasm_core.decode addr) in
        C.add_inst e.cfg addr i;
        i
      | Some v -> Utils.unsafe_get_opt (C.V.inst v)

    let pick_path e = choose e

    let pick_alternative at ?consequent ?alternative e =
      let do_pick ~first ~second =
        add_path e first;
        add_path e second;
        pick_path e in
      match consequent, alternative with
      | None, None -> pick_path e
      | Some consequent, None -> consequent
      | None, Some alternative -> alternative
      | Some consequent, Some alternative ->
        match Virtual_address.Htbl.find e.choice at with
        | exception Not_found ->
          let first, second =
            if Sse_options.Randomize.get () && Random.bool ()
            then alternative, consequent
            else consequent, alternative in
          do_pick ~first ~second
        | c ->
          let first, second =
            if Directive.Choice.is_alternative c then
              alternative, consequent
            else consequent, alternative in
          Directive.Choice.do_alternate c;
          do_pick ~first ~second



    let add_if_good e path_state =
      if Path_state.may_lead_to_goal path_state then
        add_path e path_state
      else (
        Stats.add_cut_early ();
        Stats.update_max_depth (Path_state.get_depth path_state);
        Stats.update_inj_per_path (Path_state.get_injection_points_in_path path_state);
        Logger.info "Goal unreachable from@ %a" Path_state.pp_loc path_state
      )
    
    let main_predicate = 
      match Sse_options.InjectionMethod.get () with
      | "on-demand" -> Senv.Normal
      | "always" | "test-inversion" -> Senv.Faulted
      | _ -> Senv.Normal
    
    let secondary_predicate = 
      match Sse_options.InjectionMethod.get () with
      | "on-demand" -> Senv.Faulted
      | "always" | "test-inversion" -> Senv.Faulted
      | _ -> Senv.Normal

  end

  let printed_stats = ref false;;

  let print_stats () = 
    if !printed_stats then () else begin
      printed_stats := true;
      if Sse_options.SingleSolution.get () then
        Logger.info
          "@[<v 0>\
          @[<v 2>Partial exploration@,%a@]@,\
          @]"
          Stats.pp ()
      else 
        Logger.info
        "@[<v 0>\
          @[<v 2>Exploration@,%a@]@,\
          @]"
        Stats.pp ();
      Logger.info
        "@[<v 0>\
        @[<v 2>SMT queries@,%a@]@,\
        @]"
        Senv.Query_stats.pp ();
      Logger.info 
        "@[<v 0>\
        @[<v 2>Fault injection@,%a@]@,\
        @]"
        Stats.pp_fi ()
      end
    ;;

  let halt e =
    print_stats ();
    if Sse_options.Dot_filename_out.is_set () then
      let filename = Sse_options.Dot_filename_out.get () in
      Logger.info "Outputting CFG in %s" filename;
      let oc = open_out_bin filename in
      let cfg = e.Env.cfg in
      begin
        match C.mem_vertex_a cfg e.Env.entrypoint with
        | None -> ()
        | Some entry -> C.output_graph oc e.Env.cfg [] ~entry
      end;
      close_out oc

  module Eval = struct

    let goto e ps addr =
      let i = Env.decode e addr in
      C.add_edge_a e.Env.cfg (Path_state.virtual_address ps) addr;
      Path_state.set_instruction i ps

    let static_jump ~jump_target e le =
      (* Senv.print_contraints (Path_state.symbolic_state le); *)
      match jump_target with
      | Dba.JInner idx ->
        Some (Path_state.set_block_index idx le)
      | Dba.JOuter addr ->
        if not (Path_state.may_lead_to_goal le) then (
          Stats.add_cut_early ();
          Some (Env.pick_path e)
        ) else (
          let vaddr = Dba_types.Caddress.to_virtual_address addr in
          Logger.debug ~level:5 "Jumping to new address %a"
            Virtual_address.pp vaddr;
          match Path_state.counter vaddr le with
          | Some c ->
            begin match Address_counter.check_and_decr c with
              | Some c ->
                Some (goto e (Path_state.set_counter vaddr c le) vaddr)
              | None ->
                Logger.debug
                  "Cutting path at address %a : we reached the limit ..."
                  Virtual_address.pp vaddr;
                None
                (* Could not decrement counter: should stop *)
            end
          | None -> Some (goto e le vaddr)
        )

    (* lvalue <- e *)
    let assignment ~lvalue ~rvalue idx ps env =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
      *)

      (* This patch is used for Restricts, to fault the entire variable, and not the small changed part (which bugged), for convinience *)
      (* TODO: take the micro-architecture into account on how faults on restricts actually happen *)
      let lvalue, rvalue = match lvalue with
        | Dba.LValue.Restrict (var, { lo; hi }) ->
          Dba.LValue.v var, Dba_utils.Expr.complement rvalue ~hi ~lo var
        | _ ->
          lvalue, rvalue
      in

      let assignment_lvalue name rv is_faulted ps = 
        let state = Senv.assign name rv ~first_fault_happened:(Path_state.fault_already_happened ps) is_faulted (Path_state.dual_symbolic_state ps) in
        Path_state.set_dual_symbolic_state state ps
      in

      let assignment_store dir addr rv is_faulted ps =
        let state = Senv.write ~addr rv dir ~first_fault_happened:(Path_state.fault_already_happened ps) is_faulted (Path_state.dual_symbolic_state ps) in
        Path_state.set_dual_symbolic_state state ps
      in

      let arbitrary_data_behavior rv ps fm =
        let fault b non_det ps =
          Stats.add_injection_location ();
          let ps = Path_state.add_injection_points_in_path ps in
          Logger.debug ~level:3 "Fault assignment ArbitraryData";
          let state = Path_state.dual_symbolic_state ps in
          let size_expression = Dba.LValue.size_of lvalue in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          let new_rvalue = match fm with
            (* default  x := ite b ? non_det : expr *)
            (* non_det != rvalue *)
            | "ArbitraryData" | "ArbitraryDataIte" -> 
              Dba.Expr.ite b non_det rvalue
            (* x := expr + b * non_det *)
            (* non_det != 0 *)
            | "ArbitraryDataMul" -> 
              Dba.Expr.add rvalue
              (Dba.Expr.mul (Dba.Expr.uext size_expression b)
                non_det)
            (* x := expr + (-b) & non_det *)
            (* non_det != 0 *)
            | "ArbitraryDataAnd" -> 
              Dba.Expr.add rvalue
              (Dba.Expr.binary Dba.Binary_op.And (Dba.Expr.uminus (Dba.Expr.uext size_expression b))
                non_det)
            | "ArbitraryDataXor" -> Dba.Expr.binary Dba.Binary_op.Xor rvalue (Dba.Expr.binary Dba.Binary_op.And (Dba.Expr.uminus (Dba.Expr.uext size_expression b)) non_det)
            | _ ->  assert false
          in
          (new_rvalue, ps)
        in
        let check_constrain ps = 
          let state = Path_state.dual_symbolic_state ps in
          (* Declare symbolic value non_det *)
          let size_expression = Dba.LValue.size_of lvalue in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let non_det_name = String.concat "" ["non_det_"; addr] in
          let non_det = Dba.Expr.var non_det_name size_expression in
          let state = Senv.fresh non_det_name size_expression Env.secondary_predicate state in
          (* Declare symbolic value b *)
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          (* Compute constrain according to fault model *)
          let new_constrain = match fm with
            (* default  x := ite b ? non_det : expr *)
            (* (non_det != rvalue) || !b *)
            | "ArbitraryData" | "ArbitraryDataIte" -> 
              Dba.Expr.logor (Dba.Expr.diff rvalue non_det) (Dba.Expr.lognot b)
            (* x := expr + b * non_det *)
            (* (non_det != 0) || !b *)
            | "ArbitraryDataMul" | "ArbitraryDataAnd" | "ArbitraryDataXor" -> 
              Dba.Expr.logor (Dba.Expr.diff non_det (Dba.Expr.zeros size_expression)) (Dba.Expr.lognot b)
            | _ ->  assert false
          in
          (* non_det constrains handling *)
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains new_constrain) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
          | "loc" -> (
            match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
            | None -> let state = Senv.assign b_name Dba.Expr.zero Env.secondary_predicate (Path_state.dual_symbolic_state ps) in 
              (rv, Path_state.set_dual_symbolic_state state ps)
            | Some s -> (
              let ps = Path_state.set_dual_symbolic_state s ps in
              fault b non_det ps
            )
          )
          | _ -> fault b non_det ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
          | "loc" -> (
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let max = Dba.Expr.var "MAX_FAULTS" 32 in
            let state = Path_state.dual_symbolic_state ps in
            match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
            | None -> (
              Stats.add_after_max_reached ();
              let ps = Path_state.reaching_max_faults ps in
              Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
              (rv, ps)
            )
            | Some _ -> check_constrain ps
          ) 
          | _ -> check_constrain ps
      in

      (* let arbitrary_data_behavior2 ps =
        let fault ps = 
          Logger.debug ~level:3 "Fault assignment ArbitraryDataBranchless2";
          Stats.add_injection_location ();
          let state = Path_state.symbolic_state ps in
          (* Declare symbolic value non_det *)
          let size_expression = Dba.LValue.size_of lvalue in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let non_det_name = String.concat "" ["non_det_"; addr] in
          let non_det = Dba.Expr.var non_det_name size_expression in
          let state = Senv.fresh non_det_name size_expression state in
          (* b := non_det != rvalue *)
          let b_name = String.concat "" ["b_"; addr] in
          let b = Dba.Expr.var b_name 1 in
          let state = Senv.assign b_name (Dba.Expr.diff rvalue non_det) state in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count state in
          (* x := non_det *)
          let ps = Path_state.set_symbolic_state state ps in
          (non_det, ps)
        in
        match Sse_options.WhereCheckNbFaults.get () with
          | "loc" -> (
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let max = Dba.Expr.var "MAX_FAULTS" 32 in
            let state = Path_state.symbolic_state ps in
            match Senv.assume (Dba.Expr.ult count max) state with
            | None -> (
              Stats.add_after_max_reached ();
              let ps = Path_state.reaching_max_faults ps in
              Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
              (rvalue, ps)
            )
            | Some _ -> fault ps
          ) 
          | _ -> fault ps
      in *)

      let arbitrary_data_branching_behavior assign_func rv ps =
        let fault b non_det ps =
          Logger.debug ~level:3 "Fault assignment ArbitraryDataBranching";
          Stats.add_injection_location ();
          let ps = Path_state.add_injection_points_in_path ps in
          let state = Path_state.dual_symbolic_state ps in
          let local_addr = Path_state.virtual_address ps in
          let consequent ps symbolic_state =
            Logger.debug ~level:3 "Branching into fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            (* count += 1 *)
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let state = Senv.assign "NUMBER_FAULTS" (Dba.Expr.add count (Dba.Expr.constant (Bitvector.of_int ~size:32 1))) Env.secondary_predicate (Path_state.dual_symbolic_state ps) in
            let ps = Path_state.set_dual_symbolic_state state ps in
            (* Assign *)
            assign_func non_det Env.secondary_predicate ps
          in
          let alternative ps symbolic_state =
            Logger.debug ~level:3 "Branching into no-fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            assign_func rv Env.secondary_predicate ps 
          in
          match Senv.split_on b ~n:2 Env.secondary_predicate state with
            | [ bv, symbolic_state ] when Bitvector.is_zero bv -> alternative ps symbolic_state
            | [ bv, symbolic_state; _, symbolic_state' ] ->
              Stats.add_path ();
              let symbolic_state, symbolic_state' =
                if Bitvector.is_one bv then
                  symbolic_state, symbolic_state'
                else
                  symbolic_state', symbolic_state in
              Env.pick_alternative local_addr ~consequent:(consequent ps symbolic_state) ~alternative:(alternative ps symbolic_state') env
            | _ -> Env.pick_path env
        in
        let check_constrains ps =
          let state = Path_state.dual_symbolic_state ps in
          (* Add check (expr != non_det) || !b to FAULT_CONSTRAIN *)
          let size_expression = Dba.LValue.size_of lvalue in
          (* Declare symbolic value b *)
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          (* Declare symbolic value non_det *)
          let non_det_name = String.concat "" ["non_det_"; addr] in
          let non_det = Dba.Expr.var non_det_name size_expression in
          let state = Senv.fresh non_det_name size_expression Env.secondary_predicate state in
          (* Update constrain *)
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains (Dba.Expr.logor (Dba.Expr.diff rvalue non_det) (Dba.Expr.lognot b))) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
            | "loc" -> (
              match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
                | None -> let state = Senv.assign b_name Dba.Expr.zero Env.secondary_predicate (Path_state.dual_symbolic_state ps) in 
                  assign_func rv Env.secondary_predicate (Path_state.set_dual_symbolic_state state ps)
                | Some s -> let ps = Path_state.set_dual_symbolic_state s ps in fault b non_det ps
            )
            | _ -> fault b non_det ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
          | "loc" -> (
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let max = Dba.Expr.var "MAX_FAULTS" 32 in
            let state = Path_state.dual_symbolic_state ps in
            match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
            | None -> (
              Stats.add_after_max_reached ();
              let ps = Path_state.reaching_max_faults ps in
              Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
              assign_func rv Env.secondary_predicate ps
            )
            | Some _ -> check_constrains ps
          ) 
          | _ -> check_constrains ps
      in
     

      let reset_branchless fm ps =
        let fault b ps =
          Logger.debug ~level:3 "Fault assignment ResetBranchless";
          Stats.add_injection_location ();
          let state = Path_state.dual_symbolic_state ps in
          let size_expression = Dba.LValue.size_of lvalue in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count Env.secondary_predicate state in
          let new_rvalue = match fm with
            | "ResetBranchless" | "ResetBranchlessIte" -> (
              (* x := ite b ? 0x00 : rvalue *)
              Dba.Expr.ite b (Dba.Expr.zeros size_expression) rvalue
            )
            | "ResetBranchlessSub" -> 
              (* x := expr - (b*expr) *)
              Dba.Expr.sub rvalue (Dba.Expr.mul (Dba.Expr.uext 32 b) rvalue)
            | "ResetBranchlessAnd" -> (
              (* x := expr - (-b)&expr *)
              Dba.Expr.sub rvalue (Dba.Expr.binary Dba.Binary_op.And (Dba.Expr.uminus (Dba.Expr.uext size_expression b))
              rvalue)
            )
            | "ResetBranchlessXor" -> (
              (* x := expr xor [ (-b) & expr ]*)
              Dba.Expr.binary Dba.Binary_op.Xor rvalue (Dba.Expr.binary Dba.Binary_op.And (Dba.Expr.uminus (Dba.Expr.uext size_expression b))
              rvalue)
            )
            | _ -> assert false
          in
          let ps = Path_state.set_dual_symbolic_state state ps in
          (new_rvalue, ps)
        in
        let check_constrains ps =
          (* Check constrain separated from check nulber of faults because here we want to keep the returned state *)
          let state = Path_state.dual_symbolic_state ps in
          (* Add check (expr != 0x00) || !b to FAULT_CONSTRAIN *)
          let size_expression = Dba.LValue.size_of lvalue in
          let zero_exp = Dba.Expr.zeros size_expression in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains (Dba.Expr.logor (Dba.Expr.diff rvalue zero_exp) (Dba.Expr.lognot b))) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
          | "loc" -> (
            match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
              | None -> (rvalue, ps)
              | Some s -> let ps = Path_state.set_dual_symbolic_state s ps in fault b ps
          )
          | _ -> fault b ps
        in
        let state = Path_state.dual_symbolic_state ps in
        let ps = Path_state.set_dual_symbolic_state state ps in
        match Sse_options.WhereCheckNbFaults.get () with
          | "loc" -> (
            (* Check NUMBER_FAULTS <= MAX_FAULTS *)
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let max = Dba.Expr.var "MAX_FAULTS" 32 in
            match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
            | None -> (
              Stats.add_after_max_reached ();
              Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
              let ps = Path_state.reaching_max_faults ps in (rvalue, ps)
            )
            | Some _ -> check_constrains ps
          )
          | _ -> check_constrains ps
      in


      (* let reset_branchless2 ps =
        let fault ps = 
          Logger.debug ~level:3 "Fault assignment ArbitraryDataBranchless2";
          Stats.add_injection_location ();
          let state = Path_state.symbolic_state ps in
          (* Declare symbolic value non_det *)
          let size_expression = Dba.LValue.size_of lvalue in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let non_det_name = String.concat "" ["non_det_"; addr] in
          let non_det = Dba.Expr.var non_det_name size_expression in
          let state = Senv.fresh non_det_name size_expression state in
          (* b := non_det != rvalue *)
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let b = Dba.Expr.var b_name 1 in
          let state = Senv.assign b_name (Dba.Expr.diff rvalue non_det) state in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count state in
          let ps = Path_state.set_symbolic_state state ps in
          (* x := non_det *)
          (non_det, ps)
        in
        match Sse_options.WhereCheckNbFaults.get () with
        | "loc" -> (
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let state = Path_state.symbolic_state ps in
          match Senv.assume (Dba.Expr.ult count max) state with
          | None -> (
            Stats.add_after_max_reached ();
            Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
            let ps = Path_state.reaching_max_faults ps in (rvalue, ps)
          )
          | Some _ -> fault ps
        )
        | _ -> fault ps
      in *)

      let reset_branching assign_func ps =
        let fault b ps =
          Logger.debug ~level:3 "Fault assignment ResetBranching";
          Stats.add_injection_location ();
          let state = Path_state.dual_symbolic_state ps in
          let local_addr = Path_state.virtual_address ps in
          let consequent ps symbolic_state =
            Logger.debug ~level:3 "Branching into fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            (* count += 1 *)
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let state = Senv.assign "NUMBER_FAULTS" (Dba.Expr.add count (Dba.Expr.constant (Bitvector.of_int ~size:32 1))) Env.secondary_predicate (Path_state.dual_symbolic_state ps) in
            let ps = Path_state.set_dual_symbolic_state state ps in
            (* Assign *)
            assign_func (Dba.Expr.zeros (Dba.LValue.size_of lvalue)) Env.secondary_predicate ps
          in
          let alternative ps symbolic_state =
            Logger.debug ~level:3 "Branching into no-fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            assign_func rvalue Env.secondary_predicate ps 
          in
          match Senv.split_on b ~n:2 Env.secondary_predicate state with
          | [ bv, symbolic_state ] when Bitvector.is_zero bv -> alternative ps symbolic_state
          | [ bv, symbolic_state; _, symbolic_state' ] ->
            Stats.add_path ();
            let symbolic_state, symbolic_state' =
              if Bitvector.is_one bv then
                symbolic_state, symbolic_state'
              else
                symbolic_state', symbolic_state in
            Env.pick_alternative local_addr ~consequent:(consequent ps symbolic_state) ~alternative:(alternative ps symbolic_state') env
          | _ -> Env.pick_path env
        in
        let check_constrains ps =
          let state = Path_state.dual_symbolic_state ps in
          (* Add check (expr != 0x00) || !b to FAULT_CONSTRAIN *)
          let size_expression = Dba.LValue.size_of lvalue in
          let zero_exp = Dba.Expr.zeros size_expression in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains (Dba.Expr.logor (Dba.Expr.diff rvalue zero_exp) (Dba.Expr.lognot b))) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
          | "loc" -> (
            match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
              | None -> assign_func rvalue Env.secondary_predicate ps
              | Some s -> let ps = Path_state.set_dual_symbolic_state s ps in fault b ps
          )
          | _ -> fault b ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
        | "loc" -> (
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let state = Path_state.dual_symbolic_state ps in
          match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
          | None -> (
            Stats.add_after_max_reached ();
            let ps = Path_state.reaching_max_faults ps in
            Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
            assign_func rvalue Env.secondary_predicate ps
          )
          | Some _ -> check_constrains ps
        ) 
        | _ -> check_constrains ps
      in


      let bitflip_branchless ps =
        let fault b non_det ps =
          Stats.add_injection_location ();
          let ps = Path_state.add_injection_points_in_path ps in
          Logger.debug ~level:3 "Fault assignment BitFlip branchless %a" Dba_printer.Ascii.pp_bl_term non_det;
          let state = Path_state.dual_symbolic_state ps in
          let size_expression = Dba.LValue.size_of lvalue in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          let new_rvalue = Dba.Expr.ite b (Dba.Expr.logxor rvalue (Dba.Expr.shift_left (Dba.Expr.constant  (Bitvector.of_int ~size:size_expression 1)) non_det)) rvalue
          (* let new_rvalue = Dba.Expr.ite b (Dba.Expr.add rvalue (Dba.Expr.constant (Bitvector.of_int ~size:size_expression 1)) ) rvalue *)
          in
          (new_rvalue, ps)
        in
        let check_constrain ps = 
          let state = Path_state.dual_symbolic_state ps in
          (* Declare symbolic value non_det *)
          let size_expression = Dba.LValue.size_of lvalue in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let non_det_name = String.concat "" ["non_det_"; addr] in
          let non_det = Dba.Expr.var non_det_name size_expression in
          let state = Senv.fresh non_det_name size_expression Env.secondary_predicate state in
          (* Declare symbolic value b *)
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          (* Compute constrain according to fault model *)
          let new_constrain = Dba.Expr.logor (Dba.Expr.ult non_det (Dba.Expr.constant (Bitvector.of_int ~size:size_expression size_expression))) (Dba.Expr.lognot b)
          in
          (* non_det constrains handling *)
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains new_constrain) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
          | "loc" -> (
            match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
            | None -> let state = Senv.assign b_name Dba.Expr.zero Env.secondary_predicate (Path_state.dual_symbolic_state ps) in 
              (rvalue, Path_state.set_dual_symbolic_state state ps)
            | Some s -> (
              let ps = Path_state.set_dual_symbolic_state s ps in
              fault b non_det ps
            )
          )
          | _ -> fault b non_det ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
          | "loc" -> (
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let max = Dba.Expr.var "MAX_FAULTS" 32 in
            let state = Path_state.dual_symbolic_state ps in
            match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
            | None -> (
              Stats.add_after_max_reached ();
              let ps = Path_state.reaching_max_faults ps in
              Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
              (rvalue, ps)
            )
            | Some _ -> check_constrain ps
          ) 
          | _ -> check_constrain ps
      in

      let bitflip_branching assign_func ps =
        let fault b ps =
          Logger.debug ~level:3 "Fault assignment BitflipBranching";
          Stats.add_injection_location ();
          let state = Path_state.dual_symbolic_state ps in
          let local_addr = Path_state.virtual_address ps in
          let consequent ps symbolic_state =
            Logger.debug ~level:3 "Branching into fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            (* count += 1 *)
            let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
            let state = Senv.assign "NUMBER_FAULTS" (Dba.Expr.add count (Dba.Expr.constant (Bitvector.of_int ~size:32 1))) Env.secondary_predicate (Path_state.dual_symbolic_state ps) in
            let ps = Path_state.set_dual_symbolic_state state ps in
            let size_expression = Dba.LValue.size_of lvalue in
            let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
            let non_det_name = String.concat "" ["non_det_"; addr] in
            let non_det = Dba.Expr.var non_det_name size_expression in
            let state = Senv.fresh non_det_name size_expression Env.secondary_predicate (Path_state.dual_symbolic_state ps) in
            let ps = Path_state.set_dual_symbolic_state state ps in
            let new_rvalue = Dba.Expr.ite b (Dba.Expr.logxor rvalue (Dba.Expr.shift_left (Dba.Expr.constant  (Bitvector.of_int ~size:size_expression 1)) non_det)) rvalue in
            (* Assign *)
            assign_func new_rvalue Env.secondary_predicate ps
          in
          let alternative ps symbolic_state =
            Logger.debug ~level:3 "Branching into no-fault";
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.set_block_index idx ps in
            assign_func rvalue Env.secondary_predicate ps 
          in
          match Senv.split_on b ~n:2 Env.secondary_predicate state with
          | [ bv, symbolic_state ] when Bitvector.is_zero bv -> alternative ps symbolic_state
          | [ bv, symbolic_state; _, symbolic_state' ] ->
            Stats.add_path ();
            let symbolic_state, symbolic_state' =
              if Bitvector.is_one bv then
                symbolic_state, symbolic_state'
              else
                symbolic_state', symbolic_state in
            Env.pick_alternative local_addr ~consequent:(consequent ps symbolic_state) ~alternative:(alternative ps symbolic_state') env
          | _ -> Env.pick_path env
        in
        let check_constrains ps =
          let state = Path_state.dual_symbolic_state ps in
          (* Add check (expr != 0x00) || !b to FAULT_CONSTRAIN *)
          let size_expression = Dba.LValue.size_of lvalue in
          let zero_exp = Dba.Expr.zeros size_expression in
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.secondary_predicate state in
          let b = Dba.Expr.var b_name 1 in
          let fault_constrains = Dba.Expr.var "FAULT_CONSTRAINS" 1 in
          let state = Senv.assign "FAULT_CONSTRAINS" (Dba.Expr.logand fault_constrains (Dba.Expr.logor (Dba.Expr.diff rvalue zero_exp) (Dba.Expr.lognot b))) Env.secondary_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          match Sse_options.WhereCheckNonDetConstrain.get () with
          | "loc" -> (
            match Senv.assume (Dba.Expr.equal fault_constrains Dba.Expr.one) Env.secondary_predicate state with 
              | None -> assign_func rvalue Env.secondary_predicate ps
              | Some s -> let ps = Path_state.set_dual_symbolic_state s ps in fault b ps
          )
          | _ -> fault b ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
        | "loc" -> (
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let state = Path_state.dual_symbolic_state ps in
          match Senv.assume (Dba.Expr.ult count max) Env.secondary_predicate state with
          | None -> (
            Stats.add_after_max_reached ();
            let ps = Path_state.reaching_max_faults ps in
            Logger.debug ~level:4 "Max reached at loc : %b" (Path_state.is_max_fault_reached ps);
            assign_func rvalue Env.secondary_predicate ps
          )
          | Some _ -> check_constrains ps
        ) 
        | _ -> check_constrains ps
      in



      let is_fault_location ps = 
        (* Filter tmp and flag *)
        (* Filter faultable addresses *)
        (* Filter is max fault is reached *)
        (* Filter blacklist *)
        if Sse_options.InjectionMethod.is_default () 
          || Path_state.is_max_fault_reached ps 
          || Path_state.max_under_approx_injections_reached ps 
        then false 
        else (
          let range = Sse_options.TargetAddresses.get() in
          let tmp_flag = match lvalue with
            | Dba.LValue.Var f -> (match f.info with
                                    | Dba.VarTag.Temp -> true
                                    | Dba.VarTag.Flag _ -> not (Sse_options.FaultFlags.get ())
                                    | _ -> false
                                    )
            | _ -> false
          in
          
          if range = [] || (Sse_options.FaultModel.is_default ()) || tmp_flag then
            false
          else (
            let current = Virtual_address.to_int (Path_state.inst ps).address in
            let rec in_target target = match target with
            | [] -> false
            | lo::hi::tail -> if current >= lo && current <= hi then true else in_target tail
            | _ -> assert false
            in
            if in_target range
              then (
                let above_threshold () = match rvalue with
                  | Cst bv -> Bitvector.to_int bv > Sse_options.ValueThresholdForFaults.get()
                  | _ -> false
                in
                if not (above_threshold ())
                  then (
                    (* filter blacklisted names *)
                    let contains s1 s2 =
                      let re = Str.regexp_string s2
                      in
                        try ignore (Str.search_forward re s1 0); true
                        with Not_found -> false
                    in
                    let lvalue_str = Format.asprintf "%a" Dba_printer.Ascii.pp_lhs lvalue in
                    let f acc x = (contains lvalue_str x) || acc in
                    let is_blacklisted = List.fold_left f false (Sse_options.TargetBlackList.get()) in
                    (* if is_blacklisted then Logger.debug ~level:3 "blacklist of %a : %b"
                    Dba_printer.Ascii.pp_lhs lvalue (is_blacklisted); *)
                    not is_blacklisted
                  )
                  else false
              ) 
              else false
          )
        )
      in

      (* Fault model wrapping *)
      let faulty_assignment assign_func rv ps =
          Logger.debug ~level:3 "Data fault location valid  %s" (Sse_options.FaultModel.get());

          match Sse_options.FaultModel.get() with
          | fm when (fm = "ArbitraryData" || fm = "ArbitraryDataMul" || fm = "ArbitraryDataAnd" || fm = "ArbitraryDataIte" || fm = "ArbitraryDataXor") ->
            let ps = Path_state.add_to_first_faults ps in
            let new_rvalue, ps = arbitrary_data_behavior rv ps fm in
            assign_func new_rvalue Env.secondary_predicate ps
          (* | "ArbitraryDataBranchless2" -> 
            let ps = Path_state.add_to_first_faults ps in
            let new_rvalue, ps = arbitrary_data_behavior2 ps in
            assign_func new_rvalue ps *)
          | "ArbitraryDataBranching" -> (
            match Sse_options.InjectionMethod.get () with
            | "always" -> let ps = Path_state.add_to_first_faults ps in
            arbitrary_data_branching_behavior assign_func rv ps
            | "on-demand" -> (
              Sse_options.Logger.warning "Undefined behavior: using ArbitraryDataBranching in on-demand mode"; 
              assign_func rv Env.main_predicate ps
            )
            | _ -> assign_func rv Env.main_predicate ps
          )
          | fm when (fm = "ResetBranchless" || fm = "ResetBranchlessIte" || fm = "ResetBranchlessSub" || fm = "ResetBranchlessAnd" || fm = "ResetBranchlessXor") -> 
            let ps = Path_state.add_to_first_faults ps in
            let new_rvalue, ps = reset_branchless fm ps in
            assign_func new_rvalue Env.secondary_predicate ps
            
          (* | "ResetBranchless2" -> 
            let ps = Path_state.add_to_first_faults ps in
            let new_rvalue, ps = reset_branchless2 ps in
            assign_func new_rvalue ps *)
          | "ResetBranching" -> 
            let ps = Path_state.add_to_first_faults ps in
            reset_branching assign_func ps
          | "BitFlipBranchless" -> 
            let ps = Path_state.add_to_first_faults ps in
            let new_rvalue, ps = bitflip_branchless ps in
            assign_func new_rvalue Env.secondary_predicate ps
          | "BitFlipBranching" -> 
            let ps = Path_state.add_to_first_faults ps in
            bitflip_branching assign_func ps
          | _ -> assign_func rv Env.main_predicate ps
        in

        (* Injection method wrapping *)
        let dual_assign assign_func rv ps = 
          match Sse_options.InjectionMethod.get () with
          | "on-demand" -> (
            if not (Path_state.max_under_approx_injections_reached ps) && not (Path_state.is_max_fault_reached ps) then (
              let ps = assign_func rv Env.main_predicate ps in
              match is_fault_location ps with
              | true -> faulty_assignment assign_func rv ps
              | false -> assign_func rv Env.secondary_predicate ps
            )
            else (
              assign_func rv Env.main_predicate ps
            )
          )
          | "always" -> (
            match is_fault_location ps with
            | true -> faulty_assignment assign_func rv ps
            | false -> assign_func rv Env.main_predicate ps
          )
          | "none" | "test-inversion" -> assign_func rv Env.main_predicate ps
          | _ -> assert false
        in

        (* main assign behavior *)
        let ps = match lvalue with
        | Dba.LValue.Var { name; _ } ->
          dual_assign (assignment_lvalue name) rvalue ps
        | Dba.LValue.Restrict (var, { lo; hi }) ->
          dual_assign (assignment_lvalue var.name) (Dba_utils.Expr.complement rvalue ~hi ~lo var) ps
        | Dba.LValue.Store (_, dir, addr) ->
          dual_assign (assignment_store dir addr) rvalue ps
        in
        Path_state.set_block_index idx ps

    let nondet ~lvalue ps =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
      *)
      let symbolic_state = match lvalue with
        | Dba.LValue.Var { name; size; _ } ->
          Senv.fresh name size Senv.Both (Path_state.dual_symbolic_state ps)
        | Dba.LValue.Restrict (var, { lo; hi }) ->
          let size = hi - lo + 1 in
          let nondet = Dba.Expr.var "bs_unknown" size in
          Senv.assign var.name (Dba_utils.Expr.complement nondet ~hi ~lo var)
            Senv.Both
            (Senv.fresh "bs_unknown" size Senv.Both (Path_state.dual_symbolic_state ps))
        | Dba.LValue.Store (size, dir, addr) ->
          let nondet = Dba.Expr.var "bs_unknown" (8 * size) in
          Senv.write ~addr nondet dir Senv.Both
            (Senv.fresh "bs_unknown" size Senv.Both   (Path_state.dual_symbolic_state ps)) in
      Path_state.set_dual_symbolic_state symbolic_state ps

    let ite ~condition ~jump_target ~local_target e ps =

      let ps = Path_state.add_to_depth_bcht ps in
      Stats.update_max_depth_bcht (Path_state.get_depth_bcht ps);

      let first_path = ref false in
      let then_branch path_state = 
        let ps_opt = static_jump ~jump_target e path_state in
        match ps_opt with 
        | None -> assert false
        | Some ps' -> (
          first_path := true; 
          Env.add_path e ps'
        )
      in
      let else_branch path_state = 
        if !first_path then Stats.add_path();
        let path_state = Path_state.set_block_index local_target path_state in
        Env.add_path e path_state;
      in
      let conditional_branch_normal condition ps =
        Logger.debug ~level:3 "Normal ite %a" Dba_printer.Ascii.pp_expr condition;
        let state = Path_state.dual_symbolic_state ps in
        (* fault constrain and such_that *)
        let fault_constrains = match Sse_options.WhereCheckNonDetConstrain.get () with
          | "branch" -> ((Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) (Dba.Expr.constant (Bitvector.one))))
          | _ -> (Dba.Expr.constant (Bitvector.one))
        in
        let _ = match Senv.assume condition ~fault_check:fault_constrains Env.main_predicate state with
          | Some s -> Logger.debug ~level:3 "solver ok"; then_branch (Path_state.set_dual_symbolic_state s ps)
          | None -> Logger.debug ~level:3 "Infeasible path";()
        in
        (* path and !condition *)
        Logger.debug ~level:3 "Normal ite !condition %a" Dba_printer.Ascii.pp_expr (Dba.Expr.unary Dba.Unary_op.Not condition);
        match Senv.assume (Dba.Expr.unary Dba.Unary_op.Not condition) ~fault_check:fault_constrains Env.main_predicate state with
          | Some s -> Logger.debug ~level:3 "solver ok";else_branch (Path_state.set_dual_symbolic_state s ps) 
          | None -> Logger.debug ~level:3 "Infeasible path"; ()
      in


      let test_inversion_branchless_direct cdt ps =
        Logger.debug ~level:3 "Fault assignment TestInversionBranchlessDirect";
        Stats.add_injection_location ();
        let state = Path_state.dual_symbolic_state ps in
        match Senv.split_on cdt Env.main_predicate state with
        | [ bv, symbolic_state; _, symbolic_state' ] ->
          Logger.debug ~level:3 "both outcomes possibles";
          let symbolic_state, symbolic_state' =
            if Bitvector.is_one bv then
              symbolic_state, symbolic_state'
            else
              symbolic_state', symbolic_state in
          then_branch (Path_state.set_dual_symbolic_state symbolic_state ps);
          else_branch (Path_state.set_dual_symbolic_state symbolic_state' ps);
          ()
        | [ bv, symbolic_state ] when Bitvector.is_one bv -> (
          Logger.debug ~level:3 "fault to go to else branch";
          then_branch (Path_state.set_dual_symbolic_state symbolic_state ps);
          let ps' = Path_state.add_under_approx_injections ps in
          let local_addr = Path_state.virtual_address ps' in
          let ps' = Path_state.add_test_inversion_direct_flips ps' (local_addr, 'e') in
          else_branch ps'
        )
        | [ bv, symbolic_state ] when Bitvector.is_zero bv -> (
          Logger.debug ~level:3 "fault to go to then branch";
          let ps' = Path_state.add_under_approx_injections ps in
          let local_addr = Path_state.virtual_address ps' in
          let ps' = Path_state.add_test_inversion_direct_flips ps' (local_addr, 't') in
          then_branch ps';
          else_branch (Path_state.set_dual_symbolic_state symbolic_state ps);
        )
        | _ -> ()

      in



      let test_inversion_branchless_ite cdt ps =
        let fault ps =
          Logger.debug ~level:3 "Fault assignment TestInversionBranchlessIte";
          Stats.add_injection_location ();
          let state = Path_state.dual_symbolic_state ps in
          (* Declare symbolic boolean b *)
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.main_predicate state in
          let b = Dba.Expr.var b_name 1 in
          (* Increment NUMBER_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let count = Dba.Expr.add (Dba.Expr.uext 32 b) count in
          let state = Senv.assign "NUMBER_FAULTS" count Env.main_predicate state in
          let ps = Path_state.set_dual_symbolic_state state ps in
          (* x := ite b ? !cdt : cdt *)
          let new_cdt = Dba.Expr.ite b (Dba.Expr.lognot cdt) cdt in
          conditional_branch_normal new_cdt ps
        in
        match Sse_options.WhereCheckNbFaults.get () with
        | "loc" | "branch" -> (
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let state = Path_state.dual_symbolic_state ps in
          match Senv.assume (Dba.Expr.ult count max) Env.main_predicate state with
          | None -> (
            Stats.add_after_max_reached ();
            let ps = Path_state.reaching_max_faults ps in conditional_branch_normal condition ps
          )
          | Some _ -> fault ps
        ) 
        | _ -> fault ps
      in

      let test_inversion_branching cdt ps =
        let fault ps =
          Logger.debug ~level:3 "Fault assignment TestInversionBranching";
          Stats.add_injection_location ();
          let state = Path_state.dual_symbolic_state ps in
          (* Declare symbolic boolean b *)
          let addr = Printf.sprintf "%x" (Virtual_address.to_int (Path_state.inst ps).address) in
          let b_name = String.concat "" ["b_"; addr] in
          let state = Senv.fresh b_name 1 Env.main_predicate state in
          let b = Dba.Expr.var b_name 1 in
          match Senv.split_on b ~n:2 Env.main_predicate state with
          | [ bv, symbolic_state; _, symbolic_state' ] ->
            Stats.add_path ();
            let symbolic_state, symbolic_state' =
              if Bitvector.is_one bv then
                symbolic_state, symbolic_state'
              else
                symbolic_state', symbolic_state in
            let consequent =
              Logger.debug ~level:3 "Branching into fault";
              let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
              (* count += 1 *)
              let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
              let state = Senv.assign "NUMBER_FAULTS" (Dba.Expr.add count (Dba.Expr.constant (Bitvector.of_int ~size:32 1))) Env.main_predicate (Path_state.dual_symbolic_state ps) in
              let ps = Path_state.set_dual_symbolic_state state ps in
              (* invert condition *)
              let ncdt = Dba.Expr.lognot cdt in
              conditional_branch_normal ncdt ps
            in
            let alternative =
              Logger.debug ~level:3 "Branching into no-fault";
              let ps = Path_state.set_dual_symbolic_state symbolic_state' ps in
              conditional_branch_normal cdt ps
            in
            consequent; alternative;
            ()
          | _ -> ()
        in
        match Sse_options.WhereCheckNbFaults.get () with
        | "loc" | "branch" -> (
          (* Check NUMBER_FAULTS < MAX_FAULTS *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let state = Path_state.dual_symbolic_state ps in
          match Senv.assume (Dba.Expr.ult count max) Env.main_predicate state with
          | None -> (
            Stats.add_after_max_reached ();
            let ps = Path_state.reaching_max_faults ps in conditional_branch_normal cdt ps
          )
          | Some _ -> fault ps
        )
        | _ -> fault ps
      in

      let is_fault_location ps = 
        (* Filter faultable addres *)
        if (Path_state.is_max_fault_reached ps) or (Path_state.max_under_approx_injections_reached ps) then false else (
          let range = Sse_options.TargetAddresses.get() in
          if range = [] || (Sse_options.FaultModel.is_default ()) || (Sse_options.MaxFaults.get() == 0) then
            false
          else (
            let current = Virtual_address.to_int (Path_state.inst ps).address in
            let rec in_target target = match target with
            | [] -> false
            | lo::hi::tail -> if current >= lo && current <= hi then true else in_target tail
            | _ -> assert false
            in
            if in_target range
              then (
                (* filter blacklisted names *)
                let contains s1 s2 =
                  let re = Str.regexp_string s2
                  in
                    try ignore (Str.search_forward re s1 0); true
                    with Not_found -> false
                in
                let lvalue_str = Format.asprintf "%a" Dba_printer.Ascii.pp_expr condition in
                let f acc x = (contains lvalue_str x) || acc in
                let is_blacklisted = List.fold_left f false (Sse_options.TargetBlackList.get()) in
                if is_blacklisted then Logger.debug ~level:3 "blacklist of %a : %b"
                Dba_printer.Ascii.pp_expr condition is_blacklisted;
                not is_blacklisted
              )
              else false
            )
          )
      in


      
      (* Injection method wrapping *)
      let _ = match Sse_options.InjectionMethod.get () with
      | "on-demand" -> (
        Sse_options.Logger.debug ~level:4 "first faults: %d" (Path_state.get_first_faults ps);
        let conditional_branch_OD cdt which_branch branch_func ps =
          Logger.debug ~level:3 "Test %c condition %a" which_branch Dba_printer.Ascii.pp_expr cdt;

          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          (* fault constrain and such_that *)
          let fault_check = match Sse_options.WhereCheckNonDetConstrain.get () with
            | "branch" -> ((Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) (Dba.Expr.constant (Bitvector.one))))
            | _ -> (Dba.Expr.constant (Bitvector.one))
          in
    
          let state = Path_state.dual_symbolic_state ps in
          match Senv.assume cdt ~fault_check ~such_that:(Dba.Expr.ule count max) Env.main_predicate state with
          | Some dual -> (
            Stats.add_normal_ok ();
            Logger.debug ~level:3 "solver ok";
            let ps = Path_state.set_dual_symbolic_state dual ps in
            branch_func ps
          )
          | None -> (
            Sse_options.Logger.debug ~level:4 "fault already happened %b" (Path_state.fault_already_happened ps);
            Sse_options.Logger.debug ~level:4 "max under approx injection reached %b" (Path_state.max_under_approx_injections_reached ps);
            if not (Path_state.fault_already_happened ps) || (Path_state.max_under_approx_injections_reached ps) || (Path_state.is_max_fault_reached ps)
            then (
              let _ = Stats.add_normal_ko () in
              Logger.debug ~level:3 "Test %c condition infeasible with no fault option" which_branch
            )
            else (
              Logger.debug ~level:3 "Test %c condition with fault" which_branch;
              
              (* Option to add saturation detection to OD *)
              if Sse_options.AtBranchOptim.get () then (
                match Senv.assume cdt ~fault_check ~such_that:(Dba.Expr.ult count max) Env.secondary_predicate state with
                | Some dual -> (
                  Stats.add_faulted_ok ();
                  Logger.debug ~level:3 "Test %c condition with fault option ok, not reaching max faults" which_branch;
                  let ps = Path_state.add_under_approx_injections ps in
                  let ps = Path_state.set_dual_symbolic_state dual ps in
                  let ps = Path_state.fault_normal_state ps which_branch in
                  branch_func ps
                )
                | None -> (
                  match Senv.assume cdt ~fault_check ~such_that:(Dba.Expr.equal count max) Env.secondary_predicate state with
                  | Some dual -> (
                    Stats.add_faulted_ok ();
                    Logger.debug ~level:3 "Test %c condition fault saturation detection" which_branch;
                    let ps = Path_state.reaching_max_faults ps in
                    let ps = Path_state.add_under_approx_injections ps in
                    let ps = Path_state.set_dual_symbolic_state dual ps in
                    let ps = Path_state.fault_normal_state ps which_branch in
                    branch_func ps
                  )
                  | None -> (
                    Logger.debug ~level:3 "Test %c condition infeasible despite fault option" which_branch;
                    Stats.add_faulted_ko ()
                  )
                )
              )
              else (
                (* No saturation detection *)
                match Senv.assume cdt ~fault_check ~such_that:(Dba.Expr.ule count max) Env.secondary_predicate state with
                | None -> (
                  Logger.debug ~level:3 "Test %c condition infeasible despite fault option" which_branch;
                  Stats.add_faulted_ko ()
                )
                | Some dual -> (
                  Stats.add_faulted_ok ();
                  Logger.debug ~level:3 "Test %c condition with fault option ok" which_branch;
                  let ps = Path_state.add_under_approx_injections ps in
                  let ps = Path_state.set_dual_symbolic_state dual ps in
                  let ps = Path_state.fault_normal_state ps which_branch in
                  branch_func ps
                )
              )
            )
          )
        in 
        conditional_branch_OD condition 't' then_branch ps;
        conditional_branch_OD (Dba.Expr.lognot condition) 'e' else_branch ps
      )

      | "always" -> (
        let conditional_branch_IP condition ps =
          let state = Path_state.dual_symbolic_state ps in
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          (* path and condition and nb <= max *)
          let guard = Dba.Expr.ule count max in
          (* fault constrain and such_that *)
          let fault_constrains = match Sse_options.WhereCheckNonDetConstrain.get () with
            | "branch" -> Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) Dba.Expr.one
            | _ -> Dba.Expr.one
          in
          let fault_check = (Dba.Expr.logand fault_constrains guard) in
          Logger.debug ~level:3 "ite IP optim condition %a" Dba_printer.Ascii.pp_expr condition;
          let _ = match Senv.assume condition 
                          ~fault_check
                          ~such_that:(Dba.Expr.ult count max) 
                          Env.main_predicate
                          state with
          | Some s -> (
            let ps' = Path_state.set_dual_symbolic_state s ps in
            let ps_opt = static_jump ~jump_target e ps' in
            match ps_opt with 
            | None -> assert false
            | Some ps' -> (
              Logger.debug ~level:3 "feasible without reaching max faults"; 
              first_path := true; 
              Env.add_path e ps'
            )
          )
          | None -> (
            (* path and condition and nb == max *)
            Stats.add_nb_snd_query ();
            let fault_check = Dba.Expr.logand fault_constrains (Dba.Expr.equal count max) in
            match Senv.assume condition ~fault_check Env.main_predicate state with
            | Some s -> (
              Stats.add_after_max_reached ();
              let ps' = Path_state.set_dual_symbolic_state s ps in
              let ps' = Path_state.reaching_max_faults ps' in
              let ps_opt = static_jump ~jump_target e ps' in
              match ps_opt with 
              | None -> assert false
              | Some ps' -> Logger.debug ~level:3 "Reaching max faults"; first_path := true; Env.add_path e ps'
            )
            | None -> Logger.debug ~level:3 "Infeasible path";()
          ) in
          (* path and !condition and nb < max *)
          Logger.debug ~level:3 "ite optim !condition %a"
            Dba_printer.Ascii.pp_expr (Dba.Expr.lognot condition);
          (* let _ = match Senv.assume *)
          (* (Dba.Expr.logand (Dba.Expr.lognot condition) guard) *)
          let fault_check = Dba.Expr.logand  fault_constrains guard in
          match Senv.assume
                          (Dba.Expr.lognot condition) ~fault_check ~such_that:(Dba.Expr.ult count max)
                          Env.main_predicate
                          state with
          | Some s -> (
            let ps' = Path_state.set_dual_symbolic_state s ps in
            let ps' = Path_state.set_block_index local_target ps' in
            Logger.debug ~level:3 "feasible without reaching max faults";
            if !first_path then Stats.add_path ();
            Env.add_path e ps'
          )
          | None -> (
            (* path and !condition and nb == max *)
            Stats.add_nb_snd_query ();
            let fault_check = Dba.Expr.logand fault_constrains (Dba.Expr.equal count max) in
            match Senv.assume (Dba.Expr.lognot condition) 
                                ~fault_check Env.main_predicate state with
            | Some s -> (
              Stats.add_after_max_reached ();
              let ps' = Path_state.set_dual_symbolic_state s ps in
              let ps' = Path_state.reaching_max_faults ps' in
              let ps' = Path_state.set_block_index local_target ps' in
              Logger.debug ~level:3 "Reaching max faults"; 
              if !first_path then Stats.add_path ();
              Env.add_path e ps'
            )
            | None -> Logger.debug ~level:3 "Infeasible path"; ()
          )
        in

        let conditional_branch_vanilla_faulted condition ps =
          let state = Path_state.dual_symbolic_state ps in
          (* path and condition and nb <= max *)
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          (* fault constrain and such_that *)
          let fault_constrains = match Sse_options.WhereCheckNonDetConstrain.get () with
            | "branch" -> ((Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) (Dba.Expr.constant (Bitvector.one))))
            | _ -> (Dba.Expr.constant (Bitvector.one))
          in
          Logger.debug ~level:3 "naive ite condition %a" Dba_printer.Ascii.pp_expr condition;
          let fault_check = Dba.Expr.logand (Dba.Expr.ule count max) fault_constrains in
          let _ = match Senv.assume condition ~fault_check Env.main_predicate state with
            | Some s -> Logger.debug ~level:3 "ok"; then_branch (Path_state.set_dual_symbolic_state s ps)
            | None -> Logger.debug ~level:3 "Infeasible path";()
          in
          (* path and !condition and nb <= max *)
          Logger.debug ~level:3 "naive ite !condition %a" Dba_printer.Ascii.pp_expr (Dba.Expr.unary Dba.Unary_op.Not condition);
          match Senv.assume (Dba.Expr.lognot condition) ~fault_check Env.main_predicate state with
            | Some s -> Logger.debug ~level:3 "ok"; else_branch (Path_state.set_dual_symbolic_state s ps)
            | None -> Logger.debug ~level:3 "Infeasible path"; ()
        in

        Logger.debug ~level:3 "First faults: %d" (Path_state.get_first_faults ps);
        match Sse_options.WhereCheckNbFaults.get (), (not (Sse_options.FaultModel.is_default ())) && (Path_state.fault_already_happened ps) && not (Path_state.is_max_fault_reached ps) with
        | "branch", true -> (
          if (Sse_options.AtBranchOptim.get ()) then 
            conditional_branch_IP condition ps
          else conditional_branch_vanilla_faulted condition ps
        )
        | _ -> conditional_branch_normal condition ps
      )
        
      | "test-inversion" -> (
        match is_fault_location ps, Sse_options.FaultModel.get () with
        | true, "TestInversionBranchlessIte" -> test_inversion_branchless_ite condition ps
        | true, "TestInversionBranchlessDirect" -> test_inversion_branchless_direct condition ps 
        | true, "TestInversionBranching" -> test_inversion_branching condition ps
        | _ -> conditional_branch_normal condition ps
      )
      | "none" -> conditional_branch_normal condition ps
      | _ -> ()
      in
      Env.pick_path e


    let dynamic_jump ~jump_expr e ps =
      let img = Kernel_functions.get_img () in
      let n = Sse_options.JumpEnumDepth.get () in
      let b = ref false in
      List.iter (fun (bv, symbolic_state) ->
          begin if !b then (
            Stats.add_path ();
            Logger.debug "addind path";) 
          else b := true end;
          Logger.debug ~level:4 "@[<hov>Dynamic jump@ %a@ could lead to@ %a@]"
            Path_state.pp_loc ps Bitvector.pp_hex bv;
          let addr = Virtual_address.of_bitvector bv in
          if C.mem_vertex_a e.Env.cfg addr <> None then
            Env.add_if_good e
              (goto e (Path_state.set_dual_symbolic_state symbolic_state ps) addr)
          else
            let section = Loader_utils.find_section_by_address
                ~address:(Virtual_address.to_int addr) img in
            match section with
            | Some s when
                Loader.Section.has_flag Loader_types.Read s &&
                Loader.Section.has_flag Loader_types.Exec s ->
              Env.add_if_good e
                (goto e (Path_state.set_dual_symbolic_state symbolic_state ps) addr)
            | Some _ | None ->
              (Logger.warning
                "@[<hov>Dynamic jump@ %a@ could have led to invalid address\
                 %a;@ skipping@]"
                Path_state.pp_loc ps Bitvector.pp_hex bv);
                (* Logger.info "%a" Senv.pp symbolic_state *)
              )
        (Senv.split_on jump_expr ~n Env.main_predicate (Path_state.dual_symbolic_state ps));
      Env.pick_path e

    let skip instruction idx ps =
      Logger.info ~level:3 "Skipping %a"
        Dba_printer.Ascii.pp_instruction instruction;
      Path_state.set_block_index idx ps

    let assertion =
      let assertion_failure session ps =
        Sse_options.Logger.error
          "@[<v 2> Assertion failed %@ %a@ %a@]"
          Path_state.pp_loc ps
          Senv.pp_normal session;
        Stats.add_assert_failed () in
      fun cond idx e ps ->
        (* Sse_options.Logger.debug ~level:5 "assert here"; *)
        (* Sse_options.Logger.debug ~level:5 "%a" Dba_printer.Ascii.pp_expr cond; *)
        (* Senv.print_contraints (Path_state.symbolic_state ps); *)
        match Senv.split_on cond ~n:2 Env.main_predicate (Path_state.dual_symbolic_state ps) with
        | [ bv, symbolic_state ] when Bitvector.is_one bv ->
          Path_state.set_block_index idx
            (Path_state.set_dual_symbolic_state symbolic_state ps)
        | [ _, symbolic_state ] ->
          assertion_failure symbolic_state ps;
          Env.pick_path e
        | [ bv, symbolic_state; _, symbolic_state' ] ->
          let symbolic_state, symbolic_state' =
            if Bitvector.is_one bv then
              symbolic_state, symbolic_state'
            else
              symbolic_state', symbolic_state in
          assertion_failure symbolic_state' ps;
          let res = Path_state.set_block_index idx
            (Path_state.set_dual_symbolic_state symbolic_state ps) in
          (* Senv.print_contraints (Path_state.symbolic_state ps); *)
          res
        | _ -> Env.pick_path e

    let assumption cond idx e ps =
      match Senv.assume cond Env.main_predicate (Path_state.dual_symbolic_state ps) with
      | None ->
        Logger.info "@[<h>Unsatifiable assumption %@ %a@]" Path_state.pp_loc ps;
        Env.pick_path e
      | Some symbolic_state ->
        Path_state.set_block_index idx
          (Path_state.set_dual_symbolic_state symbolic_state ps)


    let go e ps =
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc ps;
      match Path_state.dba_instruction ps with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
        assignment ~lvalue ~rvalue idx ps e
        (* Path_state.set_block_index idx ps *)
      | Dba.Instr.Nondet(lvalue,_,idx) ->
        let ps = nondet ~lvalue ps in
        Path_state.set_block_index idx ps

      | Dba.Instr.SJump (jump_target, _) -> begin
          match static_jump ~jump_target e ps with
          | None -> (* This jump has been forbidden *)
            Env.pick_path e
          | Some local -> local
        end
      | Dba.Instr.If (condition, jump_target, local_target) ->
        ite ~condition ~jump_target ~local_target e ps

      | Dba.Instr.DJump (je, _) -> dynamic_jump ~jump_expr:je e ps
      | Dba.Instr.Undef(_, idx) as instruction -> skip instruction idx ps
      | Dba.Instr.Stop _ ->
        (* Discard current path, choose a new one *)
        Env.pick_path e
      | Dba.Instr.Assert (condition, idx) ->
        assertion condition idx e ps
      | Dba.Instr.Assume (condition, idx) ->
        assumption condition idx e ps
      | Dba.Instr.NondetAssume _
      | Dba.Instr.Malloc _
      | Dba.Instr.Free _
      | Dba.Instr.Print _ as dba_instruction ->
        let msg =
          Format.asprintf "%a" Dba_printer.Ascii.pp_instruction
            dba_instruction in
        Errors.not_yet_implemented msg
  end

  let loop_until ~halt e ps =
    let last_vaddr = ref (Path_state.virtual_address ps) in

    let rec loop_aux ps =
      let vaddr = Path_state.virtual_address ps in
      if vaddr <> !last_vaddr then begin
        Logger.debug ~level:2 "%@%a %a"
          Virtual_address.pp vaddr
          Mnemonic.pp  (Instruction.mnemonic (Path_state.inst ps))
        ;
        if not (Path_state.may_lead_to_goal ps) then loop_aux (Env.pick_path e)
        else (
          last_vaddr := vaddr;
          Stats.add_analysed_instructions ();
          do_directives vaddr e ps
        )
      end
      (* When the last virtual addresse has not changed, when are still in the
         same DBA block, hence no user action can have been performed.
         So, we just continue.
      *)
      else loop_aux (Eval.go e ps)

    and handle_assumptions vaddr e ps =
      match Virtual_address.Htbl.find e.Env.assume vaddr with
      | exception Not_found -> Some ps
      | assumptions ->
        let assumption =
          List.fold_left (fun e a -> Dba.Expr.logand e a)
            (List.hd assumptions) (List.tl assumptions) in
        Logger.debug "Assume %a %@ %a"
          Dba_printer.Ascii.pp_bl_term assumption Virtual_address.pp vaddr;
        match Senv.assume assumption Env.main_predicate (Path_state.dual_symbolic_state ps) with
        | None -> (
          let end_path ps = 
            Logger.result
                "@[<h>Directive :: unsatifiable assumption %@ %a@]"
                Virtual_address.pp vaddr;
              Stats.update_max_depth (Path_state.get_depth ps);
              Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
              None
          in
          match Sse_options.InjectionMethod.get() with
          | "on-demand" -> (
            if (Path_state.is_max_fault_reached ps) || (Path_state.max_under_approx_injections_reached ps) then (
              end_path ps
            )
            else (
              let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
              let max = Dba.Expr.var "MAX_FAULTS" 32 in
              let fault_constrains = Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) (Dba.Expr.constant (Bitvector.one)) in
              let fault_check = Dba.Expr.logand fault_constrains (Dba.Expr.ule count max) in
              match Senv.assume assumption ~fault_check Env.secondary_predicate (Path_state.dual_symbolic_state ps) with
              | None -> 
                Logger.result
                "@[<h>Directive :: unsatifiable assumption with faults @]";
                Stats.update_max_depth (Path_state.get_depth ps);
                Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
                None
              | Some dual -> (
                Logger.result
                "@[<h>Directive :: satifiable assumption with faults @]";
                let ps = Path_state.add_under_approx_injections ps in
                let ps = Path_state.set_dual_symbolic_state dual ps in
                let ps = Path_state.fault_normal_state ps 'a' in
                Some(ps)
              )
            )
          )
          | _ -> end_path ps
        )
              
        | Some symbolic_state ->
          Some (Path_state.set_dual_symbolic_state symbolic_state ps)

    and handle_reach vaddr e ps =
      match Virtual_address.Htbl.find e.Env.reach vaddr with
      | exception Not_found -> ()
      | reachs ->
        let reachs' = Queue.create () in
        Queue.iter (fun ((k, guard) as r) ->
          Logger.debug "Reach";
          let count = Dba.Expr.var "NUMBER_FAULTS" 32 in
          let max = Dba.Expr.var "MAX_FAULTS" 32 in
          let fault_constrains = Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) (Dba.Expr.constant (Bitvector.one)) in
          let fault_check = Dba.Expr.logand fault_constrains (Dba.Expr.ule count max) in

          let end_path ps = Logger.info
            "@[<h>Directive :: reached address %a \
            with unsat condition despite faults (%a to go)@]"
            Virtual_address.pp vaddr Directive.Count.pp k;
            Stats.update_max_depth (Path_state.get_depth ps);
            Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
            Queue.add r reachs'
          in
          let reach_path ps which_dual = 
            let k' = Directive.Count.decr k in
            Logger.result
              "@[<h>Directive :: reached address possibly with faults %a (%a to go)@]"
              Virtual_address.pp vaddr Directive.Count.pp k';
            (* Logger.result "%a" Path_state.pp_path ps; *)
            Stats.add_reached_models ();
            Stats.update_max_depth (Path_state.get_depth ps);
            Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
            (* let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            let ps = Path_state.add_under_approx_injections ps in
            let ps = Path_state.fault_normal_state ps in *)
            let pp_dual = match which_dual with 
            | Senv.Normal -> Senv.pp_normal
            | Senv.Faulted -> Senv.pp_faulted
            | _ -> assert false
            in
            Logger.result "@[<v 0>Model %@ %a@ %a@]"
              Virtual_address.pp vaddr
              pp_dual (Path_state.dual_symbolic_state ps);
            if String.compare (Sse_options.InjectionMethod.get ()) "on-demand" == 0 then (
              Logger.result "Predicate flips:";
              List.iter (fun (addr, c) -> Logger.result "%a %c" Virtual_address.pp addr c) (Path_state.get_predicate_flips ps);
            );
            let max_f = Sse_options.MaxFaults.get () in
            let _ = match Sse_options.FaultModel.get () with
            | "TestInversionBranchlessDirect" -> (
              let count_value = Path_state.get_under_approx_injections ps in 
              Sse_options.Logger.result "NUMBER_FAULTS = %d for MAX_FAULTS = %d \n"
                count_value
                max_f
            )
            | _ -> (
              let count_value = Senv.concrete_eval count Env.main_predicate (Path_state.dual_symbolic_state ps) in
              Sse_options.Logger.result "NUMBER_FAULTS = %a for MAX_FAULTS = %d \n"
                Bitvector.pp count_value
                max_f
            )
            in 
            match k' with
            | Directive.Count.Count 0 -> ()
            | _ -> Queue.add (k', guard) reachs'
          in
          
          match Senv.assume guard ~fault_check Env.main_predicate (Path_state.dual_symbolic_state ps) with
          | None -> (
            
            match Sse_options.InjectionMethod.get() with
            | "on-demand" -> (
              if (Path_state.is_max_fault_reached ps) || (Path_state.max_under_approx_injections_reached ps) then (
                end_path ps
              )
              else (
                match Senv.assume guard ~fault_check Env.secondary_predicate (Path_state.dual_symbolic_state ps) with
                | None -> end_path ps
                | Some symbolic_state -> 
                  let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
                  reach_path ps Senv.Faulted
              )
            )
            | _ -> end_path ps
          )
          | Some symbolic_state -> 
            let ps = Path_state.set_dual_symbolic_state symbolic_state ps in
            reach_path ps Senv.Faulted
          ) reachs;
              
        if Queue.length reachs' = 0 then
          Virtual_address.Htbl.remove e.Env.reach vaddr
        else Virtual_address.Htbl.replace e.Env.reach vaddr reachs'

    and handle_enumerate vaddr e ps =
      match Virtual_address.Htbl.find e.Env.enumerate vaddr with
      | exception Not_found -> ()
      | enumerations ->
        let enumerations' = Queue.create () in
        Queue.iter (fun (k, x) ->
            let bvs =
              try Etbl.find
                    (Virtual_address.Htbl.find e.Env.enumerations vaddr) x
              with Not_found -> [] in
            let n = List.length bvs in
            let bvs' = Senv.split_on x ~n:k ~except:bvs Env.main_predicate (Path_state.dual_symbolic_state ps) in
            let n' = List.length bvs' in
            let bvs = List.sort_uniq Bitvector.compare
                (List.fold_left (fun bvs (bv, _) -> bv :: bvs) bvs bvs') in
            Logger.result
              "@[<hov 0>Directive :: enumerate@ \
               possible values (%d) for %a %@ %a:@ @[<hov 0>%a@]@]"
              (n + n')
              Dba_printer.EICAscii.pp_bl_term x
              Virtual_address.pp vaddr
              (Print_utils.pp_list ~sep:",@ " Bitvector.pp) bvs;
            Stats.update_max_depth (Path_state.get_depth ps);
            Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
            if n' < k then
              Queue.add (k - n', x) enumerations';
            try Etbl.replace
                  (Virtual_address.Htbl.find e.Env.enumerations vaddr) x bvs
            with Not_found ->
              let tbl = Etbl.create 7 in
              Etbl.add tbl x bvs;
              Virtual_address.Htbl.add e.Env.enumerations vaddr tbl)
          enumerations;
        if Queue.length enumerations' = 0 then
          Virtual_address.Htbl.remove e.Env.enumerate vaddr
        else Virtual_address.Htbl.replace e.Env.enumerate vaddr enumerations'

    and handle_cut vaddr e ps =
      match Virtual_address.Htbl.find e.Env.cut vaddr with
      | exception Not_found -> Some ps
      | cuts ->
        let guard = Dba.Expr.lognot
            (List.fold_left (fun e g -> Dba.Expr.logor e g)
               (List.hd cuts) (List.tl cuts)) in
        Stats.update_max_depth (Path_state.get_depth ps);
        Stats.update_inj_per_path (Path_state.get_injection_points_in_path ps);
        if (Dba.Expr.is_equal guard (Dba.Expr.constant Bitvector.zero)) then 
          begin
            Logger.result "@[<h>Directive :: simple cut %@ %a @]" Virtual_address.pp vaddr;
            let assert_fail_addr = match Sse_options.AssertFailAddress.is_set () with
            | true -> Sse_options.AssertFailAddress.get ()
            | false -> assert false
            in
            if assert_fail_addr = (Virtual_address.to_int vaddr) then Stats.add_assert_cut ();
            None
          end
        else
          begin
            match Senv.assume guard Env.main_predicate (Path_state.dual_symbolic_state ps) with
            | None ->
              Logger.result "@[<h>Directive :: cut %@ %a@]"
                Virtual_address.pp vaddr;
              let assert_fail_addr = match Sse_options.AssertFailAddress.is_set () with
              | true -> Sse_options.AssertFailAddress.get ()
              | false -> assert false
              in
              if assert_fail_addr = (Virtual_address.to_int vaddr) then Stats.add_assert_cut ();
              None
            | Some dual ->
              Logger.info
                "@[<h>Directive :: cut %@ %a with unsat condition@]"
                Virtual_address.pp vaddr;
              Some (Path_state.set_dual_symbolic_state dual ps)
          end

    and do_directives vaddr e ps =
      match handle_assumptions vaddr e ps with
      | None -> loop_aux (Env.pick_path e)
      | Some ps ->
        handle_reach vaddr e ps;
        handle_enumerate vaddr e ps;
        if Virtual_address.Htbl.length e.Env.reach = 0
        && Virtual_address.Htbl.length e.Env.enumerate = 0 then halt e
        else match handle_cut vaddr e ps with
          | None -> loop_aux (Env.pick_path e)
          | Some ps -> loop_aux (Eval.go e ps) in
    try loop_aux ps with Halt -> halt e



  let interval_or_set_to_cond expr is =
    let open Parse_helpers.Initialization in
    match is with
    | Singleton _ -> assert false
    | Signed_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.sle e1 expr) (Dba.Expr.sle expr e2)
    | Unsigned_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.ule e1 expr) (Dba.Expr.ule expr e2)
    | Set l ->
      match l with
      | [] -> assert false
      | a :: b ->
        let f = Dba.Expr.equal expr in
        List.fold_left (fun acc e -> Dba.Expr.logor acc @@ f e) (f a) b


  let initialize_state e ps =
    let to_load = LoadSections.get () and ro_load = LoadROSections.get () in
    let addr_size = Kernel_options.Machine.word_size ()
    and img = Kernel_functions.get_img () in
    let ps =
      if not ro_load && Basic_types.String.Set.is_empty to_load then
        ps
      else
        let state = Path_state.dual_symbolic_state ps in
        let state = Array.fold_left (fun state section ->
            let name = Loader.Section.name section in
            if ro_load && Loader.Section.
                            (has_flag Loader_types.Read section &&
                             not (has_flag Loader_types.Write section))
            || Basic_types.String.Set.mem name to_load then
              let addr = Bitvector.of_int ~size:addr_size
                  (Loader.Section.pos section).virt
              and size = (Loader.Section.size section).virt in
              Logger.info "Load section %s" name;
              Senv.load_from ~addr size img Senv.Both state 
            else state
          ) state @@ Loader.Img.sections img in
        Path_state.set_dual_symbolic_state state ps in

    let ps =
      let cli_counters = Visit_address_counter.get () in
      match cli_counters with
      | [] -> ps
      | cs ->
        Logger.info "Found some address counters ... great";
        let m =
          let open! Virtual_address in
          List.fold_left
            (fun m c -> Map.add c.Address_counter.address c m) Map.empty cs in
        Path_state.set_address_counters m ps
    in
    let set ps init =
      let open Parse_helpers.Initialization in
      match init.operation with
      | Mem_load (addr, size) ->
        Path_state.with_init_mem_at ~addr ~size (Kernel_functions.get_img ()) ps
      | Universal lval ->
        begin
          match Dba_types.LValue.name_of lval with
          | Some name ->
            let symb = Path_state.dual_symbolic_state ps in
            let size = Dba.LValue.size_of lval in
            let symb = Senv.fresh name size Senv.Both symb in
            Path_state.set_dual_symbolic_state symb ps
          | None -> ps
        end
      | Assumption cond ->
        let ps = Path_state.set_dual_symbolic_state
          (Option.get (Senv.assume cond Env.main_predicate (Path_state.dual_symbolic_state ps) )) ps in
        Path_state.set_dual_symbolic_state
          (Option.get (Senv.assume cond Env.secondary_predicate (Path_state.dual_symbolic_state ps) )) ps
      | Assignment (lval, rval, _) ->
        match rval with
        | Singleton rv ->
          Eval.assignment ~lvalue:lval ~rvalue:rv (Path_state.block_index ps) ps e
        | x ->
          let ps = Eval.nondet ~lvalue:lval ps in
          let e = Dba.LValue.to_expr lval in
          let cond = interval_or_set_to_cond e x in
          let ps = Path_state.set_dual_symbolic_state
          (Option.get (Senv.assume cond Env.main_predicate(Path_state.dual_symbolic_state ps))) ps in
          let ps = Path_state.set_dual_symbolic_state
          (Option.get (Senv.assume cond Env.secondary_predicate(Path_state.dual_symbolic_state ps))) ps in
          ps in
    let ps = match Sse_options.MemoryFile.get_opt () with
      | None -> ps
      | Some filename ->
        if not (Sys.file_exists filename) then
          Logger.fatal "Cannot find sse configuration file %s" filename;
        let initials =
          Logger.debug "Reading initialization from %s" filename;
          let parser = Parser.initialization
          and lexer = Lexer.token in
          Parse_utils.read_file ~parser ~lexer ~filename
        in List.fold_left set ps initials in
    let ps = match ScriptFile.get_opt () with
      | None -> ps
      | Some filename ->
        if not (Sys.file_exists filename) then
          Logger.fatal "Cannot find sse configuration file %s" filename;
        let script =
          Logger.debug "Reading script from %s" filename;
          let module M : Astbuilder.Env = struct
            let wordsize = Kernel_options.Machine.word_size ()
            let endianness = Kernel_options.Machine.endianness ()
            let tbl = Basic_types.String.Htbl.create 128;;
            Array.iter (fun s ->
                let name = Loader.Symbol.name s in
                Basic_types.String.Htbl.replace tbl
                  name
                  (Dba.LValue.v
                     { Dba.name; size = wordsize; info = Dba.VarTag.empty }))
              (Loader.Img.symbols (Kernel_functions.get_img ()));;
            List.iter (fun (name, var) ->
                Basic_types.String.Htbl.add tbl name var)
              (Isa_helper.get_defs ());;
            let lookup name size =
              try Basic_types.String.Htbl.find tbl name
              with Not_found ->
                if size = -1 then
                  Logger.fatal "size is missing for variable %s" name;
                let bitsize = Size.Bit.create size in
                let var = Dba.LValue.var ~bitsize name in
                Basic_types.String.Htbl.add tbl name var;
                var
          end in
          let module P = Sse_parser.Make (M) in
          let parser = P.script
          and lexer = Sse_lexer.token in
          Parse_utils.read_file ~parser ~lexer ~filename
        in
        List.fold_left (fun ps -> function
            | Script.Init i -> set ps i
            | Script.Goal g -> Env.add_directive e g; ps
            | Script.Stub (a, b) ->
              let addr = Dba_utils.Expr.eval_from_img
                  (Kernel_functions.get_img ()) a in
              C.add_inst e.Env.cfg addr (Instruction.of_dba_block addr b);
              ps) ps script in
    ps

  let do_sse ~filename =
    let level = 3 in
    Logger.debug ~level "Running SSE on %s" filename;
    let entrypoint = get_entry_point () in
    Logger.debug ~level "Starting from %a" Virtual_address.pp entrypoint;
    let e = Env.create entrypoint in
    Logger.debug ~level "@[<h>Initializing SSE goals ...@]";
    Env.update_from_cli e;
    Logger.debug ~level "Driver set ...";
    Logger.debug ~level "Creating symbolic store ...";
    Logger.debug ~level "Fault model %s" (Sse_options.FaultModel.get ());
    Logger.debug ~level "Check nb faults at %s" (Sse_options.WhereCheckNbFaults.get());
    (* Create empty symbolic state *)
    let state = Senv.empty_dual in

    (* Fault injection initialization *)
    (* Get end of analysis ~ main ret address *)
    let main_return_address = match Sse_options.GoalAddress.is_set () with
    | true -> Virtual_address.create (Sse_options.GoalAddress.get ())
    | false -> assert false
    in
    let reach_condition = ref Dba.Expr.one in
    let (fault_model, state) = match Sse_options.FaultModel.is_default () with
    | false -> (
      if String.compare (Sse_options.WhereCheckNonDetConstrain.get ()) "idem" == 0 
        then Sse_options.WhereCheckNonDetConstrain.set (Sse_options.WhereCheckNbFaults.get ());
      if String.compare (Sse_options.WhereCheckNbFaults.get ()) (Sse_options.WhereCheckNonDetConstrain.get ()) != 0 then Sse_options.Logger.warning "Max fault check not at the same level as fault constrain check";
      (* Initialization NUMBER_FAULTS = 0<32> *)
      let state = Senv.assign "NUMBER_FAULTS" (Dba.Expr.constant (Bitvector.of_int ~size:32 0)) Env.secondary_predicate state in
      (* Initialization FAULT_CONSTRAINS = 1<1> *)
      let state = Senv.assign "FAULT_CONSTRAINS" Dba.Expr.one Env.secondary_predicate state in
      (* Initialize MAX_FAULTS<32> from config *)
      let max_faults = Sse_options.MaxFaults.get () in
      let state = Senv.assign "MAX_FAULTS" (Dba.Expr.constant (Bitvector.of_int ~size:32 max_faults)) Env.secondary_predicate state in
      (* Add assume NUMBER_FAULTS <= MAX_FAULTS *)
      let limit_count = Dba.Expr.ule (Dba.Expr.var "NUMBER_FAULTS" 32) (Dba.Expr.var "MAX_FAULTS" 32) in
      reach_condition := Dba.Expr.binary Dba.Binary_op.And (!reach_condition) limit_count;
      (* Add assume FAULT_CONSTRAINS = 1<1>*)
      let non_det_constrain = Dba.Expr.equal (Dba.Expr.var "FAULT_CONSTRAINS" 1) Dba.Expr.one in
      reach_condition := Dba.Expr.logand (!reach_condition) non_det_constrain;
      (true, state)
    )
    | true -> (false, state)
    in
    (* Add reach according to configuration *)
    if Sse_options.SingleSolution.get () then (
      (* Add reach once *)
      Env.add_reach e main_return_address Directive.Count.once !reach_condition
    ) else (
      (* Add reach * and cut *)
      Env.add_reach e main_return_address Directive.Count.unlimited !reach_condition;
      Env.add_cut e main_return_address Dba.Expr.one
    );
    (* Add assert fail cut *)
    let _ = match (Sse_options.AssertFailAddress.is_set (), fault_model) with
    | (true,_) -> (
      let assert_fail_addr = Virtual_address.create (Sse_options.AssertFailAddress.get ()) in
      Env.add_cut e assert_fail_addr Dba.Expr.one
    )
    | (false, true) -> assert false
    | (false, false) -> ()
    in
    (* Create path state *)
    let ps = initialize_state e
        (Path_state.create state (Env.decode e entrypoint)) in
    (* Deactivate fault injection if needed *)
    let ps = if (Sse_options.FaultModel.is_default () || (Sse_options.MaxFaults.get () == 0) || Sse_options.InjectionMethod.is_default ()) then Path_state.reaching_max_faults ps else ps in
    Logger.debug ~level "Initialization done ...";
    let print_stats_at_end () = print_stats () in
    at_exit print_stats_at_end;
    loop_until ~halt e ps

  let do_sse_from_core ~filename ~coredump =
    let level = 3 in
    Logger.debug ~level "Running SSE on %s" filename;
    match Kernel_functions.get_img () with
    | Loader.Dump _ | Loader.PE _ -> Logger.fatal "Binary is not an ELF file."
    | Loader.ELF img as img' ->
      let hdr = Loader_elf.Img.header img in
      if hdr.Loader_elf.Ehdr.kind <> Loader_elf.Ehdr.ET.EXEC then
        raise (Errors.not_yet_implemented "Non executable file (e.g. shared)");
      let ss = Array.fold_left (fun ss section ->
          let open Loader_elf in
          let hdr = Section.header section in
          if Shdr.SHF.is hdr.Shdr.flags Shdr.SHF.WRITE = false then
            let addr = Bitvector.of_int
                ~size:(Kernel_options.Machine.word_size ())
                hdr.Shdr.addr in
            Senv.load_from ~addr hdr.Shdr.size img' Senv.Both ss
          else ss)
          (Senv.empty_dual) (Loader_elf.Img.sections img) in
      let img = Loader.load_file coredump in
      let img' = match img with
        | Loader.ELF img ->
          let hdr = Loader_elf.Img.header img in
          if hdr.Loader_elf.Ehdr.kind <> Loader_elf.Ehdr.ET.CORE then
            Logger.fatal "%s is not an CORE file." coredump;
          img
        | _ -> Logger.fatal "%s is not an ELF file." coredump in
      let entrypoint, initializations = Isa_helper.core img' in
      Logger.debug ~level "Starting from %a" Virtual_address.pp entrypoint;
      let e = Env.create entrypoint in
      Logger.debug ~level "@[<h>Initializing SSE goals ...@]";
      Env.update_from_cli e;
      Logger.debug ~level "Driver set ...";
      Logger.debug ~level "Creating symbolic store ...";
      let ss = List.fold_left (fun ss (rvalue, lvalue) ->
          match rvalue with
          | Dba.LValue.Var { name; _ } ->
            Senv.assign name lvalue Senv.Both ss
          | Dba.LValue.Restrict _ -> assert false (* TODO ? *)
          | Dba.LValue.Store _ -> assert false (* TODO *))
          ss initializations in
      let ss = Array.fold_left (fun ss section ->
          let open Loader_elf in
          let hdr = Section.header section in
          if hdr.Shdr.kind = Shdr.SHT.PROGBITS then
            let addr = Bitvector.of_int
                ~size:(Kernel_options.Machine.word_size ())
                hdr.Shdr.addr in
            Senv.load_from ~addr hdr.Shdr.size img Senv.Both ss
          else ss)
          ss (Loader_elf.Img.sections img') in
      let ps = initialize_state e
          (Path_state.create ss (Env.decode e entrypoint)) in
      Logger.debug ~level "Initialization done ...";
      loop_until ~halt e ps

  let start () =
    let filename = Kernel_options.ExecFile.get () in
    match FromCore.get_opt () with
    | None -> do_sse ~filename
    | Some coredump -> do_sse_from_core ~filename ~coredump
end

let run () =
  if Sse_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    let (module W) =
      match Search_heuristics.get () with
      | Dfs -> (module Dfs:WORKLIST)
      | Bfs -> (module Bfs:WORKLIST)
      | Nurs ->
        let seed =
          match Seed.get_opt () with
          | Some s -> s
          | None ->
            let v = Utils.random_max_int () in
            Logger.info "Random search seed is %d" v;
            Seed.set v;
            v
        in
        Random.init seed;
        (module Nurs:WORKLIST)
    in let module S = Env_make(W) in S.start ()


let _ =
  Cli.Boot.enlist ~name:"SSE" ~f:run
