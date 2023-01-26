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

(* Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
    Kernel_functions.get_img ()
    |> Loader.Img.entry
    |> Virtual_address.create

module Bv_set = struct
  (* one could use Set.Make(Bitvector) but lists are simpler
   * and might even be faster
  *)
  type t = Bitvector.t list

  let union l1 l2 = l1 @ l2 |> List.sort_uniq Bitvector.compare

  let cardinal = List.length
end

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
      visited : int; (* Does not count the number uniquely visited paths but
                      * should ... *)
      choices : int;
      asserts_unknown : int;
      asserts_failed : int;
    }

    let empty = { visited = 0; choices = 0; asserts_unknown = 0; asserts_failed = 0; }
    let add_visited s = { s with visited = s.visited + 1 }
    let add_choice  s = { s with choices = s.choices + 1 }
    let add_assert_unknown s = { s with asserts_unknown = s.asserts_unknown + 1 }
    let add_assert_failed  s = { s with asserts_failed = s.asserts_failed + 1 }

    let pp ppf s =
      Format.fprintf ppf
        "@[<v 0>\
         @[<h>selections %d@]@,\
         @[<h>choice %d@]@,\
         @[<h>unknown assertions %d@]@,\
         @[<h>failed assertions %d@]\
         @]"
        s.visited
        s.choices
        s.asserts_unknown
        s.asserts_failed
    ;;

    module R = struct
      let value = ref empty
      let add_visited () = value := add_visited !value
      let add_choice  () = value := add_choice !value
      let add_assert_unknown () = value := add_assert_unknown !value
      let add_assert_failed  () = value := add_assert_failed !value
      let pp ppf () = pp ppf !value
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
      enumerations     : Bv_set.t Etbl.t Virtual_address.Htbl.t;
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
          pick_one worklist
        | exception Not_found ->
          Logger.info "Empty path worklist: halting ...";
          raise Not_found
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

    let check_sat ps =
      let sat_status, _ = Sse_smt.Solver.check_satistifiability ps in
      sat_status

    let rec pick_path e =
      Stats.add_visited ();
      check_sat_or_choose_another e (choose e)

    and check_sat_or_choose_another e ps =
      let freq = Solver_call_frequency.get () in
      let n = Path_state.solver_calls ps + 1 in
      if n = freq then
        match check_sat ps with
        | Formula.SAT ->
          (* After a call, let's get back to the initial state *)
          Path_state.reset_solver_calls ps
        | _ -> pick_path e
      else Path_state.incr_solver_calls ps

    let pick_alternative at ~consequent ~alternative e =
      Stats.add_choice ();
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
      else
        Logger.info "Goal unreachable from@ %a" Path_state.pp_loc path_state

  end

  let halt e =
    Logger.info
      "@[<v 0>\
       @[<v 2>SMT queries@,%a@]@,\
       @[<v 2>Exploration@,%a@]@,\
       @]"
      Sse_smt.Query_stats.pp ()
      Stats.pp ();
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
      match jump_target with
      | Dba.JInner idx ->
        Some (Path_state.set_block_index idx le)
      | Dba.JOuter addr ->
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

    (* lvalue <- e *)
    let assignment ~lvalue ~rvalue ps =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
      *)
      Sse_smt.Translate.assignment lvalue rvalue ps

    let nondet ~lvalue ps =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
      *)
      Sse_smt.Translate.nondet lvalue ps

    let ite ~condition ~jump_target ~local_target e ps =
      (* expand path with assert condition and go to jump_target *)
      (* push path with assert not condition and go to local_target *)
      let addr = Path_state.virtual_address ps in
      let raw_cond, state = Sse_smt.Translate.expr' ps condition in
      let condition = Formula.(mk_bv_equal raw_cond mk_bv_one) in
      let alternate_condition = Formula.mk_bl_not condition in
      let consequent =
        Path_state.add_assertion condition state |> static_jump e ~jump_target
      in
      let alternate_state = Path_state.branch state in
      let alternative =
        Some (
          Path_state.add_assertion alternate_condition alternate_state
          |> Sse_types.Path_state.set_block_index local_target)
      in Env.pick_alternative addr ~consequent ~alternative e


    let dynamic_jump ~jump_expr e ps =
      let img = Kernel_functions.get_img () in
      let target, path_state = Sse_smt.Translate.expr' ps jump_expr in
      let n = Sse_options.JumpEnumDepth.get () in
      let concretes, path_state =
        Sse_smt.Solver.enumerate_values n target path_state in
      let with_bv bv =
        let condition = Formula.(mk_bv_equal (mk_bv_cst bv) target)
        and addr = Virtual_address.of_bitvector bv in
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %a@ could lead to@ %a@]"
          Path_state.pp_loc path_state Bitvector.pp_hex bv;
        let address = Virtual_address.to_int addr in
        if C.mem_vertex_a e.Env.cfg addr <> None then
          let ps = Path_state.add_assertion condition path_state in
          let ps = goto e ps addr in
          Env.add_if_good e ps
        else
          let section = Loader_utils.find_section_by_address ~address img in
          match section with
          | Some s when
              Loader.Section.has_flag Loader_types.Read s &&
              Loader.Section.has_flag Loader_types.Exec s ->
            let ps = Path_state.add_assertion condition path_state in
            let ps = goto e ps addr in
            Env.add_if_good e ps
          | Some _ | None ->
            Logger.warning
              "@[<hov>Dynamic jump@ %a@ could have led to invalid address %a;@ \
               skipping@]"
              Path_state.pp_loc path_state Bitvector.pp_hex bv
      in
      List.iter with_bv concretes;
      Env.pick_path e

    let skip instruction idx ps =
      Logger.info ~level:3 "Skipping %a"
        Dba_printer.Ascii.pp_instruction instruction;
      Path_state.set_block_index idx ps

  let check f cond idx ps =
    let cond, symbols =
      Sse_smt.Translate.expr (Path_state.symbolic_state ps) cond in
    let ps = Path_state.set_symbolic_state symbols ps in
    let ps = f cond ps in
    Path_state.set_block_index idx ps

  let assertion test state =
    ignore @@ Sse_smt.Solver.with_solver
      (Path_state.add_assertion
         Formula.(mk_bv_equal test Formula.mk_bv_zero) state)
      (fun session -> match Solver.Session.check_sat session with
         | Formula.UNSAT -> ()
         | Formula.UNKNOWN | Formula.TIMEOUT ->
           Sse_options.Logger.error
             "@[Assertion got unknown status %@ %a@]"
             Path_state.pp_loc state;
           Stats.add_assert_unknown ()
         | Formula.SAT ->
           let model = Solver.Session.get_model session in
           Sse_options.Logger.error
             "@[<v 2> Assertion failed %@ %a@ %a@]"
             Path_state.pp_loc state
             Smt_model.pp model;
           Stats.add_assert_failed ());
    Path_state.add_assertion
      Formula.(mk_bv_equal test Formula.mk_bv_one) state


    (* If comment is activated, this will add, for every formula entry, a
       comment about where it comes from.

       This can be usefule to debug the path predicate translation.  *)
    let maybe_add_comment ps =
      if Sse_options.Comment.get () then
        let syst = Path_state.symbolic_state ps in
        let comment =
          Print_utils.string_from_pp
            (Formula_pp.pp_as_comment Path_state.pp_loc) ps
        in
        let symbolic_state = Sse_symbolic.State.comment comment syst in
        Path_state.set_symbolic_state symbolic_state ps
      else ps


    let go e ps =
      let ps = maybe_add_comment ps in
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc ps;
      match Path_state.dba_instruction ps with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
        let ps = assignment ~lvalue ~rvalue ps in
        Path_state.set_block_index idx ps
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
        check assertion condition idx ps
      | Dba.Instr.Assume _
      | Dba.Instr.NondetAssume _
      | Dba.Instr.Malloc _
      | Dba.Instr.Free _
      | Dba.Instr.Print _ as dba_instruction ->
        let msg =
          Format.asprintf "%a" Dba_printer.Ascii.pp_instruction
            dba_instruction in
        Errors.not_yet_implemented msg
  end

  let sat_status p =
    fst @@ Sse_smt.Solver.check_satistifiability p

  let loop_until ~halt e ps =
    let last_vaddr = ref (Path_state.virtual_address ps) in

    let rec loop_aux ps =
      let vaddr = Path_state.virtual_address ps in
      if vaddr <> !last_vaddr then begin
        Logger.debug ~level:2 "%@%a %a"
          Virtual_address.pp vaddr
          Mnemonic.pp  (Instruction.mnemonic (Path_state.inst ps))
        ;
        last_vaddr := vaddr;
        do_directives vaddr e ps
      end
      (* When the last virtual addresse has not changed, when are still in the
         same DBA block, hence no user action can have been performed.
         So, we just continue.
      *)
      else loop_aux (Eval.go e ps)

    and do_directives vaddr e ps =
      (* handle assumptions *)
      let ps = match Virtual_address.Htbl.find e.Env.assume vaddr with
        | exception Not_found -> ps
        | assumptions ->
          let assumption =
            List.fold_left (fun e a -> Dba.Expr.logand e a)
              (List.hd assumptions) (List.tl assumptions) in
          let ps =
            if Comment.get () then
              (* add comment *)
              let comment =
                Print_utils.string_from_pp
                  (Formula_pp.pp_as_comment
                     (fun ppf e ->
                        Format.fprintf ppf
                          "@[<h>user constraint : assume %a@]"
                          Dba_printer.EICAscii.pp_bl_term e)) assumption in
              Path_state.set_symbolic_state
                (Sse_symbolic.State.comment comment
                   (Path_state.symbolic_state ps)) ps
            else ps in
          Logger.debug "Assume %a %@ %a"
            Dba_printer.Ascii.pp_bl_term assumption Virtual_address.pp vaddr;
          Sse_smt.Translate.assume assumption ps in
      (* handle enumerations *)
      begin match Virtual_address.Htbl.find e.Env.enumerate vaddr with
        | exception Not_found -> ()
        | enumerations ->
          let enumerations' = Queue.create () in
          Queue.iter (fun (k, x) ->
              let x', ps' = Sse_smt.Translate.expr' ps x in
              let bvs =
                try Etbl.find
                      (Virtual_address.Htbl.find e.Env.enumerations vaddr) x
                with Not_found -> [] in
              let n = Bv_set.cardinal bvs in
              let ss' = List.fold_left (fun ss bv ->
                  Sse_symbolic.State.constrain
                    (Formula.mk_bv_distinct x' (Formula.mk_bv_cst bv)) ss)
                  (Path_state.symbolic_state ps)
                  bvs in
              let bvs', _ = Sse_smt.Solver.enumerate_values k x'
                  (Path_state.set_symbolic_state ss' ps') in
              let n' = Bv_set.cardinal bvs' in
              let bvs = Bv_set.union bvs bvs' in
              Logger.result
                "@[<hov 0>Directive :: enumerate@ \
                 possible values (%d) for %a %@ %a:@ @[<hov 0>%a@]@]"
                (n + n')
                Dba_printer.EICAscii.pp_bl_term x
                Virtual_address.pp vaddr
                (Print_utils.pp_list ~sep:",@ " Bitvector.pp) bvs;
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
      end;
      (* handle reachs *)
      let discard = ref false in (* HOTFIX because we do not have
                                    the fucking invariant that the
                                    path under study is satisfiable... *)
      begin match Virtual_address.Htbl.find e.Env.reach vaddr with
        | exception Not_found -> ()
        | reachs ->
          let reachs' = Queue.create () in
          Queue.iter (fun ((k, guard) as r) ->
              Logger.debug "Reach";
              let guard', ps' = Sse_smt.Translate.expr' ps guard in
              let ps' = Path_state.add_assertion
                  (Formula.mk_bv_equal guard' Formula.mk_bv_one) ps' in
              match sat_status ps' with
              | Formula.SAT ->
                let k' = Directive.Count.decr k in
                Logger.result
                  "@[<h>Directive :: reached address %a (%a to go)@]"
                  Virtual_address.pp vaddr Directive.Count.pp k';
                (* Seriously?
                   We have to call twice the solver to get a model ????? *)
                begin match Sse_smt.Solver.get_model ps' with
                  | Some m ->
                    Logger.result "@[<v 0>Model %@ %a@ %a@]"
                      Virtual_address.pp vaddr
                      Smt_model.pp m;
                  | None ->
                    Logger.result
                      "@[<h>No model %@ %a@]" Virtual_address.pp vaddr
                end;
                begin match k' with
                  | Directive.Count.Count 0 -> ()
                  | _ -> Queue.add (k', guard) reachs'
                end;
              | Formula.UNSAT when guard <> Dba.Expr.one ->
                begin
                  match sat_status ps with
                  | Formula.SAT ->
                    Logger.info
                      "@[<h>Directive :: reached address %a \
                       with unsat condition (%a to go)@]"
                      Virtual_address.pp vaddr Directive.Count.pp k;
                    Queue.add r reachs'
                  | status ->
                    Logger.warning
                      "@[<h>Directive :: reach \
                       reached address %a with %a \
                       (still %a to go)@]"
                      Virtual_address.pp vaddr Formula_pp.pp_status status
                      Directive.Count.pp k;
                    Queue.add r reachs';
                    discard := true
                end
              | status ->
                Logger.warning
                  "@[<h>Directive :: reach \
                   reached address %a with %a \
                   (still %a to go)@]"
                  Virtual_address.pp vaddr Formula_pp.pp_status status
                  Directive.Count.pp k;
                Queue.add r reachs';
                discard := true) reachs;
          if Queue.length reachs' = 0 then
            Virtual_address.Htbl.remove e.Env.reach vaddr
          else Virtual_address.Htbl.replace e.Env.reach vaddr reachs'
      end;
      (* handle cuts *)
      begin match Virtual_address.Htbl.find e.Env.cut vaddr with
        | exception Not_found -> ()
        | cuts ->
          let guard =
            List.fold_left (fun e g -> Dba.Expr.logor e g)
              (List.hd cuts) (List.tl cuts) in
          discard := !discard || guard = Dba.Expr.one;
          if not !discard then
            let guard', ps' = Sse_smt.Translate.expr' ps guard in
            let ps' = Path_state.add_assertion
                (Formula.mk_bv_equal guard' Formula.mk_bv_one) ps' in
            match sat_status ps' with
            | Formula.SAT ->
              Logger.result "@[<h>Directive :: cut %@ %a@]"
                Virtual_address.pp vaddr
            | Formula.UNSAT ->
              begin
                match sat_status ps with
                | Formula.SAT ->
                  Logger.info
                    "@[<h>Directive :: cut %@ %a with unsat condition@]"
                    Virtual_address.pp vaddr
                  | status ->
                    Logger.warning
                      "@[<h>Directive :: cut %@ %a with %a@]"
                      Virtual_address.pp vaddr
                      Formula_pp.pp_status status;
                    discard := true
                end
              | status ->
                Logger.warning
                  "@[<h>Directive :: cut %@ %a with %a@]"
                  Virtual_address.pp vaddr
                  Formula_pp.pp_status status;
                discard := true
      end;
      if Virtual_address.Htbl.length e.Env.reach = 0
      && Virtual_address.Htbl.length e.Env.enumerate = 0 then halt e
      else if !discard then loop_aux (Env.pick_path e)
      else loop_aux (Eval.go e ps)
    in
    try loop_aux ps with Not_found -> halt e



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
            Path_state.with_init_mem_at ps ~addr ~size
          | Universal lval ->
            begin
              match Dba_types.LValue.name_of lval with
              | Some name ->
                let symb = Path_state.symbolic_state ps in
                let size = Dba.LValue.size_of lval in
                let sort = Formula.BvSort size in
                let symb = Sse_symbolic.State.declare ~wild:true
                    name sort symb in
                Path_state.set_symbolic_state symb ps
              | None -> ps
            end
          | Assumption cond ->
            Sse_smt.Translate.assume cond ps
          | Assignment (lval, rval, naming_hint) ->
            let wild = not init.controlled in
            match rval with
            | Singleton rv ->
              Sse_smt.Translate.assignment ~wild lval rv ps
            | x ->
              let state = Sse_smt.Translate.nondet ~wild ?naming_hint lval ps in
              let e = Dba.LValue.to_expr lval in
              let cond = interval_or_set_to_cond e x in
              Sse_smt.Translate.assume cond state in
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
    let ss = Sse_symbolic.State.create () in
    let ps = initialize_state e
        (Path_state.create ss (Env.decode e entrypoint)) in
    Logger.debug ~level "Initialization done ...";
    loop_until ~halt e ps


  let start () =
    let filename = Kernel_options.ExecFile.get () in
    do_sse ~filename

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
