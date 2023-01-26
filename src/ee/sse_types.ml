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

open Format

module Script = struct
  type t =
    | Init of Parse_helpers.Initialization.t
    | Goal of Directive.t
    | Stub of Dba.Expr.t * Dhunk.t
end


module C = struct
  include Instr_cfg.Make
      (struct include Basic_types.Int let hash i = i let equal = (==) end)
end

module Path_state = struct

  type t  = {
    id : int ;   (* Unique identifier for the path *)
    depth : int; (* Current depth of traversal *)
    depth_bcht : int; (* depth computed by number of branchments passed*)
    injection_points_in_path : int; (* Number of injection in this paths *)
    solver_calls : int;
    path : Virtual_address.t list;
    (* Sequence of virtual addresses for this path *)
    dual_symbolic_state : Senv.dual; (* Current symbolic state alongside with faulted state*)
    predicate_flips : (Virtual_address.t * char) list; (* list of adresses where normal predicate was faulted to continue *) 
    instruction : Instruction.t; (* Current instruction *)
    block_index : int;           (* Current index into DBA block of current
                                    instruction *)

    (* How many times we can pass at this address before cut *)
    address_counters : Sse_options.Address_counter.t Virtual_address.Map.t;
    max_faults_reached : bool;
    first_faults : int;
    under_approx_injections : int;
  }


  let gen_id = ref (-1)

  let id st = st.id
  let dual_symbolic_state st = st.dual_symbolic_state
  let block_index st = st.block_index
  let inst ps = ps.instruction
  let get_depth ps = ps.depth
  let add_to_depth_bcht ps = { ps with depth_bcht = ps.depth_bcht +1}
  let get_depth_bcht ps = ps.depth_bcht

  let add_injection_points_in_path ps = { ps with injection_points_in_path = ps.injection_points_in_path + 1}
  let get_injection_points_in_path ps = ps.injection_points_in_path
  let counter vaddr st =
    match Virtual_address.Map.find vaddr st.address_counters with
    | c -> Some c
    | exception Not_found -> None

  let set_counter vaddr c st =
    { st with address_counters =
                Virtual_address.Map.add vaddr c st.address_counters }

  let paths_created () = !gen_id

  let solver_calls p = p.solver_calls
  let incr_solver_calls p = { p with solver_calls = p.solver_calls + 1; }
  let reset_solver_calls p = { p with solver_calls = 0; }

  let dba_instruction st =
    let block = st.instruction.Instruction.dba_block in
    Dhunk.inst block st.block_index |> Utils.unsafe_get_opt

  let set_block_index block_index st = { st with block_index }

  let set_instruction instruction st =
    { st with block_index = 0; instruction; depth = st.depth + 1;
              path = Instruction.address instruction :: st.path }

  let set_dual_symbolic_state dual_symbolic_state st = { st with dual_symbolic_state }

  (* let update_dual faulted dual update_fct = Senv.update_dual faulted dual update_fct *)

  let set_address_counters address_counters st = { st with address_counters }

  let reaching_max_faults st = { st with max_faults_reached = true}

  let is_max_fault_reached st = st.max_faults_reached

  let add_to_first_faults st = { st with first_faults = st.first_faults +1}

  let get_first_faults st = st.first_faults

  let fault_already_happened st = 
    st.first_faults > 0
  
  let add_under_approx_injections st = { st with under_approx_injections = st.under_approx_injections +1}
  let get_under_approx_injections st = st.under_approx_injections
  let max_under_approx_injections_reached st = st.under_approx_injections >= Sse_options.MaxFaults.get ()

  let get_predicate_flips st = st.predicate_flips
  let add_test_inversion_direct_flips st addr = { st with predicate_flips = addr :: st.predicate_flips }

  let with_init_mem_at ~addr ~size img ?(is_faulted=Senv.Both) path_state =
      let value = Bitvector.value_of addr in
      let bvsize = Kernel_options.Machine.word_size () in
      let addr = Bitvector.create value bvsize in
      let dual = Senv.load_from ~addr size img is_faulted path_state.dual_symbolic_state in
      { path_state with dual_symbolic_state = dual }

  let virtual_address st =
    let open Instruction in
    st.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address in
    Dba_types.Caddress.reid caddress st.block_index

  let current_statement st =
    dba_instruction st
    |> Dba_types.Statement.create (location st)

  let pp_loc ppf st =
    let dba_instruction = dba_instruction st in
    let vaddress = virtual_address st in
    fprintf ppf "@[<hov>(%a, %d)@ :@ @[%a@]@]"
      Virtual_address.pp vaddress
      st.block_index
      Dba_printer.Ascii.pp_instruction dba_instruction

  let pp_path ppf ps =
    Format.pp_open_vbox ppf 0;
    List.iter (fun v -> Virtual_address.pp ppf v; Format.pp_print_space ppf ())
      (List.rev ps.path);
    Format.pp_close_box ppf ()

  let is_depth_ok ps =
    let max_depth = Sse_options.MaxDepth.get () in
    ps.depth < max_depth
  ;;

  let may_lead_to_goal = is_depth_ok ;;
  (* One might elements from the CFG here *)

  let fault_normal_state ps et = 
    { ps with 
      dual_symbolic_state = Senv.fault_normal_state ps.dual_symbolic_state ;
      predicate_flips = (ps.instruction.address, et) :: ps.predicate_flips
    }

  let create
      ?(depth=0)
      ?(address_counters=Virtual_address.Map.empty)
      ?(block_index=0) dual_symbolic_state instruction =
    assert(
      block_index >= 0 &&
      block_index <=
      Dhunk.length instruction.Instruction.dba_block);
    incr gen_id;
    { id = !gen_id; address_counters;
      depth; depth_bcht = 0;
      path = [];
      block_index; dual_symbolic_state; instruction;
      injection_points_in_path = 0;
      solver_calls = 0; (* At path creation we have never called a solver *)
      max_faults_reached = false;
      first_faults = 0;
      under_approx_injections = 0;
      predicate_flips = [];
    }

  let branch p =
    incr gen_id;
    { p with id = !gen_id }

end

(* Both the stack and the queue below are functional implementations of these
   data structures
*)

module type WORKLIST = sig
  type t
  val push : Path_state.t -> t -> t
  val pop  : t -> Path_state.t * t
  val singleton : Path_state.t -> t
  val length : t -> int
  val is_empty : t -> bool
  val empty : t
end

module W_stack:WORKLIST = Fstack.Make(Path_state)

module W_queue:WORKLIST = struct
  type t = Path_state.t Sequence.t

  let length = Sequence.length
  let is_empty q = Sequence.length q = 0
  let empty = Sequence.empty
  let push p q = Sequence.push_back p q
  let pop q =
    match Sequence.peek_front q with
    | None -> raise Not_found
    | Some v ->
      match Sequence.pop_front q with
      | None -> assert false
      | Some seq -> v, seq

  let singleton p = push p empty
end

module Random_heap: WORKLIST = struct
  (* This is actually a fairly classical heap.
     The priority added to the date is just generated at random.
  *)

  module T = struct
    type t = {
      priority : int;
      state : Path_state.t
    }

    let compare t1 t2 = compare t1.priority t2.priority

    let create ~priority ~state = {priority; state;}
  end

  module H = Worklist.Make(T)

  type t = H.t

  let gen_priority () = Utils.random_max_int ()

  let length = H.length
  let is_empty = H.is_empty
  let empty = H.empty

  let push p h =
    let priority = gen_priority () in
    H.add (T.create ~priority ~state:p) h

  let pop h =
    let e, h' = H.pop h in
    e.T.state, h'

  let singleton p = push p empty

end

module Dfs : WORKLIST = W_stack

module Bfs : WORKLIST = W_queue

module Nurs : WORKLIST = Random_heap
