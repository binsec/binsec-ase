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

module Script : sig
  type t =
    | Init of Parse_helpers.Initialization.t
    | Goal of Directive.t
    | Stub of Dba.Expr.t * Dhunk.t
end

module C : Instr_cfg.S with type addr = Virtual_address.t
                        and type inst = Instruction.t
                        and type symb = Basic_types.Int.t


module Path_state : sig
  type t

  val create :
    ?depth:int ->
    ?address_counters:
      Sse_options.Address_counter.t Virtual_address.Map.t ->
    ?block_index:int ->
    Senv.dual -> Instruction.t ->
    t

  val branch : t -> t

  (** {2 Accessors} *)

  val dba_instruction : t -> Dba.Instr.t
  val current_statement : t -> Dba_types.Statement.t
  val virtual_address : t -> Virtual_address.t
  val location : t -> Dba_types.Caddress.t

  val dual_symbolic_state : t -> Senv.dual

  val block_index : t -> int
  val id : t -> int
  val solver_calls : t -> int
  val paths_created : unit -> int
  val may_lead_to_goal : t -> bool
  val inst : t -> Instruction.t
  val get_depth : t -> int
  val add_to_depth_bcht : t -> t
  val get_depth_bcht : t -> int
  val add_injection_points_in_path : t -> t
  val get_injection_points_in_path : t -> int

  val counter : Virtual_address.t -> t -> Sse_options.Address_counter.t option

  (** {2 Modifiers} *)

  val set_counter :
    Virtual_address.t  -> Sse_options.Address_counter.t -> t -> t

  val set_block_index : int -> t -> t
  val set_instruction : Instruction.t -> t -> t
  (** increase depth and extend path *)

  val set_dual_symbolic_state : Senv.dual -> t -> t

  val incr_solver_calls : t -> t
  val reset_solver_calls : t -> t

  val set_address_counters :
    Sse_options.Address_counter.t Virtual_address.Map.t -> t -> t
  val reaching_max_faults : t -> t
  val is_max_fault_reached : t -> bool

  val add_to_first_faults : t -> t
  val get_first_faults : t -> int
  val fault_already_happened : t -> bool
  val add_under_approx_injections : t -> t
  val max_under_approx_injections_reached : t -> bool
  val get_under_approx_injections : t -> int

  val get_predicate_flips : t -> (Virtual_address.t * char) list
  val add_test_inversion_direct_flips : t -> (Virtual_address.t * char) -> t

  val fault_normal_state : t -> char -> t

  val with_init_mem_at: addr:Bitvector.t -> size:int -> Loader.Img.t -> ?is_faulted:Senv.which_dual -> t -> t

  (** {2 Printers} *)

  val pp_loc : Format.formatter -> t -> unit
  val pp_path : Format.formatter -> t -> unit

end

module type WORKLIST = sig
  type t
  val push : Path_state.t -> t -> t
  val pop  : t -> Path_state.t * t
  val singleton : Path_state.t -> t
  val length : t -> int
  val is_empty : t -> bool
  val empty : t
end

module Dfs : WORKLIST

module Bfs : WORKLIST

module Nurs : WORKLIST
(** Non uniformed randomized search heuristics *)
