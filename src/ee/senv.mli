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
(** Statistics *)

module Query_stats : sig
  type t
  val pp : Format.formatter -> unit -> unit
end

(** Symbolic state *)
type dual
type which_dual = Normal | Faulted | Both

type t

val empty_dual : dual

val update_dual : which_dual -> dual -> (t -> t) -> dual

val assume    : Dba.Expr.t -> ?such_that:Dba.Expr.t -> ?fault_check:Dba.Expr.t -> which_dual -> dual -> dual option
val print_contraints  : t -> unit
val split_on  : Dba.Expr.t -> ?n:int -> ?except: Bitvector.t list -> which_dual -> dual ->
  (Bitvector.t * dual) list
val concrete_eval  : Dba.Expr.t -> which_dual -> dual -> Bitvector.t

val fault_normal_state : dual -> dual
val fresh     : string -> int -> which_dual -> dual -> dual
val assign    : string -> Dba.Expr.t -> ?first_fault_happened:bool -> which_dual -> dual -> dual
val write     : addr: Dba.Expr.t -> Dba.Expr.t -> Machine.endianness -> ?first_fault_happened:bool -> which_dual -> dual -> dual
val load_from : addr: Bitvector.t -> int -> Loader.Img.t -> which_dual -> dual -> dual

val pp_normal : Format.formatter -> dual -> unit
val pp_faulted : Format.formatter -> dual -> unit
val pp_dual : Format.formatter -> dual -> which_dual -> unit