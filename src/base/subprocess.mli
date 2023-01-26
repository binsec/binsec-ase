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

(** A subprocess *)
type t

(**
   [spawn ~pdeathsig command]
   spawns the specified command in a new process.
   The command is searched in the path.

   When given, the signal [pdeathsig] will be delivered to the
   child process if the current thread dies (implemented only on Linux).
   The process must be closed via {!val:close}.
*)
val spawn : ?pdeathsig: int -> string array -> t

(**
   [pid t]
   returns the process identifier.
*)
val pid    : t -> int

(**
   [stdin t]
   returns the channel bound to the subprocess stdin.
*)
val stdin  : t -> out_channel

(**
   [stdout t]
   returns the channel bound to the subprocess stdout.
*)
val stdout : t -> in_channel

(**
   [stderr t]
   returns the channel bound to the subprocess stderr.
*)
val stderr : t -> in_channel

(**
   [close t]
   closes a process created with {!val:spawn}.

   Close {!val:stdin}, {!val:stdout} and {!val:stderr} channels, wait for the
   associated command to terminate and return its termination status.
*)
val close : t -> Unix.process_status
