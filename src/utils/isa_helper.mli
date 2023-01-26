val get_defs : unit -> (string * Dba.LValue.t) list
(** [get_defs ()]
    returns the list of known entities for the current architecture
    (see {!val:Kernel_options.Machine.isa}).
    Meaningfull for x86 only for now. *)

val core : Loader_elf.Img.t ->
  Virtual_address.t * (Dba.LValue.t * Dba.Expr.t) list
(** [core img]
    read and translate the content of [NT_PRSTATUS] note into
    entrypoint and initialisation values. *)
