module type Env = sig
  val lookup: string -> int -> Dba.LValue.t
  val wordsize: int
  val endianness: Machine.endianness
end
