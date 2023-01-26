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

let byte_size = (Basic_types.Constants.bytesize :> int)

type size = int

type 'a interval = 'a Interval.t = { lo: 'a; hi: 'a }

type endianness = Machine.endianness =
  | LittleEndian
  | BigEndian

let pp_endiannesss ppf = function
  | LittleEndian -> Format.pp_print_char ppf 'L'
  | BigEndian    -> Format.pp_print_char ppf 'B'

type unary = U and binary = B
type _ operator =
  | Not          : unary operator
  | Sext         : size -> unary operator
  | Uext         : size -> unary operator
  | Restrict     : int interval -> unary operator
  | Plus         : binary operator
  | Minus        : _ operator
  | Mul          : binary operator
  | Udiv         : binary operator   (* Corresponds to *)
  | Umod         : binary operator   (* the truncated division *)
  | Sdiv         : binary operator   (* of C99 and most *)
  | Smod         : binary operator   (* processors *)
  | Or           : binary operator
  | And          : binary operator
  | Xor          : binary operator
  | Concat       : binary operator
  | Lsl          : binary operator
  | Lsr          : binary operator
  | Asr          : binary operator
  | Rol          : binary operator
  | Ror          : binary operator
  | Eq           : binary operator
  | Diff         : binary operator
  | Ule          : binary operator
  | Ult          : binary operator
  | Uge          : binary operator
  | Ugt          : binary operator
  | Sle          : binary operator
  | Slt          : binary operator
  | Sge          : binary operator
  | Sgt          : binary operator

let pp_unop ppf = function
  | Minus                        -> Format.pp_print_char ppf '-'
  | Not                          -> Format.pp_print_char ppf '!'
  | Sext n                       -> Format.fprintf ppf "sext +%d" n
  | Uext n                       -> Format.fprintf ppf "uext +%d" n
  | Restrict { lo; hi } ->
    if lo = hi then Format.fprintf ppf "select %d" lo
    else Format.fprintf ppf "select <%d .. %d>" hi lo

let pp_binop ppf = function
  | Plus                         -> Format.pp_print_char ppf '+'
  | Minus                        -> Format.pp_print_char ppf '-'
  | Mul                          -> Format.pp_print_char ppf '*'
  | Udiv                         -> Format.pp_print_string ppf "udiv"
  | Sdiv                         -> Format.pp_print_string ppf "sdiv"
  | Umod                         -> Format.pp_print_string ppf "umod"
  | Smod                         -> Format.pp_print_string ppf "smod"
  | Or                           -> Format.pp_print_string ppf "or"
  | And                          -> Format.pp_print_string ppf "and"
  | Xor                          -> Format.pp_print_string ppf "xor"
  | Concat                       -> Format.pp_print_string ppf "::"
  | Lsl                          -> Format.pp_print_string ppf "lsl"
  | Lsr                          -> Format.pp_print_string ppf "lsr"
  | Asr                          -> Format.pp_print_string ppf "asr"
  | Rol                          -> Format.pp_print_string ppf "rol"
  | Ror                          -> Format.pp_print_string ppf "ror"
  | Eq                           -> Format.pp_print_char ppf '='
  | Diff                         -> Format.pp_print_string ppf "<>"
  | Ule                          -> Format.pp_print_string ppf "ule"
  | Ult                          -> Format.pp_print_string ppf "ult"
  | Uge                          -> Format.pp_print_string ppf "uge"
  | Ugt                          -> Format.pp_print_string ppf "ugt"
  | Sle                          -> Format.pp_print_string ppf "sle"
  | Slt                          -> Format.pp_print_string ppf "slt"
  | Sge                          -> Format.pp_print_string ppf "sge"
  | Sgt                          -> Format.pp_print_string ppf "sgt"

type cst = C and loc = L and exp = E
type (_, _, 'a, 'b) t =
  | Var : {
      hash  : int;
      size  : size;
      name  : string;
      label : 'a;
    } -> (exp, _, 'a, _) t
  | Load : {
      hash  : int;
      len   : size;
      dir   : endianness;
      addr  : (exp, exp, 'a, 'b) t;
      label : 'b;
    } -> (exp, _, 'a, 'b) t
  | Cst : (Bitvector.t [@unboxed]) -> (_, exp, _, _) t
  | Unary : {
      hash  : int;
      size  : size;
      f     : unary operator;
      x     : (exp, exp, 'a, 'b) t;
    } -> (exp, exp, 'a, 'b) t
  | Binary : {
      hash  : int;
      size  : size;
      f     : binary operator;
      x     : (exp, exp, 'a, 'b) t;
      y     : (exp, exp, 'a, 'b) t;
    } -> (exp, exp, 'a, 'b) t
  | Ite : {
      hash  : int;
      size  : size;
      c     : (exp, exp, 'a, 'b) t;
      t     : (exp, exp, 'a, 'b) t;
      e     : (exp, exp, 'a, 'b) t
    } -> (exp, exp, 'a, 'b) t

let rec pp : type k l. Format.formatter -> (k, l, 'a, 'b) t -> unit =
  fun ppf -> function
  | Var { name; size; _ } -> Format.fprintf ppf "%s<%d>" name size
  | Load { len; dir; addr; _ } ->
    Format.fprintf ppf "%@[%a]%d%a" pp addr len pp_endiannesss dir
  | Cst bv -> Bitvector.pp_hex_or_bin ppf bv
  | Unary { f; x; _ } -> Format.fprintf ppf "@[(%a %a)@]" pp_unop f pp x
  | Binary { f; x; y; _ } ->
    Format.fprintf ppf "@[(%a %a %a)@]" pp_binop f pp x pp y
  | Ite { c; t; e; _ } ->
    Format.fprintf ppf "@[(%a ? %a : %a)@]" pp c pp t pp e

let to_string t = Format.asprintf "%a" pp t
let abort t = raise (Invalid_argument (to_string t))

let hash : type k l. (k, l, _, _) t -> int = function
  | Cst      bv        -> Bitvector.hash bv
  | Load   { hash; _ } -> hash
  | Var    { hash; _ } -> hash
  | Unary  { hash; _ } -> hash
  | Binary { hash; _ } -> hash
  | Ite    { hash; _ } -> hash

let sizeof : type k l. (k, l, _, _) t -> int = function
  | Cst      bv        -> Bitvector.size_of bv
  | Load   { len; _  } -> byte_size * len
  | Var    { size; _ } -> size
  | Unary  { size; _ } -> size
  | Binary { f=Eq | Diff | Ule | Ult | Uge | Ugt | Sle | Slt | Sge | Sgt; _ } ->
    1
  | Binary { size; _ } -> size
  | Ite    { size; _ } -> size

type ('a, 'b) any = Term : (_, _, 'a, 'b) t -> ('a, 'b) any [@@unboxed]

let to_exp t = match Term t with
  | Term (Var _    as v) -> v
  | Term (Load _   as l) -> l
  | Term (Cst _    as c) -> c
  | Term (Unary _  as u) -> u
  | Term (Binary _ as b) -> b
  | Term (Ite _    as i) -> i

let to_loc t = match Term t with
  | Term (Var _  as v) -> Some v
  | Term (Load _ as l) -> Some l
  | _ -> None

let to_loc_exn t = match Term t with
  | Term (Var _  as v) -> v
  | Term (Load _ as l) -> l
  | _ -> abort t

let to_cst t = match Term t with
  | Term (Cst _ as c) -> Some c
  | _ -> None

let to_cst_exn t = match Term t with
  | Term (Cst _ as c) -> c
  | _ -> abort t

module Bv = struct
  include Bitvector

  let unary f x = match f with
    | Not                 -> lognot x
    | Minus               -> neg x
    | Uext n              -> extend x (size_of x + n)
    | Sext n              -> extend_signed x (size_of x + n)
    | Restrict it         -> extract x it
  let binary f x y =
    let n = size_of x in
    if f <> Concat && n <> size_of y then
      abort (Binary { f; x=Cst x; y=Cst y; size=n; hash=0 })
    else match f with
      | Plus   -> add x y
      | Minus  -> sub x y
      | Mul    -> mul x y
      | Udiv   -> udiv x y
      | Umod   -> umod x y
      | Sdiv   -> sdiv x y
      | Smod   -> smod x y

      | Or     -> logor x y
      | And    -> logand x y
      | Xor    -> logxor x y

      | Eq     -> of_bool (equal x y)
      | Diff   -> of_bool (diff x y)
      | Ule    -> of_bool (ule x y)
      | Ult    -> of_bool (ult x y)
      | Uge    -> of_bool (uge x y)
      | Ugt    -> of_bool (ugt x y)
      | Sle    -> of_bool (sle x y)
      | Slt    -> of_bool (slt x y)
      | Sge    -> of_bool (sge x y)
      | Sgt    -> of_bool (sgt x y)

      | Lsl    -> shift_left x (to_uint y)
      | Lsr    -> shift_right x (to_uint y)
      | Asr    -> shift_right_signed x (to_uint y)
      | Rol    -> rotate_left x (to_uint y)
      | Ror    -> rotate_right x (to_uint y)
      | Concat -> append x y

  let extract ~lo ~hi x = extract x { lo; hi }
end

module type S = sig
  type a and b

  type nonrec size = size

  type nonrec 'a interval = 'a interval = { lo: 'a; hi: 'a }

  type nonrec endianness = endianness =
  | LittleEndian
  | BigEndian

  type 'a op = 'a operator =
    | Not          : unary op
    | Sext         : size -> unary op
    | Uext         : size -> unary op
    | Restrict     : int interval -> unary op
    | Plus         : binary op
    | Minus        : _ op
    | Mul          : binary op
    | Udiv         : binary op   (* Corresponds to *)
    | Umod         : binary op   (* the truncated division *)
    | Sdiv         : binary op   (* of C99 and most *)
    | Smod         : binary op   (* processors *)
    | Or           : binary op
    | And          : binary op
    | Xor          : binary op
    | Concat       : binary op
    | Lsl          : binary op
    | Lsr          : binary op
    | Asr          : binary op
    | Rol          : binary op
    | Ror          : binary op
    | Eq           : binary op
    | Diff         : binary op
    | Ule          : binary op
    | Ult          : binary op
    | Uge          : binary op
    | Ugt          : binary op
    | Sle          : binary op
    | Slt          : binary op
    | Sge          : binary op
    | Sgt          : binary op

  type ('k, 'l, 'a, 'b) term = ('k, 'l, 'a, 'b) t = private
    | Var : {
        hash  : int;
        size  : size;
        name  : string;
        label : 'a;
      } -> (exp, _, 'a, _) term
    | Load : {
        hash  : int;
        len   : size;
        dir   : endianness;
        addr  : (exp, exp, 'a, 'b) term;
        label : 'b;
      } -> (exp, _, 'a, 'b) term
    | Cst : (Bitvector.t [@unboxed]) -> (_, exp, _, _) term
    | Unary : {
        hash  : int;
        size  : size;
        f     : unary operator;
        x     : (exp, exp, 'a, 'b) term;
      } -> (exp, exp, 'a, 'b) term
    | Binary : {
        hash  : int;
        size  : size;
        f     : binary operator;
        x     : (exp, exp, 'a, 'b) term;
        y     : (exp, exp, 'a, 'b) term;
      } -> (exp, exp, 'a, 'b) term
    | Ite : {
        hash  : int;
        size  : size;
        c     : (exp, exp, 'a, 'b) term;
        t     : (exp, exp, 'a, 'b) term;
        e     : (exp, exp, 'a, 'b) term
      } -> (exp, exp, 'a, 'b) term

  type t = (exp, exp, a, b) term

  (** {2 Constructors} *)

  val var : string -> size -> a -> t
  (** [var name bitsize label] *)

  val load : size -> endianness -> t -> b -> t
  (** [load nbytes endianness addr label] *)

  val constant : Bitvector.t -> t
  (** [constant bv] creates a constant expression from the bitvector [bv].
  *)

  val unary : unary op -> t -> t
  (** [unary f x] creates a unary application of [f] on [x].
  *)

  val binary : binary op ->  t -> t -> t
  (** [binary f x y] creates a binary application of [f] on [x] and [y].
  *)

  val ite : t -> t -> t -> t
  (** [ite c t e] creates an if-then-else expression [c] ? [t] : [e].
  *)

  val uminus             : t -> t

  val add                : t -> t -> t
  val sub                : t -> t -> t
  val mul                : t -> t -> t
  val smod               : t -> t -> t
  val umod               : t -> t -> t
  val udiv               : t -> t -> t
  val sdiv               : t -> t -> t
  val append             : t -> t -> t

  val equal              : t -> t -> t
  val diff               : t -> t -> t
  val ule                : t -> t -> t
  val uge                : t -> t -> t
  val ult                : t -> t -> t
  val ugt                : t -> t -> t
  val sle                : t -> t -> t
  val sge                : t -> t -> t
  val slt                : t -> t -> t
  val sgt                : t -> t -> t

  val logand             : t -> t -> t
  val logor              : t -> t -> t
  val lognot             : t -> t
  val logxor             : t -> t -> t

  val shift_left         : t -> t -> t
  val shift_right        : t -> t -> t
  val shift_right_signed : t -> t -> t
  (** [shift_(left|right) e q] shifts expression [e] by quantity [q], padding
      with zeroes *)

  val rotate_left        : t -> t -> t
  val rotate_right       : t -> t -> t
  (** [rotate_(left|right) e q] rotates expression [e] by quantity [q] *)

  val sext : size -> t -> t
  (** [sext sz e] performs a signed extension of expression [e] to size [sz] *)

  val uext : size -> t -> t
  (** [uext sz e] performs an unsigned extension expression [e] to size [sz] *)

  val restrict : lo:int -> hi:int -> t -> t
  (** [restrict lo hi e] creates [Dba.ExprUnary(Restrict(lo, hi), e)] if
      [hi >= lo && lo >=0] .
  *)

  val bit_restrict : int -> t -> t
  (** [bit_restrict o e] is [restrict o o e] *)

  (** {3 Specific constants }*)

  val zeros : int -> t
  (** [zeros n] creates a constant expression of value 0 with length [n] *)

  val ones : int -> t
  (** [ones n] creates a constant expression of value 1 with length [n].
      I.e.; it has (n - 1) zeros in binary.
  *)

  val one    : t
  val zero   : t

  val addi   : t -> int -> t

  (** {4 Utils} **)

  val hash : t -> int
  (** [hash t] returns the hash of [t] in constant time.
  *)

  val is_equal : t -> t -> bool

  val compare : t -> t -> int

  val sizeof : t -> size
  (** [sizeof t] returns the bit size of [t] in constant time.
  *)

  val map : ('a -> a) -> ('b -> b) -> (_, _, 'a, 'b) term -> t
end


module Make (A : Sigs.HASHABLE) (B : Sigs.HASHABLE) :
  S with type a := A.t and type b := B.t =
struct

 type nonrec size = size

  type nonrec 'a interval = 'a interval = { lo: 'a; hi: 'a }

  type nonrec endianness = endianness =
  | LittleEndian
  | BigEndian

  type 'a op = 'a operator =
    | Not          : unary op
    | Sext         : size -> unary op
    | Uext         : size -> unary op
    | Restrict     : int interval -> unary op
    | Plus         : binary op
    | Minus        : _ op
    | Mul          : binary op
    | Udiv         : binary op   (* Corresponds to *)
    | Umod         : binary op   (* the truncated division *)
    | Sdiv         : binary op   (* of C99 and most *)
    | Smod         : binary op   (* processors *)
    | Or           : binary op
    | And          : binary op
    | Xor          : binary op
    | Concat       : binary op
    | Lsl          : binary op
    | Lsr          : binary op
    | Asr          : binary op
    | Rol          : binary op
    | Ror          : binary op
    | Eq           : binary op
    | Diff         : binary op
    | Ule          : binary op
    | Ult          : binary op
    | Uge          : binary op
    | Ugt          : binary op
    | Sle          : binary op
    | Slt          : binary op
    | Sge          : binary op
    | Sgt          : binary op

  type ('k, 'l, 'a, 'b) term = ('k, 'l, 'a, 'b) t =
    | Var : {
        hash  : int;
        size  : size;
        name  : string;
        label : 'a;
      } -> (exp, _, 'a, 'b) term
    | Load : {
        hash  : int;
        len   : size;
        dir   : endianness;
        addr  : (exp, exp, 'a, 'b) term;
        label : 'b;
      } -> (exp, _, 'a, 'b) term
    | Cst : Bitvector.t -> (_, exp, _, _) term
    | Unary : {
        hash  : int;
        size  : size;
        f     : unary operator;
        x     : (exp, exp, 'a, 'b) term;
      } -> (exp, exp, 'a, 'b) term
    | Binary : {
        hash  : int;
        size  : size;
        f     : binary operator;
        x     : (exp, exp, 'a, 'b) term;
        y     : (exp, exp, 'a, 'b) term;
      } -> (exp, exp, 'a, 'b) term
    | Ite : {
        hash  : int;
        size  : size;
        c     : (exp, exp, 'a, 'b) term;
        t     : (exp, exp, 'a, 'b) term;
        e     : (exp, exp, 'a, 'b) term
      } -> (exp, exp, 'a, 'b) term

  type t = (exp, exp, A.t, B.t) term

  let hash = hash

  let rec is_equal t t' =
    t == t' || hash t = hash t' && match t, t' with
      | Cst bv, Cst bv' -> Bv.equal bv bv'
      | Var { name; size; label; _ },
        Var { name=name'; size=size'; label=label'; _ } ->
        size = size' && name = name' && A.equal label label'
      | Load { len; dir; addr; label; _ },
        Load { len=len'; dir=dir'; addr=addr'; label=label'; _ } ->
        len = len' && dir = dir' && is_equal addr addr' && B.equal label label'
      | Unary { f; x; _ }, Unary { f=f'; x=x'; _ } ->
        f = f' && is_equal x x'
      | Binary { f; x; y; _ }, Binary { f=f'; x=x'; y=y'; _ } ->
        f = f' && is_equal x x' && is_equal y y'
      | Ite { c; t; e; _ }, Ite { c=c'; t=t'; e=e'; _ } ->
        is_equal c c' && is_equal t t' && is_equal e e'
      | _, _ -> false

  let compare =
    let rec order t t' =
      if t == t' then 0 else match t, t' with
        | Cst bv, Cst bv' -> Bv.compare bv bv'
        | Cst _, Load _ -> -1
        | Cst _, Unary _ -> -1
        | Cst _, Binary _ -> -1
        | Cst _, Ite _ -> -1
        | Cst _, Var _ -> -1
        | Load _, Cst _ -> 1
        | Load { len; dir; addr; label; _ },
          Load { len=len'; dir=dir'; addr=addr'; label=label'; _ } ->
          let d = len - len' in
          if d <> 0 then d else
            let d = compare dir dir' in
            if d <> 0 then d else
              let d = order addr addr' in
              if d <> 0 then d else B.compare label label'
        | Load _, Unary _ -> -1
        | Load _, Binary _ -> -1
        | Load _, Ite _ -> -1
        | Load _, Var _ -> -1
        | Unary _, Cst _ -> 1
        | Unary _, Load _ -> 1
        | Unary { f; x; size; _ }, Unary { f=f'; x=x'; size=size'; _ } ->
          let d = compare f f' in
          if d <> 0 then d else
            let d = size - size' in
            if d <> 0 then d else order x x'
        | Unary _, Binary _ -> -1
        | Unary _, Ite _ -> -1
        | Unary _, Var _ -> -1
        | Binary _, Cst _ -> 1
        | Binary _, Load _ -> 1
        | Binary _, Unary _ -> 1
        | Binary { hash; f; x; y; size; _ },
          Binary { hash=hash'; f=f'; x=x'; y=y'; size=size'; _ } ->
          let d = compare f f' in
          if d <> 0 then d else
            let d = size - size' in
            if d <> 0 then d else
              let d = hash - hash' in
              if d <> 0 then d else
                let d = order x x' in
                if d <> 0 then d else order y y'
        | Binary _, Ite _ -> -1
        | Binary _, Var _ -> -1
        | Ite _, Cst _ -> 1
        | Ite _, Load _ -> 1
        | Ite _, Unary _ -> 1
        | Ite _, Binary _ -> 1
        | Ite { hash; c; t; e; size; _ },
          Ite { hash=hash'; c=c'; t=t'; e=e'; size=size'; _ } ->
          let d = size - size' in
          if d <> 0 then d else
            let d = hash - hash' in
            if d <> 0 then d else
              let d = order c c' in
              if d <> 0 then d else
                let d = order t t' in
                if d <> 0 then d else order e e'
        | Ite _, Var _ -> -1
        | Var _, Cst _ -> 1
        | Var _, Load _ -> 1
        | Var _, Unary _ -> 1
        | Var _, Binary _ -> 1
        | Var _, Ite _ -> 1
        | Var { name; size; label; _ },
          Var { name=name'; size=size'; label=label'; _ } ->
          let d = size - size' in
          if d <> 0 then d else
            let d = String.compare name name' in
            if d <> 0 then d else A.compare label label' in
    order

  let sizeof = sizeof

  let var name size label =
    Var {
      name; size; label;
      hash=Hash.(return @@ fold_int (fold_string (seed 0x88206212) name) size)
    }
  let load len dir addr label =
    Load {
      len; dir; addr; label;
      hash=Hash.(return @@ fold_int
                   (fold_int (seed 0xc4dba348) len) (hash addr))
    }
  let constant bv = Cst bv
  let mk_unary f x =
    let size = match f with
      | Uext n | Sext n -> n + sizeof x
      | Restrict { lo; hi } -> hi - lo + 1
      | Not | Minus -> sizeof x in
    Unary {
      f; x; size;
      hash=Hash.(return @@
                 fold_int (fold_int (seed 0xec9576a) (Hashtbl.hash f)) (hash x))
    }
  let mk_binary f x y =
    let size = match f with
      | Concat -> sizeof x + sizeof y
      | _ -> sizeof x in
    Binary {
      f; x; y; size;
      hash=Hash.(return @@
                 fold_int (fold_int (fold_int (seed 0x4b8498a0)
                                       (Hashtbl.hash f)) (hash x)) (hash y))
    }
  let mk_ite c t e =
    Ite {
      c; t; e;
      size=sizeof t;
      hash=Hash.(return @@
                 fold_int (fold_int (fold_int (seed 0x8bfe92b2)
                                       (hash c)) (hash t)) (hash e))
    }

  let zeros n = Cst (Bv.zeros n)
  let ones n = Cst (Bv.ones n)

  let one = Cst Bv.one
  let zero = Cst Bv.zero

  let match_compare_op op = match op with
    | Eq -> (fun x y -> Bv.equal x y)
    | Diff -> (fun x y -> not (Bv.equal x y))
    | Ule -> (fun x y -> Bv.ule x y)
    | Uge -> (fun x y -> Bv.uge x y)
    | Sle -> (fun x y -> Bv.sle x y)
    | Sge -> (fun x y -> Bv.sge x y)
    | Ult -> (fun x y -> Bv.ult x y)
    | Ugt -> (fun x y -> Bv.ugt x y)
    | Slt -> (fun x y -> Bv.slt x y)
    | Sgt -> (fun x y -> Bv.sgt x y)
    | _ -> assert false

  let rec unary f x = match f, x with

    (* safety pattern guard *)
    (* TODO: move outside of the rec pattern if the rewriter is trusted *)
    | Uext n, _
    | Sext n, _ when n < 0 -> abort @@ mk_unary f x
    | Restrict { lo; hi }, t when  lo < 0 || hi < lo || sizeof t <= hi ->
      abort @@ mk_unary f x

    (* constant folding *)
    | _, Cst bv -> constant (Bv.unary f bv)

    (* identity *)
    | Sext 0, x
    | Uext 0, x -> x
    | Restrict { lo=0; hi }, x when hi = sizeof x - 1 -> x
    | Not, Unary { f=Not; x; _ } -> x
    | Minus, Unary { f=Minus; x; _ } -> x
    | Minus, x when sizeof x = 1 -> x

    (* inversion *)
    | Minus, Binary { f=Minus; x; y; _ } -> binary Minus y x
    | Not, Binary { f=Eq; x; y; _ } -> binary Diff x y
    | Not, Binary { f=Diff; x; y; _ } -> binary Eq x y
    | Not, Binary { f=Ule; x; y; _ } -> binary Ugt x y
    | Not, Binary { f=Ult; x; y; _ } -> binary Uge x y
    | Not, Binary { f=Uge; x; y; _ } -> binary Ult x y
    | Not, Binary { f=Ugt; x; y; _ } -> binary Ule x y
    | Not, Binary { f=Sle; x; y; _ } -> binary Sgt x y
    | Not, Binary { f=Slt; x; y; _ } -> binary Sge x y
    | Not, Binary { f=Sge; x; y; _ } -> binary Slt x y
    | Not, Binary { f=Sgt; x; y; _ } -> binary Sle x y
    (* TODO: more to come like de morgan's law, etc.. *)

    (* combining *)
    | Uext n, Unary { f=Uext p; x; _ }
    | Sext n, Unary { f=Uext p; x; _ } -> unary (Uext (n + p)) x
    | Sext n, Unary { f=Sext p; x; _ } -> unary (Sext (n + p)) x
    | Restrict { lo; hi }, Unary { f=Restrict { lo=lo'; _ }; x; _ } ->
      unary (Restrict { lo=lo' + lo; hi=lo' + hi }) x

    (* revert -- extract only inside the initial term t  *)
    | Restrict { hi; _ }, Unary { f=Uext _; x; _ }
    | Restrict { hi; _ }, Unary { f=Sext _; x; _ } when hi < sizeof x ->
      unary f x
    (* absorbing element -- extract only the inserted bits *)
    | Restrict { lo; hi }, Unary { f=Uext _; x; _ } when sizeof x <= lo ->
      zeros (hi - lo + 1)
    | Restrict { lo; hi }, Unary { f=Sext _; x; _ } when sizeof x <= lo ->
      unary (Sext (hi - lo))
        (unary (Restrict { lo=sizeof x - 1; hi=sizeof x - 1 }) x)
    (* reorder -- extension on top *)
    | Restrict { lo; hi }, Unary { f=Uext _; x; _ } ->
      unary (Uext (hi - sizeof x + 1))
        (unary (Restrict { lo; hi=sizeof x - 1 }) x)
    | Restrict { lo; hi }, Unary { f=Sext _; x; _ } ->
      unary (Sext (hi - sizeof x + 1))
        (unary (Restrict { lo; hi=sizeof x - 1 }) x)

    (* absorbing element -- extract only the inserted bits *)
    | Restrict { lo; hi }, Binary { f=Lsl; y=Cst bv; _ }
      when hi < Bv.to_uint bv ->
      zeros (hi - lo + 1)
    | Restrict { lo; hi }, Binary { f=Lsr; x; y=Cst bv; _ }
      when sizeof x - Bv.to_uint bv <= lo ->
      zeros (hi - lo + 1)
    | Restrict { lo; hi }, Binary { f=Asr; x; y=Cst bv; _ }
      when sizeof x - Bv.to_uint bv - 1 <= lo ->
      unary (Sext (hi - lo))
        (unary (Restrict { lo=sizeof x - 1; hi=sizeof x - 1 }) x)
    (* combining -- extract is still inside the initial term t *)
    | Restrict { lo; hi }, Binary { f=Lsl; x; y=Cst bv; _ }
      when Bv.to_uint bv <= lo ->
      unary
        (Restrict { lo=lo - Bv.to_uint bv; hi=hi - Bv.to_uint bv }) x
    | Restrict { lo; hi }, Binary { f=Lsr; x; y=Cst bv; _ }
    | Restrict { lo; hi }, Binary { f=Asr; x; y=Cst bv; _ }
      when hi + Bv.to_uint bv < sizeof x ->
      unary (Restrict { lo=lo + Bv.to_uint bv; hi=hi + Bv.to_uint bv }) x
    (* reorder -- extension on top *)
    | Restrict { lo; hi }, Binary { f=Lsr; x; y=Cst bv; _ } ->
      unary (Uext (hi - sizeof x + Bv.to_uint bv + 1))
        (unary (Restrict { lo=lo + Bv.to_uint bv; hi=sizeof x - 1 }) x)
    | Restrict { lo; hi }, Binary { f=Asr; x; y=Cst bv; _ } ->
      unary (Sext (hi - sizeof x + Bv.to_uint bv + 1))
        (unary (Restrict { lo=lo + Bv.to_uint bv; hi=sizeof x - 1 }) x)

    (* split concatenation *)
    | Restrict { hi; _ }, Binary { f=Concat; y; _ } when hi < sizeof y ->
      unary f y
    | Restrict { lo; hi }, Binary { f=Concat; x; y; _ } when sizeof y <= lo ->
      unary (Restrict { lo=lo - sizeof y; hi=hi - sizeof y }) x
    | Restrict { hi; _ },
      Binary { f=Concat; x=Binary { f=Concat; y=z; _ }; y; _ }
      when hi < sizeof z + sizeof y ->
      unary f (binary Concat z y)
    (* TODO: more to come when term is "splitable" -- eg. t land cst *)

    (* Simplification rules ite for fault injection *)
    | Restrict {lo; hi}, Ite {c = _; t = Cst e1; e = Cst e2; _} when (lo = hi) && (Bv.is_zeros e1) && (not(Bv.get_bit e2 lo)) -> Cst Bv.zero
    | _, Ite{c; t; e; _} -> ite c (unary f t) (unary f e)

    (* default case *)
    | _, _ -> mk_unary f x

  and binary f x y = match f, x, y with

    (* safety pattern guard *)
    (* TODO: move outside of the rec pattern if the rewriter is trusted *)
    | _, _, _ when f <> Concat && sizeof x <> sizeof y ->
      abort @@ mk_binary f x y

    (* special boolean replacement *)
    | Plus, _, _
    | Minus, _, _ when sizeof x = 1 ->
      binary Xor x y

    (* constant folding *)
    | Plus, a, b when sizeof a = 1 -> binary Xor a b
    | _, Cst x, Cst y -> constant (Bv.binary f x y)
    | Plus, Binary { f=Plus; x=a; y=Cst b; _ }, Cst c ->
      binary Plus a (constant (Bv.binary Plus b c))
    | Plus, Binary { f=Minus; x=a; y=Cst b; _ }, Cst c ->
      binary Minus a (constant (Bv.binary Minus b c))
    | Minus, Binary { f=Plus; x=a; y=Cst b; _ }, Cst c ->
      binary Plus a (constant (Bv.binary Minus b c))
    | Minus, Binary { f=Minus; x=a; y=Cst b; _ }, Cst c ->
      binary Minus a (constant (Bv.binary Plus b c))
    | (Plus | Minus as f),
      Binary { f=Minus; x=Cst a; y=b; _ }, Cst c ->
      binary Minus (constant (Bv.binary f a c)) b
    | Plus, a, Cst bv when Bv.is_neg bv ->
      binary Minus a (constant (Bv.neg bv))
    | Minus, a, Cst bv when Bv.is_neg bv ->
      binary Plus a (constant (Bv.neg bv))

    (* identity *)
    | Plus,  x, Cst bv
    | Minus, x, Cst bv
    | Lsl,   x, Cst bv
    | Lsr,   x, Cst bv
    | Asr,   x, Cst bv
    | Rol,   x, Cst bv
    | Ror,   x, Cst bv
    | Xor,   x, Cst bv
    | Or,    x, Cst bv  when Bv.is_zeros bv -> x
    | Mul,   x, Cst bv
    | Udiv,  x, Cst bv
    | Sdiv,  x, Cst bv when Bv.is_ones bv -> x
    | And,   x, Cst bv when Bv.is_fill bv -> x
    | Rol,   x, Cst bv
    | Ror,   x, Cst bv when sizeof x = Bv.to_uint bv -> x


    (* absorbing element *)
    | Mul, _, Cst bv
    | And, _, Cst bv when Bv.is_zeros bv -> y
    | Or,  _, Cst bv when Bv.is_fill bv -> y
    | Lsl, Cst bv, _
    | Asr, Cst bv, _
    | Rol, Cst bv, _
    | Ror, Cst bv, _ when Bv.is_zeros bv -> x
    | Lsl, x, Cst bv
    | Lsr, x, Cst bv when sizeof x <= Bv.to_uint bv ->
      zeros (Bv.size_of bv)
    | Sdiv, Cst bv, _ when Bv.is_zeros bv -> zeros (Bv.size_of bv)

    (* elimination *)
    | And,   a, b
    | Or,    a, b when compare a b = 0 -> a
    | And, Binary { f=And; y=a; _ }, b
    | Or, Binary { f=Or; y=a; _ }, b when compare a b = 0 -> x
    | Minus, a, b
    | Xor,   a, b when compare a b = 0 -> zeros (sizeof a)
    | Minus, Binary { f=Plus; x=a; y=b; _ }, c
    | Xor, Binary { f=Xor; x=a; y=b; _ }, c when compare b c = 0 -> a

    (* factorisation *)
    | Plus, a, b when compare a b = 0 ->
      binary Mul a (constant (Bv.of_int ~size:(sizeof a) 2))

    (* commutativity -- keep sorted *)
    (* special cases for + - *)
    | Plus, a, Binary { f=Minus; x=b; y=c; _ } when compare a b < 0 ->
      binary Minus (binary Plus b a) c
    | Plus, Binary { f=Minus; x=a; y=b; _ }, c when compare b c < 0 ->
      binary Minus (binary Plus a c) b
    | Plus, Binary { f=Minus; _ }, c ->
      mk_binary Plus x c
    | Minus, Binary { f=Plus; x=a; y=b; _ }, c when compare b c < 0 ->
      binary Plus (binary Minus a c) b
    | Minus, Binary { f=Minus; x=a; y=b; _ }, c when compare b c < 0 ->
      binary Minus (binary Minus a c) b
    | Plus, Unary { f=Minus; x=a; _ }, b -> binary Minus b a
    (* generic chained *)
    | Plus, Binary { f=Plus; x=a; y=b; _ }, c
    | Mul, Binary { f=Mul; x=a; y=b; _ }, c
    | And, Binary { f=And; x=a; y=b; _ }, c
    | Or, Binary { f=Or; x=a; y=b; _ }, c
    | Xor, Binary { f=Xor; x=a; y=b; _ }, c when compare b c < 0 ->
      binary f (binary f a c) b
    | Plus, Binary { f=Plus; _ }, c
    | Mul, Binary { f=Mul; _ }, c
    | And, Binary { f=And; _ }, c
    | Or, Binary { f=Or; _ }, c
    | Xor, Binary { f=Xor; _ }, c ->
      mk_binary f x c
    (* generic dual *)
    | Plus, _, _
    | Mul, _, _
    | And, _, _
    | Or, _, _
    | Xor, _, _
    | Eq, _, _
    | Diff, _, _ when compare x y < 0 ->
      binary f y x

    (* associativity *)
    | Plus, a, Binary { f=Plus; x=b; y=c; _ }
    | Mul, a, Binary { f=Mul; x=b; y=c; _ }
    | And, a, Binary { f=And; x=b; y=c; _ }
    | Or, a, Binary { f=Or; x=b; y=c; _ }
    | Xor, a, Binary { f=Xor; x=b; y=c; _ }
    | Concat, a, Binary { f=Concat; x=b; y=c; _ } ->
      binary f (binary f a b) c

    (* trivial condition *)
    | Eq,    a, b
    | Ule,   a, b
    | Uge,   a, b
    | Sle,   a, b
    | Sge,   a, b when compare a b = 0 -> one
    | Diff,  a, b
    | Ult,   a, b
    | Ugt,   a, b
    | Slt,   a, b
    | Sgt,   a, b when compare a b = 0 -> zero

    (* condition reduction *)
    | Eq, x, Cst bv when Bv.is_one bv -> x
    | Eq, x, Cst bv when Bv.is_zero bv -> unary Not x
    | Eq, Unary { f=Uext _; x; size; _ }, Cst bv
      when not (Bv.is_zeros (Bv.extract ~lo:(sizeof x) ~hi:(size - 1) bv)) ->
      zero
    | Diff, Unary { f=Uext _; x; size; _ }, Cst bv
      when not (Bv.is_zeros (Bv.extract ~lo:(sizeof x) ~hi:(size - 1) bv)) ->
      one
    | Eq, Unary { f=Uext _; x=a; _ }, Cst bv (* see check above *)
    | Diff, Unary { f=Uext _; x=a; _ }, Cst bv -> (* see check above *)
      binary f a (constant (Bv.extract ~lo:0 ~hi:(sizeof a - 1) bv))
    | Eq, Unary { f=Not; x=a; _ }, Unary { f=Not; x=b; _ }
    | Eq, Unary { f=Minus; x=a; _ }, Unary { f=Minus; x=b; _ }
    | Diff, Unary { f=Not; x=a; _ }, Unary { f=Not; x=b; _ }
    | Diff, Unary { f=Minus; x=a; _ }, Unary { f=Minus; x=b; _ } ->
      binary f a b
    | Eq, Unary { f=Uext _; x=a; _ }, Unary { f=Uext _; x=b; _ }
    | Eq, Unary { f=Sext _; x=a; _ }, Unary { f=Sext _; x=b; _ }
    | Diff, Unary { f=Uext _; x=a; _ }, Unary { f=Uext _; x=b; _ }
    | Diff, Unary { f=Sext _; x=a; _ }, Unary { f=Sext _; x=b; _ }
      when sizeof a = sizeof b ->
      binary f a b

    (* split condition *)
    | Eq, Binary { f=Concat; x=a; y=b; _ }, Cst bv ->
      binary And
        (binary Eq a
           (constant (Bv.extract ~lo:(sizeof b) ~hi:(Bv.size_of bv - 1) bv)))
        (binary Eq b
           (constant (Bv.extract ~lo:0 ~hi:(sizeof b - 1) bv)))
    | Diff, Binary { f=Concat; x=a; y=b; _ }, Cst bv ->
      binary Or
        (binary Diff a
           (constant (Bv.extract ~lo:(sizeof b) ~hi:(Bv.size_of bv - 1) bv)))
        (binary Diff b
           (constant (Bv.extract ~lo:0 ~hi:(sizeof b - 1) bv)))
    | Eq, Binary { f=Concat; x=a; y=b; _ }, Unary { f=Uext _; x=c; _ }
      when sizeof b = sizeof c ->
      binary And
        (binary Eq a (zeros (sizeof a))) (binary Eq b c)
    | Diff, Binary { f=Concat; x=a; y=b; _ }, Unary { f=Uext _; x=c; _ }
      when sizeof b = sizeof c ->
      binary Or
        (binary Diff a (zeros (sizeof a))) (binary Diff b c)
    | Eq, Binary { f=Concat; x=a; y=b; _ }, Binary { f=Concat; x=c; y=d; _ }
      when sizeof b = sizeof d ->
      binary And (binary Eq a c) (binary Eq b d)
    | Diff, Binary { f=Concat; x=a; y=b; _ }, Binary { f=Concat; x=c; y=d; _ }
      when sizeof b = sizeof d ->
      binary Or (binary Diff a c) (binary Diff b d)
    (* TODO: possibly more to come *)

    (* inversion *)
    | Minus, a, Cst bv when Bv.is_one bv -> unary Not a
    | Xor,   a, Cst bv when Bv.is_fill bv -> unary Not a
    | Minus, a, Unary { f=Minus; x=b; _ } -> binary Plus a b
    | Minus, a, Binary { f=Plus; x=b; y=c; _ } ->
      binary Minus (binary Minus a b) c
    | Minus, a, Binary { f=Minus; x=b; y=c; _ } ->
      binary Plus (binary Minus a b) c

    (* concatenation normalization -- extension on top *)
    | Concat, Cst bv, a when Bv.is_zeros bv ->
      unary (Uext (Bv.size_of bv)) a
    | Concat, Unary { f=Uext n; x=a; _ }, b ->
      unary (Uext n) (binary Concat a b)
    | Concat, Unary { f=Sext n; x=a; _ }, b ->
      unary (Sext n) (binary Concat a b)
    (* revert -- stitch adjacent part *)
    | Concat, Unary { f=Restrict { lo; hi }; x=a; _ },
      Unary { f=Restrict { lo=lo'; hi=hi' }; x=b; _ }
      when hi' + 1 = lo && compare a b = 0 ->
      unary (Restrict { lo=lo'; hi }) a
    (* TODO: more to come like loads.. *)
    | Or, Binary { f=Lsl; x=a; y=Cst bv; _ }, Unary { f=Uext n; x=b; _ }
      when sizeof b = Bv.to_uint bv ->
      binary Concat (unary (Restrict { lo=0; hi=n - 1 }) a) b
    (* TODO!!: chain!! *)

    (* Simplification for ite opreations in fault injections *)

    | Diff, Ite { c = _; t = Cst e1; e = Cst e2 ; _ }, Cst z when not (Bv.equal e1 z) && not (Bv.equal e2 z)-> one
    | Diff, Ite { c = _; t = Cst e1; e = Cst e2 ; _ }, Cst z when (Bv.equal e1 z) && (Bv.equal e2 z)-> zero
    | Diff, Ite { c; t = Cst e1; e = Cst e2 ; _ }, Cst z when not (Bv.equal e1 z) && (Bv.equal e2 z)-> c
    | Diff, Ite { c; t = Cst e1; e = Cst e2 ; _ }, Cst z when (Bv.equal e1 z) && not (Bv.equal e2 z)-> unary Not c

    | Eq, Ite { c=  _; t = Cst e1; e = Cst e2 ; _ }, Cst z when (Bv.equal e1 z) && (Bv.equal e2 z)-> one
    | Eq, Ite { c=  _; t = Cst e1; e = Cst e2 ; _ }, Cst z when not (Bv.equal e1 z) && not (Bv.equal e2 z)-> zero
    | Eq, Ite { c; t = Cst e1; e = Cst e2 ; _ }, Cst z when not (Bv.equal e1 z) && (Bv.equal e2 z)-> unary Not c
    | Eq, Ite { c; t = Cst e1; e = Cst e2 ; _ }, Cst z when (Bv.equal e1 z) && not (Bv.equal e2 z)-> c

    | Ule as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one
    | Uge as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one
    | Sle as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one 
    | Sge as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one 
    | Ult as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one
    | Ugt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one
    | Slt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one
    | Sgt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && (match_compare_op g e2 z) -> one

    | Ule as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero
    | Uge as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero
    | Sle as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero 
    | Sge as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero 
    | Ult as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero
    | Ugt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero
    | Slt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero
    | Sgt as g, Ite{c=_; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> zero

    | Ule as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c
    | Uge as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c
    | Sle as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c 
    | Sge as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c 
    | Ult as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c
    | Ugt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c
    | Slt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c
    | Sgt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when not (match_compare_op g e1 z) && (match_compare_op g e2 z) -> c

    | Ule as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c
    | Uge as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c
    | Sle as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c 
    | Sge as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c 
    | Ult as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c
    | Ugt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c
    | Slt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c
    | Sgt as g, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z when (match_compare_op g e1 z) && not (match_compare_op g e2 z) -> unary Not c

    | Plus,  Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.add e1 z)) (Cst (Bv.add e2 z))
    | Minus, Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.sub e1 z)) (Cst (Bv.sub e2 z)) 
    | Lsl,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (binary Lsl (Cst e1) (Cst z)) (binary Lsl (Cst e2) (Cst z)) 
    | Lsr,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (binary Lsr (Cst e1) (Cst z)) (binary Lsr (Cst e2) (Cst z)) 
    | Asr,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (binary Asr (Cst e1) (Cst z)) (binary Asr (Cst e2) (Cst z)) 
    | Rol,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (binary Rol (Cst e1) (Cst z)) (binary Rol (Cst e2) (Cst z)) 
    | Ror,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (binary Ror (Cst e1) (Cst z)) (binary Ror (Cst e2) (Cst z)) 
    | And,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.logand e1 z)) (Cst (Bv.logand e2 z))
    | Xor,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.logxor e1 z)) (Cst (Bv.logxor e2 z))
    | Or,    Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.logor e1 z)) (Cst (Bv.logor e2 z))
    | Mul,   Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.mul e1 z)) (Cst (Bv.mul e2 z)) 
    | Udiv,  Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.udiv e1 z)) (Cst (Bv.udiv e2 z))
    | Sdiv,  Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.sdiv e1 z)) (Cst (Bv.sdiv e2 z))

    | Smod,  Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.smod e1 z)) (Cst (Bv.smod e2 z))
    | Umod,  Ite{c; t = Cst e1; e = Cst e2; _}, Cst z -> ite c (Cst (Bv.umod e1 z)) (Cst (Bv.umod e2 z))

    (* | And, Binary{f=Sge;x as e1;y as e2}, Binary{f=Diff;x as e3;y as e4} when is_equal e1 e3 && is_equal e2 e4 -> binary Sgt e1 e2 *)
    | And, Binary{f=Sge;x=_ as e1;y=_  as e2;_}, Binary{f=Diff;x=_  as e3;y=_  as e4;_} when is_equal e1 e3 && is_equal e2 e4 -> binary Sgt e1 e2

    (*  (b1 ? (select a b x) : e1) :: (b1 ? (select b-1 c x) : e2) *)
    | Concat, Ite{c = _ as c1; t= Unary{f=Restrict{lo=_ as lo1;hi=_ as hi1};x=_ as x1;_};e=_ as e1;_}, Ite{c = _ as c2; t= Unary{f=Restrict{lo=_ as lo2;hi=_ as hi2}; x=_ as x2;_};e=_ as e2;_} when is_equal c1 c2 && is_equal x1 x2 && lo1 = hi2+1 -> ite c1 (unary (Restrict { lo=lo2; hi=hi1 }) x1) (binary Concat e1 e2)

    (* More rules by @Soline because found in practice *)

    (* Diff (c ? 0x01 : 0x00) 0x00 => a *)
    | Diff, Ite{c; e = Cst be; t = Cst bt; _}, Cst z when Bv.is_zeros bt && Bv.is_zeros z && (not (Bv.is_zeros be)) -> c
    (* Eq (c ? 0x01 : 0x00) 0x00 => !c *)
    | Eq, Ite{c; e = Cst be; t = Cst bt; _}, Cst z when Bv.is_zeros bt && Bv.is_zeros z && (not (Bv.is_zeros be)) -> unary Not c
    (* Eq (And a b) c => And (Eq a c) (Eq b c) *)
    | Eq, Binary{f=And;x;y;_}, Cst z when Bv.size_of z = 1-> binary And (binary Eq x (Cst z)) (binary Eq y (Cst z))

    (* default case *)
    | _, _, _ -> mk_binary f x y

  and ite c t e = match c, t, e with
    | _, _, _ when sizeof c <> 1 || sizeof t <> sizeof e ->
      abort @@ mk_ite c t e
    | Cst bv, t, _ when Bv.is_one bv -> t
    | Cst bv, _, e when Bv.is_zero bv -> e
    | _, t, e when compare t e = 0 -> t
    | c, Cst bv, Cst bv' when Bv.is_fill bv && Bv.is_zeros bv' ->
      unary (Sext (Bv.size_of bv - 1)) c
    | c, Cst bv, Cst bv' when Bv.is_zeros bv && Bv.is_fill bv' ->
      unary (Sext (Bv.size_of bv - 1)) (unary Not c)
    | Unary { f=Not; x=c; _ }, t, e -> ite c e t
    | _, _, _ -> mk_ite c t e


  let lognot t = unary Not t
  let uminus t = unary Minus t
  let sext n t = unary (Sext (n - sizeof t)) t
  let uext n t = unary (Uext (n - sizeof t)) t
  let restrict ~lo ~hi t = unary (Restrict { lo; hi }) t
  let bit_restrict i t = restrict ~lo:i ~hi:i t
  let add t t'    = binary Plus t t'
  let sub t t'    = binary Minus t t'
  let mul t t'    = binary Mul t t'
  let smod t t'   = binary Smod t t'
  let umod t t'   = binary Umod t t'
  let udiv t t'   = binary Udiv t t'
  let sdiv t t'   = binary Sdiv t t'
  let logor t t'  = binary Or t t'
  let logxor t t' = binary Xor t t'
  let logand t t' = binary And t t'
  let equal t t'  = binary Eq t t'
  let diff t t'   = binary Diff t t'
  let ule t t'    = binary Ule t t'
  let sle t t'    = binary Sle t t'
  let ult t t'    = binary Ult t t'
  let slt t t'    = binary Slt t t'
  let uge t t'    = binary Uge t t'
  let sge t t'    = binary Sge t t'
  let ugt t t'    = binary Ugt t t'
  let sgt t t'    = binary Sgt t t'
  let append t t' = binary Concat t t'
  let shift_left t t' = binary Lsl t t'
  let shift_right t t' = binary Lsr t t'
  let shift_right_signed t t' = binary Asr t t'
  let rotate_left t t' = binary Rol t t'
  let rotate_right t t' = binary Ror t t'

  let addi x y = binary Plus x (constant (Bv.of_int ~size:(sizeof x) y))

  let rec map :
    type k l a b . (a -> A.t) -> (b -> B.t) -> (k, l, a, b) term -> t =
    fun a b t -> match Term t with
      | Term Var { name; size; label; _ } ->
        var name size (a label)
      | Term Load { len; dir; addr; label; _ } ->
        load len dir (map a b addr) (b label)
      | Term (Cst _ as c) -> c
      | Term Unary { f; x; _ } ->
        unary f (map a b x)
      | Term Binary { f; x; y; _ } ->
        binary f (map a b x) (map a b y)
      | Term Ite { c; t; e; _ } ->
        ite (map a b c) (map a b t) (map a b e)
end
