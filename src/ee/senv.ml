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

(* utils *)
let pp_int_as_bv ppf x = function
  | 1  -> Format.fprintf ppf "#b%d" x
  | 4  -> Format.fprintf ppf "#x%01x" x
  | 8  -> Format.fprintf ppf "#x%02x" x
  | 12 -> Format.fprintf ppf "#x%03x" x
  | 16 -> Format.fprintf ppf "#x%04x" x
  | 20 -> Format.fprintf ppf "#x%05x" x
  | 24 -> Format.fprintf ppf "#x%06x" x
  | 28 -> Format.fprintf ppf "#x%07x" x
  | 32 -> Format.fprintf ppf "#x%08x" x
  | sz -> Format.fprintf ppf "(_ bv%d %d)" x sz
let pp_bv ppf value size =
  try pp_int_as_bv ppf (Z.to_int value) size
  with Z.Overflow ->
    Format.fprintf ppf "(_ bv%a %d)" Z.pp_print value size

module Query_stats = struct
  type t = {
    total_solver_time : float;
    nb_query : int;
    nb_solver_queries : int;
    total_assume : int;
    assume_sent : int;
    total_enumerate : int;
    enumerate_sent : int;
    (* infered_variables : int; *)
    (* tigger_inference_1 : int; *)
    (* tigger_inference_2 : int; *)
    (* tigger_inference_3 : int;
    tigger_inference_4 : int;
    tigger_inference_5 : int; *)
    trivial_true : int;
    trivial_false : int;
    min_query_complexity: int;
    max_query_complexity: int;
    sum_query_complexity: int;
    nb_query_complexity: int;
    sfs_dominant: float;
    sfs_vars : float;
    total_sfs : int;
    unsafe_sfs : int;
    safe_sfs : int;
    propagated_unsafe_sfs : int;
    matched_term : int;
    sfs_recovery_triggered: int;
    unknown_solver: int;
  }

  let empty = { total_solver_time = 0.; nb_solver_queries = 0; nb_query = 0; total_assume = 0; assume_sent = 0; total_enumerate = 0; enumerate_sent = 0;
    (* infered_variables = 0;  *)
    (* tigger_inference_1 = 0; *)
    (* tigger_inference_2 = 0; *)
  (* tigger_inference_3 = 0; tigger_inference_4 = 0; tigger_inference_5 = 0; *)
    trivial_true = 0; trivial_false = 0;
    min_query_complexity = 0;
    max_query_complexity = 0;
    sum_query_complexity = 0;
    nb_query_complexity = 0;
    sfs_dominant = 0.;
    sfs_vars = 0.;
    total_sfs = 0;
    unsafe_sfs = 0;
    safe_sfs = 0;
    propagated_unsafe_sfs = 0;
    matched_term = 0;
    sfs_recovery_triggered = 0;
    unknown_solver = 0;
    }

  let add_query s = Sse_options.Logger.debug ~level:4 "STATS query"; { s with nb_query = s.nb_query + 1 }
  let add_solver_query s = Sse_options.Logger.debug ~level:4 "STATS solver call"; { s with nb_solver_queries = s.nb_solver_queries + 1 }
  let add_solver_time s time = { s with total_solver_time = s.total_solver_time +. time }

  let add_total_assume s = { s with total_assume = s.total_assume + 1 }
  let add_assume_sent s = { s with assume_sent = s.assume_sent + 1 }
  let add_total_enumerate s = { s with total_enumerate = s.total_enumerate + 1 }
  let add_enumerate_sent s = { s with enumerate_sent = s.enumerate_sent + 1 }
  (* let add_infered_variables s = Sse_options.Logger.debug ~level:4 "STATS infered variable"; { s with infered_variables = s.infered_variables + 1 } *)
  (* let add_tigger_inference_1 s = { s with tigger_inference_1 = s.tigger_inference_1 + 1 } *)
  (* let add_tigger_inference_2 s = { s with tigger_inference_2 = s.tigger_inference_2 + 1 } *)
  (* let add_tigger_inference_3 s = { s with tigger_inference_3 = s.tigger_inference_3 + 1 }
  let add_tigger_inference_4 s = { s with tigger_inference_4 = s.tigger_inference_4 + 1 }
  let add_tigger_inference_5 s = { s with tigger_inference_5 = s.tigger_inference_5 + 1 } *)
  let add_trivial_true s = { s with trivial_true = s.trivial_true + 1 }
  let add_trivial_false s = { s with trivial_false = s.trivial_false + 1 }

  let update_min_query_complexity s v = { s with min_query_complexity = min s.min_query_complexity v }
  let update_max_query_complexity s v = { s with max_query_complexity = max s.max_query_complexity v }
  let update_sum_query_complexity s v = { s with sum_query_complexity = s.sum_query_complexity + v }
  let add_nb_query_complexity s = { s with nb_query_complexity = s.nb_query_complexity + 1 }
  let update_sfs_bi s dominant vars unsafe = 
    Sse_options.Logger.debug ~level:4 "STATS %s" (if unsafe then "UNSAFE" else "SAFE");
    { s with   
    sfs_dominant =  s.sfs_dominant +. dominant ; 
    sfs_vars = s.sfs_vars +. vars; 
    total_sfs = s.total_sfs + 1; 
    unsafe_sfs = s.unsafe_sfs + (if unsafe then 1 else 0);
    safe_sfs = s.safe_sfs + (if unsafe then 0 else 1) }
  let add_propagated_unsafe_sfs s = Sse_options.Logger.debug ~level:4 "STATS PROPAGATED";
    { s with propagated_unsafe_sfs = s.propagated_unsafe_sfs + 1 }
  let add_matched_term s = { s with matched_term = s.matched_term + 1 }
  let add_sfs_recovery_triggered s = Sse_options.Logger.debug ~level:4 "SFS recovery"; { s with sfs_recovery_triggered = s.sfs_recovery_triggered + 1 }
  let add_unknown_solver s = Sse_options.Logger.info "Unknown: likely solver timeout"; { s with unknown_solver = s.unknown_solver + 1 }
  

  let pp ppf s =
    Format.fprintf ppf
      "@[<v 0>\
        @[<h>Number of queries %d@]@,\
        @[<h>Number of queries sent to the solver %d@]@,\
        @[<h>Average solving time %f@]@,\
        @[<h>Total assume queries %d@]@,\
        @[<h>Assume queries sent to solveur %d@]@,\
        @[<h>Total enumerate queries %d@]@,\
        @[<h>Enumerate queries sent to solveur %d@]@,\
        @[<h>Trivial true assume %d@]@,\
        @[<h>Trivial false assume %d@]@,\
        @[<h>Unknown solver %d @]@,\
        @]"
      s.nb_query
      s.nb_solver_queries
      (s.total_solver_time /. (Float.of_int s.nb_solver_queries))
      s.total_assume
      s.assume_sent
      s.total_enumerate
      s.enumerate_sent
      s.trivial_true
      s.trivial_false
      s.unknown_solver
    ;
    if not (Sse_options.SubFaultsSimplification.is_default ()) then (
      Format.fprintf ppf
      "@[<v 0>\
        @[<h>SubFault Simplification method: %s@]@,\
        @[<h>SubFault Simplification recovery mecanism: %s@]@,\
        @[<h>Dominant b_i %f @]@,\
        @[<h>Total b_i vars %f @]@,\
        @[<h>Condition unsafe sfs %d @]@,\
        @[<h>Condition safe sfs %d @]@,\
        @[<h>Query unsafe sfs %d @]@,\
        @[<h>Matched terms %d @]@,\
        @[<h>Sfs recovery mecanism triggered %d @]@,\
        @]"
      (Sse_options.SubFaultsSimplification.get ())
      (Sse_options.SFSrecovery.get ())
      (* 0.0 *)
      (if s.total_sfs != 0 then s.sfs_dominant /. Float.of_int(s.total_sfs) else 0.)
      (if s.total_sfs != 0 then s.sfs_vars /. Float.of_int(s.total_sfs) else 0.)
      s.unsafe_sfs
      s.safe_sfs
      s.propagated_unsafe_sfs
      s.matched_term
      s.sfs_recovery_triggered
    );
    if not (Sse_options.ComputeQueryComplexity.is_default ()) then (
      Format.fprintf ppf
      "@[<v 0>\
        @[<h>Compute query complexity active %b@]@,\
        @[<h>Min query complexity %d@]@,\
        @[<h>Max query complexity %d@]@,\
        @[<h>Sum query complexity %d@]@,\
        @[<h>Nb query complexity %d@]@,\
        @[<h>Avg query complexity %d@]@,\
        @]"
      (Sse_options.ComputeQueryComplexity.get ())
      s.min_query_complexity
      s.max_query_complexity
      s.sum_query_complexity
      s.nb_query_complexity
      (* (Float.div (Float.of_int s.sum_query_complexity) (Float.of_int s.nb_query_complexity)) *)
      (if s.nb_query_complexity != 0 then s.sum_query_complexity / s.nb_query_complexity else 0)
    )

  ;;
  module R = struct
    let value = ref empty
    let add_query () = value := add_query !value
    let add_solver_query () = value := add_solver_query !value
    let add_solver_time time = value := add_solver_time !value time 
    let add_total_assume () = value := add_total_assume !value
    let add_assume_sent () = value := add_assume_sent !value
    let add_total_enumerate () = value := add_total_enumerate !value
    let add_enumerate_sent () = value := add_enumerate_sent !value
    (* let add_infered_variables () = value := add_infered_variables !value *)
    (* let add_tigger_inference_1 () = value := add_tigger_inference_1 !value *)
    (* let add_tigger_inference_2 () = value := add_tigger_inference_2 !value *)
    (* let add_tigger_inference_3 () = value := add_tigger_inference_3 !value
    let add_tigger_inference_4 () = value := add_tigger_inference_4 !value
    let add_tigger_inference_5 () = value := add_tigger_inference_5 !value *)
    let add_trivial_true () = 
      Sse_options.Logger.debug ~level:4 "Trivial true";
      value :=  add_trivial_true !value
    let add_trivial_false () = 
      Sse_options.Logger.debug ~level:4 "Trivial false";
      value := add_trivial_false !value
    let update_min_query_complexity d = value := update_min_query_complexity !value d
    let update_max_query_complexity d = value := update_max_query_complexity !value d
    let update_sum_query_complexity d = value := update_sum_query_complexity !value d
    let add_nb_query_complexity () = value := add_nb_query_complexity !value
    let update_sfs_bi dominant vars unsafe = value := update_sfs_bi !value (Float.of_int(dominant)) (Float.of_int(vars)) unsafe
    let add_propagated_unsafe_sfs () = value := add_propagated_unsafe_sfs !value
    let add_matched_term () = value := add_matched_term !value
    let add_sfs_recovery_triggered () = value := add_sfs_recovery_triggered !value
    let add_unknown_solver () = value := add_unknown_solver !value
    
    
    let pp ppf () = pp ppf !value
  end
  include R

end

module Solver = struct

  (* NOTE: weird to not need this module with only Native Solver *)

  let n = ref 0

  type state =
    | Dead
    | Assert
    | Sat
    | Unsat


  type session = {
    solver        : Prover.t;
    mutable state : state;
    pid           : Subprocess.t;
    stdin         : out_channel;
    stdout        : in_channel;
    stderr        : in_channel;
    stdlog        : out_channel option;
    formatter     : Format.formatter
  }

  let close t =
    ignore @@ Subprocess.close t.pid

  let start ?stdlog timeout solver =
    Sse_options.Logger.debug "Openning session %d" !n; incr n;
    let cmd = Prover.command timeout solver ~incremental:true in
    let pid = Subprocess.spawn ~pdeathsig:Sys.sigkill cmd in
    let stdin = Subprocess.stdin pid
    and stdout = Subprocess.stdout pid
    and stderr = Subprocess.stderr pid in
    let formatter = match stdlog with
      | None -> Format.formatter_of_out_channel stdin
      | Some stdlog ->
        let output str pos len =
          output_substring stdlog str pos len;
          output_substring stdin str pos len
        and flush () = flush stdlog; flush stdin in
        Format.make_formatter output flush in
    Format.fprintf formatter
      "@[<v 0>\
       (set-option :print-success false)@ \
       (set-info :smt-lib-version 2.5)@ \
       (set-logic QF_ABV)@ \
       @]";
    { solver; state=Assert; pid; stdin; stdout; stderr; stdlog; formatter }

  type status = SAT | UNSAT | UNKNOWN

  let check_sat t =
    Format.fprintf t.formatter "(check-sat)@.";
    match input_line t.stdout with
    | "sat"     -> t.state <- Sat;    SAT
    | "unsat"   -> t.state <- Unsat;  UNSAT
    | "unknown" -> t.state <- Assert; UNKNOWN
    |  s -> (
      let _ = Sse_options.Logger.info "Solver returned: %s" s in
      t.state <- Assert; UNKNOWN
    )
    | exception End_of_file -> t.state <- Dead; UNKNOWN

  let get_value t pp x =
    assert (t.state = Sat);
    Format.fprintf t.formatter "(get-value (%a))@." pp x;
    let lexbuf = Lexing.from_channel t.stdout in
    match Smtlib_parser.ivalue Smtlib_lexer.token lexbuf with
    | _, Smtlib.CstBool false         -> Z.zero
    | _, Smtlib.CstBool true          -> Z.one
    | _, Smtlib.CstBinary b           -> Z.of_string_base 2 b
    | _, Smtlib.CstDecimalSize (d, _) -> Z.of_string_base 10 d
    | _, Smtlib.CstHexadecimal x      -> Z.of_string_base 16 x
    | _, _ -> assert false

  let put t pp x =
    pp t.formatter x;
    Format.pp_print_space t.formatter ();
    t.state <- Assert

end

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module BiTbl = Basic_types.BigInt.Htbl
module NiMap = Basic_types.Int.Map
module NiTbl = Basic_types.Int.Htbl

(* This module is intended to generate valid SMTLib2 short identifiers  *)
(* Are valid any non-empty sequence of letters, digits
                  and the characters ~ ! @ $ % ^ & * _ - + = < > . ? /
                  that does not start with a digit                      *)
(* To never clash with reserved keywords, generated identifiers
   always start with one of the special characters                      *)
module Suid : sig
  type t = private string

  external to_string : t -> string = "%identity"
  val pp   : Format.formatter -> t -> unit [@@ocaml.warning "-32"]

  val zero : t
  val incr : t -> t

end = struct
  type t = string

  external to_string : t -> string = "%identity"
  let pp = Format.pp_print_string

  let zero = "!0"
  let incr =
    let set t' i c = Bytes.set t' i c; Bytes.unsafe_to_string t' in
    (* to keep it compact, transition rules are impacted by the ASCII encoding:
          0-9a-zA-Z!$%&*+./<=>?@^_~                                         *)
    let rec incr t' i = match Bytes.get t' i with
      | '9' -> set t' i 'a'
      | 'z' -> set t' i 'A'
      | 'Z' -> set t' i '!'
      | '!' -> set t' i '$'
      | '&' -> set t' i '*'
      | '+' -> set t' i '.'
      | '/' -> set t' i '<'
      | '<' -> set t' i '>'
      | '@' -> set t' i '^'
      | '_' -> set t' i '~'
      | '~' when i = 0 -> (* overflow *)
        let t' = Bytes.(make (length t' + 1) '0') in
        Bytes.set t' 0 '!';
        Bytes.unsafe_to_string t'
      | '~' -> Bytes.set t' i '0'; incr t' (i - 1) (* carry *)
      |  x  -> set t' i Char.(unsafe_chr (code x + 1)) in
    fun t ->
      let t' = Bytes.of_string t in
      incr t' (Bytes.length t' - 1)
end



module Bv = Bitvector

module rec Expr : Term.S with type a := unit and type b := Memory.t =
  Term.Make
    (struct
      type t = unit
      let compare () () = 0
      let equal () () = true
      let hash () = 0
    end)
    (Memory)

and Memory : sig
  type t =
    | Unknown
    | Source of {
        id     : int;
        over   : t;
        orig   : Loader.Img.t;
        offset : int;
        len    : int
      }
    | Layer of {
        id    : int;
        over  : t;
        addr  : Expr.t;
        bytes : Expr.t NiMap.t;
        pop   : int
      }
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val source  : offset:int -> len:int -> Loader.Img.t -> t -> t
  val write   : addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t
  val read    : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t
end = struct
  type t =
    | Unknown
    | Source of {
        id     : int;
        over   : t;
        orig   : Loader.Img.t;
        offset : int;
        len    : int
      }
    | Layer of {
        id    : int;
        over  : t;
        addr  : Expr.t;
        bytes : Expr.t NiMap.t;
        pop   : int
      }

  let hash = function
    | Unknown -> 0
    | Source { id; _ }
    | Layer  { id; _ } -> id

  let compare t t' = hash t - hash t'

  let equal t t' = hash t = hash t'

  let source ~offset ~len orig over =
    Source { id=hash over + 1; orig; offset; len; over }

  let byte n value =
    Expr.restrict
      ~lo:(byte_size * n)
      ~hi:(byte_size * (n + 1) - 1)
      value

  let split dir value offset =
    let len = Expr.sizeof value / byte_size in
    match dir with
    | Expr.LittleEndian ->
      let rec fold n value offset map =
        if n = 0 then map
        else let n = n - 1 in
          fold n value offset (NiMap.add (n + offset) (byte n value) map) in
      fold len value offset NiMap.empty, len
    | Expr.BigEndian ->
      let rec fold i n value offset map =
        if i = n then map
        else fold (i + 1) n value offset
            (NiMap.add (i + offset) (byte i value) map) in
      fold 0 len value offset NiMap.empty, len

  let layer addr value dir over =
    let bytes, pop = split dir value 0 in
    Layer { id=hash over + 1; over; addr; bytes; pop }

  let write ~addr value dir over = match over with
    | Unknown | Source _ -> layer addr value dir over
    | Layer { id; addr=addr'; bytes=bytes'; pop=pop'; over=over' } ->
      match Expr.sub addr addr' with
      | Expr.Cst bv ->
        let offset = Bv.to_uint bv in
        let bytes, pop = split dir value offset in
        let cnt = ref (pop' + pop) in
        let bytes = NiMap.union
            (fun _ _ b -> decr cnt; Some b) bytes' bytes in
        Layer { id=id + 1; over=over'; addr=addr'; bytes; pop=(!cnt) }
      | _ -> layer addr value dir over

  let read =
    let concat dir buf = match dir with
      | Expr.LittleEndian ->
        let value = ref Array.(get buf (length buf - 1)) in
        for i = Array.length buf - 2 downto 0 do
          value := Expr.append !value (Array.get buf i)
        done;
        !value
      | Expr.BigEndian ->
        let value = ref Array.(get buf 0) in
        for i = 1 to Array.length buf - 1 do
          value := Expr.append !value (Array.get buf i)
        done;
        !value in
    let fill dir addr map buf memory =
      let map = ref map in
      let load = Expr.load (Array.length buf) dir addr memory in
      while !map <> Z.zero do
        let x = Z.trailing_zeros !map in
        Array.set buf x (byte x load);
        map := Z.(!map lxor one lsl x)
      done in
    let rec lookup dir addr map buf memory = match memory with
      | Memory.Unknown -> fill dir addr map buf memory
      | Memory.Source { offset; len; orig; over; _ } ->
        begin match addr with
          | Expr.Cst bv ->
            let offset' = Bv.to_uint bv in
            let map = ref map and map' = ref Z.zero in
            while !map <> Z.zero do
              let x = Z.trailing_zeros !map in
              let y = offset' + x in
              begin if offset <= y && y < offset + len then
                  Array.set buf x
                    (Expr.constant (Bv.of_int ~size:byte_size
                                      (Loader.read_address orig y)))
                else map' := Z.(!map' lor one lsl x)
              end;
              map := Z.(!map lxor one lsl x)
            done;
            if !map' <> Z.zero then lookup dir addr !map' buf over
          | _ -> fill dir addr map buf memory
        end
      | Layer { addr=addr'; bytes; over; _ } ->
        begin match Expr.sub addr addr' with
          | Expr.Cst bv ->
            let offset' = Bv.to_uint bv in
            let map = ref map and map' = ref Z.zero in
            while !map <> Z.zero do
              let x = Z.trailing_zeros !map in
              let y = offset' + x in
              begin match NiMap.find y bytes with
                | byte -> Array.set buf x byte
                | exception Not_found ->
                  map' := Z.(!map' lor one lsl x)
              end;
              map := Z.(!map lxor one lsl x)
            done;
            if !map' <> Z.zero then lookup dir addr !map' buf over
          | _ -> fill dir addr map buf memory
        end in
    fun ~addr bytes dir memory ->
      let buf = Array.make bytes (Expr.zero) (* no value *) in
      lookup dir addr (Z.pred (Z.shift_left Z.one bytes)) buf memory;
      concat dir buf
end

module BiItM = struct
  (* TODO: poor man interval map *)
  type 'a t = (Z.t * Z.t * 'a) list
  let empty = []
  let compare (base, _, _) (base', _, _) = Z.compare base base'
  let add ~base size x t = List.sort compare ((base, Z.of_int size, x) :: t)
  let find a t =
    let _, _, x = List.find
        (fun (base, size, _) -> Z.leq base a && Z.lt a (Z.add base size)) t in
    x
end

module BvTbl = Hashtbl.Make (struct include Expr let equal = is_equal end)
module AxTbl = Hashtbl.Make (Memory)

module S = Basic_types.String.Map
module I = Basic_types.BigInt.Map

module SExpr = Set.Make(Expr)
module MHistory = Map.Make(Int)

module Model = struct
  type t = Z.t BvTbl.t * char BiTbl.t

  let empty () = BvTbl.create 0, BiTbl.create 0

  let copy model = match model with
  | a, b -> BvTbl.copy a, BiTbl.copy b

  let maybe_pp_char ppf c =
    if String_utils.is_char_printable c then
      Format.fprintf ppf " (%c)" c

  let pp_variables ppf vars values =
    if S.is_empty vars = false then begin
      Format.pp_print_string ppf "# Variables";
      Format.pp_print_cut ppf ();
      S.iter (fun name list ->
          let list = List.rev list in
          Format.fprintf ppf "%s : @[<hov>%a@]@ " name
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               (fun ppf var ->
                  match BvTbl.find values var with
                  | exception Not_found -> Format.pp_print_string ppf "--"
                  | z -> pp_bv ppf z (Expr.sizeof var))) list;
          match list with
          | var :: _ :: _ when Expr.sizeof var = 8 ->
            Format.pp_print_string ppf "  [as ascii] ";
            List.iter (fun var -> match BvTbl.find values var with
                | exception Not_found -> Format.pp_print_string ppf "."
                | z -> Format.pp_print_char ppf (Char.unsafe_chr (Z.to_int z)))
              list;
            Format.pp_print_space ppf ()
          | _ -> ()
        ) vars;
    end
  let pp_memory ppf memory addr_space =
    if BiTbl.length memory = 0 then
      Format.pp_print_string ppf "-- empty memory --"
    else begin
      Format.pp_print_string ppf "# Memory";
      Format.pp_print_cut ppf ();
      let img = Kernel_functions.get_img () in
      let noname = "" in
      let section_name addr =
        let address = Z.to_int addr in
        match Loader_utils.find_section_by_address ~address img with
        | None -> noname
        | Some section -> Loader.Section.name section in
      let pp_section ppf name =
        if name == noname then
          Format.pp_print_string ppf "unamed section"
        else Format.fprintf ppf "section %s" name in
      let last_section = ref "--" in
      I.iter (fun addr byte ->
          let name = section_name addr in
          if name <> !last_section then begin
            Format.fprintf ppf "; %a@ " pp_section name;
            last_section := name;
          end;
          pp_bv ppf addr addr_space;
          Format.fprintf ppf " : %02x %a@ "
            (Char.code byte)
            maybe_pp_char byte) @@
      BiTbl.fold I.add memory I.empty
    end

  let pp ppf vars addr_space (values, memory) =
    if S.is_empty vars && BiTbl.length memory = 0 then
      Format.fprintf ppf "@[<h>--- Empty model ---@]"
    else begin
      Format.fprintf ppf "@[<v 0>--- Model ---@ ";
      pp_variables ppf vars values;
      Format.pp_print_space ppf ();
      pp_memory ppf memory addr_space;
      Format.pp_close_box ppf ()
    end

  let rec eval (vars, _ as m) = function
    | Expr.Cst bv -> Bv.value_of bv
    | e ->
      try BvTbl.find vars e
      with Not_found ->
        let value = match e with
          | Expr.Cst _ -> assert false
          | Expr.Var _ ->
            (* Z.logand (Z.of_int (Expr.hash e))
              (Z.pred (Z.shift_left Z.one (Expr.sizeof e))) *)
              Z.zero
          | Expr.Load { addr; len; dir; label; _ } ->
            let ptr = eval m addr in
            eval_load m ptr len dir label
          | Expr.Unary { f=Sext _; x; size; _ } ->
            Z.logand (Z.signed_extract (eval m x) 0 (Expr.sizeof x))
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Unary { f; x; size; _ } ->
            Z.logand (eval_unop (eval m x) f)
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Binary { f=Concat; x; y; size; _ } ->
            Z.logand (Z.logor (eval m y) (Z.shift_left (eval m x)
                                            (Expr.sizeof y)))
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Binary { f=Rol; x; y; size; _ } ->
            let x = eval m x  and y = Z.to_int (eval m y) in
            Z.logand (Z.logor (Z.shift_left x y) (Z.shift_right x (size - y)))
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Binary { f=Ror; x; y; size; _ } ->
            let x = eval m x  and y = Z.to_int (eval m y) in
            Z.logand (Z.logor (Z.shift_right x y) (Z.shift_left x (size - y)))
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Binary { f=Sdiv | Smod | Asr | Sle | Slt | Sge | Sgt as f;
                          x; y; size; _ } ->
            let x = eval m x and y = eval m y in
            let x = Z.sub x
                (Z.shift_left
                   (Z.of_int (Bool.to_int (Z.testbit x (size - 1))))
                   size)
            and y = Z.sub y
                (Z.shift_left
                   (Z.of_int (Bool.to_int (Z.testbit y (size - 1))))
                   size) in
            Z.logand (eval_binop x y f) (Z.pred (Z.shift_left Z.one size))
          | Expr.Binary { f; x; y; size; _ } ->
            Z.logand (eval_binop (eval m x) (eval m y) f)
              (Z.pred (Z.shift_left Z.one size))
          | Expr.Ite { c; t; e; _ } ->
            if Z.zero = eval m c then eval m e else eval m t in
        BvTbl.add vars e value;
        value
  and eval_load =
    let concat dir buf = match dir with
      | Expr.LittleEndian ->
        let value = ref Array.(get buf (length buf - 1)) in
        for i = Array.length buf - 2 downto 0 do
          value := Z.logor (Array.get buf i) (Z.shift_left !value 8)
        done;
        !value
      | Expr.BigEndian ->
        let value = ref Array.(get buf 0) in
        for i = 1 to Array.length buf - 1 do
          value := Z.logor (Z.shift_left !value 8) (Array.get buf i)
        done;
        !value in
    let fill memory ptr map buf =
      let map = ref map in
      while !map <> Z.zero do
        let x = Z.trailing_zeros !map in
        let ptr = Z.add ptr (Z.of_int x) in
        let byte = match BiTbl.find memory ptr with
          | exception Not_found ->
            let byte = Z.logand ptr (Z.of_int 0xff) in
            BiTbl.add memory ptr (Char.unsafe_chr (Z.to_int byte));
            byte
          | c -> Z.of_int (Char.code c) in
        Array.set buf x byte;
        map := Z.(!map lxor one lsl x)
      done in
    let rec lookup (_, memory as m) ptr map buf = function
      | Memory.Unknown -> fill memory ptr map buf
      | Memory.Source { offset; len; orig; over; _ } ->
        let offset' = Z.to_int ptr in
        let map = ref map and map' = ref Z.zero in
        while !map <> Z.zero do
          let x = Z.trailing_zeros !map in
          let y = offset' + x in
          begin if offset <= y && y < offset + len then
              Array.set buf x (Z.of_int (Loader.read_address orig y))
            else map' := Z.(!map' lor one lsl x)
          end;
          map := Z.(!map lxor one lsl x)
        done;
        if !map' <> Z.zero then lookup m ptr !map' buf over
      | Layer { addr; bytes; over; _ } ->
        let addr = eval m addr in
        let offset = Z.to_int (Z.sub ptr addr) in
        (* let size = Z.size addr in
        let offset = Z.to_int (Z.logand (Z.sub ptr addr) (Z.pred (Z.shift_left Z.one size))) in *)
        (*  *)
        let map = ref map and map' = ref Z.zero in
        while !map <> Z.zero do
          let x = Z.trailing_zeros !map in
          let y = offset + x in
          begin match NiMap.find y bytes with
            | byte -> Array.set buf x (eval m byte)
            | exception Not_found ->
              map' := Z.(!map' lor one lsl x)
          end;
          map := Z.(!map lxor one lsl x)
        done;
        if !map' <> Z.zero then lookup m ptr !map' buf over in
    fun m ptr len dir memory ->
      let buf = Array.make len Z.zero (* no value *) in
      lookup m ptr (Z.pred (Z.shift_left Z.one len)) buf memory;
      concat dir buf
  and eval_unop x = function
    | Not    -> Z.lognot x
    | Minus  -> Z.neg x
    | Uext _ -> x
    | Sext _ -> assert false
    | Restrict { lo; _ } -> Z.shift_right x lo
  and eval_binop x y = function
    | Plus   -> Z.add x y
    | Minus  -> Z.sub x y
    | Mul    -> Z.mul x y
    | Udiv   -> Z.div x y
    | Umod   -> Z.rem x y
    | Sdiv   -> Z.div x y
    | Smod   -> Z.rem x y
    | Or     -> Z.logor x y
    | And    -> Z.logand x y
    | Xor    -> Z.logxor x y
    | Concat -> assert false
    | Lsl    -> Z.shift_left x (Z.to_int y)
    | Lsr    -> Z.shift_right x (Z.to_int y)
    | Asr    -> Z.shift_right x (Z.to_int y)
    | Rol    -> assert false
    | Ror    -> assert false
    | Eq     -> Z.of_int (Bool.to_int (Z.equal x y))
    | Diff   -> Z.of_int (Bool.to_int (not (Z.equal x y)))
    | Ule    -> Z.of_int (Bool.to_int (Z.leq x y))
    | Ult    -> Z.of_int (Bool.to_int (Z.lt x y))
    | Uge    -> Z.of_int (Bool.to_int (Z.geq x y))
    | Ugt    -> Z.of_int (Bool.to_int (Z.gt x y))
    | Sle    -> Z.of_int (Bool.to_int (Z.leq x y))
    | Slt    -> Z.of_int (Bool.to_int (Z.lt x y))
    | Sge    -> Z.of_int (Bool.to_int (Z.geq x y))
    | Sgt    -> Z.of_int (Bool.to_int (Z.gt x y))
end

type t = {
  constraints : Expr.t list;     (* reversed sequence of assertions *)
  vsymbols    : Expr.t S.t;      (* collection of visible symbols *)
  vmemory     : Memory.t;        (* visible memory *)
  
  fvariables  : Expr.t list S.t; (* collection of free variables *)
  infered_variables : string list; (* variables already infered*)
  record_bi_vars : SExpr.t ; 
  record_bi_dominant : SExpr.t ;
  history     : SExpr.t MHistory.t;      (* set of term hashes already computed *)
  seen_vars   : SExpr.t;      (* set of dominated vars *)
  is_path_unsafe : bool;      (* aggregation of safe/unsafe queries in path *)
  adresse_history   : SExpr.t;  (* records addresses seen where faults have already been negated *)    
  ilocs       : Loader.Img.t BiItM.t;
  (* set of initialized memory locations *)

  model       : Model.t;         (* a model that satisfy constraints *)
  fid         : Suid.t          (* local unique indice counter *)
}

type dual = {
  normal_state    : t;
  faulted_state   : t;
  uid             : Suid.t;         (* unique indice counter *)
}

type which_dual = Normal | Faulted | Both

let empty_state fid =
  {
    constraints = [];
    vsymbols    = S.empty;
    vmemory     = Memory.Unknown;
    fvariables  = S.empty;
    infered_variables = [];
    record_bi_vars = SExpr.empty;
    record_bi_dominant = SExpr.empty;
    history     = MHistory.empty;
    seen_vars   = SExpr.empty;
    is_path_unsafe = false;
    adresse_history = SExpr.empty;
    ilocs       = BiItM.empty;
    model       = Model.empty ();
    fid
  }

let empty_dual =
  let uid = Suid.(incr zero) in
  {
    normal_state = empty_state uid;
    faulted_state = empty_state uid;
    uid         = uid; (* zero is reserved for initial memory *)
  }

let get_state_from_dual is_fautled dual = 
  match is_fautled with
  | Normal -> dual.normal_state
  | Faulted -> dual.faulted_state
  | _ -> assert false

let update_dual is_faulted dual update_fct =
  match is_faulted with
  | Normal -> { dual with normal_state = update_fct dual.normal_state }
  | Faulted -> { dual with faulted_state = update_fct dual.faulted_state }
  | Both -> { dual with
    normal_state = update_fct dual.normal_state;
    faulted_state = update_fct dual.faulted_state
  }

(* 
let load_from ~addr len orig state =
  let base = Bv.value_of addr in
  let ilocs = BiItM.add ~base len orig state.ilocs in
  let vmemory =
    Memory.source ~offset:(Bv.to_uint addr) ~len orig state.vmemory in
  { state with ilocs; vmemory } *)

  let update_faulted_constrains dual cs = 
    { dual with faulted_state = { dual.faulted_state with constraints = cs :: dual.faulted_state.constraints} }

let fault_normal_state dual = 
  Sse_options.Logger.debug ~level:3 "Switching normal state to faulted";
  let state = { dual.faulted_state with model = Model.copy dual.faulted_state.model } in
  { dual with normal_state = state }

let is_fault name substring is_faulted dual =
  let state = match is_faulted with 
  | Normal -> dual.normal_state
  | Faulted -> dual.faulted_state
  | Both -> assert false
  in
  let contains s1 s2 =
      let re = Str.regexp_string s2
      in
        try ignore (Str.search_forward re s1 0); true
      with Not_found -> false
  in
  List.length (List.filter (fun (str,el) -> 
    if (contains str substring) 
      then List.length (List.filter (fun e -> 
        match e with
        | Term.Var ev when ev.name == name -> true
        | _ -> false) el) != 0
      else false
    ) (S.bindings state.fvariables)
  ) != 0

let fresh name size is_faulted dual =
  let v = Expr.var (Suid.to_string dual.uid) size () in
  let uid = Suid.incr dual.uid in
  let update_state state =
    let h = match S.find name state.fvariables with
      | exception Not_found -> [ v ]
      | h -> v :: h in
    let fvariables = S.add name h state.fvariables in
    let vsymbols = S.add name v state.vsymbols in
    { state with vsymbols; fvariables; fid = uid }
  in
  update_dual is_faulted { dual with uid;} update_state

let assign name value ?(is_faulted=Both) dual =
  let update_state state = { state with vsymbols = S.add name value state.vsymbols }
  in
  update_dual is_faulted dual update_state

let write ~addr value dir ?(is_faulted=Both) dual =
  let update_state state = { state with vmemory = Memory.write ~addr value dir state.vmemory } in
  update_dual is_faulted dual update_state

let rec lookup name size is_faulted dual = 
  let state = match is_faulted with 
    | Normal -> dual.normal_state
    | Faulted -> dual.faulted_state
    | Both -> assert false
  in
  match S.find name state.vsymbols with
  | exception Not_found -> lookup name size is_faulted (fresh name size is_faulted dual)
  | bv -> bv, dual

  (* let is_already_inferred name state = List.exists (fun x -> x == name) state.infered_variables

let add_infered_variable name state = { state with infered_variables = name :: state.infered_variables} *)

let read ~addr bytes dir is_faulted dual = 
  let state = match is_faulted with 
    | Normal -> dual.normal_state
    | Faulted -> dual.faulted_state
    | Both -> assert false
  in
  Memory.read ~addr bytes dir state.vmemory

let load_from ~addr len orig is_faulted dual =
  let update_fct addr len orig state =
    let base = Bv.value_of addr in
    let ilocs = BiItM.add ~base len orig state.ilocs in
    let vmemory =
      Memory.source ~offset:(Bv.to_uint addr) ~len orig state.vmemory in
    { state with ilocs; vmemory }
  in
  update_dual is_faulted dual (update_fct addr len orig)


module Translate = struct

  let unary e = function
    | Dba.Unary_op.Not    -> Term.Not
    | Dba.Unary_op.UMinus -> Term.Minus
    | Dba.Unary_op.Sext n -> Term.Sext (n - Dba.Expr.size_of e)
    | Dba.Unary_op.Uext n -> Term.Uext (n - Dba.Expr.size_of e)
    | Dba.Unary_op.Restrict interval -> Term.Restrict interval

  let binary op =
    let open Dba.Binary_op in
    match op with
    | Plus   -> Term.Plus
    | Minus  -> Term.Minus
    | Mult   -> Term.Mul
    | DivU   -> Term.Udiv
    | DivS   -> Term.Sdiv
    | ModU   -> Term.Umod
    | ModS   -> Term.Smod
    | Eq     -> Term.Eq
    | Diff   -> Term.Diff
    | LeqU   -> Term.Ule
    | LtU    -> Term.Ult
    | GeqU   -> Term.Uge
    | GtU    -> Term.Ugt
    | LeqS   -> Term.Sle
    | LtS    -> Term.Slt
    | GeqS   -> Term.Sge
    | GtS    -> Term.Sgt
    | Xor    -> Term.Xor
    | And    -> Term.And
    | Or     -> Term.Or
    | Concat -> Term.Concat
    | LShift -> Term.Lsl
    | RShiftU -> Term.Lsr
    | RShiftS -> Term.Asr
    | LeftRotate -> Term.Rol
    | RightRotate -> Term.Ror

  let rec expr is_faulted dual_symbolic_state e =
    match e with
    | Dba.Expr.Var {name; size; _} ->
      lookup name size is_faulted dual_symbolic_state
    | Dba.Expr.Cst bv ->
      Expr.constant bv, dual_symbolic_state
    | Dba.Expr.Load (bytes, endianness, e) ->
      let addr, dual_symbolic_state = expr is_faulted dual_symbolic_state e in
      read ~addr bytes endianness is_faulted dual_symbolic_state, dual_symbolic_state
    | Dba.Expr.Binary (bop, lop, rop) ->
      let lop, dual_symbolic_state = expr is_faulted dual_symbolic_state lop in
      let rop, dual_symbolic_state = expr is_faulted dual_symbolic_state rop in
      Expr.binary (binary bop) lop rop, dual_symbolic_state
    | Dba.Expr.Unary (uop, e) ->
      let v, dual_symbolic_state = expr is_faulted dual_symbolic_state e in
      Expr.unary (unary e uop) v, dual_symbolic_state
    | Dba.Expr.Ite (c, then_e, else_e) ->
      let cond, dual_symbolic_state = expr is_faulted dual_symbolic_state c in
      let then_smt, dual_symbolic_state = expr is_faulted dual_symbolic_state then_e in
      let else_smt, dual_symbolic_state = expr is_faulted dual_symbolic_state else_e in
      Expr.ite cond then_smt else_smt, dual_symbolic_state

      (*
  let infer_from state cons =
    match cons with
      | Term.Unary{f = Term.Not; x = Var v; _} when v.size = 1 -> (
        Sse_options.Logger.debug ~level:4 "Assigned %s" v.name;
        assign_value v.name Bv.zero state, true
      )
      (* No pattern *)
      | Term.Binary{f=_;x;y;_} -> (
        let state, new_inferred_x = inference_rules x state in
        let state, new_inferred_y = inference_rules y state in
        state, new_inferred_x @ new_inferred_y
      )
      | Term.Unary{f=_;x;_} -> inference_rules x state
      | Term.Ite{c;e;t;_} -> (
        let state, new_inferred_c = inference_rules c state in
        let state, new_inferred_e = inference_rules e state in
        let state, new_inferred_t = inference_rules t state in
        state, new_inferred_c @ new_inferred_e @ new_inferred_t
      )
      | _ -> state, []
    in
    Sse_options.Logger.debug ~level:4 "query: %a" Term.pp query;
    let state, new_inferred = inference_rules query state in
    match new_inferred with
    | [] ->  state, query
    | infered -> (
      Query_stats.add_infered_variables();
      Sse_options.Logger.debug ~level:4 "query after inference: %a" Term.pp query;
      state, List.fold_left (fun query atom -> Expr.binary Expr.And query atom) query infered
    ) *)
  

  let get_fault_vars query is_faulted dual =
    let rec explore query =
      match query with
      | Term.Binary{f=_;x;y;_} -> SExpr.union (explore x) (explore y)
      | Term.Unary{f=_;x;_} -> explore x 
      | Term.Ite{c;t;e;_} -> SExpr.union (SExpr.union (explore c) (explore t)) (explore e)
      | Term.Var{name;_} as v when is_fault name "b_80" is_faulted dual-> SExpr.singleton v
      | _ -> SExpr.empty
    in
    explore query
  
  let update_vars_records vars is_faulted dual =
    let state = get_state_from_dual is_faulted dual in
    let record_bi_vars = SExpr.union state.record_bi_vars vars in
    match is_faulted with
      | Normal -> { dual with normal_state = { dual.normal_state with record_bi_vars }}
      | Faulted -> { dual with faulted_state = { dual.faulted_state with record_bi_vars }}
      | _ -> assert false

  (* PATTERN MATCHING BROKEN *)
  (* let remove_subfaults query is_faulted dual =
    (* pattern-match SFS *)
    let rec inference_rules query = 
      match query with
      | Term.Ite{c = Var v1;t = Var v2; e ;_}  when (is_fault v1.name "b_80" is_faulted dual) && (is_fault v2.name "non_det_80" is_faulted dual) -> (
        match e with 
        (* Fault on fault *)
        | Term.Ite{c= Var v3 as c1 ; t= Var v4; e=_ as e2;_} when (is_fault v3.name "b_80" is_faulted dual) && (is_fault v4.name "non_det_80" is_faulted dual) -> (
          Query_stats.add_tigger_inference_1 ();
          SExpr.add c1 (inference_rules e2)
        )
        (* Fault on addition *)
        | Term.Binary{f=Term.Plus;x;y;_} -> (
          let infered_x = match x with
          | Term.Ite{c = Var v3 as c2;t = Var v4;e;_} when (is_fault v3.name "b_80" is_faulted dual) && (is_fault v4.name "non_det_80" is_faulted dual) -> SExpr.add c2 (inference_rules e)
          | _ -> inference_rules x
          in
          let infered_y = match y with
          | Term.Ite{c = Var v3 as c2;t = Var v4;e;_} when (is_fault v3.name "b_80" is_faulted dual) && (is_fault v4.name "non_det_80" is_faulted dual) -> SExpr.add c2 (inference_rules e)
          | _ -> inference_rules y
          in
          if not (SExpr.is_empty infered_x) || not (SExpr.is_empty infered_y) then Query_stats.add_tigger_inference_2 ();
          SExpr.union infered_x infered_y
        )
        (* No pattern *)
        | _ -> inference_rules e
        
      ) 
      (* No pattern *)
      | Term.Binary{f=_;x;y;_} -> SExpr.union (inference_rules x) (inference_rules y)
      | Term.Unary{f=_;x;_} -> inference_rules x 
      | Term.Ite{c;t;e;_} -> SExpr.union (SExpr.union (inference_rules c) (inference_rules t)) (inference_rules e)
      | _ -> SExpr.empty
    in
    let tied_bis = inference_rules query in
    if not (SExpr.is_empty tied_bis) then Query_stats.add_infered_variables ();
    let vars_in_query = get_fault_vars query is_faulted dual in
    let free_bis = SExpr.diff vars_in_query tied_bis in
    let dual = update_registers free_bis tied_bis is_faulted dual in
    let state = get_state_from_dual is_faulted dual in
    let card_tied = SExpr.cardinal (SExpr.diff tied_bis state.register_bi_free) in
    let card_free = SExpr.cardinal (SExpr.diff free_bis state.register_bi_tied) in
    Query_stats.update_sfs_bi card_tied card_free;
    let res = SExpr.fold (fun atom query -> Expr.binary Expr.And (Expr.binary Expr.Eq atom Expr.zero) query) state.register_bi_tied Expr.one in
    dual, res *)

    
    (* Sse_options.Logger.debug ~level:4 "query before subfaults pattern-match simplification: %a" Term.pp query; *)
    (* Sse_options.Logger.info "QUERY: %a" Term.pp query; *)
    (* Sse_options.Logger.info "TIEDS:"; *)
    (* SExpr.iter (fun e -> Sse_options.Logger.info "%a" Term.pp e) tied_bis; *)
    
    (* Sse_options.Logger.debug ~level:4 "query after subfaults pattern-match simplification: %a" Term.pp res; *)
    (* Sse_options.Logger.info "AFTER QUERY: %a" Term.pp res; *)

    let add_non_free_bi query is_faulted dual =
      let rec explore_query query state =
        match query with
        | Term.Ite{c = Var v as c;e;t;_} when (is_fault v.name "b_80" is_faulted dual) -> (
          let vars = SExpr.union (get_fault_vars e is_faulted dual) (get_fault_vars t is_faulted dual) in
          let is_unsafe = not (SExpr.is_empty (SExpr.inter vars state.seen_vars)) in 
          let state = { state with seen_vars = SExpr.union state.seen_vars vars;
          record_bi_dominant = SExpr.add c state.record_bi_dominant } in
          is_unsafe, state
        )
        | Term.Ite{c;t;e;_} -> (
          let is_unsafe_c, state = explore_query c state in
          let is_unsafe_t, state = explore_query t state in
          let is_unsafe_e, state = explore_query e state in
          (is_unsafe_c || is_unsafe_t || is_unsafe_e), state
        )
        | Term.Binary{f=_;x;y;_} -> (
          let is_unsafe_x, state = explore_query x state in
          let is_unsafe_y, state = explore_query y state in
          (is_unsafe_x || is_unsafe_y), state
        )
        | Term.Unary{f=_;x;_} -> (
          explore_query x state
        )
        | Term.Cst _ -> false, state
        | Term.Var v as e_v when (is_fault v.name "b_80" is_faulted dual) -> (
          let state = { state with 
          record_bi_dominant = SExpr.add e_v state.record_bi_dominant } in
          false, state
        )
        | Term.Var _ -> false, state
        | Term.Load{addr;_} -> (
          explore_query addr state
        )
      in
      let state = get_state_from_dual is_faulted dual in
      let is_unsafe, state = explore_query query state in
      let state = { state with is_path_unsafe = state.is_path_unsafe || is_unsafe } in
      
      if state.is_path_unsafe then Query_stats.add_propagated_unsafe_sfs ();

      let dual = update_dual is_faulted dual (fun _ -> state) in
      let vars_in_query = get_fault_vars query is_faulted dual in
      let dual = update_vars_records vars_in_query is_faulted dual in
      let state = get_state_from_dual is_faulted dual in

      Query_stats.update_sfs_bi (SExpr.cardinal state.record_bi_dominant) (SExpr.cardinal state.record_bi_vars) is_unsafe;

      let dominated_bi = SExpr.fold (fun atom query -> Expr.binary Expr.And (Expr.binary Expr.Eq atom Expr.zero) query) (SExpr.diff state.record_bi_vars state.record_bi_dominant) Expr.one in

      (* Sse_options.Logger.debug ~level:6 "DOMINATED BI:"; *)
      (* SExpr.iter (fun e -> Sse_options.Logger.debug ~level:6 "%a" Term.pp e) (SExpr.diff state.record_bi_vars state.record_bi_dominant); *)
      (* Sse_options.Logger.debug ~level:6 "ALL VARS:"; *)
      (* SExpr.iter (fun e -> Sse_options.Logger.debug ~level:6 "%a" Term.pp e) state.record_bi_vars; *)

      dual, dominated_bi

      (* Sse_options.Logger.info "free %d" (SExpr.cardinal free_bis); *)
      (* Sse_options.Logger.debug ~level:4 "query after subfaults free-bi simplification: %a" Term.pp res; *)
  

    let sfs_memoization query is_faulted dual =
      let rec explore_query query state =
        match MHistory.find_opt (Expr.hash query) state.history with
        | Some seen_vars -> (
          Query_stats.add_matched_term ();
          let is_unsafe = not (SExpr.is_empty (SExpr.inter seen_vars state.seen_vars)) in
          is_unsafe, state
        )
        | None -> (
          match query with
          | Term.Ite{c = Var v as c;e;t;_} as term when (is_fault v.name "b_80" is_faulted dual) -> (
            let vars = SExpr.union (get_fault_vars e is_faulted dual) (get_fault_vars t is_faulted dual) in
            let is_unsafe = not (SExpr.is_empty (SExpr.inter vars state.seen_vars)) in 
            let state = { state with history = MHistory.add (Expr.hash term) vars state.history; 
            seen_vars = SExpr.union state.seen_vars vars;
            record_bi_dominant = SExpr.add c state.record_bi_dominant } in
            is_unsafe, state
          )
          | Term.Ite{c;t;e;_} as term -> (
            let is_unsafe_c, state = explore_query c state in
            let is_unsafe_t, state = explore_query t state in
            let is_unsafe_e, state = explore_query e state in
            let is_unsafe = is_unsafe_c || is_unsafe_t || is_unsafe_e in
            let vars = SExpr.union (SExpr.union (get_fault_vars e is_faulted dual) (get_fault_vars t is_faulted dual)) (get_fault_vars c is_faulted dual) in
            let state = { state with history = MHistory.add (Expr.hash term) vars state.history } in 
            is_unsafe, state 
          )
          | Term.Binary{f=_;x;y;_} as term -> (
            let is_unsafe_x, state = explore_query x state in
            let is_unsafe_y, state = explore_query y state in
            let is_unsafe = is_unsafe_x || is_unsafe_y in
            let vars = SExpr.union (get_fault_vars x is_faulted dual) (get_fault_vars y is_faulted dual) in
            let state = { state with history = MHistory.add (Expr.hash term) vars state.history } in 
            is_unsafe, state 
          )
          | Term.Unary{f=_;x;_} as term -> (
            let is_unsafe, state = explore_query x state in
            let vars = get_fault_vars x is_faulted dual in
            let state = { state with history = MHistory.add (Expr.hash term) vars state.history } in
            is_unsafe, state 
          )
          | Term.Cst _ -> false, state
          | Term.Var v as e_v as term when (is_fault v.name "b_80" is_faulted dual)  -> (
            let is_unsafe = false in
            let state = { state with 
            record_bi_dominant = SExpr.add e_v state.record_bi_dominant;
            history = MHistory.add (Expr.hash term) SExpr.empty state.history } in
            is_unsafe, state
          )
          | Term.Var _ -> false, state
          | Term.Load{addr;_} as term -> (
            let is_unsafe, state = explore_query addr state in
            let state = { state with history = MHistory.add (Expr.hash term) SExpr.empty state.history } in
            is_unsafe, state 
          )
        )
      in
      (* Sse_options.Logger.debug ~level:4 "query before subfaults free-bi simplification: %a" Term.pp query; *)
      let state = get_state_from_dual is_faulted dual in
      let is_unsafe, state = explore_query query state in
      let state = { state with is_path_unsafe = state.is_path_unsafe || is_unsafe } in

      let dual = update_dual is_faulted dual (fun _ -> state) in
      let vars_in_query = get_fault_vars query is_faulted dual in
      let dual = update_vars_records vars_in_query is_faulted dual in
      let state = get_state_from_dual is_faulted dual in

      (* Sse_options.Logger.info "MHistory %d" (MHistory.cardinal (MHistory.filter (fun _ b -> b) state.history)); *)
      (* Sse_options.Logger.info "Seen vars:" ; *)
      (* SExpr.iter (fun x -> Sse_options.Logger.info "%a" Term.pp x) state.seen_vars; *)

      Query_stats.update_sfs_bi (SExpr.cardinal state.record_bi_dominant) (SExpr.cardinal state.record_bi_vars) is_unsafe;
      if state.is_path_unsafe then Query_stats.add_propagated_unsafe_sfs ();

      let dominated_bi = SExpr.fold (fun atom query -> Expr.binary Expr.And (Expr.binary Expr.Eq atom Expr.zero) query) (SExpr.diff state.record_bi_vars state.record_bi_dominant) Expr.one in

      (* Sse_options.Logger.debug ~level:6 "DOMINATED BI:"; *)
      (* SExpr.iter (fun e -> Sse_options.Logger.debug ~level:6 "%a" Term.pp e) (SExpr.diff state.record_bi_vars state.record_bi_dominant); *)
      (* Sse_options.Logger.debug ~level:6 "ALL VARS:"; *)
      (* SExpr.iter (fun e -> Sse_options.Logger.debug ~level:6 "%a" Term.pp e) state.record_bi_vars; *)

      dual, dominated_bi

      (* a & ( [ uext a + uext b ] == 0x1 ) *)
  (* let infer_from state query =
    let rec inference_rules query state = 
      match query with
      (* sdiv a (ite c ? e 0x0) => c := 0b1 *)
      | Term.Binary{f=Term.Sdiv;x;y=Term.Ite{c;e;t=Cst z;_};_} when Bv.is_zeros z -> (
        let state, new_inferred_x = inference_rules x state in
        let state, new_inferred_e = inference_rules e state in
        match c with 
        | Var v -> (
          if not (is_already_inferred v.name state) then (
            Query_stats.add_tigger_inference_1 ();
            let state = add_infered_variable v.name state in
            state, (Expr.binary Expr.Eq c (Expr.constant Bv.one)) :: new_inferred_x @ new_inferred_e 
          )
          else state, new_inferred_x @ new_inferred_e
        )
        | _ -> (
          let state, new_inferred_c = inference_rules c state in
          state, new_inferred_x @ new_inferred_c @ new_inferred_e
        )
      )
      (* No pattern *)
      | Term.Binary{f=_;x;y;_} -> (
        let state, new_inferred_x = inference_rules x state in
        let state, new_inferred_y = inference_rules y state in
        state, new_inferred_x @ new_inferred_y
      )
      | Term.Unary{f=_;x;_} -> inference_rules x state
      | Term.Ite{c;e;t;_} -> (
        let state, new_inferred_c = inference_rules c state in
        let state, new_inferred_e = inference_rules e state in
        let state, new_inferred_t = inference_rules t state in
        state, new_inferred_c @ new_inferred_e @ new_inferred_t
      )
      | _ -> state, []
    in
    Sse_options.Logger.debug ~level:4 "query: %a" Term.pp query;
    let state, new_inferred = inference_rules query state in
    match new_inferred with
    | [] ->  state, query
    | infered -> (
      Query_stats.add_infered_variables();
      Sse_options.Logger.debug ~level:4 "query after inference: %a" Term.pp query;
      state, List.fold_left (fun query atom -> Expr.binary Expr.And query atom) query infered
    ) *)

  let compute_query_complexity query = 
    let rec get_nb_faults_in_expr e = 
      match e with
      | Term.Binary{f=_;x;y;_} -> (
          let nb_x = get_nb_faults_in_expr x in
          let nb_y = get_nb_faults_in_expr y in
          nb_x + nb_y
        )
        | Term.Unary{f=_;x;_} -> get_nb_faults_in_expr x
        | Term.Ite{c;e;t;_} -> (
          let nb_c = get_nb_faults_in_expr c in
          let nb_e = get_nb_faults_in_expr e in
          let nb_t = get_nb_faults_in_expr t in
          nb_c + nb_e + nb_t + 1
        )
        | Term.Load{hash=_;len=_;dir=_;addr;_} -> get_nb_faults_in_expr addr
        | _ -> 0
    in
    let res = List.fold_left (fun x y -> x + y) 0 (List.map get_nb_faults_in_expr query) in 
    (* Sse_options.Logger.info "%d" res;  *)
    res
end

module type Prover = sig

  type result = Sat | Unsat | Unknown

  type term
  type memory

  type access =
    | Select of term * int
    | Store  of term

  val put : t -> unit
  val set_memory : addr:Z.t -> Z.t -> unit

  val neq : term -> Z.t -> unit

  val bind : Expr.t -> t -> term
  val get : Expr.t -> term

  val check_sat : unit -> result

  val get_memory : unit -> memory * access Queue.t

  val get_value : term -> Z.t

  val get_at : memory -> term -> Z.t

  val succ : term -> term

  val close : unit -> unit

end

module Engine (P : Prover) = struct

  open P

  type result = Unknown | Unsat | Sat of t

  let extract_memory state =
    match get_memory () with
    | exception Not_found -> BiTbl.create 0, state.constraints
    | array, history ->
      let dirty = BiTbl.create 32 and memory = BiTbl.create 32 in
      let addr_space = Kernel_options.Machine.word_size () in
      let constraints = Queue.fold
          (fun constraints -> function
             | Select (index, len) ->
               let z = get_value index in
               let rec fold z index len memory constraints =
                 if len = 0 then constraints
                 else if BiTbl.mem dirty z then
                   fold Z.(z + one) (succ index)
                     (len - 1) memory constraints
                 else
                   let k = get_at array index in
                   let v = Z.to_int k in
                   let constraints = match BiItM.find z state.ilocs with
                     | exception Not_found ->
                       BiTbl.add memory z (Char.unsafe_chr v);
                       constraints
                     | img ->
                      let v' = Loader.read_address img (Z.to_int z) in
                      if v <> v' then
                       Expr.(equal (load 1 LittleEndian
                                      (constant (Bv.create z addr_space))
                                      Memory.Unknown)
                               (constant (Bv.of_int ~size:byte_size v'))) ::
                       constraints
                      else  constraints in
                   fold Z.(z + one) (succ index) (len - 1)
                     memory constraints in
               fold z index len memory constraints
             | Store index ->
               let z = get_value index in
               BiTbl.replace dirty z ();
               constraints)
          state.constraints history in
      memory, constraints

  let extract_vars state =
    let vars = BvTbl.create 32 in
    S.iter (fun _ -> List.iter (fun bv ->
        match get bv with
        | exception Not_found -> ()
        | x -> BvTbl.add vars bv @@ get_value x))
      state.fvariables;
    vars

  let rec force_lazy_init =
    fun constraints state ->
    if constraints == state.constraints = false then
      match constraints with
      | [] -> ()
      | eq :: constraints ->
        let addr, value =
          match eq with
          | Binary
              { f = Eq; x = Load { addr = Cst addr; _ }; y = Cst value; _ }
            ->
            (Bitvector.value_of addr, Bitvector.value_of value)
          | _ -> assert false
        in
        P.set_memory ~addr value;
        force_lazy_init constraints state

  let enumerate =
    let rec iter state e expr size n enum =
      if n = 0 then enum else (
        Query_stats.add_enumerate_sent ();
        match check_sat () with
        | Unknown -> let _ = Query_stats.add_unknown_solver () in enum
        | Unsat   -> enum
        | Sat     ->
          let memory, constraints = extract_memory state in
          if constraints == state.constraints = false then begin
            force_lazy_init constraints state;
            iter { state with constraints } e expr size n enum
          end else
            let x = get_value expr in
            let b = Bv.create x size in
            let state' = { state with
                           constraints = Expr.equal e (Expr.constant b)
                                         :: constraints;
                           model = extract_vars state, memory } in
            neq expr x;
            iter state e expr size (n - 1) ((b, state') :: enum)
      ) in
    fun e ?(n=(1 lsl Expr.sizeof e) - 1) ?(except=[]) state ->
      let size = Expr.sizeof e in
      let expr = bind e state in
      (* let init =
        let value = Model.eval state.model e in
        let bv = Bv.create value size in
        if List.mem bv except then []
        else begin
          neq expr value;
          [ bv, { state with constraints = Expr.equal e (Expr.constant bv)
                                           :: state.constraints } ]
        end in *)
        let init = [] in
      List.iter (fun bv -> neq expr (Bitvector.value_of bv)) except;
      (* iter state e expr size (n - 1) init *)
      iter state e expr size n init

  let check_sat =
    let rec check_sat_true state =
      match check_sat () with
      | Unknown -> Unknown
      | Unsat   -> Unsat
      | Sat     ->
        let memory, constraints = extract_memory state in
        if constraints == state.constraints = false then begin
          force_lazy_init constraints state;
          check_sat_true { state with constraints }
        end else Sat { state with model = extract_vars state, memory } in
    fun ?(such_that=Expr.one) state ->
      put { state with constraints = such_that :: state.constraints };
      check_sat_true state

  let close () = close ()
end

module Native () : Prover = struct
  module Solver = Bitwuzla.Incremental ()
  open Solver

  type nonrec result = result = Sat | Unsat | Unknown

  type memory = (bv, bv) ar term
  type nonrec term = bv term

  type access =
    | Select of term * int
    | Store  of term


  module Context = struct
    type t = {
      st_cons   : bv sort NiTbl.t;
      bv_cons   : term BvTbl.t;
      ax_cons   : memory AxTbl.t;
      history   : access Queue.t
    }

    let create () =
      {
        st_cons  = NiTbl.create 8;
        bv_cons  = BvTbl.create 128;
        ax_cons  = AxTbl.create 64;
        history  = Queue.create ()
      }
  end

  open Context

  let visit_sort ctx sz =
    try NiTbl.find ctx.st_cons sz
    with Not_found ->
      let st = Sort.bv sz in
      NiTbl.add ctx.st_cons sz st;
      st
  and visit_select =
    let rec iter len concat index array res =
      if len = 0 then res else
        iter (len - 1) concat (Term.Bv.succ index) array
          (concat (Term.Ar.select array index) res) in
    fun len dir index array ->
      let concat = match dir with
        | Machine.LittleEndian -> Term.Bv.append
        | Machine.BigEndian -> assert false (* TODO *) in
      iter (len - 1) concat (Term.Bv.succ index) array
        (Term.Ar.select array index)
  and visit_binop =
    let open Expr in
    function
    | Plus   -> Term.Bv.add
    | Minus  -> Term.Bv.sub
    | Mul    -> Term.Bv.mul
    | Udiv   -> Term.Bv.udiv
    | Sdiv   -> Term.Bv.sdiv
    | Umod   -> Term.Bv.urem
    | Smod   -> Term.Bv.srem
    | Or     -> Term.Bv.logor
    | And    -> Term.Bv.logand
    | Xor    -> Term.Bv.logxor
    | Concat -> Term.Bv.append
    | Lsl    -> Term.Bv.shift_left
    | Lsr    -> Term.Bv.shift_right_logical
    | Asr    -> Term.Bv.shift_right
    | Rol    -> Term.Bv.rotate_left
    | Ror    -> Term.Bv.rotate_right
    | Eq     -> Term.equal
    | Diff   -> Term.distinct
    | Ule    -> Term.Bv.ule
    | Ult    -> Term.Bv.ult
    | Uge    -> Term.Bv.uge
    | Ugt    -> Term.Bv.ugt
    | Sle    -> Term.Bv.sle
    | Slt    -> Term.Bv.slt
    | Sge    -> Term.Bv.sge
    | Sgt    -> Term.Bv.sgt
  let rec visit_bv ctx bv =
    try BvTbl.find ctx.bv_cons bv
    with Not_found ->
      let e = match bv with
        | Var { name; size; _ } ->
          Term.const (visit_sort ctx size) name
        | Load { len; dir; addr; label; _ } ->
          let sort  = visit_sort ctx (Expr.sizeof addr) in
          let index = visit_bv ctx addr
          and array = visit_ax ctx sort label in
          Queue.push (Select (index, len)) ctx.history;
          visit_select len dir index array
        | Cst bv ->
          let size = Bv.size_of bv and value = Bv.value_of bv in
          let st = visit_sort ctx size in
          Term.Bv.of_z st value
        | Unary { f=Not; x; _ } ->
          Term.Bv.lognot (visit_bv ctx x)
        | Unary { f=Minus; x; _ } ->
          Term.Bv.neg (visit_bv ctx x)
        | Unary { f=Uext n; x; _ } ->
          Term.Bv.zero_extend n (visit_bv ctx x)
        | Unary { f=Sext n; x; _ } ->
          Term.Bv.sign_extend n (visit_bv ctx x)
        | Unary { f=Restrict { lo; hi }; x; _ } ->
          Term.Bv.extract ~hi ~lo (visit_bv ctx x)
        | Binary { f; x; y; _ } ->
          (visit_binop f) (visit_bv ctx x) (visit_bv ctx y)
        | Ite { c; t; e; _ } ->
          Term.ite (visit_bv ctx c) (visit_bv ctx t) (visit_bv ctx e) in
      BvTbl.add ctx.bv_cons bv e;
      e
  and visit_ax ctx index ax =
    try AxTbl.find ctx.ax_cons ax
    with Not_found ->
      let a = match ax with
        | Memory.Unknown ->
          Term.const
            (Sort.ar index (visit_sort ctx byte_size))
            Suid.(to_string zero)
        | Memory.Source { over; _ } ->
          visit_ax ctx index over
        | Memory.Layer { addr; bytes; over; _ } ->
          let base = visit_bv ctx addr
          and array = visit_ax ctx index over in
          NiMap.fold (fun i byte array ->
              let x = visit_bv ctx byte
              and index = Term.Bv.(add base (of_int index i)) in
              Queue.push (Store index) ctx.history;
              Term.Ar.store array index x) bytes array in
      AxTbl.add ctx.ax_cons ax a;
      a

  let ctx = create ()

  let put state =
    List.iter (fun bl -> assert' (visit_bv ctx bl)) state.constraints
  let bind e state =
    let e = visit_bv ctx e in
    put state;
    e
  let get e = BvTbl.find ctx.bv_cons e

  let set_memory ~addr value =
    let sort =
      match Queue.top ctx.history with Select (t, _) | Store t -> Term.sort t
    in
    assert'
      (Term.equal
         (Term.Ar.select
            (AxTbl.find ctx.ax_cons Memory.Unknown)
            (Term.Bv.of_z sort addr))
         (Term.Bv.of_z (visit_sort ctx byte_size) value))

  let neq e x = assert' (Term.distinct e (Term.Bv.of_z (Term.sort e) x))

  let get_memory () = AxTbl.find ctx.ax_cons Memory.Unknown, ctx.history

  let get_at ar x = Term.Bv.assignment @@
    get_value (Term.Ar.select ar x)
  let get_value x = Term.Bv.assignment @@ get_value x

  let succ = Term.Bv.succ

  let check_sat () = check_sat ()

  let close () = unsafe_close ()
end

module Printer = struct
  open Expr

  type term = string * int

  type access = Select of term * int | Store of term

  and def = Bl of Expr.t | Bv of Expr.t | Ax of Memory.t

  and t = {
    mutable id : Suid.t;
    bv_decl : string BvTbl.t;
    bl_cons : string BvTbl.t;
    bv_cons : string BvTbl.t;
    ax_cons : string AxTbl.t;
    ordered_defs : def Queue.t;
    ordered_mem : access Queue.t;
    word_size : int;
  }

  let create ?(word_size = Kernel_options.Machine.word_size ()) ~next_id () =
    let bv_cons = BvTbl.create 128 and bl_cons = BvTbl.create 32 in
    BvTbl.add bl_cons Expr.zero "false";
    BvTbl.add bv_cons Expr.zero "#b0";
    BvTbl.add bl_cons Expr.one "true";
    BvTbl.add bv_cons Expr.one "#b1";
    {
      id = next_id;
      bv_decl = BvTbl.create 16;
      bl_cons;
      bv_cons;
      ax_cons = AxTbl.create 64;
      ordered_defs = Queue.create ();
      ordered_mem = Queue.create ();
      word_size;
    }

  let pp_int_as_offset size ppf i = pp_bv ppf i size

  let once = ""

  let rec visit_bl ctx bl =
    try
      if BvTbl.find ctx.bl_cons bl == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bl_cons bl name)
    with Not_found -> (
      match bl with
      | Cst _ -> ()
      | Load _ (* cannot be a bl<1> *) -> assert false
      | Unary { f = Not; x; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx x;
          Queue.push (Bl bl) ctx.ordered_defs
      | Binary { f = And | Or; x; y; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx x;
          visit_bl ctx y;
          Queue.push (Bl bl) ctx.ordered_defs
      | Binary
          {
            f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt;
            x;
            y;
            _;
          } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bv ctx x;
          visit_bv ctx y;
          Queue.push (Bl bl) ctx.ordered_defs
      | Ite { c; t; e; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx c;
          visit_bl ctx t;
          visit_bl ctx e;
          Queue.push (Bl bl) ctx.ordered_defs
      | Var _ | Unary _ | Binary _ -> visit_bv ctx bl)

  and visit_bv ctx bv =
    try
      if BvTbl.find ctx.bv_cons bv == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bv_cons bv name)
    with Not_found -> (
      match bv with
      | Var { name; size; _ } ->
          BvTbl.add ctx.bv_cons bv name;
          if size = 1 then
            BvTbl.add ctx.bl_cons bv (Printf.sprintf "(= %s #b1)" name);
          BvTbl.add ctx.bv_decl bv
            (Format.sprintf "(declare-const %s (_ BitVec %d))" name size)
      | Load { len; addr; label; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx addr;
          visit_bv ctx addr;
          visit_ax ctx label;
          if len > 1 then visit_ax ctx label;
          Queue.push (Bv bv) ctx.ordered_defs;
          Queue.push
            (Select ((BvTbl.find ctx.bv_cons addr, Expr.sizeof addr), len))
            ctx.ordered_mem
      | Cst _ ->
          BvTbl.add ctx.bv_cons bv once;
          Queue.push (Bv bv) ctx.ordered_defs
      | Unary { x; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary
          { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ }
        ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bl ctx bv;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary
          {
            f = Rol | Ror;
            x;
            y = (Load _ | Unary _ | Binary _ | Ite _) as y;
            _;
          } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          visit_bv ctx x;
          visit_bv ctx y;
          visit_bv ctx y;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary { x; y; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          visit_bv ctx y;
          Queue.push (Bv bv) ctx.ordered_defs
      | Ite { c; t; e; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bl ctx c;
          visit_bv ctx t;
          visit_bv ctx e;
          Queue.push (Bv bv) ctx.ordered_defs)

  and visit_ax ctx ax =
    try
      if AxTbl.find ctx.ax_cons ax == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        AxTbl.replace ctx.ax_cons ax name)
    with Not_found -> (
      match ax with
      | Memory.Unknown -> AxTbl.add ctx.ax_cons ax Suid.(to_string zero)
      | Memory.Source { over; _ } ->
          AxTbl.add ctx.ax_cons ax once;
          visit_ax ctx over;
          Queue.push (Ax ax) ctx.ordered_defs
      | Memory.Layer { addr; bytes; over; _ } ->
          AxTbl.add ctx.ax_cons ax once;
          visit_bv ctx addr;
          visit_bv ctx addr;
          NiMap.iter (fun _ bv -> visit_bv ctx bv) bytes;
          visit_ax ctx over;
          Queue.push (Ax ax) ctx.ordered_defs;
          let index = BvTbl.find ctx.bv_cons addr in
          NiMap.iter
            (fun i _ ->
              let index =
                Format.asprintf "(bvadd %s %a)" index
                  (pp_int_as_offset ctx.word_size)
                  (Z.of_int i)
              in
              Queue.push (Store (index, Expr.sizeof addr)) ctx.ordered_mem)
            bytes)

  let pp_unop ppf (op : Term.unary Term.operator) =
    match op with
    | Not -> Format.pp_print_string ppf "bvnot"
    | Minus -> Format.pp_print_string ppf "bvneg"
    | Uext n -> Format.fprintf ppf "(_ zero_extend %d)" n
    | Sext n -> Format.fprintf ppf "(_ sign_extend %d)" n
    | Restrict { Interval.hi; lo } ->
        Format.fprintf ppf "(_ extract %d %d)" hi lo

  let pp_binop =
    let string_of_binop (op : Term.binary Term.operator) =
      match op with
      | Plus -> "bvadd"
      | Minus -> "bvsub"
      | Mul -> "bvmul"
      | Udiv -> "bvudiv"
      | Sdiv -> "bvsdiv"
      | Umod -> "bvurem"
      | Smod -> "bvsrem"
      | Or -> "bvor"
      | And -> "bvand"
      | Xor -> "bvxor"
      | Concat -> "concat"
      | Lsl -> "bvshl"
      | Lsr -> "bvlshr"
      | Asr -> "bvashr"
      | Rol -> "rotate_left"
      | Ror -> "rotate_right"
      | Eq -> "="
      | Diff -> assert false
      | Ule -> "bvule"
      | Ult -> "bvult"
      | Uge -> "bvuge"
      | Ugt -> "bvugt"
      | Sle -> "bvsle"
      | Slt -> "bvslt"
      | Sge -> "bvsge"
      | Sgt -> "bvsgt"
    in
    fun ppf f -> Format.pp_print_string ppf (string_of_binop f)

  let rec print_bl ctx ppf bl =
    try
      let name = BvTbl.find ctx.bl_cons bl in
      if name == once then print_bl_no_cons ctx ppf bl
      else Format.pp_print_string ppf name
    with Not_found ->
      Format.pp_print_string ppf "(= ";
      Format.pp_print_space ppf ();
      print_bv ctx ppf bl;
      Format.pp_print_string ppf " #b1)"

  and print_bl_no_cons ctx ppf bl =
    match bl with
    | Cst _ (* true and false should already be in the cache *)
    | Load _ (* cannot be a bl<1> *) ->
        assert false
    | Unary { f = Not; x; _ } ->
        Format.pp_print_string ppf "(not";
        Format.pp_print_space ppf ();
        print_bl ctx ppf x;
        Format.pp_print_char ppf ')'
    | Binary { f = (And | Or) as f; x; y; _ } ->
        Format.pp_print_char ppf '(';
        (Format.pp_print_string ppf
        @@ match f with And -> "and" | Or -> "or" | _ -> assert false);
        Format.pp_print_space ppf ();
        print_bl ctx ppf x;
        Format.pp_print_space ppf ();
        print_bl ctx ppf y;
        Format.pp_print_char ppf ')'
    | Binary { f = Diff; x; y; _ } ->
        Format.pp_print_string ppf "(not";
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "(=";
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_space ppf ();
        print_bv ctx ppf y;
        Format.pp_print_string ppf "))"
    | Binary
        {
          f = (Eq | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt) as f;
          x;
          y;
          _;
        } ->
        Format.pp_print_char ppf '(';
        pp_binop ppf f;
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_space ppf ();
        print_bv ctx ppf y;
        Format.pp_print_char ppf ')'
    | Ite { c; t; e; _ } ->
        Format.pp_print_string ppf "(ite";
        Format.pp_print_space ppf ();
        print_bl ctx ppf c;
        Format.pp_print_space ppf ();
        print_bl ctx ppf t;
        Format.pp_print_space ppf ();
        print_bl ctx ppf e;
        Format.pp_print_char ppf ')'
    | Var _ | Unary _ | Binary _ ->
        Format.pp_print_string ppf "(=";
        Format.pp_print_space ppf ();
        print_bv ctx ppf bl;
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "#b1)"

  and print_bv ctx ppf bv =
    let name = BvTbl.find ctx.bv_cons bv in
    if name == once then print_bv_no_cons ctx ppf bv
    else Format.pp_print_string ppf name

  and print_bv_no_cons ctx ppf bv =
    match bv with
    | Var { name; _ } -> Format.pp_print_string ppf name
    | Load { len = 1; addr; label; _ } ->
        Format.pp_print_string ppf "(select";
        Format.pp_print_space ppf ();
        print_ax ctx ppf label;
        Format.pp_print_space ppf ();
        print_bv ctx ppf addr;
        Format.pp_print_char ppf ')'
    | Load { len; dir; addr; label; _ } ->
        Format.pp_print_string ppf "(concat";
        print_multi_select dir ppf len
          (AxTbl.find ctx.ax_cons label)
          (BvTbl.find ctx.bv_cons addr)
          (Expr.sizeof addr);
        Format.pp_print_char ppf ')'
    | Cst bv ->
        let size = Bv.size_of bv and value = Bv.value_of bv in
        pp_bv ppf value size
    | Unary { f; x; _ } ->
        Format.pp_print_char ppf '(';
        pp_unop ppf f;
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_char ppf ')'
    | Binary { f = Eq | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ } ->
        Format.pp_print_string ppf "(ite";
        Format.pp_print_space ppf ();
        print_bl ctx ppf bv;
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "#b1";
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "#b0)"
    | Binary { f = Diff; x; y; _ } ->
        Format.pp_print_string ppf "(ite (=";
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_space ppf ();
        print_bv ctx ppf y;
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "#b0";
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "#b1)"
    | Binary { f = (Rol | Ror) as f; x; y = Cst bv; _ } ->
        Format.pp_print_string ppf "((_";
        Format.pp_print_space ppf ();
        pp_binop ppf f;
        Format.pp_print_space ppf ();
        Z.pp_print ppf (Bv.value_of bv);
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_char ppf ')'
    | Binary { f = (Rol | Ror) as f; x; y; _ } ->
        Format.pp_print_string ppf "(bvor";
        Format.pp_print_space ppf ();
        Format.pp_print_char ppf '(';
        pp_binop ppf
          (match f with Rol -> Lsl | Ror -> Lsr | _ -> assert false);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons x);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons y);
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ();
        Format.pp_print_char ppf '(';
        pp_binop ppf
          (match f with Rol -> Lsr | Ror -> Lsl | _ -> assert false);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons x);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf "(bvsub";
        Format.pp_print_space ppf ();
        pp_int_as_bv ppf (Expr.sizeof x) (Expr.sizeof x);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons y);
        Format.pp_print_string ppf ")))"
    | Binary { f; x; y; _ } ->
        Format.pp_print_char ppf '(';
        pp_binop ppf f;
        Format.pp_print_space ppf ();
        print_bv ctx ppf x;
        Format.pp_print_space ppf ();
        print_bv ctx ppf y;
        Format.pp_print_char ppf ')'
    | Ite { c; t; e; _ } ->
        Format.pp_print_string ppf "(ite";
        Format.pp_print_space ppf ();
        print_bl ctx ppf c;
        Format.pp_print_space ppf ();
        print_bv ctx ppf t;
        Format.pp_print_space ppf ();
        print_bv ctx ppf e;
        Format.pp_print_char ppf ')'

  and print_ax ctx ppf ax =
    let name = AxTbl.find ctx.ax_cons ax in
    if name == once then print_ax_no_cons ctx ppf ax
    else Format.pp_print_string ppf name

  and print_ax_no_cons ctx ppf ax =
    match ax with
    | Memory.Unknown -> Suid.pp ppf Suid.zero
    | Memory.Source { over; _ } -> print_ax ctx ppf over
    | Memory.Layer { addr; bytes; over; pop = 1; _ } ->
        Format.pp_print_string ppf "(store";
        Format.pp_print_space ppf ();
        print_ax ctx ppf over;
        Format.pp_print_space ppf ();
        print_bv ctx ppf addr;
        Format.pp_print_space ppf ();
        print_bv ctx ppf (snd (NiMap.choose bytes));
        Format.pp_print_char ppf ')'
    | Memory.Layer { addr; bytes; over; pop; _ } ->
        for _ = 1 to pop do
          Format.pp_print_string ppf "(store";
          Format.pp_print_space ppf ()
        done;
        print_ax ctx ppf over;
        let addr_space = Expr.sizeof addr
        and idx = BvTbl.find ctx.bv_cons addr in
        NiMap.iter
          (fun i bv ->
            Format.pp_print_space ppf ();
            if 0 = i then (
              Format.pp_print_string ppf idx;
              Format.pp_print_space ppf ())
            else (
              Format.pp_print_string ppf "(bvadd";
              Format.pp_print_space ppf ();
              Format.pp_print_string ppf idx;
              Format.pp_print_space ppf ();
              pp_bv ppf (Z.of_int i) addr_space;
              Format.pp_print_char ppf ')';
              Format.pp_print_space ppf ());
            print_bv ctx ppf bv;
            Format.pp_print_char ppf ')')
          bytes

  and print_multi_select =
    let rec print_multi_select_le ppf len ax bv size =
      if len = 1 then Format.fprintf ppf " (select@ %s@ %s)" ax bv
      else
        let len = len - 1 in
        Format.fprintf ppf " (select@ %s@ (bvadd@ %s@ " ax bv;
        pp_int_as_bv ppf len size;
        Format.pp_print_string ppf "))";
        print_multi_select_le ppf len ax bv size
    in
    let rec print_multi_select_be i ppf len ax bv size =
      if i = 0 then (
        Format.fprintf ppf "@ (select@ %s@ %s)" ax bv;
        print_multi_select_be 1 ppf len ax bv size)
      else if i < len then (
        Format.fprintf ppf " (select@ %s@ (bvadd@ %s@ " ax bv;
        pp_int_as_bv ppf i size;
        Format.pp_print_string ppf "))";
        print_multi_select_be (i + 1) ppf len ax bv size)
    in
    function
    | LittleEndian -> print_multi_select_le
    | BigEndian -> print_multi_select_be 0

  let pp_print_decls ppf ctx =
    BvTbl.iter
      (fun _ decl ->
        Format.pp_print_string ppf decl;
        Format.pp_print_space ppf ())
      ctx.bv_decl;
    if Queue.is_empty ctx.ordered_mem = false then
      let addr_space = Kernel_options.Machine.word_size () in
      Format.fprintf ppf
        "(declare-const %a (Array (_ BitVec %d) (_ BitVec %d)))@ " Suid.pp
        Suid.zero addr_space byte_size

  let pp_print_defs ppf ctx =
    Queue.iter
      (function
        | Bl bl ->
            let name = BvTbl.find ctx.bl_cons bl in
            if name != once then (
              Format.fprintf ppf "(define-fun %s () Bool " name;
              print_bl_no_cons ctx ppf bl;
              Format.fprintf ppf ")@ ")
        | Bv bv ->
            let name = BvTbl.find ctx.bv_cons bv in
            if name != once then (
              Format.fprintf ppf "(define-fun %s () (_ BitVec %d) " name
                (Expr.sizeof bv);
              print_bv_no_cons ctx ppf bv;
              Format.fprintf ppf ")@ ")
        | Ax ax ->
            let name = AxTbl.find ctx.ax_cons ax in
            if name != once then (
              Format.fprintf ppf
                "(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) " name
                (Kernel_options.Machine.word_size ())
                byte_size;
              print_ax_no_cons ctx ppf ax;
              Format.fprintf ppf ")@ "))
      ctx.ordered_defs

end


module Smt2 () : Prover = struct

  type result = Sat | Unsat | Unknown

  type memory = unit

  type term = Printer.term

  type access = Printer.access = Select of term * int | Store of term

  let put (ctx : Printer.t) ppf constraints =
    Format.pp_open_vbox ppf 0;
    (* visit assertions *)
    List.iter (Printer.visit_bl ctx) constraints;
    (* print declarations *)
    Printer.pp_print_decls ppf ctx;
    (* print assertions *)
    Format.pp_open_hovbox ppf 0;
    Printer.pp_print_defs ppf ctx;
    List.iter
      (fun bl ->
        Format.pp_print_string ppf "(assert ";
        Printer.print_bl ctx ppf bl;
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ())
      constraints;
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()

  let session =
    Solver.start (* ~stdlog:stderr *)
      (Formula_options.Solver.Timeout.get ())
      (Formula_options.Solver.get ())

  let ctx = ref None

  let bind fid e constraints =
    let ctx =
      let x = Printer.create ~next_id:fid () in
      ctx := Some x;
      x
    in
    Printer.visit_bv ctx e;
    Printer.visit_bv ctx e;
    put ctx session.formatter constraints;
    (BvTbl.find ctx.bv_cons e, Expr.sizeof e)

  let bind e t =
    bind t.fid e t.constraints
  
  let put fid constraints =
    ctx := Some (Printer.create ~next_id:fid ());
    put (Option.get !ctx) session.formatter constraints

  let put t =
    put t.fid t.constraints
  
  let get e = (BvTbl.find (Option.get !ctx).bv_cons e, Expr.sizeof e)

  let set_memory ~addr y =
    Solver.put session
      (fun ppf () ->
        Format.fprintf ppf "(assert (= (select %s " Suid.(to_string zero);
        Format.pp_print_char ppf ' ';
        pp_bv ppf addr (Option.get !ctx).Printer.word_size;
        Format.pp_print_string ppf ") ";
        pp_bv ppf y 8;
        Format.pp_print_string ppf "))")
      ()

  let neq (e, s) x =
    Solver.put session
      (fun ppf () ->
        Format.fprintf ppf "(assert (not (= %s " e;
        pp_bv ppf x s;
        Format.pp_print_string ppf ")))")
      ()

  let get_memory () = ((), (Option.get !ctx).ordered_mem)

  let get_at () (x, _) =
    Solver.get_value session
      (fun ppf -> Format.fprintf ppf "(select %s %s)" Suid.(to_string zero))
      x

  let get_value (x, _) = Solver.get_value session Format.pp_print_string x

  let succ (x, s) =
    (Format.asprintf "(bvadd %s %a)" x (Printer.pp_int_as_offset s) Z.one, s)

  let check_sat () =
    match Solver.check_sat session with
    | UNKNOWN -> Unknown
    | UNSAT -> Unsat
    | SAT -> Sat

  let close () = Solver.close session
end

let assume cond' ~such_that state =
  Sse_options.Logger.debug ~level:6 "Assume query:";
  Sse_options.Logger.debug ~level:6 "%a" Term.pp cond';
  Sse_options.Logger.debug ~level:6 "Assume state constrains:\n%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Term.pp) state.constraints;

  let state = { state with constraints = cond' :: state.constraints } in
  (* if Z.zero = Model.eval state.model cond then *)
    let start_time = Unix.time () in

    (* Smt2 solver not implemented anymore with injection on demand, TODO but complicated for first try *)
    (* if not (Sse_options.NativeSolver.get ()) then assert false;
    let module P = (val (module Native () : Prover)) in *)
    let module P = (val (if Sse_options.NativeSolver.get () then
      (module Native () : Prover)
      else (module Smt2 () : Prover))) in
    
    let open Engine (P) in
    let r = match check_sat ~such_that state with
      | Unknown -> let _ = Query_stats.add_unknown_solver () in None
      | Unsat -> None
      | Sat state -> Some state in
    close ();
    Query_stats.add_solver_time (Unix.time () -. start_time);
    Query_stats.add_solver_query ();
    Query_stats.add_assume_sent ();
    r
  (* else
    Some state *)

let enumerate =
  let with_solver e ?n ?except state =
    Sse_options.Logger.debug ~level:5 "Enumerate state query:\n%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Term.pp) state.constraints;
    let start_time = Unix.time () in

    (* Smt2 solver not implemented anymore with injection on demand, TODO but complicated for first try *)
    (* if not (Sse_options.NativeSolver.get ()) then assert false;
    let module P = (val (module Native () : Prover)) in *)
    let module P = (val (if Sse_options.NativeSolver.get () then
      (module Native () : Prover)
      else (module Smt2 () : Prover))) in
    
    let open Engine (P) in
    let r = enumerate e ?n ?except state in
    close ();
    Query_stats.add_solver_time (Unix.time () -. start_time);
    Query_stats.add_solver_query ();
    r in
  fun e ?n ?(except = []) state ->
    match e, n with
    | Expr.Cst bv, _ when List.mem bv except = false -> [ bv, state ]
    | Expr.Cst _, _ -> []
    (* | _, Some 1 ->
      let size = Expr.sizeof e in
      let value = Model.eval state.model e in
      let bv = Bv.create value size in
      if List.mem bv except then
        with_solver e ?n ~except state
      else
        [ bv, { state with constraints = Expr.equal e (Expr.constant bv)
                                         :: state.constraints } ] *)
    | _, _ ->
      with_solver e ?n ~except state


let assume expr ?(such_that=Dba.Expr.one) ?(fault_check=Dba.Expr.one) is_faulted dual =
  match is_faulted with
  | Both -> assert false
  | _ -> ()
  ;

  Query_stats.add_query ();
  Query_stats.add_total_assume ();
  Sse_options.Logger.debug ~level:4 "Expression in assume %a" Dba_printer.Ascii.pp_expr expr;
  Sse_options.Logger.debug ~level:4 "such that %a" Dba_printer.Ascii.pp_expr such_that;

  (* Sse_options.Logger.info "VSYMBOLS:";
  S.iter (fun str v -> Sse_options.Logger.info "%s : %a" str Term.pp v ) dual.faulted_state.vsymbols;
  Sse_options.Logger.info "END VSYMBOLS:";
  Sse_options.Logger.info "FVARIABLES:";
  S.iter (fun str vl -> List.iter (fun v -> Sse_options.Logger.info "%s : %a" str Term.pp v ) vl) dual.faulted_state.fvariables;
  Sse_options.Logger.info "END FVARIABLES:";
   *)

  (* Update faulted predicate always if in Normal query *)
  let dual = match is_faulted with
    | Normal -> (
      let e_f, dual = Translate.expr Faulted dual expr in
      (* No SubFualt Simplification if the query is trivial *)
      if not (Expr.is_equal e_f Expr.one) && not (Expr.is_equal e_f Expr.zero) 
        then (
          let dual, _ = match Sse_options.SubFaultsSimplification.get () with
            | "pattern-match" -> Sse_options.Logger.error "SFS pattern-match abandonned option"; assert false
            | "free-bi" -> Translate.add_non_free_bi e_f Faulted dual
            | "memoization" -> Translate.sfs_memoization e_f Faulted dual
            | _ -> dual, Expr.one
          in
          let fault_check_f, dual = Translate.expr Faulted dual fault_check in
          let query_f = Expr.logand e_f fault_check_f in
          update_faulted_constrains dual query_f
        )
        else dual
    )
    | _ -> dual
  in

  let e_t, dual = Translate.expr is_faulted dual expr in
  
  (* Test trivial cases before adding fault check *)
  if Expr.is_equal e_t Expr.one then (
    Query_stats.add_trivial_true ();
    Sse_options.Logger.debug ~level:8 "Assume normal state query:\n%a" (Format.pp_print_list Term.pp) dual.normal_state.constraints;
    Sse_options.Logger.debug ~level:8 "Assume faulted state query:\n%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Term.pp) dual.faulted_state.constraints;
    Some dual
  )
  else if Expr.is_equal e_t Expr.zero then (
    Query_stats.add_trivial_false ();
    None
  )
  else
    let such_that_t, dual = Translate.expr is_faulted dual such_that in
    let fault_check_t, dual = Translate.expr is_faulted dual fault_check in
    (* SubFualt simplification option *)
    let dual, sfs_such_that_t = match Sse_options.SubFaultsSimplification.get () with
      | "pattern-match" -> Sse_options.Logger.error "SFS pattern-match abandonned option"; assert false
      | "free-bi" -> Translate.add_non_free_bi e_t is_faulted dual
      | "memoization" -> Translate.sfs_memoization e_t is_faulted dual
      | _ -> dual, Expr.one
    in
    let is_unsafe = (get_state_from_dual is_faulted dual).is_path_unsafe in

    Sse_options.Logger.debug ~level:4 "initial query condition %a" Term.pp e_t;
    Sse_options.Logger.debug ~level:4 "such that %a" Term.pp such_that_t;
    Sse_options.Logger.debug ~level:4 "sfs such that %a" Term.pp sfs_such_that_t;
    Sse_options.Logger.debug ~level:4 "sfs is unsafe ? %b" is_unsafe;
    Sse_options.Logger.debug ~level:4 "fault_check_t %a" Term.pp fault_check_t;

    let query_t = Expr.logand e_t fault_check_t in

    if Sse_options.ComputeQueryComplexity.get () then (
      let constraints = match is_faulted with
        | Normal -> dual.normal_state.constraints
        | Faulted -> dual.faulted_state.constraints
        | Both -> assert false
      in
      let nb_faults = Translate.compute_query_complexity (query_t :: such_that_t :: constraints) in
      Sse_options.Logger.debug ~level:4 "QUERY COMPLEXITY: %d" nb_faults;
      Query_stats.update_min_query_complexity nb_faults;
      Query_stats.update_max_query_complexity nb_faults;
      Query_stats.update_sum_query_complexity nb_faults;
      Query_stats.add_nb_query_complexity ();
    );
    (* match Sse_options.DoInference.get () with
    | false -> assume query ~such_that t
    | true -> (
      let t, new_query = Translate.infer_from t query in
      assume new_query ~such_that t
    ) *)
    

    let extended_such_that_t = 
      match Sse_options.SFSrecovery.get () with
        | "abort" when is_unsafe-> (
          Query_stats.add_sfs_recovery_triggered();
          such_that_t
        )
        | _ -> Expr.logand such_that_t sfs_such_that_t
    in
    let assume_solver_call assume_func = 
      match is_faulted with 
      | Normal -> (
        let state = dual.normal_state in
        let state_opt = assume_func state in
        match state_opt with
        | None -> None
        | Some s -> (
          Some { dual with normal_state = s }
        )
      )
      | Faulted -> (
        let state = dual.faulted_state in
        let state_opt = assume_func state in
        match state_opt with
        | None -> None
        | Some s -> Some { dual with faulted_state = s }
      )
      | Both -> assert false
    in 
    let result = assume_solver_call (assume query_t ~such_that:extended_such_that_t) in
    if is_unsafe then (
      match result, not (Sse_options.SubFaultsSimplification.is_default()), Sse_options.SFSrecovery.get () with 
      | None, true, "lazy" -> (
        Query_stats.add_sfs_recovery_triggered();
        assume_solver_call (assume query_t ~such_that:such_that_t)
      )
      | _ -> result
    ) else result

let print_contraints e =
  Sse_options.Logger.debug ~level:4 "Constrains:\n%a"
  (Format.pp_print_list ~pp_sep:Format.pp_print_newline Term.pp) e.constraints

let split_on e ?n ?except is_faulted dual =
  Query_stats.add_query ();
  Query_stats.add_total_enumerate ();
  Sse_options.Logger.debug ~level:4 "Expression in enumerate %a" Dba_printer.Ascii.pp_expr e;

  let e, dual = Translate.expr is_faulted dual e in
   match is_faulted with
  | Normal -> let res_list = enumerate e ?n ?except dual.normal_state in
    List.map (fun (bv, t) -> (bv, {dual with normal_state = t })) res_list
  | Faulted -> let res_list = enumerate e ?n ?except dual.faulted_state in
    List.map (fun (bv, t) -> (bv, {dual with faulted_state = t })) res_list
  | Both -> assert false

let concrete_eval e is_faulted dual =
  (* Query_stats.add_query ();  *)
  let e, dual = Translate.expr is_faulted dual e in
  let size = Expr.sizeof e in
  match is_faulted with
  | Normal -> (
    let value = Model.eval dual.normal_state.model e in
    Bv.create value size
  )
  | Faulted -> (
    let value = Model.eval dual.faulted_state.model e in
    Bv.create value size
  )
  | Both -> assert false


let set_faults_to_zero dual is_faulted e =
  (*  HYPOTHESIS: no SFS supoort needed, as it will come as variable is used in query *)
  let _ = Sse_options.Logger.debug ~level:4 "config: %b" (Sse_options.FaultVarsUsedInAdresses.get ()) in
  if Sse_options.FaultVarsUsedInAdresses.get () then
    dual
  else (
    match Translate.get_fault_vars e is_faulted dual with
    | f when SExpr.is_empty f -> dual
    | f -> Sse_options.Logger.debug ~level:4 "Adresse detected and not faulted"; Sse_options.Logger.debug ~level:4 "%a" Term.pp e; SExpr.iter (fun x -> Sse_options.Logger.debug ~level:4 "%a" Term.pp x) f;
    update_dual is_faulted dual (fun s -> { s with constraints = (SExpr.fold (fun atom query -> Expr.binary Expr.And (Expr.binary Expr.Eq atom Expr.zero) query) f Expr.one) :: s.constraints })
  ) 

let assign name e ?(first_fault_happened=false) is_faulted dual =
  let expr, dual = Translate.expr is_faulted dual e in
  (* if e is Load and contains fault => fault = 0 *)
  Sse_options.Logger.debug ~level:4 "assign %a \n%a" Dba_printer.Ascii.pp_expr e Term.pp expr;
  let rec find_loads expr dual = 
    match SExpr.exists (fun a -> Expr.is_equal a expr) (get_state_from_dual is_faulted dual).adresse_history with
    | true -> dual
    | false -> (
      let dual = update_dual is_faulted dual (fun s -> { s with adresse_history = SExpr.add expr s.adresse_history }) in
      match expr with
      | Term.Load{addr;_} -> Sse_options.Logger.debug ~level:4 "load %a" Term.pp addr; set_faults_to_zero dual is_faulted addr
      | Term.Ite{c;t;e;_} -> (
        Sse_options.Logger.debug ~level:4 "ite %a" Term.pp expr;
        let dual = find_loads c dual in
        let dual = find_loads t dual in
        find_loads e dual
      )
      | Term.Binary{x;y;_} -> (
        Sse_options.Logger.debug ~level:4 "binary %a" Term.pp expr;
        let dual = find_loads x dual in
        find_loads y dual
      )
      | Term.Unary{x;_} -> (
        Sse_options.Logger.debug ~level:4 "unary %a" Term.pp expr;
        find_loads x dual
      )
      | _ -> Sse_options.Logger.debug ~level:4 "final %a" Term.pp expr;dual
    )
  in
  if first_fault_happened 
    then assign name expr ~is_faulted (find_loads expr dual)
    else assign name expr ~is_faulted dual

let write ~addr value dir ?(first_fault_happened=false) is_faulted dual =
  let value, dual = Translate.expr is_faulted dual value in
  let addr, dual = Translate.expr is_faulted dual addr in
  (* if addr contains a fault => fault is 0 *)
  if first_fault_happened
    then (
      Sse_options.Logger.debug ~level:4 "write";
      match SExpr.exists (fun a -> Expr.is_equal a addr) (get_state_from_dual is_faulted dual).adresse_history with 
      | true -> write ~addr value dir ~is_faulted dual
      | false -> (
        let dual = update_dual is_faulted dual (fun s -> { s with adresse_history = SExpr.add addr s.adresse_history }) in
        write ~addr value dir ~is_faulted (set_faults_to_zero dual is_faulted addr))
      )
    else write ~addr value dir ~is_faulted dual

let pp_normal ppf dual = Model.pp ppf dual.normal_state.fvariables (Kernel_options.Machine.word_size ()) dual.normal_state.model

let pp_faulted ppf dual = Model.pp ppf dual.faulted_state.fvariables (Kernel_options.Machine.word_size ()) dual.faulted_state.model

let pp_dual ppf dual which_dual = 
  match which_dual with
  | Normal -> pp_normal ppf dual
  | Faulted -> pp_faulted ppf dual
  | Both -> assert false