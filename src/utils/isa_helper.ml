
module X86 = struct
  let defs, notes =
    let info = Dba.VarTag.register in
    let eax  = { Dba.name = "eax"; size = 32; info }
    and ebx  = { Dba.name = "ebx"; size = 32; info }
    and ecx  = { Dba.name = "ecx"; size = 32; info }
    and edx  = { Dba.name = "edx"; size = 32; info }
    and edi  = { Dba.name = "edi"; size = 32; info }
    and esi  = { Dba.name = "esi"; size = 32; info }
    and esp  = { Dba.name = "esp"; size = 32; info }
    and ebp  = { Dba.name = "ebp"; size = 32; info } in
    let cs   = { Dba.name = "cs"; size = 16; info }
    and ds   = { Dba.name = "ds"; size = 16; info }
    and es   = { Dba.name = "es"; size = 16; info }
    and fs   = { Dba.name = "fs"; size = 16; info }
    and gs   = { Dba.name = "gs"; size = 16; info }
    and ss   = { Dba.name = "ss"; size = 16; info } in
    let info = Dba.VarTag.flag Dba.Flag.unspecified in
    let cf   = { Dba.name = "CF"; size = 1; info }
    and pf   = { Dba.name = "PF"; size = 1; info }
    and af   = { Dba.name = "AF"; size = 1; info }
    and zf   = { Dba.name = "ZF"; size = 1; info }
    and sf   = { Dba.name = "SF"; size = 1; info }
    and tf   = { Dba.name = "TF"; size = 1; info }
    and if'  = { Dba.name = "IF"; size = 1; info }
    and df   = { Dba.name = "DF"; size = 1; info }
    and of'  = { Dba.name = "OF"; size = 1; info }
    and iopl = { Dba.name = "IOPL"; size = 2; info }
    and nt   = { Dba.name = "NT"; size = 1; info }
    and rf   = { Dba.name = "RF"; size = 1; info }
    and vm   = { Dba.name = "VM"; size = 1; info }
    and ac   = { Dba.name = "AC"; size = 1; info }
    and vif  = { Dba.name = "VIF"; size = 1; info }
    and vip  = { Dba.name = "VIP"; size = 1; info }
    and id   = { Dba.name = "ID"; size = 1; info } in
    [
      "eax", Dba.LValue.v eax;
      "ebx", Dba.LValue.v ebx;
      "ecx", Dba.LValue.v ecx;
      "edx", Dba.LValue.v edx;
      "edi", Dba.LValue.v edi;
      "esi", Dba.LValue.v esi;
      "esp", Dba.LValue.v esp;
      "ebp", Dba.LValue.v ebp;
      "al", Dba.LValue.restrict eax 0 7;
      "ah", Dba.LValue.restrict eax 8 15;
      "ax", Dba.LValue.restrict eax 0 15;
      "bl", Dba.LValue.restrict ebx 0 7;
      "bh", Dba.LValue.restrict ebx 8 15;
      "bx", Dba.LValue.restrict ebx 0 15;
      "cl", Dba.LValue.restrict ecx 0 7;
      "ch", Dba.LValue.restrict ecx 8 15;
      "cx", Dba.LValue.restrict ecx 0 15;
      "dl", Dba.LValue.restrict ebx 0 7;
      "dh", Dba.LValue.restrict ebx 8 15;
      "dx", Dba.LValue.restrict ebx 0 15;
      "cs", Dba.LValue.v cs;
      "ds", Dba.LValue.v ds;
      "es", Dba.LValue.v es;
      "fs", Dba.LValue.v fs;
      "gs", Dba.LValue.v gs;
      "ss", Dba.LValue.v ss;
      "CF", Dba.LValue.v cf;
      "PF", Dba.LValue.v pf;
      "AF", Dba.LValue.v af;
      "ZF", Dba.LValue.v zf;
      "SF", Dba.LValue.v sf;
      "TF", Dba.LValue.v tf;
      "IF", Dba.LValue.v if';
      "DF", Dba.LValue.v df;
      "OF", Dba.LValue.v of';
      "IOPL", Dba.LValue.v iopl;
      "NT", Dba.LValue.v nt;
      "RF", Dba.LValue.v rf;
      "VM", Dba.LValue.v vm;
      "AC", Dba.LValue.v ac;
      "VIF", Dba.LValue.v vif;
      "VIP", Dba.LValue.v vip;
      "ID", Dba.LValue.v id;
    ],
    fun img ->
      List.fold_left (fun result -> function
          | { Loader_elf.Note.name = "CORE"; kind = 1; offset=at; _ } ->
            let cursor = Loader_elf.Img.cursor ~at img in
            Loader_buf.advance cursor 0x48;
            let rebx = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let recx = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let redx = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let resi = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let redi = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let rebp = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let reax = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let rds = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            let res = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            let rfs = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            let rgs = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            Loader_buf.advance cursor 4;
            let entrypoint = Virtual_address.create
                (Loader_buf.Read.u32 cursor) in
            let rcs = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            let eflags = Loader_buf.Read.u32 cursor in
            let rcf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 0) land 0b1))
            and rpf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 2) land 0b1))
            and raf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 4) land 0b1))
            and rzf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 6) land 0b1))
            and rsf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 7) land 0b1))
            and rtf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 8) land 0b1))
            and rif = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 9) land 0b1))
            and rdf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 10) land 0b1))
            and rof = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 11) land 0b1))
            and riopl = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 12) land 0b11))
            and rnt = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 14) land 0b1))
            and rrf = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 16) land 0b1))
            and rvm = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 17) land 0b1))
            and rac = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 18) land 0b1))
            and rvif = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 19) land 0b1))
            and rvip = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 20) land 0b1))
            and rid = Dba.Expr.constant (Bitvector.of_int ~size:1
                                           ((eflags lsr 21) land 0b1)) in
            let resp = Dba.Expr.constant (Bitvector.of_int ~size:32
                                            (Loader_buf.Read.u32 cursor)) in
            let rss = Dba.Expr.constant (Bitvector.of_int ~size:16
                                            (Loader_buf.Read.u32 cursor)) in
            entrypoint,
            [
              Dba.LValue.v ebx, rebx;
              Dba.LValue.v ecx, recx;
              Dba.LValue.v edx, redx;
              Dba.LValue.v esi, resi;
              Dba.LValue.v edi, redi;
              Dba.LValue.v ebp, rebp;
              Dba.LValue.v eax, reax;
              Dba.LValue.v esp, resp;
              Dba.LValue.v ds, rds;
              Dba.LValue.v es, res;
              Dba.LValue.v fs, rfs;
              Dba.LValue.v gs, rgs;
              Dba.LValue.v cs, rcs;
              Dba.LValue.v ss, rss;
              Dba.LValue.v cf, rcf;
              Dba.LValue.v pf, rpf;
              Dba.LValue.v af, raf;
              Dba.LValue.v zf, rzf;
              Dba.LValue.v sf, rsf;
              Dba.LValue.v tf, rtf;
              Dba.LValue.v if', rif;
              Dba.LValue.v df, rdf;
              Dba.LValue.v of', rof;
              Dba.LValue.v iopl, riopl;
              Dba.LValue.v nt, rnt;
              Dba.LValue.v rf, rrf;
              Dba.LValue.v vm, rvm;
              Dba.LValue.v ac, rac;
              Dba.LValue.v vif, rvif;
              Dba.LValue.v vip, rvip;
              Dba.LValue.v id, rid;
            ]
          | _ -> result) (Virtual_address.create 0, []) (Loader_elf.notes img)

end

let get_defs () = match Kernel_options.Machine.isa () with
  | X86 { bits=`x32 } -> X86.defs
  | _ ->
    (* TODO *)
    raise (Errors.not_yet_implemented "incomplete architecture definition")

let core img = match Kernel_options.Machine.isa () with
  | X86 { bits=`x32 } -> X86.notes img
  | _ -> raise (Errors.not_yet_implemented "core dump") (* TODO *)
