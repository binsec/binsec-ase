%token ASSERT ASSUME ASSIGN GOTO JUMP
%token UNDEF NONDET
%token AT IF THEN ELSE

%%

%public
let assignment :=
  | ~=loc; ASSIGN; ~=expr;
    { Dba.Instr.assign loc expr }
  | ~=loc; ASSIGN; value=INT;
    { Dba.Instr.assign
	loc (Dba.Expr.constant
	     @@ Bitvector.of_int ~size:(Dba.LValue.size_of loc) value) }
  | ~=loc; ASSIGN; UNDEF;
    { Dba.Instr.undefined loc }
  | ~=loc; ASSIGN; NONDET;
    { Dba.Instr.non_deterministic loc }

%public
let fallthrough :=
  | ~=assignment;
    { assignment }
  | ASSERT; ~=bool;
    { Dba.Instr._assert bool }
  | ASSUME; ~=bool;
    { Dba.Instr.assume bool }

%public
let terminator :=
  | JUMP; AT; ~=expr;
    { match expr with
      | Dba.Expr.Cst bv ->
	 Dba.Instr.static_jump @@ Dba.Jump_target.outer
	 @@ Dba_types.Caddress.of_virtual_address
	 @@ Virtual_address.of_bitvector bv
      | _ -> Dba.Instr.dynamic_jump expr
    }
