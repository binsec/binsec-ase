%token FROM FILE
%token REACH CUT ENUMERATE
%token REPLACE BY
%token EOF

%start <Sse_types.Script.t list> script

%%

let script := statements=list(terminated(statement, option(SEMICOLON))); EOF;
    { statements }

let statement :=
  | ~=init;
    { init }
  | ~=goal;
    { goal }
  | ~=stub;
    { stub }

let init :=
  | a=assignment;
    { Sse_types.Script.Init
	(Parse_helpers.Initialization.from_assignment (a 0)) }
  | ~=load; FROM; FILE;
    { Sse_types.Script.Init (Parse_helpers.Initialization.from_store load) }
  | ASSUME; ~=bool;
    { Sse_types.Script.Init (Parse_helpers.Initialization.assume bool) }

let goal :=
  | AT; ~=iexpr; ~=directive;
    { let loc = match iexpr with
	| Left i -> Dba.Expr.constant (Bitvector.of_int ~size:E.wordsize i)
	| Right e -> e in
      Sse_types.Script.Goal (directive ~loc ()) }

let directive :=
  | REACH; times=option(delimited(LPAR, INT, RPAR));
    guard=option(preceded(IF, bool));
    { let n = Utils.get_opt_or_default 1 times in Directive.reach ~n ?guard}
  | REACH; MUL;
    { Directive.reach_all }
  | ENUMERATE; e=expr; times=option(delimited(LPAR, INT, RPAR));
    { let n = Utils.get_opt_or_default 1 times in Directive.enumerate ~n e }
  | ASSUME; e=bool;
    { Directive.assume e }
  | CUT; guard=option(preceded(IF,bool));
    { Directive.cut ?guard }

let stub :=
  | REPLACE; ~=iexpr; BY; ~=chunk;
    { let loc = match iexpr with
	| Left i -> Dba.Expr.constant (Bitvector.of_int ~size:E.wordsize i)
	| Right e -> e in
      Sse_types.Script.Stub (loc, chunk) }
