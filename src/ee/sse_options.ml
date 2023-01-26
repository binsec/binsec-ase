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

include Cli.Make(
struct
  let shortname = "sse"
  let name = "Static Symbolic Execution"
end
)

module FromCore = Builder.String_option(
  struct
    let name = "from-core"
    let doc = "Start exploration from the concrete state of the core dump."
  end
  )

module MaxDepth = Builder.Integer(
  struct
    let name = "depth"
    let default = 1000
    let doc = "Set exploration maximal depth"
  end
  )


module JumpEnumDepth = Builder.Integer(
  struct
    let name = "jump-enum"
    let default = 3
    let doc = "Set maximum number of jump targets to retrieve for dynamic jumps"
  end
  )

module KeepGoing = Builder.False(
  struct
    let name = "keep-going"
    let doc = "Ignore errors returned by the SMT solver. Default is to abort."
  end
  )


module Randomize = Builder.False(
  struct
    let name = "randomize"
    let doc = "randomize path selection"
  end
  )

module SMT_dir = Builder.String_option(
  struct
    let name = "smt-dir"
    let doc = "set directory to cache smt scripts"
  end
  )


module AddressTraceFile = Builder.String_option(
  struct
    let name = "address-trace-file"
    let doc = "set file for adress trace export"
  end
  )

module AvoidAddresses = Builder.String_set(
  struct
    let name = "no-explore"
    let doc = "set addresses where sse should stop"
  end
  )

module GoalAddresses = Builder.String_set(
  struct
    let name = "explore"
    let doc = "set addresses where sse should try to go"
  end
  )

module LoadSections = Builder.String_set(
  struct
    let name = "load-sections"
    let doc =
      "Sections to load in initial memory (may be overridden by -memory)"
  end
  )

module LoadROSections = Builder.False(
  struct
    let name = "load-ro-sections"
    let doc =
      "Load the content of all read-only sections (see also -sse-load-sections)"
  end
  )

module MemoryFile = Builder.String_option(
  struct
    let name = "memory"
    let doc = "set file containing the initial (concrete) memory state"
  end
  )

module ScriptFile = Builder.String_option(
  struct
    let name = "script"
    let doc = "set file containing initializations, directives and stubs"
  end
  )

(* Starting fault injection options *)

module TargetAddresses = Builder.Integer_list(
  struct
    let name = "target-addresses"
    let doc = "set addresses where injection happens"
  end
  )

module FaultModel = Builder.String_choice(
  struct
    let name = "fault-model"
    let doc =
      "Define the fault model used"
    let default = "None"
    let choices = ["None"; "ArbitraryData"; "ArbitraryDataMul"; "ArbitraryDataAnd"; "ArbitraryDataIte"; "ArbitraryDataXor"; "ArbitraryDataBranchless2"; "ArbitraryDataBranching"; "ResetBranchless"; "ResetBranchlessIte"; "ResetBranchlessSub"; "ResetBranchlessAnd"; "ResetBranchlessXor"; "ResetBranchless2"; "ResetBranching"; "BitFlipBranchless"; "BitFlipBranching"; "TestInversionBranchlessIte"; "TestInversionBranchlessDirect"; "TestInversionBranching"]
  end
  )

module MaxFaults = Builder.Integer(
  struct
    let name = "max-faults"
    let default = 0
    let doc = "Maximum number of injected faults"
  end
  )

module GoalAddress = Builder.Integer_option(
  struct
    let name = "goal-address"
    let doc = "Address of ret in main / or any place to reach and check assumes"
  end
  )

module AssertFailAddress = Builder.Integer_option(
  struct
    let name = "assert-fail-address"
    let doc = "Address of assert fail function"
  end
  )

module SingleSolution = Builder.False(
  struct
    let name = "single-solution"
    let doc = "Ask for partial program exploration stopping at the first solution found"
  end
  )

module FaultFlags = Builder.False(
  struct
    let name = "fault-flags"
    let doc = "Fault the flags in addition to 'normal' variable if set to true"
  end
)

module FaultVarsUsedInAdresses = Builder.False(
  struct
    let name = "fault-vars-in-adresses"
    let doc = "Allow faults on variable used somewhere in adresses (independant of threashold)"
  end
)
    

module InjectionMethod = Builder.String_choice(
struct
  let name = "injection-method"
  let doc = "Which injection method to use"

  let default = "none"
  let choices = ["on-demand"; "always"; "test-inversion"; "none"]
end
)

module SubFaultsSimplification = Builder.String_choice(
struct
  let name = "subfaults-simplification"
  let doc = "Should we \"cancel\" faults undermined by another in an expression for ArbitraryData"
  let default = "none"
    let choices = ["none"; "free-bi"; "memoization"]
end
)

module SFSrecovery = Builder.String_choice(
struct
  let name = "sfs-recovery-mecanism"
  let doc = "subfault simplification is unsound, what to do when detected"
  let default = "none"
    let choices = ["none"; "abort"; "lazy"]
end
)

module WhereCheckNbFaults = Builder.String_choice(
  struct
    let name = "where-check-nb-faults"
    let doc = "Where to check the constrain NUMBER_FAULTS <= MAX_FAULTS: at each injection point, at each branch, at the end. Default: at the end."
    let default = "end"
    let choices = ["end"; "branch"; "loc"; "combi"]
  end
  )

module WhereCheckNonDetConstrain = Builder.String_choice(
  struct
    let name = "where-check-non-det-constrain"
    let doc = "Check only at the end the constrain FAULT_CONSTRAINS = 1 (by default at branch). Warning: at end may induce incorrectness with check nb faults at branch"

    let default = "idem"
    let choices = ["end"; "branch"; "loc"; "idem"]
  end
  )

module TargetBlackList = Builder.String_list(
  struct
    let name = "target-blacklist"
    let doc = "List of blacklisted variables: no injections will be done on them. Default: esp."
  end
  )

module ValueThresholdForFaults = Builder.Integer(
  struct
    let name = "value-threshold-for-faults"
    let default = 0x05000000
    let doc = "Threshold above whoch the value is considered as an address and not a value"
  end
  )

(* module DoInference = Builder.True(
  struct
    let name = "do-inference"
    let doc = "By default, inference pass is activated"
  end
  ) *)

module ComputeQueryComplexity = Builder.False(
  struct
    let name = "compute-query-complexity"
    let doc = "By default, stats about query complexity, i.e. the number of faults per query, is computed, at a cost."
  end
  )

module AtBranchOptim = Builder.False(
  struct
    let name = "at-branch-optim"
    let doc = "Activates is_max_reached propagation"
  end
  )

(* End of fault injection options *)

module Comment = Builder.False(
  struct
    let name = "comment"
    let doc =
      "Add comments indicating the origin of the formulas in generated scripts"
  end
  )

module NativeSolver = Builder.True(
  struct
    let name = "native-solver"
    let doc = "Use OCaml Bitwuzla SMT solver (Default)."
  end
  )


module Timeout =
Builder.Float(
struct
  let name = "timeout"
  let doc = "Sets a timeout for symbolic execution"
  let default = infinity
end)


module Address_counter = struct
  type t  = {
    address : Virtual_address.t;
    counter : int;
    }

  let of_string s =
    match String.split_on_char ':' s with
    | address :: [counter] ->
       { address = Virtual_address.of_string address;
         counter = int_of_string counter; }
    | _ -> assert false

  let decr c = { c with counter = c.counter - 1;}

  let check_and_decr c =
    if c.counter > 0 then Some (decr c)
    else None

end

module Visit_address_counter =
  Builder.Variant_list(
      struct
        include Address_counter
        let name = "visit-until"
        let doc  =
          "Specify a the maximum number of times [n] an address [vaddr] \
           is visited by SE (format: <vaddr>:<n>)"
      end
    )

type search_heuristics =
  | Dfs
  | Bfs
  | Nurs

module Search_heuristics =
  Builder.Variant_choice_assoc(struct
      type t = search_heuristics
      let name = "heuristics"
      let doc = "Use the following search heuristics"
      let default = Dfs
      let assoc_map = [
          "dfs", Dfs;
          "bfs", Bfs;
          "nurs", Nurs
        ]
    end)


module Solver_call_frequency =
  Builder.Integer(
      struct
        let name = "solver-call-frequency"
        let default = 1
        let doc = "Call the solver every <n> times"
      end
    )


module Seed =
  Builder.Integer_option(
      struct
        let name = "seed"
        let doc = "Give a specific seed for random number generators"
      end
)

module Directives =
  Builder.Any(
   struct
     type t = Directive.t list
     let name = "directives"
     let doc = "Set SSE directive"
     let default = []
     let to_string _ = "no directives"

     let of_string s =
       let lexbuf = Lexing.from_string s in
       Parser.directives Lexer.token lexbuf
   end
  )


module Dot_filename_out =
  Builder.String_option(
      struct
        let name = "cfg-o"
        let doc  = "Output CFG in this file"
      end
    )


module SMT_log_directory =
  Builder.String(
      struct
        let name = "smt-dump-dir"
        let doc  = "Set directory where unsolved SMT scripts are dumped"
        let default = "binsec_smtdump"
      end
    )
