import Std.Data.HashMap

import Colorized

import Climax.Argument
import Climax.Matches
import Climax.Util

inductive ParseError where
  | unknownArgument (given: String)
  | missingRequired (name: String)
  | tooFewValues (arg: Argument) (got: Nat)

def ParseError.message : ParseError → String
  | .unknownArgument s => s!"unknown argument: {s}"
  | .missingRequired name => s!"missing required argument: --{name}"
  | .tooFewValues arg got =>
      s!"--{arg.name} requires {arg.numArguments} value(s) but got {got}"

instance : ToString ParseError := ⟨ParseError.message⟩

structure ArgParser where
  programName: Option String
  description: Option String
  arguments: List Argument

namespace ArgParser

-- Creates a completely blank argument parser
def blank (name: String): ArgParser := {
  programName := name
  description := none
  arguments := []
}

-- Creates a new argument parser with default options
def new (name: String): ArgParser :=
  let helpArg : Argument := {
    name        := "help"
    shortName   := some 'h'
    description := "Show this help message"
    required    := false
    numArguments := 0
    default     := none
    action      := Action.ShowHelp
  }
  { programName := name
    description := none
    arguments   := [helpArg]
  }

def addArgument (parser: ArgParser) (arg: Argument): ArgParser :=
  { parser with arguments := parser.arguments.concat arg }

def helpString (parser: ArgParser): String := Id.run do
  let mut s := ""
  match parser.programName, parser.description with
    | some name, some desc => s := s ++ s!"{name} - {desc}\n"
    | some name, none      => s := s ++ name ++ "\n"
    | none, some desc      => s := s ++ desc ++ "\n"
    | none, none           => ()
  s := s.append (open Colorized in Colorized.color Color.Cyan "Options:\n")
  for arg in parser.arguments do
    s := s.append s!"{spaceTab}{arg.optionString}\n"
  return s

-- Given a known Argument definition and the remaining tokens after the flag,
-- return the MatchedItem and the number of following tokens consumed as values.
private def resolveArg (argDef: Argument) (shortName: Option Char) (rest: List String) :
    Except ParseError (MatchedItem × Nat) :=
  if argDef.numArguments == 0 then
    pure ({ name := argDef.name, shortName := shortName, action := argDef.action }, 0)
  else
    let values := rest.take argDef.numArguments
    if values.length < argDef.numArguments then
      .error (.tooFewValues argDef values.length)
    else
      pure ({ name := argDef.name, shortName := shortName, value := values, action := argDef.action }, argDef.numArguments)

private def matchLong (longMap: Std.HashMap String Argument) (token: String) (rest: List String) :
    Except ParseError (MatchedItem × Nat) :=
  match longMap.get? (token.drop 2).toString with
  | none        => .error (.unknownArgument token)
  | some argDef => resolveArg argDef none rest

private def matchShort (shortMap: Std.HashMap Char Argument) (token: String) (rest: List String) :
    Except ParseError (MatchedItem × Nat) :=
  let shortPart := (token.drop 1).toString
  if shortPart.length != 1 then
    .error (.unknownArgument token)
  else
    match shortMap.get? shortPart.front with
    | none        => .error (.unknownArgument token)
    | some argDef => resolveArg argDef (some shortPart.front) rest

private def parseLoop
    (longMap: Std.HashMap String Argument)
    (shortMap: Std.HashMap Char Argument)
    (remaining: List String)
    (acc: List MatchedItem) :
    Except ParseError (List MatchedItem) := do
  match remaining with
  | [] => return acc.reverse
  | s :: rest =>
    if s.startsWith "--" then
      let (item, nConsumed) ← matchLong longMap s rest
      parseLoop longMap shortMap (rest.drop nConsumed) (item :: acc)
    else if s.startsWith "-" then
      let (item, nConsumed) ← matchShort shortMap s rest
      parseLoop longMap shortMap (rest.drop nConsumed) (item :: acc)
    else
      throw (.unknownArgument s)
  termination_by remaining.length
  decreasing_by
    all_goals simp +arith [List.length_cons, List.length_drop]

def getMatches (parser: ArgParser) (cliArgs: List String): Except ParseError ArgMatches := do

  let longMap : Std.HashMap String Argument :=
    parser.arguments.foldl (fun m a => m.insert a.name a) (Std.HashMap.emptyWithCapacity)
  let shortMap : Std.HashMap Char Argument :=
    parser.arguments.foldl (fun m a =>
      match a.shortName with
      | some c => m.insert c a
      | none   => m) (Std.HashMap.emptyWithCapacity)

  let items: List MatchedItem ← parseLoop longMap shortMap cliArgs []

  let wasSeen (name: String): Bool := items.any fun item => item.name == name

  let allItems ← parser.arguments.foldlM (fun (acc: List MatchedItem) (argDef: Argument) =>
    if wasSeen argDef.name then pure acc
    else match argDef.default with
      | some v =>
          let item : MatchedItem := { name := argDef.name, value := [v], action := argDef.action }
          pure (acc ++ [item])
      | none =>
          if argDef.required then throw (.missingRequired argDef.name)
          else pure acc) items
  return { items := allItems }

-- Run the argument parser, yielding an ArgMatches object, or exiting with errors
def run (parser: ArgParser) (cliArgs: List String): IO ArgMatches := do
  match getMatches parser cliArgs with
  | Except.error e =>
    IO.eprintln s!"Command line argument error: {e}. Pass `--help` for usage information."
    IO.Process.exit 1
  | Except.ok cliMatches =>
    if cliMatches.showHelp then
      IO.println <| helpString parser
      IO.Process.exit 0
    return cliMatches

end ArgParser
