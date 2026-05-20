import Std.Data.HashMap

import Colorized


import Climax.Argument
import Climax.Matches
import Climax.Util

structure Parser where
  -- name of the program
  programName: Option String

  -- description
  description: Option String

  arguments: List Argument



namespace Parser

def new (name: String): Parser := {
  programName := name
  description := none
  arguments := []
}

def addArgument (parser: Parser) (arg: Argument): Parser := 
  let newArguments := parser.arguments.concat arg
  {
    parser with arguments := newArguments
  }


def helpString (parser: Parser): String := Id.run do

  let mut s := ""

  match parser.programName, parser.description with
    | some name, some desc => 
      s := s ++ s!"{name} - {desc}\n"
    | some name, none => s := s ++ name ++ "\n"
    | none, some desc => s := s ++ desc ++ "\n"
    | none, none => ()

  s := s.append (open Colorized in Colorized.color Color.Cyan "Options:\n")

  for arg in parser.arguments do
    let line := s!"{spaceTab}{arg.optionString}"
    s := s.append line
    s := s.append "\n"

  return s


structure MatchData where
  arguments: List String

def getMatches (_parser: Parser) (cliArgs: List String): Matches := Id.run do
  let mut seen: Std.HashMap String MatchData := Std.HashMap.emptyWithCapacity

  for arg in cliArgs do
    if arg.startsWith "--" then
      let argName := (arg.drop 2).toString
      let existingKey := seen.get? argName

    else if arg.startsWith "-" then
      ()
    else
      ()

  return {
    matchedItems := []
  }



def run (parser: Parser) (cliArgs: List String): IO Unit := do
  match cliArgs with
  | [] => return
  | "--help" :: _rest => do
    IO.println <| helpString parser
    IO.Process.exit 0
  | _otherwise => return ()


end Parser
