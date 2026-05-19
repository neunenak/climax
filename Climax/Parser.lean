
import Climax.Argument
import Climax.Matches

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
  let spaceTab: String := String.ofList (List.replicate 4 ' ')

  let mut s := ""

  match parser.programName, parser.description with
    | some name, some desc => 
      s := s ++ s!"{name} - {desc}\n"
    | some name, none => s := s ++ name ++ "\n"
    | none, some desc => s := s ++ desc ++ "\n"
    | none, none => ()

  s := s.append "Options:\n"


  for arg in parser.arguments do
    let mut line := spaceTab

    if let some shortName := arg.shortName then
      line := line.append s!"-{shortName}, "
    else
      line := line.append spaceTab

    line := line.append s!"--{arg.name}"
    line := line.append spaceTab

    line := line.append "  "
    line := line.append arg.description

    s := s.append line
    s := s.append "\n"

  return s



def getMatches (_parser: Parser) (_cliArgs: List String): Matches :=
  {
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
