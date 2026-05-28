import LSpec

import Climax
import Test.Macros



def basicParser := arguments (ArgParser.new "greet")
  | "-n" "--name"    "Name to greet"
  |      "--verbose" "Enable verbose output"

def basicParserExpectedHelp :=
  "greet\n" ++
  (open Colorized in Colorized.color Color.Cyan "Options:\n") ++
  "    -h, --help      Show this help message\n" ++
  "    -n, --name      Name to greet\n" ++
  "        --verbose      Enable verbose output\n"

open LSpec

def helpTests: List TestSeq := [
  test "Help is as expected" (basicParser.helpString = basicParserExpectedHelp)
]

def basicMatchTests: List TestSeq :=
  let withCli (input: String) (testFn: ArgMatches -> Bool): Bool :=
    let argMatchesE := basicParser.getMatches $ input.splitOn " "
    match argMatchesE with 
      | .error _ => false
      | .ok argMatches => testFn argMatches
  [
    test "Matches long name" (withCli "-n bonanza" (fun aM =>
    (aM.getMatch? "name").isSome
      )) $
    test "Matches short name" (withCli "-n bonanza" (fun aM =>
    (aM.getMatch? "n").isSome
      ))
  ]



def main := lspecIO $ .ofList [
  ("Help tests", helpTests),
  ("Basic match checks", basicMatchTests),
]
