import LSpec

import Climax

open LSpec

def basicParser := arguments (Parser.new "greet")
  | "-n" "--name"    "Name to greet"
  |      "--verbose" "Enable verbose output"

def basicParserExpectedHelp :=
  "greet\n" ++
  (open Colorized in Colorized.color Color.Cyan "Options:\n") ++
  "    -n, --name      Name to greet\n" ++
  "        --verbose      Enable verbose output\n"

def helpTests: List TestSeq := [
  test "Help is as expected" (basicParser.helpString = basicParserExpectedHelp)
]

def main := lspecIO $ .ofList [
  ("Help tests", helpTests)
]
