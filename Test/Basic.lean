import LSpec

import Climax


def basicParser := arguments (ArgParser.new "greet")
  | "-n" "--name"    "Name to greet"
  |      "--verbose" "Enable verbose output"

def basicParserExpectedHelp :=
  "greet\n" ++
  (open Colorized in Colorized.color Color.Cyan "Options:\n") ++
  "    -n, --name      Name to greet\n" ++
  "        --verbose      Enable verbose output\n"

open LSpec

def helpTests: List TestSeq := [
  test "Help is as expected" (basicParser.helpString = basicParserExpectedHelp)
]

def main := lspecIO $ .ofList [
  ("Help tests", helpTests)
]
