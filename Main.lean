import Climax

def cliArgs: ArgParser :=
  let argParser := ArgParser.new "climax-example"
  arguments argParser
    | "-f" "--food" "Enter the name of a foodstuff"

def main (args: List String): IO Unit := do

  let parser := cliArgs
  let cliMatches <- parser.run args

  if args.isEmpty then
    IO.println s!"This program demonstrates a simple use of the climax command line argument parsing library. Try passing in --help"
    IO.Process.exit 1


  if let some m := cliMatches.getMatch? "food" then
    let food := m.value
    IO.println s!"Entered a food value: {food}"







