import Climax

def cliArgs: Parser :=
  let argParser := Parser.new "climax-example"
  arguments argParser
    | "-c" "--chutney" "Generate some chutney"
    | "-b" "--bananas" "Go absolutely bananas"
    | "--arch" "Architecture"

def main (args: List String): IO Unit := do

  let parser := cliArgs
  let _cliMatches <- parser.run args

  IO.println s!"This program demonstrates a simple use of the climax command line argument parsing library. Try passing in --help"





