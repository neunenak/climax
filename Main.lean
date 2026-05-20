import Climax

def cliArgs: Parser := Id.run do
  let mut parser := Parser.new "climax-example"
  parser := parser.addArgument <| Argument.flag "chutney" "Generate some chutney"
  parser := parser.addArgument <| Argument.flag "bananas" "Go absolutely bananas"
  parser := parser.addArgument <| Argument.flag "arch" "Architecture"

  return parser

def main (args: List String): IO Unit := do

  let parser := cliArgs
  let cliMatches <- parser.run args

  IO.println s!"This program demonstrates a simple use of the climax command line argument parsing library. Try passing in --help"





