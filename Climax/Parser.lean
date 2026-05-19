
import Climax.Argument

structure Parser where
  arguments: List Argument



open Parser

def new: Parser := {
  arguments := []
}

def add_argument (parser: Parser) (arg: Argument): Parser := 
  let newArguments := parser.arguments.concat arg
  {
    parser with arguments := newArguments
  }




def getMatches (parser: Parser) (cliArgs: List String): Matches :=
  sorry
