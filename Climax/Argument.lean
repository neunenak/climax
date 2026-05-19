
structure Argument where
  name: String
  shortName: Option Char
  description: String
  required: Bool
  numArguments: Nat

namespace Argument

def newFlag (name: String) (description: String): Argument := {
  name := name
  shortName := none
  description := description
  required := false
  numArguments := 0
  }


end Argument
