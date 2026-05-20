import Climax.Util

structure Argument where
  name: String
  shortName: Option Char
  description: String
  required: Bool
  numArguments: Nat
  default: Option (List String)

namespace Argument

def newFlag (name: String) (description: String): Argument := {
  name := name
  shortName := none
  description := description
  required := false
  numArguments := 0
  default := none
  }

def optionString (self: Argument): String := Id.run do
  let mut line := ""
  if let some shortName := self.shortName then
    line := line.append s!"-{shortName}, "
  else
    line := line.append spaceTab

  line := line.append s!"--{self.name}"
  line := line.append spaceTab

  line := line.append "  "
  line := line.append self.description

   return line 

end Argument
