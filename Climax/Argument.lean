import Lean

import Climax.Util

inductive Action where
  | SetTrue
  | SetFalse
  | Set
  | Append

structure Argument where
  name: String
  shortName: Option Char
  description: String
  required: Bool
  numArguments: Nat
  default: Option String
  action: Action := Action.Set

namespace Argument


def flag (name: String) (description: String): Argument := {
  name := name
  shortName := none
  description := description
  required := false
  numArguments := 0
  default := none
  action := Action.SetTrue
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

