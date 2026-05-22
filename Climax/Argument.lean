import Lean

import Climax.Util

structure Argument where
  name: String
  shortName: Option Char
  description: String
  required: Bool
  numArguments: Nat
  default: Option (List String)
  isFlag: Bool

namespace Argument

def flag (name: String) (description: String): Argument := {
  name := name
  shortName := none
  description := description
  required := false
  numArguments := 0
  default := none
  isFlag := true
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

private def parseLongFlag (longStr : String) : Lean.MacroM String := do
  unless longStr.startsWith "--" do
    Lean.Macro.throwError
      s!"argument: long flag must start with \"--\", got \"{longStr}\""
  return (longStr.drop 2).toString

private def parseShortFlag (shortStr: String): Lean.MacroM Char := do
  unless shortStr.startsWith "-" && !shortStr.startsWith "--" && shortStr.length == 2 do
    Lean.Macro.throwError
      s!"argument: short flag must be a single character like \"-x\", got \"{shortStr}\""
  return shortStr.toList[1]!

private def mkArgumentTerm (shortCharOpt : Option Char) (longName descStr : String) :
    Lean.MacroM (Lean.TSyntax `term) := do
  let nameLit := Lean.Syntax.mkStrLit longName
  let descLit := Lean.Syntax.mkStrLit descStr
  let shortNameTerm ← match shortCharOpt with
    | none   => `(none)
    | some c =>
        let charLit := Lean.Syntax.mkCharLit c
        `(some $charLit)
  `({ name         := $nameLit
      shortName    := $shortNameTerm
      description  := $descLit
      required     := false
      numArguments := 0
      default      := none
      isFlag       := true
      : Argument })


-- `argument "-x" "--full-arg" "description"`
macro "argument" short:str long:str desc:str : term => do
  let shortStr := short.raw.isStrLit? |>.getD ""
  let longStr  := long.raw.isStrLit?  |>.getD ""
  let descStr  := desc.raw.isStrLit?  |>.getD ""

  let shortName <- parseShortFlag shortStr
  let longName ← parseLongFlag longStr

  mkArgumentTerm (some shortName) longName descStr

-- `argument "--full-arg" "description"`
macro "argument" long:str desc:str : term => do
  let longStr := long.raw.isStrLit? |>.getD ""
  let descStr := desc.raw.isStrLit? |>.getD ""

  let longName ← parseLongFlag longStr

  mkArgumentTerm none longName descStr
