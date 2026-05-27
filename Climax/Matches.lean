structure MatchedFlag where
  name: String
  shortName: Option Char := none
  count: Nat := 1

structure MatchedArgument where
  name: String
  shortName: Option Char := none
  value: List String

inductive MatchedItem where
  | flag (f: MatchedFlag)
  | arg (arg: MatchedArgument)

structure ArgMatches where
  items: List MatchedItem

namespace ArgMatches

-- Get any kind of match as a `MatchItem`
def getMatch? (self: ArgMatches) (name: String) : Option MatchedItem :=
  self.items.find? $ fun (item: MatchedItem) => match item with
    | .flag f => f.name = name || f.shortName.map (·.toString) = some name
    | .arg arg => arg.name = name || arg.shortName.map (·.toString) = some name


end ArgMatches
