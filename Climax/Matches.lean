structure MatchedFlag where
  name: String
  count: Nat := 1

structure MatchedArgument where
  name: String
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
    | .flag f => f.name = name
    | .arg arg => arg.name = name


end ArgMatches
