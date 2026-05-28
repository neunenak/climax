import Climax.Argument

structure MatchedItem where
  name: String
  shortName: Option Char := none
  value: List String := []
  action: Action

structure ArgMatches where
  items: List MatchedItem

namespace ArgMatches

def getMatch? (self: ArgMatches) (name: String) : Option MatchedItem :=
  self.items.find? fun item =>
    item.name = name || item.shortName.map (·.toString) = some name

end ArgMatches
