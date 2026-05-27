structure MatchedFlag where
  name: String

structure MatchedArgument where
  name: String
  value: List String

inductive MatchedItem where
  | flag (f: MatchedFlag)
  | arg (arg: MatchedArgument)

structure ArgMatches where
  matchedItems: List MatchedItem
