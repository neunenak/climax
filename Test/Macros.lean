import LSpec
import Climax

open LSpec

/--
error: argument: short flag must be a single character like "-x", got "-fx"
-/
#guard_msgs in
#check argument "-fx" "--long-name" "description"
