module HSat.Problem.BSP (
  bool,
  var,
  not',
  (&&!),
  (||!),
  (==>!),
  (<==>!),
  BSP
  ) where

import HSat.Problem.BSP.Internal
import HSat.Problem.BSP.Common.Variable

bool :: Bool -> BSP
bool = Bool'

var :: Variable -> BSP
var = Variable'

not' :: BSP -> BSP
not' = Not

(&&!) :: BSP -> BSP -> BSP
l &&! r = And l r

(||!) :: BSP -> BSP -> BSP
l ||! r =  Or l r

(==>!) :: BSP -> BSP -> BSP
l ==>! r =  If l r

(<==>!) :: BSP -> BSP -> BSP
l <==>! r =  IfOnlyIf l r
