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
bool = AtomicExpr' . Bool'

var :: Variable -> BSP
var = AtomicExpr' . Variable'

not' :: BSP -> BSP
not' = ComplexExpr' . Not

(&&!) :: BSP -> BSP -> BSP
l &&! r = ComplexExpr' $ And l r

(||!) :: BSP -> BSP -> BSP
l ||! r = ComplexExpr' $ Or l r

(==>!) :: BSP -> BSP -> BSP
l ==>! r = ComplexExpr' $ If l r

(<==>!) :: BSP -> BSP -> BSP
l <==>! r = ComplexExpr' $ IfOnlyIf l r
