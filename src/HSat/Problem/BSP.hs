{-|
Module      : HSat.Problem.BSP
Description : BSP provides functions to create BSP datatypes
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Exp[orts several high level functions usedc to create BSP datatypes
-}

module HSat.Problem.BSP (
  BSP,
  -- * Unary Constructors
  bool,
  var,
  not',
  -- * Binary Constructors
  (&&!),
  (||!),
  (==>!),
  (<==>!)
  ) where

import HSat.Problem.BSP.Common.Variable
import HSat.Problem.BSP.Internal

{-|
Takes a 'Bool' and creates a 'BSP' wrapper around it
-}
bool :: Bool -> BSP
bool = Bool'

{-|
Takes a 'Variable' and creates a 'BSP' wrapper around it
-}
var :: Variable -> BSP
var = Variable'

{-|
Takes a 'BSP' and applies a negates it
-}
not' :: BSP -> BSP
not' = Not

{-|
Takes two 'BSP's and creates an And constructor from them
-}
(&&!) :: BSP -> BSP -> BSP
l &&! r = And l r

{-|
Takes two 'BSP's and creates a Or constructor from them
-}
(||!) :: BSP -> BSP -> BSP
l ||! r =  Or l r

{-|
Takes two 'BSP's and creates a If constructor from them
-}
(==>!) :: BSP -> BSP -> BSP
l ==>! r =  If l r

{-|
Takes two 'BSP's and creates a Iff constructor fromt hem
-}
(<==>!) :: BSP -> BSP -> BSP
l <==>! r =  IfOnlyIf l r
