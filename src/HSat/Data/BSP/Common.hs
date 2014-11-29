{-|
Module      : HSat.Data.BSP.Common
Description : The Common data types for BSP problems
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module exports the qualified names for the common data
types in a BSP instance. It is meant for ease of use. 
-}

module HSat.Data.BSP.Common (
  Clause,
  Clauses,
  Literal,
  Variable
  ) where

import HSat.Data.BSP.Common.Clause (Clause)
import HSat.Data.BSP.Common.Clauses (Clauses)
import HSat.Data.BSP.Common.Literal (Literal)
import HSat.Data.BSP.Common.Variable (Variable)
