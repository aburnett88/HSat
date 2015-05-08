{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HSat.Problem.BSP.Internal
Description : Exports the BSP data type
Copyright   : (c) Andrew Burnett, 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : -

Contains the BSP data type, used to describe general Boolean formula.

This module provides several functions that automatically change a generic
BSP
-}

module HSat.Problem.BSP.Internal (
  -- * BSP
  BSP(..),
  -- * Functions
  nnf,
  removeIf,
  removeIff,
  permutations,
  dnf,
  cnf,
  tseitin,
  numberPermutations, --Not tested yet
  -- * Tests
  isNNF,
  noIfs,
  noIffs,
  ) where

import HSat.Printer
import HSat.Problem.BSP.Common.Variable
{-|
A recursive type that describes a Boolean Formula
-}
data BSP =
  -- | A plan 'Bool' Value
  Bool'     Bool     |
  -- | Constructor for a 'Variable'
  Variable' Variable |
  -- | Constructor to negate a 'BSP'
  Not       BSP      |
  -- | Disjunction of 'BSP's
  Or        BSP BSP  |
  -- | Conjunction of 'BSP's
  And       BSP BSP  |
  -- | Logical if of 'BSP's
  If        BSP BSP  |
  -- | If and only if of 'BSP's
  IfOnlyIf  BSP BSP
  deriving (Eq,Ord)

instance Show BSP where
  showsPrec = show'

instance Printer BSP where
  compact   = printBSP Compact
  noUnicode = printBSP NoUnicode
  unicode   = printBSP Unicode

printBSP :: PrinterType -> BSP -> Doc
printBSP ptype bsp = case bsp of
  Or l r -> genericBinary "(" ")" or' l r
  And l r -> genericBinary "(" ")" and' l r
  If l r -> genericBinary "(" ")" if' l r
  IfOnlyIf l r -> genericBinary "(" ")" iff' l r
  Not n -> case ptype of
    Unicode -> "¬"
    _ -> "!"
    <> printBSP ptype n
  Variable' v -> pTypeToDoc ptype v
  Bool' b -> pTypeToDoc ptype b
  where
    genericBinary :: Doc -> Doc -> Doc -> BSP -> BSP -> Doc
    genericBinary l r m l' r' =
      l <> printBSP ptype l' <>
      m <> printBSP ptype r' <>
      r
    or',and',if',iff' :: Doc
    or' = case ptype of
      Unicode ->  "∨"
      _ -> "\\/"
    and' = case ptype of
      Unicode ->  "∧"
      _ -> "/\\"
    if' = case ptype of
      Unicode -> "⇒"
      _ -> "=>"
    iff' = case ptype of
      Unicode -> "⇔"
      _ -> "<==>"

generic :: (BSP -> BSP) -> BSP -> BSP
generic f bsp = case bsp of
  IfOnlyIf l r -> IfOnlyIf (f l) (f r)
  If l r       -> If (f l) (f r)
  Or l r       -> Or (f l) (f r)
  And l r      -> And (f l) (f r)
  Not n        -> Not (f n)
  x            -> x

tupleMap :: (a -> b) -> (a,a) -> (b,b)
tupleMap f (a,b) = (f a, f b)

{-|
Removes all 'IfOnlyIf' constructors from the BSP data type, replacing
them with equivalent logical structures
-}
removeIff :: BSP -> BSP
removeIff bsp = case bsp of
  IfOnlyIf l r ->
    let (l',r') = tupleMap removeIff (l,r)
    in And (If l' r') (If r' l')
  _            -> generic removeIff bsp

{-|
Removes all 'If' constructors from the BSP data type, replacing them with
equivalent logical structures
-}
removeIf :: BSP -> BSP
removeIf bsp = case bsp of
  If l r ->
    let (l',r') = tupleMap removeIf (l,r)
    in Or (Not l') r'
  _            -> generic removeIf bsp

{-|
Moves all 'Not' constructors inwards, so that they are around either a Variable
or a Boolean value. If any If or IfOnlyIf constructors are found, these are also
removed
-}
nnf :: BSP -> BSP
nnf bsp = case bsp of
  Not (Not n)   -> nnf n
  Not (And l r) -> Or (Not $ nnf l) (Not $ nnf r)
  Not (Or l r)  -> And (Not $ nnf l) (Not $ nnf r)
  _             -> generic nnf bsp

{-|
The final step in turning a BSP into DNF.

Note, this should be called after nnf. Failure to do so will result
in Boolean Formula's that are not in DNF
-}
dnf :: BSP -> BSP
dnf bsp = case bsp of
  And a (Or b c) -> Or (And a b) (And a c)
  And (Or b c) a -> Or (And a b) (And a c)
  _ -> generic dnf bsp

tseitin :: BSP -> BSP
tseitin = error "unwritten"

cnf :: BSP -> BSP
cnf = error "cnf unwritten"


testBranch :: (Bool -> Bool -> Bool) -> Bool -> (BSP -> Bool) -> BSP -> Bool
testBranch binaryOp baseCase f bsp =
  case bsp of
    IfOnlyIf l r -> binaryOp (f l) (f r)
    If l r       -> binaryOp (f l) (f r)
    Or l r       -> binaryOp (f l) (f r)
    And l r      -> binaryOp (f l) (f r)
    Not n        -> f n
    _            -> baseCase

conjunction :: (BSP -> Bool) -> BSP -> Bool
conjunction = testBranch (&&) True

{-|
Checks that Not constructors are only next to Variable's or Booleans
-}
isNNF :: BSP -> Bool
isNNF bsp = case bsp of
  IfOnlyIf _ _      -> False
  If _ _            -> False
  Not (Variable' _) -> True
  Not (Bool' _)     -> True
  Not _             -> False
  _                 -> conjunction isNNF bsp

{-|
Checks that no If or IfOnlyIf constructors remain within the BSP data type
-}
noIfs :: BSP -> Bool
noIfs bsp = case bsp of
  If _ _       -> False
  IfOnlyIf _ _ -> False
  _            -> conjunction noIfs bsp

{-|
Returns True if there are no IfOnlyIf constructors remaining
-}
noIffs :: BSP -> Bool
noIffs bsp = case bsp of
  IfOnlyIf _ _ -> False
  _            -> conjunction noIffs bsp

{-|
Returnsxsy all possible equivalent logic forms of the Boolean formula.

For example, swaps all And's, Or's and Iff's around. 
-}
permutations :: BSP -> [BSP]
permutations (IfOnlyIf l r) = permutations' IfOnlyIf l r True
permutations (If l r)       = permutations' If       l r False
permutations (Or l r)       = permutations' Or       l r True
permutations (And l r)      = permutations' And      l r True
permutations (Not n)        = map Not $ permutations n
permutations x              = [x]

permutations' :: (BSP -> BSP -> BSP) -> BSP -> BSP -> Bool -> [BSP]
permutations' f l r is_assoc =
  let (l',r') = tupleMap permutations (l,r)
      pairs = pairUp l' r' is_assoc
  in map (uncurry f) pairs

pairUp :: [a] -> [a] -> Bool -> [(a,a)]
pairUp ls rs switch' =
  let res = pairUp' ls rs
      res' = if switch' then switch res else []
  in res ++ res'
  where
    pairUp' :: [a] -> [a] -> [(a,a)]
    pairUp' [] _ = []
    pairUp' (x:xs) ys =
      let res = map ((,) x) ys
      in res ++ pairUp' xs ys
    switch :: [(a,a)] -> [(a,a)]
    switch = map (\(l,r) -> (r,l))

{-|
Counts the number of permutations within a BSP - that is, equivalent
logical forms found by swapping, for example, And values around
-}
numberPermutations :: BSP -> Int
numberPermutations bsp =
  case bsp of
    IfOnlyIf l r -> multiplies l r True
    If l r       -> multiplies l r False 
    And l r      -> multiplies l r True
    Or l r       -> multiplies l r True
    Not n        -> numberPermutations n
    _            -> 1
  where
     multiplies l r is_assoc = (if is_assoc then 2 else 1) * 
       (numberPermutations l) * (numberPermutations r)
