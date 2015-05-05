module HSat.Problem.BSP.Internal (
  BSP(..),
  removeIf,
  removeIff,
  nnf,
  isNNF,
  permutations,
  noIfs,
  noIffs,
  numberPermutations
  ) where

import HSat.Problem.BSP.Common.Variable
import HSat.Printer

data BSP =
  Bool' Bool |
  Variable' Variable |
  Not BSP |
  Or BSP BSP |
  And BSP BSP |
  If BSP BSP |
  IfOnlyIf BSP BSP
  deriving (Eq,Ord)

instance Show BSP where
  showsPrec = show'

instance Printer BSP where
  compact (Bool' True) = text "T"
  compact (Bool' False) = text "F"
  compact (Variable' v) = compact v
  compact (Not bsp) = (text " !") <> compact bsp
  compact (Or l r) =
    genericBinary compact "(" ")" "\\/" l r
  compact (And l r) =
    genericBinary compact "(" ")" "/\\" l r
  compact (If l r) =
    genericBinary compact "(" ")" "=>" l r
  compact (IfOnlyIf l r) =
    genericBinary compact "(" ")" "<==>" l r
  noUnicode (Bool' b) = text . show $ b
  noUnicode (Variable' v) = noUnicode v
  noUnicode (Not bsp) = (text " !") <> noUnicode bsp
  noUnicode (Or l r) =
    genericBinary noUnicode " (" ") " "\\/" l r
  noUnicode (And l r) =
    genericBinary noUnicode " (" ") " "/\\" l r
  noUnicode (If l r) =
    genericBinary noUnicode " (" ") " "=>" l r
  noUnicode (IfOnlyIf l r) =
    genericBinary noUnicode " (" ") " "<==>" l r
  unicode (Bool' True) = green . text $ "True"
  unicode (Bool' False) = red . text $ "False"
  unicode (Variable' v) = unicode v
  unicode (Not bsp) = (text "¬") <> unicode bsp
  unicode (Or l r) =
    genericBinary unicode " (" ") " "∨" l r
  unicode (And l r) =
    genericBinary unicode " (" ") " "∧" l r
  unicode (If l r) =
    genericBinary unicode " (" ") " "⇒" l r
  unicode (IfOnlyIf l r) =
    genericBinary unicode " (" ") " "⇔" l r

genericBinary :: (BSP -> Doc) -> String -> String -> String -> BSP -> BSP -> Doc
genericBinary f l_str r_str m_str l r =
  (text l_str) <>
  f l          <>
  (text m_str) <>
  f r          <>
  (text r_str)

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

removeIff :: BSP -> BSP
removeIff (IfOnlyIf l r) =
  let (l',r') = both removeIff (l,r)
  in And (If l' r') (If r' l')
removeIff (If l r) =
  let (l',r') = both removeIff (l,r)
  in  If l' r'
removeIff (Or l r) =
  let (l',r') = both removeIff (l,r)
  in Or l' r'
removeIff (And l r) =
  let (l',r') = both removeIff (l,r)
  in And l' r'
removeIff (Not n) = Not $ removeIff n
removeIff x = x

removeIf :: BSP -> BSP
removeIf bsp@(IfOnlyIf _ _) =
  removeIf . removeIff $ bsp
removeIf (If l r) =
  let (l',r') = both removeIf (l,r)
  in Or (Not l') r'
removeIf (Or l r) =
  let (l',r') = both removeIf (l,r)
  in Or l' r'
removeIf (And l r) =
  let (l',r') = both removeIf (l,r)
  in And l' r'
removeIf (Not n) = Not $ removeIf n
removeIf x = x

nnf :: BSP -> BSP
nnf bsp@(IfOnlyIf _ _) =
  nnf . removeIf $ bsp
nnf bsp@(If _ _) =
  nnf . removeIf $ bsp
nnf (Not (Not n)) = nnf n
nnf (Not (And l r)) =
  nnf $ Or (Not l) (Not r)
nnf (Not (Or l r)) =
  nnf $ And (Not l) (Not r)
nnf (Or l r) =
  let (l',r') = both nnf (l,r)
  in Or l' r'
nnf (And l r) =
  let (l',r') = both nnf (l,r)
  in And l' r'
nnf (Not n) =
  Not $ nnf n
nnf x = x

testBranch :: (BSP -> Bool) -> BSP -> BSP -> Bool
testBranch f l r = f l && f r

isNNF :: BSP -> Bool
isNNF (IfOnlyIf _ _) = False
isNNF (If _ _) = False
isNNF (Or l r) = testBranch isNNF l r
isNNF (And l r) = testBranch isNNF l r
isNNF (Not b) =
  case b of
   Variable' _ -> True
   Bool' _ -> True
   _ -> False
isNNF _ = True

noIfs :: BSP -> Bool
noIfs (If _ _) = False
noIfs (IfOnlyIf _ _) = False
noIfs (Or l r) = testBranch noIfs l r
noIfs (And l r) = testBranch noIfs l r
noIfs (Not b) = noIfs b
noIfs _ = True

noIffs :: BSP -> Bool
noIffs (If _ _) = False
noIffs (IfOnlyIf l r) = testBranch noIffs l r
noIffs (Or l r) = testBranch noIffs l r
noIffs (And l r) = testBranch noIffs l r
noIffs (Not b) = noIffs b
noIffs _ = True

{-|
Finds all possible equivilent logic forms of the boolean formula.

For example, swaps all And's, Or's and Iff's around. 
-}
permutations :: BSP -> [BSP]
permutations (IfOnlyIf l r) = permutations' IfOnlyIf l r True
permutations (If l r) =       permutations' If       l r False
permutations (Or l r) =       permutations' Or       l r True
permutations (And l r) =      permutations' And      l r True
permutations (Not n) =
  let n' = permutations n
  in map Not n'
permutations x = [x]


permutations' :: (BSP -> BSP -> BSP) -> BSP -> BSP -> Bool -> [BSP]
permutations' f l r associative =
  let (l',r') = both permutations (l,r)
      pairs = pairUp l' r' associative
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

numberPermutations :: BSP -> Int
numberPermutations bsp =
  case bsp of
    IfOnlyIf l r -> 2 * multiplies l r
    If l r -> multiplies l r
    And l r -> 2 * multiplies l r
    Or l r -> 2 * multiplies l r
    Not n -> numberPermutations n
    _ -> 1
  where
     multiplies l r = (numberPermutations l) * (numberPermutations r)
