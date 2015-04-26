module HSat.Solution.BSP (
  BSPSolution,
  bspMapping
  ) where

data BSPSolution = BSPSolution {
  bspMapping :: Map Word Bool
  } deriving (Eq)

mkSolution :: Word -> BSPSolution
mkSolution = undefined

mkRandomSolution :: (MonadRandom m) =>
                    Word -> m BSPSolution
mkRandomSolution = undefined

checkSolution :: _ --BSP -> BSPSolution
                 -- Bool
checkSolution = undefined

flip?! :: BSPSolution -> Word -> Maybe BSPSolution
flip?! = _

flip! :: BSPSolution -> Word -> BSPSolution
flip! = _

