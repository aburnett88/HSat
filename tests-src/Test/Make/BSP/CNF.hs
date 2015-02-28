module Test.Make.BSP.CNF (
  tests
  ) where

import TestUtils
import qualified Test.Make.BSP.CNF.Internal as Internal

name :: String
name = "CNF"

tests :: TestTree
tests =
  testGroup name [
    Internal.tests
    ]
  {-
    testGroup "evaluateCNFConfig" [
       evaluateCNFConfigTest1
       ],
    Internal.tests
    ]

evaluateCNFConfigTest1 :: TestTree
evaluateCNFConfigTest1 =
  testProperty ("Test CNF created. If failed, test everything but var number
in " ++
                "\"evaluateCNFConfig'\"") $ ioProperty $ do
    config <- generate arbitrary
    cnf <- evaluateCNFConfigErr config
    case cnf of
      (Left r) -> do
        newCnf <- evaluateCNFConfig config
        case newCnf of
          (Just ddd,newCnf') -> return $ checkCNFAgainstConfig True ddd newCnf'
          (Nothing,newCnf') -> return $ checkCNFAgainstConfig True
config newCnf'
      Right cnf' -> do
        return $ checkCNFAgainstConfig True config cnf'

checkCNFAgainstConfig :: Bool -> CNFConfig -> CNF -> Property
checkCNFAgainstConfig testVars config cnf =
  let clauseNumb       = getNoClauses cnf
      exptdClauseNum   = numbClauses config
      posVars          = getSetPos . getClauses $ cnf
      negVars          = getSetNeg . getClauses $ cnf
      allVars          = getSetOfVars . getClauses $ cnf
      varNumb          = getNoVars cnf
      exptdVars        = S.fromList . map mkVariable $ (if varNumb==0 then
                                                          [] else
                                                          [1..varNumb])
      clauseSizes      = V.toList . V.map clauseLength . getVectOfClauses .
getClauses $ cnf
      exptdClauseSizes = varsInEachClause config
      validateCNF      = validate cnf
  in (checkBounds clauseNumb exptdClauseNum) .&&.
     (if (varsAppearBothPosAndNeg config) then
        (testEq "Positive vars unequal" posVars exptdVars) .&&.
        (testEq "Negative vars unequal" negVars exptdVars) else
        (testEq "All vars unequal" allVars exptdVars)
        ) .&&.
     (testList exptdClauseSizes clauseSizes) .&&.
     (validateCNF) .&&.
     (if testVars then
        case numbVariables config of
          (Left doubles) -> checkBounds  varNumb (convertVariableNumber
clauseNumb (numbVariables config))
          (Right nonDoubles) -> checkBounds varNumb nonDoubles else
        property True
        )
-}
