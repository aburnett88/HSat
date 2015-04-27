module HSat.Problem.BSP.Internal (
  BSP(..)
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
  deriving (Eq,Show)

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
