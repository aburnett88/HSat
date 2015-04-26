module HSat.Problem.BSP.Internal (
  BSP(..),
  ComplexExpr(..),
  AtomicExpr(..)
  ) where

import HSat.Problem.BSP.Common.Variable
import HSat.Printer

data BSP =
  AtomicExpr' AtomicExpr |
  ComplexExpr' ComplexExpr
  deriving (Eq,Show)

instance Printer BSP where
  compact (AtomicExpr' a) = compact a
  compact (ComplexExpr' c) = compact c
  noUnicode (AtomicExpr' a) = noUnicode a
  noUnicode (ComplexExpr' c) = noUnicode c
  unicode (AtomicExpr' a) = unicode a
  unicode (ComplexExpr' c) = unicode c

data AtomicExpr =
  Bool' Bool |
  Variable' Variable
  deriving (Eq,Show)

instance Printer AtomicExpr where
  compact (Bool' True) = text "T"
  compact (Bool' False) = text "F"
  compact (Variable' v) = compact v
  noUnicode (Bool' b) = text . show $ b
  noUnicode (Variable' v) = noUnicode v
  unicode (Bool' True) = green . text $ "True"
  unicode (Bool' False) = red . text $ "False"
  unicode (Variable' v) = unicode v  

data ComplexExpr =
  Not BSP |
  Or BSP BSP |
  And BSP BSP |
  If BSP BSP |
  IfOnlyIf BSP BSP
  deriving (Eq,Show)

instance Printer ComplexExpr where
  compact (Not bsp) = (text " !") <> compact bsp
  compact (Or l r) = (text "(")  <>
                     compact l    <>
                     (text "\\/") <>
                     compact r    <>
                     (text ")")
  compact (And l r) = (text "(")  <>
                      compact l    <>
                      (text "/\\") <>
                      compact r    <>
                      (text ")")
  compact (If l r) = (text "(")  <>
                      compact l    <>
                      (text "=>") <>
                      compact r    <>
                      (text ")")
  compact (IfOnlyIf l r) = (text "(")  <>
                      compact l    <>
                      (text "<=>") <>
                      compact r    <>
                      (text ")")
  noUnicode (Not bsp) = (text " !") <> noUnicode bsp
  noUnicode (Or l r) = (text " (")  <>
                       noUnicode l  <>
                       (text "\\/") <>
                       noUnicode r  <>
                       (text ") ")
  noUnicode (And l r) = (text " (")  <>
                        noUnicode l  <>
                        (text "/\\") <>
                        noUnicode r  <>
                        (text ") ")
  noUnicode (If l r) = (text " (")  <>
                       noUnicode l  <>
                       (text "=>") <>
                       noUnicode r  <>
                       (text ") ")
  noUnicode (IfOnlyIf l r) = (text " (")  <>
                       noUnicode l  <>
                       (text "<=>") <>
                       noUnicode r  <>
                       (text ") ")
  unicode (Not bsp) = (text "¬") <> unicode bsp
  unicode (Or l r) = (text " (")  <>
                     unicode l    <>
                     (text "∨") <>
                     unicode r    <>
                     (text ") ")
  unicode (And l r) = (text " (")  <>
                      unicode l    <>
                      (text "∧") <>
                      unicode r    <>
                      (text ") ")
  unicode (If l r) = (text " (")  <>
                      unicode l    <>
                      (text "⇒") <>
                      unicode r    <>
                      (text ") ")
  unicode (IfOnlyIf l r) = (text " (")  <>
                      unicode l    <>
                      (text "⇔") <>
                      unicode r    <>
                      (text ") ")
