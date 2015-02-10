import Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "Parsing 1000 small files" []
  ]
