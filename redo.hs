import System.Process

main = do
  _ <- createProcess $ shell "./redo.do"
  return ()