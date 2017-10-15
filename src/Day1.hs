module Day1 where
import Text.Parsec

data Direction = Up | Down deriving Show

main = do
  input <- readFile "inputs/input1.txt"
  putStr "solution a: "
  case day1a input of
    (Right x)  -> print x
    (Left err) -> print err
  putStr "solution b: "
  case day1b input of
    (Right x)  -> print x
    (Left err) -> print err

day1a input = do
  d <- myParse input
  return (foldl evaluateParen 0 d)

day1b input = do
  d <- myParse input
  return $ length 
         $ takeWhile (/= (-1)) 
         $ scanl evaluateParen 0 d

myParse = parse parseFile ""

parseFile = many paren <* eof

paren :: Parsec String st Direction
paren = toFloor <$> (oneOf "()")

toFloor '(' = Up
toFloor ')' = Down

evaluateParen :: Int -> Direction -> Int
evaluateParen x Up   = x + 1
evaluateParen x Down = x - 1
