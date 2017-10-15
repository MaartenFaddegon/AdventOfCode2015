module Day2 where
import Text.Parsec
import Text.ParserCombinators.Parsec.Number(int)

data Cuboid = Cuboid Int Int Int deriving Show

main = do
  input <- readFile file
  case parse cuboids file input of
    (Left err) -> print err
    (Right cs) -> do
      putStrLn $ "solution a: " ++ show (sum $ map paper cs)
      putStrLn $ "solution b: " ++ show (sum $ map ribbon cs)
  where file = "inputs/input2.txt"

paper :: Cuboid -> Int 
paper (Cuboid w l h) = minimum [s1, s2, s3] + 2*s1 + 2*s2 + 2*s3
  where s1 = w*l
        s2 = w*h
        s3 = l*h

ribbon (Cuboid w l h) = minimum [p1, p2, p3] + w*l*h
  where p1 = 2*w + 2*l
        p2 = 2*w + 2*h
        p3 = 2*l + 2*h
        

cuboids = (endBy1 cuboid newline) <* eof

cuboid = Cuboid <$> firstDigit <*> nextDigit <*> nextDigit
  where
  firstDigit = int
  nextDigit  = (char 'x') *> int 
