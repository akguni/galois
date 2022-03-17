import Prelude
import Data.Char
import System.IO (hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

vectorToDec :: Int -> [Int] -> Int
vectorToDec base = foldr (\x xs -> x + base * xs) 0

decToVector :: Int -> Int -> [Int]
decToVector base n | n < base  = [n]
                   | otherwise = (mod n base): decToVector base (div n base)

elementsOrders :: Int -> [(Int, Int)]
elementsOrders p = map (\n -> (n, order n p)) $ coprimes p

order :: Int -> Int -> Int
order element base = orderHelp element base (fromIntegral element) 1
  where
    orderHelp :: Int -> Int -> Integer -> Int -> Int
    orderHelp e b r o | mod r (fromIntegral b) == 1 = o
                      | otherwise    = orderHelp e b (r * fromIntegral e) (succ o)

generators :: Int -> [Int]
generators base | elem base $ allprimes base  = filter (\x -> (order x base == pred base)) [2..pred base]
                | otherwise                   = []

primePower :: Int -> (Int, Int)
primePower n = divisible n (allprimes n) 
  where
    divisible _ []                    = (-1, -1)
    divisible n (x:xs) | mod n x == 0 = power n x 1
                       | otherwise    = divisible n xs

    power n x p        | n == x        = (n, p)
                       | mod n x == 0  = power (div n x) x (succ p)
                       | otherwise     = (-1, -1)

coprimes :: Int -> [Int]
coprimes n = filter (\x -> gcd n x == 1) [2..pred n]

allprimes :: Int -> [Int]
allprimes n = sieve [2..n]
  where
    sieve :: [Int] -> [Int]
    sieve  [] = []
    sieve (p:xs) = p : (sieve (filter (\x -> mod x p /= 0) xs))

irreducible :: Int -> [Int] -> Bool
irreducible base poly = (length poly > 1) && foldr ((&&).f) True [0..pred base]
  where f x = (mod (vectorToDec x poly) base /= 0)

addPoly :: Int -> [Int] -> [Int] -> [Int]
addPoly base xs ys = map (\x -> mod x base) $ go xs ys
  where go xs ys | length xs > length ys = zipWith (+) xs $ ys ++ repeat 0
                 | otherwise             = zipWith (+) ys $ xs ++ repeat 0

subPoly :: Int -> [Int] -> [Int] -> [Int]
subPoly base xs ys = normalize $ addPoly base xs $ map negate ys

normalize :: [Int] -> [Int]
normalize []                = [] 
normalize xs | last xs == 0 = normalize $ init xs
             | otherwise    = xs    

multPoly :: Int -> [Int] -> [Int] -> [Int]
multPoly base [] ys       = []
multPoly base (xcoeff: xcfs) ys  = addPoly base (cfmult xcoeff ys) $ varmult $ multPoly base xcfs ys
  where cfmult x ys = map (* x) ys
        varmult ys  = 0:ys

divPoly :: Int -> [Int] -> [Int] -> [Int]
divPoly base num den | length den > length num = num
                     | otherwise               = divPoly base (subPoly base num (subEq base num (subDiv base num den))) den 

subDiv :: Int -> [Int] -> [Int] -> [Int]
subDiv base num den | length num > length den = subDiv base num (0:den)
                    | otherwise               = den

subEq :: Int -> [Int] -> [Int] -> [Int]
subEq base num den = multPoly base [(last num)] den


polyNomials :: Int -> Int -> [[Int]]
polyNomials p r = map (\x -> decToVector p x) [0..p^r-1]

multTable :: Int -> Int -> [Int] -> [[Int]]
multTable p r irr = let poly = polyNomials p r in [ divPoly p (multPoly p x y) irr | x <- poly, y <- poly]


multTable' :: Int -> Int -> [Int] -> [(Int,Int,Int)]
multTable' p r irr = let poly = polyNomials p r in [(vectorToDec p x, vectorToDec p y, vectorToDec p (divPoly p (multPoly p x y) irr)) | x <- poly, y <- poly]


addTable :: Int -> Int -> [(Int, Int, Int)]
addTable p r = let poly = polyNomials p r in [(vectorToDec p x, vectorToDec p y, vectorToDec p (addPoly p x y)) | x <- poly, y <- poly]

createIrreducible :: Int -> Int -> [Int]
createIrreducible p r = head $ filter (irreducible p) candidates
  where candidates = map (\x -> decToVector p x) [p^r..(p^(r+1)-1)]

addP :: Int -> Int -> Int -> Int
addP gf s1 s2 = let pr = primePower gf
                    p = fst pr
                  in vectorToDec p $ addPoly p (decToVector p s1) (decToVector p s2)


subP :: Int -> Int -> Int -> Int
subP gf s1 s2 = let pr = primePower gf
                    p = fst pr
                  in vectorToDec p $ subPoly p (decToVector p s1) (decToVector p s2)


multP :: Int -> Int -> Int -> Int
multP gf s1 s2 = vectorToDec p reduced
                   where pr = primePower gf
                         p = fst pr
                         r = snd pr 
                         reduced = divPoly p mult irr
                         mult = multPoly p (decToVector p s1) (decToVector p s2)
                         irr = createIrreducible p r

divP :: Int -> Int -> Int -> Int
divP gf s1 s2 = let pr = primePower gf
                    p = fst pr
                  in vectorToDec p $ divPoly p (decToVector p s1) (decToVector p s2)


parseTable :: [String] -> [(Int,Int,[Int])]
parseTable [] = []
parseTable (poly:ps) = (idPrime poly, idOrder poly, idPoly poly) : parseTable ps
  where idPrime l = read $ takeWhile (/=',') $ tail l
        idOrder l = read $ takeWhile (/=',') $ tail $ dropWhile (/=',') $ tail l
        idPoly  l = read $ init $ init $ dropWhile (/='[') $ tail l


findConway :: Int -> Int -> [(Int,Int,[Int])] -> [Int]
findConway p r ((prime,order,poly):table) | p == prime && r == order = poly
                                          | otherwise                = findConway p r table

tablePrint :: [(Int,Int,Int)] -> IO ()
tablePrint ((x,y,r):[])                          = do
                                                     putStrLn (formatPrint 3 (show r))
                                                     return ()

tablePrint ((xx,xy,xr):(yx,yy,yr):ys) | xx == yx = do
                                                    putStr (formatPrint 3 (show xr))
                                                    tablePrint ((yx,yy,yr):ys)
                                                    return ()
                                      | otherwise = do
                                                    putStrLn (formatPrint 3 (show xr))
                                                    tablePrint ((yx,yy,yr):ys)
                                                    return ()

formatPrint :: Int -> String -> String
formatPrint l s = if length s < l then formatPrint l (' ':s) else (s ++ " ")


cayleyTable :: [(Int,Int,[Int])] -> IO ()
cayleyTable conwayPs = do
  gftriple <- askGF
  let gf = (\(x,y,z) -> x) gftriple
      prime = (\(x,y,z) -> y) gftriple
      order = (\(x,y,z) -> z) gftriple
  irreducible <- askIrr gftriple conwayPs
  putStrLn ("Using " ++ show (vectorToDec prime irreducible) ++ " as irreducible polynomial.")
  
  let cayley      = multTable' prime order irreducible
  tablePrint cayley
  return ()

additionTable :: IO ()
additionTable = do
  gftriple <- askGF
  let gf = (\(x,y,z) -> x) gftriple
      prime = (\(x,y,z) -> y) gftriple
      order = (\(x,y,z) -> z) gftriple
      cayley      = addTable prime order 

  tablePrint cayley
  return ()

askGF :: IO (Int, Int, Int)
askGF = do
  putStr "Size of GF: "
  input <- getLine
  let invalid = "Invalid Galois Field.\nThis value is not the power of a prime integer.\nPlease try again.\n"
  case (reads input) of
    [(gf, "")] -> do
      let prime       = fst $ primePower $ gf
          order       = snd $ primePower $ gf          
      if prime > 1 then do
          let galoisProperties = "Galois Field: " ++ show gf ++ " Prime Number: " ++ show prime ++ " Power: " ++ show order
          putStrLn galoisProperties
          return  (gf, prime, order)
        else putStr invalid >> askGF
    _       -> putStr invalid >> askGF


askIrr :: (Int, Int, Int) -> [(Int,Int,[Int])] -> IO ([Int])
askIrr (gf, prime, order) conwayPs = do
  putStrLn ("You can enter your own irreducible polynomial as an Integer value between " ++ show gf ++ " and " ++ show (pred (gf * prime)))
  putStrLn "OR just hit return and the program will identify a suitable irreducable polynomial using Frank Lübeck's Conway Tables"
  input <- getLine
  let ir = (\[(x,_)] -> x) (reads input :: [(Int, String)])
  let validate  | input == "" = return (findConway prime order conwayPs)
                | (reads input :: [(Int, String)]) == [] = putStrLn "Invalid entry. Please try again." >> askIrr (gf, prime, order) conwayPs
                | (ir < gf) || (ir > (gf * prime - 1))   = putStrLn "Polynomial out of range. Please try again." >> askIrr (gf, prime, order) conwayPs
                | not (irreducible prime (decToVector prime ir)) = putStrLn "This polynomial has a zero root. Please try again." >> askIrr (gf, prime, order) conwayPs
                | otherwise                                      = return (decToVector prime ir)
  validate

arithmetic :: [(Int,Int,[Int])] -> IO ()
arithmetic conwayPs = do
  gftriple <- askGF
  let gf = (\(x,y,z) -> x) gftriple
      prime = (\(x,y,z) -> y) gftriple
      order = (\(x,y,z) -> z) gftriple      
  irreducible <- askIrr gftriple conwayPs
  putStrLn ("Using " ++ show (vectorToDec prime irreducible) ++ " as irreducible polynomial.")
  putStrLn "Enter first polynomial (integer value):"
  p1i <- getLine
  putStrLn "Enter second polynomial (integer value):"
  p2i <- getLine
  let p1          = decToVector prime (read p1i)
      p2          = decToVector prime (read p2i)
      sMult p     = vectorToDec prime $ divPoly prime (multPoly prime p p2) irreducible
      rAdd        = p1i ++ " + " ++ p2i ++ " = " ++ show (vectorToDec prime $ addPoly prime p1 p2)                 
      rSub        = p1i ++ " - " ++ p2i ++ " = " ++ show (vectorToDec prime $ subPoly prime p1 p2)
      rMult       = p1i ++ " * " ++ p2i ++ " = " ++ show (sMult p1)                                         
      rDiv        = p1i ++ " / " ++ p2i ++ " = " ++ show  (foldr (\x xs -> if sMult (decToVector prime x) == (read p1i) then x else xs) 0 [0..(pred gf)])
  putStrLn rAdd
  putStrLn rSub
  putStrLn rMult
  putStrLn rDiv
  return ()

menuLoop :: [(Int,Int,[Int])] -> IO ()
menuLoop conwayPs = do
  putStrLn "\nFINITE FIELDS MENU"
  putStrLn "0 to generate a Multiplication Table"
  putStrLn "1 to generate an Addition Table"
  putStrLn "2 to perform all 4 arithmetic ops on 2 values"
  putStrLn "3 to exit"
  m <- getChar
  putStrLn ""
  case m of
    '0' -> cayleyTable conwayPs >> menuLoop conwayPs 
    '1' -> additionTable >> menuLoop conwayPs 
    '2' -> arithmetic conwayPs >> menuLoop conwayPs 
    '3' -> return ()
    otherwise ->  menuLoop conwayPs


filename = "CPimport.txt"

main :: IO ()
main = do
-- Deaktivierung des Bufferings fuer direkte Ein- und Ausgabe:
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  file <- readFile filename
  let conwayPs = parseTable $ lines file
  menuLoop conwayPs
  return () 
