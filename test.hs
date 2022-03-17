import Prelude

game :: Int -> IO ()
game 0 = return ()
game n = do
    putStrLn "guess number: 0-99"
    number <- getLine
    let y = read number
    let x = 20
    let action | y > x = putStrLn "your number is greater than x" >> game (n-1)
               | y < x = putStrLn "your number is less than x" >> game (n-1)
               | otherwise = putStrLn "U win!!"
    action