import System.Environment
import System.IO
import Data.Char
-- BrainFuck interpreter By Daniel Jones
data Expr = Output | Input | Left | Right | Inc | Dec | Loop Code | Skip deriving Show
type Code = [Expr]
data State = State [Int] Int [Int] (IO ())

instance Show State where
    -- show :: State -> String
    show (State ls m rs _) = (show ls) ++ (show m) ++ (show rs)

startState :: State
startState = State [] 0 [] (return ())

convert :: String -> Code
convert [] = []
convert ('[':xs) = Loop (convert (loopCode xs 0)) : (convert (outerCode xs 0))
    where
    loopCode :: String -> Int -> String
    loopCode []       _ = []
    loopCode (']':xs) 0 = [] 
    loopCode (']':xs) n = ']' : (loopCode xs (n-1))
    loopCode ('[':xs) n = '[' : (loopCode xs (n+1))
    loopCode ( x :xs) n = x : (loopCode xs n)

    outerCode :: String -> Int -> String
    outerCode [] n = []
    outerCode (']':xs) 0 = xs
    outerCode (']':xs) n = outerCode xs (n-1)
    outerCode ('[':xs) n = outerCode xs (n+1)
    outerCode (x:xs) n = outerCode xs n
convert (x:xs) = (f x) : (convert xs)
    where
    f :: Char -> Expr
    f '.' = Output
    f ',' = Input
    f '>' = Main.Right
    f '<' = Main.Left
    f '+' = Inc
    f '-' = Dec
    f  _  = Skip

execute :: Expr -> State -> State
execute Output (State l m r o) = State l m r (print (chr m))
execute Input (State l m r _) = State l m r (return ())
execute Main.Right (State ls m [] _) = State (ls ++ [m]) 0 [] (return ())
execute Main.Right (State ls m (r:rs) _) = State (ls ++ [m]) r rs (return ())
execute Main.Left (State [] m rs _) = State [] m rs (return ())
execute Main.Left (State (l:ls) m rs _) = State ls l ([m] ++ rs) (return ())
execute Inc (State ls m rs _) = State ls (m + 1) rs (return ())
execute Dec (State ls m rs _) = State ls (m - 1) rs (return ())
execute (Loop c) s = doLoop c s
execute Skip s = s

executeCode :: Code -> State -> State
executeCode cs s = foldr execute s cs

doLoop :: Code -> State -> State
doLoop c s@(State _ 0 _ _) = s
doLoop cs s = doLoop cs (executeCode cs s)

main :: IO ()
main = do
    x <- getArgs
    if length x /= 1 then
        putStrLn("Usage: ./interpret <file>")
    else do
        src <- readFile (head x)
        let c = convert src
        --print c
        let (State _ _ _ a) = executeCode c startState
        a
        --print ans
        -- return ()
        -- print (executeCode (convert src) startState)
