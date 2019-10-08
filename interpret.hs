import System.Environment
import System.IO
-- BrainFuck interpreter By Daniel Jones
data Expr = Output | Input | Left | Right | Inc | Dec | Loop Code | Skip deriving Show
type Code = [Expr]
data State = State [Int] Int [Int] deriving Show

startState :: State
startState = State [] 0 []

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
execute Output s = s
execute Input s = s
execute Main.Right (State ls m []) = State (ls ++ [m]) 0 []
execute Main.Right (State ls m (r:rs)) = State (ls ++ [m]) r rs
execute Main.Left (State [] m rs) = State [] m rs
execute Main.Left (State (l:ls) m rs) = State ls l ([m] ++ rs)
execute Inc (State ls m rs) = State ls (m + 1) rs
execute Dec (State ls m rs) = State ls (m - 1) rs
execute (Loop c) s = doLoop c s
execute Skip s = s

executeCode :: Code -> State -> State
executeCode cs s = foldr execute s cs

doLoop :: Code -> State -> State
doLoop c s@(State _ 0 _) = s
doLoop cs s = doLoop cs (executeCode cs s)

main :: IO ()
main = do
    x <- getArgs
    if length x /= 1 then
        putStrLn("Usage: ./interpret <file>")
    else do
        src <- readFile (head x)
        let c = convert src
        print c
        let ans = executeCode c startState
        print ans
        -- print (executeCode (convert src) startState)
