-- LASM micro assembler
-- for the LC3 computer

import Data.List
import Data.Char
import Data.Typeable

import System.Environment

data Token = LabelToken String | OpcodeToken String | RegisterToken String | ImmediateToken String | AssemblerToken String deriving (Show)

data Register = Register { number :: Int } deriving (Show, Eq, Read)

opcodes = [ "add"
          , "and"
          , "br"
          , "jmp"
          , "jsr"
          , "jsrr"
          , "ld"
          , "ldi"
          , "ldr"
          , "lea"
          , "not"
          , "ret"
          , "rti"
          , "st"
          , "sti"
          , "str"
          , "trap" ]

main :: IO ()
main = do
    args <- getArgs
    if null args then printUsage
    else do
        contents <- readFile $ head args
        let srclines = lines contents
        let tokenlines = filter (not . null) $ map (tokenizeLine . removeComments) srclines
        --mapM_ print $ map isSignificantLine tokenlines
        --mapM_ print tokenlines
        let corrected = moveLabelsDown tokenlines
        mapM_ print corrected

printUsage :: IO ()
printUsage = do
    putStrLn $ intercalate "\n" [ "-- The Lazy Assembler --"
                                , "      For the LC-3" ]

tokenizeLine :: String -> [Token]
tokenizeLine = map tokenize . words

tokenize :: String -> Token
tokenize s
    | head s == '%' = RegisterToken s
    | head s == '$' = ImmediateToken s
    | head s == '.' = AssemblerToken s
    | last s == ':' = LabelToken s
    | elem (map toLower s) opcodes = OpcodeToken s
    | otherwise = error ("Unable to tokenize string \"" ++ s ++ "\"")

isOpcodeLine :: [Token] -> Bool
isOpcodeLine = (foldl (||) False) . (map isOpcodeToken)

isOpcodeToken :: Token -> Bool
isOpcodeToken (OpcodeToken _) = True
isOpcodeToken _ = False

isAssemblerLine :: [Token] -> Bool
isAssemblerLine = (foldl (||) False) . (map isAssemblerToken)

isAssemblerToken :: Token -> Bool
isAssemblerToken (AssemblerToken _) = True
isAssemblerToken _ = False

isLabelLine :: [Token] -> Bool
isLabelLine = (foldl (||) False) . (map isLabelToken)

isLabelToken :: Token -> Bool
isLabelToken (LabelToken _) = True
isLabelToken _ = False

isSignificantLine :: [Token] -> Bool
isSignificantLine l = (isOpcodeLine l) || (isAssemblerLine l)

removeComments :: String -> String
removeComments s = let h (Just k) = take k s
                       h Nothing = s
                   in h $ elemIndex ';' s

-- Removes lone labels
moveLabelsDown :: [[Token]] -> [[Token]]
moveLabelsDown prg = let h [] _ p' = reverse p' -- Hacky reverse, fix later?
                         h (x:p) l p'
                             | isSignificantLine x = h p [] ( (l ++ x) : p')
                             | isLabelLine x = h p ((filter isLabelToken x) ++ l) p'
                             | otherwise = h p l (x:p')
                     in h prg [] []

-- Need to add checking here to make sure r <= 7
stringToRegister :: String -> Register
stringToRegister s = Register { number = read $ tail s :: Int }


