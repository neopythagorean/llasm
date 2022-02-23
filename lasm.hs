-- LASM micro assembler
-- for the LC3 computer

{-# LANGUAGE BinaryLiterals #-}

import Data.List
import Data.Char
import Data.Typeable

import Data.Monoid

import Data.Bits

import System.Environment

data Token = LabelToken { str :: String } | OpcodeToken { str :: String } | RegisterToken { str :: String } | ImmediateToken { str :: String } | AssemblerToken { str :: String } deriving ( Show )

data Register = Register { reg :: Int } deriving ( Show, Eq )

data Label = Label { text :: String, resolution :: Int } deriving ( Show, Eq )

data Immediate = Immediate { imm :: Int } deriving ( Show, Eq )

data Argument = ImmArg Immediate | RegArg Register deriving ( Show, Eq )

-- Label Symbol
data Symbol = Symbol { symb :: String } deriving ( Show, Eq )

data Instruction = ADD | AND | BR | JMP | JSR | JSRR | LD | LDI | LDR | LEA | NOT | RET | RTI | ST | STI | STR | TRAP deriving ( Show, Eq )

data AssemlerDirection = ORIGIN | PUT deriving ( Show, Eq )

data Line = InstLine Instruction [Argument] | AssemLine AssemlerDirection [Argument] deriving ( Show, Eq )

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
        let labels = getLabels corrected
        mapM_ print corrected
        print labels

printUsage :: IO ()
printUsage = do
    putStrLn $ intercalate "\n" [ "-- The Lazy Assembler --"
                                , "      For the LC-3" ]

-- Todo, redo this to not just do words
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
isSignificantLine = getAny . foldMap (Any .) [isOpcodeLine, isAssemblerLine]

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
stringToRegister s = Register { reg = read $ tail s :: Int }

stringToLabel :: String -> Label
stringToLabel s = Label { text = (init s), resolution = 0 }

-- Get all labels defined in the program
getLabels :: [[Token]] -> [Label]
getLabels prg =
    let tokens = concat $ map (filter isLabelToken) prg
    in map (stringToLabel . str) tokens 


-- Tables for Instruction info

-- Returns the number of args expected for each instruction
numArgs :: Instruction -> Int
numArgs ADD  = 3
numArgs AND  = 3
numArgs BR   = 2
numArgs JMP  = 1
numArgs JSR  = 1
numArgs JSRR = 1
numArgs LD   = 2
numArgs LDI  = 2
numArgs LDR  = 3
numArgs LEA  = 2
numArgs NOT  = 1
numArgs RET  = 0
numArgs RTI  = 0
numArgs ST   = 2
numArgs STI  = 2
numArgs STR  = 3
numArgs TRAP = 1

-- Get the amount of addressible space this line takes up
lineSize :: Line -> Int
lineSize (InstLine _ _)  = 1 -- All LC3 Instructions are only 1 address in memory
lineSize (AssemLine _ _) = 1

lineToML :: Line -> [Int]
lineToML (InstLine ADD (RegArg (Register dr) : RegArg (Register sr1) : RegArg (Register sr2) : [])) = [ constructBinary [(0b0001, 4), (dr, 3), (sr1, 3), (0b000, 3), (sr2, 3)] ]
lineToML (InstLine ADD (RegArg (Register dr) : RegArg (Register sr) : ImmArg (Immediate i) : [])) = [ constructBinary [(0b0001, 4), (dr, 3), (sr, 3), (0b1, 1), (i, 5)] ]
lineToML (InstLine _ _)  = [0b1111000011110000]
lineToML (AssemLine _ _) = [0b1010111101010000]

-- Get only the last s bits of n
lastBits :: Int -> Int -> Int
lastBits n s = n .&. ((2 ^ s) - 1)

-- Can probably reduce this?
constructBinary :: [(Int, Int)] -> Int
constructBinary b = let h ((n, s):xs) o = h xs ((shiftL o s) .|. (lastBits n s))
                        h [] o = o
                    in h b 0

opcodes :: [String] 
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


