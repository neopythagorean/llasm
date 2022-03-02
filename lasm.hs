-- LASM micro assembler
-- for the LC3 computer

{-# LANGUAGE BinaryLiterals #-}

import Data.List
import Data.Char
import Data.Typeable
import Data.Monoid
import Data.Bits
import Data.Maybe
import System.Environment
import Control.Monad

data Token = LabelToken { str :: String } | OpcodeToken { str :: String } | RegisterToken { str :: String } | ImmediateToken { str :: String } | AssemblerToken { str :: String } deriving ( Show )

data Register = Register { reg :: Int } deriving ( Show, Eq )

data Label = Label { text :: String } deriving ( Show, Eq )

data Immediate = Immediate { imm :: Int } deriving ( Show, Eq )

data Argument = ImmArg Immediate | RegArg Register | LabelArg Label deriving ( Show, Eq )

data Instruction = ADD | AND | BR | JMP | JSR | JSRR | LD | LDI | LDR | LEA | NOT | RET | RTI | ST | STI | STR | TRAP deriving ( Show, Eq )

data AssemblerDirection = ORIGIN | PUT deriving ( Show, Eq )

data Line = InstLine { inst :: Instruction, args :: [Argument] } | AssemLine { dir :: AssemblerDirection, args :: [Argument] } deriving ( Show, Eq )

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
        let asmlines = map tokensToLine corrected
        let (origin, rest) = getOrigin asmlines
        let (symbols, asmlines) = createSymbolTable origin rest
        mapM_ print symbols
        mapM_ print asmlines

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
isOpcodeLine = any isOpcodeToken

isOpcodeToken :: Token -> Bool
isOpcodeToken (OpcodeToken _) = True
isOpcodeToken _ = False

isAssemblerLine :: [Token] -> Bool
isAssemblerLine = any isAssemblerToken

isAssemblerToken :: Token -> Bool
isAssemblerToken (AssemblerToken _) = True
isAssemblerToken _ = False

isLabelLine :: [Token] -> Bool
isLabelLine = any isLabelToken

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
stringToLabel s = Label { text = (init s) }

-- Get all labels defined in the program
getLabels :: [[Token]] -> [Label]
getLabels prg =
    let tokens = concat $ map (filter isLabelToken) prg
    in map (stringToLabel . str) tokens 


-- Gets the origin place for the program.
getOrigin :: [([Label], Line)] -> (Int, [([Label], Line)])
getOrigin ((_, (AssemLine ORIGIN ((ImmArg (Immediate i)) : []))) : r) = (i, r) 
getOrigin _ = error "Initial Instruction is not an origin"

createSymbolTable :: Int -> [([Label], Line)] -> ([(Label, Int)], [Line])
createSymbolTable o' p' = let h _ [] l = (l, (map snd p'))
                              h o p l = h (o + lineSize (snd (head p))) (tail p) (concat [[(lbl, o) | lbl <- (fst (head p))],  l])
                          in h o' p' []

-- Tables for Instruction info -- 

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
lineToML (InstLine ADD (RegArg (Register dr) : RegArg (Register sr)  : ImmArg (Immediate i)  : [])) = [ constructBinary [(0b0001, 4), (dr, 3), (sr, 3), (0b1, 1), (i, 5)] ]
lineToML (AssemLine _ _) = [0b1010111101010000]
lineToML _ = error ("Unable to convert to ML!")

-- Get only the last s bits of n
lastBits :: Int -> Int -> Int
lastBits n s = n .&. ((1 `shiftL` s) - 1)

-- Can probably reduce this?
constructBinary :: [(Int, Int)] -> Int
constructBinary b = let h ((n, s):xs) o = h xs ((shiftL o s) .|. (lastBits n s))
                        h [] o = o
                    in h b 0

-- TODO
tokensToLine :: [Token] -> ([Label], Line)
tokensToLine l = let labels = map tokenToLabel $ filter isLabelToken l -- List of labels defined on this line
                     filt = filter (not . isLabelToken) l    -- List of Tokens w/o the labels
                     args = map tokenToArgument . tail
                 in (labels, (either (\a -> (AssemLine a (args filt))) (\i -> (InstLine i (args filt))) (tokenToOperation $ head filt)))

tokenToLabel :: Token -> Label
tokenToLabel (LabelToken s) = Label (init s)
tokenToLabel _ = error ("Cannot create label!")

tokenToArgument :: Token -> Argument
tokenToArgument (RegisterToken s) = RegArg $ Register $ stringToNumeral $ tail s
tokenToArgument (ImmediateToken s)
    | isDigit $ head $ tail s = ImmArg $ Immediate $ stringToNumeral $ tail s
    | otherwise = LabelArg $ Label $ tail s
tokenToArgument _ = error ("Cannot convert token to argument!")

tokenToOperation :: Token -> Either AssemblerDirection Instruction
tokenToOperation (OpcodeToken    op) = Right $ tokenToInstruction (OpcodeToken op)
tokenToOperation (AssemblerToken as) = Left $ tokenToAssemblerDirection (AssemblerToken as)
tokenToOperation _ = error ("Token is not an operation!")

tokenToInstruction :: Token -> Instruction
tokenToInstruction (OpcodeToken "add"  ) = ADD 
tokenToInstruction (OpcodeToken "and"  ) = AND
tokenToInstruction (OpcodeToken "br"   ) = BR 
tokenToInstruction (OpcodeToken "jmp"  ) = JMP
tokenToInstruction (OpcodeToken "jsr"  ) = JSR
tokenToInstruction (OpcodeToken "jsrr" ) = JSRR
tokenToInstruction (OpcodeToken "ld"   ) = LD
tokenToInstruction (OpcodeToken "ldi"  ) = LDI
tokenToInstruction (OpcodeToken "ldr"  ) = LDR
tokenToInstruction (OpcodeToken "lea"  ) = LEA
tokenToInstruction (OpcodeToken "not"  ) = NOT
tokenToInstruction (OpcodeToken "ret"  ) = RET
tokenToInstruction (OpcodeToken "rti"  ) = RTI
tokenToInstruction (OpcodeToken "st"   ) = ST
tokenToInstruction (OpcodeToken "sti"  ) = STI
tokenToInstruction (OpcodeToken "str"  ) = STR
tokenToInstruction (OpcodeToken "trap" ) = TRAP
tokenToInstruction _ = error ("Cannot convert to Instruction")

tokenToAssemblerDirection :: Token -> AssemblerDirection
tokenToAssemblerDirection (AssemblerToken ".origin" ) = ORIGIN
tokenToAssemblerDirection (AssemblerToken ".put"    ) = PUT
tokenToAssemblerDirection t = error ("Cannot convert to Assembler Direction " ++ (str t))



-- Converts Strings of the form '[0base]{digits}' to Int
-- TODO : Add negative support
stringToNumeral :: String -> Int
stringToNumeral s = let numType n -- Get the base of the number
                            | isPrefixOf "0h" n || isPrefixOf "0x" n  = "0123456789abcdef"
                            | isPrefixOf "0o" n                       = "01234567"
                            | isPrefixOf "0s" n                       = "012345"
                            | isPrefixOf "0b" n                       = "01" 
                            | (isDigit $ head n) || isPrefixOf "0d" n = "0123456789"
                            | otherwise = error ("Cannot convert " ++ s ++ " to int")
                        removeType q -- Remove base notation (if present)
                            | (isDigit $ head q) && (length q) > 2 && flip elem "hxosbd" (head $ tail q) = drop 2 q
                            | otherwise = q
                        convnum d s = fmap sum . sequence $ zipWith (liftM2 (*)) (map (\c -> elemIndex c d) s) (reverse [Just ((length d)^x) | x<-[0..(length s - 1)]])
                    in fromJust $ convnum (numType s) $ removeType s -- TODO Add error checking to this (handle fromJust)

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

assemblerInstructions :: [String]
assemblerInstructions = [ "origin"
                        , "put" ]


