-- LASM micro assembler
-- for the LC3 computer

{-# LANGUAGE BinaryLiterals #-}

import Data.List
import Data.Char
import Data.Typeable
import Data.Monoid
import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe
import System.Environment
import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put


data Token = LabelToken     { str :: String } 
           | OpcodeToken    { str :: String }
           | RegisterToken  { str :: String }
           | ImmediateToken { str :: String }
           | AssemblerToken { str :: String } deriving ( Show )

data Register = Register { reg :: Int } deriving ( Show, Eq )

data Label = Label { text :: String, labelType :: LabelType } deriving ( Show )
instance Eq Label where
    (Label a _) == (Label b _) = a == b

data LabelType = LabelDefinition | LabelRelative | LabelAbsolute | LabelInferred deriving ( Show, Eq )

data Immediate = Immediate { imm :: Int } deriving ( Show, Eq )

data Argument = ImmArg Immediate | RegArg Register | LabelArg Label deriving ( Show, Eq )

data Instruction = ADD | AND | BR | JMP | JSR | JSRR | LD | LDI | LDR | LEA | NOT | RET | RTI | ST | STI | STR | TRAP deriving ( Show, Eq )

data AssemblerDirection = ORIGIN | PUT deriving ( Show, Eq )

data Line = InstLine { inst :: Instruction, args :: [Argument], lineNum :: Int } | AssemLine { dir :: AssemblerDirection, args :: [Argument], lineNum :: Int } deriving ( Show, Eq )

main :: IO ()
main = do
    args <- getArgs
    if null args then printUsage
    else do
        contents <- readFile $ head args
        let srclines = lines contents
        let tokenlines = filter (not . null) $ map (tokenizeLine . removeComments) srclines
        let corrected = moveLabelsDown tokenlines
        let asmlines = map tokensToLine corrected
        let (origin, rest) = getOrigin asmlines
        let (symbols, asmlines) = createSymbolTable origin rest
        let resolved = resolveAllLabels symbols asmlines
        let ml = concat $ ([origin] : (map lineToML resolved))
        let words = map (\i -> fromIntegral (i :: Int) :: Word16) ml
        -- Write file out
        fileOut <- openFile "a.bin" WriteMode
        BL.hPutStr fileOut $ runPut (mapM_ putWord16le words)
        hClose fileOut
        mapM_ print symbols
        mapM_ print resolved
        mapM_ print words

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

-- Gets the origin place for the program.
getOrigin :: [([Label], Line)] -> (Int, [([Label], Line)])
getOrigin ((_, (AssemLine ORIGIN ((ImmArg (Immediate i)) : []) _)) : r) = (i, r) 
getOrigin _ = error "Initial Instruction is not an origin"

createSymbolTable :: Int -> [([Label], Line)] -> ([(Label, Int)], [Line])
createSymbolTable o' p' = let h _ [] l q = (l, (reverse q))
                              h o p l q = h (o + lineSize (snd (head p))) (tail p) (concat [[(lbl, o) | lbl <- (fst (head p))],  l]) ((replaceLineNum (snd (head p)) o) : q)
                          in h o' p' [] []

replaceLineNum :: Line -> Int -> Line
replaceLineNum (InstLine i a _) n = InstLine i a n
replaceLineNum (AssemLine d a _) n = AssemLine d a n

getLabelResolution :: [(Label, Int)] -> Label -> Int
getLabelResolution symb l = let r = lookup l symb
                            in if (isNothing r) then error ("Label not found in symbol tabel!")
                               else fromJust r

resolveLabel :: [(Label, Int)] -> Int -> Argument -> Argument
resolveLabel symb n (LabelArg l)
    | (labelType l) == LabelRelative = ImmArg $ Immediate (getLabelResolution symb l - n)
    | (labelType l) == LabelAbsolute = ImmArg $ Immediate (getLabelResolution symb l)
    | (labelType l) == LabelInferred = ImmArg $ Immediate (getLabelResolution symb l)
resolveLabel _ _ a = a

resolveLabelsLine :: [(Label, Int)] -> Line -> Line
resolveLabelsLine symb (InstLine o args n) = InstLine o (map (resolveLabel symb n) args) n
resolveLabelsLine symb (AssemLine d args n) = AssemLine d (map (resolveLabel symb n) args) n

resolveAllLabels :: [(Label, Int)] -> [Line] -> [Line]
resolveAllLabels symb lines = map (resolveLabelsLine symb) lines

-- Tables for Instruction info -- 

-- Get the amount of addressible space this line takes up
lineSize :: Line -> Int
lineSize (InstLine _ _ _)  = 1 -- All LC3 Instructions are only 1 address in memory
lineSize (AssemLine PUT args _) = length args
lineSize (AssemLine _ _ _) = 1

getArgValue :: Argument -> Int
getArgValue (RegArg (Register r)) = r
getArgValue (ImmArg (Immediate i)) = i
getArgValue _ = error ("Cannot get Argument value!")

isInstLine :: Line -> Bool
isInstLine (InstLine _ _ _) = True
isInstLine _ = False

isAssemLine :: Line -> Bool
isAssemLine = not . isInstLine

lineToML :: Line -> [Int]
lineToML (InstLine  ADD  (RegArg (Register dr) : RegArg (Register sr1) : RegArg (Register sr2) : []) _) = [constructBinary [(0b0001, 4), (dr, 3),     (sr1, 3), (0b000, 3), (sr2, 3)]]
lineToML (InstLine  ADD  (RegArg (Register dr) : RegArg (Register sr)  : ImmArg (Immediate i)  : []) _) = [constructBinary [(0b0001, 4), (dr, 3),     (sr, 3),  (0b1, 1),   (i, 5)]]
lineToML (InstLine  AND  (RegArg (Register dr) : RegArg (Register sr1) : RegArg (Register sr2) : []) _) = [constructBinary [(0b0101, 4), (dr, 3),     (sr1, 3), (0b000, 3), (sr2, 3)]]
lineToML (InstLine  AND  (RegArg (Register dr) : RegArg (Register sr)  : ImmArg (Immediate i)  : []) _) = [constructBinary [(0b0101, 4), (dr, 3),     (sr, 3),  (0b1, 1),   (i, 5)]]
lineToML (InstLine  NOT  (RegArg (Register dr) : RegArg (Register sr)  : [])                         _) = [constructBinary [(0b1001, 4), (dr, 3),     (sr, 3),  (0b111111, 6)]]
lineToML (InstLine  BR   (ImmArg (Immediate c) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0000, 4), (c, 3),      (o, 9)]]
lineToML (InstLine  JMP  (RegArg (Register br) : [])                                                 _) = [constructBinary [(0b1100, 4), (0b000, 3),  (br, 3),  (0b000000, 6)]]
lineToML (InstLine  JSR  (ImmArg (Immediate o) : [])                                                 _) = [constructBinary [(0b0100, 4), (0b1, 1),    (o, 11)]]
lineToML (InstLine  JSRR (RegArg (Register br) : [])                                                 _) = [constructBinary [(0b0100, 4), (0b000, 3),  (br, 3),  (0b000000, 6)]]
lineToML (InstLine  RET  []                                                                          _) = [0b0100000111000000] -- Ret is just JSR %7
lineToML (InstLine  LD   (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0010, 4), (dr, 3),     (o, 9)]]
lineToML (InstLine  LDI  (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1010, 4), (dr, 3),     (o, 9)]]
lineToML (InstLine  LDR  (RegArg (Register sr) : RegArg (Register br)  : ImmArg (Immediate o)  : []) _) = [constructBinary [(0b0110, 4), (sr, 3),     (br, 3),  (o, 6)]]
lineToML (InstLine  LEA  (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1110, 4), (dr, 3),     (o, 9)]]
lineToML (InstLine  ST   (RegArg (Register sr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0011, 4), (sr, 3),     (o, 9)]]
lineToML (InstLine  STI  (RegArg (Register sr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1011, 4), (sr, 3),     (o, 9)]]
lineToML (InstLine  STR  (RegArg (Register sr) : RegArg (Register br)  : ImmArg (Immediate o)  : []) _) = [constructBinary [(0b0111, 4), (sr, 3),     (br, 3),  (o, 6)]]
lineToML (InstLine  TRAP (ImmArg (Immediate o) : [])                                                 _) = [constructBinary [(0b1111, 4), (0b0000, 4), (o, 8)]]
lineToML (InstLine  RTI  []                                                                          _) = [0b1000000000000000]
lineToML (AssemLine PUT args _) = map getArgValue args
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
                 in (labels, (either (\a -> (AssemLine a (args filt) 0)) (\i -> (InstLine i (args filt) 0)) (tokenToOperation $ head filt)))

tokenToLabel :: Token -> Label
tokenToLabel (LabelToken s) = Label (init s) LabelDefinition
tokenToLabel _ = error ("Cannot create label!")

tokenToArgument :: Token -> Argument
tokenToArgument (RegisterToken s) = RegArg $ Register $ stringToNumeral $ tail s
tokenToArgument (ImmediateToken ('$':':':xs)) = LabelArg $ Label xs LabelRelative
tokenToArgument (ImmediateToken ('$':'*':xs)) = LabelArg $ Label xs LabelAbsolute
tokenToArgument (ImmediateToken s)
    | isDigit $ head $ tail s = ImmArg $ Immediate $ stringToNumeral $ tail s
    | otherwise = LabelArg $ Label (tail s) LabelInferred
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


