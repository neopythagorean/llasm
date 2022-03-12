{-# LANGUAGE BinaryLiterals #-}

module Assembler
    ( assemble
    , writeMLOut
    ) where

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

data AssemblerDirection = ORIGIN | PUT | DEFINE deriving ( Show, Eq )

data Line = InstLine { inst :: Instruction, args :: [Argument], lineNum :: Int } | AssemLine { dir :: AssemblerDirection, args :: [Argument], lineNum :: Int } deriving ( Show, Eq )

assemble :: String -> [Word16]
assemble program = 
    let srclines = lines program
        tokenlines = filter (not . null) $ map (tokenizeLine . removeComments) srclines
        corrected = moveLabelsDown tokenlines
        linelines = map tokensToLine corrected
        (origin, rest) = getOrigin linelines
        (symbols, asmlines) = createSymbolTable origin rest
        resolved = resolveAllLabels symbols asmlines
        ml = concat $ ([origin] : (map lineToML resolved))
        words = map (\i -> fromIntegral (i :: Int) :: Word16) ml
    in words

writeMLOut :: String -> [Word16] -> IO ()
writeMLOut outname ml = do
    fileOut <- openFile outname WriteMode
    BL.hPutStr fileOut $ runPut (mapM_ putWord16be ml)
    hClose fileOut

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
getLabelResolution _ (Label ('[':s) _) = stringToNumeral $ init s
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
lineToML (InstLine  BR   (ImmArg (Immediate c) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0000, 4), (c, 3),      (pred o, 9)]]
lineToML (InstLine  JMP  (RegArg (Register br) : [])                                                 _) = [constructBinary [(0b1100, 4), (0b000, 3),  (br, 3),  (0b000000, 6)]]
lineToML (InstLine  JSR  (ImmArg (Immediate o) : [])                                                 _) = [constructBinary [(0b0100, 4), (0b1, 1),    (pred o, 11)]]
lineToML (InstLine  JSRR (RegArg (Register br) : [])                                                 _) = [constructBinary [(0b0100, 4), (0b000, 3),  (br, 3),  (0b000000, 6)]]
lineToML (InstLine  RET  []                                                                          _) = [0b0100000111000000] -- Ret is just JSR %7
lineToML (InstLine  LD   (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0010, 4), (dr, 3),     (pred o, 9)]]
lineToML (InstLine  LDI  (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1010, 4), (dr, 3),     (pred o, 9)]]
lineToML (InstLine  LDR  (RegArg (Register sr) : RegArg (Register br)  : ImmArg (Immediate o)  : []) _) = [constructBinary [(0b0110, 4), (sr, 3),     (br, 3),  (o, 6)]]
lineToML (InstLine  LEA  (RegArg (Register dr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1110, 4), (dr, 3),     (pred o, 9)]]
lineToML (InstLine  ST   (RegArg (Register sr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b0011, 4), (sr, 3),     (pred o, 9)]]
lineToML (InstLine  STI  (RegArg (Register sr) : ImmArg (Immediate o)  : [])                         _) = [constructBinary [(0b1011, 4), (sr, 3),     (pred o, 9)]]
lineToML (InstLine  STR  (RegArg (Register sr) : RegArg (Register br)  : ImmArg (Immediate o)  : []) _) = [constructBinary [(0b0111, 4), (sr, 3),     (br, 3),  (o, 6)]]
lineToML (InstLine  TRAP (ImmArg (Immediate o) : [])                                                 _) = [constructBinary [(0b1111, 4), (0b0000, 4), (o, 8)]]
lineToML (InstLine  RTI  []                                                                          _) = [0b1000000000000000]
lineToML (AssemLine PUT args _) = map getArgValue args
lineToML _ = error ("Unable to convert to ML!")

-- Get only the last s bits of n
lastBits :: Int -> Int -> Int
lastBits n s = n .&. (pred ((shiftL 1) s))

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

isNumeric :: Char -> Bool
isNumeric = getAny . foldMap (Any .) [isDigit, ((==) '-')]


tokenToArgument :: Token -> Argument
tokenToArgument (RegisterToken s) = RegArg $ Register $ stringToNumeral $ tail s
tokenToArgument (ImmediateToken ('$':':':xs)) = LabelArg $ Label xs LabelRelative
tokenToArgument (ImmediateToken ('$':'*':xs)) = LabelArg $ Label xs LabelAbsolute
tokenToArgument (ImmediateToken s)
    | isNumeric $ head $ tail s = ImmArg $ Immediate $ stringToNumeral $ tail s
    | otherwise = LabelArg $ Label (tail s) LabelInferred
tokenToArgument _ = error ("Cannot convert token to argument!")

tokenToOperation :: Token -> Either AssemblerDirection Instruction
tokenToOperation (OpcodeToken    op) = Right $ tokenToInstruction (OpcodeToken op)
tokenToOperation (AssemblerToken as) = Left $ tokenToAssemblerDirection (AssemblerToken as)
tokenToOperation _ = error ("Token is not an operation!")

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
                        negative ('-':t) = (-1, t)
                        negative t       = (1, t)
                        (m, n) = negative s
                    in m * (fromJust $ convnum (numType n) $ removeType n) -- TODO Add error checking to this (handle fromJust)

opcodes :: [String] 
opcodes = map fst instructions

tokenToInstruction :: Token -> Instruction
tokenToInstruction (OpcodeToken s) = let r = lookup s instructions
                                     in if isNothing r then error ("Unknown opcode")
                                        else fromJust r
tokenToInstruction _ = error ("Cannot convert to Instruction")

instructions :: [(String, Instruction)]
instructions = [ ( "add",  ADD )
               , ( "and",  AND )
               , ( "br",   BR )
               , ( "jmp",  JMP )
               , ( "jsr",  JSR )
               , ( "jsrr", JSRR )
               , ( "ld",   LD )
               , ( "ldi",  LDI )
               , ( "ldr",  LDR )
               , ( "lea",  LEA )
               , ( "not",  NOT )
               , ( "ret",  RET )
               , ( "rti",  RTI )
               , ( "st",   ST )
               , ( "sti",  STI )
               , ( "str",  STR )
               , ( "trap", TRAP ) ]

tokenToAssemblerDirection :: Token -> AssemblerDirection
tokenToAssemblerDirection (AssemblerToken s) = let r = lookup s assemblerInstructions
                                               in if isNothing r then error ("Unknown assembler direction")
                                                  else fromJust r
tokenToAssemblerDirection _ = error ("Cannot convert to Assembler Direction")

assemblerInstructions :: [(String, AssemblerDirection)]
assemblerInstructions = [ ( ".origin", ORIGIN )
                        , ( ".put",    PUT )
                        , ( ".define", DEFINE ) ]


