import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readHex, readOct)
-- import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char

instance Show LispVal where
    show (Atom a) = "Atom " ++ a
    show (List l) = "List " ++ show l
    show (DottedList l v) = "DottedList " ++ show l ++ " " ++ show v
    show (Number i) = "Number " ++ show i
    show (String s) = "String " ++ s
    show (Bool b) = "Bool " ++ show b
    show (Character c) = "Character " ++ show c

escapedChars :: Parser Char
escapedChars = do 
    _ <- char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of 
        '\\' -> x
        '"'  -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseBool :: Parser LispVal
parseBool = do 
    _ <- char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do 
    _ <- char '"'
    x <- many (escapedChars <|> noneOf "\"\\")
    _ <- char '"'
    return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- try (string "#\\")
    value <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case value of 
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (head value)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest :: String
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do 
    _ <- try(string "#d")
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    _ <- try (string "#x")
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do 
    _ <- try (string "#o")
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do 
    _ <- try (string "#b")
    x <- many1 $ oneOf "10"
    return $ Number $ bin2dig x

oct2dig :: (Num a, Eq a) => String -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: (Num a, Eq a) => String -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: Num a => String -> a
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> String -> a
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
    bin2dig' old xs

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr $ head args
