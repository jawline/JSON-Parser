module Main where
import Data.String (String)
import Data.Map (Map, empty, insert)

{-- Helper Methods --}

skip :: String -> String
skip [] = []
skip (' ':xs) = skip xs
skip ('\n':xs) = skip xs
skip ('\r':xs) = skip xs
skip ('\t':xs) = skip xs
skip xs = xs

digit :: Char -> Bool
digit '0' = True
digit x = x >= '1' && x <= '9'

escaped :: Char -> Char
escaped '"' = '"'
escaped '\\' = '\\'
escaped '/' = '/'
escaped 'b' = '\b'
escaped 'f' = '\f'
escaped 'r' = '\r'
escaped 'n' = '\n'
escaped 't' = '\t'

{-- Parser --}

data JSValue = JSObject (Map String JSValue)
  | JSArray [JSValue]
  | JSString [Char]
  | JSNumber Double
  | JSBool Bool
  | JSNull
  | JSUndefined
  deriving Show

extractInteger :: String -> (String, String)
extractInteger [] = ([], [])
extractInteger (x:xs)
  | digit x = (x:follows, rest)
  | otherwise = ([], x:xs)
  where
    (follows, rest) = extractInteger xs

parseDouble :: String -> (Double, String)
parseDouble ('-':xs) = (-result, rest)
  where (result, rest) = parseDouble xs
parseDouble xs
  | afterFraction /= [] && charAfterFraction == '.' = (read (fraction ++ "." ++ exponent) :: Double, afterExponent)
  | otherwise = (read fraction :: Double, afterFraction)
  where
    (fraction, afterFraction) = extractInteger xs
    (charAfterFraction:remaining) = afterFraction
    (exponent, afterExponent) = extractInteger remaining

parseNumber :: String -> (JSValue, String)
parseNumber xs = (JSNumber result, rest)
  where (result, rest) = parseDouble xs

parseStringInner :: String -> (String, String)
parseStringInner ('"':xs) = ("", xs)
parseStringInner ('\\':r:xs) = (escaped r:str, follows)
    where (str, follows) = parseStringInner xs
parseStringInner (x:xs) = (x:str, follows)
    where (str, follows) = parseStringInner xs

parseString :: String -> (JSValue, String)
parseString xs = (JSString str, rest)
  where (str, rest) = parseStringInner xs

parseArrayInner :: String -> ([JSValue], String)
parseArrayInner (']':xs) = ([], xs)
parseArrayInner xs
  | followingChar == ',' = (parsedItem:recursed, recursedRest)
  | followingChar == ']' = ([parsedItem], rest)
  where
    (parsedItem, afterParsed) = parseJson xs
    (followingChar:rest) = skip afterParsed
    (recursed, recursedRest) = parseArrayInner (skip rest)

parseArray :: String -> (JSValue, String)
parseArray xs = (JSArray arr, rest)
  where (arr, rest) = parseArrayInner (skip xs)

parseObjectInner :: String -> ((Map String JSValue), String)
parseObjectInner ('}':xs) = (empty, xs)
parseObjectInner xs
  | followingChar == ',' = (insert parsedName parsedValue recursed, recursedRest)
  | followingChar == '}' = (insert parsedName parsedValue empty, rest)
  where
    ('"':nameStart) = xs
    (parsedName, afterName) = parseStringInner nameStart
    (':':followingName) = skip afterName
    (parsedValue, afterValue) = parseJson followingName
    (followingChar:rest) = skip afterValue
    (recursed, recursedRest) = parseObjectInner (skip rest)

parseObject :: String -> (JSValue, String)
parseObject xs = (JSObject map, rest)
  where (map, rest) = parseObjectInner (skip xs)

parseJsonInner :: String -> (JSValue, String)
parseJsonInner ('n':'u':'l':'l':xs) = (JSNull, xs)
parseJsonInner ('t':'r':'u':'e':xs) = (JSBool True, xs)
parseJsonInner ('f':'a':'l':'s':'e':xs) = (JSBool False, xs)
parseJsonInner (x:xs)
  | x == '-' || digit x = parseNumber(x:xs)
  | x == '"' = parseString xs
  | x == '[' = parseArray xs
  | x == '{' = parseObject xs

parseJson :: String -> (JSValue, String)
parseJson xs = parseJsonInner (skip xs)

{-- Example Usage --}

main :: IO ()
main = do
  _ <- putStrLn (show (parseJson "null"))
  _ <- putStrLn (show (parseJson "false"))
  _ <- putStrLn (show (parseJson "true"))
  _ <- putStrLn (show (parseJson "-5432.69        "))
  _ <- putStrLn (show (parseJson "523"))
  _ <- putStrLn (show (parseJson "-96"))
  _ <- putStrLn (show (parseJson "        \"Hello world \r\n\bQQQ 523\""))
  _ <- putStrLn (show (parseJson "[]"))
  _ <- putStrLn (show (parseJson "[true]"))
  _ <- putStrLn (show (parseJson "[\"Hello\"   ]"))
  _ <- putStrLn (show (parseJson "[1,2,3,4]"))
  _ <- putStrLn (show (parseJson "[1,2,[3,4]]"))
  _ <- putStrLn (show (parseJson "{\"items\":[1,   2,3,4]}"))
  _ <- putStrLn (show (parseJson "{\"items\":[    1,2   ,3,     4],\"bob\":false}"))
  _ <- putStrLn (show (parseJson "{\"items\":[1,2,   \"This is a mixed array\",4],\"bob\":false,\"cat\":\"Hello I am a cat\"}"))
  putStrLn (show (parseJson "43.7"))
