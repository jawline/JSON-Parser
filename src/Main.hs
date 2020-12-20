module Main where
import Data.String (String)
import Data.Map (Map, empty, insert)
import Data.List (isPrefixOf)

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
  deriving Show

-- If 'null' return JSNull and drop the keyword from the string.
parseNull :: String -> Maybe (JSValue, String)
parseNull xs
  | isPrefixOf "null" xs = Just (JSNull, drop 4 xs)
  | otherwise = Nothing

-- If 'true' or 'false' return a JSBool and drop the keyword from the string. Otherwise return Nothing.
parseBool :: String -> Maybe (JSValue, String)
parseBool xs
  | isPrefixOf "true" xs = Just (JSBool True, drop 4 xs)
  | isPrefixOf "false" xs = Just (JSBool False, drop 5 xs)
  | otherwise = Nothing

extractInteger :: String -> (String, String)
extractInteger [] = ([], [])
extractInteger (x:xs)
  | digit x, (follows, xs) <- extractInteger xs = (x:follows, xs)
  | otherwise = ([], x:xs)

-- Extract a double (number in the format 523123 or 52131.4231 with an optional - in front)
parseDouble :: String -> (Double, String)
parseDouble xs
  | ('-':xs) <- xs, (h, r) <- parseDouble xs = (-h, r) -- If our number starts with a - then negate it
  | afterFraction /= [] && charAfterFraction == '.' = (read (fraction ++ "." ++ exponent) :: Double, afterExponent)
  | otherwise = (read fraction :: Double, afterFraction)
  where
    (fraction, afterFraction) = extractInteger xs
    (charAfterFraction:remaining) = afterFraction
    (exponent, afterExponent) = extractInteger remaining

-- If what follows is a digit ot a - then start parsing a number, otherwise Nothing
parseNumber :: String -> Maybe (JSValue, String)
parseNumber xs
  | (head xs) == '-' || digit (head xs) = Just (JSNumber result, rest)
  | otherwise = Nothing
  where
    (result, rest) = parseDouble xs

-- Parse the string after the opening " until we find a closing " that isn't escaped
parseStringInner :: String -> (String, String)
parseStringInner xs
  | ('"':xs) <- xs = ("", xs)
  | ('\\':r:xs) <- xs, (str, follows) <- parseStringInner xs = (escaped r:str, follows)
  | (x:xs) <- xs, (str, follows) <- parseStringInner xs = (x:str, follows)

-- If the next thing starts with a " then we parse it as a string, otherwise Nothing
parseString :: String -> Maybe (JSValue, String)
parseString xs
  | ('"':xs) <- xs, (str, rest) <- parseStringInner xs = Just (JSString str, rest)
  | otherwise = Nothing

parseArrayInner :: String -> ([JSValue], String)
parseArrayInner xs
  | (',':rest) <- rest, (recursed, recursedRest) <- parseArrayInner (skip rest) = (parsedItem:recursed, recursedRest)
  | (']':rest) <- rest = ([parsedItem], rest)
  where
    (parsedItem, afterParsed) = parseJson xs
    rest = skip afterParsed

-- If the next thing starts with a '[' then we parse it as a array, otherwise Nothing
parseArray :: String -> Maybe (JSValue, String)
parseArray xs
  | ('[':xs) <- xs, (']':xs) <- skip xs = Just (JSArray [], xs)
  | ('[':xs) <- xs, (arr, rest) <- parseArrayInner (skip xs) = Just (JSArray arr, rest)
  | otherwise = Nothing 

parseObjectInner :: String -> ((Map String JSValue), String)
parseObjectInner xs
  | (',':rest) <- rest, (recursed, recursedRest) <- parseObjectInner rest = (insert parsedName parsedValue recursed, recursedRest)
  | ('}':rest) <- rest = (insert parsedName parsedValue empty, rest)
  where
    ('"':nameStart) = xs
    (parsedName, afterName) = parseStringInner nameStart
    (':':followingName) = skip afterName
    (parsedValue, afterValue) = parseJson followingName
    rest = skip afterValue

-- If the next thing starts with a '{' then we parse it as an object, otherwise Nothing
parseObject :: String -> Maybe (JSValue, String)
parseObject xs
  | ('{':xs) <- xs, ('}':xs) <- skip xs = Just (JSObject empty, xs)
  | ('{':xs) <- xs, (map, rest) <- parseObjectInner (skip xs) = Just (JSObject map, rest)
  | otherwise = Nothing

parseJsonInner :: String -> (JSValue, String)
parseJsonInner xs
  | Just jsNull <- parseNull xs = jsNull
  | Just jsBool <- parseBool xs = jsBool
  | Just jsNumber <- parseNumber xs = jsNumber
  | Just jsStr <- parseString xs = jsStr
  | Just jsArr <- parseArray xs = jsArr
  | Just jsObj <- parseObject xs = jsObj

parseJson :: String -> (JSValue, String)
parseJson xs = parseJsonInner (skip xs)

{-- Example Usage --}

main :: IO ()
main = do
  _ <- putStrLn (show (parseJson "null"))
  _ <- putStrLn (show (parseJson "false"))
  _ <- putStrLn (show (parseJson "true"))
  _ <- putStrLn (show (parseJson "523"))
  _ <- putStrLn (show (parseJson "-5432.69        "))
  _ <- putStrLn (show (parseJson "-96"))
  _ <- putStrLn (show (parseJson "\"Hello\""))
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
