module Data.SConfig
( parseConfig
, getValue
, Config
, Key
, Value
) where

import qualified Data.Map as M
import Data.List
import Data.Maybe (fromJust)

-- | Config key, alias for String.
type Key = String

-- | Config value, alias for String.
type Value = String

-- | Parsed configuration. Basically a Map String String
type Config = M.Map Key Value

-- | Parse configuration
parseConfig :: String -> Config
parseConfig str = foldl readConfigLine M.empty --apply readConfigLine to every line
                      $ filter (elem '=') --make sure all lines are actual configuration lines
                      $ concatEscaped
                      $ filter (\x -> (not $ null x) && (take 1 x) /= "#") --remove comments & empty lines
                      $ lines str

--inserts a parsed key-value pair into the Config accumulator
readConfigLine :: Config -> String -> Config
readConfigLine config str = M.insert (filter (/=' ') key) value config
    where (key,_:value) = break (=='=') str

--concatenate two lines if the first ends with a backslash
concatEscaped :: [String] -> [String]
concatEscaped lines = foldr (\x (a:acc) -> if last x == '\\' then ((init x) ++ a):acc else x:a:acc) [""] lines

-- | Lookup values in a parsed configuration (alias of Map.lookup)
getValue :: Key -> M.Map Key Value -> Maybe Value
getValue = M.lookup