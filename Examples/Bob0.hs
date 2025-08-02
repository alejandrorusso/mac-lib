module Examples.Bob0 where

import Data.ByteString.UTF8 (toString)
import Data.List ()

import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

-- Bob's code
common_pass :: String -> IO Bool
common_pass pass = elem pass <$> fetchPassDict

-- Reads passwords!
fetchPassDict :: IO [String]
fetchPassDict = do
    req <- parseRequest "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    str <- toString . getResponseBody <$> httpBS req
    return $ filter notComment $ lines str
  where
    notComment (c : _) = c /= '#'
    notComment _ = False
