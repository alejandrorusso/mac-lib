module Examples.Bob0 where

import Data.List ()
import Data.ByteString.UTF8 (toString)

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)

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
    notComment (c:_) = c /= '#'
    notComment _ = False
