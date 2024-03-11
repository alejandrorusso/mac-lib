{-# LANGUAGE Trustworthy #-}

module Examples.MACWget where

import MAC.Lattice
import MAC.Core (MAC, ioTCB)

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import Data.ByteString.UTF8 (toString)

{-
  For simplicity, when wgetMAC gets called with http://bob.evil as a domain, it
  will write the request to a file
-}
wgetMAC :: String -> MAC L String
wgetMAC s
   | take (length domain) s == domain = do
       ioTCB $ appendFile "leaks.txt" $ s ++ "\n"
       return "launch"
   | otherwise =
       ioTCB $ do
         toString . getResponseBody <$> do
           httpBS =<< parseRequest s
  where
    domain = "http://bob.evil"
