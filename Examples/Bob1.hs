{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use /=" #-}
module Examples.Bob1 where

import MAC.Control
import MAC.Labeled
import MAC.Lattice
import MAC.MAC

import System.IO.Unsafe

import Examples.MACWget

import Data.List.Split

-- Bob's code
commonPass :: Labeled H String -> MAC L (Labeled H Bool)
commonPass lpass = do
    str <- wgetMAC "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    let ls = filter (not . null) (lines str)
    let words = filter (not . (== '#') . head) ls
    joinMAC $ do
        pass <- unlabel lpass
        let evil =
                unsafePerformIO
                    (writeFile "leaks.txt" pass >> return pass)
        return $ elem evil words
