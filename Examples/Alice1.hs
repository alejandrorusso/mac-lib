{-# LANGUAGE Trustworthy #-}

module Examples.Alice1 where

import MAC.Core
import MAC.Labeled
import MAC.Lattice

import qualified Examples.Bob1 as Bob

{-
  In this example, Bob code is malicious. It utilizes unsafePerformIO to fool
  the security restrictions imposed by the security types. This problem can be
  fixed by using Safe Haskell --- just add {-# LANGUAGE Safe #-} to the
  file Bob1.hs
-}

password :: IO String
password = do
    putStr "Please, select your password:"
    pass <- getLine
    lbool <-
        runMAC $
            (label pass :: MAC L (Labeled H String))
                >>= Bob.commonPass
    let MkId b = unRes lbool
    if b
        then putStrLn "Your password is too common!" >> password
        else return pass
