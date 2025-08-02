{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Bob4 where

import Data.List
import Data.Maybe

import MAC.Control
import MAC.Labeled
import MAC.Lattice
import MAC.MAC
import MAC.Ref

import Control.Monad
import Data.List.Split

-- Bob's code
commonPass ::
    (String -> MAC L String) ->
    Labeled H String ->
    MAC L (Labeled H Bool)
commonPass wget lpass = do
    str <- wget "http://www.openwall.com/passwords/wordlists/password-2011.lst"
    let lines = filter (not . null) (linesBy (== '\n') str)
    let words = filter (not . (== '#') . head) lines
    joinMAC $ do
        pass <- unlabel lpass
        return $ isJust $ find (== pass) words

-- Memoization
memMAC :: (String -> MAC L String) -> MAC L (String -> MAC L String)
memMAC f = newMACRef (100, []) >>= (return . cacheMAC f)

cacheMAC ::
    (String -> MAC L String) ->
    MACRef L (Int, [(String, String)]) ->
    String ->
    MAC L String
cacheMAC f ref str = do
    (n, _) <- readMACRef ref
    when (n == 0) $ writeMACRef ref (100, [])
    (n, map) <- readMACRef ref
    case find (\(i, o) -> i == str) map of
        Nothing -> do
            result <- f str
            writeMACRef ref (n - 1, (str, result) : map)
            return result
        Just (_, o) -> do
            writeMACRef ref (n - 1, map)
            return o
