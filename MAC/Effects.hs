{-# LANGUAGE Unsafe #-}

-- | It provides functions which map read and write effects into security checks.
module MAC.Effects (
    create,
    writeup,
    readdown,
    fix,
    readAndFix,
    writeAndFix,
    rwRead,
    rwWrite,
)
where

import MAC.Core
import MAC.Lattice

{- |
    It lifts functions which create resources into secure functions which
    create labeled resources
-}
create :: (Less l l') => IO (d a) -> MAC l (Res l' (d a))
create io = ioTCB io >>= return . MkRes

{- |
    It lifts an 'IO'-action which writes into a data type @d a@
    into a secure function which writes into a labeled resource
-}
writeup :: (Less l l') => (d a -> IO ()) -> Res l' (d a) -> MAC l ()
writeup io (MkRes a) = ioTCB $ io a

{- |
    It lifts an 'IO'-action which reads from a data type @d a@
    into a secure function which reads from a labeled resource
-}
readdown :: (Less l' l) => (d a -> IO a) -> Res l' (d a) -> MAC l a
readdown io (MkRes da) = ioTCB $ io da

-- | Proxy function to set the index of the family member 'MAC'
fix :: l -> MAC l ()
fix _ = return ()

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
readAndFix :: (Less l l) => (d a -> IO a) -> Res l (d a) -> MAC l a
readAndFix io r = fix (labelOf r) >> readdown io r

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
writeAndFix :: (Less l' l') => (d a -> IO ()) -> Res l' (d a) -> MAC l' ()
writeAndFix io r = fix (labelOf r) >> writeup io r

{- |
    It lifts an operation which perform a read on data type @d a@, but
    it also performs a write on it as side-effect
-}
rwRead :: (Less l l', Less l' l) => (d a -> IO a) -> Res l' (d a) -> MAC l a
rwRead io r = writeup (\_ -> return ()) r >> readdown io r

{- |
    It lifts an operation which perform a write on data type @d a@, but
    it also performs a read on it as side-effect
-}
rwWrite :: (Less l l', Less l' l) => (d a -> IO ()) -> Res l' (d a) -> MAC l ()
rwWrite io r = readdown (\_ -> return undefined) r >> writeup io r
