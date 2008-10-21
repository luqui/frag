----------------------------------------
-- |
-- Module    : Data.QuotientRef
-- Copyright : (c) Luke Palmer 2008
-- License   : BSD3
--
-- Maintainer : Luke Palmer <lrpalmer@gmail.com>
-- Stability : experimental
--
-- A Quotient Reference is a reference cell that needs two values to
-- dereference.  In a way it is a two-dimensional table indexed by references.
-- The trick is that if a cell is indexed by @a@ and @b@, then if /either/
-- @a@ or @b@ gets cleaned up by the garbage collector, then so does the
-- cell, because it would not be able to be accessed anymore.
--
-- There are two different types of indices, "LeftRef" and "RightRef".  You
-- need one of each, of the same type, to access a cell.
--
-- The name comes from the idea that the product of two indices is a reference,
-- so each index is a quotient.
--
-- Example usage:
--
-- > do
-- >   l_1 <- newLeft
-- >   l_2 <- newLeft
-- >   r_1 <- newRight
-- >   r_2 <- newRight
-- >   write l_1 r_1 "Foo"
-- >   write l_2 r_1 "Bar"
-- >   print =<< read l_1 r_1  -- Just "Foo"
-- >   print =<< read l_1 r_2  -- Nothing
-- >   print =<< read l_2 r_1  -- Just "Bar"
-- >   print =<< read l_2 r_2  -- Nothing
-- 
module Data.QuotientRef 
    ( LeftRef, RightRef
    , newLeft, newRight
    , read, write
    )
where

import Prelude hiding (read)
import qualified Data.HashTable as Hash
import Data.Unique
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Mem.Weak

-- | The left half of a reference cell.  Combine this with a "RightRef" to
-- access a cell.
newtype LeftRef a
    = LeftRef (Hash.HashTable Unique a)

-- RightRef is data rather than newtype, so we have something real to make 
-- a weak reference from.

-- | The right half of a reference cell.  Combine this with a "LeftRef" to
-- access a cell.
data RightRef a
    = RightRef Unique


newLeft :: IO (LeftRef a)
newLeft = LeftRef <$> Hash.new (==) (fromIntegral . hashUnique)

newRight :: IO (RightRef a)
newRight = RightRef <$> newUnique

-- | Combine the two halves of a reference and return the result if
-- it exists.
read :: LeftRef a -> RightRef a -> IO (Maybe a)
read (LeftRef hash) (RightRef ident) = do
    Hash.lookup hash ident

-- | Combine the two halves of a reference and write a value to the
-- product.
write :: LeftRef a -> RightRef a -> a -> IO ()
write (LeftRef hash) right@(RightRef ident) val = block $ do
    existed <- Hash.update hash ident val
    unless existed $ do
        hashW <- mkWeakPtr hash Nothing
        addFinalizer right $ do
            h <- deRefWeak hashW
            case h of
                Nothing -> return ()
                Just h' -> Hash.delete h' ident
