{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}

module Collectd.Internal where

import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String

mkInt :: CInt -> Int
mkInt = fromIntegral
{-# INLINE mkInt #-}

mkDbl :: CDouble -> Double
mkDbl d = realToFrac d
{-# INLINE mkDbl #-}

infixl 4 `apInt`, `apDbl`, `fpStr`, `apArr`, `apStr`, `apULong`, `apIntegral`, `apTs`

fpStr :: (String -> b)    -> CString    -> IO b
fpStr a b = a <$> (peekCString b)

apStr :: IO (String -> b) -> CString -> IO b
apStr a b = a <*> (peekCString b)

-- apStr a b = a <*> (peekCString <$> b)

peekCArray :: (Storable a) => CInt -> IO (Ptr a) -> IO [a]
peekCArray i ir = ir >>= peekArray (mkInt i)

apArr :: Storable a => IO ([a] -> b) -> (IO CInt, IO (Ptr a)) -> IO b
apArr f (i, b) = do
  i' <- i
  r  <- peekCArray i' b
  f' <- f
  return $ f' r

apInt :: (Applicative f) => f (Int -> b) -> f CInt -> f b
apInt a b = a <*> (fromIntegral <$> b)

apULong :: (Applicative f)                       => f (Int -> b) -> f CULong -> f b
apULong a b = a <*> (fromIntegral <$> b)

apIntegral :: (Applicative f, Integral i, Num n) => f (n -> b) -> f i -> f b
apIntegral a b = a <*> (fromIntegral <$> b)

apTs :: (Applicative f) => f (Word64 -> b) -> f CULong -> f b
apTs a b = a <*> (op <$> b)
  where op i = ceiling $ (fromIntegral i) / 1073741.824 -- 2^30 = 1073741824


apDbl :: (Applicative f) => f (Double -> b) -> f CDouble -> f b
apDbl a b = a <*> (realToFrac <$> b)
