{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}
module Regex (
    PCREOption(..)
  , PCREExecOption(..)
  , PCREInfo(..)
  , Regex
  , combineOptions
  , compile
  , match
  ) where

import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Storable
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (alloca)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified System.IO.Unsafe as Unsafe
import qualified Data.ByteString.Unsafe as UBS
import           Data.ByteString.Internal (toForeignPtr)

#include <pcre.h>

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)

-- Manually is boring:
--
--caseless :: PCREOption
--caseless = PCREOption #const PCRE_CASELESS

--dollar_endonly :: PCREOption
--dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

--dotall :: PCREOption
--dotall = PCREOption #const PCRE_DOTALL

-- Automated by hcs2hs:
--
--     type       , constructor
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

--pcre *pcre_compile(const char *regex,
                   --int options,
                   --const char **errptr,
                   --int *erroffset,
                   --const unsigned char *tableptr);

--PCRE compile time options we've already chosen to represent as the abstract PCREOption newtype, whose runtime representation is a CInt. As the representations are guaranteed to be identical, we can pass the newtype safely
--
-- We don't need to know the type of the return type pcre because
-- we aren't going to use it (apart from passing it back to pcre_exec.
-- We only need to store a pointer to the address.
--
-- Usually, you can represent it with an opaque type that has no value
-- on runtime.
--
-- Options:
--
-- type PCRE = ()
-- newtype PCRE = PCRE (Ptr PCRE) -- also works
data PCRE

-- "safe" calls are less efficient but guarantees that can be safely called.
-- "unsafe" is faster but you must guarantee that the C code does not callback haskell.
foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString        -- regex
                 -> PCREOption     -- options
                 -> Ptr CString    -- error message or NULL
                 -> Ptr CInt       -- error offset
                 -> Ptr Word8      -- ???
                 -> IO (Ptr PCRE)

-- Memory management: let the GC do the work the return pointer is deallocated by the GC (at some point).
--
-- We need to use the following:
--
--   newForeignPtr :: FinalizerPtr a  -- Finalizer
--                 -> Ptr a
--                 -> IO (ForeignPtr a)

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString -- regex result
  deriving (Eq, Ord, Show)

-- The wrapper around compile
-- compile :: ByteString -> [PCREOption] -> Either String Regex

--C.useAsCString :: ByteString (CString -> IO a) -> IO a

-- import qualified Foreign.ForeignPtr as P
-- P.alloca :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
--
-- P.alloca $ \stringptr -> do
--   ... call some Ptr CString function
--   peek stringptr

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = Unsafe.unsafePerformIO $
  C.useAsCString str $ \regex -> do
    alloca $ \errptr -> do
      alloca $ \erroffset -> do
          pcre_ptr <- c_pcre_compile regex (combineOptions flags) errptr erroffset nullPtr
          if pcre_ptr == nullPtr
              then do
                  err <- peekCString =<< peek errptr
                  return (Left err)
              else do
                  reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                  return (Right (Regex reg str))

-- int pcre_exec(const pcre *code,
--               const pcre_extra *extra,
--               const char *subject,
--               int length,
--               int startoffset,
--               int options,
--               int *ovector,
--               int ovecsize);

foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE
              -> Ptr PCREExtra
              -> CString
              -> CInt            -- ^ length of the cstring
              -> CInt            -- ^ start offset of the cstring
              -> PCREExecOption  -- ^ option bits
              -> Ptr CInt        -- ^ ovector (vector of ints for result offsets
              -> CInt            -- ^ ovecsize (number of elements in the vector)
              -> IO CInt

data PCREExtra

newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
    deriving (Eq,Show)

#{enum PCREExecOption, PCREExecOption
  , anchored       = PCRE_ANCHORED
  }

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0

-- We are intereste in the int *ovector that are the matching substrings found by the regex matcher.
--
-- The size is determine by analysing the input regular expression to determine the number of captured patterns it contains.
--
-- We need to use the following provided function.

-- PCREInfo ~ CInt
--     PCRE_INFO_CAPTURECOUNT    Number of capturing subpatterns

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt }
    deriving (Eq,Show)

#{enum PCREInfo, PCREInfo
  , info_capturecount = PCRE_INFO_CAPTURECOUNT
  }

-- int pcre_fullinfo(const pcre *code, const pcre_extra *extra, int what, void *where);
foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo            -- ^ Information we are interested in.
                    -> Ptr a               -- ^ pointer to a location to store the information about the regex
                    -> IO CInt

-- | Returns the number of capturing subpatterns
-- With this information you can access the ovector from pcre_exec
capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
         c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
         return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

-- *withForeignPtr* this holds on to the Haskell data associated with the PCRE value while the call
-- is being made, preventing it from being collected for at least the time it is used by this call

match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = Unsafe.unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    let ovec_size = (n_capt + 1) * 3 -- Number of elements in the vector (a multiple of 3)
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)

    -- allocaBytes ~ alloca but with explicit size.
    allocaBytes ovec_bytes $ \ovec -> do
        -- toForeignPTr: bytestring to pointer (very efficient)
        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            -- r: number of matches
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0 -- Start offset
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> ByteString -> ByteString
    substring x y _ | x == y = C.empty
    substring a b s = end
        where
            start = UBS.unsafeDrop (fromIntegral a) s
            end   = UBS.unsafeTake (fromIntegral (b-a)) start
