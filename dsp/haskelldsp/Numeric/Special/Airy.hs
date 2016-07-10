{-# OPTIONS -fffi -fvia-C #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Special.Airy
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- FFI to GSL for the Airy functions
--
-----------------------------------------------------------------------------

module Numeric.Special.Airy (airy_Ai,              airy_Ai_e,
			     airy_Ai_scaled,       airy_Ai_scaled_e,
			     airy_Ai_deriv,        airy_Ai_deriv_e,
			     airy_Ai_deriv_scaled, airy_Ai_deriv_scaled_e,
		             airy_zero_Ai,         airy_zero_Ai_e,
			     airy_zero_Ai_deriv,   airy_zero_Ai_deriv_e,
		             airy_Bi,              airy_Bi_e,
			     airy_Bi_scaled,       airy_Bi_scaled_e,
			     airy_Bi_deriv,        airy_Bi_deriv_e,
			     airy_Bi_deriv_scaled, airy_Bi_deriv_scaled_e,
			     airy_zero_Bi,         airy_zero_Bi_e,
			     airy_zero_Bi_deriv,   airy_zero_Bi_deriv_e
	    ) where

import StdDIS

import Foreign


-------------------------------------------------------------------------------

airy_Ai :: Double -> Double
airy_Ai x =
  unsafePerformIO(
    prim_airy_Ai x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai" prim_airy_Ai :: Double -> IO (Double)

airy_Ai_e :: Double -> (Double,Double)
airy_Ai_e x =
  unsafePerformIO(
    prim_airy_Ai_e x
    >>= \ gc_result ->
    access_prim_airy_Ai_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Ai_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Ai_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Ai_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_e" prim_airy_Ai_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Ai_scaled :: Double -> Double
airy_Ai_scaled x =
  unsafePerformIO(
    prim_airy_Ai_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_scaled" prim_airy_Ai_scaled :: Double -> IO (Double)

airy_Ai_scaled_e :: Double -> (Double,Double)
airy_Ai_scaled_e x =
  unsafePerformIO(
    prim_airy_Ai_scaled_e x
    >>= \ gc_result ->
    access_prim_airy_Ai_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Ai_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Ai_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Ai_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_scaled_e" prim_airy_Ai_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Ai_deriv :: Double -> Double
airy_Ai_deriv x =
  unsafePerformIO(
    prim_airy_Ai_deriv x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_deriv" prim_airy_Ai_deriv :: Double -> IO (Double)

airy_Ai_deriv_e :: Double -> (Double,Double)
airy_Ai_deriv_e x =
  unsafePerformIO(
    prim_airy_Ai_deriv_e x
    >>= \ gc_result ->
    access_prim_airy_Ai_deriv_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Ai_deriv_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Ai_deriv_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Ai_deriv_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_deriv_e" prim_airy_Ai_deriv_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Ai_deriv_scaled :: Double -> Double
airy_Ai_deriv_scaled x =
  unsafePerformIO(
    prim_airy_Ai_deriv_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_deriv_scaled" prim_airy_Ai_deriv_scaled :: Double -> IO (Double)

airy_Ai_deriv_scaled_e :: Double -> (Double,Double)
airy_Ai_deriv_scaled_e x =
  unsafePerformIO(
    prim_airy_Ai_deriv_scaled_e x
    >>= \ gc_result ->
    access_prim_airy_Ai_deriv_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Ai_deriv_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Ai_deriv_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Ai_deriv_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Ai_deriv_scaled_e" prim_airy_Ai_deriv_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Ai_deriv_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_zero_Ai :: Int -> Double
airy_zero_Ai s =
  unsafePerformIO(
    prim_airy_zero_Ai s
    >>= \  z  ->
    (return (z)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Ai" prim_airy_zero_Ai :: Int -> IO (Double)

airy_zero_Ai_e :: Int -> (Double,Double)
airy_zero_Ai_e s =
  unsafePerformIO(
    prim_airy_zero_Ai_e s
    >>= \ gc_result ->
    access_prim_airy_zero_Ai_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_zero_Ai_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_zero_Ai_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_zero_Ai_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Ai_e" prim_airy_zero_Ai_e :: Int -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_zero_Ai_deriv :: Int -> Double
airy_zero_Ai_deriv s =
  unsafePerformIO(
    prim_airy_zero_Ai_deriv s
    >>= \  z  ->
    (return (z)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Ai_deriv" prim_airy_zero_Ai_deriv :: Int -> IO (Double)

airy_zero_Ai_deriv_e :: Int -> (Double,Double)
airy_zero_Ai_deriv_e s =
  unsafePerformIO(
    prim_airy_zero_Ai_deriv_e s
    >>= \ gc_result ->
    access_prim_airy_zero_Ai_deriv_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_zero_Ai_deriv_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_zero_Ai_deriv_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_zero_Ai_deriv_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Ai_deriv_e" prim_airy_zero_Ai_deriv_e :: Int -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_deriv_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_deriv_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_deriv_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Ai_deriv_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Bi :: Double -> Double
airy_Bi x =
  unsafePerformIO(
    prim_airy_Bi x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi" prim_airy_Bi :: Double -> IO (Double)

airy_Bi_e :: Double -> (Double,Double)
airy_Bi_e x =
  unsafePerformIO(
    prim_airy_Bi_e x
    >>= \ gc_result ->
    access_prim_airy_Bi_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Bi_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Bi_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Bi_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_e" prim_airy_Bi_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Bi_scaled :: Double -> Double
airy_Bi_scaled x =
  unsafePerformIO(
    prim_airy_Bi_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_scaled" prim_airy_Bi_scaled :: Double -> IO (Double)

airy_Bi_scaled_e :: Double -> (Double,Double)
airy_Bi_scaled_e x =
  unsafePerformIO(
    prim_airy_Bi_scaled_e x
    >>= \ gc_result ->
    access_prim_airy_Bi_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Bi_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Bi_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Bi_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_scaled_e" prim_airy_Bi_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Bi_deriv :: Double -> Double
airy_Bi_deriv x =
  unsafePerformIO(
    prim_airy_Bi_deriv x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_deriv" prim_airy_Bi_deriv :: Double -> IO (Double)

airy_Bi_deriv_e :: Double -> (Double,Double)
airy_Bi_deriv_e x =
  unsafePerformIO(
    prim_airy_Bi_deriv_e x
    >>= \ gc_result ->
    access_prim_airy_Bi_deriv_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Bi_deriv_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Bi_deriv_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Bi_deriv_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_deriv_e" prim_airy_Bi_deriv_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_Bi_deriv_scaled :: Double -> Double
airy_Bi_deriv_scaled x =
  unsafePerformIO(
    prim_airy_Bi_deriv_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_deriv_scaled" prim_airy_Bi_deriv_scaled :: Double -> IO (Double)

airy_Bi_deriv_scaled_e :: Double -> (Double,Double)
airy_Bi_deriv_scaled_e x =
  unsafePerformIO(
    prim_airy_Bi_deriv_scaled_e x
    >>= \ gc_result ->
    access_prim_airy_Bi_deriv_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_Bi_deriv_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_Bi_deriv_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_Bi_deriv_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_Bi_deriv_scaled_e" prim_airy_Bi_deriv_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_Bi_deriv_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_zero_Bi :: Int -> Double
airy_zero_Bi s =
  unsafePerformIO(
    prim_airy_zero_Bi s
    >>= \  z  ->
    (return (z)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Bi" prim_airy_zero_Bi :: Int -> IO (Double)

airy_zero_Bi_e :: Int -> (Double,Double)
airy_zero_Bi_e s =
  unsafePerformIO(
    prim_airy_zero_Bi_e s
    >>= \ gc_result ->
    access_prim_airy_zero_Bi_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_zero_Bi_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_zero_Bi_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_zero_Bi_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Bi_e" prim_airy_zero_Bi_e :: Int -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

airy_zero_Bi_deriv :: Int -> Double
airy_zero_Bi_deriv s =
  unsafePerformIO(
    prim_airy_zero_Bi_deriv s
    >>= \  z  ->
    (return (z)))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Bi_deriv" prim_airy_zero_Bi_deriv :: Int -> IO (Double)

airy_zero_Bi_deriv_e :: Int -> (Double,Double)
airy_zero_Bi_deriv_e s =
  unsafePerformIO(
    prim_airy_zero_Bi_deriv_e s
    >>= \ gc_result ->
    access_prim_airy_zero_Bi_deriv_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_airy_zero_Bi_deriv_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_airy_zero_Bi_deriv_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_airy_zero_Bi_deriv_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Airy_stub_ffi.h prim_airy_zero_Bi_deriv_e" prim_airy_zero_Bi_deriv_e :: Int -> IO (Addr)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_deriv_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_deriv_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_deriv_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Airy_stub_ffi.h" access_prim_airy_zero_Bi_deriv_e_gc_failstring :: Addr -> IO (Addr)
