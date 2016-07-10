{-# OPTIONS -fffi -fvia-C #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Special.Bessel
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- FFI to GSL for the Bessel functions
--
-----------------------------------------------------------------------------

module Numeric.Special.Bessel (bessel_J0, bessel_J0_e,
			       bessel_J1, bessel_J1_e,
			       bessel_Jn, bessel_Jn_e,

			       bessel_Y0, bessel_Y0_e,
			       bessel_Y1, bessel_Y1_e,
			       bessel_Yn, bessel_Yn_e,

			       bessel_I0, bessel_I0_e,
			       bessel_I1, bessel_I1_e,
			       bessel_In, bessel_In_e,

			       bessel_I0_scaled, bessel_I0_scaled_e,
			       bessel_I1_scaled, bessel_I1_scaled_e,
			       bessel_In_scaled, bessel_In_scaled_e,

			       bessel_K0, bessel_K0_e,
			       bessel_K1, bessel_K1_e,
			       bessel_Kn, bessel_Kn_e,

			       bessel_K0_scaled, bessel_K0_scaled_e,
			       bessel_K1_scaled, bessel_K1_scaled_e,
			       bessel_Kn_scaled, bessel_Kn_scaled_e,

			       bessel_j0, bessel_j0_e,
			       bessel_j1, bessel_j1_e,
			       bessel_jl, bessel_jl_e,

			       bessel_y0, bessel_y0_e,
			       bessel_y1, bessel_y1_e,
			       bessel_yl, bessel_yl_e,

			       bessel_i0_scaled, bessel_i0_scaled_e,
			       bessel_i1_scaled, bessel_i1_scaled_e,
			       bessel_il_scaled, bessel_il_scaled_e,

			       bessel_k0_scaled, bessel_k0_scaled_e,
			       bessel_k1_scaled, bessel_k1_scaled_e,
			       bessel_kl_scaled, bessel_kl_scaled_e,

			       bessel_Jnu, bessel_Jnu_e,
			       bessel_Ynu, bessel_Ynu_e,
			       bessel_Inu, bessel_Inu_e,
			       bessel_Inu_scaled, bessel_Inu_scaled_e,
			       bessel_Knu, bessel_Knu_e,
			       bessel_Knu_scaled, bessel_Knu_scaled_e,
			       bessel_lnKnu, bessel_lnKnu_e,

                               bessel_zero_J0,  bessel_zero_J0_e,
                               bessel_zero_J1,  bessel_zero_J1_e,
                               bessel_zero_Jnu, bessel_zero_Jnu_e
			      ) where

import StdDIS

import Foreign


-------------------------------------------------------------------------------

bessel_J0 :: Double -> Double
bessel_J0 x =
  unsafePerformIO(
    prim_bessel_J0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_J0" prim_bessel_J0 :: Double -> IO (Double)

bessel_J0_e :: Double -> (Double,Double)
bessel_J0_e x =
  unsafePerformIO(
    prim_bessel_J0_e x
    >>= \ gc_result ->
    access_prim_bessel_J0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_J0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_J0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_J0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_J0_e" prim_bessel_J0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_J1 :: Double -> Double
bessel_J1 x =
  unsafePerformIO(
    prim_bessel_J1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_J1" prim_bessel_J1 :: Double -> IO (Double)

bessel_J1_e :: Double -> (Double,Double)
bessel_J1_e x =
  unsafePerformIO(
    prim_bessel_J1_e x
    >>= \ gc_result ->
    access_prim_bessel_J1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_J1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_J1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_J1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_J1_e" prim_bessel_J1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_J1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Jn :: Int -> Double -> Double
bessel_Jn n x =
  unsafePerformIO(
    prim_bessel_Jn n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Jn" prim_bessel_Jn :: Int -> Double -> IO (Double)

bessel_Jn_e :: Int -> Double -> (Double,Double)
bessel_Jn_e n x =
  unsafePerformIO(
    prim_bessel_Jn_e n x
    >>= \ gc_result ->
    access_prim_bessel_Jn_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Jn_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Jn_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Jn_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Jn_e" prim_bessel_Jn_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jn_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jn_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jn_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jn_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Y0 :: Double -> Double
bessel_Y0 x =
  unsafePerformIO(
    prim_bessel_Y0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Y0" prim_bessel_Y0 :: Double -> IO (Double)

bessel_Y0_e :: Double -> (Double,Double)
bessel_Y0_e x =
  unsafePerformIO(
    prim_bessel_Y0_e x
    >>= \ gc_result ->
    access_prim_bessel_Y0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Y0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Y0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Y0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Y0_e" prim_bessel_Y0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Y1 :: Double -> Double
bessel_Y1 x =
  unsafePerformIO(
    prim_bessel_Y1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Y1" prim_bessel_Y1 :: Double -> IO (Double)

bessel_Y1_e :: Double -> (Double,Double)
bessel_Y1_e x =
  unsafePerformIO(
    prim_bessel_Y1_e x
    >>= \ gc_result ->
    access_prim_bessel_Y1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Y1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Y1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Y1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Y1_e" prim_bessel_Y1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Y1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Yn :: Int -> Double -> Double
bessel_Yn n x =
  unsafePerformIO(
    prim_bessel_Yn n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Yn" prim_bessel_Yn :: Int -> Double -> IO (Double)

bessel_Yn_e :: Int -> Double -> (Double,Double)
bessel_Yn_e n x =
  unsafePerformIO(
    prim_bessel_Yn_e n x
    >>= \ gc_result ->
    access_prim_bessel_Yn_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Yn_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Yn_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Yn_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Yn_e" prim_bessel_Yn_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Yn_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Yn_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Yn_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Yn_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_I0 :: Double -> Double
bessel_I0 x =
  unsafePerformIO(
    prim_bessel_I0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I0" prim_bessel_I0 :: Double -> IO (Double)

bessel_I0_e :: Double -> (Double,Double)
bessel_I0_e x =
  unsafePerformIO(
    prim_bessel_I0_e x
    >>= \ gc_result ->
    access_prim_bessel_I0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_I0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_I0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_I0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I0_e" prim_bessel_I0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_I1 :: Double -> Double
bessel_I1 x =
  unsafePerformIO(
    prim_bessel_I1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I1" prim_bessel_I1 :: Double -> IO (Double)

bessel_I1_e :: Double -> (Double,Double)
bessel_I1_e x =
  unsafePerformIO(
    prim_bessel_I1_e x
    >>= \ gc_result ->
    access_prim_bessel_I1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_I1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_I1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_I1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I1_e" prim_bessel_I1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_In :: Int -> Double -> Double
bessel_In n x =
  unsafePerformIO(
    prim_bessel_In n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_In" prim_bessel_In :: Int -> Double -> IO (Double)

bessel_In_e :: Int -> Double -> (Double,Double)
bessel_In_e n x =
  unsafePerformIO(
    prim_bessel_In_e n x
    >>= \ gc_result ->
    access_prim_bessel_In_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_In_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_In_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_In_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_In_e" prim_bessel_In_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_I0_scaled :: Double -> Double
bessel_I0_scaled x =
  unsafePerformIO(
    prim_bessel_I0_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I0_scaled" prim_bessel_I0_scaled :: Double -> IO (Double)

bessel_I0_scaled_e :: Double -> (Double,Double)
bessel_I0_scaled_e x =
  unsafePerformIO(
    prim_bessel_I0_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_I0_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_I0_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_I0_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_I0_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I0_scaled_e" prim_bessel_I0_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I0_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_I1_scaled :: Double -> Double
bessel_I1_scaled x =
  unsafePerformIO(
    prim_bessel_I1_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I1_scaled" prim_bessel_I1_scaled :: Double -> IO (Double)

bessel_I1_scaled_e :: Double -> (Double,Double)
bessel_I1_scaled_e x =
  unsafePerformIO(
    prim_bessel_I1_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_I1_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_I1_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_I1_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_I1_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_I1_scaled_e" prim_bessel_I1_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_I1_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_In_scaled :: Int -> Double -> Double
bessel_In_scaled n x =
  unsafePerformIO(
    prim_bessel_In_scaled n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_In_scaled" prim_bessel_In_scaled :: Int -> Double -> IO (Double)

bessel_In_scaled_e :: Int -> Double -> (Double,Double)
bessel_In_scaled_e n x =
  unsafePerformIO(
    prim_bessel_In_scaled_e n x
    >>= \ gc_result ->
    access_prim_bessel_In_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_In_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_In_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_In_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_In_scaled_e" prim_bessel_In_scaled_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_In_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_K0 :: Double -> Double
bessel_K0 x =
  unsafePerformIO(
    prim_bessel_K0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K0" prim_bessel_K0 :: Double -> IO (Double)

bessel_K0_e :: Double -> (Double,Double)
bessel_K0_e x =
  unsafePerformIO(
    prim_bessel_K0_e x
    >>= \ gc_result ->
    access_prim_bessel_K0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_K0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_K0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_K0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K0_e" prim_bessel_K0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_K1 :: Double -> Double
bessel_K1 x =
  unsafePerformIO(
    prim_bessel_K1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K1" prim_bessel_K1 :: Double -> IO (Double)

bessel_K1_e :: Double -> (Double,Double)
bessel_K1_e x =
  unsafePerformIO(
    prim_bessel_K1_e x
    >>= \ gc_result ->
    access_prim_bessel_K1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_K1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_K1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_K1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K1_e" prim_bessel_K1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Kn :: Int -> Double -> Double
bessel_Kn n x =
  unsafePerformIO(
    prim_bessel_Kn n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Kn" prim_bessel_Kn :: Int -> Double -> IO (Double)

bessel_Kn_e :: Int -> Double -> (Double,Double)
bessel_Kn_e n x =
  unsafePerformIO(
    prim_bessel_Kn_e n x
    >>= \ gc_result ->
    access_prim_bessel_Kn_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Kn_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Kn_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Kn_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Kn_e" prim_bessel_Kn_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_e_gc_failstring :: Addr -> IO (Addr)
-------------------------------------------------------------------------------

bessel_K0_scaled :: Double -> Double
bessel_K0_scaled x =
  unsafePerformIO(
    prim_bessel_K0_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K0_scaled" prim_bessel_K0_scaled :: Double -> IO (Double)

bessel_K0_scaled_e :: Double -> (Double,Double)
bessel_K0_scaled_e x =
  unsafePerformIO(
    prim_bessel_K0_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_K0_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_K0_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_K0_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_K0_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K0_scaled_e" prim_bessel_K0_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K0_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_K1_scaled :: Double -> Double
bessel_K1_scaled x =
  unsafePerformIO(
    prim_bessel_K1_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K1_scaled" prim_bessel_K1_scaled :: Double -> IO (Double)

bessel_K1_scaled_e :: Double -> (Double,Double)
bessel_K1_scaled_e x =
  unsafePerformIO(
    prim_bessel_K1_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_K1_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_K1_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_K1_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_K1_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_K1_scaled_e" prim_bessel_K1_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_K1_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Kn_scaled :: Int -> Double -> Double
bessel_Kn_scaled n x =
  unsafePerformIO(
    prim_bessel_Kn_scaled n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Kn_scaled" prim_bessel_Kn_scaled :: Int -> Double -> IO (Double)

bessel_Kn_scaled_e :: Int -> Double -> (Double,Double)
bessel_Kn_scaled_e n x =
  unsafePerformIO(
    prim_bessel_Kn_scaled_e n x
    >>= \ gc_result ->
    access_prim_bessel_Kn_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Kn_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Kn_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Kn_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Kn_scaled_e" prim_bessel_Kn_scaled_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Kn_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_j0 :: Double -> Double
bessel_j0 x =
  unsafePerformIO(
    prim_bessel_j0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_j0" prim_bessel_j0 :: Double -> IO (Double)

bessel_j0_e :: Double -> (Double,Double)
bessel_j0_e x =
  unsafePerformIO(
    prim_bessel_j0_e x
    >>= \ gc_result ->
    access_prim_bessel_j0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_j0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_j0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_j0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_j0_e" prim_bessel_j0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_j1 :: Double -> Double
bessel_j1 x =
  unsafePerformIO(
    prim_bessel_j1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_j1" prim_bessel_j1 :: Double -> IO (Double)

bessel_j1_e :: Double -> (Double,Double)
bessel_j1_e x =
  unsafePerformIO(
    prim_bessel_j1_e x
    >>= \ gc_result ->
    access_prim_bessel_j1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_j1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_j1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_j1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_j1_e" prim_bessel_j1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_j1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_jl :: Int -> Double -> Double
bessel_jl n x =
  unsafePerformIO(
    prim_bessel_jl n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_jl" prim_bessel_jl :: Int -> Double -> IO (Double)

bessel_jl_e :: Int -> Double -> (Double,Double)
bessel_jl_e l x =
  unsafePerformIO(
    prim_bessel_jl_e l x
    >>= \ gc_result ->
    access_prim_bessel_jl_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_jl_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_jl_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_jl_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_jl_e" prim_bessel_jl_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_jl_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_jl_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_jl_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_jl_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_y0 :: Double -> Double
bessel_y0 x =
  unsafePerformIO(
    prim_bessel_y0 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_y0" prim_bessel_y0 :: Double -> IO (Double)

bessel_y0_e :: Double -> (Double,Double)
bessel_y0_e x =
  unsafePerformIO(
    prim_bessel_y0_e x
    >>= \ gc_result ->
    access_prim_bessel_y0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_y0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_y0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_y0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_y0_e" prim_bessel_y0_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_y1 :: Double -> Double
bessel_y1 x =
  unsafePerformIO(
    prim_bessel_y1 x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_y1" prim_bessel_y1 :: Double -> IO (Double)

bessel_y1_e :: Double -> (Double,Double)
bessel_y1_e x =
  unsafePerformIO(
    prim_bessel_y1_e x
    >>= \ gc_result ->
    access_prim_bessel_y1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_y1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_y1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_y1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_y1_e" prim_bessel_y1_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_y1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_yl :: Int -> Double -> Double
bessel_yl n x =
  unsafePerformIO(
    prim_bessel_yl n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_yl" prim_bessel_yl :: Int -> Double -> IO (Double)

bessel_yl_e :: Int -> Double -> (Double,Double)
bessel_yl_e l x =
  unsafePerformIO(
    prim_bessel_yl_e l x
    >>= \ gc_result ->
    access_prim_bessel_yl_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_yl_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_yl_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_yl_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_yl_e" prim_bessel_yl_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_yl_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_yl_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_yl_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_yl_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_i0_scaled :: Double -> Double
bessel_i0_scaled x =
  unsafePerformIO(
    prim_bessel_i0_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_i0_scaled" prim_bessel_i0_scaled :: Double -> IO (Double)

bessel_i0_scaled_e :: Double -> (Double,Double)
bessel_i0_scaled_e x =
  unsafePerformIO(
    prim_bessel_i0_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_i0_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_i0_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_i0_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_i0_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_i0_scaled_e" prim_bessel_i0_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i0_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i0_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i0_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i0_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_i1_scaled :: Double -> Double
bessel_i1_scaled x =
  unsafePerformIO(
    prim_bessel_i1_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_i1_scaled" prim_bessel_i1_scaled :: Double -> IO (Double)

bessel_i1_scaled_e :: Double -> (Double,Double)
bessel_i1_scaled_e x =
  unsafePerformIO(
    prim_bessel_i1_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_i1_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_i1_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_i1_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_i1_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_i1_scaled_e" prim_bessel_i1_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i1_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i1_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i1_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_i1_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_il_scaled :: Int -> Double -> Double
bessel_il_scaled n x =
  unsafePerformIO(
    prim_bessel_il_scaled n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_il_scaled" prim_bessel_il_scaled :: Int -> Double -> IO (Double)

bessel_il_scaled_e :: Int -> Double -> (Double,Double)
bessel_il_scaled_e l x =
  unsafePerformIO(
    prim_bessel_il_scaled_e l x
    >>= \ gc_result ->
    access_prim_bessel_il_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_il_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_il_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_il_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_il_scaled_e" prim_bessel_il_scaled_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_il_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_il_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_il_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_il_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_k0_scaled :: Double -> Double
bessel_k0_scaled x =
  unsafePerformIO(
    prim_bessel_k0_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_k0_scaled" prim_bessel_k0_scaled :: Double -> IO (Double)

bessel_k0_scaled_e :: Double -> (Double,Double)
bessel_k0_scaled_e x =
  unsafePerformIO(
    prim_bessel_k0_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_k0_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_k0_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_k0_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_k0_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_k0_scaled_e" prim_bessel_k0_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k0_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k0_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k0_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k0_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_k1_scaled :: Double -> Double
bessel_k1_scaled x =
  unsafePerformIO(
    prim_bessel_k1_scaled x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_k1_scaled" prim_bessel_k1_scaled :: Double -> IO (Double)

bessel_k1_scaled_e :: Double -> (Double,Double)
bessel_k1_scaled_e x =
  unsafePerformIO(
    prim_bessel_k1_scaled_e x
    >>= \ gc_result ->
    access_prim_bessel_k1_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_k1_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_k1_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_k1_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_k1_scaled_e" prim_bessel_k1_scaled_e :: Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k1_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k1_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k1_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_k1_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_kl_scaled :: Int -> Double -> Double
bessel_kl_scaled n x =
  unsafePerformIO(
    prim_bessel_kl_scaled n x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_kl_scaled" prim_bessel_kl_scaled :: Int -> Double -> IO (Double)

bessel_kl_scaled_e :: Int -> Double -> (Double,Double)
bessel_kl_scaled_e l x =
  unsafePerformIO(
    prim_bessel_kl_scaled_e l x
    >>= \ gc_result ->
    access_prim_bessel_kl_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_kl_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_kl_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_kl_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_kl_scaled_e" prim_bessel_kl_scaled_e :: Int -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_kl_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_kl_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_kl_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_kl_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Jnu :: Double -> Double -> Double
bessel_Jnu nu x =
  unsafePerformIO(
    prim_bessel_Jnu nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Jnu" prim_bessel_Jnu :: Double -> Double -> IO (Double)

bessel_Jnu_e :: Double -> Double -> (Double,Double)
bessel_Jnu_e nu x =
  unsafePerformIO(
    prim_bessel_Jnu_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Jnu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Jnu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Jnu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Jnu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Jnu_e" prim_bessel_Jnu_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jnu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jnu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jnu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Jnu_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Ynu :: Double -> Double -> Double
bessel_Ynu nu x =
  unsafePerformIO(
    prim_bessel_Ynu nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Ynu" prim_bessel_Ynu :: Double -> Double -> IO (Double)

bessel_Ynu_e :: Double -> Double -> (Double,Double)
bessel_Ynu_e nu x =
  unsafePerformIO(
    prim_bessel_Ynu_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Ynu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Ynu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Ynu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Ynu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Ynu_e" prim_bessel_Ynu_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Ynu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Ynu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Ynu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Ynu_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Inu :: Double -> Double -> Double
bessel_Inu nu x =
  unsafePerformIO(
    prim_bessel_Inu nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Inu" prim_bessel_Inu :: Double -> Double -> IO (Double)

bessel_Inu_e :: Double -> Double -> (Double,Double)
bessel_Inu_e nu x =
  unsafePerformIO(
    prim_bessel_Inu_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Inu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Inu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Inu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Inu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Inu_e" prim_bessel_Inu_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Inu_scaled :: Double -> Double -> Double
bessel_Inu_scaled nu x =
  unsafePerformIO(
    prim_bessel_Inu_scaled nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Inu_scaled" prim_bessel_Inu_scaled :: Double -> Double -> IO (Double)

bessel_Inu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Inu_scaled_e nu x =
  unsafePerformIO(
    prim_bessel_Inu_scaled_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Inu_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Inu_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Inu_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Inu_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Inu_scaled_e" prim_bessel_Inu_scaled_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Inu_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Knu :: Double -> Double -> Double
bessel_Knu nu x =
  unsafePerformIO(
    prim_bessel_Knu nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Knu" prim_bessel_Knu :: Double -> Double -> IO (Double)

bessel_Knu_e :: Double -> Double -> (Double,Double)
bessel_Knu_e nu x =
  unsafePerformIO(
    prim_bessel_Knu_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Knu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Knu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Knu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Knu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Knu_e" prim_bessel_Knu_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_Knu_scaled :: Double -> Double -> Double
bessel_Knu_scaled nu x =
  unsafePerformIO(
    prim_bessel_Knu_scaled nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Knu_scaled" prim_bessel_Knu_scaled :: Double -> Double -> IO (Double)

bessel_Knu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Knu_scaled_e nu x =
  unsafePerformIO(
    prim_bessel_Knu_scaled_e nu x
    >>= \ gc_result ->
    access_prim_bessel_Knu_scaled_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_Knu_scaled_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_Knu_scaled_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_Knu_scaled_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_Knu_scaled_e" prim_bessel_Knu_scaled_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_scaled_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_scaled_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_scaled_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_Knu_scaled_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_lnKnu :: Double -> Double -> Double
bessel_lnKnu nu x =
  unsafePerformIO(
    prim_bessel_lnKnu nu x
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_lnKnu" prim_bessel_lnKnu :: Double -> Double -> IO (Double)

bessel_lnKnu_e :: Double -> Double -> (Double,Double)
bessel_lnKnu_e nu x =
  unsafePerformIO(
    prim_bessel_lnKnu_e nu x
    >>= \ gc_result ->
    access_prim_bessel_lnKnu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_lnKnu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_lnKnu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_lnKnu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_lnKnu_e" prim_bessel_lnKnu_e :: Double -> Double -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_lnKnu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_lnKnu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_lnKnu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_lnKnu_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_zero_J0 :: Int -> Double
bessel_zero_J0 s =
  unsafePerformIO(
    prim_bessel_zero_J0 s
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_J0" prim_bessel_zero_J0 :: Int -> IO (Double)

bessel_zero_J0_e :: Int -> (Double,Double)
bessel_zero_J0_e s =
  unsafePerformIO(
    prim_bessel_zero_J0_e s
    >>= \ gc_result ->
    access_prim_bessel_zero_J0_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_zero_J0_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_zero_J0_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_zero_J0_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_J0_e" prim_bessel_zero_J0_e :: Int -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J0_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J0_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J0_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J0_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_zero_J1 :: Int -> Double
bessel_zero_J1 s =
  unsafePerformIO(
    prim_bessel_zero_J1 s
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_J1" prim_bessel_zero_J1 :: Int -> IO (Double)

bessel_zero_J1_e :: Int -> (Double,Double)
bessel_zero_J1_e s =
  unsafePerformIO(
    prim_bessel_zero_J1_e s
    >>= \ gc_result ->
    access_prim_bessel_zero_J1_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_zero_J1_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_zero_J1_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_zero_J1_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_J1_e" prim_bessel_zero_J1_e :: Int -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J1_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J1_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J1_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_J1_e_gc_failstring :: Addr -> IO (Addr)

-------------------------------------------------------------------------------

bessel_zero_Jnu :: Double -> Int -> Double
bessel_zero_Jnu nu s =
  unsafePerformIO(
    prim_bessel_zero_Jnu nu s
    >>= \  y  ->
    (return (y)))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_Jnu" prim_bessel_zero_Jnu :: Double -> Int -> IO (Double)

bessel_zero_Jnu_e :: Double -> Int -> (Double,Double)
bessel_zero_Jnu_e nu s =
  unsafePerformIO(
    prim_bessel_zero_Jnu_e nu s
    >>= \ gc_result ->
    access_prim_bessel_zero_Jnu_e_val (gc_result :: Addr) >>= \ val ->
    access_prim_bessel_zero_Jnu_e_err (gc_result :: Addr) >>= \ err ->
    access_prim_bessel_zero_Jnu_e_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
    access_prim_bessel_zero_Jnu_e_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
    if ( gc_failed /= (0::Int))
    then unmarshall_string_ gc_failstring >>=  ioError  . userError
    else (return ((val,err))))
foreign import  ccall unsafe "Bessel_stub_ffi.h prim_bessel_zero_Jnu_e" prim_bessel_zero_Jnu_e :: Double -> Int -> IO (Addr)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_Jnu_e_val :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_Jnu_e_err :: Addr -> IO (Double)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_Jnu_e_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Bessel_stub_ffi.h" access_prim_bessel_zero_Jnu_e_gc_failstring :: Addr -> IO (Addr)
