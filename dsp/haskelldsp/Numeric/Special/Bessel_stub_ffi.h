#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_bessel.h>
#include "HsFFI.h"
extern double prim_bessel_J0(double x);
extern void* prim_bessel_J0_e(double x);
extern double access_prim_bessel_J0_e_val(HsPtr);
extern double access_prim_bessel_J0_e_err(HsPtr);
extern int access_prim_bessel_J0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_J0_e_gc_failstring(HsPtr);
extern double prim_bessel_J1(double x);
extern void* prim_bessel_J1_e(double x);
extern double access_prim_bessel_J1_e_val(HsPtr);
extern double access_prim_bessel_J1_e_err(HsPtr);
extern int access_prim_bessel_J1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_J1_e_gc_failstring(HsPtr);
extern double prim_bessel_Jn(int n,double x);
extern void* prim_bessel_Jn_e(int n,double x);
extern double access_prim_bessel_Jn_e_val(HsPtr);
extern double access_prim_bessel_Jn_e_err(HsPtr);
extern int access_prim_bessel_Jn_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Jn_e_gc_failstring(HsPtr);
extern double prim_bessel_Y0(double x);
extern void* prim_bessel_Y0_e(double x);
extern double access_prim_bessel_Y0_e_val(HsPtr);
extern double access_prim_bessel_Y0_e_err(HsPtr);
extern int access_prim_bessel_Y0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Y0_e_gc_failstring(HsPtr);
extern double prim_bessel_Y1(double x);
extern void* prim_bessel_Y1_e(double x);
extern double access_prim_bessel_Y1_e_val(HsPtr);
extern double access_prim_bessel_Y1_e_err(HsPtr);
extern int access_prim_bessel_Y1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Y1_e_gc_failstring(HsPtr);
extern double prim_bessel_Yn(int n,double x);
extern void* prim_bessel_Yn_e(int n,double x);
extern double access_prim_bessel_Yn_e_val(HsPtr);
extern double access_prim_bessel_Yn_e_err(HsPtr);
extern int access_prim_bessel_Yn_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Yn_e_gc_failstring(HsPtr);
extern double prim_bessel_I0(double x);
extern void* prim_bessel_I0_e(double x);
extern double access_prim_bessel_I0_e_val(HsPtr);
extern double access_prim_bessel_I0_e_err(HsPtr);
extern int access_prim_bessel_I0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_I0_e_gc_failstring(HsPtr);
extern double prim_bessel_I1(double x);
extern void* prim_bessel_I1_e(double x);
extern double access_prim_bessel_I1_e_val(HsPtr);
extern double access_prim_bessel_I1_e_err(HsPtr);
extern int access_prim_bessel_I1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_I1_e_gc_failstring(HsPtr);
extern double prim_bessel_In(int n,double x);
extern void* prim_bessel_In_e(int n,double x);
extern double access_prim_bessel_In_e_val(HsPtr);
extern double access_prim_bessel_In_e_err(HsPtr);
extern int access_prim_bessel_In_e_gc_failed(HsPtr);
extern void* access_prim_bessel_In_e_gc_failstring(HsPtr);
extern double prim_bessel_I0_scaled(double x);
extern void* prim_bessel_I0_scaled_e(double x);
extern double access_prim_bessel_I0_scaled_e_val(HsPtr);
extern double access_prim_bessel_I0_scaled_e_err(HsPtr);
extern int access_prim_bessel_I0_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_I0_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_I1_scaled(double x);
extern void* prim_bessel_I1_scaled_e(double x);
extern double access_prim_bessel_I1_scaled_e_val(HsPtr);
extern double access_prim_bessel_I1_scaled_e_err(HsPtr);
extern int access_prim_bessel_I1_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_I1_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_In_scaled(int n,double x);
extern void* prim_bessel_In_scaled_e(int n,double x);
extern double access_prim_bessel_In_scaled_e_val(HsPtr);
extern double access_prim_bessel_In_scaled_e_err(HsPtr);
extern int access_prim_bessel_In_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_In_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_K0(double x);
extern void* prim_bessel_K0_e(double x);
extern double access_prim_bessel_K0_e_val(HsPtr);
extern double access_prim_bessel_K0_e_err(HsPtr);
extern int access_prim_bessel_K0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_K0_e_gc_failstring(HsPtr);
extern double prim_bessel_K1(double x);
extern void* prim_bessel_K1_e(double x);
extern double access_prim_bessel_K1_e_val(HsPtr);
extern double access_prim_bessel_K1_e_err(HsPtr);
extern int access_prim_bessel_K1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_K1_e_gc_failstring(HsPtr);
extern double prim_bessel_Kn(int n,double x);
extern void* prim_bessel_Kn_e(int n,double x);
extern double access_prim_bessel_Kn_e_val(HsPtr);
extern double access_prim_bessel_Kn_e_err(HsPtr);
extern int access_prim_bessel_Kn_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Kn_e_gc_failstring(HsPtr);
extern double prim_bessel_K0_scaled(double x);
extern void* prim_bessel_K0_scaled_e(double x);
extern double access_prim_bessel_K0_scaled_e_val(HsPtr);
extern double access_prim_bessel_K0_scaled_e_err(HsPtr);
extern int access_prim_bessel_K0_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_K0_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_K1_scaled(double x);
extern void* prim_bessel_K1_scaled_e(double x);
extern double access_prim_bessel_K1_scaled_e_val(HsPtr);
extern double access_prim_bessel_K1_scaled_e_err(HsPtr);
extern int access_prim_bessel_K1_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_K1_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_Kn_scaled(int n,double x);
extern void* prim_bessel_Kn_scaled_e(int n,double x);
extern double access_prim_bessel_Kn_scaled_e_val(HsPtr);
extern double access_prim_bessel_Kn_scaled_e_err(HsPtr);
extern int access_prim_bessel_Kn_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Kn_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_j0(double x);
extern void* prim_bessel_j0_e(double x);
extern double access_prim_bessel_j0_e_val(HsPtr);
extern double access_prim_bessel_j0_e_err(HsPtr);
extern int access_prim_bessel_j0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_j0_e_gc_failstring(HsPtr);
extern double prim_bessel_j1(double x);
extern void* prim_bessel_j1_e(double x);
extern double access_prim_bessel_j1_e_val(HsPtr);
extern double access_prim_bessel_j1_e_err(HsPtr);
extern int access_prim_bessel_j1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_j1_e_gc_failstring(HsPtr);
extern double prim_bessel_jl(int n,double x);
extern void* prim_bessel_jl_e(int l,double x);
extern double access_prim_bessel_jl_e_val(HsPtr);
extern double access_prim_bessel_jl_e_err(HsPtr);
extern int access_prim_bessel_jl_e_gc_failed(HsPtr);
extern void* access_prim_bessel_jl_e_gc_failstring(HsPtr);
extern double prim_bessel_y0(double x);
extern void* prim_bessel_y0_e(double x);
extern double access_prim_bessel_y0_e_val(HsPtr);
extern double access_prim_bessel_y0_e_err(HsPtr);
extern int access_prim_bessel_y0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_y0_e_gc_failstring(HsPtr);
extern double prim_bessel_y1(double x);
extern void* prim_bessel_y1_e(double x);
extern double access_prim_bessel_y1_e_val(HsPtr);
extern double access_prim_bessel_y1_e_err(HsPtr);
extern int access_prim_bessel_y1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_y1_e_gc_failstring(HsPtr);
extern double prim_bessel_yl(int n,double x);
extern void* prim_bessel_yl_e(int l,double x);
extern double access_prim_bessel_yl_e_val(HsPtr);
extern double access_prim_bessel_yl_e_err(HsPtr);
extern int access_prim_bessel_yl_e_gc_failed(HsPtr);
extern void* access_prim_bessel_yl_e_gc_failstring(HsPtr);
extern double prim_bessel_i0_scaled(double x);
extern void* prim_bessel_i0_scaled_e(double x);
extern double access_prim_bessel_i0_scaled_e_val(HsPtr);
extern double access_prim_bessel_i0_scaled_e_err(HsPtr);
extern int access_prim_bessel_i0_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_i0_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_i1_scaled(double x);
extern void* prim_bessel_i1_scaled_e(double x);
extern double access_prim_bessel_i1_scaled_e_val(HsPtr);
extern double access_prim_bessel_i1_scaled_e_err(HsPtr);
extern int access_prim_bessel_i1_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_i1_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_il_scaled(int n,double x);
extern void* prim_bessel_il_scaled_e(int l,double x);
extern double access_prim_bessel_il_scaled_e_val(HsPtr);
extern double access_prim_bessel_il_scaled_e_err(HsPtr);
extern int access_prim_bessel_il_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_il_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_k0_scaled(double x);
extern void* prim_bessel_k0_scaled_e(double x);
extern double access_prim_bessel_k0_scaled_e_val(HsPtr);
extern double access_prim_bessel_k0_scaled_e_err(HsPtr);
extern int access_prim_bessel_k0_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_k0_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_k1_scaled(double x);
extern void* prim_bessel_k1_scaled_e(double x);
extern double access_prim_bessel_k1_scaled_e_val(HsPtr);
extern double access_prim_bessel_k1_scaled_e_err(HsPtr);
extern int access_prim_bessel_k1_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_k1_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_kl_scaled(int n,double x);
extern void* prim_bessel_kl_scaled_e(int l,double x);
extern double access_prim_bessel_kl_scaled_e_val(HsPtr);
extern double access_prim_bessel_kl_scaled_e_err(HsPtr);
extern int access_prim_bessel_kl_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_kl_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_Jnu(double nu,double x);
extern void* prim_bessel_Jnu_e(double nu,double x);
extern double access_prim_bessel_Jnu_e_val(HsPtr);
extern double access_prim_bessel_Jnu_e_err(HsPtr);
extern int access_prim_bessel_Jnu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Jnu_e_gc_failstring(HsPtr);
extern double prim_bessel_Ynu(double nu,double x);
extern void* prim_bessel_Ynu_e(double nu,double x);
extern double access_prim_bessel_Ynu_e_val(HsPtr);
extern double access_prim_bessel_Ynu_e_err(HsPtr);
extern int access_prim_bessel_Ynu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Ynu_e_gc_failstring(HsPtr);
extern double prim_bessel_Inu(double nu,double x);
extern void* prim_bessel_Inu_e(double nu,double x);
extern double access_prim_bessel_Inu_e_val(HsPtr);
extern double access_prim_bessel_Inu_e_err(HsPtr);
extern int access_prim_bessel_Inu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Inu_e_gc_failstring(HsPtr);
extern double prim_bessel_Inu_scaled(double nu,double x);
extern void* prim_bessel_Inu_scaled_e(double nu,double x);
extern double access_prim_bessel_Inu_scaled_e_val(HsPtr);
extern double access_prim_bessel_Inu_scaled_e_err(HsPtr);
extern int access_prim_bessel_Inu_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Inu_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_Knu(double nu,double x);
extern void* prim_bessel_Knu_e(double nu,double x);
extern double access_prim_bessel_Knu_e_val(HsPtr);
extern double access_prim_bessel_Knu_e_err(HsPtr);
extern int access_prim_bessel_Knu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Knu_e_gc_failstring(HsPtr);
extern double prim_bessel_Knu_scaled(double nu,double x);
extern void* prim_bessel_Knu_scaled_e(double nu,double x);
extern double access_prim_bessel_Knu_scaled_e_val(HsPtr);
extern double access_prim_bessel_Knu_scaled_e_err(HsPtr);
extern int access_prim_bessel_Knu_scaled_e_gc_failed(HsPtr);
extern void* access_prim_bessel_Knu_scaled_e_gc_failstring(HsPtr);
extern double prim_bessel_lnKnu(double nu,double x);
extern void* prim_bessel_lnKnu_e(double nu,double x);
extern double access_prim_bessel_lnKnu_e_val(HsPtr);
extern double access_prim_bessel_lnKnu_e_err(HsPtr);
extern int access_prim_bessel_lnKnu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_lnKnu_e_gc_failstring(HsPtr);
extern double prim_bessel_zero_J0(int s);
extern void* prim_bessel_zero_J0_e(int s);
extern double access_prim_bessel_zero_J0_e_val(HsPtr);
extern double access_prim_bessel_zero_J0_e_err(HsPtr);
extern int access_prim_bessel_zero_J0_e_gc_failed(HsPtr);
extern void* access_prim_bessel_zero_J0_e_gc_failstring(HsPtr);
extern double prim_bessel_zero_J1(int s);
extern void* prim_bessel_zero_J1_e(int s);
extern double access_prim_bessel_zero_J1_e_val(HsPtr);
extern double access_prim_bessel_zero_J1_e_err(HsPtr);
extern int access_prim_bessel_zero_J1_e_gc_failed(HsPtr);
extern void* access_prim_bessel_zero_J1_e_gc_failstring(HsPtr);
extern double prim_bessel_zero_Jnu(double nu,int s);
extern void* prim_bessel_zero_Jnu_e(double nu,int s);
extern double access_prim_bessel_zero_Jnu_e_val(HsPtr);
extern double access_prim_bessel_zero_Jnu_e_err(HsPtr);
extern int access_prim_bessel_zero_Jnu_e_gc_failed(HsPtr);
extern void* access_prim_bessel_zero_Jnu_e_gc_failstring(HsPtr);
