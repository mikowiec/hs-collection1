/* Auto generated GreenCard 2 code for FFI */
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_bessel.h>
#include "Numeric/Special/Bessel_stub_ffi.h"
double prim_bessel_J0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_J0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_J0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_J0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_J0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_J0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_J0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_J0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_J1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_J1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_J1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_J1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_J1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_J1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_J1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_J1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Jn(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Jn(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Jn_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Jn_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Jn_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Jn_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Jn_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Jn_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Y0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Y0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Y0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Y0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Y0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Y0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Y0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Y0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Y1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Y1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Y1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Y1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Y1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Y1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Y1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Y1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Yn(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Yn(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Yn_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Yn_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Yn_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Yn_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Yn_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Yn_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_I0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_I0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_I0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_I0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_I0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_I0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_I0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_I0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_I1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_I1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_I1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_I1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_I1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_I1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_I1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_I1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_In(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_In(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_In_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_In_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_In_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_In_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_In_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_In_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_I0_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_I0_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_I0_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_I0_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_I0_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_I0_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_I0_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_I0_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_I1_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_I1_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_I1_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_I1_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_I1_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_I1_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_I1_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_I1_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_In_scaled(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_In_scaled(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_In_scaled_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_In_scaled_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_In_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_In_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_In_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_In_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_K0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_K0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_K0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_K0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_K0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_K0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_K0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_K0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_K1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_K1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_K1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_K1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_K1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_K1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_K1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_K1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Kn(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Kn(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Kn_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Kn_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Kn_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Kn_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Kn_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Kn_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_K0_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_K0_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_K0_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_K0_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_K0_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_K0_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_K0_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_K0_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_K1_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_K1_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_K1_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_K1_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_K1_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_K1_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_K1_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_K1_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Kn_scaled(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Kn_scaled(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Kn_scaled_e(int n,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Kn_scaled_e(n, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Kn_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Kn_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Kn_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Kn_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_j0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_j0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_j0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_j0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_j0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_j0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_j0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_j0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_j1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_j1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_j1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_j1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_j1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_j1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_j1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_j1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_jl(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_jl(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_jl_e(int l,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_jl_e(l, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_jl_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_jl_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_jl_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_jl_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_y0(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_y0(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_y0_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_y0_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_y0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_y0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_y0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_y0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_y1(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_y1(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_y1_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_y1_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_y1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_y1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_y1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_y1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_yl(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_yl(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_yl_e(int l,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_yl_e(l, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_yl_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_yl_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_yl_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_yl_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_i0_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_i0_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_i0_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_i0_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_i0_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_i0_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_i0_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_i0_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_i1_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_i1_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_i1_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_i1_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_i1_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_i1_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_i1_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_i1_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_il_scaled(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_il_scaled(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_il_scaled_e(int l,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_il_scaled_e(l, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_il_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_il_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_il_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_il_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_k0_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_k0_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_k0_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_k0_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_k0_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_k0_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_k0_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_k0_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_k1_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_k1_scaled(x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_k1_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_k1_scaled_e(x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_k1_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_k1_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_k1_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_k1_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_kl_scaled(int n,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_kl_scaled(n, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_kl_scaled_e(int l,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_kl_scaled_e(l, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_kl_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_kl_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_kl_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_kl_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Jnu(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Jnu(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Jnu_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Jnu_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Jnu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Jnu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Jnu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Jnu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Ynu(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Ynu(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Ynu_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Ynu_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Ynu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Ynu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Ynu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Ynu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Inu(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Inu(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Inu_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Inu_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Inu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Inu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Inu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Inu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Inu_scaled(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Inu_scaled(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Inu_scaled_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Inu_scaled_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Inu_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Inu_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Inu_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Inu_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Knu(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Knu(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Knu_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Knu_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Knu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Knu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Knu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Knu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_Knu_scaled(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_Knu_scaled(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_Knu_scaled_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_Knu_scaled_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_Knu_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_Knu_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_Knu_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_Knu_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_lnKnu(double nu,double x)
{ double y;
  do { double y;
     y = gsl_sf_bessel_lnKnu(nu, x);
      
      return((double)(y));} while(0);
}
void* prim_bessel_lnKnu_e(double nu,double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_lnKnu_e(nu, x, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_lnKnu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_lnKnu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_lnKnu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_lnKnu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_zero_J0(int s)
{ double y;
  do { double y;
     y = gsl_sf_bessel_zero_J0(s);
      
      return((double)(y));} while(0);
}
void* prim_bessel_zero_J0_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_zero_J0_e(s, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_zero_J0_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_zero_J0_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_zero_J0_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_zero_J0_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_zero_J1(int s)
{ double y;
  do { double y;
     y = gsl_sf_bessel_zero_J1(s);
      
      return((double)(y));} while(0);
}
void* prim_bessel_zero_J1_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_zero_J1_e(s, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_zero_J1_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_zero_J1_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_zero_J1_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_zero_J1_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_bessel_zero_Jnu(double nu,int s)
{ double y;
  do { double y;
     y = gsl_sf_bessel_zero_Jnu(nu, s);
      
      return((double)(y));} while(0);
}
void* prim_bessel_zero_Jnu_e(double nu,int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_bessel_zero_Jnu_e(nu, s, &result);
     val = result.val;
     err = result.err;
      if ((gc_failed = ( status != 0 ))) {gc_failstring = gsl_strerror(status) ;}
      else {gc_failed = 0;}
      gc_result.val = (double)(val);
      gc_result.err = (double)(err);
      gc_result.gc_failed = gc_failed;
      gc_result.gc_failstring = gc_failstring;
      
      return(&gc_result);} while(0);
}
double access_prim_bessel_zero_Jnu_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_bessel_zero_Jnu_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_bessel_zero_Jnu_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_bessel_zero_Jnu_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
