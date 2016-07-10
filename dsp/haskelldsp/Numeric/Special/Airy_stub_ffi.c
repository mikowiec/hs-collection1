/* Auto generated GreenCard 2 code for FFI */
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_airy.h>
#include "Numeric/Special/Airy_stub_ffi.h"
double prim_airy_Ai(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Ai(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Ai_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Ai_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Ai_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Ai_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Ai_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Ai_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Ai_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Ai_scaled(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Ai_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Ai_scaled_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Ai_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Ai_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Ai_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Ai_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Ai_deriv(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Ai_deriv(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Ai_deriv_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Ai_deriv_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Ai_deriv_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Ai_deriv_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Ai_deriv_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Ai_deriv_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Ai_deriv_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Ai_deriv_scaled(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Ai_deriv_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Ai_deriv_scaled_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Ai_deriv_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Ai_deriv_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Ai_deriv_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Ai_deriv_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_zero_Ai(int s)
{ double z;
  do { double z;
     z = gsl_sf_airy_zero_Ai(s);
      
      return((double)(z));} while(0);
}
void* prim_airy_zero_Ai_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_zero_Ai_e(s, &result);
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
double access_prim_airy_zero_Ai_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_zero_Ai_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_zero_Ai_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_zero_Ai_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_zero_Ai_deriv(int s)
{ double z;
  do { double z;
     z = gsl_sf_airy_zero_Ai_deriv(s);
      
      return((double)(z));} while(0);
}
void* prim_airy_zero_Ai_deriv_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_zero_Ai_deriv_e(s, &result);
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
double access_prim_airy_zero_Ai_deriv_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_zero_Ai_deriv_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_zero_Ai_deriv_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_zero_Ai_deriv_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Bi(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Bi(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Bi_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Bi_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Bi_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Bi_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Bi_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Bi_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Bi_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Bi_scaled(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Bi_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Bi_scaled_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Bi_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Bi_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Bi_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Bi_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Bi_deriv(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Bi_deriv(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Bi_deriv_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Bi_deriv_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Bi_deriv_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Bi_deriv_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Bi_deriv_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Bi_deriv_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_Bi_deriv_scaled(double x)
{ double y;
  do { double y;
     y = gsl_sf_airy_Bi_deriv_scaled(x, GSL_PREC_DOUBLE);
      
      return((double)(y));} while(0);
}
void* prim_airy_Bi_deriv_scaled_e(double x)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_Bi_deriv_scaled_e(x, GSL_PREC_DOUBLE, &result);
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
double access_prim_airy_Bi_deriv_scaled_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_Bi_deriv_scaled_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_Bi_deriv_scaled_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_Bi_deriv_scaled_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_zero_Bi(int s)
{ double z;
  do { double z;
     z = gsl_sf_airy_zero_Bi(s);
      
      return((double)(z));} while(0);
}
void* prim_airy_zero_Bi_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_zero_Bi_e(s, &result);
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
double access_prim_airy_zero_Bi_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_zero_Bi_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_zero_Bi_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_zero_Bi_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
double prim_airy_zero_Bi_deriv(int s)
{ double z;
  do { double z;
     z = gsl_sf_airy_zero_Bi_deriv(s);
      
      return((double)(z));} while(0);
}
void* prim_airy_zero_Bi_deriv_e(int s)
{ static struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;} gc_result;
  double val; double err;int gc_failed;
			 char* gc_failstring;
  do { int status;
     double val;
     double err;
     gsl_sf_result result;
     status = gsl_sf_airy_zero_Bi_deriv_e(s, &result);
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
double access_prim_airy_zero_Bi_deriv_e_val(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->val);}
double access_prim_airy_zero_Bi_deriv_e_err(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->err);}
int access_prim_airy_zero_Bi_deriv_e_gc_failed(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failed);}
void* access_prim_airy_zero_Bi_deriv_e_gc_failstring(void *ptr){ return(((struct {HsDouble val;HsDouble err;HsInt gc_failed;HsPtr gc_failstring;}*) ptr)->gc_failstring);}
