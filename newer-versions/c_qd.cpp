/*
 * src/c_qd.cc

MODIFIED BY RJF 2/9/06 to work with lisp inserting fpu_fix_start.
This ALSO has polynomial evaluation.
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * Contains C wrapper function for quad-double precision arithmetic.
 * This can be used from fortran code.
 */
#include <cstring>

#include "config.h"
#include <qd/qd.h>
#include <qd/c_qd.h>
#include <qd/fpu.h>

#define TO_DOUBLE_PTR(a, ptr) ptr[0] = a.x[0]; ptr[1] = a.x[1]; \
                              ptr[2] = a.x[2]; ptr[3] = a.x[3];

extern "C" {



/* add */
void c_qd_add(const double *a, const double *b, double *c) {
     
  fpu_fix_start(NULL);

  qd_real cc;
  cc = qd_real(a) + qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_add_qd_dd(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) + dd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_add_dd_qd(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = dd_real(a) + qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_add_qd_d(const double *a, double b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) + b;
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_add_d_qd(double a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = a + qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}



/* sub */
void c_qd_sub(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) - qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_sub_qd_dd(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) - dd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_sub_dd_qd(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = dd_real(a) - qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_sub_qd_d(const double *a, double b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) - b;
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_sub_d_qd(double a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = a - qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}



/* mul */
void c_qd_mul(const double *a, const double *b, double *c) {
  qd_real cc;
     
  fpu_fix_start(NULL);

  cc = qd_real(a) * qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
  /*added by RJF 2/9/06*/
void c_qd_mulX(const double *a, const double *b, double *c) {
  qd_real cc; 
   
  fpu_fix_start(NULL);
  cc = qd_real(a) * qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}



void c_qd_mul_qd_dd(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = qd_real(a) * dd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_mul_dd_qd(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = dd_real(a) * qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_mul_qd_d(const double *a, double b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = qd_real(a) * b;
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_mul_d_qd(double a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = a * qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}



/* div */
void c_qd_div(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = qd_real(a) / qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_div_qd_dd(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = qd_real(a) / dd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_div_dd_qd(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = dd_real(a) / qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_div_qd_d(const double *a, double b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = qd_real(a) / b;
  TO_DOUBLE_PTR(cc, c);
}
void c_qd_div_d_qd(double a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = a / qd_real(b);
  TO_DOUBLE_PTR(cc, c);
}




/* selfadd */
void c_qd_selfadd(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb += qd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfadd_dd(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb += dd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfadd_d(double a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb += a;
  TO_DOUBLE_PTR(bb, b);
}



/* selfsub */
void c_qd_selfsub(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb -= qd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfsub_dd(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb -= dd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfsub_d(double a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb -= a;
  TO_DOUBLE_PTR(bb, b);
}



/* selfmul */
void c_qd_selfmul(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb *= qd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfmul_dd(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb *= dd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfmul_d(double a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb *= a;
  TO_DOUBLE_PTR(bb, b);
}



/* selfdiv */
void c_qd_selfdiv(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb /= qd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfdiv_dd(const double *a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb /= dd_real(a);
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_selfdiv_d(double a, double *b) {
  qd_real bb(b);
   
  fpu_fix_start(NULL);

  bb /= a;
  TO_DOUBLE_PTR(bb, b);
}



/* copy */
void c_qd_copy(const double *a, double *b) {
  b[0] = a[0];
  b[1] = a[1];
  b[2] = a[2];
  b[3] = a[3];
}
void c_qd_copy_dd(const double *a, double *b) {
  b[0] = a[0];
  b[1] = a[1];
  b[2] = 0.0;
  b[3] = 0.0;
}
void c_qd_copy_d(double a, double *b) {
  b[0] = a;
  b[1] = 0.0;
  b[2] = 0.0;
  b[3] = 0.0;
}


void c_qd_sqrt(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = sqrt(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_sqr(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = sqr(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_abs(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = abs(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_npwr(const double *a, int n, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = npwr(qd_real(a), n);
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_nroot(const double *a, int n, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = nroot(qd_real(a), n);
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_nint(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = nint(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_aint(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = aint(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_floor(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = floor(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_ceil(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = ceil(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_log(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = log(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_log10(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = log10(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_exp(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = exp(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_sin(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = sin(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_cos(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = cos(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_tan(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = tan(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_asin(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = asin(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_acos(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = acos(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_atan(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = atan(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_atan2(const double *a, const double *b, double *c) {
  qd_real cc;
   
  fpu_fix_start(NULL);

  cc = atan2(qd_real(a), qd_real(b));
  TO_DOUBLE_PTR(cc, c);
}

void c_qd_sinh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = sinh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_cosh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = cosh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_tanh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = tanh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_asinh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = asinh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_acosh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = acosh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}
void c_qd_atanh(const double *a, double *b) {
  qd_real bb;
   
  fpu_fix_start(NULL);

  bb = atanh(qd_real(a));
  TO_DOUBLE_PTR(bb, b);
}

void c_qd_sincos(const double *a, double *s, double *c) {
  qd_real ss, cc;
   
  fpu_fix_start(NULL);

  sincos(qd_real(a), ss, cc);
  TO_DOUBLE_PTR(cc, c);
  TO_DOUBLE_PTR(ss, s);
}

void c_qd_sincosh(const double *a, double *s, double *c) {
  qd_real ss, cc;
   
  fpu_fix_start(NULL);

  sincosh(qd_real(a), ss, cc);
  TO_DOUBLE_PTR(cc, c);
  TO_DOUBLE_PTR(ss, s);
}

void c_qd_read(const char *s, double *a) {
  qd_real aa(s);
  TO_DOUBLE_PTR(aa, a);
}

void c_qd_swrite(const double *a, char *s) {
  qd_real aa(a);
  aa.write(s);
}

void c_qd_write(const double *a) {
  std::cout << qd_real(a) << std::endl;
}

void c_qd_neg(const double *a, double *b) {
  b[0] = -a[0];
  b[1] = -a[1];
  b[2] = -a[2];
  b[3] = -a[3];
}

void c_qd_rand(double *a) {
  qd_real aa;
   
  fpu_fix_start(NULL);

  aa = qdrand();
  TO_DOUBLE_PTR(aa, a);
}

void c_qd_comp(const double *a, const double *b, int *result) {
  qd_real aa(a), bb(b);

  if (aa < bb)
    *result = -1;
  else if (aa > bb)
    *result = 1;
  else 
    *result = 0;
}

void c_qd_comp_qd_d(const double *a, double b, int *result) {
  qd_real aa(a);
  if (aa < b)
    *result = -1;
  else if (aa > b)
    *result = 1;
  else 
    *result = 0;
}

void c_qd_comp_d_qd(double a, const double *b, int *result) {
  qd_real bb(b);
  if (a < bb)
    *result = -1;
  else if (a > bb)
    *result = 1;
  else 
    *result = 0;
}

void c_qd_pi(double *a) {
  TO_DOUBLE_PTR(qd_real::_pi, a);
}



  /* can we just compile this in c_qp.cpp ?*/

  /*Perhaps a bit easier may be to pass an array of 4*(n + 1) doubles, 
    each quartet of doubles representing a coefficient. hints from Yozo.
 */

void c_qd_polyevalflat( const double *c, int n, const double *x, double *result) {
  double *p = c + 4*n;  /* pointer to current coefficient */
  fpu_fix_start(NULL);
  qd_real r = qd_real(p);
  qd_real xx = qd_real(x);
    for (int i = n-1; i >= 0; i--)
      { 
	p -= 4;
	r *= xx;
	r += qd_real(p);
  }
  TO_DOUBLE_PTR(r, result);
 }


}
