/* FPU-related code for systems without glibc on powerpc.
   Copyright (C) 1997, 1998, 1999, 2008 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).
This file incorporates code from the GNU C Library (glibc) under the GNU LGPL.

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version 3.1,
as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively. If not, see
<http://www.gnu.org/licenses/>. */

/* FPU Types/Constants: */

/* Type representing exception flags. */
typedef unsigned int fexcept_t;

/* Type representing floating-point environment. We leave it as 'double'
   for efficiency reasons (rather than writing it to a 32-bit integer). */
typedef double fenv_t;

/* We want to specify the bit pattern of the __fe_*_env constants,
   so pretend they're really 'long long' instead of 'double'. */

/* If the default argument is used we use this value. */
const unsigned long long __fe_dfl_env __attribute__ ((aligned (8))) = 
0xfff8000000000000ULL;
#define FE_DFL_ENV	((double*)(&__fe_dfl_env))

/* Floating-point environment where all exceptions are enabled.
   Note that this is not sufficient to give you SIGFPE. */
const unsigned long long __fe_enabled_env __attribute__ ((aligned (8))) = 
0xfff80000000000f8ULL;
#define FE_ENABLED_ENV	((double*)(&__fe_enabled_env))

/* Floating-point environment with (processor-dependent) non-IEEE floating point. */
const unsigned long long __fe_nonieee_env __attribute__ ((aligned (8))) = 
0xfff8000000000004ULL;
#define FE_NONIEEE_ENV	((double*)(&__fe_nonieee_env))

/* Provide __fe_mask_env and __fe_nomask_env */
#ifdef __VXWORKS__

/* These are based loosly off of glibc
   See also: glibc/sysdeps/unix/sysv/linux/powerpc/powerpc32/fpu/fe{,no}mask.c
 */

#include <arch/ppc/vxPpcLib.h>
#include <arch/ppc/archPpc.h>

#if !defined (_PPC_MSR_FE0) || !defined (_PPC_MSR_FE1)
#error FPU Support does not appear to exist on target platform
#else

const fenv_t *
__fe_nomask_env(void)
{
  UINT32 msr = vxMsrGet();
  msr |= _PPC_MSR_FE0 | _PPC_MSR_FE1;
  vxMsrSet(msr);
  return FE_ENABLED_ENV;
}

const fenv_t *
__fe_nomask_env(void)
{
  UINT32 msr = vxMsrGet();
  msr &= ~(_PPC_MSR_FE0 | _PPC_MSR_FE1);
  vxMsrSet(msr);
  return FE_DFL_ENV;
}

#endif
    
#else

/* VxWorks is the only OS that this header supports so far. It would be trivial
   to also support Linux on PowerPC, but this should be unnecessary as Linux
   uses glibc so fpu-glibc.h should be used instead anyway. */
   
/* These stubs won't work... error unless there's reason not to do so: */
#error Unsupported target, use fpu-glibc.h or similar instead.

#include <errno.h>

/* This is a generic stub. An OS specific override is required to set
   the FE0/FE1 bits in the MSR. MSR update is privileged, so this will
   normally involve a syscall. */

const fenv_t *
__fe_nomask_env(void)
{
  __set_errno (ENOSYS);
  return FE_ENABLED_ENV;
}

/* This is a generic stub. An OS specific override is required to clear
   the FE0/FE1 bits in the MSR. MSR update is privileged, so this will
   normally involve a syscall. */

const fenv_t *
__fe_mask_env(void)
{
  __set_errno (ENOSYS);
  return FE_DFL_ENV;
}
#endif

/* Define bits representing the exception. We use the bit positions of
   the appropriate bits in the FPSCR... */
enum
  {
    FE_INEXACT = 1 << (31 - 6),
#define FE_INEXACT		FE_INEXACT
    FE_DIVBYZERO = 1 << (31 - 5),
#define FE_DIVBYZERO	FE_DIVBYZERO
    FE_UNDERFLOW = 1 << (31 - 4),
#define FE_UNDERFLOW	FE_UNDERFLOW
    FE_OVERFLOW = 1 << (31 - 3),
#define FE_OVERFLOW		FE_OVERFLOW

    /* ...except for FE_INVALID, for which we use bit 31. FE_INVALID
       actually corresponds to bits 7 through 12 and 21 through 23
       in the FPSCR, but we can't use that because the current draft
       says that it must be a power of 2. Instead we use bit 2 which
       is the summary bit for all the FE_INVALID exceptions, which
       kind of makes sense. */
    FE_INVALID = 1 << (31 - 2),
#define FE_INVALID		FE_INVALID

    /* Breakdown of the FE_INVALID bits. Setting FE_INVALID on
       an input to a routine is equivalent to setting all of these bits;
       FE_INVALID will be set on output from a routine iff one of
       these bits is set. Note, though, that you can't disable or
       enable these exceptions individually. */

    /* Operation with SNaN. */
    FE_INVALID_SNAN = 1 << (31 - 7),
#define FE_INVALID_SNAN	FE_INVALID_SNAN

    /* Inf - Inf */
    FE_INVALID_ISI = 1 << (31 - 8),
#define FE_INVALID_ISI	FE_INVALID_ISI

    /* Inf / Inf */
    FE_INVALID_IDI = 1 << (31 - 9),
#define FE_INVALID_IDI	FE_INVALID_IDI

    /* 0 / 0 */
    FE_INVALID_ZDZ = 1 << (31 - 10),
#define FE_INVALID_ZDZ	FE_INVALID_ZDZ

    /* Inf * 0 */
    FE_INVALID_IMZ = 1 << (31 - 11),
#define FE_INVALID_IMZ	FE_INVALID_IMZ

    /* Comparison with NaN or SNaN.  */
    FE_INVALID_COMPARE = 1 << (31 - 12),
#define FE_INVALID_COMPARE	FE_INVALID_COMPARE

    /* Invalid operation flag for software (not set by hardware). */
    /* Note that some chips don't have this implemented, presumably
       because no-one expected anyone to write software for them %-). */
    FE_INVALID_SOFTWARE = 1 << (31 - 21),
#define FE_INVALID_SOFTWARE	FE_INVALID_SOFTWARE

    /* Square root of negative number (including -Inf).  */
    /* Note that some chips don't have this implemented.  */
    FE_INVALID_SQRT = 1 << (31 - 22),
#define FE_INVALID_SQRT	FE_INVALID_SQRT

    /* Conversion-to-integer of a NaN or a number too large or too small. */
    FE_INVALID_INTEGER_CONVERSION = 1 << (31 - 23)
#define FE_INVALID_INTEGER_CONVERSION	FE_INVALID_INTEGER_CONVERSION

#define FE_ALL_INVALID \
        (FE_INVALID_SNAN | FE_INVALID_ISI | FE_INVALID_IDI | FE_INVALID_ZDZ \
	 | FE_INVALID_IMZ | FE_INVALID_COMPARE | FE_INVALID_SOFTWARE \
	 | FE_INVALID_SQRT | FE_INVALID_INTEGER_CONVERSION)
  };

#define FE_ALL_EXCEPT \
	(FE_INEXACT | FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW | FE_INVALID)

/* PowerPC chips support all of the four defined rounding modes.
   We use the bit pattern in the FPSCR as the values
   for the appropriate macros. */
enum
  {
    FE_TONEAREST = 0,
#define FE_TONEAREST	FE_TONEAREST
    FE_TOWARDZERO = 1,
#define FE_TOWARDZERO	FE_TOWARDZERO
    FE_UPWARD = 2,
#define FE_UPWARD		FE_UPWARD
    FE_DOWNWARD = 3
#define FE_DOWNWARD		FE_DOWNWARD
  };

/* Floating-point environment with all exceptions enabled. Note that
   just evaluating this value does not change the processor exception mode.
   Passing this mask to fesetenv will result in a prctl syscall to change
   the MSR FE0/FE1 bits to "Precise Mode". On some processors this will
   result in slower floating point execution. This will last until
   a fenv or exception mask is installed that disables all FP exceptions.  */
#define FE_NOMASK_ENV	FE_ENABLED_ENV

/* Floating-point environment with all exceptions disabled. Note that
   just evaluating this value does not change the processor exception mode.
   Passing this mask to fesetenv will result in a prctl syscall to change
   the MSR FE0/FE1 bits to "Ignore Exceptions Mode". On most processors
   this allows the fastest possible floating point execution.*/
#define FE_MASK_ENV		FE_DFL_ENV

/* Type to represent floating point environment */

typedef union
{
  fenv_t fenv;
  unsigned int l[2];
} fenv_union_t;

/* Set/Get FPSCR Register */

#define fegetenv_register() \
        ({ fenv_t env; asm volatile ("mffs %0" : "=f" (env)); env; })
        
#define fesetenv_register(env) \
	do { \
	  double d = (env); \
	  if(GLRO(dl_hwcap) & PPC_FEATURE_HAS_DFP) \
	    asm volatile (".machine push; " \
			  ".machine \"power6\"; " \
			  "mtfsf 0xff,%0,1,0; " \
			  ".machine pop" : : "f" (d)); \
	  else \
	    asm volatile ("mtfsf 0xff,%0" : : "f" (d)); \
	} while(0)

#ifdef __cplusplus
extern "C" {
#endif

int
__fegetexcept (void)
{
  fenv_union_t fe;
  int result = 0;

  fe.fenv = fegetenv_register ();

  if (fe.l[1] & (1 << (31 - FPSCR_XE)))
      result |= FE_INEXACT;
  if (fe.l[1] & (1 << (31 - FPSCR_ZE)))
      result |= FE_DIVBYZERO;
  if (fe.l[1] & (1 << (31 - FPSCR_UE)))
      result |= FE_UNDERFLOW;
  if (fe.l[1] & (1 << (31 - FPSCR_OE)))
      result |= FE_OVERFLOW;
  if (fe.l[1] & (1 << (31 - FPSCR_VE)))
      result |= FE_INVALID;

  return result;
}

int
feenableexcept (int excepts)
{
  fenv_union_t fe;
  int result, new;

  result = __fegetexcept ();

  if ((excepts & FE_ALL_INVALID) == FE_ALL_INVALID)
    excepts = (excepts | FE_INVALID) & ~ FE_ALL_INVALID;

  fe.fenv = fegetenv_register ();
  if (excepts & FE_INEXACT)
    fe.l[1] |= (1 << (31 - FPSCR_XE));
  if (excepts & FE_DIVBYZERO)
    fe.l[1] |= (1 << (31 - FPSCR_ZE));
  if (excepts & FE_UNDERFLOW)
    fe.l[1] |= (1 << (31 - FPSCR_UE));
  if (excepts & FE_OVERFLOW)
    fe.l[1] |= (1 << (31 - FPSCR_OE));
  if (excepts & FE_INVALID)
    fe.l[1] |= (1 << (31 - FPSCR_VE));
  fesetenv_register (fe.fenv);

  new = __fegetexcept ();
  if (new != 0 && result == 0)
    (void)__fe_nomask_env ();

  if ((new & excepts) != excepts)
    result = -1;

  return result;
}

int
fedisableexcept (int excepts)
{
  fenv_union_t fe;
  int result, new;

  result = __fegetexcept ();

  if ((excepts & FE_ALL_INVALID) == FE_ALL_INVALID)
    excepts = (excepts | FE_INVALID) & ~ FE_ALL_INVALID;

  fe.fenv = fegetenv_register ();
  if (excepts & FE_INEXACT)
    fe.l[1] &= ~(1 << (31 - FPSCR_XE));
  if (excepts & FE_DIVBYZERO)
    fe.l[1] &= ~(1 << (31 - FPSCR_ZE));
  if (excepts & FE_UNDERFLOW)
    fe.l[1] &= ~(1 << (31 - FPSCR_UE));
  if (excepts & FE_OVERFLOW)
    fe.l[1] &= ~(1 << (31 - FPSCR_OE));
  if (excepts & FE_INVALID)
    fe.l[1] &= ~(1 << (31 - FPSCR_VE));
  fesetenv_register (fe.fenv);

  new = __fegetexcept ();
  if (new == 0 && result != 0)
    (void)__fe_mask_env ();

  if ((new & excepts) != 0)
    result = -1;
  return result;
}

/* This is from fpu-glibc.h: This is what *all of the above* is for */

void set_fpu (void)
{
  if (FE_ALL_EXCEPT != 0)
    fedisableexcept (FE_ALL_EXCEPT);

  if (options.fpe & GFC_FPE_INVALID)
#ifdef FE_INVALID
    feenableexcept (FE_INVALID);
#else
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

/* glibc does never have a FE_DENORMAL. */
  if (options.fpe & GFC_FPE_DENORMAL)
#ifdef FE_DENORMAL
    feenableexcept (FE_DENORMAL);
#else
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_ZERO)
#ifdef FE_DIVBYZERO
    feenableexcept (FE_DIVBYZERO);
#else
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef FE_OVERFLOW
    feenableexcept (FE_OVERFLOW);
#else
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef FE_UNDERFLOW
    feenableexcept (FE_UNDERFLOW);
#else
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_INEXACT)
#ifdef FE_INEXACT
    feenableexcept (FE_INEXACT);
#else
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif
}

#ifdef __cplusplus
}
#endif
