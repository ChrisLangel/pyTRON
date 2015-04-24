
#ifndef __FORTTYPE


#if ( defined(CRAY) && !defined(__crayx1) )

#define __REAL	REAL
#define __float	float
#define __NCHPWD	8

#define __int	int
#define __NCHINT	8

#define __POINTER	INTEGER
#define __NCHPTR	8

#define __FORTTYPE
#endif


#if ( (sgi && mips) || defined(DEC_ALPHA) || defined(__crayx1) )

#ifdef D_PRECISION
#define __REAL	REAL*8
#define __float	double
#define __NCHPWD	8

#else
#define __REAL	REAL
#define __float	float
#define __NCHPWD	4
#endif

#define __int	int
#define __NCHINT	4

#if ( defined(MIPS4) || _MIPS_SZPTR==64 || defined(DEC_ALPHA) || defined(__crayx1) )
#define __POINTER	INTEGER*8
#define __NCHPTR	8

#else
#define __POINTER	INTEGER
#define __NCHPTR	4
#endif

#define __FORTTYPE
#endif


#ifndef __FORTTYPE

#ifdef D_PRECISION
#define __REAL	REAL*8
#define __float	double
#define __NCHPWD	8

#else
#define __REAL	REAL
#define __float	float
#define __NCHPWD	4
#endif

#define __int	int
#define __NCHINT	4

#define __POINTER	INTEGER
#define __NCHPTR	4

#define __FORTTYPE
#endif

#endif

