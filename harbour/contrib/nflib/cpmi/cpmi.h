/*
* $Id$
*/

/*
 * File......: CPMI.H
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *    Rev 1.0   01 Jan 1995 03:01:00   TED
 *    Initial release
 *
 */

//  This is the header file for the Clipper Protected Mode Interface.

#define SUCCEED             1
#define FAIL                0
#define TRUE                1
#define FALSE               0

#define AR_READ             1          // For cpmiWillGPF() & cpmiMakeAlias()
#define AR_WRITE            2          // For cpmiWillGPF() & cpmiMakeAlias()
#define AR_EXECUTE          4          // For cpmiWillGPF() & cpmiMakeAlias()

#define INVALID_SELECTOR   -1          // Returned by cpmiWillGPF()
#define INVALID_ACCESS     -2          // Returned by cpmiWillGPF()
#define BEYOND_LIMIT       -3          // Returned by cpmiWillGPF()

typedef unsigned int  SELECTOR;
typedef unsigned int  REGISTER;
typedef unsigned int  SEGMENT;
typedef unsigned long LINEAR;
typedef unsigned int  BOOLEAN;

typedef union
{
   REGISTER Regs[ 10 ];
   struct
   {
      REGISTER AX;
      REGISTER BX;
      REGISTER CX;
      REGISTER DX;
      REGISTER SI;
      REGISTER DI;
      REGISTER BP;
      REGISTER DS;
      REGISTER ES;
      REGISTER Flags;
   } Reg;
} CPUREGS;  

BOOLEAN        hb_cpmiIsProtected( void );
int            hb_cpmiWillGPF( void *, unsigned int, unsigned int );
SELECTOR       hb_cpmiAllocateSelector( void );
unsigned int   hb_cpmiFreeSelector( SELECTOR );
SELECTOR       hb_cpmiSegmentToSelector( SEGMENT );
LINEAR         hb_cpmiGetBase( SELECTOR );
unsigned int   hb_cpmiSetBase( SELECTOR, LINEAR );
unsigned int   hb_cpmiGetLimit( SELECTOR );
unsigned int   hb_cpmiSetLimit( SELECTOR, int );
void *         hb_cpmiRealPtr( void * );
SELECTOR       hb_cpmiProtectedPtr( void *, unsigned int );
SELECTOR       hb_cpmiMakeAlias( SELECTOR, unsigned int );
SELECTOR       hb_cpmiAllocateDOSMem( unsigned int );
int            hb_cpmiResizeDOSMem( SELECTOR, unsigned int );
int            hb_cpmiFreeDOSMem( SELECTOR );
int            hb_cpmiInt86( unsigned char, CPUREGS *, CPUREGS * );
int            hb_cpmiFarCallReal( void *, CPUREGS *, CPUREGS * );


