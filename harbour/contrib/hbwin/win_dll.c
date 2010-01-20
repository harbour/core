/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows DLL handling function (Xbase++ compatible + proprietary)
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu) (win64 support)
 * Copyright 2006 Paul Tucker <ptucker@sympatico.ca> (Borland mods)
 * Copyright 2002 Vic McClung <vicmcclung@vicmcclung.com>
 * Copyright 2002 Phil Krylov <phil a t newstar.rinet.ru> (MinGW support)
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: I'm not totally familiar with how Xbase++ works. This functionality
         was derived from the context in which the functions are used. [pt] */

#define HB_OS_WIN_USED

#include "hbvm.h"
#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbwinuni.h"

#if !defined( __CYGWIN__ ) && !defined( HB_NO_ASM )

/* ==================================================================
 * DynaCall support comments below
 * ------------------------------------------------------------------
 *
 *   This part used modified code of Vic McClung.
 *   The modifications were to separate the library loading and
 *   getting the procedure address from the actual function call.
 *   The parameters have been slightly re-arranged to allow for
 *   C-like syntax, on function declaration. The changes allow to
 *   load the library and to get the procedure addresses in advance,
 *   which makes it work similarly to C import libraries. From
 *   experience, when using dynamic libraries, loading the library
 *   and getting the address of the procedure part of using the DLL.
 *   Additionally the changes will allow to use standard [x]Harbour
 *   C type defines, as used with structure types, and defined in
 *   cstruct.ch.
 *
 *   Andrew Wos.
 *   20/07/2002.
 */

/* Call flags */
#define DLL_CALLMODE_NORMAL      0x0000
#define DLL_CALLMODE_COPY        0x2000
#define DC_MICROSOFT             0x0000      /* Default */
#define DC_BORLAND               0x0001      /* Borland compatible */
#define DC_CALL_CDECL            0x0010      /* __cdecl */
#define DC_CALL_STDCALL          0x0020      /* __stdcall */
#define DC_RETVAL_MATH4          0x0100      /* Return value in ST */
#define DC_RETVAL_MATH8          0x0200      /* Return value in ST */
#define DC_UNICODE               0x0400
#define DC_CALL_STDCALL_BO       ( DC_CALL_STDCALL | DC_BORLAND )
#define DC_CALL_STDCALL_MS       ( DC_CALL_STDCALL | DC_MICROSOFT )
#define DC_CALL_STDCALL_M8       ( DC_CALL_STDCALL | DC_RETVAL_MATH8 )

/* Parameter flags */
#define DC_PARFLAG_ARGPTR        0x0002

/* C Types */
#define CTYPE_VOID               9
#define CTYPE_CHAR               1
#define CTYPE_UNSIGNED_CHAR      -1
#define CTYPE_CHAR_PTR           10
#define CTYPE_UNSIGNED_CHAR_PTR  -10
#define CTYPE_SHORT              2
#define CTYPE_UNSIGNED_SHORT     -2
#define CTYPE_SHORT_PTR          20
#define CTYPE_UNSIGNED_SHORT_PTR -20
#define CTYPE_INT                3
#define CTYPE_UNSIGNED_INT       -3
#define CTYPE_INT_PTR            30
#define CTYPE_UNSIGNED_INT_PTR   -30
#define CTYPE_LONG               4
#define CTYPE_UNSIGNED_LONG      -4
#define CTYPE_LONG_PTR           40
#define CTYPE_UNSIGNED_LONG_PTR  -40
#define CTYPE_FLOAT              5
#define CTYPE_FLOAT_PTR          50
#define CTYPE_DOUBLE             6
#define CTYPE_DOUBLE_PTR         60
#define CTYPE_VOID_PTR           7
#define CTYPE_BOOL               8
#define CTYPE_STRUCTURE          1000
#define CTYPE_STRUCTURE_PTR      10000

#pragma pack(1)

typedef union
{                                /* Various result types */
   int     Int;                  /* Generic four-byte type */
   long    Long;                 /* Four-byte long */
   void *  Pointer;              /* 32-bit pointer */
   float   Float;                /* Four byte real */
   double  Double;               /* 8-byte real */
   __int64 int64;                /* big int (64-bit) */
} HB_DYNRETVAL;

typedef struct
{
   int         iParFlags;        /* Parameter flags */
   int         iWidth;           /* Byte width */
   union
   {
      BYTE     bArg;             /* 1-byte argument */
      SHORT    usArg;            /* 2-byte argument */
      DWORD    dwArg;            /* 4-byte argument */
      double   dArg;             /* double argument */
   } numargs;
   void *      pArg;             /* Pointer to argument */
} HB_DYNPARAM;

#pragma pack()

#if ! defined( HB_OS_WIN_64 )

static HB_DYNRETVAL hb_DynaCall( int iFlags, FARPROC lpFunction, int nArgs, HB_DYNPARAM Parm[], void * pRet, int nRetSiz )
{
   /* Call the specified function with the given parameters. Build a
      proper stack and take care of correct return value processing. */
   HB_DYNRETVAL Res = { 0 };
#if defined( HB_OS_WIN_CE )
   HB_SYMBOL_UNUSED( iFlags );
   HB_SYMBOL_UNUSED( lpFunction );
   HB_SYMBOL_UNUSED( nArgs );
   HB_SYMBOL_UNUSED( Parm );
   HB_SYMBOL_UNUSED( pRet );
   HB_SYMBOL_UNUSED( nRetSiz );
#else
   int     i, nInd, nSize, nLoops;
   DWORD   dwEAX, dwEDX, dwVal, * pStack, dwStSize = 0;
   BYTE *  pArg;
#if ! defined( __MINGW32__ ) && ! defined( __BORLANDC__ ) && ! defined( __DMC__ )
   LPVOID  lpFunctionVoid = ( LPVOID ) lpFunction;
#endif

   #if defined( __MINGW32__ )
   #elif defined( __BORLANDC__ ) || defined( __DMC__ )
   #else
      DWORD * pESP;
   #endif

   /* Reserve 256 bytes of stack space for our arguments */
   #if defined( __MINGW32__ )
      asm volatile( "\tmovl %%esp, %0\n"
                    "\tsubl $0x100, %%esp\n"
                    : "=r" (pStack) );
   #elif defined( __BORLANDC__ ) || defined( __DMC__ )
      pStack = ( DWORD * ) _ESP;
      _ESP -= 0x100;
   #else
      _asm mov pStack, esp
      _asm mov pESP, esp
      _asm sub esp, 0x100
   #endif

   /* Push args onto the stack. Every argument is aligned on a
      4-byte boundary. We start at the rightmost argument. */
   for( i = 0; i < nArgs; i++ )
   {
      nInd  = ( nArgs - 1 ) - i;
      /* Start at the back of the arg ptr, aligned on a DWORD */
      nSize = ( Parm[ nInd ].iWidth + 3 ) / 4 * 4;
      pArg  = ( BYTE * ) Parm[ nInd ].pArg + nSize - 4;
      dwStSize += ( DWORD ) nSize; /* Count no of bytes on stack */

      nLoops = ( nSize / 4 ) - 1;

      while( nSize > 0 )
      {
         /* Copy argument to the stack */
         if( Parm[ nInd ].iParFlags & DC_PARFLAG_ARGPTR )
         {
            /* Arg has a ptr to a variable that has the arg */
            dwVal = ( DWORD ) pArg; /* Get first four bytes */
            pArg -= 4;              /* Next part of argument */
         }
         else
         {
            /* Arg has the real arg */
            dwVal = *( ( DWORD * )( ( BYTE * ) ( &( Parm[ nInd ].numargs.dwArg ) ) + ( nLoops * 4 ) ) );
         }

         /* Do push dwVal */
         pStack--;          /* ESP = ESP - 4 */
         *pStack = dwVal;   /* SS:[ESP] = dwVal */
         nSize -= 4;
         nLoops--;
      }
   }

   if( ( pRet != NULL ) && ( ( iFlags & DC_BORLAND ) || ( nRetSiz > 8 ) ) )
   {
      /* Return value isn't passed through registers, memory copy
         is performed instead. Pass the pointer as hidden arg. */
      dwStSize += 4;             /* Add stack size */
      pStack--;                  /* ESP = ESP - 4 */
      *pStack = ( DWORD ) pRet;  /* SS:[ESP] = pMem */
   }
   #if defined( __MINGW32__ )
      asm volatile( "\taddl $0x100, %%esp\n" /* Restore to original position */
                    "\tsubl %2, %%esp\n"     /* Adjust for our new parameters */

                    /* Stack is now properly built, we can call the function */
                    "\tcall *%3\n"
                    : "=a" (dwEAX), "=d" (dwEDX) /* Save eax/edx registers */
                    : "r" (dwStSize), "r" (lpFunction) );

      /* Possibly adjust stack and read return values. */
      if( iFlags & DC_CALL_CDECL )
      {
         asm volatile( "\taddl %0, %%esp\n" : : "r" (dwStSize) );
      }

      if( iFlags & DC_RETVAL_MATH4 )
      {
         asm volatile( "\tfstps (%0)\n" : "=r" (Res) );
      }
      else if( iFlags & DC_RETVAL_MATH8 )
      {
         asm volatile( "\tfstpl (%0)\n" : "=r" (Res) );
      }
      else if( pRet == NULL )
      {
         Res.Int = dwEAX;
         ( &Res.Int )[ 1 ] = dwEDX;
      }
      else if( ( ( iFlags & DC_BORLAND ) == 0 ) && ( nRetSiz <= 8 ) )
      {
         /* Microsoft optimized less than 8-bytes structure passing */
         ( ( int * ) pRet )[ 0 ] = dwEAX;
         ( ( int * ) pRet )[ 1 ] = dwEDX;
      }
   #elif defined( __BORLANDC__ ) || defined( __DMC__ )
      _ESP += ( 0x100 - dwStSize );
      _EDX =  ( DWORD ) &lpFunction;
      __emit__( 0xFF, 0x12 ); /* call [edx]; */
      dwEAX = _EAX;
      dwEDX = _EDX;

      /* Possibly adjust stack and read return values. */
      if( iFlags & DC_CALL_CDECL )
      {
         _ESP += dwStSize;
      }

      if( iFlags & DC_RETVAL_MATH4 )
      {
         _EBX = ( DWORD ) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
         __emit__( 0xD9, 0x1B );   /*     _asm fnstp float ptr [ebx] */
      }
      else if( iFlags & DC_RETVAL_MATH8 )
      {
         _EBX = ( DWORD ) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
         __emit__( 0xDD, 0x1B );   /*     _asm fnstp qword ptr [ebx] */
      }
      else if( pRet == NULL )
      {
         _EBX = ( DWORD ) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
/*       _asm mov DWORD PTR [ebx], eax */
/*       _asm mov DWORD PTR [ebx + 4], edx */
         __emit__( 0x89, 0x03, 0x89, 0x53, 0x04 );
      }
      else if( ( ( iFlags & DC_BORLAND ) == 0 ) && ( nRetSiz <= 8 ) )
      {
         _EBX = ( DWORD ) pRet;
         _EAX = dwEAX;
         _EDX = dwEDX;
/*       _asm mov DWORD PTR [ebx], eax */
/*       _asm mov DWORD PTR [ebx + 4], edx */
         __emit__( 0x89, 0x03, 0x89, 0x53, 0x04 );
      }
   #else
      _asm add esp, 0x100       /* Restore to original position */
      _asm sub esp, dwStSize    /* Adjust for our new parameters */

      /* Stack is now properly built, we can call the function */
      _asm call [lpFunctionVoid]

      _asm mov dwEAX, eax       /* Save eax/edx registers */
      _asm mov dwEDX, edx       /* */

      /* Possibly adjust stack and read return values. */
      if( iFlags & DC_CALL_CDECL )
      {
         _asm add esp, dwStSize
      }

      if( iFlags & DC_RETVAL_MATH4 )
      {
         _asm fstp dword ptr [Res]
      }
      else if( iFlags & DC_RETVAL_MATH8 )
      {
         _asm fstp qword ptr [Res]
      }
      else if( pRet == NULL )
      {
         _asm mov eax, [dwEAX]
         _asm mov DWORD PTR [Res], eax
         _asm mov edx, [dwEDX]
         _asm mov DWORD PTR [Res + 4], edx
      }
      else if( ( ( iFlags & DC_BORLAND ) == 0 ) && ( nRetSiz <= 8 ) )
      {
         /* Microsoft optimized less than 8-bytes structure passing */
         _asm mov ecx, DWORD PTR [pRet]
         _asm mov eax, [dwEAX]
         _asm mov DWORD PTR [ecx], eax
         _asm mov edx, [dwEDX]
         _asm mov DWORD PTR [ecx + 4], edx
      }

      _asm mov esp, pESP
   #endif
#endif

   return Res;
}

#endif

/*
 * ==================================================================
 */

typedef struct
{
   HMODULE  hDLL;       /* Handle */
   HB_BOOL  bFreeDLL;   /* Free library handle on destroy? */
   int      iCallFlags; /* Calling Flags */
   FARPROC  lpFunction; /* Function Address */
} HB_DLLEXEC, * PHB_DLLEXEC;

#define _DLLEXEC_MAXPARAM   15

#if defined( HB_OS_WIN_64 )

typedef struct
{
   void *    hString;
   HB_BOOL   bByRef;
   HB_U64    nValue;
} HB_WINARG;

typedef struct
{
   HB_BOOL     bUNICODE;
   int         iRetType;
   int         iFirst;
   HB_WINARG * pArg;
} HB_WINCALL, * PHB_WINCALL;

static HB_U64 hb_u64par( PHB_WINCALL wcall, int iParam )
{
   PHB_ITEM pParam = hb_param( wcall->iFirst + iParam, HB_IT_ANY );
   HB_U64 r = 0;

   if( pParam )
   {
      switch( HB_ITEM_TYPE( pParam ) )
      {
         case HB_IT_LOGICAL:
            wcall->pArg[ iParam - 1 ].nValue = hb_itemGetL( pParam );
            r = wcall->pArg[ iParam - 1 ].bByRef ? ( HB_PTRUINT ) &wcall->pArg[ iParam - 1 ].nValue : wcall->pArg[ iParam - 1 ].nValue;
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DATE:
            wcall->pArg[ iParam - 1 ].nValue = hb_itemGetNInt( pParam );
            r = wcall->pArg[ iParam - 1 ].bByRef ? ( HB_PTRUINT ) &wcall->pArg[ iParam - 1 ].nValue : wcall->pArg[ iParam - 1 ].nValue;
            break;

         case HB_IT_DOUBLE:
            /* TODO */
            break;

         case HB_IT_STRING:
         case HB_IT_MEMO:
            if( wcall->bUNICODE )
               r = ( HB_PTRUINT ) hb_itemGetStrU16( pParam, HB_CDP_ENDIAN_NATIVE, &wcall->pArg[ iParam - 1 ].hString, NULL );
            else
               r = ( HB_PTRUINT ) hb_itemGetStr( pParam, hb_setGetOSCP(), &wcall->pArg[ iParam - 1 ].hString, NULL );
            wcall->pArg[ iParam - 1 ].nValue = r;
            break;

         case HB_IT_POINTER:
            wcall->pArg[ iParam - 1 ].nValue = ( HB_PTRUINT ) hb_itemGetPtr( pParam );
            r = wcall->pArg[ iParam - 1 ].bByRef ? ( HB_PTRUINT ) &wcall->pArg[ iParam - 1 ].nValue : wcall->pArg[ iParam - 1 ].nValue;
            break;
      }
   }

   return r;
}

static void hb_u64ret( PHB_WINCALL wcall, HB_U64 nValue )
{
   switch( wcall->iRetType )
   {
      case CTYPE_VOID:
         hb_ret();
         break;

      case CTYPE_BOOL:
         hb_retl( nValue != 0 );
         break;

      case CTYPE_CHAR:
      case CTYPE_UNSIGNED_CHAR:
      case CTYPE_SHORT:
      case CTYPE_UNSIGNED_SHORT:
      case CTYPE_INT:
         hb_retni( ( int ) nValue );
         break;

      case CTYPE_LONG:
         hb_retnl( ( long ) nValue );
         break;

      case CTYPE_UNSIGNED_INT:
      case CTYPE_UNSIGNED_LONG:
         hb_retnint( nValue );
         break;

      case CTYPE_CHAR_PTR:
      case CTYPE_UNSIGNED_CHAR_PTR:
         if( wcall->bUNICODE )
            hb_retstr_u16( HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) nValue );
         else
            hb_retstr( hb_setGetOSCP(), ( const char * ) nValue );
         break;

      case CTYPE_INT_PTR:
      case CTYPE_UNSIGNED_SHORT_PTR:
      case CTYPE_UNSIGNED_INT_PTR:
      case CTYPE_STRUCTURE_PTR:
      case CTYPE_LONG_PTR:
      case CTYPE_UNSIGNED_LONG_PTR:
      case CTYPE_VOID_PTR:
      case CTYPE_FLOAT_PTR:
      case CTYPE_DOUBLE_PTR:
         hb_retptr( ( void * ) nValue );
         break;

      case CTYPE_FLOAT:
      case CTYPE_DOUBLE:
         /* TOFIX */
         hb_retnd( 0 );
         break;
   }
}

typedef HB_U64( WINAPI * WIN64_00 ) ( void );
typedef HB_U64( WINAPI * WIN64_01 ) ( HB_U64 );
typedef HB_U64( WINAPI * WIN64_02 ) ( HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_03 ) ( HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_04 ) ( HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_05 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_06 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_07 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_08 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_09 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_10 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_11 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_12 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_13 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_14 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );
typedef HB_U64( WINAPI * WIN64_15 ) ( HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64, HB_U64 );

static HB_U64 win64_00( FARPROC p )                                                                                                                                                                                     { return ( ( WIN64_00 ) *p )(); }
static HB_U64 win64_01( FARPROC p, HB_U64 p01 )                                                                                                                                                                         { return ( ( WIN64_01 ) *p )( p01 ); }
static HB_U64 win64_02( FARPROC p, HB_U64 p01, HB_U64 p02 )                                                                                                                                                             { return ( ( WIN64_02 ) *p )( p01, p02 ); }
static HB_U64 win64_03( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03 )                                                                                                                                                 { return ( ( WIN64_03 ) *p )( p01, p02, p03 ); }
static HB_U64 win64_04( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04 )                                                                                                                                     { return ( ( WIN64_04 ) *p )( p01, p02, p03, p04 ); }
static HB_U64 win64_05( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05 )                                                                                                                         { return ( ( WIN64_05 ) *p )( p01, p02, p03, p04, p05 ); }
static HB_U64 win64_06( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06 )                                                                                                             { return ( ( WIN64_06 ) *p )( p01, p02, p03, p04, p05, p06 ); }
static HB_U64 win64_07( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07 )                                                                                                 { return ( ( WIN64_07 ) *p )( p01, p02, p03, p04, p05, p06, p07 ); }
static HB_U64 win64_08( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08 )                                                                                     { return ( ( WIN64_08 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08 ); }
static HB_U64 win64_09( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09 )                                                                         { return ( ( WIN64_09 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09 ); }
static HB_U64 win64_10( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10 )                                                             { return ( ( WIN64_10 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10 ); }
static HB_U64 win64_11( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10, HB_U64 p11 )                                                 { return ( ( WIN64_11 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11 ); }
static HB_U64 win64_12( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10, HB_U64 p11, HB_U64 p12 )                                     { return ( ( WIN64_12 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12 ); }
static HB_U64 win64_13( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10, HB_U64 p11, HB_U64 p12, HB_U64 p13 )                         { return ( ( WIN64_13 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13 ); }
static HB_U64 win64_14( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10, HB_U64 p11, HB_U64 p12, HB_U64 p13, HB_U64 p14 )             { return ( ( WIN64_14 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14 ); }
static HB_U64 win64_15( FARPROC p, HB_U64 p01, HB_U64 p02, HB_U64 p03, HB_U64 p04, HB_U64 p05, HB_U64 p06, HB_U64 p07, HB_U64 p08, HB_U64 p09, HB_U64 p10, HB_U64 p11, HB_U64 p12, HB_U64 p13, HB_U64 p14, HB_U64 p15 ) { return ( ( WIN64_15 ) *p )( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15 ); }

#endif

static void hb_DllExec( int iCallFlags, int iRtype, FARPROC lpFunction, PHB_DLLEXEC xec, int iParams, int iFirst )
{
   int tmp;

   if( xec )
   {
      iCallFlags = xec->iCallFlags;
      lpFunction = xec->lpFunction;
   }

   if( ! lpFunction )
      return;

   if( iRtype == 0 )
      iRtype = CTYPE_UNSIGNED_LONG;

#if defined( HB_OS_WIN_64 )
   {
      HB_WINCALL wcall;

      wcall.bUNICODE = ( iCallFlags & DC_UNICODE );
      wcall.iRetType = iRtype;
      wcall.iFirst   = iFirst - 1;

      iParams -= wcall.iFirst;

      if( iParams <= _DLLEXEC_MAXPARAM )
      {
         if( iParams )
         {
            wcall.pArg = ( HB_WINARG * ) hb_xgrab( iParams * sizeof( HB_WINARG ) );
            memset( wcall.pArg, 0, iParams * sizeof( HB_WINARG ) );
         }
         else
            wcall.pArg = NULL;

         for( tmp = 0; tmp < iParams; ++tmp )
            wcall.pArg[ tmp ].bByRef = HB_ISBYREF( iFirst + 1 + tmp );

         switch( iParams )
         {
            case  0: hb_u64ret( &wcall, win64_00( lpFunction ) ); break;
            case  1: hb_u64ret( &wcall, win64_01( lpFunction, hb_u64par( &wcall, 1 ) ) ); break;
            case  2: hb_u64ret( &wcall, win64_02( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ) ) ); break;
            case  3: hb_u64ret( &wcall, win64_03( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ) ) ); break;
            case  4: hb_u64ret( &wcall, win64_04( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ) ) ); break;
            case  5: hb_u64ret( &wcall, win64_05( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ) ) ); break;
            case  6: hb_u64ret( &wcall, win64_06( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ) ) ); break;
            case  7: hb_u64ret( &wcall, win64_07( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ) ) ); break;
            case  8: hb_u64ret( &wcall, win64_08( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ) ) ); break;
            case  9: hb_u64ret( &wcall, win64_09( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ) ) ); break;
            case 10: hb_u64ret( &wcall, win64_10( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ) ) ); break;
            case 11: hb_u64ret( &wcall, win64_11( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ), hb_u64par( &wcall, 11 ) ) ); break;
            case 12: hb_u64ret( &wcall, win64_12( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ), hb_u64par( &wcall, 11 ), hb_u64par( &wcall, 12 ) ) ); break;
            case 13: hb_u64ret( &wcall, win64_13( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ), hb_u64par( &wcall, 11 ), hb_u64par( &wcall, 12 ), hb_u64par( &wcall, 13 ) ) ); break;
            case 14: hb_u64ret( &wcall, win64_14( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ), hb_u64par( &wcall, 11 ), hb_u64par( &wcall, 12 ), hb_u64par( &wcall, 13 ), hb_u64par( &wcall, 14 ) ) ); break;
            case 15: hb_u64ret( &wcall, win64_15( lpFunction, hb_u64par( &wcall, 1 ), hb_u64par( &wcall, 2 ), hb_u64par( &wcall, 3 ), hb_u64par( &wcall, 4 ), hb_u64par( &wcall, 5 ), hb_u64par( &wcall, 6 ), hb_u64par( &wcall, 7 ), hb_u64par( &wcall, 8 ), hb_u64par( &wcall, 9 ), hb_u64par( &wcall, 10 ), hb_u64par( &wcall, 11 ), hb_u64par( &wcall, 12 ), hb_u64par( &wcall, 13 ), hb_u64par( &wcall, 14 ), hb_u64par( &wcall, 15 ) ) ); break;
         }

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( wcall.pArg[ tmp ].bByRef )
            {
               switch( HB_ITEM_TYPE( hb_param( iFirst + 1 + tmp, HB_IT_ANY ) ) )
               {
                  case HB_IT_LOGICAL:
                     hb_storl( wcall.pArg[ tmp ].nValue != 0, tmp );
                     break;

                  case HB_IT_NIL:
                  case HB_IT_INTEGER:
                  case HB_IT_LONG:
                  case HB_IT_DATE:
                     hb_stornint( wcall.pArg[ tmp ].nValue, tmp );
                     break;

                  case HB_IT_DOUBLE:
                     /* TOFIX */
                     hb_stornd( 0, tmp );
                     break;

                  case HB_IT_STRING:
                  case HB_IT_MEMO:
                     if( wcall.bUNICODE )
                        hb_storstrlen_u16( HB_CDP_ENDIAN_NATIVE, ( const HB_WCHAR * ) wcall.pArg[ tmp ].nValue, hb_parclen( tmp ), tmp );
                     else
                        hb_storstrlen( hb_setGetOSCP(), ( const char * ) wcall.pArg[ tmp ].nValue, hb_parclen( tmp ), tmp );
                     break;

                  case HB_IT_POINTER:
                     hb_storptr( ( void * ) wcall.pArg[ tmp ].nValue, tmp );
                     break;
               }
            }
         }

         for( tmp = 0; tmp < iParams; ++tmp )
            hb_strfree( wcall.pArg[ tmp ].hString );

         if( wcall.pArg )
            hb_xfree( wcall.pArg );
      }
      else
         hb_errRT_BASE( EG_ARG, 2010, "A maximum of 15 parameters is supported", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
#else
   {
      HB_DYNPARAM Parm[ _DLLEXEC_MAXPARAM ];
      HB_DYNRETVAL rc;
      int iCnt, iArgCnt;

      iArgCnt = iParams - iFirst + 1;

      iCallFlags &= 0x00FF;  /* Calling Convention */

      memset( Parm, 0, sizeof( Parm ) );

      if( iArgCnt > 0 )
      {
         for( tmp = iFirst, iCnt = 0; tmp <= iParams && iCnt < _DLLEXEC_MAXPARAM; ++tmp, ++iCnt )
         {
            PHB_ITEM pParam = hb_param( tmp, HB_IT_ANY );

            switch( HB_ITEM_TYPE( pParam ) )
            {
               case HB_IT_NIL:
                  Parm[ iCnt ].iWidth = sizeof( void * );
                  /* TOFIX: Store NULL pointer in pointer variable. */
                  Parm[ iCnt ].numargs.dwArg = 0;
                  break;

               case HB_IT_POINTER:
                  Parm[ iCnt ].iWidth = sizeof( void * );
                  /* TOFIX: Store pointer in pointer variable. */
                  Parm[ iCnt ].numargs.dwArg = ( DWORD ) hb_itemGetPtr( pParam );

                  if( hb_parinfo( tmp ) & HB_IT_BYREF )
                  {
                     Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dwArg );
                     Parm[ iCnt ].iParFlags = DC_PARFLAG_ARGPTR;  /* use the pointer */
                  }
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DATE:
               case HB_IT_LOGICAL:
                  /* TOFIX: HB_IT_LONG is 64 bit integer */
                  Parm[ iCnt ].iWidth = sizeof( DWORD );
                  Parm[ iCnt ].numargs.dwArg = ( DWORD ) hb_itemGetNL( pParam );

                  if( hb_parinfo( tmp ) & HB_IT_BYREF )
                  {
                     Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dwArg );
                     Parm[ iCnt ].iParFlags = DC_PARFLAG_ARGPTR;  /* use the pointer */
                  }
                  break;

               case HB_IT_DOUBLE:
                  Parm[ iCnt ].iWidth = sizeof( double );
                  Parm[ iCnt ].numargs.dArg = hb_itemGetND( pParam );

                  if( hb_parinfo( tmp ) & HB_IT_BYREF )
                  {
                     Parm[ iCnt ].iWidth = sizeof( void * );
                     Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dArg );
                     Parm[ iCnt ].iParFlags = DC_PARFLAG_ARGPTR;  /* use the pointer */
                  }

                  iCallFlags |= DC_RETVAL_MATH8;
                  break;

               case HB_IT_STRING:
               case HB_IT_MEMO:
                  Parm[ iCnt ].iWidth = sizeof( void * );

                  if( hb_parinfo( tmp ) & HB_IT_BYREF )
                  {
                     Parm[ iCnt ].pArg = hb_xgrab( hb_itemGetCLen( pParam ) + 1 );
                     memcpy( Parm[ iCnt ].pArg, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) + 1 );
                  }
                  else
                  {
                     if( iCallFlags & DLL_CALLMODE_COPY )
                        pParam = hb_itemUnShareString( pParam );

                     Parm[ iCnt ].pArg = ( void * ) hb_itemGetCPtr( pParam );
                  }

                  Parm[ iCnt ].iParFlags = DC_PARFLAG_ARGPTR;  /* use the pointer */
                  break;

               case HB_IT_ARRAY:
               case HB_IT_HASH:
               case HB_IT_SYMBOL:
               case HB_IT_BLOCK:

               default:
                  hb_errRT_BASE( EG_ARG, 2010, "Unknown parameter type to DLL function", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
                  return;
            }
         }
      }

      rc = hb_DynaCall( iCallFlags, lpFunction, iArgCnt, Parm, NULL, 0 );

      if( iArgCnt > 0 )
      {
         for( tmp = iFirst, iCnt = 0; tmp <= iParams && iCnt < _DLLEXEC_MAXPARAM; ++tmp, ++iCnt )
         {
            if( HB_ISBYREF( tmp ) )
            {
               switch( HB_ITEM_TYPE( hb_param( tmp, HB_IT_ANY ) ) )
               {
                  case HB_IT_NIL:
                     hb_stornl( Parm[ iCnt ].numargs.dwArg, tmp );
                     break;

                  case HB_IT_POINTER:
                     hb_storptr( ( void * ) Parm[ iCnt ].numargs.dwArg, tmp );
                     break;

                  case HB_IT_INTEGER:
                  case HB_IT_LONG:
                  case HB_IT_DATE:
                  case HB_IT_LOGICAL:
                     hb_stornl( Parm[ iCnt ].numargs.dwArg, tmp );
                     break;

                  case HB_IT_DOUBLE:
                     hb_stornd( Parm[ iCnt ].numargs.dArg, tmp );
                     break;

                  case HB_IT_STRING:
                  case HB_IT_MEMO:
                     if( ! hb_storclen_buffer( ( char * ) Parm[ iCnt ].pArg, hb_parclen( tmp ), tmp ) )
                        hb_xfree( Parm[ iCnt ].pArg );
                     break;
                  default:
                     hb_errRT_BASE( EG_ARG, 2010, "Unknown reference parameter type to DLL function", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
                     return;
               }
            }
         }
      }

      /* return the correct value */
      switch( iRtype )
      {
         case CTYPE_BOOL:
            hb_retl( rc.Long != 0 );
            break;

         case CTYPE_VOID:
            hb_ret();
            break;

         case CTYPE_CHAR:
         case CTYPE_UNSIGNED_CHAR:
            hb_retni( ( char ) rc.Int );
            break;

         case CTYPE_SHORT:
         case CTYPE_UNSIGNED_SHORT:
            hb_retni( ( int ) rc.Int );
            break;

         case CTYPE_INT:
            hb_retni( ( int ) rc.Long );
            break;

         case CTYPE_LONG:
            hb_retnl( ( long ) rc.Long );
            break;

         case CTYPE_CHAR_PTR:
         case CTYPE_UNSIGNED_CHAR_PTR:
            hb_retc( ( char * ) rc.Long );
            break;

         case CTYPE_UNSIGNED_INT:
         case CTYPE_UNSIGNED_LONG:
            hb_retnint( ( unsigned long ) rc.Long );
            break;

         case CTYPE_INT_PTR:
         case CTYPE_UNSIGNED_SHORT_PTR:
         case CTYPE_UNSIGNED_INT_PTR:
         case CTYPE_STRUCTURE_PTR:
         case CTYPE_LONG_PTR:
         case CTYPE_UNSIGNED_LONG_PTR:
         case CTYPE_VOID_PTR:
         case CTYPE_FLOAT_PTR:
         case CTYPE_DOUBLE_PTR:
            hb_retptr( ( void * ) rc.Long );
            break;

         case CTYPE_FLOAT:
            hb_retnd( rc.Float );
            break;

         case CTYPE_DOUBLE:
            hb_retnd( rc.Double );
            break;

         default:
            hb_errRT_BASE( EG_ARG, 2010, "Unknown return type from DLL function", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
   }

#endif
}

/* ------------------------------------------------------------------ */

static HB_GARBAGE_FUNC( _DLLUnload )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) Cargo;

   if( xec->hDLL && xec->bFreeDLL )
   {
      FreeLibrary( xec->hDLL );
      xec->hDLL = NULL;
   }
}

static const HB_GC_FUNCS s_gcDllFuncs =
{
   _DLLUnload,
   hb_gcDummyMark
};

static FARPROC hb_getprocaddress( HMODULE hDLL, int iParam, HB_BOOL * pbUNICODE )
{
#if defined( HB_OS_WIN_CE )
   void * hStr;
   HB_SIZE nLen;
   LPCWSTR szProc = hb_parstr_u16( iParam, HB_CDP_ENDIAN_NATIVE, &hStr, &nLen );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCWSTR ) ( HB_PTRDIFF ) ( hb_parni( iParam ) & 0x0FFFF ) );

   if( ! lpFunction && szProc ) /* try with WIDE suffix? */
   {
      LPWSTR pszProcW = ( LPWSTR ) hb_xgrab( ( nLen + 2 ) * sizeof( WCHAR ) );
      memcpy( pszProcW, szProc, nLen * sizeof( WCHAR ) );
      pszProcW[ nLen++ ] = L'W';
      pszProcW[ nLen++ ] = 0;
      lpFunction = GetProcAddress( hDLL, pszProcW );
      hb_xfree( pszProcW );
   }
   hb_strfree( hStr );

   if( pbUNICODE )
      *pbUNICODE = HB_FALSE; /* TOFIX: Should be set to HB_TRUE when UNICODE support gets implemented. */
#else
   const char * szProc = hb_parc( iParam );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCSTR ) ( HB_PTRDIFF ) ( hb_parni( iParam ) & 0x0FFFF ) );

   if( pbUNICODE )
      *pbUNICODE = HB_FALSE;

#if defined( HB_OS_WIN_64 ) /* TOFIX: Remove this when UNICODE support gets implemented for non-Win64. */
#if defined( UNICODE )
   if( ! lpFunction && szProc ) /* try with WIDE suffix? */
   {
      char * pszFuncName = hb_xstrcpy( NULL, szProc, "W", NULL );
      lpFunction = GetProcAddress( hDLL, pszFuncName );
      hb_xfree( pszFuncName );
      if( pbUNICODE )
         *pbUNICODE = HB_TRUE;
   }
#endif
#endif

   if( ! lpFunction && szProc ) /* try with ANSI suffix? */
   {
      char * pszFuncName = hb_xstrcpy( NULL, szProc, "A", NULL );
      lpFunction = GetProcAddress( hDLL, pszFuncName );
      hb_xfree( pszFuncName );
      if( pbUNICODE )
         *pbUNICODE = HB_FALSE;
   }
#endif
   return lpFunction;
}

HB_FUNC( LOADLIBRARY )
{
   void * hFileName;

   hb_retnint( ( HB_PTRDIFF ) LoadLibrary( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );

   hb_strfree( hFileName );
}

HB_FUNC( FREELIBRARY )
{
   if( HB_ISPOINTER( 1 ) )
      hb_retl( FreeLibrary( ( HMODULE ) hb_parptr( 1 ) ) ? HB_TRUE : HB_FALSE );
   else if( HB_ISNUM( 1 ) )
      hb_retl( FreeLibrary( ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) ? HB_TRUE : HB_FALSE );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( GETPROCADDRESS )
{
   HMODULE hDLL;

   if( HB_ISNUM( 1 ) )
      hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );
   else
      hDLL = ( HMODULE ) hb_parptr( 1 );

   hb_retptr( hDLL ? ( void * ) hb_getprocaddress( hDLL, 2, NULL ) : NULL );
}

#ifdef HB_COMPAT_XPP

HB_FUNC( DLLLOAD )
{
   HB_FUNC_EXEC( LOADLIBRARY );
}

HB_FUNC( DLLUNLOAD )
{
   HB_FUNC_EXEC( FREELIBRARY );
}

HB_FUNC( DLLCALL )
{
   HB_DLLEXEC xec;

   memset( &xec, 0, sizeof( xec ) );

   if( HB_ISPOINTER( 1 ) )
      xec.hDLL = ( HMODULE ) hb_parptr( 1 );
   else if( HB_ISNUM( 1 ) )
      xec.hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      void * hFileName;
      xec.hDLL = LoadLibrary( HB_PARSTR( 1, &hFileName, NULL ) );
      hb_strfree( hFileName );
   }

   if( xec.hDLL && ( HB_PTRDIFF ) xec.hDLL >= 32 )
   {
      HB_BOOL bUNICODE;
      xec.lpFunction = hb_getprocaddress( ( HMODULE ) xec.hDLL, 3, &bUNICODE );

      xec.iCallFlags = HB_ISNUM( 2 ) ? hb_parni( 2 ) : DC_CALL_STDCALL;
      if( bUNICODE )
         xec.iCallFlags |= DC_UNICODE;

      hb_DllExec( 0, 0, NULL, &xec, hb_pcount(), 4 );

      if( HB_ISCHAR( 1 ) )
         FreeLibrary( xec.hDLL );
   }
}

/* TODO: Add support for UNICODE (*W()) calls. */

HB_FUNC( DLLPREPARECALL )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) hb_gcAllocate( sizeof( HB_DLLEXEC ), &s_gcDllFuncs );
   const char * pszErrorText;

   memset( xec, 0, sizeof( HB_DLLEXEC ) );

   if( HB_ISCHAR( 1 ) )
   {
      void * hFileName;
      xec->hDLL = LoadLibrary( HB_PARSTR( 1, &hFileName, NULL ) );
      hb_strfree( hFileName );
      if( xec->hDLL )
         xec->bFreeDLL = HB_TRUE;
   }
   else if( HB_ISPOINTER( 1 ) )
      xec->hDLL = ( HMODULE ) hb_parptr( 1 );
   else if( HB_ISNUM( 1 ) )
      xec->hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( xec->hDLL )
   {
      HB_BOOL bUNICODE;
      xec->lpFunction = hb_getprocaddress( xec->hDLL, 3, &bUNICODE );
      if( xec->lpFunction )
      {
         xec->iCallFlags = HB_ISNUM( 2 ) ? hb_parni( 2 ) : DC_CALL_STDCALL;
         if( bUNICODE )
            xec->iCallFlags |= DC_UNICODE;
         hb_retptrGC( xec );
         return;
      }
      pszErrorText = HB_ISCHAR( 3 ) ? "Invalid function name" : "Invalid function ordinal";
   }
   else
      pszErrorText = HB_ISCHAR( 1 ) ? "Invalid library name" : "Invalid library handle";

   hb_gcFree( xec );

   hb_errRT_BASE( EG_ARG, 2010, pszErrorText, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DLLEXECUTECALL )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) hb_parptrGC( &s_gcDllFuncs, 1 );

   if( xec && xec->hDLL && xec->lpFunction )
      hb_DllExec( 0, 0, NULL, xec, hb_pcount(), 2 );
}

#endif /* HB_COMPAT_XPP */

/* ------------------------------------------------------------------ */

/* Call a DLL function from Harbour, the first parameter is a pointer returned from
   GetProcAddress() above. Note that it is hardcoded to use PASCAL calling convention. */
HB_FUNC( CALLDLL )
{
   hb_DllExec( DC_CALL_STDCALL, 0, ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 2 );
}

HB_FUNC( CALLDLLBOOL )
{
   hb_DllExec( DC_CALL_STDCALL, CTYPE_BOOL, ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 2 );
}

HB_FUNC( CALLDLLTYPED )
{
   hb_DllExec( DC_CALL_STDCALL, hb_parni( 2 ), ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 3 );
}

#endif /* HB_OS_WIN && && !__CYGWIN__ !HB_NO_ASM */
