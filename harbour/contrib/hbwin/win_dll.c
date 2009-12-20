/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows DLL handling function (Xbase++ compatible + proprietary)
 *
 * Copyright 2006 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 2002 Vic McClung <vicmcclung@vicmcclung.com>
 * www - http://www.vicmcclung.com
 * Borland mods by ptucker@sympatico.ca
 * MinGW support by Phil Krylov <phil a t newstar.rinet.ru>
 *
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

#if defined( HB_OS_WIN ) && !defined( __CYGWIN__ ) && !defined( HB_NO_ASM )

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

/* Calling conventions */
#define DLL_CDECL                DC_CALL_CDECL
#define DLL_STDCALL              DC_CALL_STD

/* Parameter passing mode */
#define DLL_CALLMODE_NORMAL      0x0000
#define DLL_CALLMODE_COPY        0x2000

#define DC_MICROSOFT             0x0000      /* Default */
#define DC_BORLAND               0x0001      /* Borland compatible */
#define DC_CALL_CDECL            0x0010      /* __cdecl */
#define DC_CALL_STD              0x0020      /* __stdcall */
#define DC_RETVAL_MATH4          0x0100      /* Return value in ST */
#define DC_RETVAL_MATH8          0x0200      /* Return value in ST */

#define DC_CALL_STD_BO           ( DC_CALL_STD | DC_BORLAND )
#define DC_CALL_STD_MS           ( DC_CALL_STD | DC_MICROSOFT )
#define DC_CALL_STD_M8           ( DC_CALL_STD | DC_RETVAL_MATH8 )

#define DC_FLAG_ARGPTR           0x00000002

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

typedef union RESULT
{                                /* Various result types */
   int     Int;                  /* Generic four-byte type */
   long    Long;                 /* Four-byte long */
   void *  Pointer;              /* 32-bit pointer */
   float   Float;                /* Four byte real */
   double  Double;               /* 8-byte real */
   __int64 int64;                /* big int (64-bit) */
} RESULT;

typedef struct DYNAPARM
{
   DWORD       dwFlags;          /* Parameter flags */
   int         nWidth;           /* Byte width */
   union
   {
      BYTE     bArg;             /* 1-byte argument */
      SHORT    usArg;            /* 2-byte argument */
      DWORD    dwArg;            /* 4-byte argument */
      double   dArg;             /* double argument */
   } numargs;
   void *      pArg;             /* Pointer to argument */
} DYNAPARM;

#pragma pack()

RESULT DynaCall( int iFlags, FARPROC lpFunction, int nArgs,
                 DYNAPARM Parm[], LPVOID pRet, int nRetSiz )
{
   /* Call the specified function with the given parameters. Build a
      proper stack and take care of correct return value processing. */
   RESULT  Res = { 0 };
#if defined( HB_OS_WIN_CE ) || defined( HB_OS_WIN_64 )
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
      nSize = ( Parm[ nInd ].nWidth + 3 ) / 4 * 4;
      pArg  = ( BYTE * ) Parm[ nInd ].pArg + nSize - 4;
      dwStSize += ( DWORD ) nSize; /* Count no of bytes on stack */

      nLoops = ( nSize / 4 ) - 1;

      while( nSize > 0 )
      {
         /* Copy argument to the stack */
         if( Parm[ nInd ].dwFlags & DC_FLAG_ARGPTR )
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
      _asm call [lpFunction]

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

/*
 * ==================================================================
 */

typedef struct _XPP_DLLEXEC
{
   HMODULE  hDLL;       /* Handle */
   HB_BOOL  bFreeDLL;   /* Free library handle on destroy? */
   DWORD    dwFlags;    /* Calling Flags */
   FARPROC  lpFunc;     /* Function Address */
} XPP_DLLEXEC, * PXPP_DLLEXEC;

#define _DLLEXEC_MAXPARAM   15

/* Based originally on CallDLL() from What32 */
static void DllExec( int iFlags, int iRtype, FARPROC lpFunction, PXPP_DLLEXEC xec, int iParams, int iFirst )
{
   DYNAPARM Parm[ _DLLEXEC_MAXPARAM ];
   RESULT rc;
   int i, iCnt, iArgCnt;

   if( xec )
   {
      iFlags     = xec->dwFlags;
      lpFunction = xec->lpFunc;

      /* TODO: Params maybe explictly specified in xec! */
   }

   if( ! lpFunction )
      return;

   iArgCnt = iParams - iFirst + 1;

   iFlags &= 0x00ff;  /* Calling Convention */

   if( iRtype == 0 )
      iRtype = CTYPE_UNSIGNED_LONG;

   memset( Parm, 0, sizeof( Parm ) );

   if( iArgCnt > 0 )
   {
      for( i = iFirst, iCnt = 0; i <= iParams && iCnt < _DLLEXEC_MAXPARAM; i++, iCnt++ )
      {
         PHB_ITEM pParam = hb_param( i, HB_IT_ANY );

         switch( HB_ITEM_TYPE( pParam ) )
         {
            case HB_IT_NIL:
               Parm[ iCnt ].nWidth = sizeof( void * );
               /* TOFIX: Store NULL pointer in pointer variable. */
               Parm[ iCnt ].numargs.dwArg = 0;
               break;

            case HB_IT_POINTER:
               Parm[ iCnt ].nWidth = sizeof( void * );
               /* TOFIX: Store pointer in pointer variable. */
               Parm[ iCnt ].numargs.dwArg = ( DWORD ) hb_itemGetPtr( pParam );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dwArg );
                  Parm[ iCnt ].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }
               break;

            case HB_IT_INTEGER:
            case HB_IT_LONG:
            case HB_IT_DATE:
            case HB_IT_LOGICAL:
               /* TOFIX: HB_IT_LONG is 64 bit integer */
               Parm[ iCnt ].nWidth = sizeof( DWORD );
               Parm[ iCnt ].numargs.dwArg = ( DWORD ) hb_itemGetNL( pParam );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dwArg );
                  Parm[ iCnt ].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }
               break;

            case HB_IT_DOUBLE:
               Parm[ iCnt ].nWidth = sizeof( double );
               Parm[ iCnt ].numargs.dArg = hb_itemGetND( pParam );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[ iCnt ].nWidth = sizeof( void * );
                  Parm[ iCnt ].pArg = &( Parm[ iCnt ].numargs.dArg );
                  Parm[ iCnt ].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }

               iFlags |= DC_RETVAL_MATH8;
               break;

            case HB_IT_STRING:
            case HB_IT_MEMO:
               Parm[ iCnt ].nWidth = sizeof( void * );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[ iCnt ].pArg = hb_xgrab( hb_itemGetCLen( pParam ) + 1 );
                  memcpy( Parm[ iCnt ].pArg, hb_itemGetCPtr( pParam ), hb_itemGetCLen( pParam ) + 1 );
               }
               else
               {
                  if( iFlags & DLL_CALLMODE_COPY )
                     pParam = hb_itemUnShareString( pParam );

                  Parm[ iCnt ].pArg = ( void * ) hb_itemGetCPtr( pParam );
               }

               Parm[ iCnt ].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
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

   rc = DynaCall( iFlags, lpFunction, iArgCnt, Parm, NULL, 0 );

   if( iArgCnt > 0 )
   {
      for( i = iFirst, iCnt = 0; i <= iParams && iCnt < _DLLEXEC_MAXPARAM; i++, iCnt++ )
      {
         if( HB_ISBYREF( i ) )
         {
            switch( HB_ITEM_TYPE( hb_param( i, HB_IT_ANY ) ) )
            {
               case HB_IT_NIL:
                  hb_stornl( Parm[ iCnt ].numargs.dwArg, i );
                  break;

               case HB_IT_POINTER:
                  hb_storptr( ( void * ) Parm[ iCnt ].numargs.dwArg, i );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DATE:
               case HB_IT_LOGICAL:
                  hb_stornl( Parm[ iCnt ].numargs.dwArg, i );
                  break;

               case HB_IT_DOUBLE:
                  hb_stornd( Parm[ iCnt ].numargs.dArg, i );
                  break;

               case HB_IT_STRING:
               case HB_IT_MEMO:
                  if( ! hb_storclen_buffer( ( char * ) Parm[ iCnt ].pArg, hb_parclen( i ), i ) )
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
         hb_retni( 0 );
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

/* ------------------------------------------------------------------ */

static HB_GARBAGE_FUNC( _DLLUnload )
{
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) Cargo;

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


static FARPROC hb_getprocaddress( HMODULE hDLL, int iParam )
{
#if defined( HB_OS_WIN_CE )
   void * hStr;
   ULONG ulLen;
   LPCWSTR szProc = hb_parstr_u16( iParam, HB_CDP_ENDIAN_NATIVE, &hStr, &ulLen );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCWSTR ) ( HB_PTRDIFF ) ( hb_parni( iParam ) & 0x0FFFF ) );

   if( ! lpFunction && szProc ) /* try with WIDE suffix? */
   {
      LPWSTR pszProcW = ( LPWSTR ) hb_xgrab( ( ulLen + 2 ) * sizeof( WCHAR ) );
      memcpy( pszProcW, szProc, ulLen * sizeof( WCHAR ) );
      pszProcW[ ulLen++ ] = L'W';
      pszProcW[ ulLen++ ] = 0;
      lpFunction = GetProcAddress( hDLL, pszProcW );
      hb_xfree( pszProcW );
   }
   hb_strfree( hStr );
#else
   const char * szProc = hb_parc( iParam );
   FARPROC lpFunction = GetProcAddress( hDLL, szProc ? szProc :
                  ( LPCSTR ) ( HB_PTRDIFF ) ( hb_parni( iParam ) & 0x0FFFF ) );

   if( ! lpFunction && szProc ) /* try with ANSI suffix? */
   {
      char * pszFuncName = hb_xstrcpy( NULL, szProc, "A", NULL );
      lpFunction = GetProcAddress( hDLL, pszFuncName );
      hb_xfree( pszFuncName );
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
      hb_retl( FreeLibrary( ( HMODULE ) hb_parptr( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retl( FreeLibrary( ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) );
   else
      hb_retl( FALSE );
}

HB_FUNC( GETPROCADDRESS )
{
   HMODULE hDLL;

   if( HB_ISNUM( 1 ) )
      hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );
   else
      hDLL = ( HMODULE ) hb_parptr( 1 );

   hb_retptr( hDLL ? ( void * ) hb_getprocaddress( hDLL, 2 ) : NULL );
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
   HMODULE hDLL = NULL;

   if( HB_ISPOINTER( 1 ) )
      hDLL = ( HMODULE ) hb_parptr( 1 );
   else if( HB_ISNUM( 1 ) )
      hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      void * hFileName;
      hDLL = LoadLibrary( HB_PARSTR( 1, &hFileName, NULL ) );
      hb_strfree( hFileName );
   }

   if( hDLL && ( HB_PTRDIFF ) hDLL >= 32 )
   {
      DllExec( hb_parni( 2 ), 0, hb_getprocaddress( ( HMODULE ) hDLL, 3 ), NULL, hb_pcount(), 4 );

      if( HB_ISCHAR( 1 ) )
         FreeLibrary( hDLL );
   }
}

/* TODO: Add support for UNICODE (*W()) calls. */

HB_FUNC( DLLPREPARECALL )
{
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) hb_gcAllocate( sizeof( XPP_DLLEXEC ), &s_gcDllFuncs );
   const char * pszErrorText;

   memset( xec, 0, sizeof( XPP_DLLEXEC ) );

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
      xec->lpFunc = hb_getprocaddress( xec->hDLL, 3 );
      if( xec->lpFunc )
      {
         xec->dwFlags = HB_ISNUM( 2 ) ? hb_parnl( 2 ) : DC_CALL_STD;
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
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) hb_parptrGC( &s_gcDllFuncs, 1 );

   if( xec && xec->hDLL && xec->lpFunc )
      DllExec( 0, 0, NULL, xec, hb_pcount(), 2 );
}

#endif /* HB_COMPAT_XPP */

/* ------------------------------------------------------------------ */

/* Call a DLL function from Harbour, the first parameter is a pointer returned from
   GetProcAddress() above. Note that it is hardcoded to use PASCAL calling convention. */
HB_FUNC( CALLDLL )
{
   DllExec( DC_CALL_STD, 0, ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 2 );
}

HB_FUNC( CALLDLLBOOL )
{
   DllExec( DC_CALL_STD, CTYPE_BOOL, ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 2 );
}

HB_FUNC( CALLDLLTYPED )
{
   DllExec( DC_CALL_STD, hb_parni( 2 ), ( FARPROC ) hb_parptr( 1 ), NULL, hb_pcount(), 3 );
}

#endif /* HB_OS_WIN && && !__CYGWIN__ !HB_NO_ASM */
