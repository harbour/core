/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Win32 DLL handling function (Xbase++ compatible + proprietary)
 *
 * Copyright 2006 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 2002 Vic McClung <vicmcclung@vicmcclung.com>
 * www - http://www.vicmcclung.com
 * Borland mods by ptucker@sympatico.ca
 * MinGW support by Phil Krylov <phil a t newstar.rinet.ru>
 *
 * www - http://www.harbour-project.org; http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
*/

/*
 * Notes:
 *
 * I'm not totally familiar with how Xbase++ works.  This functionality was
 * derived from the context in which the functions are used.
 *
 * pt
 *
 */

#define _WIN32_WINNT   0x0400

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbapicls.h"

#if !defined( HB_NO_ASM ) && defined( HB_OS_WIN_32 )

typedef struct _XPP_DLLEXEC
{
   DWORD     dwType;       /* type info */
   char *    cDLL;         /* DLL */
   HINSTANCE hDLL;         /* Handle */
   char *    cProc;        /* Ordinal or Name */
   WORD      wOrdinal;
   DWORD     dwFlags;      /* Calling Flags */
   LPVOID    lpFunc;
} XPP_DLLEXEC, * PXPP_DLLEXEC;

#ifdef __XHARBOUR__

static PHB_DYNS pHB_CSTRUCTURE = NULL, pPOINTER, pVALUE, pBUFFER, pDEVALUE;

HB_EXTERN_BEGIN
HB_EXPORT char * hb_parcstruct( int iParam, ... );
HB_EXTERN_END

HB_EXPORT char * hb_parcstruct( int iParam, ... )
{
   HB_THREAD_STUB_ANY

   HB_TRACE(HB_TR_DEBUG, ("hb_parcstruct(%d, ...)", iParam));

   if( pHB_CSTRUCTURE == NULL )
   {
      pHB_CSTRUCTURE = hb_dynsymFind( "HB_CSTRUCTURE" );

      pPOINTER       = hb_dynsymGetCase( "POINTER" );
      pVALUE         = hb_dynsymGetCase( "VALUE" );
      pBUFFER        = hb_dynsymGetCase( "BUFFER" );
      pDEVALUE       = hb_dynsymGetCase( "DEVALUE" );
   }

   if( ( iParam >= 0 && iParam <= hb_pcount() ) || ( iParam == -1 ) )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bRelease = FALSE;

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) && ! HB_IS_OBJECT( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;
         PHB_ITEM pArray = pItem;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItem = hb_itemNew( NULL );
         bRelease = TRUE;

         hb_arrayGet( pArray, ulArrayIndex, pItem );
      }

      if( strncmp( hb_objGetClsName( pItem ), "C Structure", 11 ) == 0 )
      {
         hb_vmPushDynSym( pVALUE );
         hb_vmPush( pItem );
         hb_vmSend( 0 );

         if( bRelease )
            hb_itemRelease( pItem );

         return hb_itemGetCPtr( hb_stackReturnItem() );
      }
   }

   return NULL;
}

#endif

/* ==================================================================
 * DynaCall support comments below
 * ------------------------------------------------------------------
 *
 *   This part used modified code of Vic McClung.
 *   The modifications were to separate the library
 *   loading and getting the procedure address
 *   from the actual function call.
 *   The parameters have been slightly re-arranged
 *   to allow for C-like syntax, on function
 *   declaration. The changes allow to load the library
 *   and to get the procedure addresses in advance,
 *   which makes it work similarly to C import libraries.
 *   From experience, when using dynamic libraries, loading
 *   the library and getting the address of the procedure
 *   part of using the DLL.
 *   Additionally the changes will allow to use standard
 *   xHarbour C type defines, as used with structure types,
 *   ande defined in cstruct.ch .
 *
 *   Andrew Wos.
 *   20/07/2002.
 *
 */

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

#define CTYPE_VOID               0
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
   };
   void *      pArg;             /* Pointer to argument */
} DYNAPARM;

#pragma pack()

RESULT DynaCall( int iFlags,      LPVOID lpFunction, int nArgs,
                 DYNAPARM Parm[], LPVOID pRet,       int nRetSiz )
{
   /* Call the specified function with the given parameters. Build a
      proper stack and take care of correct return value processing. */
   RESULT  Res = { 0 };
   int     i, nInd, nSize, nLoops;
   DWORD   dwEAX, dwEDX, dwVal, * pStack, dwStSize = 0;
   BYTE *  pArg;

   #if defined( __MINGW32__ )
   #elif defined( __BORLANDC__ ) || defined(__DMC__)
   #else
      DWORD *pESP;
   #endif

   /* Reserve 256 bytes of stack space for our arguments */
   #if defined( __MINGW32__ )
      asm volatile( "\tmovl %%esp, %0\n"
                    "\tsubl $0x100, %%esp\n"
                    : "=r" (pStack) );
   #elif defined( __BORLANDC__ ) || defined(__DMC__)
      pStack = (DWORD *)_ESP;
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
      nInd  = (nArgs - 1) - i;
      /* Start at the back of the arg ptr, aligned on a DWORD */
      nSize = (Parm[nInd].nWidth + 3) / 4 * 4;
      pArg  = (BYTE *) Parm[nInd].pArg + nSize - 4;
      dwStSize += (DWORD) nSize; /* Count no of bytes on stack */

      nLoops = ( nSize / 4 ) - 1;

      while( nSize > 0 )
      {
         /* Copy argument to the stack */
         if( Parm[nInd].dwFlags & DC_FLAG_ARGPTR )
         {
            /* Arg has a ptr to a variable that has the arg */
            dwVal = (DWORD) pArg; /* Get first four bytes */
            pArg -= 4;            /* Next part of argument */
         }
         else
         {
            /* Arg has the real arg */
            dwVal = *( (DWORD *)( (BYTE *) ( &( Parm[nInd].dwArg ) ) + ( nLoops * 4 ) ) );
         }

         /* Do push dwVal */
         pStack--;          /* ESP = ESP - 4 */
         *pStack = dwVal;   /* SS:[ESP] = dwVal */
         nSize -= 4;
         nLoops--;
      }
   }

   if ((pRet != NULL) && ((iFlags & DC_BORLAND) || (nRetSiz > 8)))
   {
      /* Return value isn't passed through registers, memory copy
         is performed instead. Pass the pointer as hidden arg. */
      dwStSize += 4;          /* Add stack size */
      pStack--;               /* ESP = ESP - 4 */
      *pStack = (DWORD)pRet;  /* SS:[ESP] = pMem */
   }
   #if defined( __MINGW32__ )
      asm volatile( "\taddl $0x100, %%esp\n" /* Restore to original position */
                    "\tsubl %2, %%esp\n"     /* Adjust for our new parameters */

                    /* Stack is now properly built, we can call the function */
                    "\tcall *%3\n"
                    : "=a" (dwEAX), "=d" (dwEDX) /* Save eax/edx registers */
                    : "r" (dwStSize), "r" (lpFunction) );

      /* Possibly adjust stack and read return values. */
      if (iFlags & DC_CALL_CDECL)
      {
         asm volatile( "\taddl %0, %%esp\n" : : "r" (dwStSize) );
      }

      if (iFlags & DC_RETVAL_MATH4)
      {
         asm volatile( "\tfstps (%0)\n" : "=r" (Res) );
      }
      else if (iFlags & DC_RETVAL_MATH8)
      {
         asm volatile( "\tfstpl (%0)\n" : "=r" (Res) );
      }
      else if (pRet == NULL)
      {
         Res.Int = dwEAX;
         (&Res.Int)[1] = dwEDX;
      }
      else if (((iFlags & DC_BORLAND) == 0) && (nRetSiz <= 8))
      {
         /* Microsoft optimized less than 8-bytes structure passing */
         ((int *)pRet)[0] = dwEAX;
         ((int *)pRet)[1] = dwEDX;
      }
   #elif defined( __BORLANDC__ ) || defined(__DMC__)
      _ESP += (0x100 - dwStSize);
      _EDX =  (DWORD) &lpFunction;
      __emit__(0xff,0x12); /* call [edx]; */
      dwEAX = _EAX;
      dwEDX = _EDX;

      /* Possibly adjust stack and read return values. */
      if (iFlags & DC_CALL_CDECL)
      {
         _ESP += dwStSize;
      }

      if (iFlags & DC_RETVAL_MATH4)
      {
         _EBX = (DWORD) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
         __emit__(0xd9,0x1b);   /*     _asm fnstp float ptr [ebx] */
      }
      else if (iFlags & DC_RETVAL_MATH8)
      {
         _EBX = (DWORD) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
         __emit__(0xdd,0x1b);   /*     _asm fnstp qword ptr [ebx] */
      }
      else if (pRet == NULL)
      {
         _EBX = (DWORD) &Res;
         _EAX = dwEAX;
         _EDX = dwEDX;
/*       _asm mov DWORD PTR [ebx], eax */
/*       _asm mov DWORD PTR [ebx + 4], edx */
         __emit__(0x89,0x03,0x89,0x53,0x04);
      }
      else if (((iFlags & DC_BORLAND) == 0) && (nRetSiz <= 8))
      {
         _EBX = (DWORD) pRet;
         _EAX = dwEAX;
         _EDX = dwEDX;
/*       _asm mov DWORD PTR [ebx], eax */
/*       _asm mov DWORD PTR [ebx + 4], edx */
         __emit__(0x89,0x03,0x89,0x53,0x04);
      }
   #else
      _asm add esp, 0x100       /* Restore to original position */
      _asm sub esp, dwStSize    /* Adjust for our new parameters */

      /* Stack is now properly built, we can call the function */
      _asm call [lpFunction]

      _asm mov dwEAX, eax       /* Save eax/edx registers */
      _asm mov dwEDX, edx       /* */

      /* Possibly adjust stack and read return values. */
      if (iFlags & DC_CALL_CDECL)
      {
         _asm add esp, dwStSize
      }

      if (iFlags & DC_RETVAL_MATH4)
      {
         _asm fstp dword ptr [Res]
      }
      else if (iFlags & DC_RETVAL_MATH8)
      {
         _asm fstp qword ptr [Res]
      }
      else if (pRet == NULL)
      {
         _asm mov eax, [dwEAX]
         _asm mov DWORD PTR [Res], eax
         _asm mov edx, [dwEDX]
         _asm mov DWORD PTR [Res + 4], edx
      }
      else if (((iFlags & DC_BORLAND) == 0) && (nRetSiz <= 8))
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

   return Res;
}

/*
 * ==================================================================
 */

/* Based originally on CallDLL() from What32 */
static void DllExec( int iFlags, int iRtype, LPVOID lpFunction, int iParams, int iFirst, int iArgCnt, PXPP_DLLEXEC xec )
{
   int iCnt = 0;
   int i;
   DYNAPARM   Parm[ 15 ];
   RESULT     rc;

   if( xec )
   {
      iFlags     = xec->dwFlags;
      lpFunction = xec->lpFunc;

      /* TODO: Params maybe explictly specified in xec! */
   }

   iFlags &= 0x00ff;  /* Calling Convention */

   if( iRtype == 0 )
      iRtype = CTYPE_UNSIGNED_LONG;

   memset( Parm, 0, sizeof( Parm ) );

   if( iArgCnt > 0 )
   {
      for( i = iFirst; i <= iParams; i++ )
      {
         switch( hb_parinfo( i ) & ~HB_IT_BYREF )
         {
            case HB_IT_NIL:
               Parm[iCnt].nWidth = sizeof( void * );
               Parm[iCnt].dwArg = (DWORD) NULL;
               break;

            case HB_IT_POINTER:
               Parm[iCnt].nWidth = sizeof( void * );
               Parm[iCnt].dwArg = (DWORD) hb_parptr( i );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[iCnt].pArg   = &( Parm[iCnt].dwArg );
                  Parm[iCnt].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }
               break;

            case HB_IT_INTEGER:
            case HB_IT_LONG:
            case HB_IT_DATE:
            case HB_IT_LOGICAL:
               Parm[iCnt].nWidth = sizeof( DWORD );
               Parm[iCnt].dwArg = (DWORD) hb_parnl( i );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[iCnt].pArg   = &( Parm[iCnt].dwArg );
                  Parm[iCnt].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }
               break;

            case HB_IT_DOUBLE:
               Parm[iCnt].nWidth = sizeof( double );
               Parm[iCnt].dArg = hb_parnd( i );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[iCnt].nWidth = sizeof( void * );
                  Parm[iCnt].pArg   = &( Parm[iCnt].dArg );
                  Parm[iCnt].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               }

               iFlags |= DC_RETVAL_MATH8;
               break;

            case HB_IT_STRING:
            case HB_IT_MEMO:
               Parm[iCnt].nWidth = sizeof( void * );

               if( hb_parinfo( i ) & HB_IT_BYREF )
               {
                  Parm[iCnt].pArg = hb_xgrab( hb_parclen( i ) );
                  memcpy( Parm[iCnt].pArg, hb_parc( i ), hb_parclen( i ) );
               }
               else
               {
                  Parm[iCnt].pArg = (void *) hb_parc( i );
               }

               Parm[iCnt].dwFlags = DC_FLAG_ARGPTR;  /* use the pointer */
               break;

#ifdef __XHARBOUR__
            case HB_IT_ARRAY:
               if( strncmp( hb_objGetClsName( hb_param(i, HB_IT_ANY ) ), "C Structure", 11 ) == 0 )
               {
                  Parm[iCnt].nWidth = sizeof(  void * );
                  Parm[iCnt].dwArg = (DWORD) hb_parcstruct( i );
                  break;
               }
#endif

            case HB_IT_HASH:
            case HB_IT_SYMBOL:
            case HB_IT_ALIAS:
            case HB_IT_MEMOFLAG:
            case HB_IT_BLOCK:
            case HB_IT_MEMVAR:

            default:
               MessageBox( GetActiveWindow(), "Parameter error: Unknown parameter type", "hbw32 - DllCall()", MB_OK | MB_ICONERROR );
               return;
         }

         iCnt++;
      }
   }

   /* SetLastError(0); */
   rc = DynaCall( iFlags, lpFunction, iArgCnt, Parm, NULL, 0 );

   /*
   if( GetLastError() )
   {
      LPVOID lpMsgBuf;

      FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER |
      FORMAT_MESSAGE_FROM_SYSTEM,
      NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPTSTR) &lpMsgBuf,
      0, NULL );

      MessageBox( GetActiveWindow(), (LPCSTR) lpMsgBuf, "hbw32 - DynaCall() failure", MB_OK | MB_ICONERROR );

      LocalFree(lpMsgBuf);
   }
   */

   if( iArgCnt > 0 )
   {
      iCnt = 0;

      for( i = iFirst; i <= iParams; i++ )
      {
         if( hb_parinfo( i ) & HB_IT_BYREF )
         {
            switch( hb_parinfo( i ) & ~HB_IT_BYREF )
            {
               case HB_IT_NIL:
                  hb_stornl( Parm[iCnt].dwArg, i );
                  break;

               case HB_IT_POINTER:
                  hb_storptr( (void *) Parm[iCnt].dwArg, i );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DATE:
               case HB_IT_LOGICAL:
                  hb_stornl( Parm[iCnt].dwArg, i );
                  break;

               case HB_IT_DOUBLE:
                  hb_stornd( Parm[iCnt].dArg, i );
                  break;

               case HB_IT_STRING:
               case HB_IT_MEMO:
                  if( !hb_storclen_buffer( (char *) Parm[iCnt].pArg, hb_parclen( i ), i ) )
                     hb_xfree( Parm[iCnt].pArg );
                  break;

#ifdef __XHARBOUR__
               case HB_IT_ARRAY:
                  if( strncmp( hb_objGetClsName( hb_param(i, HB_IT_ANY ) ), "C Structure", 11 ) == 0 )
                  {
                     hb_vmPushDynSym( pDEVALUE );
                     hb_vmPush( hb_param( i, HB_IT_ANY ) );
                     hb_vmSend(0);

                     break;
                  }
#endif

               default:
                  MessageBox( GetActiveWindow(), "Parameter error: Unknown parameter type (return)", "hbw32 - DllCall()", MB_OK | MB_ICONERROR );
                  return;
            }
         }

         iCnt++;
      }
   }

   /* return the correct value */
   switch( iRtype )
   {
      case CTYPE_BOOL:
         hb_retl( ( BOOL ) rc.Long );
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
         hb_retnl( rc.Long );
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
         MessageBox( GetActiveWindow(), "Parameter error: Unknown return type", "hbw32 - DllCall()", MB_OK | MB_ICONERROR );
         break;
   }
}

/* ------------------------------------------------------------------ */

#define EXEC_DLL               0x45584543

static HB_GARBAGE_FUNC( _DLLUnload )
{
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) Cargo;

   if( xec->dwType == EXEC_DLL )
   {
      if( xec->cDLL )
      {
         if( xec->hDLL != ( HINSTANCE ) 0 )
            FreeLibrary( xec->hDLL );

         hb_xfree( xec->cDLL );
      }

      if( xec->cProc )
         hb_xfree( xec->cProc );

      xec->dwType = 0;
   }
}

HB_FUNC( DLLPREPARECALL )
{
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) hb_gcAlloc( sizeof( XPP_DLLEXEC ), _DLLUnload );

   memset( xec, 0, sizeof( XPP_DLLEXEC ) );

   if( ISCHAR( 1 ) )
   {
      xec->cDLL = hb_strdup( hb_parc( 1 ) );
      xec->hDLL = LoadLibrary( xec->cDLL );
   }
   else if( ISNUM( 1 ) )
      xec->hDLL = ( HINSTANCE ) hb_parnl( 1 );

   xec->dwFlags = ISNUM( 2 ) ? hb_parnl( 2 ) : DC_CALL_STD;

   if( xec->hDLL )
   {
      if( ISCHAR( 3 ) )
      {
         /* Not a typo - reserving space for possible Ansi 'A' suffix! */
         xec->cProc = ( char * ) hb_xgrab( hb_parclen( 3 ) + 2 );
         hb_strncpy( xec->cProc, hb_parc( 3 ), hb_parclen( 3 ) );
      }
      else if( ISNUM( 3 ) )
         xec->wOrdinal = hb_parni( 3 );
   }
   else
   {
      if( xec->cDLL )
         MessageBox( GetActiveWindow(), "DllPrepareCall() -> LoadLibrary() failed", xec->cDLL, MB_OK | MB_ICONERROR );
      else
         MessageBox( GetActiveWindow(), "Invalid handle argument", "hbw32 - DllPrepareCall()", MB_OK | MB_ICONERROR );
   }

   xec->dwType = EXEC_DLL;
   xec->lpFunc = ( LPVOID ) GetProcAddress( xec->hDLL, xec->cProc != NULL ? (LPCSTR) xec->cProc : (LPCSTR) xec->wOrdinal );

   if( xec->lpFunc == NULL && xec->cProc )
   {
      /* try ANSI flavour ? */
      xec->cProc[ hb_parclen( 3 ) ] = 'A';
      xec->cProc[ hb_parclen( 3 ) + 1 ] = '\0';

      xec->lpFunc = ( LPVOID ) GetProcAddress( xec->hDLL, xec->cProc != NULL ? (LPCSTR) xec->cProc : (LPCSTR) xec->wOrdinal );
   }

   if( xec->hDLL && xec->lpFunc )
   {
       hb_retptrGC( xec );
   }
   else if( xec->hDLL && xec->lpFunc == NULL )
   {
      if( xec->cProc )
      {
         LPVOID lpMsgBuf;

         FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER |
         FORMAT_MESSAGE_FROM_SYSTEM,
         NULL,
         GetLastError(),
         MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
         (LPTSTR) &lpMsgBuf,
         0, NULL );

         MessageBox( GetActiveWindow(), (LPCSTR) lpMsgBuf, "hbw32 - DllPrepareCall():GetProcAddress() failed", MB_OK | MB_ICONERROR );

         LocalFree(lpMsgBuf);
      }
      else
         MessageBox( GetActiveWindow(), "GetProcAddress() invalid ordinal argument", "hbw32 - DllPrepareCall()", MB_OK | MB_ICONERROR );
   }
}

HB_FUNC( DLLLOAD )
{
   hb_retnl( ( DWORD ) LoadLibraryA( ( LPCSTR ) hb_parcx( 1 ) ) ) ;
}

HB_FUNC( DLLUNLOAD )
{
   hb_retl( FreeLibrary( ( HMODULE ) hb_parnl( 1 ) ) ) ;
}

HB_FUNC( DLLEXECUTECALL )
{
   PXPP_DLLEXEC xec = ( PXPP_DLLEXEC ) hb_parptr( 1 );

   if( xec && 
       xec->dwType == EXEC_DLL &&
       xec->hDLL && 
       xec->lpFunc )
   {
      int iParams = hb_pcount();

      DllExec( 0, 0, NULL, iParams, 2, iParams - 1, xec );
   }
}

HB_FUNC( DLLCALL )
{
   BOOL lUnload;

   HINSTANCE hInst;
   LPVOID lpFunction;

   if( ISCHAR( 1 ) )
   {
      hInst = LoadLibrary( hb_parc( 1 ) );
      lUnload = TRUE;
   }
   else
   {
      hInst = ( HINSTANCE ) hb_parnl( 1 );
      lUnload = FALSE;
   }

   if( hInst && ( DWORD ) hInst >= 32 )
   {
      if( ( lpFunction = ( LPVOID ) GetProcAddress( ( HMODULE ) hInst, ISCHAR( 3 ) ? ( LPCSTR ) hb_parc( 3 ) :
                                                                                     ( LPCSTR ) hb_parni( 3 ) ) ) == 0 )
      {
         if( ISCHAR( 3 ) )
         {
            /* try forced ANSI flavour? */
            char cFuncName[ MAX_PATH ];
            hb_strncpy( cFuncName, hb_parc( 3 ), sizeof( cFuncName ) - 2 );
            strcat( cFuncName, "A" );
            lpFunction = (LPVOID) GetProcAddress( (HMODULE) hInst, cFuncName );
         }
      }
      
      if( lpFunction )
      {
         int iParams = hb_pcount();
      
         DllExec( hb_parni( 2 ), 0, lpFunction, iParams, 4, iParams - 3, NULL );
      }
      
      if( lUnload )
         FreeLibrary( hInst );
   }
}

/* ------------------------------------------------------------------ */

HB_FUNC( LOADLIBRARY ) /* compatibility */
{
   HB_FUNC_EXEC( DLLLOAD );
}

HB_FUNC( FREELIBRARY ) /* compatibility */
{
   HB_FUNC_EXEC( DLLUNLOAD );
}

HB_FUNC( GETLASTERROR )
{
   hb_retnl( GetLastError() );
}

HB_FUNC( SETLASTERROR )
{
   hb_retnl( GetLastError() );
   SetLastError( hb_parnl( 1 ) );
}

HB_FUNC( GETPROCADDRESS )
{
   LPVOID lpFunction;

   if( ( lpFunction = ( LPVOID ) GetProcAddress( ( HMODULE ) hb_parnl( 1 ),
                                    ISCHAR( 2 ) ? ( LPCSTR ) hb_parc( 2 ) :
                                                  ( LPCSTR ) hb_parni( 2 ) ) ) == 0 )
   {
      if( ISCHAR( 2 ) )
      {
         /* try ANSI flavour */
         char cFuncName[ MAX_PATH ];
         hb_strncpy( cFuncName, hb_parc( 2 ), sizeof( cFuncName ) - 2 );
         strcat( cFuncName, "A" );
         lpFunction = ( LPVOID ) GetProcAddress( ( HMODULE ) hb_parnl( 1 ), cFuncName );
      }
   }

   hb_retptr( lpFunction );
}

/* Call a DLL function from (x)Harbour, the first parameter is a pointer returned from
   GetProcAddress above. Note that it is hardcoded to use PASCAL calling convention. */
HB_FUNC( CALLDLL )
{
   LPVOID lpFunction = ( LPVOID ) hb_parptr( 1 );

   if( lpFunction )
   {
      int iParams = hb_pcount();

      DllExec( DC_CALL_STD, 0, lpFunction, iParams, 2, iParams - 1, NULL );
   }
}

HB_FUNC( CALLDLLBOOL )
{
   LPVOID lpFunction = ( LPVOID ) hb_parptr( 1 );

   if( lpFunction )
   {
      int iParams = hb_pcount();

      DllExec( DC_CALL_STD, CTYPE_BOOL, lpFunction, iParams, 2, iParams - 1, NULL );
   }
}

#endif /* HB_OS_WIN32 */
