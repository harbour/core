/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      A Contribution from Andy Wos
 *                                   .
 *                            A Big Thank You
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 * Calback pointer interface
 * ( to be used with wincback.prg )
 * 6 June 2004, 13 June 2004
 * 5 April  2005 - optional (void) spec added
 * January  2006 - used VirtualAlloc to overcome DEP
 * February 2006 - reworked using Przemek's brilliant ideas
 *                 prg no longer required.
 */

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"

extern void hb_ToOutDebug( const char * sTraceMsg, ... );

#define MAX_FUNC_SIZE 128 /* this must be higher than the largest possible generated  */
                          /* machine code plus size of CALLBACKDATA structure         */
                          /* 128 bytes is now more than enough.                       */

/* my virtual memory management structures: */

/* callback function pointer and status */
typedef struct _FuncData {
   LPTSTR pFunc;        /* the actual function pointer */
   BOOL   bActive;      /* active flag                 */
} FuncData, *pFuncData;

/* page reservation and commitments */
typedef struct _PageData {
   LPTSTR    lpPage;    /* pointer to this page            */
   BOOL      bCommited; /* pages commitment status         */
   pFuncData Functions; /* pointer to "array" of functions */
} PageData, *pPageData;

/* granular reservations */
typedef struct _MemReservation {
   LPVOID    lpvBase ; /* reserved area                    */
   pPageData MemPages; /* pointer to "array" of pages      */
} MemReservation, *pMemReservation;

/* callback info */
/*(stored inside the callback function memory block beyond the function code) */
typedef struct CALLBACKDATA {
   PHB_DYNS pDynSym;
   PHB_ITEM pSelf;
   int      iFormalParams;
   int      iCargoParams;
   PHB_ITEM * pParams;
   BOOL     bVoid;
} CALLBACKDATA, * PCALLBACKDATA;

static void   _udp( BYTE * pCode, ULONG ulOffset, void * Address );                /* absolute */
static void   _ucp( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext );  /* relative */
static LPVOID FuncMemAlloc( void );
static BOOL   FuncMemFree( LPBYTE pMem );
static LPVOID _GenerateCallback( CALLBACKDATA * pCallback );

DWORD dwPageSize           = 0;
DWORD dwMinAlloc           = 0;
DWORD dwMinReserve         = 0;
DWORD dwPagesInAlloc       = 0;
DWORD dwFuncsInPage        = 0;
DWORD dwReservedLen        = 0;

pMemReservation pAllAllocs = NULL;

/*----------------------------------------------------------------------*/
/*
   prepare the callback structure and save the xHarbour symbols

   params: pbcFunc, oObj, nParams, lVoid, cargo_params...
*/
HB_FUNC( _ASCALLBACK )
{
   CALLBACKDATA Callback;

   LPVOID pMem;

   int i, iParam;

   if( HB_ISBLOCK( 1 ) )
   {
      Callback.pDynSym = hb_dynsymGet( "EVAL" );
      Callback.pSelf   = hb_itemNew( hb_param( 1, HB_IT_BLOCK ) );
   }
   else
   {
      if( HB_ISOBJECT( 2 ) )
         Callback.pSelf = hb_itemNew( hb_param( 2, HB_IT_OBJECT ) );
      else
         Callback.pSelf = NULL;

      if( HB_ISCHAR( 1 ) )
         Callback.pDynSym = hb_dynsymGet( hb_parc( 1 ) );
      else if( HB_ISPOINTER( 1 ) )
         Callback.pDynSym = ( ( PHB_SYMB ) hb_parptr( 1 ) )->pDynSym;
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1, NULL, "AsCallback", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         hb_retnl( 0 );
         return;
      }
   }

   if( ! Callback.pDynSym ) /* is it actually an error? */
   {
      hb_errRT_BASE_SubstR( EG_ARG, 2, NULL, "AsCallback", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      hb_retnl( 0 );
      return;
   }

   Callback.iFormalParams = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 4 ;      /* default to 4 formal parameters */
   Callback.bVoid         = HB_ISLOG( 4 ) ? hb_parl ( 4 ) : FALSE;   /* default to non-void function   */
   Callback.iCargoParams  = hb_pcount() - 4;

   Callback.pParams = ( PHB_ITEM * ) hb_xgrab( Callback.iCargoParams * sizeof( PHB_ITEM ) );
   iParam = 5; /* i + 1; */
   for( i = 0; i < Callback.iCargoParams; ++i )
   {
      Callback.pParams[ i ] = hb_itemNew( hb_param( i + iParam, HB_IT_ANY ) );
   }

   hb_retnint( ( HB_PTRDIFF ) ( pMem = _GenerateCallback( &Callback ) ) );

   /* debugging only             */
   /* to see what was generated  */
   if ( HB_ISBYREF( 5 ) )
      hb_storclen( ( char * ) pMem, 128, 5 );
}
/*----------------------------------------------------------------------*/
/*
 * the actual processing of the callbacks
 */
LRESULT __CallbackDispatcher( PCALLBACKDATA pCallback, ... )
{
   int i;
   long lResult;

   /* save state? */
   hb_vmPushState();

   hb_vmPushSymbol( hb_dynsymSymbol( pCallback->pDynSym ) );
   if( pCallback->pSelf )
      hb_vmPush( pCallback->pSelf );
   else
      hb_vmPushNil();

   /* first push the formal parameters */
   if ( pCallback->iFormalParams )
   {
      va_list va;
      va_start( va, pCallback );

      for( i = 1; i <= pCallback->iFormalParams; i++ )
      {
          hb_vmPushLong( va_arg( va, DWORD ) );
      }
      va_end( va );
   }

   /* then push cargo params */
   for( i = 0; i < pCallback->iCargoParams; ++i )
   {
      hb_vmPush( pCallback->pParams[ i ] );
   }

   /* execute */
   if( pCallback->pSelf )
      hb_vmSend( ( USHORT ) ( pCallback->iFormalParams + pCallback->iCargoParams ) );
   else
      hb_vmDo( ( USHORT ) ( pCallback->iFormalParams + pCallback->iCargoParams ) );

   lResult = hb_parnl( -1 ) ;

   hb_vmPopState();

   return lResult;
}
/*----------------------------------------------------------------------*/
#if 0
/*i tylko jedna wersje funkcji callback od dynamicznej allokacji: */

LRESULT CALLBACK CallbackTestFunc( LONG hWnd, long nMsg, WPARAM wParam, LPARAM lParam )
{
   int iHandle = 0;  /* to podmieniasz na numer hanldera */
                     /* own address */

   return CallbackDispacher( iHandle, hWnd, nMsg, wParam, lParam );
}

/*Zas CallbackDispacher() wygladalby tak:*/

#endif

/*

  purpose of this function:
  - allocate enough memory for the function itself and the CALLBACK structure
  - store pointer to self (to read the callback structure)
  - pass the above pointer to the dispatcher plus the formal parameters
  - either return a value, or void

The generated code is equivalent to:
------------------------------------

LRESULT CALLBACK GeneratedCode( formal params.... )
{
  PCALLBACKDATA pCallback = [actual real address]

  return __CallbackDispatcher( pCallback, formal params );
}

 void and no void generate the same code, so, it does not matter
 except in the universal handler

 if no formal params expected the code is very simple

 68 .. .. .. .. // push long (constant)
 E8 .. .. .. .. // call function
 59             // pop
 C3             // return
 90 90 90       // padding


 if parameters are passed, there is a prolog,

 55
 8B EC

 pushing params back to front
 4 bytes per parameter:

 1: 8B
 2: 45->55->4D->45...
 3: 08+(nParams-nCurrentParam )*4 // 1 based
 4: 50->52->51->50...

 push long constant
 68 .. .. .. .. // push long (constant)

 call function
 E8 .. .. .. .. // call function

 clean up after the function call
 83 C4 08 // where 08 is param count dependent -> as per first param

 and epilog+padding
 5D
 C2 04 00 // where 04 is paramater count dependent
 90 90 90
*/

static LPVOID _GenerateCallback( CALLBACKDATA * pCallback )
{
   byte *        pMem;
   PCALLBACKDATA pCallbackRecord;
   int           iOffset;
   int           i;
   int           iParVal1;
   int           iParVal2;

   pMem = ( byte * ) FuncMemAlloc();

   if( pMem )
   {
      pCallbackRecord = ( PCALLBACKDATA ) ( pMem + MAX_FUNC_SIZE - sizeof( CALLBACKDATA )-1 );
      memcpy( pCallbackRecord, pCallback, sizeof( CALLBACKDATA ) );

      if ( pCallback->iFormalParams == 0 )
      {

         BYTE pFuncBody[] = { 0x68, 0x00, 0x00, 0x00, 0x00,    /* push long (constant)         */
                              0xE8, 0x00, 0x00, 0x00, 0x00,    /* call function                */
                              0x59,                            /* pop cx                       */
                              0xC3, 0x90, 0x90, 0x90 };        /* return                       */
                              /* size: 15 */

         memcpy( pMem, pFuncBody, 15 );
         _udp( pMem, 1, pCallbackRecord ) ;                     /* update callbackdata pointer */
         _ucp( pMem, 6, ( void * ) __CallbackDispatcher, 10 );  /* update code pointer         */

      }
      else
      {
         BYTE pFuncProlog[] = { 0x55,                           /* push bp               */
                                0x8B, 0xEC };                   /* mov bp,sp             */

         BYTE pFuncEpilog[] = { 0x68, 0x00, 0x00, 0x00, 0x00,   /* push long (constant)  */
                                0xE8, 0x00, 0x00, 0x00, 0x00,   /* call function         */
                                0x83, 0xC4, 0x00,
                                0x5D,                           /* pop bp                */
                                0xC2, 0x00, 0x00,
                                0x90, 0x90, 0x90 };             /* size: 20              */

         BYTE pParamData[]  = { 0x8B, 0x00, 0x00, 0x00 };

         memcpy( pMem, pFuncProlog, 3 );
         iOffset = 3;

         /* add formal parameters */

         iParVal1 = 0x45;
         iParVal2 = 0x50;
         for ( i = 1; i <= pCallback->iFormalParams; i++ )
         {
            pParamData[1] = ( BYTE ) iParVal1;
            pParamData[2] = ( BYTE ) ( 0x08 + ( ( pCallback->iFormalParams - i )* 4 ) );
            pParamData[3] = ( BYTE ) iParVal2;
            memcpy( pMem+iOffset, pParamData, 4 );
            iOffset += 4;

            iParVal1 -= 8;
            if ( iParVal1 < 0x45 )
               iParVal1 = 0x55;

            iParVal2 --;
            if ( iParVal2 < 0x50 )
               iParVal2 = 0x52;
         }

         memcpy( pMem+iOffset, pFuncEpilog, 20 );
         _udp( pMem, iOffset+1, pCallbackRecord ) ;                    /* update callbackdata pointer */
         _ucp( pMem, iOffset+6, ( void * ) __CallbackDispatcher, iOffset+ 10 );   /* update code pointer         */
         pMem[ iOffset+12 ] = ( BYTE ) ( 0x08 + ( ( pCallback->iFormalParams - 1 )* 4 ) );
         pMem[ iOffset+15 ] = ( BYTE ) ( pCallback->iFormalParams * 4 );
      }
   }
   return pMem;
}

/*----------------------------------------------------------------------*/

HB_FUNC( _FREECALLBACK )
{
   LPBYTE ptr = ( LPBYTE ) ( HB_PTRDIFF ) hb_parnint( 1 );

   hb_retl( FuncMemFree( ptr ) );
   return;
}

/*----------------------------------------------------------------------*/
/*
 * Intel specific ?? Patch an address relative to the next instruction
 */
static void _ucp( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

   ulBase = ( ULONG ) pCode + ( ULONG ) ulNext;
   /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ( ULONG ) ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );
}
/*----------------------------------------------------------------------*/
/*
 * Patch an address of the dynamic function
 */
static void _udp( BYTE * pCode, ULONG ulOffset, void * Address )
{
   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );
}
/*----------------------------------------------------------------------*/
/*
 * allocate memory for a function
 * it is assumed that in the worst case scenario the function body requires
 * 256 bytes. The tests shown that it is in fact 159 bytes, but it has been
 * increased to be on the safe side and provide for future expansion of the
 * code.
 *
 * on my PC:
 * dwPageSize              = 4096
 * dwAllocationGranularity = 65536
 *
 * one page may contain 4096/256 = 16 callback functions
 * one min allocation can contain 65536/4096 = 16 pages, and 16*16 = 256 callback functions
 *
 * Note to self:
 * it may be required to remove READWRITE flag for systems above Win9*
 * use VirtualProtect to change the comitted memory protection scheme
 * Note: VirtualProtect is not supported on Win9* systems
 */
/*----------------------------------------------------------------------*/

static LPVOID FuncMemAlloc( void )
{

   SYSTEM_INFO      sSysInfo;         /* useful information about the system */
   LPVOID           lpvBase;
   DWORD            i,j,k,l;
   BOOL             bFound;
   char *           lpPage;
   BOOL             bError;
   LPVOID           lpReturn;

   if( dwPageSize == 0 )
   {
      GetSystemInfo( &sSysInfo );     /* populate the system information structure */
      dwPageSize     = sSysInfo.dwPageSize;
      dwMinAlloc     = sSysInfo.dwAllocationGranularity;
      dwMinReserve   = max( dwPageSize, dwMinAlloc );
      dwPagesInAlloc = dwMinReserve/dwPageSize;
      dwFuncsInPage  = dwPageSize/MAX_FUNC_SIZE;
   }

   k            = 0;
   j            = 0;
   bFound       = FALSE;
   bError       = FALSE;
   lpReturn     = NULL;

   /* first time called? */
   if( pAllAllocs == NULL )
   {
     /* allocate space for the structure */
     pAllAllocs = ( MemReservation* ) malloc( sizeof( MemReservation ) );

     /* reserve the minimum */
     lpvBase = VirtualAlloc(
                             NULL,                 /* system selects address  */
                             dwMinReserve,         /* size of allocation      */
                             MEM_RESERVE,          /* allocate reserved pages */
                             PAGE_NOACCESS );      /* protection = no access  */

     pAllAllocs->lpvBase  = lpvBase;               /* save the reservation    */
     pAllAllocs->MemPages = ( PageData* ) malloc( dwPagesInAlloc * sizeof( PageData ) );

     for( l = 0 ; l < dwPagesInAlloc ; l++ )
     {
        ( ( pAllAllocs )->MemPages+l )->bCommited = FALSE;
     }
     dwReservedLen = 1;
     j = 0;
     k = 0;

   }

   /* find empty slot */
   for( i = 0 ; i < dwReservedLen ; i++ )                  /* each reservation   */
   {
      lpPage = ( char * ) ( pAllAllocs+i )->lpvBase;

      for( j = 0 ; j < dwPagesInAlloc ; j++ )              /* each reserved page */
      {
         if( ( ( pAllAllocs+i )->MemPages+j )->bCommited ) /* if committed       */
         {
            for( k = 0 ; k < dwFuncsInPage ; k++ )
            {
               if( ! ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive ) /* function slot not active */
               {
                   bFound = TRUE;
                   break;
               }
            }
            lpPage += dwPageSize;
         }
         else /* found uncommited page */
         {
            lpvBase = VirtualAlloc(
                                     lpPage,                   /* system selects address  */
                                     dwPageSize,               /* size of allocation      */
                                     MEM_COMMIT,               /* allocate reserved pages */
                                     PAGE_EXECUTE_READWRITE ); /* protection = no access  */
            if( lpvBase == NULL )
            {
               /* error commiting mem page */
               bError = TRUE;
               break;
            }

            ( ( pAllAllocs+i )->MemPages+j )->lpPage    = ( LPTSTR ) lpvBase;
            ( ( pAllAllocs+i )->MemPages+j )->bCommited = TRUE;
            ( ( pAllAllocs+i )->MemPages+j )->Functions = ( FuncData* ) malloc( dwFuncsInPage * sizeof( FuncData ) );

            for( l = 0 ; l < dwFuncsInPage ; l++ )
            {
               ( ( ( pAllAllocs+i )->MemPages+j )->Functions+l )->bActive = FALSE;
               ( ( ( pAllAllocs+i )->MemPages+j )->Functions+l )->pFunc   =
                                               ( LPTSTR ) ( ( ( LPSTR ) lpvBase) + ( l*MAX_FUNC_SIZE ) );
            }

            j--; /* to repeat */
         }

         if( bFound || bError )
         {
            break;
         }
      }

      if( bError || bFound )
      {
         break;
      }

      /* need to reseve more memory (allocate another block) */
      if( i == dwReservedLen-1 )
      {
         /* allocate space for the structure */
         pAllAllocs = ( MemReservation* ) realloc( pAllAllocs, ( dwReservedLen+1 ) * sizeof( MemReservation ) );

         /* reserve the minimum */
         lpvBase = VirtualAlloc(
                             NULL,                 /* system selects address  */
                             dwMinReserve,         /* size of allocation      */
                             MEM_RESERVE,          /* allocate reserved pages */
                             PAGE_NOACCESS );      /* protection = no access  */

         if( lpvBase == NULL )
         {
            /* error commiting mem page */
            bError = TRUE;
            break;
         }

         ( pAllAllocs+i+1 )->lpvBase  = lpvBase;   /* save the reservation    */
         ( pAllAllocs+i+1 )->MemPages = ( PageData* ) malloc( dwPagesInAlloc * sizeof( PageData ) );

         for( l = 0 ; l < dwPagesInAlloc ; l++ )
         {
            ( ( pAllAllocs+i+1 )->MemPages+l )->bCommited = FALSE;
         }
         dwReservedLen++;
      }
   }

   if( bError )
   {
   }
   else if( bFound )
   {
      ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive = TRUE;
      lpReturn = ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->pFunc;
   }

   return lpReturn;
}
/*----------------------------------------------------------------------*/
/*
 * free the allocated CALLBACKDATA structure
 */
static void FreeCallbackRecord( PCALLBACKDATA pCallback )
{
   int i;

   if ( pCallback->pSelf )
      hb_itemRelease( pCallback->pSelf );

   if ( pCallback->iCargoParams )
   {
      for ( i = 0; i < pCallback->iCargoParams; ++i )
      {
         hb_itemRelease( pCallback->pParams[ i ] );
      }
      hb_xfree( pCallback->pParams );
   }
}
/*----------------------------------------------------------------------*/

static BOOL FuncMemFree( LPBYTE pMem )
{
   DWORD     i,j,k;
   BOOL      bSuccess = FALSE;

   for( i = 0 ; i < dwReservedLen ; i++ )                 /* each reservation   */
   {
      for( j = 0 ; j < dwPagesInAlloc ; j++ )             /* each reserved page */
      {
         if( ( (pAllAllocs+i )->MemPages+j )->bCommited ) /* if committed       */
         {
            for( k = 0 ; k < dwFuncsInPage ; k++ )
            {
               if( ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive ) /* function slot active */
               {
                  if( pMem == ( LPBYTE ) ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->pFunc )
                  {
                     FreeCallbackRecord( ( PCALLBACKDATA ) ( pMem + MAX_FUNC_SIZE - sizeof( CALLBACKDATA )-1 ) );
                     ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive = FALSE;
                     bSuccess = TRUE;
                  }
               }
            }
         }
      }
   }
   return bSuccess;
}
/*----------------------------------------------------------------------*/
/*
 * according to MSDN all memory is freed automatically
 * perhaps this function is not required at all?
 */
static void FuncMemFreeAll( void )
{
   DWORD     i,j,k;
   LPBYTE    pMem;

   if( pAllAllocs != NULL )
   {
      for( i = 0 ; i < dwReservedLen ; i++ )                  /* each reservation   */
      {
         for( j = 0 ; j < dwPagesInAlloc ; j++ )              /* each reserved page */
         {
            if( ( ( pAllAllocs+i )->MemPages+j )->bCommited ) /* if committed       */
            {
               for( k = 0 ; k < dwFuncsInPage ; k++ )
               {
                  if( ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive ) /* function slot not active */
                  {
                     pMem = ( LPBYTE ) ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->pFunc;
                     FreeCallbackRecord( ( PCALLBACKDATA ) ( pMem + MAX_FUNC_SIZE - sizeof( CALLBACKDATA )-1 ) );
                     ( ( ( pAllAllocs+i )->MemPages+j )->Functions+k )->bActive = FALSE;
                  }
               }
               free( ( ( pAllAllocs+i )->MemPages+j )->Functions );
               ( ( pAllAllocs+i )->MemPages+j )->bCommited = FALSE;
            }
         }
         free( ( pAllAllocs+i )->MemPages );
         VirtualFree( ( pAllAllocs+1 )->lpvBase, 0, MEM_RELEASE );
      }
      free( pAllAllocs );
      pAllAllocs = NULL;
      dwReservedLen = 0;
   }
   return;
}
/*----------------------------------------------------------------------*/
/*
 * as exit procedure
 */
HB_FUNC( _FREEALLCALLBACKS )
{
   FuncMemFreeAll();
}
/*----------------------------------------------------------------------*/
