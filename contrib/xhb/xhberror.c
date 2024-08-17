/*
 * The default error handler
 *
 * Copyright 2023 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapicls.h"
#include "hbvm.h"
#include "hbstack.h"

#include "xhb.h"

#define XHB_ERROR_RESIZE_OBJECT
#define XHB_ERROR_OVERLOAD_ERRORNEW

HB_FUNC_EXTERN( XHB_ERRORNEW );

static HB_SIZE s_nErrObjSize = 0;
static HB_SIZE s_nErrProcName = 0;
static HB_SIZE s_nErrProcLine = 0;
static HB_SIZE s_nErrProcModule = 0;
static HB_SIZE s_nErrCallStack = 0;

static HB_SYMB s_symXhbErrorNew = { "XHB_ERRORNEW", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( XHB_ERRORNEW ) }, NULL };
#ifdef XHB_ERROR_OVERLOAD_ERRORNEW
   static HB_SYMB s_symErrorNew = { "ERRORNEW"    , { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( XHB_ERRORNEW ) }, NULL };
#endif

static void s_xhbErrorResize( PHB_ITEM pError )
{
   hb_arraySize( pError, s_nErrObjSize );
   hb_arraySetCConst( pError, s_nErrProcName, NULL );
   hb_arraySetNI( pError, s_nErrProcLine, 0 );
   hb_arraySetCConst( pError, s_nErrProcModule, NULL );
   hb_arrayNew( hb_arrayGetItemPtr( pError, s_nErrCallStack ), 0 );
}

const char * hb_errGetProcName( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetProcName(%p)", pError ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   return hb_arrayGetCPtr( pError, s_nErrProcName );
}

PHB_ITEM hb_errPutProcName( PHB_ITEM pError, const char * szProcName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutProcName(%p, %s)", pError, szProcName ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   hb_arraySetC( pError, s_nErrProcName, szProcName );

   return pError;
}

HB_UINT hb_errGetProcLine( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetProcLine(%p)", pError ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   return ( HB_USHORT ) hb_arrayGetNI( pError, s_nErrProcLine );
}

PHB_ITEM hb_errPutProcLine( PHB_ITEM pError, HB_UINT uiProcLine )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutProcLine(%p, %u)", pError, uiProcLine ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   hb_arraySetNI( pError, s_nErrProcLine, uiProcLine );

   return pError;
}

const char * hb_errGetModuleName( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetModuleName(%p)", pError ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   return hb_arrayGetCPtr( pError, s_nErrProcModule );
}

PHB_ITEM hb_errPutModuleName( PHB_ITEM pError, const char * szModuleName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutModuleName(%p, %s)", pError, szModuleName ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   hb_arraySetC( pError, s_nErrProcModule, szModuleName );

   return pError;
}

PHB_ITEM hb_errGetCallStack( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetCallStack(%p)", pError ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   return hb_arrayGetItemPtr( pError, s_nErrCallStack );
}

PHB_ITEM hb_errPutCallStack( PHB_ITEM pError, PHB_ITEM pCallStack )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutCallStack(%p, %p)", pError, pCallStack ) );

   if( hb_arrayLen( pError ) < s_nErrProcName )
      s_xhbErrorResize( pError );

   hb_arraySet( pError, s_nErrCallStack, pCallStack );

   return pError;
}

HB_FUNC_STATIC( PROCNAME )
{
   hb_retc( hb_errGetProcName( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _PROCNAME )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutProcName( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( PROCLINE )
{
   hb_retni( hb_errGetProcLine( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _PROCLINE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_NUMERIC( pItem ) )
      hb_errPutProcLine( hb_stackSelfItem(), ( HB_UINT ) hb_itemGetNI( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( MODULENAME )
{
   hb_retc( hb_errGetModuleName( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _MODULENAME )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutModuleName( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( AASTACK )
{
   hb_itemReturn( hb_errGetCallStack( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _AASTACK )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_ARRAY( pItem ) )
      hb_errPutCallStack( hb_stackSelfItem(), pItem );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( ERRORINIT )
{
   PHB_ITEM pError = hb_stackSelfItem();

   if( s_nErrObjSize != 0 )
   {
      PHB_ITEM pStack = hb_itemArrayNew( 0 ), pItem = NULL;
      char szProcName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
      char szProcFile[ HB_PATH_MAX ];
      HB_USHORT uiProcLine;
      int iLevel = 0;

      /* start with 1 instead of 0 to skip this method */
      while( hb_procinfo( ++iLevel, szProcName, &uiProcLine, szProcFile ) )
      {
         if( ! pItem )
         {
            if( strcmp( szProcName, "ERRORNEW" ) == 0 ||
                strcmp( szProcName, "XHB_ERRORNEW" ) == 0 ||
                strcmp( szProcName, "__ERRRT_BASE" ) == 0 ||
                strcmp( szProcName, "__ERRRT_SBASE" ) == 0 )
               continue;
            hb_errPutProcName( pError, szProcName );
            hb_errPutProcLine( pError, uiProcLine );
            hb_errPutModuleName( pError, szProcFile );
            pItem = hb_itemArrayNew( 3 );
         }
         else
            hb_arrayNew( pItem, 3 );

         hb_arraySetC( pItem, 1, szProcFile );
         hb_arraySetC( pItem, 2, szProcName );
         hb_arraySetNI( pItem, 3, ( int ) uiProcLine );
         hb_arrayAddForward( pStack, pItem );
      }
      hb_errRelease( pItem );
      hb_errPutCallStack( pError, pStack );
      hb_errRelease( pStack );
   }

   hb_itemReturn( pError );
}

HB_FUNC( XHB_ERRORNEW )
{
   PHB_ITEM pError = hb_errNew();

   if( HB_ISCHAR( 1 ) )
      hb_errPutSubSystem( pError, hb_parc( 1 ) );

   if( HB_ISNUM( 2 ) )
      hb_errPutGenCode( pError, ( HB_ERRCODE ) hb_parni( 2 ) );

   if( HB_ISNUM( 3 ) )
      hb_errPutSubCode( pError, ( HB_ERRCODE ) hb_parni( 3 ) );

   if( HB_ISCHAR( 4 ) )
      hb_errPutOperation( pError, hb_parc( 4 ) );

   if( HB_ISCHAR( 5 ) )
      hb_errPutDescription( pError, hb_parc( 5 ) );

   if( HB_ISARRAY( 6 ) )
      hb_errPutArgsArray( pError, hb_param( 6, HB_IT_ARRAY ) );

   if( HB_ISCHAR( 7 ) )
      hb_errPutModuleName( pError, hb_parc( 7 ) );

   if( HB_ISCHAR( 8 ) )
      hb_errPutProcName( pError, hb_parc( 8 ) );

   if( HB_ISNUM( 9 ) )
      hb_errPutProcLine( pError, hb_parni( 9 ) );

   hb_itemReturnRelease( pError );
}

static void xhb_errRedefineClass( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_nErrObjSize == 0 )
   {
      PHB_ITEM pError = hb_errNew();
      HB_USHORT usClassH = hb_objGetClass( pError );

      s_nErrObjSize = hb_arrayLen( pError );

      s_nErrProcName = ++s_nErrObjSize;
      hb_clsAdd( usClassH, "PROCNAME"   , HB_FUNCNAME( PROCNAME )    );
      hb_clsAdd( usClassH, "_PROCNAME"  , HB_FUNCNAME( _PROCNAME )   );

      s_nErrProcLine = ++s_nErrObjSize;
      hb_clsAdd( usClassH, "PROCLINE"   , HB_FUNCNAME( PROCLINE )    );
      hb_clsAdd( usClassH, "_PROCLINE"  , HB_FUNCNAME( _PROCLINE )   );

      s_nErrProcModule = ++s_nErrObjSize;
      hb_clsAdd( usClassH, "MODULENAME" , HB_FUNCNAME( MODULENAME )  );
      hb_clsAdd( usClassH, "_MODULENAME", HB_FUNCNAME( _MODULENAME ) );

      s_nErrCallStack = ++s_nErrObjSize;
      hb_clsAdd( usClassH, "AASTACK"    , HB_FUNCNAME( AASTACK )     );
      hb_clsAdd( usClassH, "_AASTACK"   , HB_FUNCNAME( _AASTACK )    );

      hb_clsAdd( usClassH, "INIT"       , HB_FUNCNAME( ERRORINIT )   );

#ifdef XHB_ERROR_RESIZE_OBJECT
      s_xhbErrorResize( pError );
      hb_errReinit( pError );
#else
      hb_errReinit( NULL );
#endif

#ifdef XHB_ERROR_OVERLOAD_ERRORNEW
      {
         PHB_DYNS pDynSym = hb_dynsymFind( "ERRORNEW" );
         if( pDynSym )
         {
            s_symErrorNew.pDynSym = pDynSym;
            pDynSym->pSymbol = &s_symErrorNew;
            hb_vmSetDynFunc( pDynSym );
         }
      }
#endif

      hb_errRelease( pError );
   }
}

HB_CALL_ON_STARTUP_BEGIN( _xhb_error_init_ )
   hb_dynsymNew( &s_symXhbErrorNew );
   hb_vmAtInit( xhb_errRedefineClass, NULL );
HB_CALL_ON_STARTUP_END( _xhb_error_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _xhb_error_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _xhb_error_init_ )
   #include "hbiniseg.h"
#endif
