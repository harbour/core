/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://harbour-project.org
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

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"
#include "hbapierr.h"

#if QT_VERSION >= 0x040500

#include <QtCore/QTextCodec>

/*----------------------------------------------------------------------*/

static HB_GARBAGE_FUNC( Q_release )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->ph )
      p->func( p );
}

static const HB_GC_FUNCS QT_gcFuncs =
{
   Q_release,
   hb_gcDummyMark
};

const HB_GC_FUNCS * hbqt_gcFuncs( void )
{
   return &QT_gcFuncs;
}

static void * hbqt_gcpointerFromItem( PHB_ITEM pObj )
{
   HBQT_GC_T * p;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointerFromItem( %p )", pObj ) );

   p = ( HBQT_GC_T * ) hb_itemGetPtrGC( pObj, hbqt_gcFuncs() );

   if( p && p->ph )
      return p->ph;
   else if( hb_itemGetPtr( pObj ) )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointerFromItem(): returns raw pointer: %p", hb_itemGetPtr( pObj ) ) );
      return hb_itemGetPtr( pObj ); /* TOFIX: In what cases is this needed? Reference counting to avoid referring to freed pointers? */
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointerFromItem(): returns NULL" ) );
      return NULL; /* TODO: Still better if RTE. */
   }
}

void * hbqt_par_obj( int iParam )
{
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_obj( %d )", iParam ) );

   if( ( pItem = hb_param( iParam, HB_IT_OBJECT ) ) != NULL )
   {
      HBQT_GC_T * p;
      void * pr;

      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pItem );
      hb_vmSend( 0 );

      pItem = hb_param( -1, HB_IT_POINTER );

      p = ( HBQT_GC_T * ) hb_itemGetPtrGC( pItem, hbqt_gcFuncs() );

      if( p && p->ph )
         return p->ph;
#if 1
      else if( ( pr = hb_itemGetPtr( pItem ) ) != NULL )
         return pr; /* TOFIX: Only required in QList.cpp hb_retptr() calls. Though the latter should be fixed, rather than this code. */
#endif
      else
         hb_errRT_BASE( EG_ARG, 8001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else if( HB_ISPOINTER( iParam ) )
   {
      return hb_parptr( iParam );
   }
   else
   {
      //hb_errRT_BASE( EG_ARG, 8000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      HBQT_GC_T * p;
      p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), iParam );
      if( p && p->ph )
      {
         return p->ph;
      }
   }

   return NULL;
}

void * hbqt_gcpointer( int iParam )
{
#if 0
   return hbqt_par_obj( iParam );
#else
   HBQT_GC_T * p;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointer( %d )", iParam ) );

   p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), iParam );

   if( p && p->ph )
   {
      return p->ph;
   }
   else if( HB_ISPOINTER( iParam ) )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointer(): returns RAW pointer: %p", hb_parptr( iParam ) ) );
      return hb_parptr( iParam ); /* TOFIX: In what cases is this needed? Reference counting to avoid referring to freed pointers? */
   }
   else if( HB_ISOBJECT( iParam ) )
   {
      PHB_ITEM pObj = hb_param( iParam, HB_IT_ANY );

      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pObj );
      hb_vmSend( 0 );
      return hbqt_gcpointer( -1 );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcpointer(): returns NULL" ) );
      return NULL; /* TODO: Still better if RTE. */
   }
#endif
}

void * hbqt_pPtrFromObj( int iParam )
{
   PHB_ITEM pObj;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_pPtrFromObj( %d )", iParam ) );

   pObj = hb_param( iParam, HB_IT_ANY );

   if( hb_itemType( pObj ) == HB_IT_OBJECT )
   {
      hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( "PPTR" ) ) );
      hb_vmPush( pObj );
      hb_vmSend( 0 );

      return hbqt_gcpointer( -1 );
   }
   else if( hb_itemType( pObj ) == HB_IT_POINTER )
      return hbqt_gcpointer( iParam );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "hbqt_pPtrFromObj(): returns NULL" ) );
      return NULL; /* TODO: Still better if RTE. */
   }
}

HB_FUNC( __HBQT_SETCODECFORCSTRINGS )
{
   QTextCodec * codec = QTextCodec::codecForName( ( char * ) hb_parc( 1 ) );
   QTextCodec::setCodecForCStrings( codec );
}

HB_FUNC( __HBQT_SETCODECFORTR )
{
   QTextCodec * codec = QTextCodec::codecForName( ( char * ) hb_parc( 1 ) );
   QTextCodec::setCodecForTr( codec );
}

/*----------------------------------------------------------------------*/

#endif                  // #if QT_VERSION >= 0x040500
