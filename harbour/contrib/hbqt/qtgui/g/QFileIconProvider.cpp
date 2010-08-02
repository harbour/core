/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum IconType { Computer, Desktop, Trashcan, Network, ..., File }
 */

#include <QtCore/QPointer>

#include <QtGui/QFileIconProvider>


/* QFileIconProvider ()
 * virtual ~QFileIconProvider ()
 */

typedef struct
{
   QFileIconProvider * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFileIconProvider;

QT_G_FUNC( hbqt_gcRelease_QFileIconProvider )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFileIconProvider   /.\\", p->ph ) );
         delete ( ( QFileIconProvider * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFileIconProvider   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFileIconProvider    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFileIconProvider    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileIconProvider( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QFileIconProvider * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileIconProvider;
   p->type = HBQT_TYPE_QFileIconProvider;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFileIconProvider", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFileIconProvider", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFILEICONPROVIDER )
{
   QFileIconProvider * pObj = NULL;

   pObj = new QFileIconProvider() ;

   hb_retptrGC( hbqt_gcAllocate_QFileIconProvider( ( void * ) pObj, true ) );
}

/*
 * virtual QIcon icon ( IconType type ) const
 */
HB_FUNC( QT_QFILEICONPROVIDER_ICON )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( ( QFileIconProvider::IconType ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEICONPROVIDER_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( ( QFileIconProvider::IconType ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QIcon icon ( const QFileInfo & info ) const
 */
HB_FUNC( QT_QFILEICONPROVIDER_ICON_1 )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( *hbqt_par_QFileInfo( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEICONPROVIDER_ICON_1 FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( *hbqt_par_QFileInfo( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QString type ( const QFileInfo & info ) const
 */
HB_FUNC( QT_QFILEICONPROVIDER_TYPE )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retc( ( p )->type( *hbqt_par_QFileInfo( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEICONPROVIDER_TYPE FP=hb_retc( ( p )->type( *hbqt_par_QFileInfo( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
