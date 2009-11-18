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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QGroupBox>


/*
 * QGroupBox ( QWidget * parent = 0 )
 * QGroupBox ( const QString & title, QWidget * parent = 0 )
 * ~QGroupBox ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QGroupBox > pq;
} QGC_POINTER_QGroupBox;

QT_G_FUNC( release_QGroupBox )
{
   QGC_POINTER_QGroupBox * p = ( QGC_POINTER_QGroupBox * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QGroupBox                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QGroupBox                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QGroupBox * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QGroupBox * ) p->ph )->~QGroupBox();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QGroupBox * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QGroupBox                   Object deleted!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  YES release_QGroupBox                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QGroupBox                   Object Name Missing!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  NO  release_QGroupBox" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QGroupBox                   Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QGroupBox" );
      #endif
   }
}

void * gcAllocate_QGroupBox( void * pObj )
{
   QGC_POINTER_QGroupBox * p = ( QGC_POINTER_QGroupBox * ) hb_gcAllocate( sizeof( QGC_POINTER_QGroupBox ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QGroupBox;
   new( & p->pq ) QPointer< QGroupBox >( ( QGroupBox * ) pObj );
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QGroupBox                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QGROUPBOX )
{
   void * pObj = NULL;

   pObj = ( QGroupBox * ) new QGroupBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QGroupBox( pObj ) );
}
/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QGROUPBOX_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QGroupBox( 1 )->alignment() );
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QGROUPBOX_ISCHECKABLE )
{
   hb_retl( hbqt_par_QGroupBox( 1 )->isCheckable() );
}

/*
 * bool isChecked () const
 */
HB_FUNC( QT_QGROUPBOX_ISCHECKED )
{
   hb_retl( hbqt_par_QGroupBox( 1 )->isChecked() );
}

/*
 * bool isFlat () const
 */
HB_FUNC( QT_QGROUPBOX_ISFLAT )
{
   hb_retl( hbqt_par_QGroupBox( 1 )->isFlat() );
}

/*
 * void setAlignment ( int alignment )
 */
HB_FUNC( QT_QGROUPBOX_SETALIGNMENT )
{
   hbqt_par_QGroupBox( 1 )->setAlignment( hb_parni( 2 ) );
}

/*
 * void setCheckable ( bool checkable )
 */
HB_FUNC( QT_QGROUPBOX_SETCHECKABLE )
{
   hbqt_par_QGroupBox( 1 )->setCheckable( hb_parl( 2 ) );
}

/*
 * void setFlat ( bool flat )
 */
HB_FUNC( QT_QGROUPBOX_SETFLAT )
{
   hbqt_par_QGroupBox( 1 )->setFlat( hb_parl( 2 ) );
}

/*
 * void setTitle ( const QString & title )
 */
HB_FUNC( QT_QGROUPBOX_SETTITLE )
{
   hbqt_par_QGroupBox( 1 )->setTitle( hbqt_par_QString( 2 ) );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QGROUPBOX_TITLE )
{
   hb_retc( hbqt_par_QGroupBox( 1 )->title().toAscii().data() );
}

/*
 * void setChecked ( bool checked )
 */
HB_FUNC( QT_QGROUPBOX_SETCHECKED )
{
   hbqt_par_QGroupBox( 1 )->setChecked( hb_parl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
