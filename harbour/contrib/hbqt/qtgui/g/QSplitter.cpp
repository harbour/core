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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 21/23 [ 91.30% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setSizes ( const QList<int> & list )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //QSplitterHandle * handle ( int index ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QSplitter>


/*
 * QSplitter ( QWidget * parent = 0 )
 * QSplitter ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QSplitter ()
 */

typedef struct
{
   QPointer< QSplitter > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QSplitter;

QT_G_FUNC( hbqt_gcRelease_QSplitter )
{
   QSplitter  * ph = NULL ;
   QGC_POINTER_QSplitter * p = ( QGC_POINTER_QSplitter * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSplitter   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSplitter   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSplitter          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSplitter    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSplitter    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSplitter( void * pObj, bool bNew )
{
   QGC_POINTER_QSplitter * p = ( QGC_POINTER_QSplitter * ) hb_gcAllocate( sizeof( QGC_POINTER_QSplitter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSplitter >( ( QSplitter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSplitter;
   p->type = HBQT_TYPE_QSplitter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSplitter  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSplitter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSPLITTER )
{
   QSplitter * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      pObj =  new QSplitter( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj =  new QSplitter( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSplitter( ( void * ) pObj, true ) );
}

/*
 * void addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSPLITTER_ADDWIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->addWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_ADDWIDGET FP=( p )->addWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool childrenCollapsible () const
 */
HB_FUNC( QT_QSPLITTER_CHILDRENCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->childrenCollapsible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_CHILDRENCOLLAPSIBLE FP=hb_retl( ( p )->childrenCollapsible() ); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QSPLITTER_COUNT )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * void getRange ( int index, int * min, int * max ) const
 */
HB_FUNC( QT_QSPLITTER_GETRANGE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   int iMin = 0;
   int iMax = 0;

   if( p )
      ( p )->getRange( hb_parni( 2 ), &iMin, &iMax );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_GETRANGE FP=( p )->getRange( hb_parni( 2 ), &iMin, &iMax ); p is NULL" ) );
   }

   hb_storni( iMin, 3 );
   hb_storni( iMax, 4 );
}

/*
 * int handleWidth () const
 */
HB_FUNC( QT_QSPLITTER_HANDLEWIDTH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->handleWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_HANDLEWIDTH FP=hb_retni( ( p )->handleWidth() ); p is NULL" ) );
   }
}

/*
 * int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QSPLITTER_INDEXOF )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_INDEXOF FP=hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void insertWidget ( int index, QWidget * widget )
 */
HB_FUNC( QT_QSPLITTER_INSERTWIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_INSERTWIDGET FP=( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool isCollapsible ( int index ) const
 */
HB_FUNC( QT_QSPLITTER_ISCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->isCollapsible( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_ISCOLLAPSIBLE FP=hb_retl( ( p )->isCollapsible( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool opaqueResize () const
 */
HB_FUNC( QT_QSPLITTER_OPAQUERESIZE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->opaqueResize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_OPAQUERESIZE FP=hb_retl( ( p )->opaqueResize() ); p is NULL" ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QSPLITTER_ORIENTATION )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_ORIENTATION FP=hb_retni( ( Qt::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QSPLITTER_REFRESH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->refresh();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_REFRESH FP=( p )->refresh(); p is NULL" ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state )
 */
HB_FUNC( QT_QSPLITTER_RESTORESTATE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_RESTORESTATE FP=hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray saveState () const
 */
HB_FUNC( QT_QSPLITTER_SAVESTATE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SAVESTATE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setChildrenCollapsible ( bool )
 */
HB_FUNC( QT_QSPLITTER_SETCHILDRENCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setChildrenCollapsible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETCHILDRENCOLLAPSIBLE FP=( p )->setChildrenCollapsible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCollapsible ( int index, bool collapse )
 */
HB_FUNC( QT_QSPLITTER_SETCOLLAPSIBLE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setCollapsible( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETCOLLAPSIBLE FP=( p )->setCollapsible( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHandleWidth ( int )
 */
HB_FUNC( QT_QSPLITTER_SETHANDLEWIDTH )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setHandleWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETHANDLEWIDTH FP=( p )->setHandleWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOpaqueResize ( bool opaque = true )
 */
HB_FUNC( QT_QSPLITTER_SETOPAQUERESIZE )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setOpaqueResize( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETOPAQUERESIZE FP=( p )->setOpaqueResize( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QSPLITTER_SETORIENTATION )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETORIENTATION FP=( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStretchFactor ( int index, int stretch )
 */
HB_FUNC( QT_QSPLITTER_SETSTRETCHFACTOR )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      ( p )->setStretchFactor( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SETSTRETCHFACTOR FP=( p )->setStretchFactor( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QList<int> sizes () const
 */
HB_FUNC( QT_QSPLITTER_SIZES )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->sizes() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_SIZES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->sizes() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QSPLITTER_WIDGET )
{
   QSplitter * p = hbqt_par_QSplitter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSPLITTER_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
