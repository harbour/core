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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QAbstractProxyModel>
#include <QtGui/QItemSelection>

/* QAbstractProxyModel ( QObject * parent = 0 )
 * ~QAbstractProxyModel ()
 */

typedef struct
{
   QPointer< QAbstractProxyModel > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QAbstractProxyModel;

QT_G_FUNC( hbqt_gcRelease_QAbstractProxyModel )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractProxyModel( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractProxyModel * p = ( QGC_POINTER_QAbstractProxyModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractProxyModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractProxyModel >( ( QAbstractProxyModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractProxyModel;
   p->type = HBQT_TYPE_QAbstractProxyModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractProxyModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractProxyModel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTPROXYMODEL )
{
   // hb_retptr( new QAbstractProxyModel() );
}

/*
 * virtual QModelIndex mapFromSource ( const QModelIndex & sourceIndex ) const = 0
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPFROMSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapFromSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_MAPFROMSOURCE FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapFromSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QItemSelection mapSelectionFromSource ( const QItemSelection & sourceSelection ) const
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPSELECTIONFROMSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionFromSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_MAPSELECTIONFROMSOURCE FP=hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionFromSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QItemSelection mapSelectionToSource ( const QItemSelection & proxySelection ) const
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPSELECTIONTOSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionToSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_MAPSELECTIONTOSOURCE FP=hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionToSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QModelIndex mapToSource ( const QModelIndex & proxyIndex ) const = 0
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPTOSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapToSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_MAPTOSOURCE FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapToSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void setSourceModel ( QAbstractItemModel * sourceModel )
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_SETSOURCEMODEL )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      ( p )->setSourceModel( hbqt_par_QAbstractItemModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_SETSOURCEMODEL FP=( p )->setSourceModel( hbqt_par_QAbstractItemModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * QAbstractItemModel * sourceModel () const
 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_SOURCEMODEL )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->sourceModel(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPROXYMODEL_SOURCEMODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->sourceModel(), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
