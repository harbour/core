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
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 28/33 [ 84.85% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // const QStandardItem * itemPrototype () const
 */

#include <QtCore/QPointer>

#include <QtGui/QStandardItemModel>


/*
 * QStandardItemModel ( QObject * parent = 0 )
 * QStandardItemModel ( int rows, int columns, QObject * parent = 0 )
 * ~QStandardItemModel ()
 */

typedef struct
{
   QPointer< QStandardItemModel > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QStandardItemModel;

QT_G_FUNC( hbqt_gcRelease_QStandardItemModel )
{
   QStandardItemModel  * ph = NULL ;
   QGC_POINTER_QStandardItemModel * p = ( QGC_POINTER_QStandardItemModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QStandardItemModel   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QStandardItemModel   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QStandardItemModel          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStandardItemModel    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStandardItemModel    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStandardItemModel( void * pObj, bool bNew )
{
   QGC_POINTER_QStandardItemModel * p = ( QGC_POINTER_QStandardItemModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QStandardItemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStandardItemModel >( ( QStandardItemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStandardItemModel;
   p->type = HBQT_TYPE_QStandardItemModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStandardItemModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStandardItemModel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTANDARDITEMMODEL )
{
   QStandardItemModel * pObj = NULL;

   pObj =  new QStandardItemModel( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStandardItemModel( ( void * ) pObj, true ) );
}

/*
 * void appendRow ( QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_APPENDROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->appendRow( hbqt_par_QStandardItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_APPENDROW FP=( p )->appendRow( hbqt_par_QStandardItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_CLEAR )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QList<QStandardItem *> findItems ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_FINDITEMS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->findItems( QStandardItemModel::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly ), hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_FINDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->findItems( QStandardItemModel::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly ), hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_HORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_HORIZONTALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex indexFromItem ( const QStandardItem * item ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INDEXFROMITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexFromItem( hbqt_par_QStandardItem( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_INDEXFROMITEM FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexFromItem( hbqt_par_QStandardItem( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTCOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retl( ( p )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_INSERTCOLUMN FP=hb_retl( ( p )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) ); p is NULL" ) );
   }
}

/*
 * bool insertRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retl( ( p )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_INSERTROW FP=hb_retl( ( p )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_INSERTROW_1 FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * invisibleRootItem () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INVISIBLEROOTITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->invisibleRootItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_INVISIBLEROOTITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->invisibleRootItem(), false ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * item ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_ITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * itemFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEMFROMINDEX )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->itemFromIndex( *hbqt_par_QModelIndex( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_ITEMFROMINDEX FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->itemFromIndex( *hbqt_par_QModelIndex( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETCOLUMNCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setColumnCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETCOLUMNCOUNT FP=( p )->setColumnCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalHeaderItem ( int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERITEM FP=( p )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERLABELS FP=( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItem ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETITEM FP=( p )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETITEM_1 FP=( p )->setItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setItemPrototype ( const QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEMPROTOTYPE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setItemPrototype( hbqt_par_QStandardItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETITEMPROTOTYPE FP=( p )->setItemPrototype( hbqt_par_QStandardItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETROWCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setRowCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETROWCOUNT FP=( p )->setRowCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSortRole ( int role )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETSORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setSortRole( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETSORTROLE FP=( p )->setSortRole( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalHeaderItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETVERTICALHEADERITEM FP=( p )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      ( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SETVERTICALHEADERLABELS FP=( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * int sortRole () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retni( ( p )->sortRole() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_SORTROLE FP=hb_retni( ( p )->sortRole() ); p is NULL" ) );
   }
}

/*
 * QList<QStandardItem *> takeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKECOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_TAKECOLUMN FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_TAKEHORIZONTALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * takeItem ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_TAKEITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QList<QStandardItem *> takeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_TAKEROW FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_TAKEVERTICALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_VERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEMMODEL_VERTICALHEADERITEM FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
