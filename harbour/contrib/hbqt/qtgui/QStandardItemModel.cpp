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

/*
 *  Constructed[ 26/33 [ 78.79% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  QList<QStandardItem *> findItems ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly, int column = 0 ) const
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *  QList<QStandardItem *> takeColumn ( int column )
 *  QList<QStandardItem *> takeRow ( int row )
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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QStandardItemModel > pq;
} QGC_POINTER_QStandardItemModel;

QT_G_FUNC( release_QStandardItemModel )
{
   QGC_POINTER_QStandardItemModel * p = ( QGC_POINTER_QStandardItemModel * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QStandardItemModel           p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QStandardItemModel          ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QStandardItemModel * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QStandardItemModel * ) p->ph )->~QStandardItemModel();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QStandardItemModel * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QStandardItemModel          Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QStandardItemModel          Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QStandardItemModel          Object Already deleted!" ) );
   }
}

void * gcAllocate_QStandardItemModel( void * pObj )
{
   QGC_POINTER_QStandardItemModel * p = ( QGC_POINTER_QStandardItemModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QStandardItemModel ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QStandardItemModel;
   new( & p->pq ) QPointer< QStandardItemModel >( ( QStandardItemModel * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QStandardItemModel          %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QSTANDARDITEMMODEL )
{
   void * pObj = NULL;

   pObj = ( QStandardItemModel* ) new QStandardItemModel( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( gcAllocate_QStandardItemModel( pObj ) );
}
/*
 * void appendRow ( QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_APPENDROW )
{
   hbqt_par_QStandardItemModel( 1 )->appendRow( hbqt_par_QStandardItem( 2 ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_CLEAR )
{
   hbqt_par_QStandardItemModel( 1 )->clear();
}

/*
 * QStandardItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_HORIZONTALHEADERITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->horizontalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QModelIndex indexFromItem ( const QStandardItem * item ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INDEXFROMITEM )
{
   hb_retptrGC( gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QStandardItemModel( 1 )->indexFromItem( hbqt_par_QStandardItem( 2 ) ) ) ) );
}

/*
 * bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTCOLUMN )
{
   hb_retl( hbqt_par_QStandardItemModel( 1 )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * bool insertRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW )
{
   hb_retl( hbqt_par_QStandardItemModel( 1 )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW_1 )
{
   hbqt_par_QStandardItemModel( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * QStandardItem * invisibleRootItem () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INVISIBLEROOTITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->invisibleRootItem() );
}

/*
 * QStandardItem * item ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->item( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QStandardItem * itemFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEMFROMINDEX )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->itemFromIndex( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * const QStandardItem * itemPrototype () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEMPROTOTYPE )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->itemPrototype() );
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETCOLUMNCOUNT )
{
   hbqt_par_QStandardItemModel( 1 )->setColumnCount( hb_parni( 2 ) );
}

/*
 * void setHorizontalHeaderItem ( int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERITEM )
{
   hbqt_par_QStandardItemModel( 1 )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERLABELS )
{
   hbqt_par_QStandardItemModel( 1 )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setItem ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM )
{
   hbqt_par_QStandardItemModel( 1 )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
}

/*
 * void setItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM_1 )
{
   hbqt_par_QStandardItemModel( 1 )->setItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * void setItemPrototype ( const QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEMPROTOTYPE )
{
   hbqt_par_QStandardItemModel( 1 )->setItemPrototype( hbqt_par_QStandardItem( 2 ) );
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETROWCOUNT )
{
   hbqt_par_QStandardItemModel( 1 )->setRowCount( hb_parni( 2 ) );
}

/*
 * void setSortRole ( int role )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETSORTROLE )
{
   hbqt_par_QStandardItemModel( 1 )->setSortRole( hb_parni( 2 ) );
}

/*
 * void setVerticalHeaderItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERITEM )
{
   hbqt_par_QStandardItemModel( 1 )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERLABELS )
{
   hbqt_par_QStandardItemModel( 1 )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
}

/*
 * int sortRole () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SORTROLE )
{
   hb_retni( hbqt_par_QStandardItemModel( 1 )->sortRole() );
}

/*
 * QStandardItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEHORIZONTALHEADERITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->takeHorizontalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QStandardItem * takeItem ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QStandardItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEVERTICALHEADERITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->takeVerticalHeaderItem( hb_parni( 2 ) ) );
}

/*
 * QStandardItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_VERTICALHEADERITEM )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItemModel( 1 )->verticalHeaderItem( hb_parni( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
