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
 *  Constructed[ 28/32 [ 87.50% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *
 *  *** Commented out protostypes ***
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStandardItemModel;

HBQT_GC_FUNC( hbqt_gcRelease_QStandardItemModel )
{
   QStandardItemModel  * ph = NULL ;
   HBQT_GC_T_QStandardItemModel * p = ( HBQT_GC_T_QStandardItemModel * ) Cargo;

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
   HBQT_GC_T_QStandardItemModel * p = ( HBQT_GC_T_QStandardItemModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStandardItemModel ), hbqt_gcFuncs() );

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
   {
      ( p )->appendRow( hbqt_par_QStandardItem( 2 ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_CLEAR )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * QList<QStandardItem *> findItems ( const QString & text, Qt::MatchFlags flags = Qt::MatchExactly, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_FINDITEMS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->findItems( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::MatchFlags ) hb_parni( 3 ) : ( Qt::MatchFlags ) Qt::MatchExactly ), hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QStandardItem * horizontalHeaderItem ( int column ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_HORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->horizontalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QModelIndex indexFromItem ( const QStandardItem * item ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INDEXFROMITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->indexFromItem( hbqt_par_QStandardItem( 2 ) ) ), true ) );
   }
}

/*
 * bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTCOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * bool insertRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INSERTROW_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * QStandardItem * invisibleRootItem () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_INVISIBLEROOTITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->invisibleRootItem(), false ) );
   }
}

/*
 * QStandardItem * item ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->item( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QStandardItem * itemFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_ITEMFROMINDEX )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->itemFromIndex( *hbqt_par_QModelIndex( 2 ) ), false ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETCOLUMNCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setColumnCount( hb_parni( 2 ) );
   }
}

/*
 * void setHorizontalHeaderItem ( int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setHorizontalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * void setHorizontalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETHORIZONTALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setHorizontalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setItem ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setItem( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
   }
}

/*
 * void setItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEM_1 )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * void setItemPrototype ( const QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETITEMPROTOTYPE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setItemPrototype( hbqt_par_QStandardItem( 2 ) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETROWCOUNT )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setRowCount( hb_parni( 2 ) );
   }
}

/*
 * void setSortRole ( int role )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETSORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setSortRole( hb_parni( 2 ) );
   }
}

/*
 * void setVerticalHeaderItem ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setVerticalHeaderItem( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * void setVerticalHeaderLabels ( const QStringList & labels )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SETVERTICALHEADERLABELS )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      ( p )->setVerticalHeaderLabels( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * int sortRole () const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_SORTROLE )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retni( ( p )->sortRole() );
   }
}

/*
 * QList<QStandardItem *> takeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKECOLUMN )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QStandardItem * takeHorizontalHeaderItem ( int column )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEHORIZONTALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeHorizontalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QStandardItem * takeItem ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeItem( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QList<QStandardItem *> takeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEROW )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QStandardItem * takeVerticalHeaderItem ( int row )
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_TAKEVERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeVerticalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QStandardItem * verticalHeaderItem ( int row ) const
 */
HB_FUNC( QT_QSTANDARDITEMMODEL_VERTICALHEADERITEM )
{
   QStandardItemModel * p = hbqt_par_QStandardItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->verticalHeaderItem( hb_parni( 2 ) ), false ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
