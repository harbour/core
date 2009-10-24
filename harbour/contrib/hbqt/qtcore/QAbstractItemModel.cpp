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
 *  Constructed[ 32/36 [ 88.89% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  virtual QMap<int, QVariant> itemData ( const QModelIndex & index ) const
 *  virtual bool setItemData ( const QModelIndex & index, const QMap<int, QVariant> & roles )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // virtual QModelIndexList match ( const QModelIndex & start, int role, const QVariant & value, int hits = 1, Qt::MatchFlags flags = Qt::MatchFlags( Qt::MatchStartsWith | Qt::MatchWrap ) ) const
 *  // virtual QMimeData * mimeData ( const QModelIndexList & indexes ) const
 */

#include <QtCore/QPointer>

#include <QSize>
#include <QStringList>
#include <QtCore/QAbstractItemModel>

#include "../hbqt_slots.h"

/*
 * QAbstractItemModel ( QObject * parent = 0 )
 * virtual ~QAbstractItemModel ()
 */

HB_FUNC( QT_HBDBFMODEL )
{
   //hb_retptrGC( hbqt_ptrTOgcpointer( new HbDbfModel( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) ), release_QAbstractItemModel ) );
   hb_retptr( new HbDbfModel( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) ) );
}

HB_FUNC( QT_HBDBFMODEL_RESET )
{
   hbqt_par_HbDbfModel( 1 )->reset();
}

HB_FUNC( QT_HBDBFMODEL_INDEX )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_HbDbfModel( 1 )->index( hb_parni( 2 ), hb_parni( 3 ), QModelIndex() ) ), release_QModelIndex ) );
}

HB_FUNC( QT_HBDBFMODEL_HBSETROWCOLUMNS )
{
   hbqt_par_HbDbfModel( 1 )->hbSetRowColumns( hb_parni( 2 ), hb_parni( 3 ) );
}


QT_G_FUNC( release_QAbstractItemModel )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QAbstractItemModel" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QAbstractItemModel * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QAbstractItemModel" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QABSTRACTITEMMODEL )
{
}
/*
 * virtual QModelIndex buddy ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_BUDDY )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QAbstractItemModel( 1 )->buddy( *hbqt_par_QModelIndex( 2 ) ) ), release_QModelIndex ) );
}

/*
 * virtual bool canFetchMore ( const QModelIndex & parent ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_CANFETCHMORE )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->canFetchMore( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * virtual int columnCount ( const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QAbstractItemModel( 1 )->columnCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_DATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QAbstractItemModel( 1 )->data( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : Qt::DisplayRole ) ) ), release_QVariant ) );
}

/*
 * virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_DROPMIMEDATA )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
}

/*
 * virtual void fetchMore ( const QModelIndex & parent )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_FETCHMORE )
{
   hbqt_par_QAbstractItemModel( 1 )->fetchMore( *hbqt_par_QModelIndex( 2 ) );
}

/*
 * virtual Qt::ItemFlags flags ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QAbstractItemModel( 1 )->flags( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * virtual bool hasChildren ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HASCHILDREN )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->hasChildren( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * bool hasIndex ( int row, int column, const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HASINDEX )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->hasIndex( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HEADERDATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QAbstractItemModel( 1 )->headerData( hb_parni( 2 ), ( Qt::Orientation ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::DisplayRole ) ) ), release_QVariant ) );
}

/*
 * virtual QModelIndex index ( int row, int column, const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INDEX )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QAbstractItemModel( 1 )->index( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) ), release_QModelIndex ) );
}

/*
 * bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTCOLUMN )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * virtual bool insertColumns ( int column, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTCOLUMNS )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->insertColumns( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * bool insertRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTROW )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * virtual bool insertRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTROWS )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->insertRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * virtual QStringList mimeTypes () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_MIMETYPES )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QAbstractItemModel( 1 )->mimeTypes() ), release_QStringList ) );
}

/*
 * virtual QModelIndex parent ( const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_PARENT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QAbstractItemModel( 1 )->parent( *hbqt_par_QModelIndex( 2 ) ) ), release_QModelIndex ) );
}

/*
 * bool removeColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVECOLUMN )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->removeColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * virtual bool removeColumns ( int column, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVECOLUMNS )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->removeColumns( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * bool removeRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVEROW )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->removeRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
}

/*
 * virtual bool removeRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVEROWS )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->removeRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_ROWCOUNT )
{
   hb_retni( hbqt_par_QAbstractItemModel( 1 )->rowCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETDATA )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->setData( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QVariant( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::EditRole ) ) );
}

/*
 * virtual bool setHeaderData ( int section, Qt::Orientation orientation, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETHEADERDATA )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->setHeaderData( hb_parni( 2 ), ( Qt::Orientation ) hb_parni( 3 ), *hbqt_par_QVariant( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : Qt::EditRole ) ) );
}

/*
 * void setSupportedDragActions ( Qt::DropActions actions )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETSUPPORTEDDRAGACTIONS )
{
   hbqt_par_QAbstractItemModel( 1 )->setSupportedDragActions( ( Qt::DropActions ) hb_parni( 2 ) );
}

/*
 * QModelIndex sibling ( int row, int column, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SIBLING )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QAbstractItemModel( 1 )->sibling( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QModelIndex( 4 ) ) ), release_QModelIndex ) );
}

/*
 * virtual void sort ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SORT )
{
   hbqt_par_QAbstractItemModel( 1 )->sort( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/*
 * virtual QSize span ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SPAN )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QAbstractItemModel( 1 )->span( *hbqt_par_QModelIndex( 2 ) ) ), release_QSize ) );
}

/*
 * Qt::DropActions supportedDragActions () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUPPORTEDDRAGACTIONS )
{
   hb_retni( ( Qt::DropActions ) hbqt_par_QAbstractItemModel( 1 )->supportedDragActions() );
}

/*
 * virtual Qt::DropActions supportedDropActions () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUPPORTEDDROPACTIONS )
{
   hb_retni( ( Qt::DropActions ) hbqt_par_QAbstractItemModel( 1 )->supportedDropActions() );
}

/*
 * virtual void revert ()
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REVERT )
{
   hbqt_par_QAbstractItemModel( 1 )->revert();
}

/*
 * virtual bool submit ()
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUBMIT )
{
   hb_retl( hbqt_par_QAbstractItemModel( 1 )->submit() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
