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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 32/34 [ 94.12% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  virtual QMap<int, QVariant> itemData ( const QModelIndex & index ) const
 *  virtual bool setItemData ( const QModelIndex & index, const QMap<int, QVariant> & roles )
 *
 *  *** Commented out protostypes ***
 *
 *  // virtual QModelIndexList match ( const QModelIndex & start, int role, const QVariant & value, int hits = 1, Qt::MatchFlags flags = Qt::MatchFlags( Qt::MatchStartsWith | Qt::MatchWrap ) ) const
 *  // virtual QMimeData * mimeData ( const QModelIndexList & indexes ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QSize>
#include <QtCore/QStringList>
#include <QtCore/QAbstractItemModel>


/*
 * QAbstractItemModel ( QObject * parent = 0 )
 * virtual ~QAbstractItemModel ()
 */



typedef struct
{
   QPointer< QAbstractItemModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractItemModel;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractItemModel )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractItemModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractItemModel * p = ( HBQT_GC_T_QAbstractItemModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractItemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractItemModel >( ( QAbstractItemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractItemModel;
   p->type = HBQT_TYPE_QAbstractItemModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractItemModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractItemModel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTITEMMODEL )
{
}

/*
 * virtual QModelIndex buddy ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_BUDDY )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->buddy( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * virtual bool canFetchMore ( const QModelIndex & parent ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_CANFETCHMORE )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->canFetchMore( *hbqt_par_QModelIndex( 2 ) ) );
   }
}

/*
 * virtual int columnCount ( const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_COLUMNCOUNT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_DATA )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( *hbqt_par_QModelIndex( 2 ), hb_parnidef( 3, Qt::DisplayRole ) ) ), true ) );
   }
}

/*
 * virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_DROPMIMEDATA )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
   }
}

/*
 * virtual void fetchMore ( const QModelIndex & parent )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_FETCHMORE )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      ( p )->fetchMore( *hbqt_par_QModelIndex( 2 ) );
   }
}

/*
 * virtual Qt::ItemFlags flags ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_FLAGS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::ItemFlags ) ( p )->flags( *hbqt_par_QModelIndex( 2 ) ) );
   }
}

/*
 * virtual bool hasChildren ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HASCHILDREN )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->hasChildren( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
   }
}

/*
 * bool hasIndex ( int row, int column, const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HASINDEX )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->hasIndex( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_HEADERDATA )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->headerData( hb_parni( 2 ), ( Qt::Orientation ) hb_parni( 3 ), hb_parnidef( 4, Qt::DisplayRole ) ) ), true ) );
   }
}

/*
 * virtual QModelIndex index ( int row, int column, const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INDEX )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) ), true ) );
   }
}

/*
 * bool insertColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTCOLUMN )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual bool insertColumns ( int column, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTCOLUMNS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertColumns( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
   }
}

/*
 * bool insertRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTROW )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual bool insertRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_INSERTROWS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->insertRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual QStringList mimeTypes () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_MIMETYPES )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->mimeTypes() ), true ) );
   }
}

/*
 * virtual QModelIndex parent ( const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_PARENT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->parent( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * bool removeColumn ( int column, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVECOLUMN )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->removeColumn( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual bool removeColumns ( int column, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVECOLUMNS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->removeColumns( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
   }
}

/*
 * bool removeRow ( int row, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVEROW )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->removeRow( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QModelIndex( 3 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual bool removeRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REMOVEROWS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->removeRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_ROWCOUNT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retni( ( p )->rowCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
   }
}

/*
 * virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETDATA )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->setData( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QVariant( 3 ), hb_parnidef( 4, Qt::EditRole ) ) );
   }
}

/*
 * virtual bool setHeaderData ( int section, Qt::Orientation orientation, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETHEADERDATA )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->setHeaderData( hb_parni( 2 ), ( Qt::Orientation ) hb_parni( 3 ), *hbqt_par_QVariant( 4 ), hb_parnidef( 5, Qt::EditRole ) ) );
   }
}

/*
 * void setSupportedDragActions ( Qt::DropActions actions )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SETSUPPORTEDDRAGACTIONS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      ( p )->setSupportedDragActions( ( Qt::DropActions ) hb_parni( 2 ) );
   }
}

/*
 * QModelIndex sibling ( int row, int column, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SIBLING )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->sibling( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QModelIndex( 4 ) ) ), true ) );
   }
}

/*
 * virtual void sort ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SORT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      ( p )->sort( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
   }
}

/*
 * virtual QSize span ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SPAN )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->span( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * Qt::DropActions supportedDragActions () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUPPORTEDDRAGACTIONS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::DropActions ) ( p )->supportedDragActions() );
   }
}

/*
 * virtual Qt::DropActions supportedDropActions () const
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUPPORTEDDROPACTIONS )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::DropActions ) ( p )->supportedDropActions() );
   }
}

/*
 * virtual void revert ()
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_REVERT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      ( p )->revert();
   }
}

/*
 * virtual bool submit ()
 */
HB_FUNC( QT_QABSTRACTITEMMODEL_SUBMIT )
{
   QAbstractItemModel * p = hbqt_par_QAbstractItemModel( 1 );
   if( p )
   {
      hb_retl( ( p )->submit() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
