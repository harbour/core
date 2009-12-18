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
 *  enum Roles { FileIconRole, FilePathRole, FileNameRole }
 */

#include <QtCore/QPointer>

#include <QtGui/QDirModel>


/*
 * QDirModel ( const QStringList & nameFilters, QDir::Filters filters, QDir::SortFlags sort, QObject * parent = 0 )
 * QDirModel ( QObject * parent = 0 )
 * ~QDirModel ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QDirModel > pq;
} QGC_POINTER_QDirModel;

QT_G_FUNC( release_QDirModel )
{
   QGC_POINTER_QDirModel * p = ( QGC_POINTER_QDirModel * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QDirModel                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QDirModel                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QDirModel * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QDirModel * ) p->ph )->~QDirModel();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QDirModel * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QDirModel                   Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QDirModel                   Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QDirModel                   Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QDirModel( void * pObj )
{
   QGC_POINTER_QDirModel * p = ( QGC_POINTER_QDirModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QDirModel ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = release_QDirModel;
   new( & p->pq ) QPointer< QDirModel >( ( QDirModel * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QDirModel                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QDIRMODEL )
{
   void * pObj = NULL;

   pObj = ( QDirModel* ) new QDirModel( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDirModel( pObj ) );
}
/*
 * virtual int columnCount ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QDIRMODEL_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QDirModel( 1 )->columnCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QDIRMODEL_DATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QDirModel( 1 )->data( *hbqt_par_QModelIndex( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : Qt::DisplayRole ) ) ) ) );
}

/*
 * virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent )
 */
HB_FUNC( QT_QDIRMODEL_DROPMIMEDATA )
{
   hb_retl( hbqt_par_QDirModel( 1 )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
}

/*
 * QIcon fileIcon ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_FILEICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QDirModel( 1 )->fileIcon( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * QFileInfo fileInfo ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_FILEINFO )
{
   hb_retptrGC( hbqt_gcAllocate_QFileInfo( new QFileInfo( hbqt_par_QDirModel( 1 )->fileInfo( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * QString fileName ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_FILENAME )
{
   hb_retc( hbqt_par_QDirModel( 1 )->fileName( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}

/*
 * QString filePath ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_FILEPATH )
{
   hb_retc( hbqt_par_QDirModel( 1 )->filePath( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QDIRMODEL_FILTER )
{
   hb_retni( ( QDir::Filters ) hbqt_par_QDirModel( 1 )->filter() );
}

/*
 * virtual Qt::ItemFlags flags ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QDirModel( 1 )->flags( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * virtual bool hasChildren ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QDIRMODEL_HASCHILDREN )
{
   hb_retl( hbqt_par_QDirModel( 1 )->hasChildren( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual QVariant headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QDIRMODEL_HEADERDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QDirModel( 1 )->headerData( hb_parni( 2 ), ( Qt::Orientation ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::DisplayRole ) ) ) ) );
}

/*
 * QFileIconProvider * iconProvider () const
 */
HB_FUNC( QT_QDIRMODEL_ICONPROVIDER )
{
   hb_retptr( ( QFileIconProvider* ) hbqt_par_QDirModel( 1 )->iconProvider() );
}

/*
 * virtual QModelIndex index ( int row, int column, const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QDIRMODEL_INDEX )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QDirModel( 1 )->index( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) ) ) );
}

/*
 * QModelIndex index ( const QString & path, int column = 0 ) const
 */
HB_FUNC( QT_QDIRMODEL_INDEX_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QDirModel( 1 )->index( hbqt_par_QString( 2 ), hb_parni( 3 ) ) ) ) );
}

/*
 * bool isDir ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QDIRMODEL_ISDIR )
{
   hb_retl( hbqt_par_QDirModel( 1 )->isDir( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QDIRMODEL_ISREADONLY )
{
   hb_retl( hbqt_par_QDirModel( 1 )->isReadOnly() );
}

/*
 * bool lazyChildCount () const
 */
HB_FUNC( QT_QDIRMODEL_LAZYCHILDCOUNT )
{
   hb_retl( hbqt_par_QDirModel( 1 )->lazyChildCount() );
}

/*
 * virtual QStringList mimeTypes () const
 */
HB_FUNC( QT_QDIRMODEL_MIMETYPES )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QDirModel( 1 )->mimeTypes() ) ) );
}

/*
 * QModelIndex mkdir ( const QModelIndex & parent, const QString & name )
 */
HB_FUNC( QT_QDIRMODEL_MKDIR )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QDirModel( 1 )->mkdir( *hbqt_par_QModelIndex( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QDIRMODEL_NAMEFILTERS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QDirModel( 1 )->nameFilters() ) ) );
}

/*
 * virtual QModelIndex parent ( const QModelIndex & child ) const
 */
HB_FUNC( QT_QDIRMODEL_PARENT )
{
   hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QDirModel( 1 )->parent( *hbqt_par_QModelIndex( 2 ) ) ) ) );
}

/*
 * bool remove ( const QModelIndex & index )
 */
HB_FUNC( QT_QDIRMODEL_REMOVE )
{
   hb_retl( hbqt_par_QDirModel( 1 )->remove( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QDIRMODEL_RESOLVESYMLINKS )
{
   hb_retl( hbqt_par_QDirModel( 1 )->resolveSymlinks() );
}

/*
 * bool rmdir ( const QModelIndex & index )
 */
HB_FUNC( QT_QDIRMODEL_RMDIR )
{
   hb_retl( hbqt_par_QDirModel( 1 )->rmdir( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QDIRMODEL_ROWCOUNT )
{
   hb_retni( hbqt_par_QDirModel( 1 )->rowCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QDIRMODEL_SETDATA )
{
   hb_retl( hbqt_par_QDirModel( 1 )->setData( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QVariant( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::EditRole ) ) );
}

/*
 * void setFilter ( QDir::Filters filters )
 */
HB_FUNC( QT_QDIRMODEL_SETFILTER )
{
   hbqt_par_QDirModel( 1 )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
}

/*
 * void setIconProvider ( QFileIconProvider * provider )
 */
HB_FUNC( QT_QDIRMODEL_SETICONPROVIDER )
{
   hbqt_par_QDirModel( 1 )->setIconProvider( hbqt_par_QFileIconProvider( 2 ) );
}

/*
 * void setLazyChildCount ( bool enable )
 */
HB_FUNC( QT_QDIRMODEL_SETLAZYCHILDCOUNT )
{
   hbqt_par_QDirModel( 1 )->setLazyChildCount( hb_parl( 2 ) );
}

/*
 * void setNameFilters ( const QStringList & filters )
 */
HB_FUNC( QT_QDIRMODEL_SETNAMEFILTERS )
{
   hbqt_par_QDirModel( 1 )->setNameFilters( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setReadOnly ( bool enable )
 */
HB_FUNC( QT_QDIRMODEL_SETREADONLY )
{
   hbqt_par_QDirModel( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setResolveSymlinks ( bool enable )
 */
HB_FUNC( QT_QDIRMODEL_SETRESOLVESYMLINKS )
{
   hbqt_par_QDirModel( 1 )->setResolveSymlinks( hb_parl( 2 ) );
}

/*
 * void setSorting ( QDir::SortFlags sort )
 */
HB_FUNC( QT_QDIRMODEL_SETSORTING )
{
   hbqt_par_QDirModel( 1 )->setSorting( ( QDir::SortFlags ) hb_parni( 2 ) );
}

/*
 * virtual void sort ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QDIRMODEL_SORT )
{
   hbqt_par_QDirModel( 1 )->sort( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/*
 * QDir::SortFlags sorting () const
 */
HB_FUNC( QT_QDIRMODEL_SORTING )
{
   hb_retni( ( QDir::SortFlags ) hbqt_par_QDirModel( 1 )->sorting() );
}

/*
 * virtual Qt::DropActions supportedDropActions () const
 */
HB_FUNC( QT_QDIRMODEL_SUPPORTEDDROPACTIONS )
{
   hb_retni( ( Qt::DropActions ) hbqt_par_QDirModel( 1 )->supportedDropActions() );
}

/*
 * void refresh ( const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QDIRMODEL_REFRESH )
{
   hbqt_par_QDirModel( 1 )->refresh( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
