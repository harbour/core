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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Roles { FileIconRole, FilePathRole, FileNameRole, FilePermissions }
 */


#include <QtGui/QFileSystemModel>
#include <QtCore/QDateTime>

/*
 * QFileSystemModel ( QObject * parent = 0 )
 * ~QFileSystemModel ()
 */
HB_FUNC( QT_QFILESYSTEMMODEL )
{
   hb_retptr( ( QFileSystemModel * ) new QFileSystemModel() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QFILESYSTEMMODEL_DESTROY )
{
   hbqt_par_QFileSystemModel( 1 )->~QFileSystemModel();
}

/*
 * virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_DROPMIMEDATA )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
}

/*
 * QIcon fileIcon ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEICON )
{
   hb_retptr( new QIcon( hbqt_par_QFileSystemModel( 1 )->fileIcon( *hbqt_par_QModelIndex( 2 ) ) ) );
}

/*
 * QFileInfo fileInfo ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEINFO )
{
   hb_retptr( new QFileInfo( hbqt_par_QFileSystemModel( 1 )->fileInfo( *hbqt_par_QModelIndex( 2 ) ) ) );
}

/*
 * QString fileName ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILENAME )
{
   hb_retc( hbqt_par_QFileSystemModel( 1 )->fileName( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}

/*
 * QString filePath ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEPATH )
{
   hb_retc( hbqt_par_QFileSystemModel( 1 )->filePath( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILTER )
{
   hb_retni( ( QDir::Filters ) hbqt_par_QFileSystemModel( 1 )->filter() );
}

/*
 * QFileIconProvider * iconProvider () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ICONPROVIDER )
{
   hb_retptr( ( QFileIconProvider* ) hbqt_par_QFileSystemModel( 1 )->iconProvider() );
}

/*
 * QModelIndex index ( const QString & path, int column = 0 ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_INDEX )
{
   hb_retptr( new QModelIndex( hbqt_par_QFileSystemModel( 1 )->index( hbqt_par_QString( 2 ), hb_parni( 3 ) ) ) );
}

/*
 * bool isDir ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ISDIR )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->isDir( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ISREADONLY )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->isReadOnly() );
}

/*
 * QDateTime lastModified ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_LASTMODIFIED )
{
   hb_retptr( new QDateTime( hbqt_par_QFileSystemModel( 1 )->lastModified( *hbqt_par_QModelIndex( 2 ) ) ) );
}

/*
 * virtual QMimeData * mimeData ( const QModelIndexList & indexes ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MIMEDATA )
{
   hb_retptr( ( QMimeData* ) hbqt_par_QFileSystemModel( 1 )->mimeData( *hbqt_par_QModelIndexList( 2 ) ) );
}

/*
 * virtual QStringList mimeTypes () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MIMETYPES )
{
   hb_retptr( new QStringList( hbqt_par_QFileSystemModel( 1 )->mimeTypes() ) );
}

/*
 * QModelIndex mkdir ( const QModelIndex & parent, const QString & name )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MKDIR )
{
   hb_retptr( new QModelIndex( hbqt_par_QFileSystemModel( 1 )->mkdir( *hbqt_par_QModelIndex( 2 ), hbqt_par_QString( 3 ) ) ) );
}

/*
 * QVariant myComputer ( int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MYCOMPUTER )
{
   hb_retptr( new QVariant( hbqt_par_QFileSystemModel( 1 )->myComputer( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : Qt::DisplayRole ) ) ) );
}

/*
 * bool nameFilterDisables () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERDISABLES )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->nameFilterDisables() );
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERS )
{
   hb_retptr( new QStringList( hbqt_par_QFileSystemModel( 1 )->nameFilters() ) );
}

/*
 * QFile::Permissions permissions ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_PERMISSIONS )
{
   hb_retni( ( QFile::Permissions ) hbqt_par_QFileSystemModel( 1 )->permissions( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool remove ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_REMOVE )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->remove( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_RESOLVESYMLINKS )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->resolveSymlinks() );
}

/*
 * bool rmdir ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_RMDIR )
{
   hb_retl( hbqt_par_QFileSystemModel( 1 )->rmdir( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * QDir rootDirectory () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTDIRECTORY )
{
   hb_retptr( new QDir( hbqt_par_QFileSystemModel( 1 )->rootDirectory() ) );
}

/*
 * QString rootPath () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTPATH )
{
   hb_retc( hbqt_par_QFileSystemModel( 1 )->rootPath().toAscii().data() );
}

/*
 * void setFilter ( QDir::Filters filters )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETFILTER )
{
   hbqt_par_QFileSystemModel( 1 )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
}

/*
 * void setIconProvider ( QFileIconProvider * provider )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETICONPROVIDER )
{
   hbqt_par_QFileSystemModel( 1 )->setIconProvider( hbqt_par_QFileIconProvider( 2 ) );
}

/*
 * void setNameFilterDisables ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERDISABLES )
{
   hbqt_par_QFileSystemModel( 1 )->setNameFilterDisables( hb_parl( 2 ) );
}

/*
 * void setNameFilters ( const QStringList & filters )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERS )
{
   hbqt_par_QFileSystemModel( 1 )->setNameFilters( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setReadOnly ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETREADONLY )
{
   hbqt_par_QFileSystemModel( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setResolveSymlinks ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETRESOLVESYMLINKS )
{
   hbqt_par_QFileSystemModel( 1 )->setResolveSymlinks( hb_parl( 2 ) );
}

/*
 * QModelIndex setRootPath ( const QString & newPath )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETROOTPATH )
{
   hb_retptr( new QModelIndex( hbqt_par_QFileSystemModel( 1 )->setRootPath( hbqt_par_QString( 2 ) ) ) );
}

/*
 * qint64 size ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SIZE )
{
   hb_retnint( hbqt_par_QFileSystemModel( 1 )->size( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * QString type ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_TYPE )
{
   hb_retc( hbqt_par_QFileSystemModel( 1 )->type( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
