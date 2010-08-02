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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Roles { FileIconRole, FilePathRole, FileNameRole, FilePermissions }
 */

#include <QtCore/QPointer>

#include <QtGui/QFileSystemModel>
#include <QtCore/QDateTime>

/*
 * QFileSystemModel ( QObject * parent = 0 )
 * ~QFileSystemModel ()
 */

typedef struct
{
   QPointer< QFileSystemModel > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFileSystemModel;

QT_G_FUNC( hbqt_gcRelease_QFileSystemModel )
{
   QFileSystemModel  * ph = NULL ;
   QGC_POINTER_QFileSystemModel * p = ( QGC_POINTER_QFileSystemModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFileSystemModel   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFileSystemModel   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFileSystemModel          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFileSystemModel    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFileSystemModel    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileSystemModel( void * pObj, bool bNew )
{
   QGC_POINTER_QFileSystemModel * p = ( QGC_POINTER_QFileSystemModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QFileSystemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFileSystemModel >( ( QFileSystemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileSystemModel;
   p->type = HBQT_TYPE_QFileSystemModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFileSystemModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFileSystemModel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFILESYSTEMMODEL )
{
   QFileSystemModel * pObj = NULL;

   pObj = ( QFileSystemModel * ) new QFileSystemModel() ;

   hb_retptrGC( hbqt_gcAllocate_QFileSystemModel( ( void * ) pObj, true ) );
}

/*
 * virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_DROPMIMEDATA )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_DROPMIMEDATA FP=hb_retl( ( p )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) ); p is NULL" ) );
   }
}

/*
 * QIcon fileIcon ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEICON )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->fileIcon( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_FILEICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->fileIcon( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QFileInfo fileInfo ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEINFO )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFileInfo( new QFileInfo( ( p )->fileInfo( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_FILEINFO FP=hb_retptrGC( hbqt_gcAllocate_QFileInfo( new QFileInfo( ( p )->fileInfo( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString fileName ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILENAME )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retc( ( p )->fileName( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_FILENAME FP=hb_retc( ( p )->fileName( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString filePath ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retc( ( p )->filePath( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_FILEPATH FP=hb_retc( ( p )->filePath( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDir::Filters filter () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_FILTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retni( ( QDir::Filters ) ( p )->filter() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_FILTER FP=hb_retni( ( QDir::Filters ) ( p )->filter() ); p is NULL" ) );
   }
}

/*
 * QModelIndex index ( const QString & path, int column = 0 ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_INDEX )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( QFileSystemModel::tr( hb_parc( 2 ) ), hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_INDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( QFileSystemModel::tr( hb_parc( 2 ) ), hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isDir ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ISDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->isDir( *hbqt_par_QModelIndex( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_ISDIR FP=hb_retl( ( p )->isDir( *hbqt_par_QModelIndex( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ISREADONLY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * QDateTime lastModified ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_LASTMODIFIED )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_LASTMODIFIED FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified( *hbqt_par_QModelIndex( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QStringList mimeTypes () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MIMETYPES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->mimeTypes() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_MIMETYPES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->mimeTypes() ), true ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex mkdir ( const QModelIndex & parent, const QString & name )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MKDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mkdir( *hbqt_par_QModelIndex( 2 ), QFileSystemModel::tr( hb_parc( 3 ) ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_MKDIR FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mkdir( *hbqt_par_QModelIndex( 2 ), QFileSystemModel::tr( hb_parc( 3 ) ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QVariant myComputer ( int role = Qt::DisplayRole ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_MYCOMPUTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->myComputer( hb_parnidef( 2, Qt::DisplayRole ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_MYCOMPUTER FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->myComputer( hb_parnidef( 2, Qt::DisplayRole ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool nameFilterDisables () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERDISABLES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->nameFilterDisables() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_NAMEFILTERDISABLES FP=hb_retl( ( p )->nameFilterDisables() ); p is NULL" ) );
   }
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_NAMEFILTERS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFile::Permissions permissions ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_PERMISSIONS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retni( ( QFile::Permissions ) ( p )->permissions( *hbqt_par_QModelIndex( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_PERMISSIONS FP=hb_retni( ( QFile::Permissions ) ( p )->permissions( *hbqt_par_QModelIndex( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool remove ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_REMOVE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->remove( *hbqt_par_QModelIndex( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_REMOVE FP=hb_retl( ( p )->remove( *hbqt_par_QModelIndex( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool resolveSymlinks () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_RESOLVESYMLINKS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->resolveSymlinks() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_RESOLVESYMLINKS FP=hb_retl( ( p )->resolveSymlinks() ); p is NULL" ) );
   }
}

/*
 * bool rmdir ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_RMDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->rmdir( *hbqt_par_QModelIndex( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_RMDIR FP=hb_retl( ( p )->rmdir( *hbqt_par_QModelIndex( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QDir rootDirectory () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTDIRECTORY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->rootDirectory() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_ROOTDIRECTORY FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->rootDirectory() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString rootPath () const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retc( ( p )->rootPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_ROOTPATH FP=hb_retc( ( p )->rootPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setFilter ( QDir::Filters filters )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETFILTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETFILTER FP=( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilterDisables ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERDISABLES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setNameFilterDisables( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETNAMEFILTERDISABLES FP=( p )->setNameFilterDisables( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilters ( const QStringList & filters )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setNameFilters( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETNAMEFILTERS FP=( p )->setNameFilters( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadOnly ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETREADONLY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResolveSymlinks ( bool enable )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETRESOLVESYMLINKS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setResolveSymlinks( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETRESOLVESYMLINKS FP=( p )->setResolveSymlinks( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex setRootPath ( const QString & newPath )
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SETROOTPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->setRootPath( QFileSystemModel::tr( hb_parc( 2 ) ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SETROOTPATH FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->setRootPath( QFileSystemModel::tr( hb_parc( 2 ) ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qint64 size ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_SIZE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retnint( ( p )->size( *hbqt_par_QModelIndex( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_SIZE FP=hb_retnint( ( p )->size( *hbqt_par_QModelIndex( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString type ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QFILESYSTEMMODEL_TYPE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retc( ( p )->type( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILESYSTEMMODEL_TYPE FP=hb_retc( ( p )->type( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
