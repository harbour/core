/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum Roles { FileIconRole, FilePathRole, FileNameRole, FilePermissions }
 */

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QFileIconProvider * iconProvider () const
 *  // virtual QMimeData * mimeData ( const QModelIndexList & indexes ) const
 *  // void setIconProvider ( QFileIconProvider * provider )
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFileSystemModel;

HBQT_GC_FUNC( hbqt_gcRelease_QFileSystemModel )
{
   HBQT_GC_T_QFileSystemModel * p = ( HBQT_GC_T_QFileSystemModel * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QFileSystemModel * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileSystemModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QFileSystemModel * p = ( HBQT_GC_T_QFileSystemModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFileSystemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFileSystemModel >( ( QFileSystemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileSystemModel;
   p->type = HBQT_TYPE_QFileSystemModel;

   return p;
}

HB_FUNC( QT_QFILESYSTEMMODEL )
{
   QFileSystemModel * pObj = NULL;

   pObj = new QFileSystemModel() ;

   hb_retptrGC( hbqt_gcAllocate_QFileSystemModel( ( void * ) pObj, true ) );
}

/* virtual bool dropMimeData ( const QMimeData * data, Qt::DropAction action, int row, int column, const QModelIndex & parent ) */
HB_FUNC( QT_QFILESYSTEMMODEL_DROPMIMEDATA )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->dropMimeData( hbqt_par_QMimeData( 2 ), ( Qt::DropAction ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QModelIndex( 6 ) ) );
}

/* QIcon fileIcon ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEICON )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->fileIcon( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* QFileInfo fileInfo ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEINFO )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFileInfo( new QFileInfo( ( p )->fileInfo( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* QString fileName ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_FILENAME )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName( *hbqt_par_QModelIndex( 2 ) ).toUtf8().data() );
}

/* QString filePath ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_FILEPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retstr_utf8( ( p )->filePath( *hbqt_par_QModelIndex( 2 ) ).toUtf8().data() );
}

/* QDir::Filters filter () const */
HB_FUNC( QT_QFILESYSTEMMODEL_FILTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retni( ( QDir::Filters ) ( p )->filter() );
}

/* QModelIndex index ( const QString & path, int column = 0 ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_INDEX )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* bool isDir ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_ISDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->isDir( *hbqt_par_QModelIndex( 2 ) ) );
}

/* bool isReadOnly () const */
HB_FUNC( QT_QFILESYSTEMMODEL_ISREADONLY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
}

/* QDateTime lastModified ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_LASTMODIFIED )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* virtual QStringList mimeTypes () const */
HB_FUNC( QT_QFILESYSTEMMODEL_MIMETYPES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->mimeTypes() ), true ) );
}

/* QModelIndex mkdir ( const QModelIndex & parent, const QString & name ) */
HB_FUNC( QT_QFILESYSTEMMODEL_MKDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mkdir( *hbqt_par_QModelIndex( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QVariant myComputer ( int role = Qt::DisplayRole ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_MYCOMPUTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->myComputer( hb_parnidef( 2, Qt::DisplayRole ) ) ), true ) );
}

/* bool nameFilterDisables () const */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERDISABLES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->nameFilterDisables() );
}

/* QStringList nameFilters () const */
HB_FUNC( QT_QFILESYSTEMMODEL_NAMEFILTERS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) );
}

/* QFile::Permissions permissions ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_PERMISSIONS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retni( ( QFile::Permissions ) ( p )->permissions( *hbqt_par_QModelIndex( 2 ) ) );
}

/* bool remove ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_REMOVE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->remove( *hbqt_par_QModelIndex( 2 ) ) );
}

/* bool resolveSymlinks () const */
HB_FUNC( QT_QFILESYSTEMMODEL_RESOLVESYMLINKS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->resolveSymlinks() );
}

/* bool rmdir ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_RMDIR )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retl( ( p )->rmdir( *hbqt_par_QModelIndex( 2 ) ) );
}

/* QDir rootDirectory () const */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTDIRECTORY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->rootDirectory() ), true ) );
}

/* QString rootPath () const */
HB_FUNC( QT_QFILESYSTEMMODEL_ROOTPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retstr_utf8( ( p )->rootPath().toUtf8().data() );
}

/* void setFilter ( QDir::Filters filters ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETFILTER )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
}

/* void setNameFilterDisables ( bool enable ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERDISABLES )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setNameFilterDisables( hb_parl( 2 ) );
}

/* void setNameFilters ( const QStringList & filters ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETNAMEFILTERS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setNameFilters( *hbqt_par_QStringList( 2 ) );
}

/* void setReadOnly ( bool enable ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETREADONLY )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
}

/* void setResolveSymlinks ( bool enable ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETRESOLVESYMLINKS )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      ( p )->setResolveSymlinks( hb_parl( 2 ) );
}

/* QModelIndex setRootPath ( const QString & newPath ) */
HB_FUNC( QT_QFILESYSTEMMODEL_SETROOTPATH )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->setRootPath( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* qint64 size ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_SIZE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retnint( ( p )->size( *hbqt_par_QModelIndex( 2 ) ) );
}

/* QString type ( const QModelIndex & index ) const */
HB_FUNC( QT_QFILESYSTEMMODEL_TYPE )
{
   QFileSystemModel * p = hbqt_par_QFileSystemModel( 1 );
   if( p )
      hb_retstr_utf8( ( p )->type( *hbqt_par_QModelIndex( 2 ) ).toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
