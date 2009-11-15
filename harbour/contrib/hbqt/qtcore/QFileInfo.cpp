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
 *  flags PermissionSpec
 */

#include <QtCore/QPointer>

#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QDateTime>


/* QFileInfo ()
 * QFileInfo ( const QString & file )
 * QFileInfo ( const QFile & file )
 * QFileInfo ( const QDir & dir, const QString & file )
 * QFileInfo ( const QFileInfo & fileinfo )
 * ~QFileInfo ()
 */

QT_G_FUNC( release_QFileInfo )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QFileInfo                    p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QFileInfo                   ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QFileInfo * ) p->ph )->~QFileInfo();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QFileInfo                   Object deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  YES release_QFileInfo                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QFileInfo                   Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QFileInfo" );
      #endif
   }
}

void * gcAllocate_QFileInfo( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QFileInfo;
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QFileInfo                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QFILEINFO )
{
   void * pObj = NULL;

   pObj = new QFileInfo() ;

   hb_retptrGC( gcAllocate_QFileInfo( pObj ) );
}
/*
 * QDir absoluteDir () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEDIR )
{
   hb_retptrGC( gcAllocate_QDir( new QDir( hbqt_par_QFileInfo( 1 )->absoluteDir() ) ) );
}

/*
 * QString absoluteFilePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEFILEPATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->absoluteFilePath().toAscii().data() );
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEPATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->absolutePath().toAscii().data() );
}

/*
 * QString baseName () const
 */
HB_FUNC( QT_QFILEINFO_BASENAME )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->baseName().toAscii().data() );
}

/*
 * QString bundleName () const
 */
HB_FUNC( QT_QFILEINFO_BUNDLENAME )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->bundleName().toAscii().data() );
}

/*
 * bool caching () const
 */
HB_FUNC( QT_QFILEINFO_CACHING )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->caching() );
}

/*
 * QString canonicalFilePath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALFILEPATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->canonicalFilePath().toAscii().data() );
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALPATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->canonicalPath().toAscii().data() );
}

/*
 * QString completeBaseName () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETEBASENAME )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->completeBaseName().toAscii().data() );
}

/*
 * QString completeSuffix () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETESUFFIX )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->completeSuffix().toAscii().data() );
}

/*
 * QDateTime created () const
 */
HB_FUNC( QT_QFILEINFO_CREATED )
{
   hb_retptrGC( gcAllocate_QDateTime( new QDateTime( hbqt_par_QFileInfo( 1 )->created() ) ) );
}

/*
 * QDir dir () const
 */
HB_FUNC( QT_QFILEINFO_DIR )
{
   hb_retptrGC( gcAllocate_QDir( new QDir( hbqt_par_QFileInfo( 1 )->dir() ) ) );
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QFILEINFO_EXISTS )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->exists() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QFILEINFO_FILENAME )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->fileName().toAscii().data() );
}

/*
 * QString filePath () const
 */
HB_FUNC( QT_QFILEINFO_FILEPATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->filePath().toAscii().data() );
}

/*
 * QString group () const
 */
HB_FUNC( QT_QFILEINFO_GROUP )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->group().toAscii().data() );
}

/*
 * uint groupId () const
 */
HB_FUNC( QT_QFILEINFO_GROUPID )
{
   hb_retni( hbqt_par_QFileInfo( 1 )->groupId() );
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QFILEINFO_ISABSOLUTE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isAbsolute() );
}

/*
 * bool isBundle () const
 */
HB_FUNC( QT_QFILEINFO_ISBUNDLE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isBundle() );
}

/*
 * bool isDir () const
 */
HB_FUNC( QT_QFILEINFO_ISDIR )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isDir() );
}

/*
 * bool isExecutable () const
 */
HB_FUNC( QT_QFILEINFO_ISEXECUTABLE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isExecutable() );
}

/*
 * bool isFile () const
 */
HB_FUNC( QT_QFILEINFO_ISFILE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isFile() );
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QFILEINFO_ISHIDDEN )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isHidden() );
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QFILEINFO_ISREADABLE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isReadable() );
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QFILEINFO_ISRELATIVE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isRelative() );
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QFILEINFO_ISROOT )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isRoot() );
}

/*
 * bool isSymLink () const
 */
HB_FUNC( QT_QFILEINFO_ISSYMLINK )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isSymLink() );
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QFILEINFO_ISWRITABLE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->isWritable() );
}

/*
 * QDateTime lastModified () const
 */
HB_FUNC( QT_QFILEINFO_LASTMODIFIED )
{
   hb_retptrGC( gcAllocate_QDateTime( new QDateTime( hbqt_par_QFileInfo( 1 )->lastModified() ) ) );
}

/*
 * QDateTime lastRead () const
 */
HB_FUNC( QT_QFILEINFO_LASTREAD )
{
   hb_retptrGC( gcAllocate_QDateTime( new QDateTime( hbqt_par_QFileInfo( 1 )->lastRead() ) ) );
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QFILEINFO_MAKEABSOLUTE )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->makeAbsolute() );
}

/*
 * QString owner () const
 */
HB_FUNC( QT_QFILEINFO_OWNER )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->owner().toAscii().data() );
}

/*
 * uint ownerId () const
 */
HB_FUNC( QT_QFILEINFO_OWNERID )
{
   hb_retni( hbqt_par_QFileInfo( 1 )->ownerId() );
}

/*
 * QString path () const
 */
HB_FUNC( QT_QFILEINFO_PATH )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->path().toAscii().data() );
}

/*
 * bool permission ( QFile::Permissions permissions ) const
 */
HB_FUNC( QT_QFILEINFO_PERMISSION )
{
   hb_retl( hbqt_par_QFileInfo( 1 )->permission( ( QFile::Permissions ) hb_parni( 2 ) ) );
}

/*
 * QFile::Permissions permissions () const
 */
HB_FUNC( QT_QFILEINFO_PERMISSIONS )
{
   hb_retni( ( QFile::Permissions ) hbqt_par_QFileInfo( 1 )->permissions() );
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QFILEINFO_REFRESH )
{
   hbqt_par_QFileInfo( 1 )->refresh();
}

/*
 * void setCaching ( bool enable )
 */
HB_FUNC( QT_QFILEINFO_SETCACHING )
{
   hbqt_par_QFileInfo( 1 )->setCaching( hb_parl( 2 ) );
}

/*
 * void setFile ( const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE )
{
   hbqt_par_QFileInfo( 1 )->setFile( hbqt_par_QString( 2 ) );
}

/*
 * void setFile ( const QFile & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_1 )
{
   hbqt_par_QFileInfo( 1 )->setFile( *hbqt_par_QFile( 2 ) );
}

/*
 * void setFile ( const QDir & dir, const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_2 )
{
   hbqt_par_QFileInfo( 1 )->setFile( *hbqt_par_QDir( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QFILEINFO_SIZE )
{
   hb_retnint( hbqt_par_QFileInfo( 1 )->size() );
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QFILEINFO_SUFFIX )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->suffix().toAscii().data() );
}

/*
 * QString symLinkTarget () const
 */
HB_FUNC( QT_QFILEINFO_SYMLINKTARGET )
{
   hb_retc( hbqt_par_QFileInfo( 1 )->symLinkTarget().toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
