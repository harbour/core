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

typedef struct
{
   QFileInfo * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFileInfo;

HBQT_GC_FUNC( hbqt_gcRelease_QFileInfo )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFileInfo   /.\\", p->ph ) );
         delete ( ( QFileInfo * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFileInfo   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFileInfo    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFileInfo    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileInfo( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFileInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileInfo;
   p->type = HBQT_TYPE_QFileInfo;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFileInfo", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFileInfo", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFILEINFO )
{
   QFileInfo * pObj = NULL;

   pObj = new QFileInfo() ;

   hb_retptrGC( hbqt_gcAllocate_QFileInfo( ( void * ) pObj, true ) );
}

/*
 * QDir absoluteDir () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEDIR )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->absoluteDir() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ABSOLUTEDIR FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->absoluteDir() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString absoluteFilePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEFILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->absoluteFilePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ABSOLUTEFILEPATH FP=hb_retc( ( p )->absoluteFilePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->absolutePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ABSOLUTEPATH FP=hb_retc( ( p )->absolutePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString baseName () const
 */
HB_FUNC( QT_QFILEINFO_BASENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->baseName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_BASENAME FP=hb_retc( ( p )->baseName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString bundleName () const
 */
HB_FUNC( QT_QFILEINFO_BUNDLENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->bundleName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_BUNDLENAME FP=hb_retc( ( p )->bundleName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool caching () const
 */
HB_FUNC( QT_QFILEINFO_CACHING )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->caching() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_CACHING FP=hb_retl( ( p )->caching() ); p is NULL" ) );
   }
}

/*
 * QString canonicalFilePath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALFILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->canonicalFilePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_CANONICALFILEPATH FP=hb_retc( ( p )->canonicalFilePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->canonicalPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_CANONICALPATH FP=hb_retc( ( p )->canonicalPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString completeBaseName () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETEBASENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->completeBaseName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_COMPLETEBASENAME FP=hb_retc( ( p )->completeBaseName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString completeSuffix () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETESUFFIX )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->completeSuffix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_COMPLETESUFFIX FP=hb_retc( ( p )->completeSuffix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDateTime created () const
 */
HB_FUNC( QT_QFILEINFO_CREATED )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->created() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_CREATED FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->created() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDir dir () const
 */
HB_FUNC( QT_QFILEINFO_DIR )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->dir() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_DIR FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->dir() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QFILEINFO_EXISTS )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->exists() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_EXISTS FP=hb_retl( ( p )->exists() ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QFILEINFO_FILENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString filePath () const
 */
HB_FUNC( QT_QFILEINFO_FILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->filePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_FILEPATH FP=hb_retc( ( p )->filePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString group () const
 */
HB_FUNC( QT_QFILEINFO_GROUP )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->group().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_GROUP FP=hb_retc( ( p )->group().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * uint groupId () const
 */
HB_FUNC( QT_QFILEINFO_GROUPID )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retni( ( p )->groupId() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_GROUPID FP=hb_retni( ( p )->groupId() ); p is NULL" ) );
   }
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QFILEINFO_ISABSOLUTE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isAbsolute() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISABSOLUTE FP=hb_retl( ( p )->isAbsolute() ); p is NULL" ) );
   }
}

/*
 * bool isBundle () const
 */
HB_FUNC( QT_QFILEINFO_ISBUNDLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isBundle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISBUNDLE FP=hb_retl( ( p )->isBundle() ); p is NULL" ) );
   }
}

/*
 * bool isDir () const
 */
HB_FUNC( QT_QFILEINFO_ISDIR )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isDir() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISDIR FP=hb_retl( ( p )->isDir() ); p is NULL" ) );
   }
}

/*
 * bool isExecutable () const
 */
HB_FUNC( QT_QFILEINFO_ISEXECUTABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isExecutable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISEXECUTABLE FP=hb_retl( ( p )->isExecutable() ); p is NULL" ) );
   }
}

/*
 * bool isFile () const
 */
HB_FUNC( QT_QFILEINFO_ISFILE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isFile() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISFILE FP=hb_retl( ( p )->isFile() ); p is NULL" ) );
   }
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QFILEINFO_ISHIDDEN )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISHIDDEN FP=hb_retl( ( p )->isHidden() ); p is NULL" ) );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QFILEINFO_ISREADABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isReadable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISREADABLE FP=hb_retl( ( p )->isReadable() ); p is NULL" ) );
   }
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QFILEINFO_ISRELATIVE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isRelative() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISRELATIVE FP=hb_retl( ( p )->isRelative() ); p is NULL" ) );
   }
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QFILEINFO_ISROOT )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isRoot() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISROOT FP=hb_retl( ( p )->isRoot() ); p is NULL" ) );
   }
}

/*
 * bool isSymLink () const
 */
HB_FUNC( QT_QFILEINFO_ISSYMLINK )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isSymLink() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISSYMLINK FP=hb_retl( ( p )->isSymLink() ); p is NULL" ) );
   }
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QFILEINFO_ISWRITABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->isWritable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_ISWRITABLE FP=hb_retl( ( p )->isWritable() ); p is NULL" ) );
   }
}

/*
 * QDateTime lastModified () const
 */
HB_FUNC( QT_QFILEINFO_LASTMODIFIED )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_LASTMODIFIED FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime lastRead () const
 */
HB_FUNC( QT_QFILEINFO_LASTREAD )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastRead() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_LASTREAD FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastRead() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QFILEINFO_MAKEABSOLUTE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->makeAbsolute() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_MAKEABSOLUTE FP=hb_retl( ( p )->makeAbsolute() ); p is NULL" ) );
   }
}

/*
 * QString owner () const
 */
HB_FUNC( QT_QFILEINFO_OWNER )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->owner().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_OWNER FP=hb_retc( ( p )->owner().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * uint ownerId () const
 */
HB_FUNC( QT_QFILEINFO_OWNERID )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retni( ( p )->ownerId() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_OWNERID FP=hb_retni( ( p )->ownerId() ); p is NULL" ) );
   }
}

/*
 * QString path () const
 */
HB_FUNC( QT_QFILEINFO_PATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->path().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_PATH FP=hb_retc( ( p )->path().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool permission ( QFile::Permissions permissions ) const
 */
HB_FUNC( QT_QFILEINFO_PERMISSION )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retl( ( p )->permission( ( QFile::Permissions ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_PERMISSION FP=hb_retl( ( p )->permission( ( QFile::Permissions ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QFile::Permissions permissions () const
 */
HB_FUNC( QT_QFILEINFO_PERMISSIONS )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retni( ( QFile::Permissions ) ( p )->permissions() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_PERMISSIONS FP=hb_retni( ( QFile::Permissions ) ( p )->permissions() ); p is NULL" ) );
   }
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QFILEINFO_REFRESH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      ( p )->refresh();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_REFRESH FP=( p )->refresh(); p is NULL" ) );
   }
}

/*
 * void setCaching ( bool enable )
 */
HB_FUNC( QT_QFILEINFO_SETCACHING )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      ( p )->setCaching( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SETCACHING FP=( p )->setCaching( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFile ( const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      ( p )->setFile( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SETFILE FP=( p )->setFile( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFile ( const QFile & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_1 )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      ( p )->setFile( *hbqt_par_QFile( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SETFILE_1 FP=( p )->setFile( *hbqt_par_QFile( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFile ( const QDir & dir, const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_2 )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      ( p )->setFile( *hbqt_par_QDir( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SETFILE_2 FP=( p )->setFile( *hbqt_par_QDir( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QFILEINFO_SIZE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retnint( ( p )->size() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SIZE FP=hb_retnint( ( p )->size() ); p is NULL" ) );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QFILEINFO_SUFFIX )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->suffix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SUFFIX FP=hb_retc( ( p )->suffix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString symLinkTarget () const
 */
HB_FUNC( QT_QFILEINFO_SYMLINKTARGET )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
      hb_retc( ( p )->symLinkTarget().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFILEINFO_SYMLINKTARGET FP=hb_retc( ( p )->symLinkTarget().toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
