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

/*
 *  Constructed[ 44/44 [ 100.00% ] ]
 *
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
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->absoluteDir() ), true ) );
   }
}

/*
 * QString absoluteFilePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEFILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->absoluteFilePath().toUtf8().data() );
   }
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QFILEINFO_ABSOLUTEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->absolutePath().toUtf8().data() );
   }
}

/*
 * QString baseName () const
 */
HB_FUNC( QT_QFILEINFO_BASENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->baseName().toUtf8().data() );
   }
}

/*
 * QString bundleName () const
 */
HB_FUNC( QT_QFILEINFO_BUNDLENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->bundleName().toUtf8().data() );
   }
}

/*
 * bool caching () const
 */
HB_FUNC( QT_QFILEINFO_CACHING )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->caching() );
   }
}

/*
 * QString canonicalFilePath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALFILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->canonicalFilePath().toUtf8().data() );
   }
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QFILEINFO_CANONICALPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->canonicalPath().toUtf8().data() );
   }
}

/*
 * QString completeBaseName () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETEBASENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->completeBaseName().toUtf8().data() );
   }
}

/*
 * QString completeSuffix () const
 */
HB_FUNC( QT_QFILEINFO_COMPLETESUFFIX )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->completeSuffix().toUtf8().data() );
   }
}

/*
 * QDateTime created () const
 */
HB_FUNC( QT_QFILEINFO_CREATED )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->created() ), true ) );
   }
}

/*
 * QDir dir () const
 */
HB_FUNC( QT_QFILEINFO_DIR )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->dir() ), true ) );
   }
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QFILEINFO_EXISTS )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->exists() );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QFILEINFO_FILENAME )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
   }
}

/*
 * QString filePath () const
 */
HB_FUNC( QT_QFILEINFO_FILEPATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->filePath().toUtf8().data() );
   }
}

/*
 * QString group () const
 */
HB_FUNC( QT_QFILEINFO_GROUP )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->group().toUtf8().data() );
   }
}

/*
 * uint groupId () const
 */
HB_FUNC( QT_QFILEINFO_GROUPID )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retni( ( p )->groupId() );
   }
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QFILEINFO_ISABSOLUTE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isAbsolute() );
   }
}

/*
 * bool isBundle () const
 */
HB_FUNC( QT_QFILEINFO_ISBUNDLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isBundle() );
   }
}

/*
 * bool isDir () const
 */
HB_FUNC( QT_QFILEINFO_ISDIR )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isDir() );
   }
}

/*
 * bool isExecutable () const
 */
HB_FUNC( QT_QFILEINFO_ISEXECUTABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isExecutable() );
   }
}

/*
 * bool isFile () const
 */
HB_FUNC( QT_QFILEINFO_ISFILE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isFile() );
   }
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QFILEINFO_ISHIDDEN )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isHidden() );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QFILEINFO_ISREADABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadable() );
   }
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QFILEINFO_ISRELATIVE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isRelative() );
   }
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QFILEINFO_ISROOT )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isRoot() );
   }
}

/*
 * bool isSymLink () const
 */
HB_FUNC( QT_QFILEINFO_ISSYMLINK )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isSymLink() );
   }
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QFILEINFO_ISWRITABLE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->isWritable() );
   }
}

/*
 * QDateTime lastModified () const
 */
HB_FUNC( QT_QFILEINFO_LASTMODIFIED )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastModified() ), true ) );
   }
}

/*
 * QDateTime lastRead () const
 */
HB_FUNC( QT_QFILEINFO_LASTREAD )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastRead() ), true ) );
   }
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QFILEINFO_MAKEABSOLUTE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->makeAbsolute() );
   }
}

/*
 * QString owner () const
 */
HB_FUNC( QT_QFILEINFO_OWNER )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->owner().toUtf8().data() );
   }
}

/*
 * uint ownerId () const
 */
HB_FUNC( QT_QFILEINFO_OWNERID )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retni( ( p )->ownerId() );
   }
}

/*
 * QString path () const
 */
HB_FUNC( QT_QFILEINFO_PATH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->path().toUtf8().data() );
   }
}

/*
 * bool permission ( QFile::Permissions permissions ) const
 */
HB_FUNC( QT_QFILEINFO_PERMISSION )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retl( ( p )->permission( ( QFile::Permissions ) hb_parni( 2 ) ) );
   }
}

/*
 * QFile::Permissions permissions () const
 */
HB_FUNC( QT_QFILEINFO_PERMISSIONS )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retni( ( QFile::Permissions ) ( p )->permissions() );
   }
}

/*
 * void refresh ()
 */
HB_FUNC( QT_QFILEINFO_REFRESH )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      ( p )->refresh();
   }
}

/*
 * void setCaching ( bool enable )
 */
HB_FUNC( QT_QFILEINFO_SETCACHING )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      ( p )->setCaching( hb_parl( 2 ) );
   }
}

/*
 * void setFile ( const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFile( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setFile ( const QFile & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_1 )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      ( p )->setFile( *hbqt_par_QFile( 2 ) );
   }
}

/*
 * void setFile ( const QDir & dir, const QString & file )
 */
HB_FUNC( QT_QFILEINFO_SETFILE_2 )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFile( *hbqt_par_QDir( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QFILEINFO_SIZE )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retnint( ( p )->size() );
   }
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QFILEINFO_SUFFIX )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->suffix().toUtf8().data() );
   }
}

/*
 * QString symLinkTarget () const
 */
HB_FUNC( QT_QFILEINFO_SYMLINKTARGET )
{
   QFileInfo * p = hbqt_par_QFileInfo( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->symLinkTarget().toUtf8().data() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
