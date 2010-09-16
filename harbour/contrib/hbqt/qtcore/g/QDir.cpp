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
 *  enum Filter { Dirs, AllDirs, Files, Drives, ..., CaseSensitive }
 *  enum SortFlag { Name, Time, Size, Type, ..., LocaleAware }
 *  flags Filters
 *  flags SortFlags
 */

#include <QtCore/QPointer>

#include <QtCore/QDir>


/*
 * QDir ( const QDir & dir )
 * QDir ( const QString & path = QString() )
 * QDir ( const QString & path, const QString & nameFilter, SortFlags sort = SortFlags( Name | IgnoreCase ), Filters filters = AllEntries )
 * ~QDir ()
 */

typedef struct
{
   QDir * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDir;

HBQT_GC_FUNC( hbqt_gcRelease_QDir )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QDir   /.\\", p->ph ) );
         delete ( ( QDir * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QDir   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDir    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDir    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDir( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDir * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDir;
   p->type = HBQT_TYPE_QDir;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDir", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDir", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDIR )
{
   QDir * pObj = NULL;

   pObj = new QDir( hbqt_par_QString( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDir( ( void * ) pObj, true ) );
}

/*
 * QString absoluteFilePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_ABSOLUTEFILEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->absoluteFilePath( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QDIR_ABSOLUTEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->absolutePath().toUtf8().data() );
   }
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QDIR_CANONICALPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->canonicalPath().toUtf8().data() );
   }
}

/*
 * bool cd ( const QString & dirName )
 */
HB_FUNC( QT_QDIR_CD )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->cd( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool cdUp ()
 */
HB_FUNC( QT_QDIR_CDUP )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->cdUp() );
   }
}

/*
 * uint count () const
 */
HB_FUNC( QT_QDIR_COUNT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * QString dirName () const
 */
HB_FUNC( QT_QDIR_DIRNAME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->dirName().toUtf8().data() );
   }
}

/*
 * QStringList entryList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( *hbqt_par_QStringList( 2 ), ( HB_ISNUM( 3 ) ? ( QDir::Filters ) hb_parni( 3 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 4 ) ? ( QDir::SortFlags ) hb_parni( 4 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) );
   }
}

/*
 * QStringList entryList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( ( HB_ISNUM( 2 ) ? ( QDir::Filters ) hb_parni( 2 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 3 ) ? ( QDir::SortFlags ) hb_parni( 3 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) );
   }
}

/*
 * bool exists ( const QString & name ) const
 */
HB_FUNC( QT_QDIR_EXISTS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->exists( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QDIR_EXISTS_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->exists() );
   }
}

/*
 * QString filePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_FILEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->filePath( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * Filters filter () const
 */
HB_FUNC( QT_QDIR_FILTER )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retni( ( QDir::Filters ) ( p )->filter() );
   }
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QDIR_ISABSOLUTE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->isAbsolute() );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QDIR_ISREADABLE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadable() );
   }
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QDIR_ISRELATIVE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->isRelative() );
   }
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QDIR_ISROOT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->isRoot() );
   }
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QDIR_MAKEABSOLUTE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retl( ( p )->makeAbsolute() );
   }
}

/*
 * bool mkdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_MKDIR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->mkdir( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool mkpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_MKPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->mkpath( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QDIR_NAMEFILTERS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) );
   }
}

/*
 * QString path () const
 */
HB_FUNC( QT_QDIR_PATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->path().toUtf8().data() );
   }
}

/*
 * void refresh () const
 */
HB_FUNC( QT_QDIR_REFRESH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      ( p )->refresh();
   }
}

/*
 * QString relativeFilePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_RELATIVEFILEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->relativeFilePath( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * bool remove ( const QString & fileName )
 */
HB_FUNC( QT_QDIR_REMOVE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool rename ( const QString & oldName, const QString & newName )
 */
HB_FUNC( QT_QDIR_RENAME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->rename( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool rmdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_RMDIR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->rmdir( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool rmpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_RMPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->rmpath( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * void setFilter ( Filters filters )
 */
HB_FUNC( QT_QDIR_SETFILTER )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      ( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
   }
}

/*
 * void setNameFilters ( const QStringList & nameFilters )
 */
HB_FUNC( QT_QDIR_SETNAMEFILTERS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      ( p )->setNameFilters( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setPath ( const QString & path )
 */
HB_FUNC( QT_QDIR_SETPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setSorting ( SortFlags sort )
 */
HB_FUNC( QT_QDIR_SETSORTING )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      ( p )->setSorting( ( QDir::SortFlags ) hb_parni( 2 ) );
   }
}

/*
 * SortFlags sorting () const
 */
HB_FUNC( QT_QDIR_SORTING )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retni( ( QDir::SortFlags ) ( p )->sorting() );
   }
}

/*
 * void addSearchPath ( const QString & prefix, const QString & path )
 */
HB_FUNC( QT_QDIR_ADDSEARCHPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      ( p )->addSearchPath( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString cleanPath ( const QString & path )
 */
HB_FUNC( QT_QDIR_CLEANPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->cleanPath( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QDir current ()
 */
HB_FUNC( QT_QDIR_CURRENT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->current() ), true ) );
   }
}

/*
 * QString currentPath ()
 */
HB_FUNC( QT_QDIR_CURRENTPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->currentPath().toUtf8().data() );
   }
}

/*
 * QString fromNativeSeparators ( const QString & pathName )
 */
HB_FUNC( QT_QDIR_FROMNATIVESEPARATORS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->fromNativeSeparators( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QDir home ()
 */
HB_FUNC( QT_QDIR_HOME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->home() ), true ) );
   }
}

/*
 * QString homePath ()
 */
HB_FUNC( QT_QDIR_HOMEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->homePath().toUtf8().data() );
   }
}

/*
 * bool isAbsolutePath ( const QString & path )
 */
HB_FUNC( QT_QDIR_ISABSOLUTEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isAbsolutePath( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool isRelativePath ( const QString & path )
 */
HB_FUNC( QT_QDIR_ISRELATIVEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isRelativePath( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool match ( const QString & filter, const QString & fileName )
 */
HB_FUNC( QT_QDIR_MATCH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->match( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool match ( const QStringList & filters, const QString & fileName )
 */
HB_FUNC( QT_QDIR_MATCH_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->match( *hbqt_par_QStringList( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QDir root ()
 */
HB_FUNC( QT_QDIR_ROOT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->root() ), true ) );
   }
}

/*
 * QString rootPath ()
 */
HB_FUNC( QT_QDIR_ROOTPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->rootPath().toUtf8().data() );
   }
}

/*
 * QStringList searchPaths ( const QString & prefix )
 */
HB_FUNC( QT_QDIR_SEARCHPATHS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QChar separator ()
 */
HB_FUNC( QT_QDIR_SEPARATOR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->separator() ), true ) );
   }
}

/*
 * bool setCurrent ( const QString & path )
 */
HB_FUNC( QT_QDIR_SETCURRENT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->setCurrent( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * void setSearchPaths ( const QString & prefix, const QStringList & searchPaths )
 */
HB_FUNC( QT_QDIR_SETSEARCHPATHS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      ( p )->setSearchPaths( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * QDir temp ()
 */
HB_FUNC( QT_QDIR_TEMP )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->temp() ), true ) );
   }
}

/*
 * QString tempPath ()
 */
HB_FUNC( QT_QDIR_TEMPPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->tempPath().toUtf8().data() );
   }
}

/*
 * QString toNativeSeparators ( const QString & pathName )
 */
HB_FUNC( QT_QDIR_TONATIVESEPARATORS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->toNativeSeparators( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
