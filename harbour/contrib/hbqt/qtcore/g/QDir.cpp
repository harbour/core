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
#include "hbqtcore_garbage.h"

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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDir;

QT_G_FUNC( hbqt_gcRelease_QDir )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

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
      hb_retc( ( p )->absoluteFilePath( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ABSOLUTEFILEPATH FP=hb_retc( ( p )->absoluteFilePath( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QDIR_ABSOLUTEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->absolutePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ABSOLUTEPATH FP=hb_retc( ( p )->absolutePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QDIR_CANONICALPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->canonicalPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CANONICALPATH FP=hb_retc( ( p )->canonicalPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool cd ( const QString & dirName )
 */
HB_FUNC( QT_QDIR_CD )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->cd( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CD FP=hb_retl( ( p )->cd( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool cdUp ()
 */
HB_FUNC( QT_QDIR_CDUP )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->cdUp() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CDUP FP=hb_retl( ( p )->cdUp() ); p is NULL" ) );
   }
}

/*
 * uint count () const
 */
HB_FUNC( QT_QDIR_COUNT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * QString dirName () const
 */
HB_FUNC( QT_QDIR_DIRNAME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->dirName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_DIRNAME FP=hb_retc( ( p )->dirName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList entryList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( *hbqt_par_QStringList( 2 ), ( HB_ISNUM( 3 ) ? ( QDir::Filters ) hb_parni( 3 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 4 ) ? ( QDir::SortFlags ) hb_parni( 4 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ENTRYLIST FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( *hbqt_par_QStringList( 2 ), ( HB_ISNUM( 3 ) ? ( QDir::Filters ) hb_parni( 3 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 4 ) ? ( QDir::SortFlags ) hb_parni( 4 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStringList entryList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( ( HB_ISNUM( 2 ) ? ( QDir::Filters ) hb_parni( 2 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 3 ) ? ( QDir::SortFlags ) hb_parni( 3 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ENTRYLIST_1 FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->entryList( ( HB_ISNUM( 2 ) ? ( QDir::Filters ) hb_parni( 2 ) : ( QDir::Filters ) QDir::NoFilter ), ( HB_ISNUM( 3 ) ? ( QDir::SortFlags ) hb_parni( 3 ) : ( QDir::SortFlags ) QDir::NoSort ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool exists ( const QString & name ) const
 */
HB_FUNC( QT_QDIR_EXISTS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->exists( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_EXISTS FP=hb_retl( ( p )->exists( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QDIR_EXISTS_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->exists() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_EXISTS_1 FP=hb_retl( ( p )->exists() ); p is NULL" ) );
   }
}

/*
 * QString filePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_FILEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->filePath( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_FILEPATH FP=hb_retc( ( p )->filePath( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Filters filter () const
 */
HB_FUNC( QT_QDIR_FILTER )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retni( ( QDir::Filters ) ( p )->filter() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_FILTER FP=hb_retni( ( QDir::Filters ) ( p )->filter() ); p is NULL" ) );
   }
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QDIR_ISABSOLUTE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isAbsolute() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISABSOLUTE FP=hb_retl( ( p )->isAbsolute() ); p is NULL" ) );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QDIR_ISREADABLE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isReadable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISREADABLE FP=hb_retl( ( p )->isReadable() ); p is NULL" ) );
   }
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QDIR_ISRELATIVE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isRelative() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISRELATIVE FP=hb_retl( ( p )->isRelative() ); p is NULL" ) );
   }
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QDIR_ISROOT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isRoot() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISROOT FP=hb_retl( ( p )->isRoot() ); p is NULL" ) );
   }
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QDIR_MAKEABSOLUTE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->makeAbsolute() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_MAKEABSOLUTE FP=hb_retl( ( p )->makeAbsolute() ); p is NULL" ) );
   }
}

/*
 * bool mkdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_MKDIR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->mkdir( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_MKDIR FP=hb_retl( ( p )->mkdir( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool mkpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_MKPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->mkpath( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_MKPATH FP=hb_retl( ( p )->mkpath( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QDIR_NAMEFILTERS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_NAMEFILTERS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->nameFilters() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString path () const
 */
HB_FUNC( QT_QDIR_PATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->path().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_PATH FP=hb_retc( ( p )->path().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void refresh () const
 */
HB_FUNC( QT_QDIR_REFRESH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->refresh();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_REFRESH FP=( p )->refresh(); p is NULL" ) );
   }
}

/*
 * QString relativeFilePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_RELATIVEFILEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->relativeFilePath( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_RELATIVEFILEPATH FP=hb_retc( ( p )->relativeFilePath( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool remove ( const QString & fileName )
 */
HB_FUNC( QT_QDIR_REMOVE )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->remove( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_REMOVE FP=hb_retl( ( p )->remove( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool rename ( const QString & oldName, const QString & newName )
 */
HB_FUNC( QT_QDIR_RENAME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->rename( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_RENAME FP=hb_retl( ( p )->rename( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool rmdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_RMDIR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->rmdir( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_RMDIR FP=hb_retl( ( p )->rmdir( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool rmpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_RMPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->rmpath( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_RMPATH FP=hb_retl( ( p )->rmpath( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setFilter ( Filters filters )
 */
HB_FUNC( QT_QDIR_SETFILTER )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETFILTER FP=( p )->setFilter( ( QDir::Filters ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNameFilters ( const QStringList & nameFilters )
 */
HB_FUNC( QT_QDIR_SETNAMEFILTERS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->setNameFilters( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETNAMEFILTERS FP=( p )->setNameFilters( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPath ( const QString & path )
 */
HB_FUNC( QT_QDIR_SETPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->setPath( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETPATH FP=( p )->setPath( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSorting ( SortFlags sort )
 */
HB_FUNC( QT_QDIR_SETSORTING )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->setSorting( ( QDir::SortFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETSORTING FP=( p )->setSorting( ( QDir::SortFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * SortFlags sorting () const
 */
HB_FUNC( QT_QDIR_SORTING )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retni( ( QDir::SortFlags ) ( p )->sorting() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SORTING FP=hb_retni( ( QDir::SortFlags ) ( p )->sorting() ); p is NULL" ) );
   }
}

/*
 * void addSearchPath ( const QString & prefix, const QString & path )
 */
HB_FUNC( QT_QDIR_ADDSEARCHPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->addSearchPath( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ADDSEARCHPATH FP=( p )->addSearchPath( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QString cleanPath ( const QString & path )
 */
HB_FUNC( QT_QDIR_CLEANPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->cleanPath( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CLEANPATH FP=hb_retc( ( p )->cleanPath( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDir current ()
 */
HB_FUNC( QT_QDIR_CURRENT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->current() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CURRENT FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->current() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString currentPath ()
 */
HB_FUNC( QT_QDIR_CURRENTPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->currentPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_CURRENTPATH FP=hb_retc( ( p )->currentPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString fromNativeSeparators ( const QString & pathName )
 */
HB_FUNC( QT_QDIR_FROMNATIVESEPARATORS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->fromNativeSeparators( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_FROMNATIVESEPARATORS FP=hb_retc( ( p )->fromNativeSeparators( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QDir home ()
 */
HB_FUNC( QT_QDIR_HOME )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->home() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_HOME FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->home() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString homePath ()
 */
HB_FUNC( QT_QDIR_HOMEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->homePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_HOMEPATH FP=hb_retc( ( p )->homePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool isAbsolutePath ( const QString & path )
 */
HB_FUNC( QT_QDIR_ISABSOLUTEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isAbsolutePath( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISABSOLUTEPATH FP=hb_retl( ( p )->isAbsolutePath( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isRelativePath ( const QString & path )
 */
HB_FUNC( QT_QDIR_ISRELATIVEPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->isRelativePath( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ISRELATIVEPATH FP=hb_retl( ( p )->isRelativePath( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool match ( const QString & filter, const QString & fileName )
 */
HB_FUNC( QT_QDIR_MATCH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->match( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_MATCH FP=hb_retl( ( p )->match( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool match ( const QStringList & filters, const QString & fileName )
 */
HB_FUNC( QT_QDIR_MATCH_1 )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->match( *hbqt_par_QStringList( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_MATCH_1 FP=hb_retl( ( p )->match( *hbqt_par_QStringList( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QDir root ()
 */
HB_FUNC( QT_QDIR_ROOT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->root() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ROOT FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->root() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString rootPath ()
 */
HB_FUNC( QT_QDIR_ROOTPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->rootPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_ROOTPATH FP=hb_retc( ( p )->rootPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList searchPaths ( const QString & prefix )
 */
HB_FUNC( QT_QDIR_SEARCHPATHS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SEARCHPATHS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QChar separator ()
 */
HB_FUNC( QT_QDIR_SEPARATOR )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->separator() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->separator() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool setCurrent ( const QString & path )
 */
HB_FUNC( QT_QDIR_SETCURRENT )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retl( ( p )->setCurrent( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETCURRENT FP=hb_retl( ( p )->setCurrent( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setSearchPaths ( const QString & prefix, const QStringList & searchPaths )
 */
HB_FUNC( QT_QDIR_SETSEARCHPATHS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      ( p )->setSearchPaths( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_SETSEARCHPATHS FP=( p )->setSearchPaths( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * QDir temp ()
 */
HB_FUNC( QT_QDIR_TEMP )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->temp() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_TEMP FP=hb_retptrGC( hbqt_gcAllocate_QDir( new QDir( ( p )->temp() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString tempPath ()
 */
HB_FUNC( QT_QDIR_TEMPPATH )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->tempPath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_TEMPPATH FP=hb_retc( ( p )->tempPath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toNativeSeparators ( const QString & pathName )
 */
HB_FUNC( QT_QDIR_TONATIVESEPARATORS )
{
   QDir * p = hbqt_par_QDir( 1 );
   if( p )
      hb_retc( ( p )->toNativeSeparators( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDIR_TONATIVESEPARATORS FP=hb_retc( ( p )->toNativeSeparators( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
