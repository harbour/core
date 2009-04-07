/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include <QtCore/QDir>


/*
 * QDir ( const QDir & dir )
 * QDir ( const QString & path = QString() )
 * QDir ( const QString & path, const QString & nameFilter, SortFlags sort = SortFlags( Name | IgnoreCase ), Filters filters = AllEntries )
 * ~QDir ()
 */
HB_FUNC( QT_QDIR )
{
   hb_retptr( new QDir( hbqt_par_QString( 1 ) ) );
}

/*
 * QString absoluteFilePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_ABSOLUTEFILEPATH )
{
   hb_retc( hbqt_par_QDir( 1 )->absoluteFilePath( hbqt_par_QString( 2 ) ).toLatin1().data() );
}

/*
 * QString absolutePath () const
 */
HB_FUNC( QT_QDIR_ABSOLUTEPATH )
{
   hb_retc( hbqt_par_QDir( 1 )->absolutePath().toLatin1().data() );
}

/*
 * QString canonicalPath () const
 */
HB_FUNC( QT_QDIR_CANONICALPATH )
{
   hb_retc( hbqt_par_QDir( 1 )->canonicalPath().toLatin1().data() );
}

/*
 * bool cd ( const QString & dirName )
 */
HB_FUNC( QT_QDIR_CD )
{
   hb_retl( hbqt_par_QDir( 1 )->cd( hbqt_par_QString( 2 ) ) );
}

/*
 * bool cdUp ()
 */
HB_FUNC( QT_QDIR_CDUP )
{
   hb_retl( hbqt_par_QDir( 1 )->cdUp() );
}

/*
 * uint count () const
 */
HB_FUNC( QT_QDIR_COUNT )
{
   hb_retni( hbqt_par_QDir( 1 )->count() );
}

/*
 * QString dirName () const
 */
HB_FUNC( QT_QDIR_DIRNAME )
{
   hb_retc( hbqt_par_QDir( 1 )->dirName().toLatin1().data() );
}

/*
 * QFileInfoList entryInfoList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYINFOLIST )
{
   hb_retptr( new QFileInfoList( hbqt_par_QDir( 1 )->entryInfoList( *hbqt_par_QStringList( 2 ), ( HB_ISNIL( 3 ) ? ( QDir::Filters ) QDir::NoFilter : ( QDir::Filters ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QDir::SortFlags ) QDir::NoSort : ( QDir::SortFlags ) hb_parni( 4 ) ) ) ) );
}

/*
 * QFileInfoList entryInfoList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYINFOLIST_1 )
{
   hb_retptr( new QFileInfoList( hbqt_par_QDir( 1 )->entryInfoList( ( HB_ISNIL( 2 ) ? ( QDir::Filters ) QDir::NoFilter : ( QDir::Filters ) hb_parni( 2 ) ), ( HB_ISNIL( 3 ) ? ( QDir::SortFlags ) QDir::NoSort : ( QDir::SortFlags ) hb_parni( 3 ) ) ) ) );
}

/*
 * QStringList entryList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST )
{
   hb_retptr( new QStringList( hbqt_par_QDir( 1 )->entryList( *hbqt_par_QStringList( 2 ), ( HB_ISNIL( 3 ) ? ( QDir::Filters ) QDir::NoFilter : ( QDir::Filters ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QDir::SortFlags ) QDir::NoSort : ( QDir::SortFlags ) hb_parni( 4 ) ) ) ) );
}

/*
 * QStringList entryList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
 */
HB_FUNC( QT_QDIR_ENTRYLIST_1 )
{
   hb_retptr( new QStringList( hbqt_par_QDir( 1 )->entryList( ( HB_ISNIL( 2 ) ? ( QDir::Filters ) QDir::NoFilter : ( QDir::Filters ) hb_parni( 2 ) ), ( HB_ISNIL( 3 ) ? ( QDir::SortFlags ) QDir::NoSort : ( QDir::SortFlags ) hb_parni( 3 ) ) ) ) );
}

/*
 * bool exists ( const QString & name ) const
 */
HB_FUNC( QT_QDIR_EXISTS )
{
   hb_retl( hbqt_par_QDir( 1 )->exists( hbqt_par_QString( 2 ) ) );
}

/*
 * bool exists () const
 */
HB_FUNC( QT_QDIR_EXISTS_1 )
{
   hb_retl( hbqt_par_QDir( 1 )->exists() );
}

/*
 * QString filePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_FILEPATH )
{
   hb_retc( hbqt_par_QDir( 1 )->filePath( hbqt_par_QString( 2 ) ).toLatin1().data() );
}

/*
 * Filters filter () const
 */
HB_FUNC( QT_QDIR_FILTER )
{
   hb_retni( ( QDir::Filters ) hbqt_par_QDir( 1 )->filter() );
}

/*
 * bool isAbsolute () const
 */
HB_FUNC( QT_QDIR_ISABSOLUTE )
{
   hb_retl( hbqt_par_QDir( 1 )->isAbsolute() );
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QDIR_ISREADABLE )
{
   hb_retl( hbqt_par_QDir( 1 )->isReadable() );
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QDIR_ISRELATIVE )
{
   hb_retl( hbqt_par_QDir( 1 )->isRelative() );
}

/*
 * bool isRoot () const
 */
HB_FUNC( QT_QDIR_ISROOT )
{
   hb_retl( hbqt_par_QDir( 1 )->isRoot() );
}

/*
 * bool makeAbsolute ()
 */
HB_FUNC( QT_QDIR_MAKEABSOLUTE )
{
   hb_retl( hbqt_par_QDir( 1 )->makeAbsolute() );
}

/*
 * bool mkdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_MKDIR )
{
   hb_retl( hbqt_par_QDir( 1 )->mkdir( hbqt_par_QString( 2 ) ) );
}

/*
 * bool mkpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_MKPATH )
{
   hb_retl( hbqt_par_QDir( 1 )->mkpath( hbqt_par_QString( 2 ) ) );
}

/*
 * QStringList nameFilters () const
 */
HB_FUNC( QT_QDIR_NAMEFILTERS )
{
   hb_retptr( new QStringList( hbqt_par_QDir( 1 )->nameFilters() ) );
}

/*
 * QString path () const
 */
HB_FUNC( QT_QDIR_PATH )
{
   hb_retc( hbqt_par_QDir( 1 )->path().toLatin1().data() );
}

/*
 * void refresh () const
 */
HB_FUNC( QT_QDIR_REFRESH )
{
   hbqt_par_QDir( 1 )->refresh();
}

/*
 * QString relativeFilePath ( const QString & fileName ) const
 */
HB_FUNC( QT_QDIR_RELATIVEFILEPATH )
{
   hb_retc( hbqt_par_QDir( 1 )->relativeFilePath( hbqt_par_QString( 2 ) ).toLatin1().data() );
}

/*
 * bool remove ( const QString & fileName )
 */
HB_FUNC( QT_QDIR_REMOVE )
{
   hb_retl( hbqt_par_QDir( 1 )->remove( hbqt_par_QString( 2 ) ) );
}

/*
 * bool rename ( const QString & oldName, const QString & newName )
 */
HB_FUNC( QT_QDIR_RENAME )
{
   hb_retl( hbqt_par_QDir( 1 )->rename( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * bool rmdir ( const QString & dirName ) const
 */
HB_FUNC( QT_QDIR_RMDIR )
{
   hb_retl( hbqt_par_QDir( 1 )->rmdir( hbqt_par_QString( 2 ) ) );
}

/*
 * bool rmpath ( const QString & dirPath ) const
 */
HB_FUNC( QT_QDIR_RMPATH )
{
   hb_retl( hbqt_par_QDir( 1 )->rmpath( hbqt_par_QString( 2 ) ) );
}

/*
 * void setFilter ( Filters filters )
 */
HB_FUNC( QT_QDIR_SETFILTER )
{
   hbqt_par_QDir( 1 )->setFilter( ( QDir::Filters ) hb_parni( 2 ) );
}

/*
 * void setNameFilters ( const QStringList & nameFilters )
 */
HB_FUNC( QT_QDIR_SETNAMEFILTERS )
{
   hbqt_par_QDir( 1 )->setNameFilters( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setPath ( const QString & path )
 */
HB_FUNC( QT_QDIR_SETPATH )
{
   hbqt_par_QDir( 1 )->setPath( hbqt_par_QString( 2 ) );
}

/*
 * void setSorting ( SortFlags sort )
 */
HB_FUNC( QT_QDIR_SETSORTING )
{
   hbqt_par_QDir( 1 )->setSorting( ( QDir::SortFlags ) hb_parni( 2 ) );
}

/*
 * SortFlags sorting () const
 */
HB_FUNC( QT_QDIR_SORTING )
{
   hb_retni( ( QDir::SortFlags ) hbqt_par_QDir( 1 )->sorting() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

