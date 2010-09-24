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


#include "hbclass.ch"


FUNCTION QDir( ... )
   RETURN HB_QDir():new( ... )


CREATE CLASS QDir INHERIT HbQtObjectHandler FUNCTION HB_QDir

   METHOD  new( ... )

   METHOD  absoluteFilePath( cFileName )
   METHOD  absolutePath()
   METHOD  canonicalPath()
   METHOD  cd( cDirName )
   METHOD  cdUp()
   METHOD  count()
   METHOD  dirName()
   METHOD  entryList( ... )
   METHOD  exists( ... )
   METHOD  filePath( cFileName )
   METHOD  filter()
   METHOD  isAbsolute()
   METHOD  isReadable()
   METHOD  isRelative()
   METHOD  isRoot()
   METHOD  makeAbsolute()
   METHOD  mkdir( cDirName )
   METHOD  mkpath( cDirPath )
   METHOD  nameFilters()
   METHOD  path()
   METHOD  refresh()
   METHOD  relativeFilePath( cFileName )
   METHOD  remove( cFileName )
   METHOD  rename( cOldName, cNewName )
   METHOD  rmdir( cDirName )
   METHOD  rmpath( cDirPath )
   METHOD  setFilter( nFilters )
   METHOD  setNameFilters( pNameFilters )
   METHOD  setPath( cPath )
   METHOD  setSorting( nSort )
   METHOD  sorting()
   METHOD  addSearchPath( cPrefix, cPath )
   METHOD  cleanPath( cPath )
   METHOD  current()
   METHOD  currentPath()
   METHOD  fromNativeSeparators( cPathName )
   METHOD  home()
   METHOD  homePath()
   METHOD  isAbsolutePath( cPath )
   METHOD  isRelativePath( cPath )
   METHOD  match( ... )
   METHOD  root()
   METHOD  rootPath()
   METHOD  searchPaths( cPrefix )
   METHOD  separator()
   METHOD  setCurrent( cPath )
   METHOD  setSearchPaths( cPrefix, pSearchPaths )
   METHOD  temp()
   METHOD  tempPath()
   METHOD  toNativeSeparators( cPathName )

   ENDCLASS


METHOD QDir:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDir( ... )
   RETURN Self


METHOD QDir:absoluteFilePath( cFileName )
   RETURN Qt_QDir_absoluteFilePath( ::pPtr, cFileName )


METHOD QDir:absolutePath()
   RETURN Qt_QDir_absolutePath( ::pPtr )


METHOD QDir:canonicalPath()
   RETURN Qt_QDir_canonicalPath( ::pPtr )


METHOD QDir:cd( cDirName )
   RETURN Qt_QDir_cd( ::pPtr, cDirName )


METHOD QDir:cdUp()
   RETURN Qt_QDir_cdUp( ::pPtr )


METHOD QDir:count()
   RETURN Qt_QDir_count( ::pPtr )


METHOD QDir:dirName()
   RETURN Qt_QDir_dirName( ::pPtr )


METHOD QDir:entryList( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QStringList entryList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
                // PO p QStringList, N n QDir::Filters, N n QDir::SortFlags
         RETURN QStringList():from( Qt_QDir_entryList( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QStringList entryList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
                // N n QDir::Filters, N n QDir::SortFlags
         RETURN QStringList():from( Qt_QDir_entryList_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QStringList entryList ( const QStringList & nameFilters, Filters filters = NoFilter, SortFlags sort = NoSort ) const
                // PO p QStringList, N n QDir::Filters, N n QDir::SortFlags
         RETURN QStringList():from( Qt_QDir_entryList( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QStringList entryList ( Filters filters = NoFilter, SortFlags sort = NoSort ) const
             // N n QDir::Filters, N n QDir::SortFlags
      RETURN QStringList():from( Qt_QDir_entryList_1( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QDir:exists( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool exists ( const QString & name ) const
                // C c QString
         RETURN Qt_QDir_exists( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // bool exists () const
      RETURN Qt_QDir_exists_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QDir:filePath( cFileName )
   RETURN Qt_QDir_filePath( ::pPtr, cFileName )


METHOD QDir:filter()
   RETURN Qt_QDir_filter( ::pPtr )


METHOD QDir:isAbsolute()
   RETURN Qt_QDir_isAbsolute( ::pPtr )


METHOD QDir:isReadable()
   RETURN Qt_QDir_isReadable( ::pPtr )


METHOD QDir:isRelative()
   RETURN Qt_QDir_isRelative( ::pPtr )


METHOD QDir:isRoot()
   RETURN Qt_QDir_isRoot( ::pPtr )


METHOD QDir:makeAbsolute()
   RETURN Qt_QDir_makeAbsolute( ::pPtr )


METHOD QDir:mkdir( cDirName )
   RETURN Qt_QDir_mkdir( ::pPtr, cDirName )


METHOD QDir:mkpath( cDirPath )
   RETURN Qt_QDir_mkpath( ::pPtr, cDirPath )


METHOD QDir:nameFilters()
   RETURN Qt_QDir_nameFilters( ::pPtr )


METHOD QDir:path()
   RETURN Qt_QDir_path( ::pPtr )


METHOD QDir:refresh()
   RETURN Qt_QDir_refresh( ::pPtr )


METHOD QDir:relativeFilePath( cFileName )
   RETURN Qt_QDir_relativeFilePath( ::pPtr, cFileName )


METHOD QDir:remove( cFileName )
   RETURN Qt_QDir_remove( ::pPtr, cFileName )


METHOD QDir:rename( cOldName, cNewName )
   RETURN Qt_QDir_rename( ::pPtr, cOldName, cNewName )


METHOD QDir:rmdir( cDirName )
   RETURN Qt_QDir_rmdir( ::pPtr, cDirName )


METHOD QDir:rmpath( cDirPath )
   RETURN Qt_QDir_rmpath( ::pPtr, cDirPath )


METHOD QDir:setFilter( nFilters )
   RETURN Qt_QDir_setFilter( ::pPtr, nFilters )


METHOD QDir:setNameFilters( pNameFilters )
   RETURN Qt_QDir_setNameFilters( ::pPtr, hbqt_ptr( pNameFilters ) )


METHOD QDir:setPath( cPath )
   RETURN Qt_QDir_setPath( ::pPtr, cPath )


METHOD QDir:setSorting( nSort )
   RETURN Qt_QDir_setSorting( ::pPtr, nSort )


METHOD QDir:sorting()
   RETURN Qt_QDir_sorting( ::pPtr )


METHOD QDir:addSearchPath( cPrefix, cPath )
   RETURN Qt_QDir_addSearchPath( ::pPtr, cPrefix, cPath )


METHOD QDir:cleanPath( cPath )
   RETURN Qt_QDir_cleanPath( ::pPtr, cPath )


METHOD QDir:current()
   RETURN Qt_QDir_current( ::pPtr )


METHOD QDir:currentPath()
   RETURN Qt_QDir_currentPath( ::pPtr )


METHOD QDir:fromNativeSeparators( cPathName )
   RETURN Qt_QDir_fromNativeSeparators( ::pPtr, cPathName )


METHOD QDir:home()
   RETURN Qt_QDir_home( ::pPtr )


METHOD QDir:homePath()
   RETURN Qt_QDir_homePath( ::pPtr )


METHOD QDir:isAbsolutePath( cPath )
   RETURN Qt_QDir_isAbsolutePath( ::pPtr, cPath )


METHOD QDir:isRelativePath( cPath )
   RETURN Qt_QDir_isRelativePath( ::pPtr, cPath )


METHOD QDir:match( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // bool match ( const QString & filter, const QString & fileName )
                // C c QString, C c QString
         RETURN Qt_QDir_match( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "C"
                // bool match ( const QStringList & filters, const QString & fileName )
                // PO p QStringList, C c QString
         RETURN Qt_QDir_match_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QDir:root()
   RETURN Qt_QDir_root( ::pPtr )


METHOD QDir:rootPath()
   RETURN Qt_QDir_rootPath( ::pPtr )


METHOD QDir:searchPaths( cPrefix )
   RETURN Qt_QDir_searchPaths( ::pPtr, cPrefix )


METHOD QDir:separator()
   RETURN Qt_QDir_separator( ::pPtr )


METHOD QDir:setCurrent( cPath )
   RETURN Qt_QDir_setCurrent( ::pPtr, cPath )


METHOD QDir:setSearchPaths( cPrefix, pSearchPaths )
   RETURN Qt_QDir_setSearchPaths( ::pPtr, cPrefix, hbqt_ptr( pSearchPaths ) )


METHOD QDir:temp()
   RETURN Qt_QDir_temp( ::pPtr )


METHOD QDir:tempPath()
   RETURN Qt_QDir_tempPath( ::pPtr )


METHOD QDir:toNativeSeparators( cPathName )
   RETURN Qt_QDir_toNativeSeparators( ::pPtr, cPathName )

