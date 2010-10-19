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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QDir( ... )
   RETURN HB_QDir():new( ... )

FUNCTION QDirFromPointer( ... )
   RETURN HB_QDir():fromPointer( ... )


CREATE CLASS QDir INHERIT HbQtObjectHandler FUNCTION HB_QDir

   METHOD  new( ... )

   METHOD  absoluteFilePath              // ( cFileName )                                      -> cQString
   METHOD  absolutePath                  // (  )                                               -> cQString
   METHOD  canonicalPath                 // (  )                                               -> cQString
   METHOD  cd                            // ( cDirName )                                       -> lBool
   METHOD  cdUp                          // (  )                                               -> lBool
   METHOD  count                         // (  )                                               -> nUint
   METHOD  dirName                       // (  )                                               -> cQString
   METHOD  entryList                     // ( oQStringList, nFilters, nSort )                  -> oQStringList
                                         // ( nFilters, nSort )                                -> oQStringList
   METHOD  exists                        // ( cName )                                          -> lBool
                                         // (  )                                               -> lBool
   METHOD  filePath                      // ( cFileName )                                      -> cQString
   METHOD  filter                        // (  )                                               -> nFilters
   METHOD  isAbsolute                    // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isRelative                    // (  )                                               -> lBool
   METHOD  isRoot                        // (  )                                               -> lBool
   METHOD  makeAbsolute                  // (  )                                               -> lBool
   METHOD  mkdir                         // ( cDirName )                                       -> lBool
   METHOD  mkpath                        // ( cDirPath )                                       -> lBool
   METHOD  nameFilters                   // (  )                                               -> oQStringList
   METHOD  path                          // (  )                                               -> cQString
   METHOD  refresh                       // (  )                                               -> NIL
   METHOD  relativeFilePath              // ( cFileName )                                      -> cQString
   METHOD  remove                        // ( cFileName )                                      -> lBool
   METHOD  rename                        // ( cOldName, cNewName )                             -> lBool
   METHOD  rmdir                         // ( cDirName )                                       -> lBool
   METHOD  rmpath                        // ( cDirPath )                                       -> lBool
   METHOD  setFilter                     // ( nFilters )                                       -> NIL
   METHOD  setNameFilters                // ( oQStringList )                                   -> NIL
   METHOD  setPath                       // ( cPath )                                          -> NIL
   METHOD  setSorting                    // ( nSort )                                          -> NIL
   METHOD  sorting                       // (  )                                               -> nSortFlags
   METHOD  addSearchPath                 // ( cPrefix, cPath )                                 -> NIL
   METHOD  cleanPath                     // ( cPath )                                          -> cQString
   METHOD  current                       // (  )                                               -> oQDir
   METHOD  currentPath                   // (  )                                               -> cQString
   METHOD  fromNativeSeparators          // ( cPathName )                                      -> cQString
   METHOD  home                          // (  )                                               -> oQDir
   METHOD  homePath                      // (  )                                               -> cQString
   METHOD  isAbsolutePath                // ( cPath )                                          -> lBool
   METHOD  isRelativePath                // ( cPath )                                          -> lBool
   METHOD  match                         // ( cFilter, cFileName )                             -> lBool
                                         // ( oQStringList, cFileName )                        -> lBool
   METHOD  root                          // (  )                                               -> oQDir
   METHOD  rootPath                      // (  )                                               -> cQString
   METHOD  searchPaths                   // ( cPrefix )                                        -> oQStringList
   METHOD  separator                     // (  )                                               -> oQChar
   METHOD  setCurrent                    // ( cPath )                                          -> lBool
   METHOD  setSearchPaths                // ( cPrefix, oQStringList )                          -> NIL
   METHOD  temp                          // (  )                                               -> oQDir
   METHOD  tempPath                      // (  )                                               -> cQString
   METHOD  toNativeSeparators            // ( cPathName )                                      -> cQString

   ENDCLASS


METHOD QDir:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDir( ... )
   RETURN Self


METHOD QDir:absoluteFilePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_absoluteFilePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:absolutePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_absolutePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:canonicalPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_canonicalPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:cd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_cd( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:cdUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_cdUp( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:dirName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_dirName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:entryList( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QStringListFromPointer( Qt_QDir_entryList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStringListFromPointer( Qt_QDir_entryList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStringListFromPointer( Qt_QDir_entryList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QDir_entryList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QDir_entryList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QStringListFromPointer( Qt_QDir_entryList_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:exists( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_exists( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDir_exists_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:filePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_filePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:filter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_filter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_isAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isRelative( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_isRelative( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isRoot( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_isRoot( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:makeAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_makeAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:mkdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_mkdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:mkpath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_mkpath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:nameFilters( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDir_nameFilters( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_path( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:refresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:relativeFilePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_relativeFilePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:rename( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QDir_rename( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:rmdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_rmdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:rmpath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_rmpath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDir_setFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setNameFilters( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDir_setNameFilters( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_setPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setSorting( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDir_setSorting( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:sorting( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_sorting( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:addSearchPath( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QDir_addSearchPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:cleanPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_cleanPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:current( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QDir_current( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:currentPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_currentPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:fromNativeSeparators( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_fromNativeSeparators( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:home( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QDir_home( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:homePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_homePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isAbsolutePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_isAbsolutePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:isRelativePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_isRelativePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:match( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QDir_match( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QDir_match_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:root( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QDir_root( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:rootPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_rootPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:searchPaths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QDir_searchPaths( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:separator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QDir_separator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setCurrent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_setCurrent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:setSearchPaths( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QDir_setSearchPaths( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:temp( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QDir_temp( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:tempPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDir_tempPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDir:toNativeSeparators( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDir_toNativeSeparators( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

