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


FUNCTION QFileInfo( ... )
   RETURN HB_QFileInfo():new( ... )

FUNCTION QFileInfoFrom( ... )
   RETURN HB_QFileInfo():from( ... )

FUNCTION QFileInfoFromPointer( ... )
   RETURN HB_QFileInfo():fromPointer( ... )


CREATE CLASS QFileInfo INHERIT HbQtObjectHandler FUNCTION HB_QFileInfo

   METHOD  new( ... )

   METHOD  absoluteDir                   // (  )                                               -> oQDir
   METHOD  absoluteFilePath              // (  )                                               -> cQString
   METHOD  absolutePath                  // (  )                                               -> cQString
   METHOD  baseName                      // (  )                                               -> cQString
   METHOD  bundleName                    // (  )                                               -> cQString
   METHOD  caching                       // (  )                                               -> lBool
   METHOD  canonicalFilePath             // (  )                                               -> cQString
   METHOD  canonicalPath                 // (  )                                               -> cQString
   METHOD  completeBaseName              // (  )                                               -> cQString
   METHOD  completeSuffix                // (  )                                               -> cQString
   METHOD  created                       // (  )                                               -> oQDateTime
   METHOD  dir                           // (  )                                               -> oQDir
   METHOD  exists                        // (  )                                               -> lBool
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  filePath                      // (  )                                               -> cQString
   METHOD  group                         // (  )                                               -> cQString
   METHOD  groupId                       // (  )                                               -> nUint
   METHOD  isAbsolute                    // (  )                                               -> lBool
   METHOD  isBundle                      // (  )                                               -> lBool
   METHOD  isDir                         // (  )                                               -> lBool
   METHOD  isExecutable                  // (  )                                               -> lBool
   METHOD  isFile                        // (  )                                               -> lBool
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isRelative                    // (  )                                               -> lBool
   METHOD  isRoot                        // (  )                                               -> lBool
   METHOD  isSymLink                     // (  )                                               -> lBool
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  lastModified                  // (  )                                               -> oQDateTime
   METHOD  lastRead                      // (  )                                               -> oQDateTime
   METHOD  makeAbsolute                  // (  )                                               -> lBool
   METHOD  owner                         // (  )                                               -> cQString
   METHOD  ownerId                       // (  )                                               -> nUint
   METHOD  path                          // (  )                                               -> cQString
   METHOD  permission                    // ( nPermissions )                                   -> lBool
   METHOD  permissions                   // (  )                                               -> nQFile_Permissions
   METHOD  refresh                       // (  )                                               -> NIL
   METHOD  setCaching                    // ( lEnable )                                        -> NIL
   METHOD  setFile                       // ( cFile )                                          -> NIL
                                         // ( oQFile )                                         -> NIL
                                         // ( oQDir, cFile )                                   -> NIL
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  suffix                        // (  )                                               -> cQString
   METHOD  symLinkTarget                 // (  )                                               -> cQString

   ENDCLASS


METHOD QFileInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileInfo( ... )
   RETURN Self


METHOD QFileInfo:absoluteDir( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileInfo_absoluteDir( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:absoluteFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_absoluteFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:absolutePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_absolutePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:baseName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_baseName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:bundleName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_bundleName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:caching( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_caching( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:canonicalFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_canonicalFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:canonicalPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_canonicalPath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:completeBaseName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_completeBaseName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:completeSuffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_completeSuffix( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:created( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_created( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:dir( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileInfo_dir( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:exists( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_exists( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:filePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_filePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:group( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_group( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:groupId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_groupId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isBundle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isBundle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isDir( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isDir( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isExecutable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isExecutable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isFile( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isFile( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isRelative( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isRelative( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isRoot( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isRoot( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isSymLink( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isSymLink( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:lastModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_lastModified( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:lastRead( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_lastRead( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:makeAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_makeAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:owner( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_owner( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:ownerId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_ownerId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_path( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:permission( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_permission( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:permissions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_permissions( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:refresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:setCaching( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setCaching( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:setFile( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFileInfo_setFile_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setFile( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setFile_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_size( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:suffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_suffix( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileInfo:symLinkTarget( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_symLinkTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

