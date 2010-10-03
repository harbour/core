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


FUNCTION QFile( ... )
   RETURN HB_QFile():new( ... )


CREATE CLASS QFile INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QFile

   METHOD  new( ... )

   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  close                         // (  )                                               -> NIL
   METHOD  copy                          // ( cNewName )                                       -> lBool
   METHOD  error                         // (  )                                               -> nFileError
   METHOD  exists                        // (  )                                               -> lBool
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  flush                         // (  )                                               -> lBool
   METHOD  handle                        // (  )                                               -> nInt
   METHOD  isSequential                  // (  )                                               -> lBool
   METHOD  link                          // ( cLinkName )                                      -> lBool
   METHOD  map                           // ( nOffset, nSize, nFlags )                         -> cUchar
   METHOD  open                          // ( nMode )                                          -> lBool
                                         // ( nFd, nMode )                                     -> lBool
   METHOD  permissions                   // (  )                                               -> nPermissions
   METHOD  remove                        // (  )                                               -> lBool
   METHOD  rename                        // ( cNewName )                                       -> lBool
   METHOD  resize                        // ( nSz )                                            -> lBool
   METHOD  setFileName                   // ( cName )                                          -> NIL
   METHOD  setPermissions                // ( nPermissions )                                   -> lBool
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  symLinkTarget                 // (  )                                               -> cQString
   METHOD  unsetError                    // (  )                                               -> NIL
                                         // ( cFileName, cNewName )                            -> lBool
   METHOD  decodeName                    // ( cLocalFileName )                                 -> cQString
   METHOD  encodeName                    // ( cFileName )                                      -> oQByteArray
                                         // ( cFileName )                                      -> lBool
                                         // ( cFileName, cLinkName )                           -> lBool
                                         // ( cFileName )                                      -> nPermissions
                                         // ( cFileName )                                      -> lBool
                                         // ( cOldName, cNewName )                             -> lBool
                                         // ( cFileName, nSz )                                 -> lBool
                                         // ( cFileName, nPermissions )                        -> lBool
                                         // ( cFileName )                                      -> cQString

   ENDCLASS


METHOD QFile:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFile( ... )
   RETURN Self


METHOD QFile:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_close( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:copy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_copy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_copy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_error( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:exists( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_exists_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_exists( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:flush( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_flush( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:handle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_handle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:isSequential( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_isSequential( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:link( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_link_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_link( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:map( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFile_map( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_map( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_open_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:permissions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_permissions_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_permissions( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_remove_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_remove( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:rename( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_rename_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_rename( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:resize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_resize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:setPermissions( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_setPermissions_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_setPermissions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_size( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:symLinkTarget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_symLinkTarget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_symLinkTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:unsetError( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_unsetError( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:decodeName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_decodeName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFile:encodeName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QByteArray():from( Qt_QFile_encodeName( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

