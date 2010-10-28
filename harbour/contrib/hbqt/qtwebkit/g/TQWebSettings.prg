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


REQUEST __HBQTWEBKIT


FUNCTION QWebSettings( ... )
   RETURN HB_QWebSettings():new( ... )

FUNCTION QWebSettingsFromPointer( ... )
   RETURN HB_QWebSettings():fromPointer( ... )


CREATE CLASS QWebSettings INHERIT HbQtObjectHandler FUNCTION HB_QWebSettings

   METHOD  new( ... )

   METHOD  fontFamily                    // ( nWhich )                                         -> cQString
   METHOD  fontSize                      // ( nType )                                          -> nInt
   METHOD  resetAttribute                // ( nAttribute )                                     -> NIL
   METHOD  resetFontFamily               // ( nWhich )                                         -> NIL
   METHOD  resetFontSize                 // ( nType )                                          -> NIL
   METHOD  setAttribute                  // ( nAttribute, lOn )                                -> NIL
   METHOD  setFontFamily                 // ( nWhich, cFamily )                                -> NIL
   METHOD  setFontSize                   // ( nType, nSize )                                   -> NIL
   METHOD  setUserStyleSheetUrl          // ( oQUrl )                                          -> NIL
   METHOD  testAttribute                 // ( nAttribute )                                     -> lBool
   METHOD  userStyleSheetUrl             // (  )                                               -> oQUrl
   METHOD  clearIconDatabase             // (  )                                               -> NIL
   METHOD  globalSettings                // (  )                                               -> oQWebSettings
   METHOD  iconDatabasePath              // (  )                                               -> cQString
   METHOD  iconForUrl                    // ( oQUrl )                                          -> oQIcon
   METHOD  maximumPagesInCache           // (  )                                               -> nInt
   METHOD  offlineStorageDefaultQuota    // (  )                                               -> nQint64
   METHOD  offlineStoragePath            // (  )                                               -> cQString
   METHOD  setIconDatabasePath           // ( cPath )                                          -> NIL
   METHOD  setMaximumPagesInCache        // ( nPages )                                         -> NIL
   METHOD  setObjectCacheCapacities      // ( nCacheMinDeadCapacity, nCacheMaxDead, nTotalCapacity ) -> NIL
   METHOD  setOfflineStorageDefaultQuota // ( nMaximumSize )                                   -> NIL
   METHOD  setOfflineStoragePath         // ( cPath )                                          -> NIL
   METHOD  setWebGraphic                 // ( nType, oQPixmap )                                -> NIL
   METHOD  webGraphic                    // ( nType )                                          -> oQPixmap

   ENDCLASS


METHOD QWebSettings:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebSettings( ... )
   RETURN Self


METHOD QWebSettings:fontFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_fontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:fontSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_fontSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:resetAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_resetAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:resetFontFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_resetFontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:resetFontSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_resetFontSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setAttribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWebSettings_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setFontFamily( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWebSettings_setFontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setFontSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebSettings_setFontSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setUserStyleSheetUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_setUserStyleSheetUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:testAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_testAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:userStyleSheetUrl( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebSettings_userStyleSheetUrl( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:clearIconDatabase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSettings_clearIconDatabase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:globalSettings( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebSettingsFromPointer( Qt_QWebSettings_globalSettings( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:iconDatabasePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSettings_iconDatabasePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:iconForUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QWebSettings_iconForUrl( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:maximumPagesInCache( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSettings_maximumPagesInCache( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:offlineStorageDefaultQuota( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSettings_offlineStorageDefaultQuota( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:offlineStoragePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSettings_offlineStoragePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setIconDatabasePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_setIconDatabasePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setMaximumPagesInCache( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_setMaximumPagesInCache( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setObjectCacheCapacities( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QWebSettings_setObjectCacheCapacities( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setOfflineStorageDefaultQuota( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_setOfflineStorageDefaultQuota( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setOfflineStoragePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebSettings_setOfflineStoragePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:setWebGraphic( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWebSettings_setWebGraphic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSettings:webGraphic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QWebSettings_webGraphic( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

