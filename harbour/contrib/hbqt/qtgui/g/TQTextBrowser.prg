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


FUNCTION QTextBrowser( ... )
   RETURN HB_QTextBrowser():new( ... )

FUNCTION QTextBrowserFromPointer( ... )
   RETURN HB_QTextBrowser():fromPointer( ... )


CREATE CLASS QTextBrowser INHERIT HbQtObjectHandler, HB_QTextEdit FUNCTION HB_QTextBrowser

   METHOD  new( ... )

   METHOD  backwardHistoryCount          // (  )                                               -> nInt
   METHOD  clearHistory                  // (  )                                               -> NIL
   METHOD  forwardHistoryCount           // (  )                                               -> nInt
   METHOD  historyTitle                  // ( nI )                                             -> cQString
   METHOD  historyUrl                    // ( nI )                                             -> oQUrl
   METHOD  isBackwardAvailable           // (  )                                               -> lBool
   METHOD  isForwardAvailable            // (  )                                               -> lBool
   METHOD  loadResource                  // ( nType, oQUrl )                                   -> oQVariant
   METHOD  openExternalLinks             // (  )                                               -> lBool
   METHOD  openLinks                     // (  )                                               -> lBool
   METHOD  searchPaths                   // (  )                                               -> oQStringList
   METHOD  setOpenExternalLinks          // ( lOpen )                                          -> NIL
   METHOD  setOpenLinks                  // ( lOpen )                                          -> NIL
   METHOD  setSearchPaths                // ( oQStringList )                                   -> NIL
   METHOD  source                        // (  )                                               -> oQUrl
   METHOD  backward                      // (  )                                               -> NIL
   METHOD  forward                       // (  )                                               -> NIL
   METHOD  home                          // (  )                                               -> NIL
   METHOD  reload                        // (  )                                               -> NIL
   METHOD  setSource                     // ( oQUrl )                                          -> NIL

   ENDCLASS


METHOD QTextBrowser:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBrowser( ... )
   RETURN Self


METHOD QTextBrowser:backwardHistoryCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_backwardHistoryCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:clearHistory( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_clearHistory( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:forwardHistoryCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_forwardHistoryCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:historyTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_historyTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:historyUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QUrlFromPointer( Qt_QTextBrowser_historyUrl( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:isBackwardAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_isBackwardAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:isForwardAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_isForwardAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:loadResource( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QTextBrowser_loadResource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:openExternalLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_openExternalLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:openLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_openLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:searchPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QTextBrowser_searchPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setOpenExternalLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setOpenExternalLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setOpenLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setOpenLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setSearchPaths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setSearchPaths( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:source( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QTextBrowser_source( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:backward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_backward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:forward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_forward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:home( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_home( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:reload( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_reload( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setSource( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

