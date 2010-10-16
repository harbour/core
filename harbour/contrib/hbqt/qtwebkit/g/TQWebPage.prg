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


FUNCTION QWebPage( ... )
   RETURN HB_QWebPage():new( ... )

FUNCTION QWebPageFrom( ... )
   RETURN HB_QWebPage():from( ... )

FUNCTION QWebPageFromPointer( ... )
   RETURN HB_QWebPage():fromPointer( ... )


CREATE CLASS QWebPage INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebPage

   METHOD  new( ... )

   METHOD  action                        // ( nAction )                                        -> oQAction
   METHOD  bytesReceived                 // (  )                                               -> nQuint64
   METHOD  createStandardContextMenu     // (  )                                               -> oQMenu
   METHOD  currentFrame                  // (  )                                               -> oQWebFrame
   METHOD  findText                      // ( cSubString, nOptions )                           -> lBool
   METHOD  focusNextPrevChild            // ( lNext )                                          -> lBool
   METHOD  forwardUnsupportedContent     // (  )                                               -> lBool
   METHOD  history                       // (  )                                               -> oQWebHistory
   METHOD  inputMethodQuery              // ( nProperty )                                      -> oQVariant
   METHOD  isContentEditable             // (  )                                               -> lBool
   METHOD  isModified                    // (  )                                               -> lBool
   METHOD  linkDelegationPolicy          // (  )                                               -> nLinkDelegationPolicy
   METHOD  mainFrame                     // (  )                                               -> oQWebFrame
   METHOD  palette                       // (  )                                               -> oQPalette
   METHOD  pluginFactory                 // (  )                                               -> oQWebPluginFactory
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  setContentEditable            // ( lEditable )                                      -> NIL
   METHOD  setForwardUnsupportedContent  // ( lForward )                                       -> NIL
   METHOD  setLinkDelegationPolicy       // ( nPolicy )                                        -> NIL
   METHOD  setPalette                    // ( oQPalette )                                      -> NIL
   METHOD  setPluginFactory              // ( oQWebPluginFactory )                             -> NIL
   METHOD  setView                       // ( oQWidget )                                       -> NIL
   METHOD  setViewportSize               // ( oQSize )                                         -> NIL
   METHOD  settings                      // (  )                                               -> oQWebSettings
   METHOD  supportsExtension             // ( nExtension )                                     -> lBool
   METHOD  swallowContextMenuEvent       // ( oQContextMenuEvent )                             -> lBool
   METHOD  totalBytes                    // (  )                                               -> nQuint64
   METHOD  triggerAction                 // ( nAction, lChecked )                              -> NIL
   METHOD  updatePositionDependentActions // ( oQPoint )                                        -> NIL
   METHOD  view                          // (  )                                               -> oQWidget
   METHOD  viewportSize                  // (  )                                               -> oQSize

   ENDCLASS


METHOD QWebPage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebPage( ... )
   RETURN Self


METHOD QWebPage:action( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QWebPage_action( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:bytesReceived( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_bytesReceived( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:createStandardContextMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QWebPage_createStandardContextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:currentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebFrameFromPointer( Qt_QWebPage_currentFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:findText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWebPage_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_findText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:focusNextPrevChild( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_focusNextPrevChild( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:forwardUnsupportedContent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_forwardUnsupportedContent( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:history( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryFromPointer( Qt_QWebPage_history( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:inputMethodQuery( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QWebPage_inputMethodQuery( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:isContentEditable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_isContentEditable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:isModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_isModified( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:linkDelegationPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_linkDelegationPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:mainFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebFrameFromPointer( Qt_QWebPage_mainFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:palette( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaletteFromPointer( Qt_QWebPage_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:pluginFactory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebPluginFactoryFromPointer( Qt_QWebPage_pluginFactory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setContentEditable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setContentEditable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setForwardUnsupportedContent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setForwardUnsupportedContent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setLinkDelegationPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setLinkDelegationPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setPalette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setPluginFactory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setPluginFactory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setView( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setView( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:setViewportSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_setViewportSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:settings( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebSettingsFromPointer( Qt_QWebPage_settings( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:supportsExtension( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_supportsExtension( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:swallowContextMenuEvent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_swallowContextMenuEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:totalBytes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPage_totalBytes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:triggerAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWebPage_triggerAction( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_triggerAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:updatePositionDependentActions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebPage_updatePositionDependentActions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:view( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWebPage_view( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWebPage:viewportSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWebPage_viewportSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()

