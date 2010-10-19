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


FUNCTION QTabBar( ... )
   RETURN HB_QTabBar():new( ... )

FUNCTION QTabBarFromPointer( ... )
   RETURN HB_QTabBar():fromPointer( ... )


CREATE CLASS QTabBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabBar

   METHOD  new( ... )

   METHOD  addTab                        // ( cText )                                          -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  drawBase                      // (  )                                               -> lBool
   METHOD  elideMode                     // (  )                                               -> nQt_TextElideMode
   METHOD  expanding                     // (  )                                               -> lBool
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertTab                     // ( nIndex, cText )                                  -> nInt
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isTabEnabled                  // ( nIndex )                                         -> lBool
   METHOD  moveTab                       // ( nFrom, nTo )                                     -> NIL
   METHOD  removeTab                     // ( nIndex )                                         -> NIL
   METHOD  selectionBehaviorOnRemove     // (  )                                               -> nSelectionBehavior
   METHOD  setDocumentMode               // ( lSet )                                           -> NIL
   METHOD  setDrawBase                   // ( lDrawTheBase )                                   -> NIL
   METHOD  setElideMode                  // ( nQt::TextElideMode )                             -> NIL
   METHOD  setExpanding                  // ( lEnabled )                                       -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setSelectionBehaviorOnRemove  // ( nBehavior )                                      -> NIL
   METHOD  setShape                      // ( nShape )                                         -> NIL
   METHOD  setTabButton                  // ( nIndex, nPosition, oQWidget )                    -> NIL
   METHOD  setTabData                    // ( nIndex, oQVariant )                              -> NIL
   METHOD  setTabEnabled                 // ( nIndex, lEnabled )                               -> NIL
   METHOD  setTabText                    // ( nIndex, cText )                                  -> NIL
   METHOD  setTabTextColor               // ( nIndex, oQColor )                                -> NIL
   METHOD  setTabToolTip                 // ( nIndex, cTip )                                   -> NIL
   METHOD  setTabWhatsThis               // ( nIndex, cText )                                  -> NIL
   METHOD  setTabsClosable               // ( lClosable )                                      -> NIL
   METHOD  setUsesScrollButtons          // ( lUseButtons )                                    -> NIL
   METHOD  shape                         // (  )                                               -> nShape
   METHOD  tabAt                         // ( oQPoint )                                        -> nInt
   METHOD  tabButton                     // ( nIndex, nPosition )                              -> oQWidget
   METHOD  tabData                       // ( nIndex )                                         -> oQVariant
   METHOD  tabIcon                       // ( nIndex )                                         -> oQIcon
   METHOD  tabRect                       // ( nIndex )                                         -> oQRect
   METHOD  tabText                       // ( nIndex )                                         -> cQString
   METHOD  tabTextColor                  // ( nIndex )                                         -> oQColor
   METHOD  tabToolTip                    // ( nIndex )                                         -> cQString
   METHOD  tabWhatsThis                  // ( nIndex )                                         -> cQString
   METHOD  tabsClosable                  // (  )                                               -> lBool
   METHOD  usesScrollButtons             // (  )                                               -> lBool
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL

   ENDCLASS


METHOD QTabBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabBar( ... )
   RETURN Self


METHOD QTabBar:addTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_addTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:drawBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_drawBase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:elideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_elideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:expanding( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_expanding( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTabBar_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:insertTab( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_insertTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:isTabEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_isTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:moveTab( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_moveTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:removeTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_removeTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:selectionBehaviorOnRemove( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_selectionBehaviorOnRemove( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setDrawBase( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setDrawBase( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setElideMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setElideMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setExpanding( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setExpanding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setSelectionBehaviorOnRemove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setSelectionBehaviorOnRemove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabButton( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTabBar_setTabButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabTextColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabTextColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabsClosable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setTabsClosable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setUsesScrollButtons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setUsesScrollButtons( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QTabBar_tabButton( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QTabBar_tabData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTabBar_tabIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTabBar_tabRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabTextColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QTabBar_tabTextColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabsClosable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_tabsClosable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:usesScrollButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_usesScrollButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

