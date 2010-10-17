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


FUNCTION QTabWidget( ... )
   RETURN HB_QTabWidget():new( ... )

FUNCTION QTabWidgetFrom( ... )
   RETURN HB_QTabWidget():from( ... )

FUNCTION QTabWidgetFromPointer( ... )
   RETURN HB_QTabWidget():fromPointer( ... )


CREATE CLASS QTabWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabWidget

   METHOD  new( ... )

   METHOD  addTab                        // ( oQWidget, cLabel )                               -> nInt
                                         // ( oQWidget, coQIcon, cLabel )                      -> nInt
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  cornerWidget                  // ( nCorner )                                        -> oQWidget
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  currentWidget                 // (  )                                               -> oQWidget
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  elideMode                     // (  )                                               -> nQt_TextElideMode
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  insertTab                     // ( nIndex, oQWidget, cLabel )                       -> nInt
                                         // ( nIndex, oQWidget, coQIcon, cLabel )              -> nInt
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isTabEnabled                  // ( nIndex )                                         -> lBool
   METHOD  removeTab                     // ( nIndex )                                         -> NIL
   METHOD  setCornerWidget               // ( oQWidget, nCorner )                              -> NIL
   METHOD  setDocumentMode               // ( lSet )                                           -> NIL
   METHOD  setElideMode                  // ( nQt::TextElideMode )                             -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setTabEnabled                 // ( nIndex, lEnable )                                -> NIL
   METHOD  setTabIcon                    // ( nIndex, coQIcon )                                -> NIL
   METHOD  setTabPosition                // ( nTabPosition )                                   -> NIL
   METHOD  setTabShape                   // ( nS )                                             -> NIL
   METHOD  setTabText                    // ( nIndex, cLabel )                                 -> NIL
   METHOD  setTabToolTip                 // ( nIndex, cTip )                                   -> NIL
   METHOD  setTabWhatsThis               // ( nIndex, cText )                                  -> NIL
   METHOD  setTabsClosable               // ( lCloseable )                                     -> NIL
   METHOD  setUsesScrollButtons          // ( lUseButtons )                                    -> NIL
   METHOD  tabIcon                       // ( nIndex )                                         -> oQIcon
   METHOD  tabPosition                   // (  )                                               -> nTabPosition
   METHOD  tabShape                      // (  )                                               -> nTabShape
   METHOD  tabText                       // ( nIndex )                                         -> cQString
   METHOD  tabToolTip                    // ( nIndex )                                         -> cQString
   METHOD  tabWhatsThis                  // ( nIndex )                                         -> cQString
   METHOD  tabsClosable                  // (  )                                               -> lBool
   METHOD  usesScrollButtons             // (  )                                               -> lBool
   METHOD  widget                        // ( nIndex )                                         -> oQWidget
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL
   METHOD  setCurrentWidget              // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QTabWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabWidget( ... )
   RETURN Self


METHOD QTabWidget:addTab( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QTabWidget_addTab_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_addTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:cornerWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QTabWidget_cornerWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QWidgetFromPointer( Qt_QTabWidget_cornerWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:currentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QTabWidget_currentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:elideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_elideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTabWidget_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:insertTab( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. ( hb_isObject( hb_pvalue( 3 ) ) .OR. hb_isChar( hb_pvalue( 3 ) ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QTabWidget_insertTab_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QTabWidget_insertTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:isTabEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_isTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:removeTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_removeTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCornerWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setCornerWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCornerWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setElideMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setElideMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QTabWidget_setTabIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabsClosable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabsClosable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setUsesScrollButtons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setUsesScrollButtons( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTabWidget_tabIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabsClosable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabsClosable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:usesScrollButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_usesScrollButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QTabWidget_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCurrentWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCurrentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

