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


FUNCTION QToolBar( ... )
   RETURN HB_QToolBar():new( ... )

FUNCTION QToolBarFrom( ... )
   RETURN HB_QToolBar():from( ... )

FUNCTION QToolBarFromPointer( ... )
   RETURN HB_QToolBar():fromPointer( ... )


CREATE CLASS QToolBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QToolBar

   METHOD  new( ... )

   METHOD  actionAt                      // ( oQPoint )                                        -> oQAction
                                         // ( nX, nY )                                         -> oQAction
   METHOD  addAction                     // ( oQAction )                                       -> NIL
                                         // ( cText )                                          -> oQAction
                                         // ( coQIcon, cText )                                 -> oQAction
                                         // ( cText, oQObject, cMember )                       -> oQAction
                                         // ( coQIcon, cText, oQObject, cMember )              -> oQAction
   METHOD  addSeparator                  // (  )                                               -> oQAction
   METHOD  addWidget                     // ( oQWidget )                                       -> oQAction
   METHOD  allowedAreas                  // (  )                                               -> nQt_ToolBarAreas
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertSeparator               // ( oQAction )                                       -> oQAction
   METHOD  insertWidget                  // ( oQAction, oQWidget )                             -> oQAction
   METHOD  isAreaAllowed                 // ( nArea )                                          -> lBool
   METHOD  isFloatable                   // (  )                                               -> lBool
   METHOD  isFloating                    // (  )                                               -> lBool
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  setAllowedAreas               // ( nAreas )                                         -> NIL
   METHOD  setFloatable                  // ( lFloatable )                                     -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setOrientation                // ( nOrientation )                                   -> NIL
   METHOD  toggleViewAction              // (  )                                               -> oQAction
   METHOD  toolButtonStyle               // (  )                                               -> nQt_ToolButtonStyle
   METHOD  widgetForAction               // ( oQAction )                                       -> oQWidget
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setToolButtonStyle            // ( nToolButtonStyle )                               -> NIL

   ENDCLASS


METHOD QToolBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBar( ... )
   RETURN Self


METHOD QToolBar:actionAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_actionAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_actionAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addAction( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_addAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QToolBar_addSeparator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:allowedAreas( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_allowedAreas( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QToolBar_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:insertSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_insertSeparator( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:insertWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_insertWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isAreaAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_isAreaAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isFloatable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isFloatable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isFloating( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isFloating( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setAllowedAreas( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setAllowedAreas( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setFloatable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setFloatable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:toggleViewAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QToolBar_toggleViewAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:toolButtonStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_toolButtonStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:widgetForAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QToolBar_widgetForAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setToolButtonStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setToolButtonStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

