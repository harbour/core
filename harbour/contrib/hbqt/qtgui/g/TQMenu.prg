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


FUNCTION QMenu( ... )
   RETURN HB_QMenu():new( ... )

FUNCTION QMenuFromPointer( ... )
   RETURN HB_QMenu():fromPointer( ... )


CREATE CLASS QMenu INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMenu

   METHOD  new( ... )

   METHOD  actionAt                      // ( oQPoint )                                        -> oQAction
   METHOD  actionGeometry                // ( oQAction )                                       -> oQRect
   METHOD  activeAction                  // (  )                                               -> oQAction
   METHOD  addAction                     // ( cText )                                          -> oQAction
                                         // ( coQIcon, cText )                                 -> oQAction
                                         // ( cText, oQObject, cMember, oQKeySequence )        -> oQAction
                                         // ( coQIcon, cText, oQObject, cMember, oQKeySequence ) -> oQAction
                                         // ( oQAction )                                       -> NIL
   METHOD  addMenu                       // ( oQMenu )                                         -> oQAction
                                         // ( cTitle )                                         -> oQMenu
                                         // ( coQIcon, cTitle )                                -> oQMenu
   METHOD  addSeparator                  // (  )                                               -> oQAction
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  defaultAction                 // (  )                                               -> oQAction
   METHOD  exec                          // (  )                                               -> oQAction
                                         // ( oQPoint, oQAction )                              -> oQAction
   METHOD  hideTearOffMenu               // (  )                                               -> NIL
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  insertMenu                    // ( oQAction, oQMenu )                               -> oQAction
   METHOD  insertSeparator               // ( oQAction )                                       -> oQAction
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isTearOffEnabled              // (  )                                               -> lBool
   METHOD  isTearOffMenuVisible          // (  )                                               -> lBool
   METHOD  menuAction                    // (  )                                               -> oQAction
   METHOD  popup                         // ( oQPoint, oQAction )                              -> NIL
   METHOD  separatorsCollapsible         // (  )                                               -> lBool
   METHOD  setActiveAction               // ( oQAction )                                       -> NIL
   METHOD  setDefaultAction              // ( oQAction )                                       -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setSeparatorsCollapsible      // ( lCollapse )                                      -> NIL
   METHOD  setTearOffEnabled             // ( lBool )                                          -> NIL
   METHOD  setTitle                      // ( cTitle )                                         -> NIL
   METHOD  title                         // (  )                                               -> cQString

   ENDCLASS


METHOD QMenu:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMenu( ... )
   RETURN Self


METHOD QMenu:actionAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenu_actionAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:actionGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QMenu_actionGeometry( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:activeAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenu_activeAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:addAction( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction_2( ::pPtr, ... ) )
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenu_addAction( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_addAction_4( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:addMenu( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QMenuFromPointer( Qt_QMenu_addMenu_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QMenuFromPointer( Qt_QMenu_addMenu_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenu_addMenu( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:addSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenu_addSeparator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:defaultAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenu_defaultAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:exec( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QMenu_exec_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenu_exec_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QActionFromPointer( Qt_QMenu_exec( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:hideTearOffMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_hideTearOffMenu( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QMenu_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:insertMenu( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QMenu_insertMenu( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:insertSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenu_insertSeparator( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:isTearOffEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_isTearOffEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:isTearOffMenuVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_isTearOffMenuVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:menuAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenu_menuAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:popup( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMenu_popup( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_popup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:separatorsCollapsible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_separatorsCollapsible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setActiveAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_setActiveAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setDefaultAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_setDefaultAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QMenu_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setSeparatorsCollapsible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_setSeparatorsCollapsible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setTearOffEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_setTearOffEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:setTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_setTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenu:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenu_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

