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


FUNCTION QAbstractButton( ... )
   RETURN HB_QAbstractButton():new( ... )

FUNCTION QAbstractButtonFrom( ... )
   RETURN HB_QAbstractButton():from( ... )

FUNCTION QAbstractButtonFromPointer( ... )
   RETURN HB_QAbstractButton():fromPointer( ... )


CREATE CLASS QAbstractButton INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractButton

   METHOD  new( ... )

   METHOD  autoExclusive                 // (  )                                               -> lBool
   METHOD  autoRepeat                    // (  )                                               -> lBool
   METHOD  autoRepeatDelay               // (  )                                               -> nInt
   METHOD  autoRepeatInterval            // (  )                                               -> nInt
   METHOD  group                         // (  )                                               -> oQButtonGroup
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  isCheckable                   // (  )                                               -> lBool
   METHOD  isChecked                     // (  )                                               -> lBool
   METHOD  isDown                        // (  )                                               -> lBool
   METHOD  setAutoExclusive              // ( lBool )                                          -> NIL
   METHOD  setAutoRepeat                 // ( lBool )                                          -> NIL
   METHOD  setAutoRepeatDelay            // ( nInt )                                           -> NIL
   METHOD  setAutoRepeatInterval         // ( nInt )                                           -> NIL
   METHOD  setCheckable                  // ( lBool )                                          -> NIL
   METHOD  setDown                       // ( lBool )                                          -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setShortcut                   // ( oQKeySequence )                                  -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  shortcut                      // (  )                                               -> oQKeySequence
   METHOD  text                          // (  )                                               -> cQString
   METHOD  animateClick                  // ( nMsec )                                          -> NIL
   METHOD  click                         // (  )                                               -> NIL
   METHOD  setChecked                    // ( lBool )                                          -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  toggle                        // (  )                                               -> NIL

   ENDCLASS


METHOD QAbstractButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractButton( ... )
   RETURN Self


METHOD QAbstractButton:autoExclusive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoExclusive( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:autoRepeat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeat( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:autoRepeatDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeatDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:autoRepeatInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeatInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:group( ... )
   SWITCH PCount()
   CASE 0
      RETURN QButtonGroupFromPointer( Qt_QAbstractButton_group( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QAbstractButton_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QAbstractButton_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:isCheckable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isCheckable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:isChecked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isChecked( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:isDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isDown( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setAutoExclusive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoExclusive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setAutoRepeat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setAutoRepeatDelay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeatDelay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setAutoRepeatInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeatInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setCheckable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setCheckable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setDown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setDown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QAbstractButton_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setShortcut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:shortcut( ... )
   SWITCH PCount()
   CASE 0
      RETURN QKeySequenceFromPointer( Qt_QAbstractButton_shortcut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_text( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:animateClick( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_animateClick( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAbstractButton_animateClick( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:click( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_click( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setChecked( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setChecked( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractButton:toggle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_toggle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

