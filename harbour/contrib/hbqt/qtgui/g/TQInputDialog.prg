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


REQUEST __HBQTGUI


FUNCTION QInputDialog( ... )
   RETURN HB_QInputDialog():new( ... )

FUNCTION QInputDialogFromPointer( ... )
   RETURN HB_QInputDialog():fromPointer( ... )


CREATE CLASS QInputDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QInputDialog

   METHOD  new( ... )

   METHOD  cancelButtonText              // (  )                                               -> cQString
   METHOD  comboBoxItems                 // (  )                                               -> oQStringList
   METHOD  done                          // ( nResult )                                        -> NIL
   METHOD  doubleDecimals                // (  )                                               -> nInt
   METHOD  doubleMaximum                 // (  )                                               -> nDouble
   METHOD  doubleMinimum                 // (  )                                               -> nDouble
   METHOD  doubleValue                   // (  )                                               -> nDouble
   METHOD  inputMode                     // (  )                                               -> nInputMode
   METHOD  intMaximum                    // (  )                                               -> nInt
   METHOD  intMinimum                    // (  )                                               -> nInt
   METHOD  intStep                       // (  )                                               -> nInt
   METHOD  intValue                      // (  )                                               -> nInt
   METHOD  isComboBoxEditable            // (  )                                               -> lBool
   METHOD  labelText                     // (  )                                               -> cQString
   METHOD  okButtonText                  // (  )                                               -> cQString
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  options                       // (  )                                               -> nInputDialogOptions
   METHOD  setCancelButtonText           // ( cText )                                          -> NIL
   METHOD  setComboBoxEditable           // ( lEditable )                                      -> NIL
   METHOD  setComboBoxItems              // ( oQStringList )                                   -> NIL
   METHOD  setDoubleDecimals             // ( nDecimals )                                      -> NIL
   METHOD  setDoubleMaximum              // ( nMax )                                           -> NIL
   METHOD  setDoubleMinimum              // ( nMin )                                           -> NIL
   METHOD  setDoubleRange                // ( nMin, nMax )                                     -> NIL
   METHOD  setDoubleValue                // ( nValue )                                         -> NIL
   METHOD  setInputMode                  // ( nMode )                                          -> NIL
   METHOD  setIntMaximum                 // ( nMax )                                           -> NIL
   METHOD  setIntMinimum                 // ( nMin )                                           -> NIL
   METHOD  setIntRange                   // ( nMin, nMax )                                     -> NIL
   METHOD  setIntStep                    // ( nStep )                                          -> NIL
   METHOD  setIntValue                   // ( nValue )                                         -> NIL
   METHOD  setLabelText                  // ( cText )                                          -> NIL
   METHOD  setOkButtonText               // ( cText )                                          -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setTextEchoMode               // ( nMode )                                          -> NIL
   METHOD  setTextValue                  // ( cText )                                          -> NIL
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  textEchoMode                  // (  )                                               -> nQLineEdit_EchoMode
   METHOD  textValue                     // (  )                                               -> cQString
   METHOD  getDouble                     // ( oQWidget, cTitle, cLabel, nValue, nMin, nMax, nDecimals, @lOk, nFlags ) -> nDouble
   METHOD  getInt                        // ( oQWidget, cTitle, cLabel, nValue, nMin, nMax, nStep, @lOk, nFlags ) -> nInt
   METHOD  getItem                       // ( oQWidget, cTitle, cLabel, oQStringList, nCurrent, lEditable, @lOk, nFlags ) -> cQString
   METHOD  getText                       // ( oQWidget, cTitle, cLabel, nMode, cText, @lOk, nFlags ) -> cQString

   ENDCLASS


METHOD QInputDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QInputDialog( ... )
   RETURN Self


METHOD QInputDialog:cancelButtonText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_cancelButtonText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:comboBoxItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QInputDialog_comboBoxItems( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:done( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_done( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:doubleDecimals( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_doubleDecimals( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:doubleMaximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_doubleMaximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:doubleMinimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_doubleMinimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:doubleValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_doubleValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:inputMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_inputMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:intMaximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_intMaximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:intMinimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_intMinimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:intStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_intStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:intValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_intValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:isComboBoxEditable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_isComboBoxEditable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:labelText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_labelText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:okButtonText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_okButtonText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QInputDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setCancelButtonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setCancelButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setComboBoxEditable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setComboBoxEditable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setComboBoxItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setComboBoxItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setDoubleDecimals( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setDoubleDecimals( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setDoubleMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setDoubleMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setDoubleMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setDoubleMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setDoubleRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QInputDialog_setDoubleRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setDoubleValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setDoubleValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setInputMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setInputMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setIntMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setIntMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setIntMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setIntMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setIntRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QInputDialog_setIntRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setIntStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setIntStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setIntValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setIntValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setLabelText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setLabelText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setOkButtonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setOkButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QInputDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setTextEchoMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setTextEchoMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:setTextValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_setTextValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QInputDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:textEchoMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_textEchoMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:textValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QInputDialog_textValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:getDouble( ... )
   SWITCH PCount()
   CASE 9
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isLogical( hb_pvalue( 8 ) ) .AND. hb_isNumeric( hb_pvalue( 9 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 8
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isLogical( hb_pvalue( 8 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QInputDialog_getDouble( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:getInt( ... )
   SWITCH PCount()
   CASE 9
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isLogical( hb_pvalue( 8 ) ) .AND. hb_isNumeric( hb_pvalue( 9 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 8
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isLogical( hb_pvalue( 8 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QInputDialog_getInt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:getItem( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isLogical( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN Qt_QInputDialog_getItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isLogical( hb_pvalue( 7 ) )
         RETURN Qt_QInputDialog_getItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) )
         RETURN Qt_QInputDialog_getItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QInputDialog_getItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN Qt_QInputDialog_getItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QInputDialog:getText( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isChar( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QInputDialog_getText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isChar( hb_pvalue( 5 ) ) .AND. hb_isLogical( hb_pvalue( 6 ) )
         RETURN Qt_QInputDialog_getText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isChar( hb_pvalue( 5 ) )
         RETURN Qt_QInputDialog_getText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QInputDialog_getText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QInputDialog_getText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

