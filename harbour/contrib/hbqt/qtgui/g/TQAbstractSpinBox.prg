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


FUNCTION QAbstractSpinBox( ... )
   RETURN HB_QAbstractSpinBox():new( ... )

FUNCTION QAbstractSpinBoxFrom( ... )
   RETURN HB_QAbstractSpinBox():from( ... )

FUNCTION QAbstractSpinBoxFromPointer( ... )
   RETURN HB_QAbstractSpinBox():fromPointer( ... )


CREATE CLASS QAbstractSpinBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractSpinBox

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  buttonSymbols                 // (  )                                               -> nButtonSymbols
   METHOD  correctionMode                // (  )                                               -> nCorrectionMode
   METHOD  hasAcceptableInput            // (  )                                               -> lBool
   METHOD  hasFrame                      // (  )                                               -> lBool
   METHOD  interpretText                 // (  )                                               -> NIL
   METHOD  isAccelerated                 // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  keyboardTracking              // (  )                                               -> lBool
   METHOD  setAccelerated                // ( lOn )                                            -> NIL
   METHOD  setAlignment                  // ( nFlag )                                          -> NIL
   METHOD  setButtonSymbols              // ( nBs )                                            -> NIL
   METHOD  setCorrectionMode             // ( nCm )                                            -> NIL
   METHOD  setFrame                      // ( lBool )                                          -> NIL
   METHOD  setKeyboardTracking           // ( lKt )                                            -> NIL
   METHOD  setReadOnly                   // ( lR )                                             -> NIL
   METHOD  setSpecialValueText           // ( cTxt )                                           -> NIL
   METHOD  setWrapping                   // ( lW )                                             -> NIL
   METHOD  specialValueText              // (  )                                               -> cQString
   METHOD  stepBy                        // ( nSteps )                                         -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  wrapping                      // (  )                                               -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  stepDown                      // (  )                                               -> NIL
   METHOD  stepUp                        // (  )                                               -> NIL

   ENDCLASS


METHOD QAbstractSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractSpinBox( ... )
   RETURN Self


METHOD QAbstractSpinBox:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:buttonSymbols( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_buttonSymbols( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:correctionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_correctionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:hasAcceptableInput( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_hasAcceptableInput( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:hasFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_hasFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:interpretText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_interpretText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:isAccelerated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_isAccelerated( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:keyboardTracking( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_keyboardTracking( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setAccelerated( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setAccelerated( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setButtonSymbols( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setButtonSymbols( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setCorrectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setCorrectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setKeyboardTracking( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setKeyboardTracking( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setSpecialValueText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setSpecialValueText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:setWrapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_setWrapping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:specialValueText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_specialValueText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:stepBy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSpinBox_stepBy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_text( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:wrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_wrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:stepDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_stepDown( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSpinBox:stepUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSpinBox_stepUp( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

