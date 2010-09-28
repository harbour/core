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


CREATE CLASS QAbstractSpinBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractSpinBox

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  buttonSymbols()
   METHOD  correctionMode()
   METHOD  hasAcceptableInput()
   METHOD  hasFrame()
   METHOD  interpretText()
   METHOD  isAccelerated()
   METHOD  isReadOnly()
   METHOD  keyboardTracking()
   METHOD  setAccelerated( lOn )
   METHOD  setAlignment( nFlag )
   METHOD  setButtonSymbols( nBs )
   METHOD  setCorrectionMode( nCm )
   METHOD  setFrame( lBool )
   METHOD  setKeyboardTracking( lKt )
   METHOD  setReadOnly( lR )
   METHOD  setSpecialValueText( cTxt )
   METHOD  setWrapping( lW )
   METHOD  specialValueText()
   METHOD  stepBy( nSteps )
   METHOD  text()
   METHOD  wrapping()
   METHOD  clear()
   METHOD  selectAll()
   METHOD  stepDown()
   METHOD  stepUp()

   ENDCLASS


METHOD QAbstractSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractSpinBox( ... )
   RETURN Self


METHOD QAbstractSpinBox:alignment()
   RETURN Qt_QAbstractSpinBox_alignment( ::pPtr )


METHOD QAbstractSpinBox:buttonSymbols()
   RETURN Qt_QAbstractSpinBox_buttonSymbols( ::pPtr )


METHOD QAbstractSpinBox:correctionMode()
   RETURN Qt_QAbstractSpinBox_correctionMode( ::pPtr )


METHOD QAbstractSpinBox:hasAcceptableInput()
   RETURN Qt_QAbstractSpinBox_hasAcceptableInput( ::pPtr )


METHOD QAbstractSpinBox:hasFrame()
   RETURN Qt_QAbstractSpinBox_hasFrame( ::pPtr )


METHOD QAbstractSpinBox:interpretText()
   RETURN Qt_QAbstractSpinBox_interpretText( ::pPtr )


METHOD QAbstractSpinBox:isAccelerated()
   RETURN Qt_QAbstractSpinBox_isAccelerated( ::pPtr )


METHOD QAbstractSpinBox:isReadOnly()
   RETURN Qt_QAbstractSpinBox_isReadOnly( ::pPtr )


METHOD QAbstractSpinBox:keyboardTracking()
   RETURN Qt_QAbstractSpinBox_keyboardTracking( ::pPtr )


METHOD QAbstractSpinBox:setAccelerated( lOn )
   RETURN Qt_QAbstractSpinBox_setAccelerated( ::pPtr, lOn )


METHOD QAbstractSpinBox:setAlignment( nFlag )
   RETURN Qt_QAbstractSpinBox_setAlignment( ::pPtr, nFlag )


METHOD QAbstractSpinBox:setButtonSymbols( nBs )
   RETURN Qt_QAbstractSpinBox_setButtonSymbols( ::pPtr, nBs )


METHOD QAbstractSpinBox:setCorrectionMode( nCm )
   RETURN Qt_QAbstractSpinBox_setCorrectionMode( ::pPtr, nCm )


METHOD QAbstractSpinBox:setFrame( lBool )
   RETURN Qt_QAbstractSpinBox_setFrame( ::pPtr, lBool )


METHOD QAbstractSpinBox:setKeyboardTracking( lKt )
   RETURN Qt_QAbstractSpinBox_setKeyboardTracking( ::pPtr, lKt )


METHOD QAbstractSpinBox:setReadOnly( lR )
   RETURN Qt_QAbstractSpinBox_setReadOnly( ::pPtr, lR )


METHOD QAbstractSpinBox:setSpecialValueText( cTxt )
   RETURN Qt_QAbstractSpinBox_setSpecialValueText( ::pPtr, cTxt )


METHOD QAbstractSpinBox:setWrapping( lW )
   RETURN Qt_QAbstractSpinBox_setWrapping( ::pPtr, lW )


METHOD QAbstractSpinBox:specialValueText()
   RETURN Qt_QAbstractSpinBox_specialValueText( ::pPtr )


METHOD QAbstractSpinBox:stepBy( nSteps )
   RETURN Qt_QAbstractSpinBox_stepBy( ::pPtr, nSteps )


METHOD QAbstractSpinBox:text()
   RETURN Qt_QAbstractSpinBox_text( ::pPtr )


METHOD QAbstractSpinBox:wrapping()
   RETURN Qt_QAbstractSpinBox_wrapping( ::pPtr )


METHOD QAbstractSpinBox:clear()
   RETURN Qt_QAbstractSpinBox_clear( ::pPtr )


METHOD QAbstractSpinBox:selectAll()
   RETURN Qt_QAbstractSpinBox_selectAll( ::pPtr )


METHOD QAbstractSpinBox:stepDown()
   RETURN Qt_QAbstractSpinBox_stepDown( ::pPtr )


METHOD QAbstractSpinBox:stepUp()
   RETURN Qt_QAbstractSpinBox_stepUp( ::pPtr )

