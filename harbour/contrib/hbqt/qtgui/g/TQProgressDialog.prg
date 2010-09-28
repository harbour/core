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


FUNCTION QProgressDialog( ... )
   RETURN HB_QProgressDialog():new( ... )


CREATE CLASS QProgressDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QProgressDialog

   METHOD  new( ... )

   METHOD  autoClose()
   METHOD  autoReset()
   METHOD  labelText()
   METHOD  maximum()
   METHOD  minimum()
   METHOD  minimumDuration()
   METHOD  open( pReceiver, pMember )
   METHOD  setAutoClose( lClose )
   METHOD  setAutoReset( lReset )
   METHOD  setBar( pBar )
   METHOD  setCancelButton( pCancelButton )
   METHOD  setLabel( pLabel )
   METHOD  sizeHint()
   METHOD  value()
   METHOD  wasCanceled()
   METHOD  cancel()
   METHOD  reset()
   METHOD  setCancelButtonText( cCancelButtonText )
   METHOD  setLabelText( cText )
   METHOD  setMaximum( nMaximum )
   METHOD  setMinimum( nMinimum )
   METHOD  setMinimumDuration( nMs )
   METHOD  setRange( nMinimum, nMaximum )
   METHOD  setValue( nProgress )

   ENDCLASS


METHOD QProgressDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProgressDialog( ... )
   RETURN Self


METHOD QProgressDialog:autoClose()
   RETURN Qt_QProgressDialog_autoClose( ::pPtr )


METHOD QProgressDialog:autoReset()
   RETURN Qt_QProgressDialog_autoReset( ::pPtr )


METHOD QProgressDialog:labelText()
   RETURN Qt_QProgressDialog_labelText( ::pPtr )


METHOD QProgressDialog:maximum()
   RETURN Qt_QProgressDialog_maximum( ::pPtr )


METHOD QProgressDialog:minimum()
   RETURN Qt_QProgressDialog_minimum( ::pPtr )


METHOD QProgressDialog:minimumDuration()
   RETURN Qt_QProgressDialog_minimumDuration( ::pPtr )


METHOD QProgressDialog:open( pReceiver, pMember )
   RETURN Qt_QProgressDialog_open( ::pPtr, hbqt_ptr( pReceiver ), hbqt_ptr( pMember ) )


METHOD QProgressDialog:setAutoClose( lClose )
   RETURN Qt_QProgressDialog_setAutoClose( ::pPtr, lClose )


METHOD QProgressDialog:setAutoReset( lReset )
   RETURN Qt_QProgressDialog_setAutoReset( ::pPtr, lReset )


METHOD QProgressDialog:setBar( pBar )
   RETURN Qt_QProgressDialog_setBar( ::pPtr, hbqt_ptr( pBar ) )


METHOD QProgressDialog:setCancelButton( pCancelButton )
   RETURN Qt_QProgressDialog_setCancelButton( ::pPtr, hbqt_ptr( pCancelButton ) )


METHOD QProgressDialog:setLabel( pLabel )
   RETURN Qt_QProgressDialog_setLabel( ::pPtr, hbqt_ptr( pLabel ) )


METHOD QProgressDialog:sizeHint()
   RETURN HB_QSize():from( Qt_QProgressDialog_sizeHint( ::pPtr ) )


METHOD QProgressDialog:value()
   RETURN Qt_QProgressDialog_value( ::pPtr )


METHOD QProgressDialog:wasCanceled()
   RETURN Qt_QProgressDialog_wasCanceled( ::pPtr )


METHOD QProgressDialog:cancel()
   RETURN Qt_QProgressDialog_cancel( ::pPtr )


METHOD QProgressDialog:reset()
   RETURN Qt_QProgressDialog_reset( ::pPtr )


METHOD QProgressDialog:setCancelButtonText( cCancelButtonText )
   RETURN Qt_QProgressDialog_setCancelButtonText( ::pPtr, cCancelButtonText )


METHOD QProgressDialog:setLabelText( cText )
   RETURN Qt_QProgressDialog_setLabelText( ::pPtr, cText )


METHOD QProgressDialog:setMaximum( nMaximum )
   RETURN Qt_QProgressDialog_setMaximum( ::pPtr, nMaximum )


METHOD QProgressDialog:setMinimum( nMinimum )
   RETURN Qt_QProgressDialog_setMinimum( ::pPtr, nMinimum )


METHOD QProgressDialog:setMinimumDuration( nMs )
   RETURN Qt_QProgressDialog_setMinimumDuration( ::pPtr, nMs )


METHOD QProgressDialog:setRange( nMinimum, nMaximum )
   RETURN Qt_QProgressDialog_setRange( ::pPtr, nMinimum, nMaximum )


METHOD QProgressDialog:setValue( nProgress )
   RETURN Qt_QProgressDialog_setValue( ::pPtr, nProgress )

