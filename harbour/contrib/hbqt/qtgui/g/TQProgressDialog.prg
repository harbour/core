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


FUNCTION QProgressDialog( ... )
   RETURN HB_QProgressDialog():new( ... )

FUNCTION QProgressDialogFromPointer( ... )
   RETURN HB_QProgressDialog():fromPointer( ... )


CREATE CLASS QProgressDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QProgressDialog

   METHOD  new( ... )

   METHOD  autoClose                     // (  )                                               -> lBool
   METHOD  autoReset                     // (  )                                               -> lBool
   METHOD  labelText                     // (  )                                               -> cQString
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  minimumDuration               // (  )                                               -> nInt
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  setAutoClose                  // ( lClose )                                         -> NIL
   METHOD  setAutoReset                  // ( lReset )                                         -> NIL
   METHOD  setBar                        // ( oQProgressBar )                                  -> NIL
   METHOD  setCancelButton               // ( oQPushButton )                                   -> NIL
   METHOD  setLabel                      // ( oQLabel )                                        -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  value                         // (  )                                               -> nInt
   METHOD  wasCanceled                   // (  )                                               -> lBool
   METHOD  cancel                        // (  )                                               -> NIL
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  setCancelButtonText           // ( cCancelButtonText )                              -> NIL
   METHOD  setLabelText                  // ( cText )                                          -> NIL
   METHOD  setMaximum                    // ( nMaximum )                                       -> NIL
   METHOD  setMinimum                    // ( nMinimum )                                       -> NIL
   METHOD  setMinimumDuration            // ( nMs )                                            -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum )                             -> NIL
   METHOD  setValue                      // ( nProgress )                                      -> NIL

   ENDCLASS


METHOD QProgressDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProgressDialog( ... )
   RETURN Self


METHOD QProgressDialog:autoClose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_autoClose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:autoReset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_autoReset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:labelText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_labelText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:minimumDuration( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_minimumDuration( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QProgressDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setAutoClose( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setAutoClose( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setAutoReset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setAutoReset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setCancelButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setCancelButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setLabel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setLabel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QProgressDialog_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:wasCanceled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_wasCanceled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:cancel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_cancel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProgressDialog_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setCancelButtonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setCancelButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setLabelText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setLabelText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setMinimumDuration( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setMinimumDuration( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProgressDialog_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProgressDialog:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProgressDialog_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

