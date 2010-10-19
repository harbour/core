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


FUNCTION QWizardPage( ... )
   RETURN HB_QWizardPage():new( ... )

FUNCTION QWizardPageFromPointer( ... )
   RETURN HB_QWizardPage():fromPointer( ... )


CREATE CLASS QWizardPage INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QWizardPage

   METHOD  new( ... )

   METHOD  buttonText                    // ( nWhich )                                         -> cQString
   METHOD  cleanupPage                   // (  )                                               -> NIL
   METHOD  initializePage                // (  )                                               -> NIL
   METHOD  isCommitPage                  // (  )                                               -> lBool
   METHOD  isComplete                    // (  )                                               -> lBool
   METHOD  isFinalPage                   // (  )                                               -> lBool
   METHOD  nextId                        // (  )                                               -> nInt
   METHOD  pixmap                        // ( nWhich )                                         -> oQPixmap
   METHOD  setButtonText                 // ( nWhich, cText )                                  -> NIL
   METHOD  setCommitPage                 // ( lCommitPage )                                    -> NIL
   METHOD  setFinalPage                  // ( lFinalPage )                                     -> NIL
   METHOD  setPixmap                     // ( nWhich, oQPixmap )                               -> NIL
   METHOD  setSubTitle                   // ( cSubTitle )                                      -> NIL
   METHOD  setTitle                      // ( cTitle )                                         -> NIL
   METHOD  subTitle                      // (  )                                               -> cQString
   METHOD  title                         // (  )                                               -> cQString
   METHOD  validatePage                  // (  )                                               -> lBool

   ENDCLASS


METHOD QWizardPage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizardPage( ... )
   RETURN Self


METHOD QWizardPage:buttonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_buttonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:cleanupPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_cleanupPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:initializePage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_initializePage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isCommitPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isCommitPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isComplete( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isComplete( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isFinalPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isFinalPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:nextId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_nextId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:pixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QWizardPage_pixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setButtonText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWizardPage_setButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setCommitPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setCommitPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setFinalPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setFinalPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setPixmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizardPage_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setSubTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setSubTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:subTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_subTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:validatePage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_validatePage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

