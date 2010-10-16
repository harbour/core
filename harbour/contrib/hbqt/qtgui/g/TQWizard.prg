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


FUNCTION QWizard( ... )
   RETURN HB_QWizard():new( ... )

FUNCTION QWizardFrom( ... )
   RETURN HB_QWizard():from( ... )

FUNCTION QWizardFromPointer( ... )
   RETURN HB_QWizard():fromPointer( ... )


CREATE CLASS QWizard INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QWizard

   METHOD  new( ... )

   METHOD  addPage                       // ( oQWizardPage )                                   -> nInt
   METHOD  button                        // ( nWhich )                                         -> oQAbstractButton
   METHOD  buttonText                    // ( nWhich )                                         -> cQString
   METHOD  currentId                     // (  )                                               -> nInt
   METHOD  currentPage                   // (  )                                               -> oQWizardPage
   METHOD  field                         // ( cName )                                          -> oQVariant
   METHOD  hasVisitedPage                // ( nId )                                            -> lBool
   METHOD  nextId                        // (  )                                               -> nInt
   METHOD  options                       // (  )                                               -> nWizardOptions
   METHOD  page                          // ( nId )                                            -> oQWizardPage
   METHOD  pageIds                       // (  )                                               -> oQList_int>
   METHOD  pixmap                        // ( nWhich )                                         -> oQPixmap
   METHOD  removePage                    // ( nId )                                            -> NIL
   METHOD  setButton                     // ( nWhich, oQAbstractButton )                       -> NIL
   METHOD  setButtonText                 // ( nWhich, cText )                                  -> NIL
   METHOD  setDefaultProperty            // ( cClassName, cProperty, cChangedSignal )          -> NIL
   METHOD  setField                      // ( cName, oQVariant )                               -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setPage                       // ( nId, oQWizardPage )                              -> NIL
   METHOD  setPixmap                     // ( nWhich, oQPixmap )                               -> NIL
   METHOD  setStartId                    // ( nId )                                            -> NIL
   METHOD  setSubTitleFormat             // ( nFormat )                                        -> NIL
   METHOD  setTitleFormat                // ( nFormat )                                        -> NIL
   METHOD  setWizardStyle                // ( nStyle )                                         -> NIL
   METHOD  startId                       // (  )                                               -> nInt
   METHOD  subTitleFormat                // (  )                                               -> nQt_TextFormat
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  titleFormat                   // (  )                                               -> nQt_TextFormat
   METHOD  validateCurrentPage           // (  )                                               -> lBool
   METHOD  visitedPages                  // (  )                                               -> oQList_int>
   METHOD  wizardStyle                   // (  )                                               -> nWizardStyle
   METHOD  back                          // (  )                                               -> NIL
   METHOD  next                          // (  )                                               -> NIL
   METHOD  restart                       // (  )                                               -> NIL

   ENDCLASS


METHOD QWizard:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizard( ... )
   RETURN Self


METHOD QWizard:addPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_addPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:button( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractButtonFromPointer( Qt_QWizard_button( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:buttonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_buttonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:currentId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_currentId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:currentPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWizardPageFromPointer( Qt_QWizard_currentPage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:field( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QWizard_field( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:hasVisitedPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_hasVisitedPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:nextId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_nextId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_options( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:page( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWizardPageFromPointer( Qt_QWizard_page( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:pageIds( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWizard_pageIds( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:pixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QWizard_pixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:removePage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_removePage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setButtonText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setDefaultProperty( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QWizard_setDefaultProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setField( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setField( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setPage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setPixmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setStartId( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setStartId( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setSubTitleFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setSubTitleFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setTitleFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setTitleFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:setWizardStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setWizardStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:startId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_startId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:subTitleFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_subTitleFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:titleFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_titleFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:validateCurrentPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_validateCurrentPage( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:visitedPages( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWizard_visitedPages( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:wizardStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_wizardStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_back( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:next( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_next( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWizard:restart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_restart( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

