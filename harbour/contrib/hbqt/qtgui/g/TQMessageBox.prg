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


FUNCTION QMessageBox( ... )
   RETURN HB_QMessageBox():new( ... )


CREATE CLASS QMessageBox INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QMessageBox

   METHOD  new( ... )

   METHOD  addButton( ... )
   METHOD  button( nWhich )
   METHOD  buttonRole( pButton )
   METHOD  buttons()
   METHOD  clickedButton()
   METHOD  defaultButton()
   METHOD  detailedText()
   METHOD  escapeButton()
   METHOD  icon()
   METHOD  iconPixmap()
   METHOD  informativeText()
   METHOD  open( pReceiver, pMember )
   METHOD  removeButton( pButton )
   METHOD  setDefaultButton( ... )
   METHOD  setDetailedText( cText )
   METHOD  setEscapeButton( ... )
   METHOD  setIcon( nIcon )
   METHOD  setIconPixmap( pPixmap )
   METHOD  setInformativeText( cText )
   METHOD  setStandardButtons( nButtons )
   METHOD  setText( cText )
   METHOD  setTextFormat( nFormat )
   METHOD  setWindowModality( nWindowModality )
   METHOD  setWindowTitle( cTitle )
   METHOD  standardButton( pButton )
   METHOD  standardButtons()
   METHOD  text()
   METHOD  textFormat()
   METHOD  about( pParent, cTitle, cText )
   METHOD  aboutQt( pParent, cTitle )
   METHOD  critical( pParent, cTitle, cText, nButtons, nDefaultButton )
   METHOD  information( pParent, cTitle, cText, nButtons, nDefaultButton )
   METHOD  question( pParent, cTitle, cText, nButtons, nDefaultButton )
   METHOD  warning( pParent, cTitle, cText, nButtons, nDefaultButton )
   METHOD  exec()

   ENDCLASS


METHOD QMessageBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMessageBox( ... )
   RETURN Self


METHOD QMessageBox:addButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QPushButton():from( Qt_QMessageBox_addButton_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QMessageBox_addButton( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QPushButton():from( Qt_QMessageBox_addButton_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMessageBox:button( nWhich )
   RETURN HB_QAbstractButton():from( Qt_QMessageBox_button( ::pPtr, nWhich ) )


METHOD QMessageBox:buttonRole( pButton )
   RETURN Qt_QMessageBox_buttonRole( ::pPtr, hbqt_ptr( pButton ) )


METHOD QMessageBox:buttons()
   RETURN HB_QList():from( Qt_QMessageBox_buttons( ::pPtr ) )


METHOD QMessageBox:clickedButton()
   RETURN HB_QAbstractButton():from( Qt_QMessageBox_clickedButton( ::pPtr ) )


METHOD QMessageBox:defaultButton()
   RETURN HB_QPushButton():from( Qt_QMessageBox_defaultButton( ::pPtr ) )


METHOD QMessageBox:detailedText()
   RETURN Qt_QMessageBox_detailedText( ::pPtr )


METHOD QMessageBox:escapeButton()
   RETURN HB_QAbstractButton():from( Qt_QMessageBox_escapeButton( ::pPtr ) )


METHOD QMessageBox:icon()
   RETURN Qt_QMessageBox_icon( ::pPtr )


METHOD QMessageBox:iconPixmap()
   RETURN HB_QPixmap():from( Qt_QMessageBox_iconPixmap( ::pPtr ) )


METHOD QMessageBox:informativeText()
   RETURN Qt_QMessageBox_informativeText( ::pPtr )


METHOD QMessageBox:open( pReceiver, pMember )
   RETURN Qt_QMessageBox_open( ::pPtr, hbqt_ptr( pReceiver ), hbqt_ptr( pMember ) )


METHOD QMessageBox:removeButton( pButton )
   RETURN Qt_QMessageBox_removeButton( ::pPtr, hbqt_ptr( pButton ) )


METHOD QMessageBox:setDefaultButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setDefaultButton_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setDefaultButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMessageBox:setDetailedText( cText )
   RETURN Qt_QMessageBox_setDetailedText( ::pPtr, cText )


METHOD QMessageBox:setEscapeButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setEscapeButton_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setEscapeButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMessageBox:setIcon( nIcon )
   RETURN Qt_QMessageBox_setIcon( ::pPtr, nIcon )


METHOD QMessageBox:setIconPixmap( pPixmap )
   RETURN Qt_QMessageBox_setIconPixmap( ::pPtr, hbqt_ptr( pPixmap ) )


METHOD QMessageBox:setInformativeText( cText )
   RETURN Qt_QMessageBox_setInformativeText( ::pPtr, cText )


METHOD QMessageBox:setStandardButtons( nButtons )
   RETURN Qt_QMessageBox_setStandardButtons( ::pPtr, nButtons )


METHOD QMessageBox:setText( cText )
   RETURN Qt_QMessageBox_setText( ::pPtr, cText )


METHOD QMessageBox:setTextFormat( nFormat )
   RETURN Qt_QMessageBox_setTextFormat( ::pPtr, nFormat )


METHOD QMessageBox:setWindowModality( nWindowModality )
   RETURN Qt_QMessageBox_setWindowModality( ::pPtr, nWindowModality )


METHOD QMessageBox:setWindowTitle( cTitle )
   RETURN Qt_QMessageBox_setWindowTitle( ::pPtr, cTitle )


METHOD QMessageBox:standardButton( pButton )
   RETURN Qt_QMessageBox_standardButton( ::pPtr, hbqt_ptr( pButton ) )


METHOD QMessageBox:standardButtons()
   RETURN Qt_QMessageBox_standardButtons( ::pPtr )


METHOD QMessageBox:text()
   RETURN Qt_QMessageBox_text( ::pPtr )


METHOD QMessageBox:textFormat()
   RETURN Qt_QMessageBox_textFormat( ::pPtr )


METHOD QMessageBox:about( pParent, cTitle, cText )
   RETURN Qt_QMessageBox_about( ::pPtr, hbqt_ptr( pParent ), cTitle, cText )


METHOD QMessageBox:aboutQt( pParent, cTitle )
   RETURN Qt_QMessageBox_aboutQt( ::pPtr, hbqt_ptr( pParent ), cTitle )


METHOD QMessageBox:critical( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_critical( ::pPtr, hbqt_ptr( pParent ), cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:information( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_information( ::pPtr, hbqt_ptr( pParent ), cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:question( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_question( ::pPtr, hbqt_ptr( pParent ), cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:warning( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_warning( ::pPtr, hbqt_ptr( pParent ), cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:exec()
   RETURN Qt_QMessageBox_exec( ::pPtr )

