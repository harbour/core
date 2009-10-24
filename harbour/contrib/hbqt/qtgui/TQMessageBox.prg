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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


#include "hbclass.ch"


CREATE CLASS QMessageBox INHERIT QDialog

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addButton( pButton, nRole )
   METHOD  addButton_1( cText, nRole )
   METHOD  addButton_2( nButton )
   METHOD  button( nWhich )
   METHOD  buttonRole( pButton )
   METHOD  clickedButton()
   METHOD  defaultButton()
   METHOD  detailedText()
   METHOD  escapeButton()
   METHOD  icon()
   METHOD  iconPixmap()
   METHOD  informativeText()
   METHOD  open( pReceiver, pMember )
   METHOD  removeButton( pButton )
   METHOD  setDefaultButton( pButton )
   METHOD  setDefaultButton_1( nButton )
   METHOD  setDetailedText( cText )
   METHOD  setEscapeButton( pButton )
   METHOD  setEscapeButton_1( nButton )
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

/*----------------------------------------------------------------------*/

METHOD QMessageBox:New( pParent )
   ::pParent := pParent
   ::pPtr := Qt_QMessageBox( pParent )
   RETURN Self


METHOD QMessageBox:Configure( xObject )
   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF
   RETURN Self


METHOD QMessageBox:addButton( pButton, nRole )
   RETURN Qt_QMessageBox_addButton( ::pPtr, pButton, nRole )


METHOD QMessageBox:addButton_1( cText, nRole )
   RETURN Qt_QMessageBox_addButton_1( ::pPtr, cText, nRole )


METHOD QMessageBox:addButton_2( nButton )
   RETURN Qt_QMessageBox_addButton_2( ::pPtr, nButton )


METHOD QMessageBox:button( nWhich )
   RETURN Qt_QMessageBox_button( ::pPtr, nWhich )


METHOD QMessageBox:buttonRole( pButton )
   RETURN Qt_QMessageBox_buttonRole( ::pPtr, pButton )


METHOD QMessageBox:clickedButton()
   RETURN Qt_QMessageBox_clickedButton( ::pPtr )


METHOD QMessageBox:defaultButton()
   RETURN Qt_QMessageBox_defaultButton( ::pPtr )


METHOD QMessageBox:detailedText()
   RETURN Qt_QMessageBox_detailedText( ::pPtr )


METHOD QMessageBox:escapeButton()
   RETURN Qt_QMessageBox_escapeButton( ::pPtr )


METHOD QMessageBox:icon()
   RETURN Qt_QMessageBox_icon( ::pPtr )


METHOD QMessageBox:iconPixmap()
   RETURN Qt_QMessageBox_iconPixmap( ::pPtr )


METHOD QMessageBox:informativeText()
   RETURN Qt_QMessageBox_informativeText( ::pPtr )


METHOD QMessageBox:open( pReceiver, pMember )
   RETURN Qt_QMessageBox_open( ::pPtr, pReceiver, pMember )


METHOD QMessageBox:removeButton( pButton )
   RETURN Qt_QMessageBox_removeButton( ::pPtr, pButton )


METHOD QMessageBox:setDefaultButton( pButton )
   RETURN Qt_QMessageBox_setDefaultButton( ::pPtr, pButton )


METHOD QMessageBox:setDefaultButton_1( nButton )
   RETURN Qt_QMessageBox_setDefaultButton_1( ::pPtr, nButton )


METHOD QMessageBox:setDetailedText( cText )
   RETURN Qt_QMessageBox_setDetailedText( ::pPtr, cText )


METHOD QMessageBox:setEscapeButton( pButton )
   RETURN Qt_QMessageBox_setEscapeButton( ::pPtr, pButton )


METHOD QMessageBox:setEscapeButton_1( nButton )
   RETURN Qt_QMessageBox_setEscapeButton_1( ::pPtr, nButton )


METHOD QMessageBox:setIcon( nIcon )
   RETURN Qt_QMessageBox_setIcon( ::pPtr, nIcon )


METHOD QMessageBox:setIconPixmap( pPixmap )
   RETURN Qt_QMessageBox_setIconPixmap( ::pPtr, pPixmap )


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
   RETURN Qt_QMessageBox_standardButton( ::pPtr, pButton )


METHOD QMessageBox:standardButtons()
   RETURN Qt_QMessageBox_standardButtons( ::pPtr )


METHOD QMessageBox:text()
   RETURN Qt_QMessageBox_text( ::pPtr )


METHOD QMessageBox:textFormat()
   RETURN Qt_QMessageBox_textFormat( ::pPtr )


METHOD QMessageBox:about( pParent, cTitle, cText )
   RETURN Qt_QMessageBox_about( ::pPtr, pParent, cTitle, cText )


METHOD QMessageBox:aboutQt( pParent, cTitle )
   RETURN Qt_QMessageBox_aboutQt( ::pPtr, pParent, cTitle )


METHOD QMessageBox:critical( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_critical( ::pPtr, pParent, cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:information( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_information( ::pPtr, pParent, cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:question( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_question( ::pPtr, pParent, cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:warning( pParent, cTitle, cText, nButtons, nDefaultButton )
   RETURN Qt_QMessageBox_warning( ::pPtr, pParent, cTitle, cText, nButtons, nDefaultButton )


METHOD QMessageBox:exec()
   RETURN Qt_QMessageBox_exec( ::pPtr )

