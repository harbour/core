/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QMessageBox INHERIT QDialog

   DATA    pPtr

   METHOD  New()

   METHOD  addButton( pButton, nButtonRole )   INLINE  Qt_QMessageBox_addButton( ::pPtr, pButton, nButtonRole )
   METHOD  addButton_1( cText, nButtonRole )   INLINE  Qt_QMessageBox_addButton_1( ::pPtr, cText, nButtonRole )
   METHOD  addButton_2( nStandardButton )      INLINE  Qt_QMessageBox_addButton_2( ::pPtr, nStandardButton )
   METHOD  button( nStandardButton )           INLINE  Qt_QMessageBox_button( ::pPtr, nStandardButton )
   METHOD  buttonRole( pButton )               INLINE  Qt_QMessageBox_buttonRole( ::pPtr, pButton )
   METHOD  clickedButton()                     INLINE  Qt_QMessageBox_clickedButton( ::pPtr )
   METHOD  defaultButton()                     INLINE  Qt_QMessageBox_defaultButton( ::pPtr )
   METHOD  detailedText()                      INLINE  Qt_QMessageBox_detailedText( ::pPtr )
   METHOD  escapeButton()                      INLINE  Qt_QMessageBox_escapeButton( ::pPtr )
   METHOD  icon()                              INLINE  Qt_QMessageBox_icon( ::pPtr )
   METHOD  informativeText()                   INLINE  Qt_QMessageBox_informativeText( ::pPtr )
   METHOD  open( pReceiver, pMember )          INLINE  Qt_QMessageBox_open( ::pPtr, pReceiver, pMember )
   METHOD  removeButton( pButton )             INLINE  Qt_QMessageBox_removeButton( ::pPtr, pButton )
   METHOD  setDefaultButton( pButton )         INLINE  Qt_QMessageBox_setDefaultButton( ::pPtr, pButton )
   METHOD  setDefaultButton_1( nStandardButton )  INLINE  Qt_QMessageBox_setDefaultButton_1( ::pPtr, nStandardButton )
   METHOD  setDetailedText( cText )            INLINE  Qt_QMessageBox_setDetailedText( ::pPtr, cText )
   METHOD  setEscapeButton( pButton )          INLINE  Qt_QMessageBox_setEscapeButton( ::pPtr, pButton )
   METHOD  setEscapeButton_1( nStandardButton )  INLINE  Qt_QMessageBox_setEscapeButton_1( ::pPtr, nStandardButton )
   METHOD  setIcon( nIcon )                    INLINE  Qt_QMessageBox_setIcon( ::pPtr, nIcon )
   METHOD  setInformativeText( cText )         INLINE  Qt_QMessageBox_setInformativeText( ::pPtr, cText )
   METHOD  setStandardButtons( nStandardButtons )  INLINE  Qt_QMessageBox_setStandardButtons( ::pPtr, nStandardButtons )
   METHOD  setText( cText )                    INLINE  Qt_QMessageBox_setText( ::pPtr, cText )
   METHOD  setTextFormat( nFormat )            INLINE  Qt_QMessageBox_setTextFormat( ::pPtr, nFormat )
   METHOD  setWindowModality( nWindowModality )  INLINE  Qt_QMessageBox_setWindowModality( ::pPtr, nWindowModality )
   METHOD  setWindowTitle( cTitle )            INLINE  Qt_QMessageBox_setWindowTitle( ::pPtr, cTitle )
   METHOD  standardButton( pButton )           INLINE  Qt_QMessageBox_standardButton( ::pPtr, pButton )
   METHOD  standardButtons()                   INLINE  Qt_QMessageBox_standardButtons( ::pPtr )
   METHOD  text()                              INLINE  Qt_QMessageBox_text( ::pPtr )
   METHOD  textFormat()                        INLINE  Qt_QMessageBox_textFormat( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QMessageBox

   ::pPtr := Qt_QMessageBox( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

