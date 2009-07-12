/*
 * $Id$
 */

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


CREATE CLASS QInputDialog INHERIT QDialog

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QInputDialog_destroy( ::pPtr )

   METHOD  cancelButtonText()                  INLINE  Qt_QInputDialog_cancelButtonText( ::pPtr )
   METHOD  comboBoxItems()                     INLINE  Qt_QInputDialog_comboBoxItems( ::pPtr )
   METHOD  done( nResult )                     INLINE  Qt_QInputDialog_done( ::pPtr, nResult )
   METHOD  doubleDecimals()                    INLINE  Qt_QInputDialog_doubleDecimals( ::pPtr )
   METHOD  doubleMaximum()                     INLINE  Qt_QInputDialog_doubleMaximum( ::pPtr )
   METHOD  doubleMinimum()                     INLINE  Qt_QInputDialog_doubleMinimum( ::pPtr )
   METHOD  doubleValue()                       INLINE  Qt_QInputDialog_doubleValue( ::pPtr )
   METHOD  inputMode()                         INLINE  Qt_QInputDialog_inputMode( ::pPtr )
   METHOD  intMaximum()                        INLINE  Qt_QInputDialog_intMaximum( ::pPtr )
   METHOD  intMinimum()                        INLINE  Qt_QInputDialog_intMinimum( ::pPtr )
   METHOD  intStep()                           INLINE  Qt_QInputDialog_intStep( ::pPtr )
   METHOD  intValue()                          INLINE  Qt_QInputDialog_intValue( ::pPtr )
   METHOD  isComboBoxEditable()                INLINE  Qt_QInputDialog_isComboBoxEditable( ::pPtr )
   METHOD  labelText()                         INLINE  Qt_QInputDialog_labelText( ::pPtr )
   METHOD  okButtonText()                      INLINE  Qt_QInputDialog_okButtonText( ::pPtr )
   METHOD  open( pReceiver, pMember )          INLINE  Qt_QInputDialog_open( ::pPtr, pReceiver, pMember )
   METHOD  options()                           INLINE  Qt_QInputDialog_options( ::pPtr )
   METHOD  setCancelButtonText( cText )        INLINE  Qt_QInputDialog_setCancelButtonText( ::pPtr, cText )
   METHOD  setComboBoxEditable( lEditable )    INLINE  Qt_QInputDialog_setComboBoxEditable( ::pPtr, lEditable )
   METHOD  setComboBoxItems( pItems )          INLINE  Qt_QInputDialog_setComboBoxItems( ::pPtr, pItems )
   METHOD  setDoubleDecimals( nDecimals )      INLINE  Qt_QInputDialog_setDoubleDecimals( ::pPtr, nDecimals )
   METHOD  setDoubleMaximum( nMax )            INLINE  Qt_QInputDialog_setDoubleMaximum( ::pPtr, nMax )
   METHOD  setDoubleMinimum( nMin )            INLINE  Qt_QInputDialog_setDoubleMinimum( ::pPtr, nMin )
   METHOD  setDoubleRange( nMin, nMax )        INLINE  Qt_QInputDialog_setDoubleRange( ::pPtr, nMin, nMax )
   METHOD  setDoubleValue( nValue )            INLINE  Qt_QInputDialog_setDoubleValue( ::pPtr, nValue )
   METHOD  setInputMode( nMode )               INLINE  Qt_QInputDialog_setInputMode( ::pPtr, nMode )
   METHOD  setIntMaximum( nMax )               INLINE  Qt_QInputDialog_setIntMaximum( ::pPtr, nMax )
   METHOD  setIntMinimum( nMin )               INLINE  Qt_QInputDialog_setIntMinimum( ::pPtr, nMin )
   METHOD  setIntRange( nMin, nMax )           INLINE  Qt_QInputDialog_setIntRange( ::pPtr, nMin, nMax )
   METHOD  setIntStep( nStep )                 INLINE  Qt_QInputDialog_setIntStep( ::pPtr, nStep )
   METHOD  setIntValue( nValue )               INLINE  Qt_QInputDialog_setIntValue( ::pPtr, nValue )
   METHOD  setLabelText( cText )               INLINE  Qt_QInputDialog_setLabelText( ::pPtr, cText )
   METHOD  setOkButtonText( cText )            INLINE  Qt_QInputDialog_setOkButtonText( ::pPtr, cText )
   METHOD  setOption( nOption, lOn )           INLINE  Qt_QInputDialog_setOption( ::pPtr, nOption, lOn )
   METHOD  setOptions( nOptions )              INLINE  Qt_QInputDialog_setOptions( ::pPtr, nOptions )
   METHOD  setTextEchoMode( nMode )            INLINE  Qt_QInputDialog_setTextEchoMode( ::pPtr, nMode )
   METHOD  setTextValue( cText )               INLINE  Qt_QInputDialog_setTextValue( ::pPtr, cText )
   METHOD  testOption( nOption )               INLINE  Qt_QInputDialog_testOption( ::pPtr, nOption )
   METHOD  textEchoMode()                      INLINE  Qt_QInputDialog_textEchoMode( ::pPtr )
   METHOD  textValue()                         INLINE  Qt_QInputDialog_textValue( ::pPtr )
   METHOD  getDouble( pParent, cTitle, cLabel, nValue, nMin, nMax, nDecimals, lOk, nFlags )  INLINE  Qt_QInputDialog_getDouble( ::pPtr, pParent, cTitle, cLabel, nValue, nMin, nMax, nDecimals, lOk, nFlags )
   METHOD  getInt( pParent, cTitle, cLabel, nValue, nMin, nMax, nStep, lOk, nFlags )  INLINE  Qt_QInputDialog_getInt( ::pPtr, pParent, cTitle, cLabel, nValue, nMin, nMax, nStep, lOk, nFlags )
   METHOD  getItem( pParent, cTitle, cLabel, pItems, nCurrent, lEditable, lOk, nFlags )  INLINE  Qt_QInputDialog_getItem( ::pPtr, pParent, cTitle, cLabel, pItems, nCurrent, lEditable, lOk, nFlags )
   METHOD  getText( pParent, cTitle, cLabel, nMode, cText, lOk, nFlags )  INLINE  Qt_QInputDialog_getText( ::pPtr, pParent, cTitle, cLabel, nMode, cText, lOk, nFlags )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QInputDialog

   ::pParent := pParent

   ::pPtr := Qt_QInputDialog( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QInputDialog

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

