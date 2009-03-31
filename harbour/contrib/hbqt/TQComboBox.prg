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


CLASS QComboBox INHERIT QWidget

   DATA    pPtr

   METHOD  New()

   METHOD  completer()                         INLINE  Qt_QComboBox_completer( ::pPtr )
   METHOD  count()                             INLINE  Qt_QComboBox_count( ::pPtr )
   METHOD  currentIndex()                      INLINE  Qt_QComboBox_currentIndex( ::pPtr )
   METHOD  currentText()                       INLINE  Qt_QComboBox_currentText( ::pPtr )
   METHOD  duplicatesEnabled()                 INLINE  Qt_QComboBox_duplicatesEnabled( ::pPtr )
   METHOD  hasFrame()                          INLINE  Qt_QComboBox_hasFrame( ::pPtr )
   METHOD  hidePopup()                         INLINE  Qt_QComboBox_hidePopup( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QComboBox_iconSize( ::pPtr )
   METHOD  insertPolicy()                      INLINE  Qt_QComboBox_insertPolicy( ::pPtr )
   METHOD  insertSeparator( nIndex )           INLINE  Qt_QComboBox_insertSeparator( ::pPtr, nIndex )
   METHOD  isEditable()                        INLINE  Qt_QComboBox_isEditable( ::pPtr )
   METHOD  itemDelegate()                      INLINE  Qt_QComboBox_itemDelegate( ::pPtr )
   METHOD  itemText( nIndex )                  INLINE  Qt_QComboBox_itemText( ::pPtr, nIndex )
   METHOD  lineEdit()                          INLINE  Qt_QComboBox_lineEdit( ::pPtr )
   METHOD  maxCount()                          INLINE  Qt_QComboBox_maxCount( ::pPtr )
   METHOD  maxVisibleItems()                   INLINE  Qt_QComboBox_maxVisibleItems( ::pPtr )
   METHOD  minimumContentsLength()             INLINE  Qt_QComboBox_minimumContentsLength( ::pPtr )
   METHOD  model()                             INLINE  Qt_QComboBox_model( ::pPtr )
   METHOD  modelColumn()                       INLINE  Qt_QComboBox_modelColumn( ::pPtr )
   METHOD  removeItem( nIndex )                INLINE  Qt_QComboBox_removeItem( ::pPtr, nIndex )
   METHOD  setCompleter( pCompleter )          INLINE  Qt_QComboBox_setCompleter( ::pPtr, pCompleter )
   METHOD  setDuplicatesEnabled( lEnable )     INLINE  Qt_QComboBox_setDuplicatesEnabled( ::pPtr, lEnable )
   METHOD  setEditable( lEditable )            INLINE  Qt_QComboBox_setEditable( ::pPtr, lEditable )
   METHOD  setFrame( lBool )                   INLINE  Qt_QComboBox_setFrame( ::pPtr, lBool )
   METHOD  setIconSize( aSizeSize )            INLINE  Qt_QComboBox_setIconSize( ::pPtr, aSizeSize )
   METHOD  setInsertPolicy( nInsertPolicy )    INLINE  Qt_QComboBox_setInsertPolicy( ::pPtr, nInsertPolicy )
   METHOD  setItemDelegate( pDelegate )        INLINE  Qt_QComboBox_setItemDelegate( ::pPtr, pDelegate )
   METHOD  setItemIcon( nIndex, cIcon )        INLINE  Qt_QComboBox_setItemIcon( ::pPtr, nIndex, cIcon )
   METHOD  setItemText( nIndex, cText )        INLINE  Qt_QComboBox_setItemText( ::pPtr, nIndex, cText )
   METHOD  setLineEdit( pEdit )                INLINE  Qt_QComboBox_setLineEdit( ::pPtr, pEdit )
   METHOD  setMaxCount( nMax )                 INLINE  Qt_QComboBox_setMaxCount( ::pPtr, nMax )
   METHOD  setMaxVisibleItems( nMaxItems )     INLINE  Qt_QComboBox_setMaxVisibleItems( ::pPtr, nMaxItems )
   METHOD  setMinimumContentsLength( nCharacters )  INLINE  Qt_QComboBox_setMinimumContentsLength( ::pPtr, nCharacters )
   METHOD  setModel( pModel )                  INLINE  Qt_QComboBox_setModel( ::pPtr, pModel )
   METHOD  setModelColumn( nVisibleColumn )    INLINE  Qt_QComboBox_setModelColumn( ::pPtr, nVisibleColumn )
   METHOD  setSizeAdjustPolicy( nSizeAdjustPolicy )  INLINE  Qt_QComboBox_setSizeAdjustPolicy( ::pPtr, nSizeAdjustPolicy )
   METHOD  setValidator( pValidator )          INLINE  Qt_QComboBox_setValidator( ::pPtr, pValidator )
   METHOD  setView( pItemView )                INLINE  Qt_QComboBox_setView( ::pPtr, pItemView )
   METHOD  showPopup()                         INLINE  Qt_QComboBox_showPopup( ::pPtr )
   METHOD  sizeAdjustPolicy()                  INLINE  Qt_QComboBox_sizeAdjustPolicy( ::pPtr )
   METHOD  validator()                         INLINE  Qt_QComboBox_validator( ::pPtr )
   METHOD  view()                              INLINE  Qt_QComboBox_view( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QComboBox

   ::pPtr := Qt_QComboBox( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

