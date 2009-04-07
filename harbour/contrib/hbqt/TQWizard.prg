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


#include "hbclass.ch"


CREATE CLASS QWizard INHERIT QDialog

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  addPage( pPage )                    INLINE  Qt_QWizard_addPage( ::pPtr, pPage )
   METHOD  button( nWhich )                    INLINE  Qt_QWizard_button( ::pPtr, nWhich )
   METHOD  buttonText( nWhich )                INLINE  Qt_QWizard_buttonText( ::pPtr, nWhich )
   METHOD  currentId()                         INLINE  Qt_QWizard_currentId( ::pPtr )
   METHOD  currentPage()                       INLINE  Qt_QWizard_currentPage( ::pPtr )
   METHOD  field( cName )                      INLINE  Qt_QWizard_field( ::pPtr, cName )
   METHOD  hasVisitedPage( nId )               INLINE  Qt_QWizard_hasVisitedPage( ::pPtr, nId )
   METHOD  nextId()                            INLINE  Qt_QWizard_nextId( ::pPtr )
   METHOD  options()                           INLINE  Qt_QWizard_options( ::pPtr )
   METHOD  page( nId )                         INLINE  Qt_QWizard_page( ::pPtr, nId )
   METHOD  pixmap( nWhich )                    INLINE  Qt_QWizard_pixmap( ::pPtr, nWhich )
   METHOD  removePage( nId )                   INLINE  Qt_QWizard_removePage( ::pPtr, nId )
   METHOD  setButton( nWhich, pButton )        INLINE  Qt_QWizard_setButton( ::pPtr, nWhich, pButton )
   METHOD  setButtonText( nWhich, cText )      INLINE  Qt_QWizard_setButtonText( ::pPtr, nWhich, cText )
   METHOD  setDefaultProperty( pClassName, pProperty, pChangedSignal )  INLINE  Qt_QWizard_setDefaultProperty( ::pPtr, pClassName, pProperty, pChangedSignal )
   METHOD  setField( cName, pValue )           INLINE  Qt_QWizard_setField( ::pPtr, cName, pValue )
   METHOD  setOption( nOption, lOn )           INLINE  Qt_QWizard_setOption( ::pPtr, nOption, lOn )
   METHOD  setOptions( nOptions )              INLINE  Qt_QWizard_setOptions( ::pPtr, nOptions )
   METHOD  setPage( nId, pPage )               INLINE  Qt_QWizard_setPage( ::pPtr, nId, pPage )
   METHOD  setPixmap( nWhich, pPixmap )        INLINE  Qt_QWizard_setPixmap( ::pPtr, nWhich, pPixmap )
   METHOD  setStartId( nId )                   INLINE  Qt_QWizard_setStartId( ::pPtr, nId )
   METHOD  setSubTitleFormat( nFormat )        INLINE  Qt_QWizard_setSubTitleFormat( ::pPtr, nFormat )
   METHOD  setTitleFormat( nFormat )           INLINE  Qt_QWizard_setTitleFormat( ::pPtr, nFormat )
   METHOD  setWizardStyle( nStyle )            INLINE  Qt_QWizard_setWizardStyle( ::pPtr, nStyle )
   METHOD  startId()                           INLINE  Qt_QWizard_startId( ::pPtr )
   METHOD  subTitleFormat()                    INLINE  Qt_QWizard_subTitleFormat( ::pPtr )
   METHOD  testOption( nOption )               INLINE  Qt_QWizard_testOption( ::pPtr, nOption )
   METHOD  titleFormat()                       INLINE  Qt_QWizard_titleFormat( ::pPtr )
   METHOD  validateCurrentPage()               INLINE  Qt_QWizard_validateCurrentPage( ::pPtr )
   METHOD  wizardStyle()                       INLINE  Qt_QWizard_wizardStyle( ::pPtr )
   METHOD  back()                              INLINE  Qt_QWizard_back( ::pPtr )
   METHOD  next()                              INLINE  Qt_QWizard_next( ::pPtr )
   METHOD  restart()                           INLINE  Qt_QWizard_restart( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWizard

   ::pParent := pParent

   ::pPtr := Qt_QWizard( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

