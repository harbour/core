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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QWizardPage( ... )
   RETURN HB_QWizardPage():new( ... )


CREATE CLASS QWizardPage INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QWizardPage

   METHOD  new( ... )

   METHOD  buttonText( nWhich )
   METHOD  cleanupPage()
   METHOD  initializePage()
   METHOD  isCommitPage()
   METHOD  isComplete()
   METHOD  isFinalPage()
   METHOD  nextId()
   METHOD  pixmap( nWhich )
   METHOD  setButtonText( nWhich, cText )
   METHOD  setCommitPage( lCommitPage )
   METHOD  setFinalPage( lFinalPage )
   METHOD  setPixmap( nWhich, pPixmap )
   METHOD  setSubTitle( cSubTitle )
   METHOD  setTitle( cTitle )
   METHOD  subTitle()
   METHOD  title()
   METHOD  validatePage()

   ENDCLASS


METHOD QWizardPage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizardPage( ... )
   RETURN Self


METHOD QWizardPage:buttonText( nWhich )
   RETURN Qt_QWizardPage_buttonText( ::pPtr, nWhich )


METHOD QWizardPage:cleanupPage()
   RETURN Qt_QWizardPage_cleanupPage( ::pPtr )


METHOD QWizardPage:initializePage()
   RETURN Qt_QWizardPage_initializePage( ::pPtr )


METHOD QWizardPage:isCommitPage()
   RETURN Qt_QWizardPage_isCommitPage( ::pPtr )


METHOD QWizardPage:isComplete()
   RETURN Qt_QWizardPage_isComplete( ::pPtr )


METHOD QWizardPage:isFinalPage()
   RETURN Qt_QWizardPage_isFinalPage( ::pPtr )


METHOD QWizardPage:nextId()
   RETURN Qt_QWizardPage_nextId( ::pPtr )


METHOD QWizardPage:pixmap( nWhich )
   RETURN Qt_QWizardPage_pixmap( ::pPtr, nWhich )


METHOD QWizardPage:setButtonText( nWhich, cText )
   RETURN Qt_QWizardPage_setButtonText( ::pPtr, nWhich, cText )


METHOD QWizardPage:setCommitPage( lCommitPage )
   RETURN Qt_QWizardPage_setCommitPage( ::pPtr, lCommitPage )


METHOD QWizardPage:setFinalPage( lFinalPage )
   RETURN Qt_QWizardPage_setFinalPage( ::pPtr, lFinalPage )


METHOD QWizardPage:setPixmap( nWhich, pPixmap )
   RETURN Qt_QWizardPage_setPixmap( ::pPtr, nWhich, hbqt_ptr( pPixmap ) )


METHOD QWizardPage:setSubTitle( cSubTitle )
   RETURN Qt_QWizardPage_setSubTitle( ::pPtr, cSubTitle )


METHOD QWizardPage:setTitle( cTitle )
   RETURN Qt_QWizardPage_setTitle( ::pPtr, cTitle )


METHOD QWizardPage:subTitle()
   RETURN Qt_QWizardPage_subTitle( ::pPtr )


METHOD QWizardPage:title()
   RETURN Qt_QWizardPage_title( ::pPtr )


METHOD QWizardPage:validatePage()
   RETURN Qt_QWizardPage_validatePage( ::pPtr )

