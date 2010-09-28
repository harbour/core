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


CREATE CLASS QWizard INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QWizard

   METHOD  new( ... )

   METHOD  addPage( pPage )
   METHOD  button( nWhich )
   METHOD  buttonText( nWhich )
   METHOD  currentId()
   METHOD  currentPage()
   METHOD  field( cName )
   METHOD  hasVisitedPage( nId )
   METHOD  nextId()
   METHOD  options()
   METHOD  page( nId )
   METHOD  pageIds()
   METHOD  pixmap( nWhich )
   METHOD  removePage( nId )
   METHOD  setButton( nWhich, pButton )
   METHOD  setButtonText( nWhich, cText )
   METHOD  setDefaultProperty( pClassName, pProperty, pChangedSignal )
   METHOD  setField( cName, pValue )
   METHOD  setOption( nOption, lOn )
   METHOD  setOptions( nOptions )
   METHOD  setPage( nId, pPage )
   METHOD  setPixmap( nWhich, pPixmap )
   METHOD  setStartId( nId )
   METHOD  setSubTitleFormat( nFormat )
   METHOD  setTitleFormat( nFormat )
   METHOD  setWizardStyle( nStyle )
   METHOD  startId()
   METHOD  subTitleFormat()
   METHOD  testOption( nOption )
   METHOD  titleFormat()
   METHOD  validateCurrentPage()
   METHOD  visitedPages()
   METHOD  wizardStyle()
   METHOD  back()
   METHOD  next()
   METHOD  restart()

   ENDCLASS


METHOD QWizard:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizard( ... )
   RETURN Self


METHOD QWizard:addPage( pPage )
   RETURN Qt_QWizard_addPage( ::pPtr, hbqt_ptr( pPage ) )


METHOD QWizard:button( nWhich )
   RETURN HB_QAbstractButton():from( Qt_QWizard_button( ::pPtr, nWhich ) )


METHOD QWizard:buttonText( nWhich )
   RETURN Qt_QWizard_buttonText( ::pPtr, nWhich )


METHOD QWizard:currentId()
   RETURN Qt_QWizard_currentId( ::pPtr )


METHOD QWizard:currentPage()
   RETURN HB_QWizardPage():from( Qt_QWizard_currentPage( ::pPtr ) )


METHOD QWizard:field( cName )
   RETURN HB_QVariant():from( Qt_QWizard_field( ::pPtr, cName ) )


METHOD QWizard:hasVisitedPage( nId )
   RETURN Qt_QWizard_hasVisitedPage( ::pPtr, nId )


METHOD QWizard:nextId()
   RETURN Qt_QWizard_nextId( ::pPtr )


METHOD QWizard:options()
   RETURN Qt_QWizard_options( ::pPtr )


METHOD QWizard:page( nId )
   RETURN HB_QWizardPage():from( Qt_QWizard_page( ::pPtr, nId ) )


METHOD QWizard:pageIds()
   RETURN HB_QList():from( Qt_QWizard_pageIds( ::pPtr ) )


METHOD QWizard:pixmap( nWhich )
   RETURN HB_QPixmap():from( Qt_QWizard_pixmap( ::pPtr, nWhich ) )


METHOD QWizard:removePage( nId )
   RETURN Qt_QWizard_removePage( ::pPtr, nId )


METHOD QWizard:setButton( nWhich, pButton )
   RETURN Qt_QWizard_setButton( ::pPtr, nWhich, hbqt_ptr( pButton ) )


METHOD QWizard:setButtonText( nWhich, cText )
   RETURN Qt_QWizard_setButtonText( ::pPtr, nWhich, cText )


METHOD QWizard:setDefaultProperty( pClassName, pProperty, pChangedSignal )
   RETURN Qt_QWizard_setDefaultProperty( ::pPtr, hbqt_ptr( pClassName ), hbqt_ptr( pProperty ), hbqt_ptr( pChangedSignal ) )


METHOD QWizard:setField( cName, pValue )
   RETURN Qt_QWizard_setField( ::pPtr, cName, hbqt_ptr( pValue ) )


METHOD QWizard:setOption( nOption, lOn )
   RETURN Qt_QWizard_setOption( ::pPtr, nOption, lOn )


METHOD QWizard:setOptions( nOptions )
   RETURN Qt_QWizard_setOptions( ::pPtr, nOptions )


METHOD QWizard:setPage( nId, pPage )
   RETURN Qt_QWizard_setPage( ::pPtr, nId, hbqt_ptr( pPage ) )


METHOD QWizard:setPixmap( nWhich, pPixmap )
   RETURN Qt_QWizard_setPixmap( ::pPtr, nWhich, hbqt_ptr( pPixmap ) )


METHOD QWizard:setStartId( nId )
   RETURN Qt_QWizard_setStartId( ::pPtr, nId )


METHOD QWizard:setSubTitleFormat( nFormat )
   RETURN Qt_QWizard_setSubTitleFormat( ::pPtr, nFormat )


METHOD QWizard:setTitleFormat( nFormat )
   RETURN Qt_QWizard_setTitleFormat( ::pPtr, nFormat )


METHOD QWizard:setWizardStyle( nStyle )
   RETURN Qt_QWizard_setWizardStyle( ::pPtr, nStyle )


METHOD QWizard:startId()
   RETURN Qt_QWizard_startId( ::pPtr )


METHOD QWizard:subTitleFormat()
   RETURN Qt_QWizard_subTitleFormat( ::pPtr )


METHOD QWizard:testOption( nOption )
   RETURN Qt_QWizard_testOption( ::pPtr, nOption )


METHOD QWizard:titleFormat()
   RETURN Qt_QWizard_titleFormat( ::pPtr )


METHOD QWizard:validateCurrentPage()
   RETURN Qt_QWizard_validateCurrentPage( ::pPtr )


METHOD QWizard:visitedPages()
   RETURN HB_QList():from( Qt_QWizard_visitedPages( ::pPtr ) )


METHOD QWizard:wizardStyle()
   RETURN Qt_QWizard_wizardStyle( ::pPtr )


METHOD QWizard:back()
   RETURN Qt_QWizard_back( ::pPtr )


METHOD QWizard:next()
   RETURN Qt_QWizard_next( ::pPtr )


METHOD QWizard:restart()
   RETURN Qt_QWizard_restart( ::pPtr )

