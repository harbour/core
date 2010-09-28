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


FUNCTION QWebPage( ... )
   RETURN HB_QWebPage():new( ... )


CREATE CLASS QWebPage INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebPage

   METHOD  new( ... )

   METHOD  action( nAction )
   METHOD  bytesReceived()
   METHOD  createStandardContextMenu()
   METHOD  currentFrame()
   METHOD  findText( cSubString, nOptions )
   METHOD  focusNextPrevChild( lNext )
   METHOD  forwardUnsupportedContent()
   METHOD  history()
   METHOD  inputMethodQuery( nProperty )
   METHOD  isContentEditable()
   METHOD  isModified()
   METHOD  linkDelegationPolicy()
   METHOD  mainFrame()
   METHOD  palette()
   METHOD  pluginFactory()
   METHOD  selectedText()
   METHOD  setContentEditable( lEditable )
   METHOD  setForwardUnsupportedContent( lForward )
   METHOD  setLinkDelegationPolicy( nPolicy )
   METHOD  setPalette( pPalette )
   METHOD  setPluginFactory( pFactory )
   METHOD  setView( pView )
   METHOD  setViewportSize( pSize )
   METHOD  settings()
   METHOD  supportsExtension( nExtension )
   METHOD  swallowContextMenuEvent( pEvent )
   METHOD  totalBytes()
   METHOD  triggerAction( nAction, lChecked )
   METHOD  updatePositionDependentActions( pPos )
   METHOD  view()
   METHOD  viewportSize()

   ENDCLASS


METHOD QWebPage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebPage( ... )
   RETURN Self


METHOD QWebPage:action( nAction )
   RETURN HB_QAction():from( Qt_QWebPage_action( ::pPtr, nAction ) )


METHOD QWebPage:bytesReceived()
   RETURN Qt_QWebPage_bytesReceived( ::pPtr )


METHOD QWebPage:createStandardContextMenu()
   RETURN HB_QMenu():from( Qt_QWebPage_createStandardContextMenu( ::pPtr ) )


METHOD QWebPage:currentFrame()
   RETURN HB_QWebFrame():from( Qt_QWebPage_currentFrame( ::pPtr ) )


METHOD QWebPage:findText( cSubString, nOptions )
   RETURN Qt_QWebPage_findText( ::pPtr, cSubString, nOptions )


METHOD QWebPage:focusNextPrevChild( lNext )
   RETURN Qt_QWebPage_focusNextPrevChild( ::pPtr, lNext )


METHOD QWebPage:forwardUnsupportedContent()
   RETURN Qt_QWebPage_forwardUnsupportedContent( ::pPtr )


METHOD QWebPage:history()
   RETURN HB_QWebHistory():from( Qt_QWebPage_history( ::pPtr ) )


METHOD QWebPage:inputMethodQuery( nProperty )
   RETURN HB_QVariant():from( Qt_QWebPage_inputMethodQuery( ::pPtr, nProperty ) )


METHOD QWebPage:isContentEditable()
   RETURN Qt_QWebPage_isContentEditable( ::pPtr )


METHOD QWebPage:isModified()
   RETURN Qt_QWebPage_isModified( ::pPtr )


METHOD QWebPage:linkDelegationPolicy()
   RETURN Qt_QWebPage_linkDelegationPolicy( ::pPtr )


METHOD QWebPage:mainFrame()
   RETURN HB_QWebFrame():from( Qt_QWebPage_mainFrame( ::pPtr ) )


METHOD QWebPage:palette()
   RETURN HB_QPalette():from( Qt_QWebPage_palette( ::pPtr ) )


METHOD QWebPage:pluginFactory()
   RETURN HB_QWebPluginFactory():from( Qt_QWebPage_pluginFactory( ::pPtr ) )


METHOD QWebPage:selectedText()
   RETURN Qt_QWebPage_selectedText( ::pPtr )


METHOD QWebPage:setContentEditable( lEditable )
   RETURN Qt_QWebPage_setContentEditable( ::pPtr, lEditable )


METHOD QWebPage:setForwardUnsupportedContent( lForward )
   RETURN Qt_QWebPage_setForwardUnsupportedContent( ::pPtr, lForward )


METHOD QWebPage:setLinkDelegationPolicy( nPolicy )
   RETURN Qt_QWebPage_setLinkDelegationPolicy( ::pPtr, nPolicy )


METHOD QWebPage:setPalette( pPalette )
   RETURN Qt_QWebPage_setPalette( ::pPtr, hbqt_ptr( pPalette ) )


METHOD QWebPage:setPluginFactory( pFactory )
   RETURN Qt_QWebPage_setPluginFactory( ::pPtr, hbqt_ptr( pFactory ) )


METHOD QWebPage:setView( pView )
   RETURN Qt_QWebPage_setView( ::pPtr, hbqt_ptr( pView ) )


METHOD QWebPage:setViewportSize( pSize )
   RETURN Qt_QWebPage_setViewportSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QWebPage:settings()
   RETURN HB_QWebSettings():from( Qt_QWebPage_settings( ::pPtr ) )


METHOD QWebPage:supportsExtension( nExtension )
   RETURN Qt_QWebPage_supportsExtension( ::pPtr, nExtension )


METHOD QWebPage:swallowContextMenuEvent( pEvent )
   RETURN Qt_QWebPage_swallowContextMenuEvent( ::pPtr, hbqt_ptr( pEvent ) )


METHOD QWebPage:totalBytes()
   RETURN Qt_QWebPage_totalBytes( ::pPtr )


METHOD QWebPage:triggerAction( nAction, lChecked )
   RETURN Qt_QWebPage_triggerAction( ::pPtr, nAction, lChecked )


METHOD QWebPage:updatePositionDependentActions( pPos )
   RETURN Qt_QWebPage_updatePositionDependentActions( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWebPage:view()
   RETURN HB_QWidget():from( Qt_QWebPage_view( ::pPtr ) )


METHOD QWebPage:viewportSize()
   RETURN HB_QSize():from( Qt_QWebPage_viewportSize( ::pPtr ) )

