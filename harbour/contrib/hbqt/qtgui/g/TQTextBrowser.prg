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


FUNCTION QTextBrowser( ... )
   RETURN HB_QTextBrowser():new( ... )


CREATE CLASS QTextBrowser INHERIT HbQtObjectHandler, HB_QTextEdit FUNCTION HB_QTextBrowser

   METHOD  new( ... )

   METHOD  backwardHistoryCount()
   METHOD  clearHistory()
   METHOD  forwardHistoryCount()
   METHOD  historyTitle( nI )
   METHOD  historyUrl( nI )
   METHOD  isBackwardAvailable()
   METHOD  isForwardAvailable()
   METHOD  loadResource( nType, pName )
   METHOD  openExternalLinks()
   METHOD  openLinks()
   METHOD  searchPaths()
   METHOD  setOpenExternalLinks( lOpen )
   METHOD  setOpenLinks( lOpen )
   METHOD  setSearchPaths( pPaths )
   METHOD  source()
   METHOD  backward()
   METHOD  forward()
   METHOD  home()
   METHOD  reload()
   METHOD  setSource( pName )

   ENDCLASS


METHOD QTextBrowser:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBrowser( ... )
   RETURN Self


METHOD QTextBrowser:backwardHistoryCount()
   RETURN Qt_QTextBrowser_backwardHistoryCount( ::pPtr )


METHOD QTextBrowser:clearHistory()
   RETURN Qt_QTextBrowser_clearHistory( ::pPtr )


METHOD QTextBrowser:forwardHistoryCount()
   RETURN Qt_QTextBrowser_forwardHistoryCount( ::pPtr )


METHOD QTextBrowser:historyTitle( nI )
   RETURN Qt_QTextBrowser_historyTitle( ::pPtr, nI )


METHOD QTextBrowser:historyUrl( nI )
   RETURN Qt_QTextBrowser_historyUrl( ::pPtr, nI )


METHOD QTextBrowser:isBackwardAvailable()
   RETURN Qt_QTextBrowser_isBackwardAvailable( ::pPtr )


METHOD QTextBrowser:isForwardAvailable()
   RETURN Qt_QTextBrowser_isForwardAvailable( ::pPtr )


METHOD QTextBrowser:loadResource( nType, pName )
   RETURN Qt_QTextBrowser_loadResource( ::pPtr, nType, hbqt_ptr( pName ) )


METHOD QTextBrowser:openExternalLinks()
   RETURN Qt_QTextBrowser_openExternalLinks( ::pPtr )


METHOD QTextBrowser:openLinks()
   RETURN Qt_QTextBrowser_openLinks( ::pPtr )


METHOD QTextBrowser:searchPaths()
   RETURN Qt_QTextBrowser_searchPaths( ::pPtr )


METHOD QTextBrowser:setOpenExternalLinks( lOpen )
   RETURN Qt_QTextBrowser_setOpenExternalLinks( ::pPtr, lOpen )


METHOD QTextBrowser:setOpenLinks( lOpen )
   RETURN Qt_QTextBrowser_setOpenLinks( ::pPtr, lOpen )


METHOD QTextBrowser:setSearchPaths( pPaths )
   RETURN Qt_QTextBrowser_setSearchPaths( ::pPtr, hbqt_ptr( pPaths ) )


METHOD QTextBrowser:source()
   RETURN Qt_QTextBrowser_source( ::pPtr )


METHOD QTextBrowser:backward()
   RETURN Qt_QTextBrowser_backward( ::pPtr )


METHOD QTextBrowser:forward()
   RETURN Qt_QTextBrowser_forward( ::pPtr )


METHOD QTextBrowser:home()
   RETURN Qt_QTextBrowser_home( ::pPtr )


METHOD QTextBrowser:reload()
   RETURN Qt_QTextBrowser_reload( ::pPtr )


METHOD QTextBrowser:setSource( pName )
   RETURN Qt_QTextBrowser_setSource( ::pPtr, hbqt_ptr( pName ) )

