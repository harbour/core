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

#define QT_PTROF( oObj )  ( oObj:pPtr )

/*----------------------------------------------------------------------*/

INIT PROCEDURE Qt_Start()
   qt_qapplication()
   RETURN

EXIT PROCEDURE Qt_End()
   qt_qapplication_exec()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   Local oLabel
   Local oWnd
   Local oMenuBar
   Local oMenuA
   LOCAL oPS, oPPrv, oMB, oWZ, oCD, oWP

   oWnd := QMainWindow():New()
   oWnd:SetWindowTitle("Testing - QMainWindow, QMenu, QMenuBar and QLabel" )
   oWnd:Resize( { 640, 400 } )

   oMenuBar := QMenuBar():new( QT_PTROF( oWnd ) )
   oMenuBar:resize( { oWnd:width(), 20 } )
   oMenuBar:addAction( "First" )
   oMenuBar:addSeparator()
   oMenuBar:addAction( "Second" )

   oMenuA := QMenu():new( QT_PTROF( oMenuBar ) )
   oMenuA:setTitle( "New" )
   oMenuA:addAction( "File" )
   oMenuA:addAction( "Open" )
   oMenuA:addSeparator()
   oMenuA:addAction( "Close" )
   oMenuBar:addMenu( QT_PTROF( oMenuA ) )

   oLabel := QLabel():New( QT_PTROF( oWnd ) )
   oLabel:SetText( "Testing Harbour + Qt" )
   oLabel:move( { 100,100 } )
   oLabel:Show()

   oWnd:Show()

   oPS := QPageSetupDialog():new()
   oPS:setWindowTitle( "Harbour-QT PageSetup Dialog" )
   oPS:show()
   oPPrv := QPrintPreviewDialog():new()
   oPPrv:setWindowTitle( "Harbour-QT Preview Preview Dialog" )
   oPPrv:show()
   oWZ := QWizard():new()
   oWZ:setWindowTitle( "Harbour-QT Wizard to Show Slides etc." )
   oWZ:show()
   oCD := QColorDialog():new()
   oCD:setWindowTitle( "Harbour-QT Color Selection Dialog" )
   oCD:show()
   oWP := QWebView():new()
   oWP:setWindowTitle( "Harbour-QT Web Page Navigator" )
   oWP:show()

   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE HB_GtSys()
   HB_GT_GUI_DEFAULT()
   RETURN

/*----------------------------------------------------------------------*/

