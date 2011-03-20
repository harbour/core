/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ xbpHtmlViewer compatible Class
 *
 *                            Pritpal Bedi
 *                              02Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

#define evBeforeNavigate     100
#define evNavigateComplete   101
#define evStatusTextChange   102
#define evDownloadComplete   104
#define evCommandStateChange 105
#define evDownloadBegin      106
#define evProgressChange     108
#define evTitleChange        113

#define evPropertyChange     112
#define evBeforeNavigate2    250
#define evNavigateComplete2  252
#define evDocumentComplete   259
#define evNavigateError      271

/*----------------------------------------------------------------------*/

CLASS XbpHTMLViewer INHERIT XbpWindow

   DATA     oURL
   DATA     cSelectedText

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   execSlot( cSlot, p )

   METHOD   setHTML( cHTML )                      INLINE  ::oWidget:setHTML( cHTML )

   METHOD   back()                                INLINE  ::oWidget:back()
   METHOD   forward()                             INLINE  ::oWidget:forward()
   METHOD   home()                                INLINE  Self
   METHOD   search()                              INLINE  Self
   METHOD   isBusy()                              INLINE  .f.
   METHOD   refresh()                             INLINE  ::oWidget:reload()
   METHOD   stop()                                INLINE  ::oWidget:stop()

   METHOD   navigate( cURL )

   DATA     sl_beforeNavigate                                                  PROTECTED
   ACCESS   beforeNavigate                        INLINE ::sl_beforeNavigate
   ASSIGN   beforeNavigate( bBlock )              INLINE ::sl_beforeNavigate := bBlock

   DATA     sl_navigateComplete                                                PROTECTED
   ACCESS   navigateComplete                      INLINE ::sl_navigateComplete
   ASSIGN   navigateComplete( bBlock )            INLINE ::sl_navigateComplete := bBlock

   DATA     sl_statusTextChange                                                PROTECTED
   ACCESS   statusTextChange                      INLINE ::sl_statusTextChange
   ASSIGN   statusTextChange( bBlock )            INLINE ::sl_statusTextChange := bBlock

   DATA     sl_progressChange                                                  PROTECTED
   ACCESS   progressChange                        INLINE ::sl_progressChange
   ASSIGN   progressChange( bBlock )              INLINE ::sl_progressChange := bBlock

   DATA     sl_titleChange                                                     PROTECTED
   ACCESS   titleChange                           INLINE ::sl_titleChange
   ASSIGN   titleChange( bBlock )                 INLINE ::sl_titleChange := bBlock

   DATA     sl_documentComplete                                                PROTECTED
   ACCESS   documentComplete                      INLINE ::sl_documentComplete
   ASSIGN   documentComplete( bBlock )            INLINE ::sl_documentComplete := bBlock

   DATA     CLSID                                 INIT   "Shell.Explorer"      PROTECTED

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   #if 0  /* Discontinued till QWebKit is integrated separately - Pritpal */
   ::oWidget := QWebView( ::pParent )

   ::oWidget:connect( "iconChanged()"            , {|p| ::execSlot( "iconChanged()"            , p ) } )
   ::oWidget:connect( "linkClicked(QUrl)"        , {|p| ::execSlot( "linkClicked(QUrl)"        , p ) } )
   ::oWidget:connect( "loadFinished(bool)"       , {|p| ::execSlot( "loadFinished(bool)"       , p ) } )
   ::oWidget:connect( "loadProgress(int)"        , {|p| ::execSlot( "loadProgress(int)"        , p ) } )
   ::oWidget:connect( "loadStarted()"            , {|p| ::execSlot( "loadStarted()"            , p ) } )
   ::oWidget:connect( "titleChanged(QString)"    , {|p| ::execSlot( "titleChanged(QString)"    , p ) } )
   ::oWidget:connect( "urlChanged(QUrl)"         , {|p| ::execSlot( "urlChanged(QUrl)"         , p ) } )
   ::oWidget:connect( "selectionChanged()"       , {|p| ::execSlot( "selectionChanged()"       , p ) } )
   ::oWidget:connect( "statusBarMessage(QString)", {|p| ::execSlot( "statusBarMessage(QString)", p ) } )
   #if 0
   ::mapEvent( evNavigateComplete, {| cURL | ::xNavigateComplete( cURL ) } )
   #endif

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:addChild( SELF )

   #endif
   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:destroy()

   ::oWidget:stop()

   IF !empty( ::oURL )
      ::oURL := NIL
   ENDIF
   ::sl_beforeNavigate       := NIL
   ::sl_navigateComplete     := NIL
   ::sl_statusTextChange     := NIL
   ::sl_progressChange       := NIL
   ::sl_titleChange          := NIL
   ::sl_documentComplete     := NIL

   ::xbpWindow:destroy()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:execSlot( cSlot, p )

   DO CASE
   CASE cSlot == "iconChanged()"
      IF hb_isBlock( ::sl_beforeNavigate )
         eval( ::sl_beforeNavigate, /*cURL*/, NIL, Self )
      ENDIF
   CASE cSlot == "linkClicked(QUrl)"
   CASE cSlot == "loadFinished(bool)"
      IF hb_isBlock( ::sl_documentComplete )
         eval( ::sl_documentComplete, /*cURI*/, p, Self )
      ENDIF
   CASE cSlot == "loadProgress(int)"
      IF hb_isBlock( ::sl_progressChange )
         eval( ::sl_progressChange, p, 100, Self )
      ENDIF
   CASE cSlot == "loadStarted()"
   CASE cSlot == "titleChanged(QString)"
      IF hb_isBlock( ::sl_titleChange )
         eval( ::sl_titleChange, p, NIL, Self )
      ENDIF
   CASE cSlot == "urlChanged(QUrl)"
   CASE cSlot == "selectionChanged()"
      ::cSelectedText := ::oWidget:selectedText()
HB_TRACE( HB_TR_DEBUG, ::cSelectedText )
   CASE cSlot == "statusBarMessage(QString)"
      IF hb_isBlock( ::sl_statusTextChange )
         eval( ::sl_statusTextChange, p, NIL, Self )
      ENDIF
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:navigate( cURL )

   IF empty( ::oURL )
      ::oURL := QUrl()
   ENDIF

   ::oURL:setURL( cURL )

   ::oWidget:setURL( ::oURL )

   RETURN .t.

/*----------------------------------------------------------------------*/
#if 0                                        /* Some reference material */

// QWebView
"iconChanged()"
"linkClicked(QUrl)"
"loadFinished(bool)"
"loadProgress(int)"
"loadStarted()"
"titleChanged(QString)"
"urlChanged(QUrl)"
"selectionChanged()"
"statusBarMessage(QString)"

// QWebPage
"contentsChanged()"
"databaseQuotaExceeded(QWebFrame,QString)"
"downloadRequested(QNetworkRequest)"
"frameCreated(QWebFrame)"
"geometryChangeRequested(QRect)"
"linkClicked(QUrl)"
"linkHovered(QString,QString,QString)"
"loadFinished(bool)"
"loadProgress(int)"
"loadStarted()"
"menuBarVisibilityChangeRequested(bool)"
"microFocusChanged()"
"printRequested(QWebFrame)"
"repaintRequested(QRect)"
"restoreFrameStateRequested(QWebFrame)"
"saveFrameStateRequested(QWebFrame,QWebHistoryItem)"
"scrollRequested(int,int,QRect)"
"statusBarMessage(QString)"
"statusBarVisibilityChangeRequested(bool)"
"toolBarVisibilityChangeRequested(bool)"
"unsupportedContent(QNetworkReply)"
"windowCloseRequested()"

#endif
/*----------------------------------------------------------------------*/
