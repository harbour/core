/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
 *                 Pritpal Bedi  <pritpal@vouchcac.com>
 *                              02Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

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

   METHOD   new()
   METHOD   create()
   METHOD   exeBlock()

   METHOD   setHTML( cHTML )                      INLINE  ::oWidget:setHTML( cHTML )

   METHOD   back()                                INLINE  ::oWidget:back()
   METHOD   forward()                             INLINE  ::oWidget:forward()
   METHOD   home()                                INLINE  Self
   METHOD   search()                              INLINE  Self
   METHOD   isBusy()                              INLINE  .f.
   METHOD   refresh()                             INLINE  ::oWidget:reload()
   METHOD   stop()                                INLINE  ::oWidget:stop()

   METHOD   navigate( cURL, cCGI )


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

METHOD XbpHTMLViewer:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QWebView():new( ::pParent )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   ::Connect( QT_PTROF( ::oWidget ), "iconChanged()"            , {|o,p| ::exeBlock( 1,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "linkClicked(QUrl)"        , {|o,p| ::exeBlock( 2,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "loadFinished(bool)"       , {|o,p| ::exeBlock( 3,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "loadProgress(int)"        , {|o,p| ::exeBlock( 4,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "loadStarted()"            , {|o,p| ::exeBlock( 5,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "titleChanged(QString)"    , {|o,p| ::exeBlock( 6,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "urlChanged(QUrl)"         , {|o,p| ::exeBlock( 7,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "selectionChanged()"       , {|o,p| ::exeBlock( 8,p,o ) } )
   ::Connect( QT_PTROF( ::oWidget ), "statusBarMessage(QString)", {|o,p| ::exeBlock( 9,p,o ) } )
   #if 0
   ::mapEvent( evNavigateComplete, {| cURL | ::xNavigateComplete( cURL ) } )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:exeBlock( nEvent, p1 )

//hb_outDebug( str( nEvent ) )
   DO CASE
   CASE nEvent == 1
      IF hb_isBlock( ::sl_beforeNavigate )
         eval( ::sl_beforeNavigate, /*cURL*/, NIL, Self )
      ENDIF
   CASE nEvent == 2
   CASE nEvent == 3
      IF hb_isBlock( ::sl_documentComplete )
         eval( ::sl_documentComplete, /*cURI*/, p1, Self )
      ENDIF
   CASE nEvent == 4
//hb_outDebug( str( p1 ) )
      IF hb_isBlock( ::sl_progressChange )
         eval( ::sl_progressChange, p1, 100, Self )
      ENDIF
   CASE nEvent == 5
   CASE nEvent == 6
      IF hb_isBlock( ::sl_titleChange )
         eval( ::sl_titleChange, p1, NIL, Self )
      ENDIF
   CASE nEvent == 7
   CASE nEvent == 8
      ::cSelectedText := ::oWidget:SelectedText()
hb_outDebug( ::cSelectedText )
   CASE nEvent == 9
      IF hb_isBlock( ::sl_statusTextChange )
         eval( ::sl_statusTextChange, p1, NIL, Self )
      ENDIF
   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpHTMLViewer:navigate( cURL )
   LOCAL oURL

   oURL := QUrl():new()
   oURL:setURL( cURL )

   ::oWidget:setURL( QT_PTROF( oURL ) )

   RETURN .t.

/*----------------------------------------------------------------------*/

