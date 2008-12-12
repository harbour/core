/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                               EkOnkar
//                         ( The LORD is ONE )
//
//                Xbase++ Compatible xbpPartHandler Class
//
//                  Pritpal Bedi <pritpal@vouchcac.com>
//                               08Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'

#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'wvgparts.ch'

//----------------------------------------------------------------------//

CLASS WvgSysWindow INHERIT WvgPartHandler


   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   disable()
   METHOD   enable()
   METHOD   hide()
   METHOD   show()
   METHOD   setPos()

   METHOD   currentPos()
   METHOD   currentSize()
   METHOD   WndProc()

   DATA     aPos                                  INIT    { 0, 0 }

   DATA     hWnd                                  PROTECTED
   DATA     nOldProc                              PROTECTED
   DATA     nWndProc                              PROTECTED


   DATA     sl_helpRequest
   ACCESS   helpRequest                           INLINE  ::sl_helpRequest
   ASSIGN   helpRequest( bBlock )                 INLINE  ::sl_helpRequest := bBlock

   DATA     sl_move
   ACCESS   move                                  INLINE  ::sl_move
   ASSIGN   move( bBlock )                        INLINE  ::sl_move := bBlock

   DATA     sl_quit
   ACCESS   quit                                  INLINE  ::sl_quit
   ASSIGN   quit( bBlock )                        INLINE  ::sl_quit := bBlock

   ENDCLASS

//----------------------------------------------------------------------//

METHOD new( oParent, oOwner, aPos ) CLASS WvgSysWindow

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner
   DEFAULT aPos    TO ::aPos

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:INIT( oParent, oOwner )

   RETURN Self

//----------------------------------------------------------------------//

METHOD create( oParent, oOwner, aPos ) CLASS WvgSysWindow

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner
   DEFAULT aPos    TO ::aPos

   ::oParent := oParent
   ::oOwner  := oOwner
   ::aPos    := aPos

   ::WvgPartHandler:create( oParent, oOwner )

   RETURN Self

//----------------------------------------------------------------------//

METHOD configure() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD destroy() CLASS WvgSysWindow

   hb_FreeCallBack( ::nWndProc )

   RETURN Self

//----------------------------------------------------------------------//

METHOD disable() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD enable() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD hide() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD show() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD setPos() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD currentPos() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//

METHOD currentSize() CLASS WvgSysWindow

   RETURN Self

//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                       Class WvgFontDialog()
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

CLASS WvgFontDialog INHERIT WvgSysWindow

   DATA     bitmapOnly                            INIT  .F.
   DATA     buttonApply                           INIT  .F.
   DATA     buttonCancel                          INIT  .T.
   DATA     buttonHelp                            INIT  .F.
   DATA     buttonOk                              INIT  .T.
   DATA     buttonReset                           INIT  .F.
   DATA     displayFilter                         INIT  .T.
   DATA     familyName                            INIT  " "
   DATA     fixedOnly                             INIT  .F.
   DATA     name                                  INIT  .T.
   DATA     nominalPointSize                      INIT  10
   DATA     outLine                               INIT  .T.
   DATA     previewBGClr                          INIT  RGB( 255,255,255 )
   DATA     previewFGClr                          INIT  RGB( 0,0,0 )
   DATA     previewString                         INIT  ' '
   DATA     printerFilter                         INIT  .T.
   DATA     printerPS                             INIT  NIL
   DATA     proportionalOnly                      INIT  .T.
   DATA     screenPS                              INIT  NIL
   DATA     size                                  INIT  .T.
   DATA     strikeOut                             INIT  .T.
   DATA     style                                 INIT  .T.
   DATA     synthesizeFonts                       INIT  .T.
   DATA     title                                 INIT  'Font Dialog'
   DATA     underscore                            INIT  .T.
   DATA     vectorOnly                            INIT  .F.
   DATA     vectorSizes                           INIT  {}
   DATA     viewPrinterFonts                      INIT  .F.
   DATA     viewScreenFonts                       INIT  .T.

   METHOD   create()
   METHOD   destroy()
   METHOD   display( nMode )

   DATA     sl_activateApply
   ACCESS   activateApply                         INLINE ::sl_activateApply
   ASSIGN   activateApply( bBlock )               INLINE ::sl_activateApply := bBlock

   DATA     sl_activateCancel
   ACCESS   activateCancel                        INLINE ::sl_activateCancel
   ASSIGN   activateCancel( bBlock )              INLINE ::sl_activateCancel := bBlock

   DATA     sl_activateOk
   ACCESS   activateOk                            INLINE ::sl_activateOk
   ASSIGN   activateOk( bBlock )                  INLINE ::sl_activateOk := bBlock

   DATA     sl_activateReset
   ACCESS   activateReset                         INLINE ::sl_activateReset
   ASSIGN   activateReset( bBlock )               INLINE ::sl_activateReset := bBlock

   DATA     hFont

   ENDCLASS

//----------------------------------------------------------------------//





