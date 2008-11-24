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
//                  Xbase++ xbpToolBar Compatible Class
//
//                  Pritpal Bedi <pritpal@vouchcac.com>
//                              23Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'hbgtinfo.ch'
#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//

CLASS WvgToolBar  INHERIT  WvgActiveXControl

   DATA     appearance
   DATA     style                                 INIT WVGTOOLBAR_STYLE_STANDARD
   DATA     allowCustomize                        INIT .T.
   DATA     enabled                               INIT .T.
   DATA     showToolTips                          INIT .T.
   DATA     borderStyle                           INIT WVGFRAME_NONE
   DATA     wrappable                             INIT .T.
   DATA     buttonWidth                           INIT 0
   DATA     buttonHeight                          INIT 0
   DATA     textAlign                             INIT WVGALIGN_BOTTOM
   DATA     imageWidth                            INIT 0
   DATA     imageHeight                           INIT 0
   DATA     transparentColor                      INIT 0

   DATA     aItems                                INIT {}

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   addItem()
   METHOD   delItem()
   METHOD   getItem()
   METHOD   numItems()                            INLINE Len( ::aItems )
   METHOD   clear()
   METHOD   customize()
   METHOD   loadImageSet()
   METHOD   saveToolbar()
   METHOD   restToolbar()
   METHOD   setPosAndSize()
   METHOD   setSize()

   DATA     sl_buttonClick
   DATA     sl_change
   DATA     sl_buttonMenuClick
   DATA     sl_buttonDropDown

   METHOD   buttonClick()                         SETGET
   METHOD   change()                              SETGET
   METHOD   buttonMenuClick()                     SETGET
   METHOD   buttonDropDown()                      SETGET


   ENDCLASS
//----------------------------------------------------------------------//

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::WvgActiveXControl:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

//----------------------------------------------------------------------//

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

//----------------------------------------------------------------------//

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

//----------------------------------------------------------------------//

METHOD destroy()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD addItem()

   RETURN Self

//----------------------------------------------------------------------//

METHOD delItem()

   RETURN Self

//----------------------------------------------------------------------//

METHOD getItem()

   RETURN Self

//----------------------------------------------------------------------//

METHOD clear()

   RETURN Self

//----------------------------------------------------------------------//

METHOD customize()

   RETURN Self

//----------------------------------------------------------------------//

METHOD loadImageSet()

   RETURN Self

//----------------------------------------------------------------------//

METHOD saveToolbar()

   RETURN Self

//----------------------------------------------------------------------//

METHOD restToolbar()

   RETURN Self

//----------------------------------------------------------------------//

METHOD setPosAndSize()

   RETURN Self

//----------------------------------------------------------------------//

METHOD setSize()

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonClick := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD change( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_change := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonMenuClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonMenuClick := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonDropDown( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonDropDown := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

