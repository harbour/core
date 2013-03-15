/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PushButton class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"
#include "setcurs.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

CREATE CLASS PushButton FUNCTION HBPushButton

   EXPORTED:

   VAR cargo                          /* NOTE: CA-Cl*pper 5.3 has a bug, where this var cannot be assigned NIL. */

   VAR bmpXOff    INIT -1             /* NOTE: Fully compatible behaviour not implemented. */
   VAR bmpYOff    INIT -1             /* NOTE: Fully compatible behaviour not implemented. */
   VAR capXOff    INIT -1             /* NOTE: Fully compatible behaviour not implemented. */
   VAR capYOff    INIT -1             /* NOTE: Fully compatible behaviour not implemented. */
   VAR sizeX      INIT 0              /* NOTE: Fully compatible behaviour not implemented. */
   VAR sizeY      INIT 0              /* NOTE: Fully compatible behaviour not implemented. */

   METHOD display()
   METHOD hitTest( nMRow, nMCol )
   METHOD killFocus()
   METHOD select( nPos )
   METHOD setFocus()

   METHOD bitmap( cBitmap ) SETGET
   METHOD buffer() SETGET
   METHOD caption( cCaption ) SETGET
   METHOD col( nCol ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD fBlock( bFBlock ) SETGET
   METHOD hasFocus() SETGET
   METHOD message( cMessage ) SETGET
   METHOD row( nRow ) SETGET
   METHOD sBlock( bSBlock ) SETGET
   METHOD typeOut() SETGET
   METHOD style( cStyle ) SETGET

   METHOD New( nRow, nCol, cCaption ) /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR cBitmap    INIT ""
   VAR lBuffer    INIT .F.
   VAR cCaption
   VAR nCol
   VAR cColorSpec
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cMessage   INIT ""
   VAR nRow
   VAR bSBlock
   VAR cStyle     INIT "<>"
   VAR lTypeOut   INIT .F.

ENDCLASS

METHOD setFocus() CLASS PushButton

   IF ! ::lHasFocus
      ::lHasFocus := .T.
      ::display()

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD select( nPos ) CLASS PushButton

   LOCAL nCurPos := nPos

   IF ::lHasFocus
      ::lbuffer := .T.
      ::display()

      IF HB_ISNUMERIC( nPos )

         IF nPos == 32

            Inkey( 0.4 )
            DO WHILE nCurPos == 32
               nCurPos := Inkey( 0.1 )
            ENDDO
         ELSE
            DO WHILE nPos == Inkey( 0 )
            ENDDO
         ENDIF
      ENDIF

      IF HB_ISBLOCK( ::bSBlock )
         Eval( ::bSBlock )
      ENDIF

      ::lBuffer := .F.
      ::display()
   ENDIF

   RETURN Self

METHOD killFocus() CLASS PushButton

   IF ::lHasFocus
      ::lHasFocus := .F.

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD hitTest( nMRow, nMCol ) CLASS PushButton

   LOCAL nCurrentPos := 1
   LOCAL nLen := Len( ::cCaption )
   LOCAL nStyleLen
   LOCAL nAccelPos

   IF ( nAccelPos := At( "&", ::cCaption ) ) > 0 .AND. nAccelPos < nLen
      nLen--
   ENDIF

   IF ( nStyleLen := Len( ::cStyle ) ) == 2
      nLen += 2
   ELSEIF nStyleLen == 8
      nCurrentPos := 3
      nLen += 2
   ENDIF

   IF nMRow >= ::Row .AND. ;
      nMCol >= ::Col .AND. ;
      nMRow < ::Row + nCurrentPos .AND. ;
      nMCol < ::Col + nLen
      RETURN HTCLIENT
   ENDIF

   RETURN HTNOWHERE

METHOD display() CLASS PushButton

   LOCAL cColor
   LOCAL cStyle := ::cStyle
   LOCAL cCaption := ::cCaption
   LOCAL nRow := ::nRow
   LOCAL nCol := ::nCol
   LOCAL nPos

   DispBegin()

   IF ::lBuffer
      cColor := hb_ColorIndex( ::cColorSpec, 2 )
   ELSEIF ::lHasFocus
      cColor := hb_ColorIndex( ::cColorSpec, 1 )
   ELSE
      cColor := hb_ColorIndex( ::cColorSpec, 0 )
   ENDIF

   IF ( nPos := At( "&", cCaption ) ) == 0
   ELSEIF nPos == Len( cCaption )
      nPos := 0
   ELSE
      cCaption := Stuff( cCaption, nPos, 1, "" )
   ENDIF

   IF ! Empty( cStyle )

      nCol++

      IF Len( cStyle ) == 2
         hb_DispOutAt( ::nRow, ::nCol, SubStr( cStyle, 1, 1 ), cColor )
         hb_DispOutAt( ::nRow, ::nCol + Len( cCaption ) + 1, SubStr( cStyle, 2, 1 ), cColor )
      ELSE
         nRow++
         hb_DispBox( ::nRow, ::nCol, ::nRow + 2, ::nCol + Len( cCaption ) + 1, cStyle, cColor )
      ENDIF
   ENDIF

   IF ! Empty( cCaption )

      hb_DispOutAt( nRow, nCol, cCaption, cColor )

      IF nPos != 0
         hb_DispOutAt( nRow, nCol + nPos - 1, SubStr( cCaption, nPos, 1 ), hb_ColorIndex( ::cColorSpec, 3 ) )
      ENDIF

   ENDIF

   DispEnd()

   RETURN Self

METHOD bitmap( cBitmap ) CLASS PushButton

   IF cBitmap != NIL
      ::cBitmap := __eInstVar53( Self, "BITMAP", cBitmap, "C", 1001 )
   ENDIF

   RETURN ::cBitmap

METHOD buffer() CLASS PushButton
   RETURN ::lBuffer

METHOD caption( cCaption ) CLASS PushButton

   IF cCaption != NIL
      ::cCaption := __eInstVar53( Self, "CAPTION", cCaption, "C", 1001 )
   ENDIF

   RETURN ::cCaption

METHOD col( nCol ) CLASS PushButton

   IF nCol != NIL
      ::nCol := __eInstVar53( Self, "COL", nCol, "N", 1001 )
   ENDIF

   RETURN ::nCol

METHOD colorSpec( cColorSpec ) CLASS PushButton

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001,;
         {|| ! Empty( hb_ColorIndex( cColorSpec, 3 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 5 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD fBlock( bFBlock ) CLASS PushButton

   IF PCount() > 0
      ::bFBlock := iif( bFBlock == NIL, NIL, __eInstVar53( Self, "FBLOCK", bFBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bFBlock

METHOD hasFocus() CLASS PushButton
   RETURN ::lHasFocus

METHOD message( cMessage ) CLASS PushButton

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD row( nRow ) CLASS PushButton

   IF nRow != NIL
      ::nRow := __eInstVar53( Self, "ROW", nRow, "N", 1001 )
   ENDIF

   RETURN ::nRow

METHOD sBlock( bSBlock ) CLASS PushButton

   IF PCount() > 0
      ::bSBlock := iif( bSBlock == NIL, NIL, __eInstVar53( Self, "SBLOCK", bSBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bSBlock

METHOD typeOut() CLASS PushButton
   RETURN .F.

METHOD style( cStyle ) CLASS PushButton

   IF cStyle != NIL
      ::cStyle := __eInstVar53( Self, "STYLE", cStyle, "C", 1001, {|| Len( cStyle ) == 0 .OR. Len( cStyle ) == 2 .OR. Len( cStyle ) == 8 } )
   ENDIF

   RETURN ::cStyle

METHOD New( nRow, nCol, cCaption ) CLASS PushButton

   LOCAL cColor

   IF ! HB_ISNUMERIC( nRow ) .OR. ;
      ! HB_ISNUMERIC( nCol )
      RETURN NIL
   ENDIF

   __defaultNIL( @cCaption, "" )

   ::caption  := cCaption
   ::nCol     := nCol
   ::nRow     := nRow

   IF IsDefColor()
      ::cColorSpec := "W/N,N/W,W+/N,W+/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," + ;
         hb_ColorIndex( cColor, CLR_STANDARD   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BACKGROUND )
   ENDIF

   RETURN Self

FUNCTION PushButton( nRow, nCol, cCaption )
   RETURN HBPushButton():New( nRow, nCol, cCaption )

FUNCTION _PUSHBUTT_( cCaption, cMessage, cColorSpec, bFBlock, bSBlock, cStyle, nSizeX, nSizeY, nCapXOff, nCapYOff, cBitmap, nBmpXOff, nBmpYOff )

   LOCAL o := HBPushButton():New( Row(), Col(), cCaption )

   o:message   := cMessage
   o:colorSpec := cColorSpec
   o:fBlock    := bFBlock
   o:sBlock    := bSBlock
   o:style     := cStyle
   o:sizeX     := nSizeX
   o:sizeY     := nSizeY
   o:capXOff   := nCapXOff
   o:capYOff   := nCapYOff
   o:bitmap    := cBitmap
   o:bmpXOff   := nBmpXOff
   o:bmpYOff   := nBmpYOff

   RETURN o

#endif
