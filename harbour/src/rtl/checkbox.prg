/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHECKBOX class
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

CREATE CLASS CHECKBOX FUNCTION HBCheckBox

   EXPORTED:

   VAR cargo

   METHOD display()
   METHOD hitTest( nMRow, nMCol )
   METHOD killFocus()
   METHOD select( lState )
   METHOD setFocus()

   METHOD bitmaps( aBitmaps ) SETGET
   METHOD buffer() SETGET
   METHOD capCol( nCapCol ) SETGET
   METHOD capRow( nCapRow ) SETGET
   METHOD caption( cCaption ) SETGET
   METHOD col( nCol ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD fBlock( bFBlock ) SETGET
   METHOD hasFocus() SETGET
   METHOD message( cMessage ) SETGET
   METHOD row( nRow ) SETGET
   METHOD sBlock( bSBlock ) SETGET
   METHOD style( cStyle ) SETGET
   METHOD typeOut() SETGET

   METHOD New( nRow, nCol, cCaption ) /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR aBitmaps   INIT { "check_f.bmu", "check_e.bmu" }
   VAR lBuffer    INIT .F.
   VAR nCapCol
   VAR nCapRow
   VAR cCaption
   VAR nCol
   VAR cColorSpec
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cMessage   INIT ""
   VAR nRow
   VAR bSBlock
   VAR cStyle     INIT hb_UTF8ToStr( "[√ ]" )

   VAR nCursor

ENDCLASS

METHOD setFocus() CLASS CHECKBOX

   IF ! ::lHasFocus
      ::nCursor := SetCursor( SC_NONE )
      ::lHasFocus := .T.
      ::display()

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD select( lState ) CLASS CHECKBOX

   LOCAL lOldState := ::lBuffer

   ::lBuffer := iif( HB_ISLOGICAL( lState ), lState, ! ::lBuffer )

   IF lOldState != ::lBuffer
      ::display()

      IF HB_ISBLOCK( ::bSBlock )
         Eval( ::bSBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD killFocus() CLASS CHECKBOX

   IF ::lHasFocus
      ::lHasFocus := .F.

      IF HB_ISBLOCK( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF

      ::display()
      SetCursor( ::nCursor )

   ENDIF

   RETURN Self

METHOD hitTest( nMRow, nMCol ) CLASS CHECKBOX

   LOCAL nPosAccel
   LOCAL nLenCaption

   IF nMRow == ::nRow .AND. ;
      nMCol >= ::nCol .AND. ;
      nMCol < ::nCol + 3
      RETURN HTCLIENT
   ENDIF

   nLenCaption := Len( ::cCaption )

   IF ( nPosAccel := At( "&", ::cCaption ) ) > 0 .AND. ;
      nPosAccel < nLenCaption
      nLenCaption--
   ENDIF

   IF nMRow == ::nCapRow .AND. ;
      nMCol >= ::nCapCol .AND. ;
      nMCol < ::nCapCol + nLenCaption
      RETURN HTCAPTION
   ENDIF

   RETURN HTNOWHERE

METHOD display() CLASS CHECKBOX

   LOCAL cColor
   LOCAL cStyle := ::cStyle
   LOCAL cCaption
   LOCAL nPos

   DispBegin()

   hb_DispOutAt( ::nRow, ::nCol + 1, iif( ::lBuffer, SubStr( cStyle, 2, 1 ), SubStr( cStyle, 3, 1 ) ), ;
      hb_ColorIndex( ::cColorSpec, iif( ::lHasFocus, 1, 0 ) ) )

   cColor := hb_ColorIndex( ::cColorSpec, 2 )
   hb_DispOutAt( ::nRow, ::nCol, Left( cStyle, 1 ), cColor )
   hb_DispOutAt( ::nRow, ::nCol + 2, Right( cStyle, 1 ), cColor )

   IF ! Empty( cCaption := ::cCaption )

      IF ( nPos := At( "&", cCaption ) ) == 0
      ELSEIF nPos == Len( cCaption )
         nPos := 0
      ELSE
         cCaption := Stuff( cCaption, nPos, 1, "" )
      ENDIF

      IF ::lHasFocus
         cColor := hb_ColorIndex( ::cColorSpec, 3 )
      ENDIF

      hb_DispOutAt( ::nCapRow, ::nCapCol, cCaption, cColor )

      IF ! ::lHasFocus .AND. nPos != 0
         hb_DispOutAt( ::nCapRow, ::nCapCol + nPos - 1, SubStr( cCaption, nPos, 1 ), ;
            hb_ColorIndex( ::cColorSpec, 3 ) )
      ENDIF

   ENDIF

   DispEnd()

   RETURN Self

METHOD bitmaps( aBitmaps ) CLASS CHECKBOX

   IF aBitmaps != NIL
      ::aBitmaps := __eInstVar53( Self, "BITMAPS", aBitmaps, "A", 1001 )
   ENDIF

   RETURN ::aBitmaps

METHOD buffer() CLASS CHECKBOX
   RETURN ::lBuffer

METHOD capCol( nCapCol ) CLASS CHECKBOX

   IF nCapCol != NIL
      ::nCapCol := __eInstVar53( Self, "CAPCOL", nCapCol, "N", 1001 )
   ENDIF

   RETURN ::nCapCol

METHOD capRow( nCapRow ) CLASS CHECKBOX

   IF nCapRow != NIL
      ::nCapRow := __eInstVar53( Self, "CAPROW", nCapRow, "N", 1001 )
   ENDIF

   RETURN ::nCapRow

METHOD caption( cCaption ) CLASS CHECKBOX

   IF cCaption != NIL
      ::cCaption := __eInstVar53( Self, "CAPTION", cCaption, "C", 1001 )
   ENDIF

   RETURN ::cCaption

METHOD col( nCol ) CLASS CHECKBOX

   IF nCol != NIL
      ::nCol := __eInstVar53( Self, "COL", nCol, "N", 1001 )
   ENDIF

   RETURN ::nCol

METHOD colorSpec( cColorSpec ) CLASS CHECKBOX

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         {|| ! Empty( hb_ColorIndex( cColorSpec, 3 ) ) .AND. Empty( hb_ColorIndex( cColorSpec, 4 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD fBlock( bFBlock ) CLASS CHECKBOX

   IF PCount() > 0
      ::bFBlock := iif( bFBlock == NIL, NIL, __eInstVar53( Self, "FBLOCK", bFBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bFBlock

METHOD hasFocus() CLASS CHECKBOX
   RETURN ::lHasFocus

METHOD message( cMessage ) CLASS CHECKBOX

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD row( nRow ) CLASS CHECKBOX

   IF nRow != NIL
      ::nRow := __eInstVar53( Self, "ROW", nRow, "N", 1001 )
   ENDIF

   RETURN ::nRow

METHOD sBlock( bSBlock ) CLASS CHECKBOX

   IF PCount() > 0
      ::bSBlock := iif( bSBlock == NIL, NIL, __eInstVar53( Self, "SBLOCK", bSBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bSBlock

METHOD style( cStyle ) CLASS CHECKBOX

   IF cStyle != NIL
      ::cStyle := __eInstVar53( Self, "STYLE", cStyle, "C", 1001, {|| Len( cStyle ) == 0 .OR. Len( cStyle ) == 4 } )
   ENDIF

   RETURN ::cStyle

METHOD typeOut() CLASS CHECKBOX
   RETURN .F.

METHOD New( nRow, nCol, cCaption ) CLASS CHECKBOX

   LOCAL cColor

   __defaultNIL( @cCaption, "" )

   ::caption  := cCaption
   ::capRow   := nRow
   ::capCol   := nCol + 3 + 1
   ::row      := nRow
   ::col      := nCol

   IF IsDefColor()
      ::cColorSpec := "W/N,W+/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
         hb_ColorIndex( cColor, CLR_ENHANCED   ) + "," + ;
         hb_ColorIndex( cColor, CLR_STANDARD   ) + "," + ;
         hb_ColorIndex( cColor, CLR_BACKGROUND )
   ENDIF

   RETURN Self

FUNCTION _CHECKBOX_( lState, cCaption, cMessage, cColorSpec, bFBlock, bSBlock, cStyle, aBitmaps )

   LOCAL o := HBCheckBox():New( Row(), Col(), cCaption )

   o:select( lState )
   o:caption   := cCaption
   o:message   := cMessage
   o:colorSpec := cColorSpec
   o:fBlock    := bFBlock
   o:sBlock    := bSBlock
   o:style     := cStyle
   o:bitmaps   := aBitmaps

   RETURN o

FUNCTION CheckBox( nRow, nCol, cCaption )
   RETURN HBCheckBox():New( nRow, nCol, cCaption )

#endif
