/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ScrollBar class
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

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

CREATE CLASS SCROLLBAR FUNCTION HBScrollBar

   EXPORTED:

   VAR cargo

   METHOD barLength() SETGET
   METHOD bitmaps( aBitmaps ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD current( nCurrent ) SETGET
   METHOD end( nEnd ) SETGET
   METHOD offset( nOffset ) SETGET
   METHOD orient( nOrient ) SETGET
   METHOD sBlock( bSBlock ) SETGET
   METHOD start( nStart ) SETGET
   METHOD style( cStyle ) SETGET
   METHOD thumbPos( nThumbPos ) SETGET
   METHOD total( nTotal ) SETGET

   METHOD display()
   METHOD update()
   METHOD hitTest( nMRow, nMCol )

   METHOD New( nStart, nEnd, nOffset, bSBlock, nOrient ) /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR aBitmaps
   VAR nBarLength
   VAR cColorSpec
   VAR cStyle
   VAR nCurrent   INIT 1
   VAR nEnd       INIT 0
   VAR nOffset
   VAR nOrient
   VAR nStart     INIT 0
   VAR nThumbPos  INIT 1
   VAR nTotal     INIT 100
   VAR bSBlock

   VAR lOverride  INIT .F.

   METHOD CalcThumbPos()

ENDCLASS

METHOD display() CLASS SCROLLBAR

   LOCAL cColor
   LOCAL cStyle
   LOCAL nOffset
   LOCAL nStart
   LOCAL nEnd
   LOCAL nPos

   IF ::CalcThumbPos()

      cStyle    := ::cStyle
      nOffset   := ::nOffset
      nStart    := ::nStart
      nEnd      := ::nEnd - 1

      DispBegin()

      cColor := hb_ColorIndex( ::cColorSpec, 0 )

      IF ::nOrient == SCROLL_VERTICAL

         FOR nPos := nStart + 1 TO nEnd
            hb_dispOutAt( nPos, nOffset, SubStr( cStyle, 2, 1 ), cColor )
         NEXT

         cColor := hb_ColorIndex( ::cColorSpec, 1 )
         hb_dispOutAt( nStart, nOffset, SubStr( cStyle, 1, 1 ), cColor )
         hb_dispOutAt( nStart + ::nThumbPos, nOffset, SubStr( cStyle, 3, 1 ), cColor )
         hb_dispOutAt( nEnd + 1, nOffset, SubStr( cStyle, 4, 1 ), cColor )
      ELSE

         hb_dispOutAt( nOffset, nStart + 1, Replicate( SubStr( cStyle, 2, 1 ), nEnd - nStart ), cColor )

         cColor := hb_ColorIndex( ::cColorSpec, 1 )
         hb_dispOutAt( nOffset, nStart, SubStr( cStyle, 1, 1 ), cColor )
         hb_dispOutAt( nOffset, nStart + ::nThumbPos, SubStr( cStyle, 3, 1 ), cColor )
         hb_dispOutAt( nOffset, nEnd + 1, SubStr( cStyle, 4, 1 ), cColor )

      ENDIF

      DispEnd()

      RETURN .T.
   ENDIF

   RETURN .F.

METHOD update() CLASS SCROLLBAR

   LOCAL nOldThumbPos := ::nThumbPos

   IF HB_ISBLOCK( ::bSBlock )
      Eval( ::bSBlock )
   ENDIF

   IF ::CalcThumbPos() .AND. nOldThumbPos != ::nThumbPos

      DispBegin()

      IF ::nOrient == SCROLL_VERTICAL
         hb_dispOutAt( ::nStart + nOldThumbPos, ::nOffSet, SubStr( ::cStyle, 2,  1), hb_ColorIndex( ::cColorSpec, 0 ) )
         hb_dispOutAt( ::nStart + ::nThumbPos, ::nOffset, SubStr( ::cStyle, 3, 1 ), hb_ColorIndex( ::cColorSpec, 1 ) )
      ELSE
         hb_dispOutAt( ::nOffset, ::nStart + nOldThumbPos, SubStr( ::cStyle, 2, 1 ), hb_ColorIndex( ::cColorSpec, 0 ) )
         hb_dispOutAt( ::nOffset, ::nStart + ::nThumbPos, SubStr( ::cStyle, 3, 1 ), hb_ColorIndex( ::cColorSpec, 1 ) )
      ENDIF

      DispEnd()

      RETURN .T.
   ENDIF

   RETURN .F.

METHOD hitTest( nMRow, nMCol ) CLASS SCROLLBAR

   IF ::nOrient == SCROLL_VERTICAL

      DO CASE
      CASE nMCol != ::nOffset
      CASE nMRow < ::nStart
      CASE nMRow > ::nEnd
      CASE nMRow == ::nStart
         RETURN HTSCROLLUNITDEC
      CASE nMRow == ::nEnd
         RETURN HTSCROLLUNITINC
      CASE nMRow < ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKDEC
      CASE nMRow > ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKINC
      CASE nMRow == ::nThumbPos + ::nStart
         RETURN HTSCROLLTHUMBDRAG
      ENDCASE

      IF nMCol == ::nOffset + 1 .OR. nMCol == ::nOffset

         DO CASE
         CASE nMCol != ::nOffset .AND. nMCol != ::nOffset + 1
         CASE nMRow < ::nStart
         CASE nMRow > ::nEnd
         CASE nMRow == ::nStart
            RETURN HTSCROLLUNITDEC
         CASE nMRow == ::nEnd
            RETURN HTSCROLLUNITINC
         CASE nMRow < ::nThumbPos + ::nStart
            RETURN HTSCROLLBLOCKDEC
         CASE nMRow > ::nThumbPos + ::nStart
            RETURN HTSCROLLBLOCKINC
         CASE nMRow == ::nThumbPos + ::nStart
            RETURN HTSCROLLTHUMBDRAG
         ENDCASE

      ENDIF

   ELSE

      DO CASE
      CASE nMRow != ::nOffset
      CASE nMCol < ::nStart
      CASE nMCol > ::nEnd
      CASE nMCol == ::nStart
         RETURN HTSCROLLUNITDEC
      CASE nMCol == ::nEnd
         RETURN HTSCROLLUNITINC
      CASE nMCol < ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKDEC
      CASE nMCol > ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKINC
      CASE nMCol == ::nThumbPos + ::nStart
         RETURN HTSCROLLTHUMBDRAG
      ENDCASE

   ENDIF

   RETURN HTNOWHERE

METHOD barLength() CLASS SCROLLBAR
   RETURN ::nBarLength

METHOD bitmaps( aBitmaps ) CLASS SCROLLBAR

   IF HB_ISARRAY( aBitmaps ) .AND. ;
      Len( aBitmaps ) == 3

      ::aBitmaps := aBitmaps
   ENDIF

   RETURN ::aBitmaps

METHOD colorSpec( cColorSpec ) CLASS SCROLLBAR

   IF HB_ISSTRING( cColorSpec ) .AND. ;
      !Empty( hb_ColorIndex( cColorSpec, 1 ) ) .AND. ;
       Empty( hb_ColorIndex( cColorSpec, 2 ) )

      ::cColorSpec := cColorSpec
   ENDIF

   RETURN ::cColorSpec

METHOD current( nCurrent ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nCurrent ) .AND. ;
      nCurrent <= ::nTotal .AND. ;
      nCurrent != ::nCurrent

      ::nCurrent := nCurrent
   ENDIF

   RETURN ::nCurrent

METHOD end( nEnd ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nEnd ) .AND. ;
      nEnd >= ::nStart .AND. ;
      nEnd != ::nEnd

      ::nEnd := nEnd
      ::nBarLength := nEnd - ::nStart - 1
   ENDIF

   RETURN ::nEnd

METHOD offset( nOffset ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nOffset ) .AND. ;
      nOffset != ::nOffset

      ::nOffset := nOffset
   ENDIF

   RETURN ::nOffset

METHOD orient( nOrient ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nOrient ) .AND. ;
      ( nOrient == SCROLL_VERTICAL .OR. nOrient == SCROLL_HORIZONTAL )

      ::nOrient := nOrient
   ENDIF

   RETURN ::nOrient

METHOD sBlock( bSBlock ) CLASS SCROLLBAR

   IF HB_ISBLOCK( bSBlock )
      ::bSBlock := bSBlock
   ENDIF

   RETURN ::bSBlock

METHOD start( nStart ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nStart ) .AND. ;
      nStart <= ::nEnd .AND. ;
      nStart != ::nStart

      ::nStart := nStart
      ::nBarLength := ::nEnd - nStart - 1
   ENDIF

   RETURN ::nStart

METHOD style( cStyle ) CLASS SCROLLBAR

   IF HB_ISSTRING( cStyle ) .AND. ;
      Len( cStyle ) == 4

      ::cStyle := cStyle
   ENDIF

   RETURN ::cStyle

METHOD thumbPos( nThumbPos ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nThumbPos )

      IF nThumbPos < 1
         ::nThumbPos := 1
      ELSEIF nThumbPos >= ::nBarLength
         ::nThumbPos := ::nBarLength
      ELSEIF nThumbPos >= ::nBarLength - 1
         ::nThumbPos := nThumbPos
      ELSE
         ::nThumbPos := nThumbPos
      ENDIF

      ::lOverride := ( nThumbPos != 0 )
   ENDIF

   RETURN ::nThumbPos

METHOD total( nTotal ) CLASS SCROLLBAR

   IF HB_ISNUMERIC( nTotal ) .AND. ;
      nTotal >= 2 .AND. ;
      nTotal != ::nTotal

      ::nTotal := nTotal
   ENDIF

   RETURN ::nTotal

METHOD CalcThumbPos() CLASS SCROLLBAR

   LOCAL nBarLength := ::nBarLength
   LOCAL nTotal := ::nTotal

   IF nBarLength < 2 .OR. nTotal < 2
      RETURN .F.
   ENDIF

   IF !::lOverride
      ::nThumbPos := Min( Max( Round( ::nCurrent * ( ( nBarLength - 1 ) / nTotal ) + 1, 0 ), 1 ), nBarLength )
   ENDIF

   RETURN .T.

/* New definitions for better coding. These are screen codepage dependent,
   but can be changed with the setStyle method. */
#define SB_UPARROW      Chr( 24 )
#define SB_DNARROW      Chr( 25 )
#define SB_RIGHTARROW   Chr( 26 )
#define SB_LEFTARROW    Chr( 27 )

#define SB_THUMB        Chr( 176 )
#define SB_TRACK        Chr( 178 )

METHOD New( nStart, nEnd, nOffset, bSBlock, nOrient ) CLASS SCROLLBAR

   LOCAL cColor

   __defaultNIL( @nOrient, SCROLL_VERTICAL )

   IF ! HB_ISNUMERIC( nStart ) .OR. ;
      ! HB_ISNUMERIC( nEnd ) .OR. ;
      ! HB_ISNUMERIC( nOffset ) .OR. ;
      ! ValType( bSBlock ) $ "BU" .OR. ;
      ! HB_ISNUMERIC( nOrient ) .OR. ;
      ( nOrient != SCROLL_VERTICAL .AND. nOrient != SCROLL_HORIZONTAL )
      RETURN NIL
   ENDIF

   ::end        := nEnd
   ::offSet     := nOffset
   ::orient     := nOrient
   ::sBlock     := bSBlock
   ::start      := nStart
   ::nBarLength := nEnd - nStart - 1

   IF nOrient == SCROLL_VERTICAL
      ::cStyle := SB_UPARROW + SB_THUMB + SB_TRACK + SB_DNARROW
      ::aBitmaps := { "arrow_u.bmu", "arrow_d.bmu", "arrow_e.bmu" }
   ELSEIF nOrient == SCROLL_HORIZONTAL
      ::cStyle := SB_LEFTARROW + SB_THUMB + SB_TRACK + SB_RIGHTARROW
      ::aBitmaps := { "arrow_l.bmu", "arrow_r.bmu", "arrow_e.bmu" }
   ENDIF

   cColor := SetColor()
   ::cColorSpec := hb_ColorIndex( cColor, CLR_UNSELECTED ) + "," + ;
                   hb_ColorIndex( cColor, CLR_ENHANCED )

   RETURN Self

FUNCTION ScrollBar( nStart, nEnd, nOffset, bSBlock, nOrient )
   RETURN HBScrollBar():New( nStart, nEnd, nOffset, bSBlock, nOrient )

#endif
