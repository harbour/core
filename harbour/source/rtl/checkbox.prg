/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHECKBOX class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbclass.ch"
#include "common.ch"
#include "button.ch"

#ifdef HB_COMPAT_C53

CREATE CLASS CHECKBOX FUNCTION HBCHECKBOX

   VAR Buffer    INIT .f.
   VAR Caption
   VAR CapRow
   VAR CapCol
   VAR Cargo
   VAR Col
   VAR colorspec
   VAR FBlock
   VAR HasFocus  INIT  .f.
   VAR Message   INIT ""
   VAR Row
   VAR SBlock
   VAR Style     INIT "[û ]"
   VAR lCursor
   VAR Typeout   INIT   .f.

   METHOD New( nRow, nCol, cCaption )
   METHOD SetFocus()
   MESSAGE Select( lState )    METHOD _Select( lState )
   METHOD KillFocus()
   METHOD Display()
   METHOD HitTest( nMouseRow, nMouseCol )

ENDCLASS

METHOD New( nRow, nCol, cCaption ) CLASS CHECKBOX

   LOCAL cColor := ""

   ::Buffer   := .f.
   ::Caption  := cCaption
   ::CapRow   := nRow
   ::CapCol   := nCol + 3 + 1
   ::Col      := nCol

   IF IsDefColor()
      ::ColorSpec:="W/N,W+/N,W/N,W+/N"
   ELSE
      cColor  := SetColor()
      ::ColorSpec := __guicolor( cColor, 5 ) + "," + ;
                     __guicolor( cColor, 2 ) + "," + ;
                     __guicolor( cColor, 1 ) + "," + ;
                     __guicolor( cColor, 4 )
   ENDIF

   ::HasFocus := .f.
   ::Message  := ""
   ::Row      := nRow

   ::Style    := "[û ]"

   ::Typeout  := .f.

RETURN Self

METHOD SetFocus() CLASS CHECKBOX

   IF !::HasFocus
      ::lCursor := SetCursor( 0 )
      ::HasFocus := .T.
      ::Display()
      IF ISBLOCK( ::FBlock )
         Eval( ::FBlock )
      ENDIF
   ENDIF

RETURN Self

METHOD _Select( lState ) CLASS CHECKBOX

   LOCAL lStatus := ::Buffer

   IF ISLOGICAL( lState )
      ::Buffer := lState
   ELSE
      ::Buffer := !::Buffer
   ENDIF

   IF lStatus != ::Buffer
      ::Display()
      IF ISBLOCK( ::SBlock )
         Eval( ::SBlock )
      ENDIF
   ENDIF

RETURN Self

METHOD KillFocus() CLASS CHECKBOX

   IF ::HasFocus
      ::HasFocus := .F.

      IF ISBLOCK( ::FBlock )
         Eval( ::FBlock )
      ENDIF

      ::Display()
      SetCursor( ::lCursor )

   ENDIF

RETURN Self

METHOD HitTest( nMouseRow, nMouseCol ) CLASS CHECKBOX

   LOCAL nPosAccel, nLenCaption

   IF nMouseRow == ::Row .AND. ;
      nMouseCol >= ::Col .AND. nMouseCol < ::Col + 3
      RETURN HTCLIENT
   ENDIF

   IF ISCHARACTER( ::Caption )
      nLenCaption := Len( ::Caption )
      IF ( nPosAccel := At( "&", ::Caption ) ) != 0 .AND. ;
         nPosAccel < nLenCaption
         nLenCaption--
      ENDIF

      IF nMouseRow == ::CapRow .AND. ;
         nMouseCol >= ::CapCol .AND. nMouseCol < ::CapCol + nLenCaption
         RETURN HTCAPTION
      ENDIF
   ENDIF

RETURN HTNOWHERE

METHOD Display() CLASS CHECKBOX

   LOCAL cColor := SetColor(), ;
         nCurRow := Row(), nCurCol := Col(), ;
         cOldStyle := ::Style, ;
         cCaption, nPos

   DispBegin()

   IF ::HasFocus
      SET COLOR TO ( __GuiColor( ::ColorSpec, 2 ) )
   ELSE
      SET COLOR TO ( __GuiColor( ::ColorSpec, 1 ) )
   ENDIF

   SetPos(::Row, ::Col + 1)
   IF ::Buffer
      ?? Substr( cOldStyle, 2, 1 )
   ELSE
      ?? Substr( cOldStyle, 3, 1 )
   ENDIF

   SET COLOR TO ( __GuiColor( ::ColorSpec, 3 ) )
   SetPos( ::Row, ::Col )
   ?? Left( cOldStyle, 1 )
   SetPos( ::Row, ::Col + 2 )
   ?? Right( cOldStyle, 1 )

   IF !Empty( cCaption := ::Caption )
      IF ( nPos := At( "&", cCaption ) ) != 0
         IF nPos == Len( cCaption )
            nPos := 0
         ELSE
            cCaption := Stuff( cCaption, nPos, 1, "" )
         ENDIF
      ENDIF

      IF ::HasFocus
         SET COLOR TO ( __GuiColor( ::ColorSpec, 4 ) )
      ENDIF

      SetPos( ::CapRow, ::CapCol )
      ?? cCaption

      IF !::HasFocus .and. nPos != 0
         SET COLOR TO ( __GuiColor( ::ColorSpec, 4 ) )
         SetPos( ::CapRow, ::CapCol + nPos - 1 )
         ?? SubStr( cCaption, nPos, 1 )
      ENDIF

   ENDIF

   DispEnd()

   SET COLOR TO ( cColor )
   SetPos( nCurRow, nCurCol )

RETURN Self


FUNCTION _CHECKBOX_( lState, cCaption, cMessage, cColor, FBlock, SBlock, cStyle )

   LOCAL oCheck

   oCheck := hbCheckBox():New( Row(), Col(), cCaption )

   IF !ISNIL( oCheck )

      oCheck:Select( lState )
      oCheck:Caption := cCaption

      IF cColor != NIL
         oCheck:ColorSpec := cColor
      ENDIF

      oCheck:Message := cMessage

      IF cStyle != NIL
         oCheck:Style := cStyle
      ENDIF

      oCheck:FBlock := FBlock
      oCheck:SBlock := SBlock

   ENDIF

RETURN oCheck

FUNCTION Checkbox( nRow, nCol, cCaption )

   Default cCaption to ''

RETURN hbCheckBox():New( nRow, nCol, cCaption )

FUNCTION __GuiColor( cPair, nPos )
RETURN hb_colorindex( cpair, npos - 1 )

FUNCTION IsDefColor()
RETURN UPPER( SetColor() ) == "W/N,N/W,N/N,N/N,N/W"

#endif
