 /*
  * $Id$
  */

 /*
  * Harbour Project source code:
  * ScrollBar class
  *
  * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
  * www - http://www.harbour-project.org
  *
  * Copyright 2005 Alejandro de Garate <alex_degarate@hotmail.com>
  * METHOD SetStyle( cStyle )  
  * METHOD SetColor( cColor )  
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
 #include "color.ch"
 #include "common.ch"
 #include "button.ch"


#ifdef HB_COMPAT_C53

 // new definitions for better coding. Are screen Codepage dependent, but 
 // can be changed with the setStyle method.
 #define SB_UPARROW    CHR(24)
 #define SB_TRACK      CHR(178)
 #define SB_THUMB      CHR(254)
 #define SB_DNARROW    CHR(25)
 #define SB_LEFTARROW  CHR(27)
 #define SB_RIGHTARROW CHR(26)

 #define SB_VERT_SCROLL   1
 #define SB_HORZ_SCROLL   2

 // converted to macro to speed up things...
 #define __GuiColor( cPair, nPos)  (hb_colorindex( cPair, nPos - 1))

 CLASS HBScrollBar

    DATA BarLength  INIT 1  // 1er error (no tenia INIT 1)
    DATA Cargo
    DATA sBlock
    DATA Style
    DATA ClassName  INIT "HBSCROLLBAR"
    DATA ColorSpec
    DATA aStyle   // Note: new instance var for old Harbour versions to
                  // speed up displaying, <Style> instance var is conserved 
                  // for compatibility purpose. [Alejandro de Garate]

    METHOD Display()
    METHOD HitTest()
    METHOD Update()
    METHOD New( nStart, nEnd, nOffSet, bsBlock, nOrient ) 
    ACCESS Current inline ::GetCurrent()
    ASSIGN Current( nCurrent ) inline ::GetCurrent( nCurrent )
    ACCESS End inline ::GetEnd()
    ASSIGN End ( nEnd ) inline ::GetEnd( nEnd )
    ACCESS OffSet inline ::GetOffSet()
    ASSIGN OffSet( nOffSet ) inline ::GetOffSet( nOffSet )
    ACCESS Orient inline ::GetOrient()
    ASSIGN Orient( nOrient ) inline ::GetOrient( nOrient )
    ACCESS Start inline ::GetStart()
    ASSIGN Start( nStart ) inline ::GetStart( nStart )
    ACCESS ThumbPos inline ::GetThumbPos()
    ASSIGN ThumbPos( nPos ) inline ::GetThumbPos( nPos )
    ACCESS TOTAL inline ::GetTotal()
    ASSIGN TOTAL( nTotal ) inline ::GetTotal( nTotal )

    DATA nCurrent   INIT 0
    DATA nEnd       INIT 0
    DATA nOffSet    INIT 0
    DATA nOrient    INIT 0
    DATA nStart     INIT 0
    DATA nThumbPos  INIT 1
    DATA nTotal     INIT 100
    DATA hb_p_lShow INIT .F.

    METHOD GetCurrent( nCurrent )
    METHOD GetEnd( nEnd )
    METHOD GetStart( nStart )
    METHOD GetThumbPos( nPos )
    METHOD GetTotal( nTotal )
    METHOD GetOffSet( nOffSet )
    METHOD GetOrient( nOrient )
    METHOD SetStyle( cStyle ) 
    METHOD SetColor( cColor ) 

 ENDCLASS


 // NEW METHOD !
 METHOD SetStyle( cStyle )  CLASS HBScrollBar
    IF LEN( cStyle ) == 4
       ::aStyle[ 1] := SUBST( cStyle, 1, 1) 
       ::aStyle[ 2] := SUBST( cStyle, 2, 1) 
       ::aStyle[ 3] := SUBST( cStyle, 3, 1) 
       ::aStyle[ 4] := SUBST( cStyle, 4, 1) 
       ::Style := cStyle
    ENDIF
 RETURN Self

 // NEW METHOD !
 METHOD SetColor( cColor )  CLASS HBScrollBar
   ::ColorSpec := cColor
 RETURN Self


 METHOD New( nStart, nEnd, nOffSet, bsBlock, nOrient ) CLASS HBScrollBar

    LOCAL cStyle, aStyle, cColor := Setcolor()

    IF nOrient == SB_VERT_SCROLL  
       cStyle := "â–‘"          
       aStyle := { SB_UPARROW, SB_TRACK, SB_THUMB, SB_DNARROW }

    ELSEIF nOrient == SB_HORZ_SCROLL
       cStyle := "â–" + Chr(26)  
       aStyle := { SB_LEFTARROW, SB_TRACK, SB_THUMB, SB_RIGHTARROW }
    ENDIF                  

    ::Style  := aStyle[ 1] + aStyle[ 2] + aStyle[ 3] + aStyle[ 4]
    ::aStyle := aStyle

    ::BarLength := nEnd - nStart - 1
    ::Current   := 1
    ::Cargo     := NIL
    ::ColorSpec := __guicolor( cColor, CLR_UNSELECTED + 1 ) + "," + ;
                   __guicolor( cColor, CLR_ENHANCED + 1 )
    ::End       := nEnd
    ::OffSet    := nOffSet
    ::Orient    := nOrient
    ::sBlock    := bsBlock
    ::Start     := nStart
    ::Thumbpos  := 1
    ::Total     := 1

 RETURN Self


 METHOD Display() CLASS HBScrollBar

    LOCAL cCurColor := Setcolor()
    LOCAL nCurRow   := Row()
    LOCAL nCurCol   := Col()
    LOCAL cOffSet, cColor2, cColor1
    LOCAL nStart
    LOCAL nEnd
    LOCAL nPos
    LOCAL lDisplay  := .F.

    IF ThumbPos( Self )
       lDisplay := .T.
       cOffSet  := ::OffSet

       DispBegin()
       cColor1 := __guicolor( ::ColorSpec, 1 )
       cColor2 := __guicolor( ::ColorSpec, 2 )

       IF ::Orient == SB_VERT_SCROLL
          nStart := ::Start
          nEnd   := ::End - 1

          SET COLOR TO (cColor1)
          FOR nPos := nStart + 1 TO nEnd
             DispOutAt( nPos, cOffSet, ::aStyle[ 2 ] )
          NEXT

          SET COLOR TO (cColor2)
          DispOutAt( nStart, cOffSet, ::aStyle[ 1 ] )  
          DispOutAt( nStart + ::ThumbPos, cOffSet, ::aStyle[ 3 ] )
          DispOutAt( nEnd + 1, cOffSet, ::aStyle[ 4 ] )

       ELSE

          nStart := ::Start
          nEnd   := ::End - 1

          DispOutAt( cOffSet, nStart +1, Replic(::aStyle[ 2], nEnd - nStart ), cColor1)

          SET COLOR TO (cColor2)
          DispOutAt( cOffSet, nStart, ::aStyle[ 1 ] )
          DispOutAt( cOffSet, nStart + ::ThumbPos, ::aStyle[ 3 ] )
          DispOutAt( cOffSet, nEnd + 1, ::aStyle[ 4 ] )

       ENDIF

       DispEnd()

       SET COLOR TO (cCurColor)
       Setpos( nCurRow, nCurCol )
    ENDIF

 RETURN lDisplay


 METHOD HitTest( nRow, nCol ) CLASS HBScrollBar

    IF ::Orient == SB_VERT_SCROLL

       DO CASE
          CASE nCol != ::OffSet
          CASE nRow < ::Start
          CASE nRow > ::End
          CASE nRow == ::Start
             RETURN HTSCROLLUNITDEC
          CASE nRow == ::End
             RETURN HTSCROLLUNITINC
          CASE nRow < ::ThumbPos + ::Start
             RETURN HTSCROLLBLOCKDEC
          CASE nRow > ::ThumbPos + ::Start
             RETURN HTSCROLLBLOCKINC
          CASE nRow == ::ThumbPos + ::Start
             RETURN HTSCROLLTHUMBDRAG
       ENDCASE

       IF nCol == ::OffSet + 1 .OR. nCol == ::OffSet

          DO CASE
             CASE nCol != ::OffSet .AND. nCol != ::OffSet + 1
             CASE nRow < ::Start
             CASE nRow > ::End
             CASE nRow == ::Start
                RETURN HTSCROLLUNITDEC
             CASE nRow == ::End
                RETURN HTSCROLLUNITINC
             CASE nRow < ::ThumbPos + ::Start
                RETURN HTSCROLLBLOCKDEC
             CASE nRow > ::ThumbPos + ::Start
                RETURN HTSCROLLBLOCKINC
             CASE nRow == ::ThumbPos + ::Start
                RETURN HTSCROLLTHUMBDRAG
          ENDCASE

       ENDIF

    ELSEIF ::Orient == SB_HORZ_SCROLL

       DO CASE
          CASE nRow != ::OffSet
          CASE nCol < ::Start
          CASE nCol > ::End
          CASE nCol == ::Start
             RETURN HTSCROLLUNITDEC
          CASE nCol == ::End
             RETURN HTSCROLLUNITINC
          CASE nCol < ::ThumbPos + ::Start
             RETURN HTSCROLLBLOCKDEC
          CASE nCol > ::ThumbPos + ::Start
             RETURN HTSCROLLBLOCKINC
          CASE nCol == ::ThumbPos + ::Start
             RETURN HTSCROLLTHUMBDRAG
       ENDCASE

    ENDIF

 RETURN HTNOWHERE


 METHOD Update() CLASS HBScrollBar

    LOCAL nCurRow, nCurCol
    LOCAL lUpdated  := .F.
    LOCAL nThumbPos := ::ThumbPos

    IF !ThumbPos( Self )
    ELSEIF nThumbPos != ::ThumbPos
       lUpdated  := .T.
       nCurRow   := Row()
       nCurCol   := Col()

       DispBegin()

       IF ::Orient == SB_VERT_SCROLL
          DispOutAt( ::Start + nThumbPos, ::OffSet, ::aStyle[ 2 ], __guicolor( ::ColorSpec, 1 ) )
          DispOutAt( ::Start + ::ThumbPos, ::OffSet, ::aStyle[ 3 ], __guicolor( ::ColorSpec, 2 ) )
       ELSE
          DispOutAt( ::OffSet, ::Start + nThumbPos, ::aStyle[ 2 ], __guicolor( ::ColorSpec, 1 ) )
          DispOutAt( ::OffSet, ::Start + ::ThumbPos, ::aStyle[ 3 ], __guicolor( ::ColorSpec, 2 ) )
       ENDIF

       DispEnd()

       SetPos( nCurRow, nCurCol )
    ENDIF

 RETURN lUpdated


 METHOD GetCurrent( nCurrent ) CLASS HBScrollBar

    IF ! IsNumber( nCurrent )
    ELSEIF nCurrent > ::nTotal
    ELSEIF nCurrent != ::nCurrent
       ::nCurrent := nCurrent
    ENDIF

 RETURN ::nCurrent


 METHOD GetEnd( nEnd ) CLASS HBScrollBar

    IF !Isnumber( nEnd )
    ELSEIF nEnd < ::nStart
    ELSEIF nEnd != ::nEnd
       ::nEnd      := nEnd
       ::BarLength := nEnd - ::nStart - 1
    ENDIF

 RETURN ::nEnd


 METHOD GetOffSet( nOffSet ) CLASS HBScrollBar

    IF ! IsNumber( nOffSet )
    ELSEIF nOffSet != ::nOffSet
       ::nOffSet := nOffSet
    ENDIF

 RETURN ::nOffSet


 METHOD GetOrient( nOrient ) CLASS HBScrollBar

    IF ! IsNumber( nOrient )
    ELSEIF nOrient == SB_VERT_SCROLL .OR. nOrient == SB_HORZ_SCROLL
       ::nOrient := nOrient
    ENDIF

 RETURN ::nOrient


 METHOD GetStart( nStart ) CLASS HBScrollBar

    IF ! IsNumber( nStart )
    ELSEIF nStart > ::End
    ELSEIF nStart != ::nStart
       ::nStart    := nStart
       ::BarLength := ::nEnd - nStart - 1
    ENDIF

 RETURN ::nStart


 METHOD GetThumbPos( nPos ) CLASS HBScrollBar

    IF IsNumber( nPos )
       IF nPos < 1
          ::nThumbPos := 1
       ELSEIF nPos >= ::BarLength
          ::nThumbPos := ::BarLength

       ELSEIF nPos >= ::BarLength - 1
          ::nThumbPos := nPos
       ELSE
          ::nThumbPos := nPos
       ENDIF

       IF nPos == 0
          ::hb_p_lShow := .F.
       ELSE
          ::hb_p_lShow := .T.
       ENDIF

    ENDIF

 RETURN ::nThumbPos


 METHOD GetTotal( nTotal ) CLASS HBScrollBar

    IF ! IsNumber( nTotal )
    ELSEIF nTotal < 2
    ELSEIF nTotal != ::nTotal
       ::nTotal := nTotal
    ENDIF

 RETURN ::nTotal


 STATIC FUNCTION ThumbPos( oScroll )

    LOCAL nSize
    LOCAL nCurrent
    LOCAL nBarLength
    LOCAL nTotal

    IF oScroll:barLength < 2
       RETURN .F.
    ENDIF

    IF oScroll:total < 2
       RETURN .F.
    ENDIF

 /*
    IF oScroll:hb_p_lShow
       RETURN .T.
    ENDIF
 */

    nCurrent   := oScroll:Current
    nBarLength := oScroll:BarLength
    nTotal     := oScroll:Total

    // percent relative to total
    nSize := (100 * nCurrent / nTotal)
 
    // percent relative to nBarLength
    nSize := (nBarLength * nSize / 100)

    // remove decimal point
    nSize := IIF( nSize < (nBarLength / 2), ROUND( nSize, 0), INT( nSize) )

    IF nSize <= 1
       if (nCurrent > 1)
           nSize := 2
       else
           nSize := 1
       endif
    ENDIF

    IF nSize >= nBarLength
       if (nCurrent < nTotal)
           nSize := nBarLength - 1
       else
           nSize := nBarLength
       endif
    ENDIF

    if (nCurrent == 1)
        nSize := 1
    elseif (nCurrent == nTotal)
        nSize := nBarLength
    endif

    oScroll:ThumbPos := nSize

 RETURN .T.


 FUNCTION SCROLLBAR( nStart, nEnd, nOffSet, bsBlock, nOrient )

    IF !( IsNumber( nStart ) ) .OR. !( Isnumber( nEnd ) ) .OR.;
       !( IsNumber( nOffSet )) .OR. !( IsNumber( nOrient ))
       RETURN NIL
    ENDIF

    IF nOrient == NIL
       nOrient := SB_VERT_SCROLL
    ENDIF

 RETURN( HBScrollBar():New( nStart, nEnd, nOffSet, bsBlock, nOrient ) )

#endif



