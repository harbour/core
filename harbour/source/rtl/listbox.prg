/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Listbox class
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

#include 'hbclass.ch'
#include 'common.ch'
#include "box.ch"
#ifdef HB_COMPAT_C53
Class HBListBox

    Method New( nTop, nLeft, nBottom, nRigth, lDrop )

    MESSAGE Select( nPos ) Method SELECTS( nPos )
    Method AddItem( cText, xValue )
    Method Close()
    Method DelItem( nPos )
    Method Display()
    Method FindText( cText, nPos, lCaseSensitive, lExact )
    Method FindData( cText, nPos, lCaseSensitive, lExact )
    Method GetData( xItem )
    Method GetItem( nPos )
    Method GetText( nPos )
    Method HitTest( n, p )
    Method InsItem( nPos, cText, xVal )
    Method KillFocus()
    Method NextItem()
    Method Open()
    Method PrevItem()
    MESSAGE Scroll( n ) Method _Scroll( n )

    Method SetData( nPos, xValue )
    Method SetFocus()
    Method SetItem( nPos, aitem )
    Method SetText( nPos, xValue )
    DATA ClassName Init "LISTBOX"
    DATA Buffer
    DATA CapCol
    DATA CapRow
    DATA Cargo Init NIL
    DATA HasFocus Init .T.
    DATA ItemCount Init 0
    DATA Left Init 0
    DATA Message Init ''
    DATA TextValue Init ''
    DATA Style Init ""
    DATA sBlock Init NIL
    DAta fBlock Init Nil
    DATA Hotbox Init ""
    Data ColorSpec Init ""
    DATA ColdBox
    Data ISOPEN Init .f.
    Data aItems Init {}
    Data vScrolls

    DATA Value Init 0
    Data Top Init 0
    Data right Init 0
    data Bottom Init 0
    Data TopItem Init 1
    Data dropdown Init .f.
    ACCESS nTop inline ::SetTop()
    ASSIGN nTop( xData ) inline ::SetTop( xData )
    ACCESS vScroll inline ::vScrolls
    ASSIGN vScroll( xData ) inline ::SetScroll( xData )
    ACCESS NRight inline ::SetRight()
    ASSIGN nRight( xData ) inline ::SetRight( xData )
    ACCESS lDropDown inline ::SetDropDown()
    ASSIGN lDropDown( xData ) inline ::SetDropDown( xData )
    ACCESS caption inline ::SetCaption()
    ASSIGN Caption( xData ) inline ::SetCaption( xData )
    ACCESS nBottom inline ::SetBottom()
    ASSIGN nBottom( xData ) inline ::SetBottom( xData )
    ACCESS nTopItem inline ::SetTopItem()
    ASSIGN nTopItem( xTop ) inline ::SetTopItem( xTop )
    ACCESS TypeOut inline ::itemCount == 0
    ASSIGN TypeOut( x ) inline If( x != nil, x, ::itemCount == 0 )

  Hidden:

    Method SetScroll( xData )
    Data xTop Init 0
    Method SetTop( xData )
    Data xRight Init 0
    Method SetRight( xData )
    DATA xDropDown Init .f.
    Method SetDropDown( xData )
    Data cCaption Init ''
    Method SetCaption( xData )
    Data xBottom Init 0
    Method SetBottom( xData )
    Data aScreen Init NIL
    DATA nCursor Init 0
    DATA xtopItem Init 0
    Method SetTopItem( xTop )
Endclass

Method New( nTop, nLeft, nBottom, nRigth, lDrop )

     Local ccolor

     ::ClassName := 'LISTBOX'
     ::Bottom    := nBottom
     ::nBottom   := nBottom
     ::right     := nRigth
     ::nright    := nRigth
     ::Top       := nTop
     ::ntop      := nTop
     ::left      := nleft
     ::Buffer    := Nil
     ::Caption   := ""
     ::CapCol    := nleft
     ::CapRow    := nTop
     ::Cargo     := Nil
     ::ColdBox   := B_SINGLE
     If ( Isdefcolor() )
        ::Colorspec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
     Else
        cColor      := Setcolor()
        ::Colorspec := __guicolor( cColor, 5 ) + "," + ;
                                   __guicolor( cColor, 5 ) + "," + __guicolor( cColor, 5 ) + ;
                                   "," + __guicolor( cColor, 2 ) + "," + __guicolor( cColor, ;
                                   3 ) + "," + __guicolor( cColor, 1 ) + "," + ;
                                   __guicolor( cColor, 4 )
     Endif
     ::aItems    := {}
     ::dropdown  := lDrop
     ::ldropdown := lDrop
     ::fBlock    := Nil
     ::hasfocus  := .F.

     ::hotbox    := B_DOUBLE
     ::itemCount := 0

     ::message := ""

     ::ascreen := Str( nTop + 1, 2 ) + Str( nleft, 2 ) + Str( nBottom, ;
                       2 ) + Str( nRigth, 2 ) + Savescreen( nTop + 1, nleft, nBottom, nRigth )
     ::isopen := !lDrop

     ::sBlock    := Nil
     ::nCursor   := Nil
     ::Style     := chr(31)
     ::TextValue := ""

     ::Topitem  := 0
     ::nTopItem := 0
     ::vScroll  := Nil
     ::Value    := 0

Return Self
/**** Get/Set Datas ****/
Method SetScroll( xData ) Class HBListBox

     If ( ISOBJECT( xData ) ) /*.and. xData:Classname=="SCROLLBAR" .and. xData:orient==1)*/
        ::vScrolls  := xData
        xData:total := ::iTemCount
     Endif
Return ::vScrolls

Method SetTop( xData ) Class HBListBox

     Local nTop

     If ( !( ISNIL( xData ) .and. Isnumber( xData ) ) .and. Isnumber( ( ::xTop := xData ) ) .and. ISOBJECT( ::vScroll ) )
        ::vScroll:start := xData + 1
     Endif
Return ::xTop
Method SetRight( xData ) Class HBListBox

     If ( !( ISNIL( xData ) ) .and. ISOBJECT( ( ::xRight := xData, ::vScroll ) ) )
        ::vScroll:offset := xData
     Endif
Return ::xRight
Method SetDropDown( xData ) Class HBListBox

     If ( !( ISNIL( xData ) ) ) .and. ISLOGICAL( xData )
        ::xDropDown := xData
        If xData
        Elseif ( !::isOpen )
           ::isOpen := .T.
        Endif

     Endif
Return ::xDropDown

Method SetCaption( xData ) Class HBListBox

     If ( Ischaracter( xData ) .and. ISNIL( ::Capcol ) )
        ::cCaption := xData
        ::Caprow   := ::top
        ::Capcol   := ::left - Len( xData )
     Endif
Return ::cCaption

Method SetBottom( xData ) Class HBListBox

     Local nBottom
     If ( !( ISNIL( xData ) .and. Isnumber( xData ) ) .and. Isnumber( ( ::xBottom := xData ) ) .and. ISOBJECT( ( ::vScroll ) ) )
        nBottom       := ::xBottom
        ::vScroll:end := xData - 1
     Endif
Return ::xBottom
/*** Class Methods ***/

Method ADDITEM( cText, xValue ) Class HBListBox

     If ( !( Ischaracter( cText ) ) )
     Elseif ( Valtype( xValue ) $ "CUN" )
        Aadd( ::aItems, { cText, xValue } )
        ::iTemCount ++
        If ( ::iTemCount == 1 .and. ISOBJECT( ( ::Topitem := 1, ::nTopItem := 1, ::vScroll ) ) )
           ::vScroll:total := ( ::iTemCount - ( ::bottom - ;
                                ::top - 2 ) )
        Endif
     Endif
Return Self

Method Close() Class HBListBox

     Local Local1
     Local Local2
     Local Local3
     Local cColor
     Local Local5
     If ( ::isOpen )
        Restscreen( Val( Substr( ::aScreen, 1, 2 ) ), ;
                    Val( Substr( ::aScreen, 3, 2 ) ), ;
                    Val( Substr( ::aScreen, 5, 2 ) ), ;
                    Val( Substr( ::aScreen, 7, 2 ) ), Substr( ::aScreen, ;
                    9 ) )
        ::isOpen  := .F.
        ::aScreen := Nil
     Endif
Return self

Method DELITEM( xitem )

     If ( xitem < 1 )
     Elseif ( xitem <= ::iTemCount )
        Adel( ::aItems[ xitem ] )
        Asize( ::aItems, -- ::iTemCount )
        If ( ::Value > ::iTemCount )
           ::Value := ::iTemCount
           If ( ::Value == 0 )
              ::TextValue := ""
           Else
              ::TextValue := _Getdata( ::aItems[ ::iTemCount ] )
           Endif
           If ( ISNIL( ::Buffer ) )
           Elseif ( Isnumber( ::Buffer ) )
              ::Buffer := ::iTemCount
           Elseif ( ::Value > 0 )
              ::Buffer := ::TextValue
           Endif
        Endif
        If ( ::Topitem > ::iTemCount )
           ::Topitem  := ::iTemCount
           ::nTopitem := ::iTemCount
        Endif
        If ( ISOBJECT( ::vScroll ) )
           ::vScroll:total := ::iTemCount - ( ::Bottom - ;
                                              ::top - 2 )
        Endif
     Endif
Return self

Method Getdata( xData ) Class HBListBox

     Local xRet := Nil
     If ( xData < 1 )
     Elseif ( xData <= ::itemCount )
        xRet := ::aitems[ xData, 2 ]
     Endif
Return xRet

Method FindData( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

     Local nPosFound
     Local lOldExact
     Local nStart
     Local nEnd
     Local nSize
     If ( ISLOGICAL( lExact ) )
        lOldExact := Set( _SET_EXACT, lExact )
     Endif
     nEnd := 1
     If ( Isnumber( nPos ) )
        nEnd ++
     Else
        nPos := 1
     Endif
     nSize := Len( ::aitems ) - nPos + 1
     If ( !( ISLOGICAL( lCaseSensitive ) ) )
        lCaseSensitive := .T.
     Endif
     For nStart := 1 To nEnd
        If ( lCaseSensitive )
           If ( Set( _SET_EXACT ) )
              nPosFound := Ascan( ::aitems, { | _1 | _Getdata( _1 ) == cText ;
                      }, nPos, nSize )
           Else
              nPosFound := Ascan( ::aitems, { | _1 | _Getdata( _1 ) = cText ;
                      }, nPos, nSize )
           Endif
        Elseif ( Set( _SET_EXACT ) )
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _Getdata( _1 ) ) == ;
                   Lower( cText ) }, nPos, nSize )
        Else
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _Getdata( _1 ) ) = ;
                   Lower( cText ) }, nPos, nSize )
        Endif
        If ( nPosFound > 0 )
           Exit
        Endif
        nSize := nPos - 1
        nPos  := 1
     Next
     If ( !( ISNIL( lOldExact ) ) )
        Set Exact ( lOldExact )
     Endif
Return nPosFound

Method FindText( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

     Local nPosFound
     Local lOldExact
     Local nStart
     Local nEnd
     Local nSize
     If ( ISLOGICAL( lExact ) )
        lOldExact := Set( _SET_EXACT, lExact )
     Endif
     nEnd := 1
     If ( Isnumber( nPos ) )
        nEnd ++
     Else
        nPos := 1
     Endif
     nSize := Len( ::aitems ) - nPos + 1
     If ( !( ISLOGICAL( lCaseSensitive ) ) )
        lCaseSensitive := .T.
     Endif
     For nStart := 1 To nEnd
        If ( lCaseSensitive )
           If ( Set( _SET_EXACT ) )
              nPosFound := Ascan( ::aitems, { | _1 | _1[ 1 ] == cText ;
                      }, nPos, nSize )

           Else
              nPosFound := Ascan( ::aitems, { | _1 | _1[ 1 ] == cText ;
                      }, nPos, nSize )
           Endif
        Elseif ( Set( _SET_EXACT ) )
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _1[ 1 ] ) == ;
                   Lower( cText ) }, nPos, nSize )
        Else
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _1[ 1 ] ) = ;
                   Lower( cText ) }, nPos, nSize )
        Endif
        If ( nPosFound > 0 )
           Exit
        Endif
        nSize := nPos - 1
        nPos  := 1
     Next
     If ( !( ISNIL( lOldExact ) ) )
        Set Exact ( lOldExact )
     Endif
Return nPosFound

Method NEXTITEM() Class HBListBox

     Local nCurValue
     Local nValue
     If ( !::hasfocus )
     Elseif ( ::itemCount > 0 )
        If ( ( nCurValue := ::value ) == ::itemCount )
           nValue := nCurValue
        Else
           nValue := nCurValue + 1
        Endif
        changeitem( self, nCurValue, nValue )
     Endif
Return self
Method PREVITEM() Class HBListBox

     Local nCurValue
     Local nValue
     If ( !::hasfocus )
     Elseif ( ::itemCount > 0 )
        If ( ( nCurValue := ::value ) == 0 )
           nValue := 1
        Elseif ( nCurValue == 1 )
           nValue := nCurValue
        Else
           nValue := nCurValue - 1
        Endif
        changeitem( self, nCurValue, nValue )
     Endif
Return self

Method _SCROLL( nMethod ) Class HBListBox

     Local nPos
     Local nTopItem
     Local nCount
     Local nThumbPos
     Local nCurrent
     Local nBarLength
     Local nTotal
     Local nSize
     Local nMouRow
     Local nMouseRow
     Local nKey
     Local nStart
     Do Case
         Case nMethod == - 3074
             If ( ::topitem > 1 )
                ::topitem --
                ::vScroll:current := lbadjustcu( Self )
                Self:display()
             Endif
         Case nMethod == - 3075
             If ( ( ::topitem + ::bottom - ::top ) <= ::itemCount + 1 )
                ::topitem ++
                ::vScroll:current( lbadjustcu( Self ) )
                Self:display()
             Endif
         Case nMethod == - 3077
             nPos     := ::bottom - ::top - 1
             nCount   := ::itemCount
             nTopItem := ::topitem + nPos
             If ( ::topitem < nCount - nPos + 1 )
                If ( nTopItem + nPos - 1 > nCount )
                   nTopItem := nCount - nPos + 1
                Endif
                ::topitem  := nTopItem
                ::ntopitem := nTopItem
                ::vScroll:current( lbadjustcu( Self ) )
                Self:display()
             Endif
         Case nMethod == - 3076
             nPos := ::bottom - ::top - Iif( ::bitmap, 2, ;
                     1 )
             nCount   := ::itemCount
             nTopItem := ::topitem - nPos
             If ( ::topitem > 1 )
                If ( nTopItem < 1 )
                   nTopItem := 1
                Endif
                ::topitem  := nTopItem
                ::ntopitem := nTopItem
                ::vScroll:current( lbadjustcu( Self ) )
                Self:display()
             Endif
         Case nMethod == - 3073
             nMouseRow := Mrow()
             Do While ( ( nKey := Inkey( 0 ) ) != 1003 )
               If ( nKey == 1001 )
                  nMouRow := Mrow()
                  If ( nMouRow <=::vScroll:start() )
                     nMouRow :=::vScroll:start() + 1
                  Endif
                  If ( nMouRow >=::vScroll:end() )
                     nMouRow :=::vScroll:end() - 1
                  Endif
                  If ( nMouRow != nMouseRow )
                     nThumbPos :=::vScroll:thumbpos() + ( nMouRow - ;
                                                   nMouseRow )
                     nBarLength :=::vScroll:barlength()
                     nTotal     :=::vScroll:total()
                     nSize      := ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * ;
                                     nBarLength + 1 - nTotal ) / ( nBarLength - 1 )
                     If ( nSize < 1 )
                        nSize := 1
                     Endif
                     If ( nSize > nTotal )
                        nSize := nTotal
                     Endif
                     nCurrent :=::vScroll:current()
                     If ( nSize - nCurrent > 0 )
                        For nStart := 1 To nSize - nCurrent
                           Self:scroll( - 3075 )
                        Next
                     Else
                        For nStart := 1 To nCurrent - nSize
                           Self:scroll( - 3074 )
                        Next
                     Endif
                     nMouseRow := nMouRow
                  Endif
               Endif
             Enddo
     Endcase
Return Self

Method SELECTS( nPosition ) Class HBListBox

     Local nValue
     Local nPos
     Local xType
     Do Case
         Case ( xType := Valtype( nPosition ) ) == "C"
             nPos := Self:finddata( nPosition )
             If ( !( Valtype( ::buffer ) $ "CU" ) )
                ::buffer := nPos
             Elseif ( ::value == 0 )
                ::buffer := nPosition
             Else
                ::buffer := _Getdata( ::aitems[ nPos ] )
             Endif
         Case !( xType == "N" )
             Return ::value
         Case nPosition < 1
             Return ::value
         Case nPosition > ::itemCount
             Return ::value
         Case nPosition == ::value
             Return ::value
         Otherwise
             nPos := nPosition
             If ( Valtype( ::buffer ) $ "NU" )
                ::buffer := nPos
             Elseif ( nPos == 0 )
                ::buffer := ""
             Else
                ::buffer := _Getdata( ::aitems[ nPos ] )
             Endif
     Endcase
     ::value := nPos
     If ( nPos == 0 )
        ::textvalue := ""
     Else
        ::textvalue := _Getdata( ::aitems[ nPos ] )
     Endif
     If ( Empty( ::hotbox + ::coldbox ) )
        nPos := 0
     Else
        nPos := 2
     Endif
     nValue := ::value - ( ::bottom - ::top - nPos )
     If ( ::topitem <= nValue )
        ::topitem  := nValue
        ::ntopitem := nValue
        If ( ISOBJECT( ::vScroll ) )
           ::vScroll:current := lbadjustcu( Self )
        Endif
     Elseif ( ::value == 0 )
     Elseif ( ::topitem > ::value .and. ISOBJECT( ( ;
                 ::topitem := ::value, ::ntopitem := ::value, ::vScroll ) ) )
        ::vScroll:current := lbadjustcu( Self )
     Endif
     Self:display()
     If ( ISBLOCK( ::sBlock ) )
        Eval( ::sBlock )
     Endif
Return ::value
Method SetTOPITEM( xData ) Class HBListBox

     Local nSize
     Local nPos
     If ( !( ISNIL( xData ) ) ) .and. xData > 0 .and. xData <= ::itemCount

        If ( Empty( ::hotbox + ::coldbox ) )
           nPos := 0
        Else
           nPos := 2
        Endif
        nSize := ::itemCount - ( ::bottom - ::top - ;
                                 nPos )
        If ( xData > nSize )
           xData := nSize
        Endif
        If ( ::topitem != xData )
           ::xtopitem := xData
           If ( ISOBJECT( ::vScroll ) )
              ::vScroll:current := lbadjustcu( Self )
           Endif
           Self:display()
        Endif
     Endif
Return ::xtopitem

Method Display() Class HBListBox

     Local nCurRow       := Row()
     Local nCurCol       := Col()
     Local cCurrentColor := Setcolor()
     Local nStart
     Local nEnd
     Local cColor4
     Local cColor3
     Local nTop          := ::top
     Local nLeft         := ::left
     Local nSize
     Local cHotBox
     Local cCaption
     Local nAmpPos
     Local cColorAny
     nSize := ::right - nLeft + 1
     If ( ::hasfocus )
        cHotBox := ::hotbox
        cColor3 := __guicolor( ::colorspec, 3 )
        cColor4 := __guicolor( ::colorspec, 4 )
        If ( ::isopen )
           cColorAny := __guicolor( ::colorspec, 2 )
        Else
           cColorAny := __guicolor( ::colorspec, 4 )
        Endif
     Else
        cHotBox   := ::coldbox
        cColor3   := __guicolor( ::colorspec, 1 )
        cColor4   := __guicolor( ::colorspec, 2 )
        cColorAny := __guicolor( ::colorspec, 2 )
     Endif

     Dispbegin()
     nEnd := ::topitem + ::bottom - ::top
     If ( ::dropdown )
        Set Color To (cColorAny)
        Setpos( nTop ++, nLeft )
        If ( ::value == 0 )
           ?? Space( nSize - 1 )
        Else
           ?? Padr( ::aitems[ ::value, 1 ], nSize - 1 )
        Endif
        Set Color To (__guicolor(::colorspec, 8))
        ?? Left( ::style, 1 )
        nEnd --
     Endif
     If ( ::isopen )
        If ( !Empty( cHotBox ) )
           Set Color To (__guicolor(::colorspec, 5))
           @ nTop, nLeft clear To ::bottom, ::right
           @ nTop, nLeft, ::bottom, ::right Box cHotBox
           If ( ISOBJECT( ::vScroll ) )
              ::vScroll:display()
           Endif
           nTop ++
           nLeft ++
           nSize -= 2
           nEnd  -= 2
        Endif
        If ( nEnd > ::itemCount )
           nEnd := ::itemCount
        Endif
        For nStart := ::topitem To nEnd
           If ( nStart == ::value )
              Set Color To (cColor4)
           Else
              Set Color To (cColor3)
           Endif
           Setpos( nTop ++, nLeft )
           ?? Padr( ::aitems[ nStart, 1 ], nSize )
        Next
     Endif
     If ( !Empty( cCaption := ::caption ) )
        If ( ( nAmpPos := At( "&", cCaption ) ) == 0 )
        Elseif ( nAmpPos == Len( cCaption ) )
           nAmpPos := 0
        Else
           cCaption := Stuff( cCaption, nAmpPos, 1, "" )
        Endif
        Set Color To (__guicolor(::colorspec, 6))
        Setpos( ::caprow, ::capcol - 1 )
        ?? cCaption
        If ( nAmpPos != 0 )
           Set Color To (__guicolor(::colorspec, 7))
           Setpos( ::caprow, ::capcol + nAmpPos - 2 )
           ?? Substr( cCaption, nAmpPos, 1 )
        Endif
     Endif
     Dispend()

     Set Color To (cCurrentColor)
     Setpos( nCurRow, nCurCol )
Return Self

Method GETITEM( xItem ) Class HBListBox

     Local xRet := Nil
     If ( xItem < 1 )
     Elseif ( xItem <= ::itemCount )
        xRet := ::aitems[ xItem ]
     Endif
Return xRet
Method GETTEXT( xItem ) Class HBListBox

     Local xRet := Nil
     If ( xItem < 1 )
     Elseif ( xItem <= ::itemCount )
        xRet := ::aitems[ xItem, 1 ]
     Endif
Return xRet
Method INSITEM( nPosition, cText, xExp )

     If ( !( Ischaracter( cText ) ) )
     Elseif ( !( Isnumber( nPosition ) ) )
     Elseif ( nPosition < ::itemCount )
        Asize( ::aitems, ++ ::itemCount )
        Ains( ::aitems, nPosition )
        ::aitems[ nPosition ] := { cText, xExp }
        If ( ::itemCount == 1 )
           ::topitem  := 1
           ::ntopitem := 1
        Endif
        If ( ISOBJECT( ::vScroll ) )
           ::vScroll:total := ::itemCount - ( ::bottom - ;
                                              ::top - 2 )
        Endif
     Endif
Return self

Method HITTEST( nMouseRow, nMouseCol ) Class HBListBox

     Local Local1
     Local Local2 := 0
     Local Local3
     Local cColor
     If ( !::isopen )
     Elseif ( !( ISOBJECT( ::vScroll ) ) )
     Elseif ( ( Local2 :=::vScroll:hittest( nMouseRow, nMouseCol ) ) != 0 )
        Return Local2
     Endif
     If ( !::isopen .or. Empty( ::hotbox + ::coldbox ) )
        Local1 := 0
     Else
        cColor := ::top
        If ( ::DropDown )
           cColor ++
        Endif
        Do Case
            Case nMouseRow == cColor
                If ( nMouseCol == ::left )
                   Return - 1
                Elseif ( nMouseCol == ::right )
                   Return - 3
                Elseif ( nMouseCol >= ::left .and. nMouseCol <= ::right )
                   Return - 2
                Endif
            Case nMouseRow == ::bottom
                If ( nMouseCol == ::left )
                   Return - 7
                Elseif ( nMouseCol == ::right )
                   Return - 5
                Elseif ( nMouseCol >= ::left .and. nMouseCol <= ::right )
                   Return - 6
                Endif
            Case nMouseCol == ::left
                If ( nMouseRow >= ::top .and. nMouseRow <= ::bottom )
                   Return - 8
                Else
                   Return 0
                Endif
            Case nMouseCol == ::right
                If ( nMouseRow >= ::top .and. nMouseRow <= ::bottom )
                   Return - 4
                Else
                   Return 0
                Endif
        Endcase
        Local1 := 1
     Endif
     Do Case
         Case !::isopen
         Case nMouseRow < cColor + Local1
         Case nMouseRow > ::bottom - Local1
         Case nMouseCol < ::left + Local1
         Case nMouseCol <= ::right - Local1
             Return ::topitem + nMouseRow - ( cColor + Local1 )
     Endcase
     Do Case
         Case !::dropdown
         Case nMouseRow != ::top
         Case nMouseCol < ::left
         Case nMouseCol < ::right
             Return - 2049
         Case nMouseCol == ::right
             Return - 4097
     Endcase
     Do Case
         Case Empty( ::caption )
         Case nMouseRow != ::caprow
         Case nMouseCol < ::capcol
         Case nMouseCol < ::capcol + __caplengt( ::caption )
             Return - 1025
     Endcase
Return 0
Method KillFocus() Class HBListBox

     Local Local1
     If ( ::hasfocus )
        ::hasfocus := .F.
        If ( ISBLOCK( ::fblock ) )
           Eval( ::fblock )
        Endif

        Dispbegin()
        If ( ::dropdown .and. ::isopen )
           ::close()
        Endif
        ::display()
        Dispend()

        Setcursor( ::nCursor )
     Endif
Return self

Method Open() Class HBListBox

     If ( !::isopen )

        ::ascreen := Str( ::top + 1, 2 ) + ;
                          Str( ::left, 2 ) + Str( ::bottom, 2 ) + ;
                          Str( ::right, 2 ) + Savescreen( ::top + 1, ;
                          ::left, ::bottom, ::right )
        ::isopen := .T.
        Self:display()
     Endif
Return self

Method SetText( nPos, cText ) Class HBListBox

     If ( nPos < 1 )
     Elseif ( nPos <= ::itemCount )
        ::aitems[ nPos, 1 ] := cText
     Endif
Return self

Method SetItem( nPos, cText ) Class HBListBox

     Do Case
         Case nPos < 1
         Case nPos > ::itemCount
         Case Len( cText ) != 2
         Case Ischaracter( cText[ 1 ] )
             ::aitems[ nPos ] := cText
     Endcase
Return self
Method SETFOCUS() Class HBListBox

     If ( !::hasfocus )
        ::nCursor  := Setcursor( 0 )
        ::hasfocus := .T.
        Dispbegin()
        Self:display()
        Dispend()

        If ( ISBLOCK( ::fblock ) )
           Eval( ::fblock )
        Endif
     Endif
Return self

Method SetDAta( nPos, xData ) Class HBListBox

     If ( !( nPos < 1 ) )
     Elseif ( nPos <= ::itemCount )
        ::aitems[ nPos, 2 ] := xData
     Endif
Return Self

Static Function CHANGEITEM( oList, nPos, nItem )

     Local Local1
     Local Local2
     If ( nPos != nItem )
        oList:value := nItem
        If ( oList:value == 0 )
           oList:Textvalue := ""
        Else
           oList:Textvalue := _Getdata( oList:aItems[ oList:value ] )
        Endif
        If ( ISNIL( oList:Buffer ) )
        Elseif ( Isnumber( oList:Buffer ) )
           oList:Buffer := oList:value
        Elseif ( oList:value > 0 )
           oList:Buffer := oList:Textvalue
        Endif
        If ( Empty( oList:hotbox + oList:coldbox ) )
           Local2 := 0
        Else
           Local2 := 2
        Endif
        If ( oList:Dropdown )
           Local2 ++
        Endif
        Local1 := oList:value - ( oList:Bottom - oList:top - Local2 )
        If ( oList:Topitem > oList:value )
           oList:topitem := oList:value
           If ( ISOBJECT( oList:vScroll ) )
              oList:vScroll:current := lbadjustcu( oList )
           Endif

        Elseif ( oList:topitem <= Local1 .and. ISOBJECT( ( oList:topitem := Local1, ;
                    oList:vScroll ) ) )
           oList:vScroll:current := lbadjustcu( oList )
        Endif
        oList:display()
        If ( ISBLOCK( oList:sBlock ) )
           Eval( oList:sBlock )
        Endif
     Endif
Return oList
Static Function LBADJUSTCU( oList )

     Local nSize
     Local nCount
     Local nLength
     Local nTopItem
     Local nNewSize
     nSize    := oList:Bottom - oList:top - Iif( oList:dropdown, 2, 1 )
     nCount   := oList:itemCount
     nLength  := oList:vScroll:barlength
     nTopItem := oList:Topitem
     nNewSize := ( ( nCount - nLength ) * nTopItem + nLength - nSize ) / ( ;
                   nCount - nSize )
Return nNewSize

Function Listbox( nTop, nLeft, nBottom, nRigth, lDrop )

     If !( Isnumber( nTop ) ) .or. !( Isnumber( nleft ) ) .or. !( Isnumber( nBottom ) ) .or. !( Isnumber( nRigth ) )
        Return nil
     Endif
Return HBListBox():New( nTop, nLeft, nBottom, nRigth, lDrop )

Static Function _Getdata( xItem )

     If ( ISNIL( xItem[ 2 ] ) )
        Return xItem[ 1 ]
     Endif
Return xItem[ 2 ]
Function _LISTBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, ;
                         Arg9, Arg10, Arg11, Arg12, Arg13 )

     Local oScroll
     Local nPos
     Local nLen
     Local nCurPos
     Default arg5 To 1
     Default arg12 To .f.
     Default arg13 To .f.
     oScroll := Listbox( Arg1, Arg2, Arg3, Arg4, Arg12 )
     If ( !( ISNIL( oScroll ) ) )
        If ( Ischaracter( Arg7 ) )
           oScroll:caption := Arg7
           oScroll:capcol  := Arg2 - __caplengt( Arg7 )
        Endif
        If arg9 != nil
           oScroll:colorspec := Arg9
        Endif
        oScroll:message := Arg8
        oScroll:fblock  := Arg10
        oScroll:sblock  := Arg11
        oScroll:isopen  := arg13
        nLen            := Len( Arg6 )
        For nPos := 1 To nLen
           nCurPos := Arg6[ nPos ]
           If ( !( Isarray( nCurPos ) ) )
              oScroll:additem( nCurPos )
           Elseif ( Len( nCurPos ) == 1 )
              oScroll:additem( nCurPos[ 1 ] )
           Else
              oScroll:additem( nCurPos[ 1 ], nCurPos[ 2 ] )
           Endif
        Next
        If ( !( ISNIL( Arg13 ) ) .and. Arg13 )
           If ( !( ISLOGICAL( Arg12 ) ) )
           Elseif ( Arg12 )
              Arg1 ++
           Endif
           oScroll:vscroll := Scrollbar( Arg1 + 1, Arg3 - 1, Arg4,, 1 )
        Endif

        oScroll:select( Arg5 )
     Endif
Return oScroll
Function __CAPLENGT( Arg1 )

     Local Local1 := Len( Arg1 )
     Local Local2
     If ( ( Local2 := At( "&", Arg1 ) ) == 0 )
     Elseif ( Local2 < Local1 )
        Local1 --
     Endif
Return Local1
#endif

*+ EOF: LISTBOX.PRG
