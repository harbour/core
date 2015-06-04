/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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


#include 'CacheRDD.ch'
#include 'CacheMgr.ch'

#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'

#define HB_FUNC_UNUSED( x )  ( ( x ) )


FUNCTION uiDispOut( nRow, nCol, cText, cColor, cPic, oWin )
   LOCAL nR := row()
   LOCAL nC := col()

   DEFAULT cColor TO SetColor()

   HB_FUNC_UNUSED( oWin)

   IF cPic == NIL
      @ nRow, nCol SAY cText COLOR cColor
   ELSE
      @ nRow, nCol SAY cText COLOR cColor PICTURE cPic
   ENDIF

   SetPos( nR, nC )

   RETURN NIL


FUNCTION uiDispBox( nTop, nLeft, nBottom, nRight, cBoxStr, cColor )

   DEFAULT cColor TO SetColor()

   DispBox( nTop, nLeft, nBottom, nRight, cBoxStr, cColor )

   RETURN NIL


FUNCTION VouSaveScrn( nTop, nLeft, nBottom, nRight, lBmp )
   LOCAL a_
   HB_FUNC_UNUSED( lBmp )
   #ifdef __WVT__
   IF ValType( lBmp ) == 'L' .and. lBmp
      a_:= Wvt_SaveScreen( nTop, nLeft, nBottom, nRight )
   ENDIF
   #else
   a_:= NIL
   #endif

   RETURN { nTop, nLeft, nBottom, nRight, SaveScreen( nTop, nLeft, nBottom, nRight ), a_ }


FUNCTION VouRestScrn( aScrn, lDoNotDestroyBMP )

   HB_FUNC_UNUSED( lDoNotDestroyBMP )

   IF ValType( aScrn ) == 'A' .and. ! Empty( aScrn )
      RestScreen( aScrn[ 1 ], aScrn[ 2 ], aScrn[ 3 ], aScrn[ 4 ], aScrn[ 5 ] )

      #ifdef __WVT__
      IF aScrn[ 6 ] <> NIL
         // Must destroy BMP unless instructed
         //
         DEFAULT lDoNotDestroyBMP TO .F.

         Wvt_RestScreen( aScrn[ 1 ], aScrn[ 2 ], aScrn[ 3 ], aScrn[ 4 ], aScrn[ 6 ], lDoNotDestroyBMP )
      ENDIF
      #endif
   ENDIF

   RETURN NIL


FUNCTION VouWinSave( nTp, nLf, nBt, nRt )
   RETURN VouSaveScrn( nTp, nLf, nBt, nRt )


FUNCTION VouWinRest( scr_,lDoNotDestroyBMP )
   RETURN VouRestScrn( scr_,lDoNotDestroyBMP )


FUNCTION ArView( a_,msg_ )
   RETURN VouArView( a_,msg_ )


FUNCTION VouArView( a_, msg_ )
   LOCAL n      := 0
   LOCAL array_
   LOCAL m_     := {}
   LOCAL aScr   := VouSaveScrn( 0,0,MaxRow(),MaxCol() )
   LOCAL ved_   := VouSetVstk()

   SetColor( 'N/W,w+/r' )

   IF ValType( a_ ) <> 'A'
      a_:= { a_ }
   ENDIF

   IF msg_ <> NIL
      IF ValType( msg_ ) <> 'A'
         msg_:= { msg_ }
      ENDIF
      AEval( msg_, {|e| AAdd( m_, e ) })
      AAdd( m_, 'Ele.Typ  Contents' )
   ELSE
      AAdd( m_, 'Ele.Typ  Contents' )
   ENDIF

   IF ! Empty( a_ )
      array_:= bLodVal( a_ )
      n     := 0

      DO WHILE .T.
         CLS
         n := Achoice( 2,5,22,75, array_, , , n+1 )

         IF n == 0  .or. LastKey() == K_CTRL_ENTER
            EXIT
         ENDIF

         IF ValType( a_[ n ] ) == 'A'
            VouArView( a_[ n ] )

         ELSEIF ValType( a_[ n ] ) == 'C'
            // VouGetSome( '', a_[n], , '@ ' )

         ENDIF

         IF n == Len( array_ )
            n := 0
         endi
      ENDDO
   ENDIF

   VouRestScrn( aScr )
   VouSetVstk( ved_ )

   RETURN n


STATIC FUNCTION bLodVal( a_ )
   LOCAL arr_:={}
   LOCAL i, type, s

   FOR i := 1 TO Len( a_ )
      type := ValType( a_[ i ] )
      s    := pad( ltrim( str( i ) ), 4 )

      DO CASE
      CASE type == 'A'
         AAdd( arr_, s+'  A  '+'Array of Length '+str(Len(a_[i]),4))
      CASE type == 'B'
         AAdd( arr_, s+'  B  '+'Code Block')
      CASE type == 'C'
         AAdd( arr_, s+'  C  '+trim(a_[i]))
      CASE type == 'D'
         AAdd( arr_, s+'  D  '+dtoc(a_[i]))
      CASE type == 'L'
         AAdd( arr_, s+'  L  '+if(a_[i],'Yes','No'))
      CASE type == 'M'
         AAdd( arr_, s+'  M  '+'Memo')
      CASE type == 'N'
         AAdd( arr_, s+'  N  '+ltrim(str(a_[i])))
      CASE type == 'O'
         AAdd( arr_, s+'  U  '+'Object')
      CASE type == 'U'
         AAdd( arr_, s+'  U  '+'Undefined')
      CASE type == 'UE'
         AAdd( arr_, s+'  Ue '+'Undefined')
      otherwise
         AAdd( arr_, s+'  UX '+type)

      ENDCASE
   NEXT

   RETURN arr_


FUNCTION VouPopUp( aMsg_, cMsg, tlbr_, aSel_ )
   LOCAL nTop, nLeft, nBottom, nRight, nChoice, scr_, ved_, nPrompts, nLen

   DEFAULT cMsg  TO 'Select an Option'
   DEFAULT aSel_ TO {}

   IF tlbr_ == NIL
      nPrompts := Len( aMsg_ )
      nTop     := max( 1, Int( ( MaxRow() - nPrompts - 4 ) / 2 ) )
      nTop++
      nBottom  := min( MaxRow()-2, nTop + nPrompts + 2 )
      nLen     := 0
      AEval( aMsg_, {|e| nLen := max( Len( e ), nLen ) } )
      nLeft    := max( 1, Int( ( MaxCol() - nLen - 5 ) / 2 ) )
      nRight   := min( MaxCol() - 2, nLeft + nLen + 5 )
      tlbr_:={ nTop,nLeft,nBottom,nRight }
   ENDIF

   asize( aSel_, Len( aMsg_ ) )
   AEval( aSel_, {|e,i| if( e == NIL, aSel_[ i ] := ! Empty( aMsg_[ i ] ), NIL ) } )

   ved_:= VouSetVstk()
   scr_:= VouWinSave( tlbr_[1], tlbr_[2], tlbr_[3]+1, tlbr_[4]+2 )

   setColor( VOU_CLR_POPUP )

   @ tlbr_[1]+1,tlbr_[2] clear to tlbr_[3], tlbr_[4]

   DispOutAt( tlbr_[1], tlbr_[2], padc( cMsg, tlbr_[4]-tlbr_[2]+1 ), 'W+/RB' )
   IF setVisualBrowser()
      hb_Shadow( tlbr_[1], tlbr_[2], tlbr_[3], tlbr_[4] )
   ELSE
      VouShadow( tlbr_[1], tlbr_[2], tlbr_[3], tlbr_[4] )
   ENDIF

   IF ValType( aMsg_ ) == 'C'
      aMsg_:= { aMsg_ }
   ENDIF

   nChoice := aChoice( tlbr_[1]+2, tlbr_[2]+2, tlbr_[3]-1, tlbr_[4]-2, aMsg_, aSel_ )

   VouWinRest( scr_ )
   VouSetVstk( ved_ )

   RETURN nChoice


FUNCTION VouSetVstk( aStack )
   LOCAL lStack := {}

   IF aStack == NIL
      lStack := { SetCursor(), SetColor(), Row(), Col(), Wvt_GetTitle() }
   ELSE
      SetCursor( aStack[ 1 ] )
      SetColor( aStack[ 2 ] )
      SetPos( aStack[ 3 ], aStack[ 4 ] )
      Wvt_SetTitle( aStack[ 5 ] )
   ENDIF

   RETURN lStack


FUNCTION VouShadow( t,l,b,r )
   LOCAL i

   IF !( b+1 <= MaxRow() .and. r+1 <= MaxCol() )
      RETURN NIL
   ENDIF

   @ b+1, l+1 SAY replicate( chr( 223 ), r-l+1 ) COLOR VOU_CLR_MAIN
   @ t, r+1 SAY chr( 220 ) COLOR VOU_CLR_MAIN

   FOR i := t+1 to b
      @ i,r+1 SAY chr( 219 ) COLOR VOU_CLR_MAIN
   NEXT

   RETURN NIL


FUNCTION VouGetSome( msg, vrb, pic, nDec )
   LOCAL nMaxLen, nLenMsg, nLenVrb, scr
   LOCAL GetList := {}
   LOCAL dType   := ValType(vrb)
   LOCAL ved_    := VouSetVstk()
   LOCAL t := MaxRow()-7, l, b := maxrow()-3, r
   LOCAL pnt_:= WvtSetPaint()

   DEFAULT nDec TO 0

   IF SetVisualBrowser()
      WvtSetPaint( {} )
      t -= 2
      b -= 2
   ENDIF

   DEFAULT msg TO 'Please Enter Required Value'
   DEFAULT pic TO if( dType == 'Y', 'Y', '@ ' )

   nMaxLen := MaxCol()-7
   nLenMsg := Len( msg )

   DO CASE
   CASE dType == 'D' ; nLenVrb := 8
   CASE dType == 'N' ; nLenVrb := 12 + nDec + 1
   CASE dType == 'C' ; nLenVrb := Len( vrb )
   CASE dType == 'L' ; nLenVrb := 1
   ENDCASE

   IF nLenMsg + nLenVrb > nMaxLen
      nLenVrb := nMaxLen - nLenMsg - 7
      pic     := substr( pic,1,1 ) + 'S' + NTRIM( nLenVrb ) + substr( pic,2 )
   ENDIF

   pic := if( dType == 'N', '@Z 999999999999' + iif( nDec > 0, replicate( "9", nDec ), "" ), pic )
   pic := substr( pic, 1, 1 ) + 'K' + substr( pic,2 )
   l   := ( ( ( MaxCol()+1 ) - ( nLenMsg + nLenVrb + 7 ) ) / 2 )
   r   := l + nLenMsg + nLenVrb + 6

   scr := VouWinSave( t, l, b+1, r+2 )

   setColor( "W+/RB,GR+/BG,,,W+/BG" )

   #define BOXPX  "лплллмлл"

   @ t,l,b,r BOX BOXPX + " "
   hb_shadow( t,l,b,r )

   @ t+2, l+3 SAY msg GET vrb PICTURE pic
   setCursor( 1 )
   read

   VouWinRest( scr )
   VouSetVstk( ved_ )

   IF SetVisualBrowser()
      WvtSetPaint( pnt_ )
   ENDIF
   RETURN vrb


FUNCTION VouInit()
   LOCAL v_:={}

   AAdd( v_, 'Pritpal Bedi' )
   AAdd( v_, '60, New Professor Colony' )
   AAdd( v_, 'Ludhiana' )

   AAdd( v_, 'Vardhman Knit' )
   AAdd( v_, 'Sunder Nagar' )
   AAdd( v_, 'Ludhiana' )

   RETURN v_


FUNCTION VouXtoS( xVar )
   LOCAL cType := ValType( xVar )

   SWITCH cType
   CASE 'C'
      EXIT
   CASE 'N'
      xVar := str( xVar )
      EXIT
   CASE 'D'
      xVar := dtoc( xVar )
      EXIT
   CASE 'L'
      xVar := if( xVar, 'YES','NO' )
      EXIT
   END

   RETURN xVar


FUNCTION VouGetSomeA( vv_, h_, cTitle, pic_, row, col, bWhen_, bValid_, nMode )
   LOCAL i, scr, nLenPmt, nDiff, mLen, nRgt, nBtm, aStk, tlbr_
   LOCAL nLenVrb, cTyp, nColGet, nMaxGet, nMaxRow, nMaxCol, lWVT
   LOCAL getList := {}
#ifdef __WVT__
   LOCAL tlbr_, pnt_
#endif
   HB_FUNC_UNUSED( nMode )

#ifdef __WVT__
   lWVT := .T.
#else
   lWVT := .F.
#endif

   IF vv_== NIL .or. ValType( vv_ ) <> 'A'
      RETURN {}
   ENDIF

   IF h_== NIL
      h_:= array( Len( vv_ ) )
      AEval( h_,{|e,i| HB_FUNC_UNUSED( e ), h_[ i ] := 'Element '+str( i,2 ) } )
   ENDIF

   nLenPmt  := 0
   AEval( h_, {|x| nLenPmt := max( nLenPmt, Len( x ) ) } )

   IF bWhen_ == NIL
      bWhen_:= afill( array( Len( vv_ ) ), {|| .T. } )
      FOR i := 1 to Len( vv_ )
         IF ValType( vv_[ i ] ) == 'L'
            bWhen_[ i ] := GetHeading( h_, i )
         ENDIF
      NEXT
   ENDIF

   IF bValid_ == NIL
      bValid_:= afill( array( Len( vv_ ) ),{|| .T. } )
   ENDIF

   IF pic_ == NIL
      pic_:= array( Len( vv_ ) )
      FOR i := 1 to Len( vv_ )
         cTyp := ValType( vv_[ i ] )
         pic_[ i ] := if( cTyp == 'C', '@ ', if( cTyp == 'N', '@Z 99999999.999', ;
                                                    if( cTyp == 'L','Y','@ ' ) ) )
      NEXT
   ENDIF

   nLenVrb := 0
   AEval( vv_, {|e| cTyp := ValType( e ), nLenVrb := max( if( cTyp == 'C', Len( e ), ;
                             if( cTyp=='N', 15, if( cTyp=='D',8,3 ) ) ), nLenVrb ) } )

   mLen := nLenPmt + 2 + nLenVrb + 2

   nMaxRow := MaxRow()
   nMaxCol := MaxCol()

   IF row == NIL
      row := int( ( nMaxRow - min( 3+Len( h_ ), nMaxRow-2 ) ) / 2 )
   ENDIF
   IF col == NIL
      col := max( 4, int( ( nMaxCol - min( 2+mLen, nMaxCol-8 ) ) / 2 ) )
   ENDIF

   IF nBtm == NIL
      nBtm := min( row + Len( h_ ) + 3, nMaxRow - 2 )
   ENDIF
   IF nRgt == NIL
      nRgt := col + mLen
      IF nRgt > nMaxCol - 4
         nDiff := nRgt - ( nMaxCol-4 )
         IF col - nDiff < 0
            col  := 4
            nRgt := nMaxCol - 4
         ELSE
            col  := col - nDiff
            nRgt := col + mLen
         ENDIF
      ENDIF
   ENDIF

   IF cTitle == NIL .or. Empty( cTitle )
      cTitle = 'Please Enter Data'
   ENDIF
   cTitle := alltrim( cTitle )
   cTitle := padc( cTitle, nRgt-col+1-4 )

#ifdef __WVT__
   pnt_:= WvtSetPaint( {} )
#endif

   aStk := VouSetVStack()
   SetCursor( 0 )

   scr  := VouSaveScrn( row-1, col-1, nBtm + 1, nRgt + 2, .T. )

#ifndef __WVT__
   SetColor( "W+/BG,GR+/G,,,W+/B" )

   tlbr_:= { row,col,nBtm,nRgt}

   @ tlbr_[1]+1,tlbr_[2] clear to tlbr_[3], tlbr_[4]

   DispOutAt( tlbr_[1], tlbr_[2], padc( cTitle, tlbr_[4]-tlbr_[2]+1 ), "W+/RB" )
   VouShadow( tlbr_[1], tlbr_[2], tlbr_[3], tlbr_[4] )
#else
   SetColor( "w+/b,gr+/g,,,w+/b" )
#endif

   row += 2

   nColGet := col  + 2 + nLenPmt + 1
   nMaxGet := nRgt - 1 - nColGet
   AEval( vv_, {|e,i| if( ValType( e ) == 'C', if( Len( e ) > nMaxGet, pic_[i] := "@KS"+NTRIM( nMaxGet ), NIL ), NIL ) } )

#ifdef __WVT__
   //Wvt_DrawBoxRecessed( row+1, nColGet, row +Len( h_ ), nColGet+nMaxGet-1 )
#endif

   FOR i := 1 to Len( h_ )
      VouDispOut( row+i, col+2, padL( trim( h_[i] ),nLenPmt ), if( lWvt,'w+/b'/*n/w'*/,'n/bg' ) )

      @ row+i, nColGet GET vv_[ i ] PICTURE pic_[ i ] WHEN bbWhen() VALID bbValid() COLOR 'N/W*,B/GR*'
      ATail( getlist ):cargo := { bWhen_[ i ], bValid_[ i ] }
   NEXT

   READ

   VouSetVStack( aStk )

#ifdef __WVT__
   WvtSetPaint( pnt_ )
#endif
   VouRestScrn( scr )

   RETURN vv_


STATIC FUNCTION GetHeading( h_, i )

   RETURN {|| GetActive():VarPut( Alert( h_[ i ], {'Yes','No'} )==1 ),.F. }


FUNCTION VouSetVStack( aStk )

   IF ValType( aStk ) == 'A'
      SetColor( aStk[ 1 ] )
      SetPos( aStk[ 2 ], aStk[ 3 ] )
      SetCursor( aStk[ 4 ] )
   ELSE
      RETURN { SetColor(), Row(), Col(), SetCursor() }
   ENDIF

   RETURN NIL


FUNCTION VouDispOut( nRow, nCol, cText, cColor, cPic, oWin )
   LOCAL nR := row()
   LOCAL nC := col()

   HB_FUNC_UNUSED( oWin )
   DEFAULT cColor TO SetColor()

   IF cPic == NIL
      @ nRow, nCol SAY cText COLOR cColor
   ELSE
      @ nRow, nCol SAY cText COLOR cColor PICTURE cPic
   ENDIF
   SetPos( nR, nC )
   RETURN NIL


FUNCTION bbWhen()
   RETURN Eval( GetActive():cargo[ 1 ] )


FUNCTION bbValid()
   RETURN Eval( GetActive():cargo[ 2 ] )


FUNCTION VouGetString( cHeading, cString )
   LOCAL nKey, n, nn
   LOCAL aScr := VouWinSave( MaxRow(),0,maxrow(),MaxCol() )

   HB_FUNC_UNUSED( cHeading )

   nn := 0
   DO WHILE .T.
      n := ( MaxCol()-Len( cString ) ) / 2
      VouDispOut( MaxRow(),0, Pad( ' ',MaxCol()+1 ), 'N/GR*' )
      VouDispOut( MaxRow(), n, cString, 'N/G*' )

      VouDispout( MaxRow(), n + Len( cString ), " ", iif( ++nn % 2 == 0, 'N/B*', 'N/GR*' ) )
      nKey := inkey( 0.5 )

      DO CASE
      CASE nKey == K_BS
         cString := substr( cString,1,Len(cString)-1 )

      CASE nKey == K_ESC
         cString := ''
         EXIT

      CASE nKey == K_ENTER
         EXIT

      CASE nKey == K_DEL
         cString := ''

      CASE nKey == K_CTRL_V
         cString := hb_gtInfo( HB_GTI_CLIPBOARDDATA )

      OTHERWISE
         IF nKey > 30 .and. nKey < 127
            cString += chr( nKey )
         ENDIF

      ENDCASE
   ENDDO

   VouWinRest( aScr )
   RETURN cString


#include 'wvtwin.ch'

FUNCTION VouModalPopup( aMsg_, cMsg, tlbr_, aSel_ )
   LOCAL oCrt, nChoice, nPrompts, nLen
   LOCAL nTop, nLeft, nBottom, nRight, nWidth, nHeight
   LOCAL pnt_:= WvtSetPaint( {} )

   DEFAULT cMsg  TO 'Select an Option'
   DEFAULT aSel_ TO {}

   IF ValType( aMsg_ ) == 'C'
      aMsg_:= { aMsg_ }
   ENDIF

   nPrompts := Len( aMsg_ )
   nTop     := max( 1, Int( ( MaxRow() - nPrompts - 4 ) / 2 ) )
   nTop++
   nBottom  := min( MaxRow()-2, nTop + nPrompts + 2 )
   nLen     := 0
   AEval( aMsg_, {|e| nLen := max( Len( e ), nLen ) } )
   nLeft    := Int( ( MaxCol() - nLen - 5 ) / 2 )
   nRight   := nLeft + nLen + 5
   nWidth   := nRight-nLeft+1
   nHeight  := nBottom-nTop+1

   tlbr_    := { 1,2,nBottom-nTop+1+1,nRight-nLeft+1+2 }

   asize( aSel_, Len( aMsg_ ) )
   AEval( aSel_, {|e,i| if( e == NIL, aSel_[ i ] := ! Empty( aMsg_[ i ] ), NIL ) } )

   oCrt := WvgCrt():New( , , { nTop-2,nLeft-2 }, { nHeight+1,nWidth+2 }, , .T. )
   //
   oCrt:lModal      := .T.
   oCrt:resizable   := .F.
   oCrt:closable    := .F.
   oCrt:title       := cMsg
   oCrt:Create()

   SetColor( 'N/W' )
   CLS

   SetColor( VOU_CLR_POPUP )
   DispBox( 1, 2, nHeight, nWidth, '         ', 'W+/B' )

   DispOutAt( 1, 2, padc( cMsg, nWidth-1 ), 'W+/RB' )
   VouShadow( 1, 2, nHeight, nWidth-1 )
   //hb_Shadow( 1, 2, nHeight, nWidth )

   nChoice := AChoice( 3, 4, MaxRow()-2, 4+nLen-1, aMsg_, aSel_ )

   oCrt:Destroy()
   WvtSetPaint( pnt_ )

   RETURN nChoice

