/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CUI Forms Editor
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
 *
 * This program is free software; you can redistribute it AND/OR modify
 * it under the terms of the GNU General PUBLIC License as published by
 * the Free Software Foundation; either version 2, OR (at your option)
 * any later version.
 *
 * This program is distributed IN the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General PUBLIC License FOR more details.
 *
 * You should have received a copy of the GNU General PUBLIC License
 * along WITH this software; see the file COPYING.  IF NOT, write TO
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (OR visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission FOR
 * additional uses of the text contained IN its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries WITH other
 * files TO produce an executable, this does NOT by itself cause the
 * resulting executable TO be covered by the GNU General PUBLIC License.
 * Your use of that executable is IN no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does NOT however invalidate any other reasons why
 * the executable file might be covered by the GNU General PUBLIC License.
 *
 * This exception applies only TO the code released by the Harbour
 * Project under the name Harbour.  IF you copy code FROM other
 * Harbour Project OR Free Software Foundation releases into a copy of
 * Harbour, as the General PUBLIC License permits, the exception does
 * NOT apply TO the code that you add IN this way.  TO avoid misleading
 * anyone as TO the status of such modified files, you must delete
 * this exception notice FROM them.
 *
 * IF you write modifications of your own FOR Harbour, it is your choice
 * whether TO permit this exception TO apply TO your modifications.
 * IF you DO NOT wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                       Harbour CUI Editor Source
 *
 *                             Pritpal Bedi
 *                               13Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbcuied.ch"
#include "hbgtinfo.ch"
#include "common.ch"
#include "inkey.ch"
#include "achoice.ch"
#include "box.ch"

/*----------------------------------------------------------------------*/

#define CGO_POS                                   1
#define CGO_ROW                                   2
#define CGO_LSEL                                  3
#define CGO_LNUM                                  4
#define CGO_CH_                                   5
#define CGO_SEL_                                  6
#define CGO_EXE_                                  7
#define CGO_SCROL                                 8
#define CGO_LENSCR                                9


#define LEN_COL_STR                               20
#define LEN_VID_STK_ENTRY                         LEN_COL_STR + 3

/*----------------------------------------------------------------------*/

THREAD STATIC s_vid_stk := ""

//----------------------------------------------------------------------//

FUNCTION VouchInRange( v, r1, r2 )
   RETURN ( v >= r1 .AND. v <= r2 )

/*----------------------------------------------------------------------*/

FUNCTION pad_max( a_,lNum,max )
   LOCAL i := 1
   DEFAULT lNum TO .f.
   IF max == NIL
      max := 0
      aeval( a_, {|x| max := max( max,len( x ) )} )
   ENDIF
   aeval( a_, {|x| a_[ i ] := iif( lNum, str( i,3 ) + '  ', '' ) + pad( x,max ), i++ } )
   RETURN a_

/*----------------------------------------------------------------------*/

FUNCTION VouchInArray( v,a_ )
   RETURN( ascan( a_,{|e| e = v } ) > 0 )

//----------------------------------------------------------------------//

FUNCTION VouchAShrink( a_,n )
   IF n > 0
      adel( a_,n )
      asize( a_,len( a_ )-1 )
   ENDIF
   RETURN a_

//----------------------------------------------------------------------//

FUNCTION uiDebug( ... )
   LOCAL a_:= hb_aParams()
   LOCAL s := "", x

   FOR EACH x IN a_
      s += xtos( x ) + "   "
   NEXT

   //WAPI_OutputDebugString( s )

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION setGetAch( v_ )
   LOCAL lCrt
   THREAD STATIC sCrt := {}
   lCrt := sCrt
   IF hb_isArray( v_ )
      sCrt := v_
   ENDIF
   RETURN lCrt

/*----------------------------------------------------------------------*/

FUNCTION VouchWndSave( t, l, b, r )
   LOCAL wnd_,crs

   crs := mSetCursor( .f. )
   DEFAULT t TO 0, ;
           l TO 0, ;
           b TO maxrow(), ;
           r TO maxcol()

   wnd_:= { t, l, b, r, saveScreen( t,l,b,r ) }

   mSetCursor( crs )

   RETURN wnd_

//----------------------------------------------------------------------//

FUNCTION VouchWndRest( wnd_ )
   LOCAL crs, bError

   bError := errorblock( {|oErr| Break( oErr ) } )
   BEGIN SEQUENCE
      crs := mSetCursor( .f. )
      RestScreen( wnd_[1], wnd_[2], wnd_[3], wnd_[4], wnd_[5] )
      mSetCursor( crs )
   END
   errorblock( bError )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION Wvt()
   RETURN .F.

/*----------------------------------------------------------------------*/

FUNCTION VouchGetArray( h_,vv_, sel_, nTop, nLft, nBtm, nRgt, title, bWhen_, bValid_, pic_, hlp, ord_ )
   LOCAL i, scr , nSel, maxL, mLen, nDiff, clr
   LOCAL nLenVrb, clr1, s, cTyp
   LOCAL nLastKey,cgo_, aScrol_,nLenMnu,pmt_:={}

   HB_SYMBOL_UNUSED( hlp )
   HB_SYMBOL_UNUSED( ord_ )

   IF h_== NIL .OR. valtype(h_)<>'A' .OR. vv_== NIL .OR. valtype(vv_)<>'A'
      RETURN {vv_, 0}
   ENDIF
   IF bWhen_ == NIL
      bWhen_:= afill(array(len(vv_)), {|| .t. })
      FOR i := 1 TO len(vv_)
         s := h_[i]
         IF valtype(vv_[i]) == 'L'
            bWhen_[i] := {|| VouchYN(s,oGet()),.f. }
         ENDIF
      NEXT
   ENDIF

   IF bValid_ == NIL
      bValid_:= afill(array(len(vv_)),{|| .t. })
   ENDIF

   IF pic_ == NIL
      pic_:= array(len(vv_))
      FOR i := 1 TO len(vv_)
         cTyp := valtype(vv_[i])
         pic_[i] := iif(cTyp=="C","@ ",iif(cTyp=="N","@Z 99999999.999",iif(cTyp=="L","Y","@ ")))
      NEXT
   ENDIF

   nLenVrb := 0
   aeval(vv_, {|e| cTyp := valtype(e), nLenVrb := max( ;
                  iif(cTyp == 'C', len( e ), ;
                         iif(cTyp=='N', 15, iif( cTyp=='D',8,3))), nLenVrb ) })
   pmt_:={}
   aeval(h_,{|e,i| aadd( pmt_, e + " {"+xtos(vv_[i])+ "}" ) })

   //  decide maximum length of the largest prompt
   mLen := 0
   aeval( pmt_, {|x| mLen := max(mLen, len(x)) } )
   mLen := max( len(h_[1])+2+nLenVrb, mLen)+2

   IF nTop == NIL
      nTop := int( ( maxrow() - min( iif( wvt(),2,3 ) + len( h_ ), maxrow()-3 ) ) / 2 )
   ENDIF
   IF nBtm == NIL
      nBtm := min( nTop + len( h_ ) + iif( wvt(),2,3 ), maxrow()-3 )
   ENDIF

   IF nLft == NIL
      nLft := max( 4,int( ( maxcol() - min( 2+mLen, maxcol()-8 ) ) / 2 ) )
   ENDIF
   IF nRgt == NIL
      nRgt := nLft + mLen
      IF nRgt > maxcol() - 4
         nDiff := nRgt - (maxcol()-4)
         IF nLft - nDiff < 0
            nLft := 4
            nRgt := maxcol()-4
         ELSE
            nLft  := nLft - nDiff
            nRgt := nLft + mLen
         ENDIF
      ENDIF
   ENDIF

   IF title == NIL .OR. empty( title )
      title = "Untitled"
   ELSE
      title := alltrim( title )
   ENDIF
   title := padc( title, nRgt - nLft )
   title := { title, replicate(chr(196), len(title)+2) }
   maxL  := len( h_[ 1 ] )
   sel_  := iif( sel_ == NIL,.t., sel_ )

   vstk_push()
   setcursor(0)

   scr := VouchWndSave( max( 0, nTop -1 ), max( 0, nLft-1 ), nBtm + 1, nRgt + 2, .f. )

   B_MSG title AT nTop, nLft TO nBtm, nRgt SHADOW
   clr := "W+/BG"
   setcolor( clr + "," + "+GR/B" + ",,," + "N" + substr( clr, at( "/", clr ) ) )
   aScrol_ := ScrolBarNew( nTop + 2, nRgt, nBtm, "gr+/b" )

   nLenMnu := len( pmt_ )
   clr1    := setColor()

   ScrolBarDisplay( aScrol_ )
   ScrolBarUpdate( aScrol_, 1, nLenMnu, .t. )

   cgo_:= { 1, 0, .f., .f., pmt_, sel_,/*exe_*/, aScrol_, nLenMnu }

   SetGetAch( vv_ )           //  Put on stack FOR aChPut(), aChGet()

   DO WHILE .t.
      setColor( clr1 )

      pmt_:= {}
      aeval(h_, {|e,i| aadd(pmt_, e+" {"+xtos(vv_[i])+"}") })
      cgo_[ CGO_CH_ ] := pmt_

      clear typeahead
      nSel := VouchAChoice(nTop+iif(wvt(),2,3),nLft+1+iif(wvt(),1,0),nBtm-1,nRgt-1-iif(wvt(),1,0), ;
                           cgo_[CGO_CH_], cgo_[CGO_SEL_], "VouchFunc1", ;
                           cgo_[CGO_POS], cgo_[CGO_ROW],/* oWin */, @nLastKey, cgo_ )

      IF  nLastKey == K_ENTER
         vv_[ nSel ]  := VouchGetChoice(vv_[ nSel ], nTop + cgo_[ CGO_ROW ]+iif(wvt(),2,3), ;
                           nLft + maxL + 1, nRgt-iif(wvt(),2,1), bWhen_[ nSel ], ;
                           bValid_[ nSel ], pic_[ nSel ])

      ELSEIF nLastKey == K_F10
         EXIT
      ELSEIF nLastKey == K_ESC
         EXIT
      ELSEIF nLastKey == K_CTRL_ENTER
         EXIT
      ELSEIF nLastKey == K_CTRL_END
         EXIT
      ENDIF
   ENDDO

   vstk_pop()
   VouchWndRest(scr)

   RETURN{ vv_, nSel }

/*----------------------------------------------------------------------*/

FUNCTION VouchFunc1( mode, nElem, nRow, nKey, cgo_ )
   LOCAL ret := AC_CONT

   IF nKey <> 0
      ScrolBarUpdate( cgo_[CGO_SCROL], nElem, cgo_[CGO_LENSCR], .t. )
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRow

   DO CASE
   CASE mode == AC_IDLE
   CASE mode == AC_HITTOP
      //NannyBoo
   CASE mode == AC_HITBOTTOM
      //Charge
   CASE mode == AC_NOITEM
      ret := AC_ABORT
   OTHERWISE
      DO CASE
      CASE nKey == K_CTRL_END
         ret := AC_SELECT
      CASE nKey == K_ENTER
         ret := AC_SELECT
      CASE nKey == K_CTRL_ENTER
         ret := AC_SELECT
      CASE nKey == K_F10
         ret := AC_SELECT
      CASE nKey == K_ESC
         ret := AC_ABORT
      CASE nKey > 31 .AND. nKey < 123
         cgo_[CGO_POS] := scan_ff( cgo_[CGO_POS], cgo_[CGO_CH_], chr( nKey ), 3 )
         RETURN AC_ABORT
      ENDCASE
   ENDCASE

   RETURN ret

//----------------------------------------------------------------------//

STATIC FUNCTION scan_ff( elem, a_, c /*, nFrom */ )
   LOCAL na, nlen

   c := lower( substr( c,1,1 ) )
   nLen := len( c )
   IF( na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nLen ) ) == c }, min( elem+1, len( a_ ) ) ) ) == 0
      na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nlen ) ) == c },1,elem-1 )
   ENDIF

   RETURN iif( na == 0, elem, na )

//----------------------------------------------------------------------//

STATIC FUNCTION VouchGetChoice( vrb, row, col, e_col, whn, vld, pic )
   LOCAL scr, maxL, n_vrb, dec, r, c, r1, c1, crs, clr
   LOCAL type := valtype( vrb )
   LOCAL getlist := {}

   IF type == "N"
      n_vrb := str( vrb )
      dec   := at( ".", n_vrb )
      IF pic == NIL
         IF dec > 0
            pic := replicate( "9", maxL -( maxL - dec ) - 1 ) + "." + replicate( "9", maxL - dec )
         ELSE
            pic := replicate( "9", maxL )
         ENDIF
      ENDIF
   ELSEIF type == "D"
      pic := ""
   ELSEIF type == "L"
      pic := "Y"
   ELSEIF type == "C"
      maxL := len( vrb )
      pic  := "@K"
      IF( maxL + col ) > e_col
         pic  := pic + "S" + ltrim( str( maxL ) )
      ENDIF
   ENDIF

   r  := row
   c  := col
   r1 := r
   c1 := e_col

   clr := SetColor()
   scr := VouchWndSave( r-1, c-1, r1, c1 )

   @ r, c clear TO r1, c1

   crs := setcursor( 1 )
   @ r, c+1 get vrb when whn() valid vld() picture pic
   atail( getlist ):cargo := { whn,vld }
   read

   setcursor( crs )
   SetColor( clr )
   VouchWndRest( scr )

   RETURN vrb

//----------------------------------------------------------------------//

STATIC FUNCTION ScrolBarUpdate()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ScrolBarDisplay()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ScrolBarNew()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION whn()
   RETURN eval( getActive():cargo[1] )

//----------------------------------------------------------------------//

STATIC FUNCTION vld()
   RETURN eval( getActive():cargo[2] )

//----------------------------------------------------------------------//

FUNCTION oAchGet( n )
   RETURN setGetAch()[n]

//----------------------------------------------------------------------//

STATIC FUNCTION oAchPut( n,v )
   setGetAch()[n] := v
   RETURN .t.

//----------------------------------------------------------------------//

FUNCTION oCPut( v )
   getactive():varPut( v )
   RETURN .t.

//----------------------------------------------------------------------//

STATIC FUNCTION oGet()
   RETURN getActive():varGet()

//----------------------------------------------------------------------//

FUNCTION GetCrtCargoSlots()
   RETURN { .f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f. }

/*----------------------------------------------------------------------*/

FUNCTION xtos( x )
   LOCAL type := valtype( x )
   DO CASE
   CASE type == 'C'
      RETURN alltrim( x )
   CASE type == 'D'
      RETURN dtoc( x )
   CASE type == 'L'
      RETURN iif( x, 'Y', 'N' )
   CASE type == 'N'
      RETURN ltrim( str( x ) )
   ENDCASE
   RETURN ""

//----------------------------------------------------------------------//

FUNCTION VouchRgb( nR, nG, nB )
   RETURN ( nR +( nG * 256 ) +( nB * 256 * 256 ) )

//---------------------------------------------------------------------//

FUNCTION VouchYN( msg, nInit )
   LOCAL g := getactive(), sel

   msg  := iif( msg==NIL,'',msg )
   nInit := iif( nInit==NIL,1,iif( valtype( nInit )=='N',nInit,iif( nInit,1,2 ) ) )

   B_MSG msg CHOOSE 'Yes','No ' TRIGGER {1,1} INITIAL nInit ;
   RESTORE SHADOW AT g:row - 3, g:col INTO sel

   IF g <> NIL
      g:varPut( iif( sel == 1, .t., .f. ) )
   ENDIF

   RETURN sel == 1

//----------------------------------------------------------------------//

FUNCTION VouchMenuMM( mnu_,nInit,msg,lExact,aSel )
   LOCAL n, i, t, m_:={}

   DEFAULT nInit  TO getActive():varGet()
   DEFAULT msg    TO 'Select an Option'
   DEFAULT lExact TO .f.
   DEFAULT aSel   TO {}

   aSel := asize( aSel, len( mnu_ ) )
   FOR i := 1 TO len( mnu_ )
      DEFAULT aSel[ i ] TO .t.
   NEXT

   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )

   IF( t := valtype( nInit ) == 'C' )
      //nInit := iif( lExact, nInit, trim( nInit ) )
   ENDIF

   n := max( 1, ascan( mnu_, {|e_| ;
      iif( t, iif( lExact, nInit, trim( nInit ) ) $ e_[ 2 ], nInit = e_[ 2 ] )  } ) )

   B_MSG msg CHOOSE m_ INITIAL n SELECTABLES aSel RESTORE SHADOW AT row()-3,col() WVT .T. INTO n
   n := max( 1,n )

   getActive():varPut( iif( t,pad( mnu_[n,2],len( nInit ) ),mnu_[n,2] ) )

   RETURN .f.

//----------------------------------------------------------------------//

FUNCTION VouchMenuM( id,nInit,msg )
   LOCAL n, m_:={},t, mnu_

   DEFAULT msg   TO 'Select'
   DEFAULT nInit TO getActive():varGet()

   mnu_:={}
   DO CASE
   CASE id == "MN_TYFLD"
      aadd( mnu_, { "Character", "C" } )
      aadd( mnu_, { "Numeric"  , "N" } )
      aadd( mnu_, { "Date"     , "D" } )
      aadd( mnu_, { "Logical"  , "L" } )

   CASE id == "MN_BOX"
      aadd( mnu_, { "B_SINGLE"        , B_SINGLE        } )
      aadd( mnu_, { "B_DOUBLE"        , B_DOUBLE        } )
      aadd( mnu_, { "B_SINGLE_DOUBLE" , B_SINGLE_DOUBLE } )
      aadd( mnu_, { "B_DOUBLE_SINGLE" , B_DOUBLE_SINGLE } ) 
                                         
   CASE id == "MN_FILL"
      aadd( mnu_, { "Clear" , "CLEAR"  } )
      aadd( mnu_, { "Filled", "FILLED" } )
      
   ENDCASE

   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )
   t := valtype( nInit ) == 'C'
   n := max( 1, ascan( mnu_, {|e_| iif( t, trim( nInit ) $ e_[ 2 ], nInit == e_[ 2 ] ) } ) )

   B_MSG msg CHOOSE m_ INITIAL n INTO n RESTORE SHADOW AT row()-3,col() WVT .T.
   n := max( 1,n )

   getActive():varPut( mnu_[n,2] )

   RETURN .f.   //  Note, because the FUNCTION is used IN when clause

//----------------------------------------------------------------------//

STATIC FUNCTION vstk_push()
   s_vid_stk := chr( set( _SET_CURSOR ) ) + ;
                        chr( row() ) + chr( col() ) + ;
                        pad( setcolor(), LEN_COL_STR ) + ;
                        s_vid_stk
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION vstk_pop()
   IF len( s_vid_stk ) > 0
      setcursor( asc( substr( s_vid_stk, 1, 1 ) ) )
      @ asc( substr( s_vid_stk, 2, 1 ) ), asc( substr( s_vid_stk, 3, 1 ) ) SAY ""
      setcolor( substr( s_vid_stk, 4, LEN_COL_STR ) )
      s_vid_stk := substr( s_vid_stk, LEN_VID_STK_ENTRY + 1 )
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION VouchMsgBox(r1, c1, r2, c2, width, depth, msg_, msgClr, ;
      ch_, chClr, wait, restore, paste, shadow, trg_, sel, lSelect_, abr, ;
      lSlctns, lLeftRight, center, tagged_,lNumeric,help,exe_,num_,;
      lNoXpp, oWin, cIcon, lWvt, nAlign )

   LOCAL msgLen := 0, chLen := 0, maxLen, pmtWidth, xRet:= NIL
   LOCAL boxWide, boxDeep, oldCur, oldClr, oldScr, oldR, oldC, tBoxDeep
   LOCAL i, oGet, oVal, gap, mCrs,n,nLastKey, cr1
   LOCAL nLenScrol, nMsg, clr, aScrolbar
   LOCAL nSlctns_:={}, dd_:={}, cgo_:={}

   HB_SYMBOL_UNUSED( trg_  )
   HB_SYMBOL_UNUSED( help  )
   HB_SYMBOL_UNUSED( cIcon )
   HB_SYMBOL_UNUSED( lWvt  )

   DEFAULT lNoXpp     TO .f.
   DEFAULT nAlign     TO 2    // only FOR wvt . center

   DEFAULT ch_        TO {}
   DEFAULT restore    TO .f.

   DEFAULT r1         TO row()
   DEFAULT c1         TO col()
   DEFAULT msg_       TO {}
   DEFAULT ch_        TO {}
   DEFAULT lSelect_   TO {}
   DEFAULT msgClr     TO 1 //"W+/BG"
   DEFAULT chClr      TO 3 //"+W/B"
   DEFAULT restore    TO .f.
   DEFAULT paste      TO .f.
   DEFAULT shadow     TO .f.
   DEFAULT abr        TO .f.
   DEFAULT lSlctns    TO .f.
   DEFAULT lLeftRight TO .f.
   DEFAULT center     TO .f.
   DEFAULT tagged_    TO {}
   DEFAULT lNumeric   TO .f.
   DEFAULT num_       TO {}

   oGet := iif( paste, getactive(), oGet )

   IF ( len( msg_) > 0) .AND. (valtype (msg_[1]) == "A" )
      msg_ := aclone( msg_[ 1 ] )
   ENDIF
   IF ( len( msg_ ) > 0 ) .AND. ( msg_[1] == NIL )
      msg_:= {}
   ENDIF
   IF ( len( ch_ ) > 0 ) .AND. ( valtype( ch_[ 1 ] ) = "A" )
      ch_:= aclone( ch_[ 1 ] )
   ENDIF
   IF len( msg_ ) == 0 .AND. len( ch_ ) == 0
      RETURN .f.
   ENDIF

   IF lSlctns
      IF lNumeric
         IF empty(num_)
            FOR i := 1 TO len (ch_)
               ch_[i] := '    '+ch_[i]
            NEXT
         ELSE
            FOR i := 1 TO len (ch_)
               IF (n := ascan(num_,i))==0
                  ch_[ i ] := '    '+ch_[i]
               ELSE
                  ch_[ i ] := pad( hb_ntos( n ), 4 ) + ch_[ i ]
               ENDIF
            NEXT
         ENDIF
      ELSE
         FOR i := 1 TO len( ch_ )
            ch_[ i ] := iif( empty( tagged_ ),'  ', iif( tagged_[ i ], CHECKMARK + ' ', '  ' ) ) + ch_[ i ]
         NEXT
      ENDIF
   ENDIF

   aeval( msg_, {|s| msgLen := max( msgLen, len( s )) })
   aeval( ch_,  {|s| chLen  := max( chLen,  len( s )) })
   maxlen := max( msgLen, chLen )
   aeval( ch_, {|s,i| s:=s, ch_[i] := pad( ch_[i], maxLen ) } )

   IF empty( lSelect_ )
      lSelect_:= {}
      aeval( ch_,  {|s| aadd(lSelect_, iif(empty(s), .f., .t.)) })
   ELSE
      aeval( ch_, {|s,i| lSelect_[i] := iif(empty(s),.f.,lSelect_[i]) })
   ENDIF
   IF ascan( lSelect_, {|e| e } ) == 0
      IF len(ch_) > 0
         RETURN 0
      ENDIF
   ENDIF

   nMsg := Len( msg_ )
//   nOff := iif( nMsg == 1, 0,  1 )

   boxDeep  := iif( len( msg_ )=0,0,len( msg_ )+1 ) + iif( len( ch_  )=0,0,len( ch_  )+1 )
   tBoxDeep := boxDeep
   boxDeep  := min( boxDeep, maxrow() - r1 )
   boxWide  := max( msgLen, chLen ) + 3

   DEFAULT r2 TO r1 + iif( depth = NIL, boxDeep, depth )
   DEFAULT c2 TO c1 + iif( width = NIL, boxWide, width )

   IF center
      r1 := int( ( maxrow() - tBoxDeep ) / 2 )
      r1 := iif( r1 <= 0,1,r1 )
      r2 := r1 + tBoxDeep
      IF r2 > maxrow() - 1
         r2 := maxrow() - 1
      ENDIF

      c1 := int( ( maxcol() - boxWide ) / 2 )
      c1 := iif( c1 < 0, 3, c1 )
      c2 := c1 + boxWide
      IF c2 > maxcol()-3
         c2 := maxcol()-3
      ENDIF
   ELSE
      IF r2 <= r1
         r2 := maxrow()
      ENDIF
      IF c2 <= c1
         c2 := maxcol()
      ENDIF
      IF r2 > maxrow()
         gap := (r2 - maxrow () )
         r2 := r2 - gap
         r1 := r1 - gap
      ENDIF
      IF c2 > maxcol() - 4
         gap := ( c2 - maxcol() + 4 )
         c2  := c2 - gap
         c1  := c1 - gap
      ENDIF
   ENDIF

   IF shadow
      IF r2 == maxrow()
         r2 := r2 - 1
      ENDIF
      IF c2 == maxcol ()
         c2 := c2 - 2
         c1 := c1 - 2
      ENDIF
   ENDIF

   IF restore
      IF shadow
         oldScr := VouchWndSave( max( 0,r1-2 ), max( 0,c1-2 ), r2 + 1 , c2 + 3 )
      ELSE
         oldScr := VouchWndSave( r1, c1, r2, c2 )
      ENDIF
   ENDIF

   IF sel == NIL .OR. sel < 1 .OR. sel > len( ch_ )
      sel := 1
   ENDIF

   vstk_push()

   oldR   := row()
   oldC   := col()
   oldCur := setcursor( 0 )
   oldClr := SetColor( "W+/BG" )
   mCrs   := mSetCursor( .f. )

   dispbox( r1, c1, r2, c2, "лплллмлл " )
   IF shadow
      VouchShadow( r1, c1, r2, c2 )
   ENDIF

   FOR i = 1 TO min( len( msg_ ), r2 - ( r1 + 1 ) )
      devpos( r1 + i, c1 + 2 )
      devout( pad( msg_[ i ], c2 - ( c1 + 3 ) ) )
   NEXT i
   mSetCursor( mCrs )

   clr := "W+/BG"
   IF len( ch_ ) > 0
      IF nMsg > 0
         mCrs := mSetCursor(.f.)
         devpos( r1 + 1 + nMsg, c1 + 1 )
         devout( replicate( chr( 196 ), c2 - ( c1 + 1 ) ) )
         mSetCursor( mCrs )
         cr1 := r1 + nMsg + 2
      ELSE
         cr1 := r1 + 1
      ENDIF

      setcolor( clr + "," + "+W/B" + ",,," + "N" + substr( clr, at( "/", clr ) ) )
      aScrolBar := ScrolBarNew( cr1 - 1, c2, r2 /*, colorGet( C_SCROLL ) )*/ )

      nLenScrol := len( ch_ )
      pmtWidth  := c2 - c1 - 3
      aeval( ch_, {|e,i| ch_[ i ] := pad( e, pmtWidth ) } )

      ScrolBarDisplay( aScrolBar )
      ScrolBarUpdate( aScrolBar, sel, nLenScrol, .t. )

      cgo_:= { sel, 0, lSlctns, lNumeric, ch_, lSelect_, exe_, aScrolbar, nLenScrol }

      DO WHILE .t.
         sel := VouchAChoice( cr1, c1 + 2, r2 - 1, c1 + (c2 - c1) - 2, ;
                              cgo_[CGO_CH_], cgo_[CGO_SEL_], "VouchFunc2", ;
                              cgo_[CGO_POS], cgo_[CGO_ROW], oWin, ;
                              @nLastKey, @cgo_ )
         IF !lSlctns
            EXIT
         ELSE
            IF nLastKey == K_ESC ;
                           .OR. nLastKey == K_CTRL_ENTER ;
                           .OR. nLastKey == K_ALT_F7
               EXIT
            ENDIF
         ENDIF
      ENDDO

   ELSEIF valtype (wait) = "N"
      sel := inkey (wait)
   ENDIF

   IF paste
      IF valtype( oGet:varGet() ) == "C"
         oVal := oGet:varGet()
         oGet:varPut( pad( ch_[ iif( sel = 0,1,sel ) ], len( oVal ) ) )
         oGet:display()
      ENDIF
   ENDIF

   IF restore
      VouchWndRest( oldScr )
      oldscr := NIL
   ENDIF

   IF lSlctns
      IF !lNumeric
         FOR i = 1 TO len( cgo_[CGO_CH_] )
            IF substr( cgo_[CGO_CH_,i], 1, 1) == CHECKMARK
               aadd( nSlctns_,i )
            ENDIF
         NEXT
      ELSE
         FOR i := 1 TO len(cgo_[CGO_CH_])
            IF val( left( cgo_[CGO_CH_,i],4 ) )>0
               aadd( dd_,{val( left( cgo_[CGO_CH_,i],4 ) ),i} )
            ENDIF
         NEXT
         IF !empty(dd_)
            asort(dd_,,,{|e_,f_| e_[1]<f_[1] })
            aeval(dd_,{|e_| aadd(nSlctns_,e_[2]) })
         ENDIF
      ENDIF
   ENDIF

   setcursor( oldCur )
   devpos( oldR,oldC )

   vstk_pop()
   setcolor( oldClr )

   RETURN iif( lSlctns, nSlctns_, sel )

//----------------------------------------------------------------------//

FUNCTION VouchFunc2( nMode, nElem, nRel, nKey, cgo_ )
   LOCAL n, i, nn, s

   IF nKey <> 0 .AND. nKey <> K_MOUSEMOVE
      ScrolBarUpdate( cgo_[ CGO_SCROL ], nElem, cgo_[ CGO_LENSCR ], .t. )
      IF cgo_[ CGO_EXE_ ] <> NIL
         eval( cgo_[ CGO_EXE_,nElem ] )
      ENDIF
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRel

   DO CASE
   CASE nKey == K_F1
      //  help()
      RETURN AC_CONT
   CASE nmode = AC_IDLE
      RETURN AC_CONT
   CASE nmode = AC_HITTOP
      KEYBOARD CHR( K_CTRL_PGDN )
      RETURN AC_CONT
   CASE nmode = AC_HITBOTTOM
      KEYBOARD CHR( K_CTRL_PGUP )
      RETURN AC_CONT
   CASE nmode = AC_EXCEPT
      DO CASE
      CASE nKey == K_F1
         //  help()
         RETURN AC_CONT
      CASE nKey = K_ESC
         RETURN AC_ABORT
      CASE nKey == K_F9      // TAG ALL
         IF cgo_[CGO_LSEL]
            IF cgo_[CGO_LNUM]
               FOR i := 1 TO len( cgo_[CGO_CH_] )
                  IF cgo_[CGO_SEL_]
                     cgo_[CGO_CH_,i] := chr( 251 ) + substr( cgo_[ CGO_CH_,i ], 2 )
                  ENDIF
               NEXT
               RETURN AC_ABORT
            ELSE
               RETURN AC_CONT
            ENDIF
         ELSE
            RETURN AC_CONT
         ENDIF
      CASE nKey == K_F10      // UnTAG ALL
         IF cgo_[CGO_LSEL]
            FOR i := 1 TO len( cgo_[CGO_CH_] )
               cgo_[CGO_CH_,i] := " "+substr( cgo_[CGO_CH_,i],2 )
            NEXT
            RETURN AC_ABORT
         ELSE
            RETURN AC_CONT
         ENDIF

      CASE nKey = K_ENTER
         IF cgo_[CGO_LSEL]
            IF !cgo_[CGO_LNUM]
               cgo_[CGO_CH_,cgo_[CGO_POS]] := iif( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,1 )==CHECKMARK, ;
                             " ",CHECKMARK )+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],2 )
               cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
               RETURN AC_ABORT
            ELSE
               IF( n:=val( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,4 ) ) )>0
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := "    "+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
                  FOR i := 1 TO len( cgo_[CGO_CH_] )
                     IF( nn := val( left( cgo_[CGO_CH_,i],4 ) ) )>0
                        IF nn > n
                           nn := nn - 1
                           s := iif( nn > 0,pad( hb_ntos( nn ),4 ),"    " )
                           cgo_[CGO_CH_,i] := s + substr( cgo_[CGO_CH_,i],5 )
                        ENDIF
                     ENDIF
                  NEXT
               ELSE
                  nn := 0
                  n  := 0
                  aeval( cgo_[CGO_CH_], {|e| n := val( left( e,4 ) ), nn := iif( n>nn,n,nn ) } )
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := pad( hb_ntos( nn+1 ),4 ) + substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1, len( cgo_[CGO_CH_] ) )
               ENDIF
               RETURN AC_ABORT
            ENDIF
         ELSE
            RETURN AC_SELECT
         ENDIF

      CASE nKey = K_CTRL_ENTER
         RETURN AC_SELECT
      CASE nKey = HB_K_RESIZE
         RETURN AC_CONT
      OTHERWISE
         IF cgo_[CGO_LSEL]
            cgo_[CGO_POS] := scan_f( cgo_[CGO_POS], cgo_[CGO_CH_], nKey, iif( !cgo_[CGO_LNUM],3,5 ) )
            RETURN AC_ABORT
         ELSE
            RETURN AC_GOTO
         ENDIF
      ENDCASE
   CASE nmode = AC_NOITEM
      RETURN AC_ABORT
   OTHERWISE
      RETURN AC_GOTO
   ENDCASE

   RETURN AC_CONT

//----------------------------------------------------------------------//

STATIC FUNCTION scan_f( elem, a_, key, nFrom )
   LOCAL n := elem, na, c

   c := lower( chr( key ) )
   na := ascan( a_, {|e| lower( substr( e, nFrom, 1 ) ) == c }, min( elem + 1, len( a_ ) ) )
   IF na == 0
      na := ascan( a_,{|e| lower( substr( e, nFrom, 1 ) ) == c },1,elem-1 )
   ENDIF
   IF na <> 0
      n := na
   ENDIF
   RETURN n

//----------------------------------------------------------------------//

#define BLACK                                     0
#define WHITE                                     7
#define DK_GRAY                                   8

#define ATTR_CONV( FORE, BACK )                   (BACK)*16+(FORE)
#define COL_SHADOW_ATTRIBUTE                      ATTR_CONV( DK_GRAY, BLACK )

//----------------------------------------------------------------------//

STATIC FUNCTION VouchShadow( t, l, b, r )
   IF r < maxcol() - 1 .AND. b < maxrow()
      sha_attr( b + 1, l + 1, b + 1, r + 1, COL_SHADOW_ATTRIBUTE )
      sha_attr( t + 1, r + 1, b + 1, r + 2, COL_SHADOW_ATTRIBUTE )
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION sha_attr( t, l, b, r, new_attr )
   LOCAL old_scr_area, new_scr_area, i

   old_scr_area := savescreen( t, l, b, r )
   new_scr_area = ""

   FOR i = 1 TO len(old_scr_area) STEP 2
      new_scr_area := new_scr_area + substr( old_scr_area, i, 1 ) + chr( new_attr )
   NEXT

   restscreen( t, l, b, r, new_scr_area )
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION VouchGetSome( msg, vrb, pass, pic, set_, wh, vl, nLastKey )
   LOCAL screen, l, nMaxLen, nLenMsg, nLenVrb, clr, r
   LOCAL t       := maxrow()-7
   LOCAL b       := maxrow()-3
   LOCAL GetList := {}
   LOCAL dType   := valtype( vrb )

   HB_SYMBOL_UNUSED( set_ )
   HB_SYMBOL_UNUSED( nLastKey )

   DEFAULT msg  TO 'Please Enter Required Value'
   DEFAULT wh   TO {|| .t. }
   DEFAULT vl   TO {|| .t. }
   DEFAULT pass TO .f.
   DEFAULT pic  TO iif( dType == 'Y', 'Y', '@! ' )

   clr := SetColor()

   nMaxLen := maxcol() - 7
   nLenMsg := len( msg )

   DO CASE
   CASE dType == 'D' ; nLenVrb := 8
   CASE dType == 'N' ; nLenVrb := 17
   CASE dType == 'C' ; nLenVrb := len( vrb )
   CASE dType == 'L' ; nLenVrb := 1
   ENDCASE

   IF nLenMsg + nLenVrb > nMaxLen   //  Only when vrb type c will be asked
      nLenVrb := nMaxLen - nLenMsg - 7
      pic     := substr(pic,1,1)+'S'+hb_ntos(nLenVrb)+substr(pic,2)
   ENDIF

   pic := iif( dType=='N','@Z 99999999999999.99', pic )
   l   := ( ( maxcol() + 1 - ( nLenMsg + nLenVrb + 7 ) ) / 2 )
   r   := l + nLenMsg+nLenVrb + 6

   SetColor( 'W+/RB,GR+/BG,,,W+/BG' )
   vstk_push()
   screen := VouchWndSave( t-1, l-4, b+2, r+3 )

   dispbox( t, l, b, r, "лплллмлл " )
   VouchShadow( t,l,b,r )

   @ t+2, l+3 SAY msg GET vrb PICTURE pic  WHEN eval(wh) VALID eval(vl)
   setCursor(1)
   read

   VouchWndRest( screen )
   vstk_pop()
   SetColor( clr )

   RETURN vrb

/*----------------------------------------------------------------------*/

FUNCTION help( cToken )
   LOCAL aScr := VouchWndSave( 0, 0, maxrow(), maxcol() )
   
   Vstk_push()
   SetCursor( 0 )
   SetColor( "W/B" )   
   CLS 
   
   SWITCH Upper( cToken )
   CASE "KEYS"
      /* HB_SCREEN_BEGINS <Keys> */
       
      /// 1 3 C 15 0 
      @ 1, 2     SAY "F1  This screen"                                      
      /// 2 3 C 34 0 
      @ 2, 2     SAY "F4  Properties of hilighted object"                   
      /// 3 3 C 25 0 
      @ 3, 2     SAY "F5  Edit hilighted object"                            
      /// 4 3 C 27 0 
      @ 4, 2     SAY "F6  Select hilighted object"                          
      /// 5 3 C 25 0 
      @ 5, 2     SAY "F7  Copy hilighted object"                            
      /// 6 3 C 23 0 
      @ 6, 2     SAY "F8  Paste copied object"                              
      /// 7 3 C 27 0 
      @ 8, 2     SAY "F10 Define a new GET object"                          
      /// 8 3 C 34 0 
      @ 7, 2     SAY "F9  Start to define new box object"                   
      /// 9 3 C 27 0 
      @ 10, 2    SAY "Del Delete hilighted object"                          
      /// 10 3 C 30 0 
      @ 12, 2    SAY "Ctrl+F6 Begins block selection"                       
      /// 11 3 C 52 0 
      @ 14, 2    SAY "Ctrl+F8 Cut and paste selected block at new location" 
      /// 12 3 C 43 0 
      @ 13, 2    SAY "Ctrl+F7 Copy selected block at new location"          
      /// 13 3 C 34 0 
      @ 24, 2    SAY "Press ESC to return to designer..."                   COLOR "GR+/B"
      /// 14 3 C 26 0 
      @ 20, 2    SAY "Alt+S Save designed screen"                           
      /// 15 3 C 25 0 
      @ 21, 2    SAY "Alt+L Load another screen"                            
       
      /* HB_SCREEN_ENDS <Keys> */
      EXIT
   ENDSWITCH

   DO WHILE inkey() != K_ESC; ENDDO
      
   VouchWndRest( aScr )
   Vstk_pop()
   
   RETURN NIL

/*----------------------------------------------------------------------*/

