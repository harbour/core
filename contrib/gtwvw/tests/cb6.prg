/* Copyright 2005 Budyanto Dj. <budyanto@centrin.net.id>

   This is an example on how to integrate GTWVW's combobox into
   regular GET/GETLIST system. Be creative, do not be bound by
   the ideas presented herein. There may be better ways to do it. */

#require "gtwvw"

#include "getexit.ch"
#include "inkey.ch"

/* Two different keyboard handling method
   (notes: WVW_CB_KBD_CLIPPER doesn't mimic anything from Cl*pper
           it just 'feels like Cl*pper' to me) */
#define WVW_CB_KBD_STANDARD  0
#define WVW_CB_KBD_CLIPPER   1

// our preferred method (choose one of the above)
STATIC s_nCB_Kbd := WVW_CB_KBD_CLIPPER

// list of created comboboxes
STATIC s_aComboList := {}

#xcommand @ <row>, <col> COMBOBOX <var> ;
      OPTIONS <aOptions> ;
      WIDTH <nWidth>   => ;
      ;
      __nCBid__ := wvw_cbCreate( , <row>, <col>, <nWidth>, ;
      <aOptions>, ;
      {| nWinNum, nId, nEvent, nIndex, temp | ;
      CBhandler( nWinNum, nId, nEvent, nIndex, <"var">, GetList ), HB_SYMBOL_UNUSED( temp ) ;
      }, ;
      , , s_nCB_Kbd ); ;
      AAdd( s_aComboList, { __nCBid__, <"var"> } ); ;
      __temp__ := wvw_cbFindString( , __nCBid__, <var> ); ;
      iif( __temp__ >= 0, wvw_cbSetIndex( , __nCBid__, __temp__ ), NIL ); ;
      SetPos( <row>, <col> ); ;
      AAdd( GetList, _GET_( <var>, <"var">, Replicate( "X", <nWidth> ),, ) ) ; ;
      ATail( GetList ):cargo := __nCBid__; ;
      ATail( GetList ):reader := {| get | CBreader( get ) }

PROCEDURE Main()

   LOCAL GetList := {}
   LOCAL mname := PadR( "Budyanto Dj.", 30 ), msex := "MALE", mage := 17, mstat := "married"
   LOCAL __nCBid__, __temp__  // these two are temporary var required by CB get creation

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   wvw_SetCodepage( , 255 )
   wvw_SetLineSpacing( , 4 )
   wvw_SetLSpaceColor( , 0 )
   wvw_cbSetFont( , "Arial", 14 )  // std: 20-2
#if 0
   Set( _SET_TRACESTACK, 0 )

   wvw_SetMouseMove( , .T. )
#endif

   CLS

   // reset combobox list:
   s_aComboList := {}

   @ 0, 0 SAY     "Name :" GET mname PICTURE "@K"
   @ 1, 0 SAY     "Sex  :"
   @ 1, 7 COMBOBOX msex OPTIONS { "Male", "Female", "Unknown" } WIDTH 10
   @ 2, 0 SAY     "Stat :"
   @ 2, 7 COMBOBOX mstat OPTIONS { "Married", "Unmarried", "Widowed" } WIDTH 15
   @ 3, 0 SAY     "age  :" GET mage PICTURE "999"
   READ

   // disable all comboboxes:
   AEval( s_aComboList, {| x | wvw_cbEnable( , x[ 1 ], .F. ) } )

   DevPos( 5, 0 )
   ? "name:", "'" + mname + "'"
   ? "sex :", "'" + msex + "'"
   ? "stat:", "'" + mstat + "'"
   ? "age :", hb_ntos( mage )
   ? "that's what you've got from GET"
   Inkey( 0 )

   // destroy all comboboxes:
   AEval( s_aComboList, {| x | wvw_cbDestroy( , x[ 1 ] ) } )
   s_aComboList := {}

   ?
   ? "Comboboxes have now been removed"
   ? "Now press ESC to exit"

   DO WHILE hb_keyStd( Inkey( 0 ) ) != K_ESC
   ENDDO

   RETURN

/* this function is called by GTWVW indirectly, through the main program's codeblock
 * which adds 'cVar' and 'GetList' parameter to the original 4 parameter passed
 * by GTWVW.
 *
 * This is a sample of how a combobox bound to a GetList is handled.
 *
 * Events handled here:
 * CBN_SETFOCUS: (3)
 * Find i where GetList[ i ] is Get object beneath the combobox, then
 * synchronize the contents, and let TGetList think she is working on it.
 *
 * CBN_KILLFOCUS: (4)
 * Find i where GetList[ i ] is Get object beneath the combobox, then
 * synchronize the contents.
 *
 * CBN_SELCHANGE: (1)
 * (do nothing)
 */
STATIC PROCEDURE CBhandler( nWinNum, nId, nEvent, nIndex, cVar, GetList )

   LOCAL i
   LOCAL oGet := GetActive()

   HB_SYMBOL_UNUSED( nIndex )

   /* if GetList is empty, then READ session is already ended
      this should not be happenning! */

   IF Empty( GetList )
      MyAlert( "Bad practice: you left an active combobox, but READ already ended" )
      RETURN  // ignore this event
   ENDIF

   SWITCH nEvent
   CASE 3  // CBN_SETFOCUS

      IF ( i := AScan( GetList, {| x | x:Name == cVar } ) ) > 0
         /* ! oGet:HasFocus means
          * CBN_SETFOCUS was NOT initiated from mouseclick
          * then we don't need to bother about setting focus to the
          * new GET. GetSys has already done that via CBreader().
          * It is CBreader() that brought us here, so ignore it.
          */
         IF oGet:HasFocus
            /* So user has jumped here by clicking on the combobox.
             * And this combobox has oNewGet beneath it.
             * But do NOT assign oGetList:oGet into this oNewGet
             * from within here!
             * Remember that the reader() is still suspended
             * on Inkey( 0 ). If we change the ActiveGet from here, then
             * when we leave this CB (eg. by means of K_TAB)
             * this reader() will resume, but alas the active get
             * is no longer the same! Thus reader() most likely
             * will behave incorrectly.
             *
             * The trick is simple:
             * reject the SETFOCUS. This will cause reader()
             * resume its action on Inkey( 0 ).
             * All we have to do here is emulate the mouseclick
             * event on oNewGet object beneath the CB, ie.
             * putting K_LBUTTONDOWN into keyboard buffer
             * at proper coordinate.
             * We will then arrive at cbreader().
             */

            SetWinFocus( nWinNum )
            MSetPos( GetList[ i ]:row, GetList[ i ]:col + 1 )
            hb_keyPut( K_LBUTTONDOWN )
         ENDIF  // oGet:HasFocus
      ELSE
         /* there's no GET object beneath the combobox.
          * This must be a combobox living in the wild.
          * Do what you want with it, we do nothing here.
          */
      ENDIF
      EXIT

   CASE 4 // CBN_KILLFOCUS
      // put current content of combobox into GET variable beneath it.
      oGet:varput( wvw_cbGetCurText( nWinNum, nId ) )
      oGet:display()  // this is optional
      EXIT

   ENDSWITCH

   RETURN

/* --- custom get reader --- */

// This is the reader() for oGet, a GET object hidden beneath a combobox.
//
// Some notes:
// oGet:cargo stores combobox id over this oGet
//
STATIC PROCEDURE CBreader( oGet )

   LOCAL nKey, nKeyStd, bKeyBlock
   LOCAL oGetList := __GetListActive()

   IF ! wvw_cbIsFocused( , oGet:cargo )
      wvw_cbSetFocus( , oGet:cargo )
   ENDIF

   oGet:setfocus()
   nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

   DO CASE
   CASE nKeyStd == K_ENTER
      // NOTE that in WVW_CB_KBD_CLIPPER mode we will never get here
      oGet:exitState := GE_DOWN

   CASE nKeyStd == K_UP
      oGet:exitState := GE_UP

   CASE nKeyStd == K_SH_TAB
      oGet:exitState := GE_UP

   CASE nKeyStd == K_DOWN
      // NOTE that in WVW_CB_KBD_STANDARD mode we will never get here
      oGet:exitState := GE_DOWN

   CASE nKeyStd == K_TAB
      oGet:exitState := GE_DOWN

   CASE nKeyStd == K_ESC
      IF Set( _SET_ESCAPE )
         oGet:exitState := GE_ESCAPE
      ENDIF

   CASE nKeyStd == K_PGUP
      oGet:exitState := GE_WRITE

   CASE nKeyStd == K_PGDN
      oGet:exitState := GE_WRITE

   CASE nKeyStd == K_CTRL_HOME
      oGet:exitState := GE_TOP

   CASE nKeyStd == K_LBUTTONDOWN .OR. nKeyStd == K_LDBLCLK
      // is there any GET object hit?
      IF Empty( HitTest( oGetList:aGetList, MRow(), MCol() ) )
         oGet:exitState := GE_NOEXIT
      ELSE
         oGet:exitState := GE_MOUSEHIT
      ENDIF

   CASE HB_ISEVALITEM( bKeyBlock := SetKey( nKey ) ) .OR. ;
        HB_ISEVALITEM( bKeyBlock := SetKey( nKeyStd ) )

      oGetList:GetDoSetKey( bKeyBlock )  // Eval(bKeyBlock)
      oGet:exitState := GE_NOEXIT

   ENDCASE

   IF oGet:exitState != GE_NOEXIT
      SetWinFocus( NIL )  // assume current window
      oGet:killfocus()
   ENDIF

   RETURN

#if 0
// move focus to GET object at GetList[nPos]
STATIC FUNCTION MoveToGet( GetList, nPos )

   LOCAL i
   LOCAL oGetList := __GetListActive()
   LOCAL oGet

   // leave current active get
   oGet := GetActive()

   IF oGet != NIL .AND. oGet:HasFocus
      IF oGet:changed
         IF GetPostValidate( oGet )
            oGet:updatebuffer()
         ELSE
            // oGet:undo()
            RETURN .F.
         ENDIF
      ENDIF
      oGet:exitstate := GE_MOUSEHIT
      oGetList:nNextGet := nPos
      oGetList:settle( , .F. )
   ENDIF

   RETURN .T.
#endif

/* miscellaneous */

// Set FOCUS to window nWinNum
STATIC PROCEDURE SetWinFocus( nWinNum )

   wapi_SetFocus( wvw_GetWindowHandle( nWinNum ) )

   RETURN

STATIC FUNCTION MyAlert( cMsg, par2, par3, par4, par5, par6 )

   LOCAL nLineSpacing := wvw_SetLineSpacing( NIL, 0 )
   LOCAL retval := Alert( cMsg, par2, par3, par4, par5, par6 )

   wvw_SetLineSpacing( NIL, nLineSpacing )

   RETURN retval
