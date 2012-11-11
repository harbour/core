/*
 * $Id$
 */

/*
   Copyright 2005 Budyanto Dj. <budyanto@centrin.net.id>

   This is an example on how to integrate GTWVW's combobox into
   regular GET/GETLIST system. Be creative, do not be bound by
   the ideas presented herein. There may be better ways to do it.

   This program requires GTWVW.LIB.

   Compile: bldwvw cbtest6
*/

#require "gtwvw"

#include "getexit.ch"
#include "inkey.ch"

/* Two different keyboard handling method
 * (notes: WVW_CB_KBD_CLIPPER doesn't mimic anything from Clipper
 *         it just 'feels like Clipper' to me)
 */
#define WVW_CB_KBD_STANDARD  0
#define WVW_CB_KBD_CLIPPER   1

// our preferred method (choose one of the above)
STATIC s_nCB_Kbd := WVW_CB_KBD_CLIPPER

// list of created comboboxes
STATIC s_aComboList := {}

// create these two as local, otherwise it will be assumed PRIVATE
MEMVAR __nCBid__, __temp__

#xcommand @ <row>, <col> COMBOBOX <var>                        ;
      OPTIONS <aOptions>                    ;
      WIDTH <nWidth>   =>                   ;
      ;
      ;
      __nCBid__ := wvw_cbCreate( NIL, <row>, <col>, <nWidth>, ;
      <aOptions>, ;
      {| nWinNum, nId, nEvent, nIndex, temp |     ;
      CBhandler( nWinNum, nId, nEvent, nIndex, <"var">, GetList ), HB_SYMBOL_UNUSED( temp );
      },                                          ;
      NIL, NIL, s_nCB_Kbd, NIL );                     ;
      AAdd( s_aComboList, { __nCBid__, <"var"> } );                        ;
      __temp__ := wvw_cbFindString( NIL, __nCBid__, <var> );             ;
      iif( __temp__ >= 0, wvw_cbSetIndex( NIL, __nCBid__, __temp__ ), NIL );   ;
      SetPos( <row>, <col> );                                              ;
      AAdd( GetList, _GET_( <var>, <"var">, Replicate( "X", <nWidth> ),, ) ) ;   ;
      ATail( GetList ):cargo := __nCBid__;                                ;
      ATail( GetList ):reader := {| get | CBreader( get ) }

PROCEDURE Main()

   LOCAL getlist := {}
   LOCAL mname := PadR( "Budyanto Dj.", 30 ), msex := "MALE", mage := 17, mstat := "married"
   LOCAL __nCBid__, __temp__  // these two are temporary var required by CB get creation

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   wvw_SetCodepage( NIL, 255 )
   wvw_SetLineSpacing( NIL, 4 )
   wvw_SetLSpaceColor( NIL, 0 )
   wvw_cbSetFont( NIL, "Arial", 14 )  // std: 20-2
#if 0
   Set( _SET_TRACESTACK, 0 )

   wvw_SetMouseMove( , .T. )
#endif

   CLS

   // reset combobox list:
   s_aComboList := {}

   @ 0, 0 SAY     "Name :" GET mname PICT "@K"
   @ 1, 0 SAY     "Sex  :"
   @ 1, 7 COMBOBOX msex OPTIONS { "Male", "Female", "Unknown" } WIDTH 10
   @ 2, 0 SAY     "Stat :"
   @ 2, 7 COMBOBOX mstat OPTIONS { "Married", "Unmarried", "Widowed" } WIDTH 15
   @ 3, 0 SAY     "age  :" GET mage PICT "999"
   READ

   // disable all comboboxes:
   AEval( s_aComboList, {| x | wvw_cbEnable( NIL, x[ 1 ], .F. ) } )

   DevPos( 5, 0 )
   ? "name: '" + mname + "'"
   ? "sex : '" + msex + "'"
   ? "stat: '" + mstat + "'"
   ? "age : " + hb_ntos( mage )
   ? "that's what you've got from GET"
   Inkey( 0 )

   // destroy all comboboxes:
   AEval( s_aComboList, {| x | wvw_cbDestroy( NIL, x[ 1 ] ) } )
   s_aComboList := {}

   ?
   ? "Comboboxes have now been removed"
   ? "Now press ESC to exit"

   DO WHILE Inkey( 0 ) != K_ESC
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
 * Find i where GetList[i] is Get object beneath the combobox, then
 * synchronize the contents, and let TGetList think she is working on it.
 *
 * CBN_KILLFOCUS: (4)
 * Find i where GetList[i] is Get object beneath the combobox, then
 * synchronize the contents.
 *
 * CBN_SELCHANGE: (1)
 * (do nothing)
 */
FUNCTION CBhandler( nWinNum, nId, nEvent, nIndex, cVar, GetList )

   LOCAL i, ccursel
   LOCAL oGet := GetActive()

   HB_SYMBOL_UNUSED( nIndex )

   /* if GetList is empty, then READ session is already ended
    * this should not be happenning!
    */

   IF Empty( GetList )
      MyAlert( "Bad practice: you left an active combobox, but READ already ended" )
      RETURN NIL // ignore this event
   ENDIF

   DO CASE
   CASE nEvent == 3 // CBN_SETFOCUS
      i := AScan( GetList, {| x | x:Name == cVar } )
      IF i > 0
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
             * on inkey(0). If we change the ActiveGet from here, then
             * when we leave this CB (eg. by means of K_TAB)
             * this reader() will resume, but alas the active get
             * is no longer the same! Thus reader() most likely
             * will behave incorrectly.
             *
             * The trick is simple:
             * reject the SETFOCUS. This will cause reader()
             * resume its action on inkey(0).
             * All we have to do here is emulate the mouseclick
             * event on oNewGet object beneath the CB, ie.
             * putting K_LBUTTONDOWN into keyboard buffer
             * at proper coordinate.
             * We will then arrive at cbreader().
             */

            SetWinFocus( nWinNum )
            MSetPos( GetList[ i ]:row, GetList[ i ]:col + 1 )
            hb_keyPut( K_LBUTTONDOWN )
         ENDIF // oGet:HasFocus

      ELSE  // i==0
         /* there's no GET object beneath the combobox.
          * This must be a combobox living in the wild.
          * Do what you want with it, we do nothing here.
          */
      ENDIF

   CASE nEvent == 4 // CBN_KILLFOCUS
      // put current content of combobox into GET variable beneath it.
      cCurSel := wvw_cbGetCurText( nWinNum, nId )
      oGet:varput( cCurSel )
      oGet:display() // this is optional

   ENDCASE

   RETURN NIL

/************* custom get reader ******************/

// This is the reader() for oGet, a GET object hidden beneath a combobox.
//
// Some notes:
// oGet:cargo stores combobox id over this oGet
//
FUNCTION CBreader( oGet )

   LOCAL nKey, bKeyBlock
   LOCAL oGetList := __GetListActive()

   IF ! wvw_cbIsFocused( NIL, oGet:cargo )
      wvw_cbSetFocus( NIL, oGet:cargo )
   ENDIF

   oGet:setfocus()
   nKey := Inkey( 0 )

   IF nKey == K_ENTER
      // NOTE that in WVW_CB_KBD_CLIPPER mode we will never get here
      oGet:exitState := GE_DOWN

   ELSEIF nKey == K_UP
      oGet:exitState := GE_UP

   ELSEIF nKey == K_SH_TAB
      oGet:exitState := GE_UP

   ELSEIF nKey == K_DOWN
      // NOTE that in WVW_CB_KBD_STANDARD mode we will never get here
      oGet:exitState := GE_DOWN

   ELSEIF nKey == K_TAB
      oGet:exitState := GE_DOWN

   ELSEIF nKey == K_ESC
      IF Set( _SET_ESCAPE )
         oGet:exitState := GE_ESCAPE
      ENDIF

   ELSEIF nKey == K_PGUP
      oGet:exitState := GE_WRITE

   ELSEIF nKey == K_PGDN
      oGet:exitState := GE_WRITE

   ELSEIF nKey == K_CTRL_HOME
      oGet:exitState := GE_TOP

   ELSEIF nKey == K_LBUTTONDOWN .OR. nKey == K_LDBLCLK
      // is there any GET object hit?
      IF ! Empty( HitTest( oGetList:aGetList, MRow(), MCol(), NIL ) )
         oGet:exitState := GE_MOUSEHIT
      ELSE
         oGet:exitState := GE_NOEXIT
      ENDIF

   ELSEIF HB_ISBLOCK( bKeyBlock := SetKey( nKey ) )
      oGetList:GetDoSetKey( bKeyBlock )  // eval(bKeyBlock)
      oGet:exitState := GE_NOEXIT

   ENDIF

   IF oGet:exitState != GE_NOEXIT
      SetWinFocus( NIL )  // assume current window
      oGet:killfocus()
   ENDIF

   RETURN NIL

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

/* miscellaneous **********************************/

// Set FOCUS to window nWinNum
STATIC FUNCTION SetWinFocus( nWinNum )

   LOCAL hWnd := wvw_GetWindowHandle( nWinNum )

   win_SetFocus( hWnd )

   RETURN NIL

STATIC FUNCTION MyAlert( cMsg, par2, par3, par4, par5, par6 )

   LOCAL nLineSpacing := wvw_SetLineSpacing( NIL, 0 )
   LOCAL retval := Alert( cMsg, par2, par3, par4, par5, par6 )

   wvw_SetLineSpacing( NIL, nLineSpacing )

   RETURN retval
