/* Copyright (c) Budyanto Dj. <budyanto@centrin.net.id>

   Editbox demo
   multi-field GET session(s) using editboxes
   with a simple sample of @...EBGET command translation

   This sample also shows simple implementation of WVW_INPUTFOCUS()
   to handle some keyboard inputs on non topmost window

   (The editbox itself always accept input independent of WVW_INPUTFOCUS())

   Some parts of this sample are modifications from MINIGUI's sourcecode
   to handle "masks" during editbox input session:
     ProcessCharMask()
     CharMaskTekstOK()
     GetValFromText()
     GetNumMask() */

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"

#include "hbwin.ch"

#define EN_SETFOCUS         0x0100
#define EN_KILLFOCUS        0x0200
#define EN_CHANGE           0x0300
#define EN_UPDATE           0x0400
#define EN_ERRSPACE         0x0500
#define EN_MAXTEXT          0x0501
#define EN_HSCROLL          0x0601
#define EN_VSCROLL          0x0602

#command @ <row>, <col> EBGET <var> ;
      [LABEL <label>] ;
      [<multiline: MULTILINE>] ;
      [PICTURE <pic>] ;
      => AddEBGet( aEBGets, <row>, <col>, @<var>, <"var">, {| x | <var> := x }, <label>, <.multiline.>, <pic> )

// constants to aEBGets member,
// according to EBReadGets() convention
// NOTE: a smarter way would be to use CLASS instead of arrays
#define __GET_LMULTILINE 1
#define __GET_CLABEL 2
#define __GET_NROW   3
#define __GET_NCOL   4
#define __GET_XINIT  5
#define __GET_CPICT  6
#define __GET_CVALTYPE  7
#define __GET_BTEXT   8
#define __GET_BASSIGN 9
#define __GET_NEBID  10
#define __GET_LFOCUSED  11

// REQUEST HB_NOSTARTUPWINDOW

PROCEDURE Main()

   LOCAL lClosepermitted := .F.
   LOCAL bSetKey := SetKey( K_F8, {|| MyHelp() } )

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SetMode( 4, 54 )   // a small window
   SetColor( "N/W" )
   wvw_SetFont( 0, "Courier New", 16, - 7 )
   wvw_ebSetFont( 0, "Arial" )  // font for editbox
   wvw_pbSetFont( 0, "Arial" )  // font for pushbuttons

   wvw_SetCodepage( 0, 255 )
   wvw_AllowNonTopEvent( .T. )  // this will make pushbuttons to work
   // even on non-topmost window
   wvw_RecurseCBlock( .T. )  // this will allow recursed execution
   // of control's codeblocks
   // eg. multiple executions of pushbutton's codeblock
   //    invoking "GetSession()"

   SetCursor( SC_NONE )  // we don't need cursor

   CLS
   @ 0, 1 SAY "Click NEW to open a new GET session, CLOSE when done"
   wvw_pbCreate( 0, 2, 1, 2, 10, "New", , {|| GetSession() } )
   wvw_pbCreate( 0, 2, 12, 2, 22, "Close", , {|| ToCloseWindow( 0, @lClosepermitted ) } )

   // activate/show the main window
   wvw_ShowWindow( 0 )

   // wait until user click the close button
   DO WHILE ! lClosepermitted
      Inkey( 0.2 )
   ENDDO
   SetKey( K_F8, bSetKey )

   RETURN

// This is the sample of a GET session.
// This sample is reentrant, can be executed more than once if user clicks
// "NEW" button in the main window multiple times.
STATIC PROCEDURE GetSession()

   STATIC s_nsession := 0

   LOCAL aEBGets := {}
   LOCAL cName      := Space( 30 )
   LOCAL cNickName  := Space( 10 )
   LOCAL dBirthdate := hb_SToD()
   LOCAL nBudget    := 125000
   LOCAL cRemark    := ;
      "Some notes" + hb_eol() + ;
      "about this person"
   LOCAL nwinnum
   LOCAL nrow1, ncol1, nrow2, ncol2

   IF s_nsession > 15
      MyMessageBox( 0, "Sorry, maximum number of sessions reached" )
      RETURN
   ENDIF

   s_nsession++
   nwinnum := s_nsession

   nrow1 := 4 + ( s_nsession - 1 ) * 1
   ncol1 := 10 + ( s_nsession - 1 ) * 2
   nrow2 := nrow1 + 15
   ncol2 := ncol1 + 60
   wvw_nOpenWindow( "Session " + hb_ntos( s_nsession ) + " (press F8 for help)", ;
      nrow1, ncol1, nrow2, ncol2, , 0 )

   cRemark += hb_eol() + "(from Session " + hb_ntos( nwinnum ) + ")"

   @ 1, 15 ebGET cName      LABEL "Name:"
   @ 3, 15 ebGET cNickName  LABEL "Nickname:"   PICTURE Replicate( "!", Len( cNickName ) )
   @ 5, 15 ebGET dBirthDate LABEL "Birth Date:"
   @ 7, 15 ebGET nBudget    PICTURE "999,999.99"   // using default label
   @ 9, 15 ebGET cRemark    LABEL "Remarks:" MULTILINE
   EBReadGets( nwinnum, @aEBGets )   // READ

   // debugging text
   MyMessageBox( nwinnum, ;
      "Back to GetSession() of window " + hb_ntos( nwinnum ) + " with these values returned:" + hb_eol() + ;
      "cName:" + cName + hb_eol() + ;
      "cNickName:" + cNickName + hb_eol() + ;
      "dBirthDate:" + DToC( dBirthDate ) + hb_eol() + ;
      "nBudget:" + Transform( nBudget, "999,999.99" ) + hb_eol() + ;
      "cRemark:" + cRemark )

   wvw_lCloseWindow()

   s_nsession--

   RETURN

STATIC PROCEDURE MyHelp()

   LOCAL ccallstack := ""
   LOCAL i

   FOR i := 0 TO 8
      ccallstack += hb_ntos( i ) + ". " + ProcName( i ) + "(" + hb_ntos( ProcLine( i ) ) + ")" + hb_eol()
   NEXT

   MyMessageBox( , ;
      "Sorry, this is not really a help :-)" + hb_eol() + ;
      "It is only to show that SetKey() codeblock is handled by our editboxes" + hb_eol() + ;
      "Call stack:" + hb_eol() + ;
      ccallstack )

   RETURN

PROCEDURE WVW_SETFOCUS( nWinNum, hWnd )  /* must be a public function */

   HB_SYMBOL_UNUSED( hWnd )

   IF nwinnum != 0
      wvw_nSetCurWindow( nwinnum )
   ENDIF

   RETURN

/* Typical application ends here.
   Below are EBReadGets() and its supporters that are
   usable for general needs
   (can be put into separated module) */

// 2007-05-25

// adding one EBGet variable into aEBGets array
// returns .T. if successful
STATIC FUNCTION AddEBGet( aEBGets, mnrow, mncol, mxValue, mcVarName, mbAssign, mcLabel, mlMultiline, mcPict )

   LOCAL mcVarType, mbText

   SWITCH mcVarType := ValType( mxValue )
   CASE "C"
      mcPict := hb_defaultValue( mcPict, Replicate( "X", Len( mxValue ) ) )
      mbText := {|| mxValue }
      EXIT
   CASE "N"
      mcPict := hb_defaultValue( mcPict, "999,999,999.99" )
      mbText := {|| Transform( mxValue, mcPict ) }
      EXIT
   CASE "D"
      mcPict := hb_defaultValue( mcPict, "9999-99-99" )
      mbText := {|| DToC( mxValue ) }
      EXIT
   OTHERWISE
      // unsupported valtype
      RETURN .F.
   ENDSWITCH

   hb_default( @aEBGEts, {} )

   IF ! HB_ISLOGICAL( mlMultiline ) .OR. ;
      ! HB_ISSTRING( mxValue )
      mlMultiline := .F.
   ENDIF
   hb_default( @mcLabel, mcVarName + ":" )

   AAdd( aEBGets, { ;
      mlMultiline, ;    // __GET_LMULTILINE
      mcLabel, ;        // __GET_CLABEL
      mnrow, ;          // __GET_NROW
      mncol, ;          // __GET_NCOL
      mxValue, ;        // __GET_XINIT
      mcPict, ;         // __GET_CPICT
      mcVarType, ;      // __GET_CVALTYPE
      mbText, ;         // __GET_BTEXT
      mbAssign, ;       // __GET_BASSIGN
      NIL, ;            // __GET_NEBID
      .F. } )           // __GET_LFOCUSED

   RETURN .T.

// generic procedure to run aEBGets, array of editboxes
STATIC PROCEDURE EBReadGets( nwinnum, aEBGets )

   LOCAL nmaxrow, nmincol
   LOCAL aGet, nlen, lmultiline, clabel, nrow1, ncol1, nrow2, ncol2
   LOCAL nOKbutton, nCancelbutton, nClosebutton, ldone := .F.
   LOCAL lclosePermitted := .F.
   LOCAL nNumGets := Len( aEBGets )
   LOCAL nKey, nKeyStd
   LOCAL nfocus, lchangefocus

   IF nNumGets == 0
      RETURN
   ENDIF

   wvw_nSetCurWindow( nwinnum )
   nmaxrow := 0
   nmincol := 99999
   FOR EACH aGet IN aEBGets
      lmultiline := aGet[ __GET_LMULTILINE ]
      IF lmultiline
         nlen := 30
      ELSE
         nlen := Len( aGet[ __GET_CPICT ] )
      ENDIF
      clabel := aGet[ __GET_CLABEL ]
      nrow1 := aGet[ __GET_NROW ]
      ncol1 := aGet[ __GET_NCOL ]
      nrow2 := iif( aGet[ __GET_LMULTILINE ], nrow1 + 3, nrow1 )
      ncol2 := ncol1 + nlen - 1

      @ nrow1, ncol1 - Len( clabel ) - 1 SAY clabel

      aGet[ __GET_NEBID ] := wvw_ebCreate( nwinnum, nrow1, ncol1, nrow2, ncol2, ;
         Transform( aGet[ __GET_XINIT ], aGet[ __GET_CPICT ] ), ;
         {| nWinNum, nId, nEvent | MaskEditBox( nWinNum, nId, nEvent, @aEBGets ) }, ;
         aGet[ __GET_LMULTILINE ], ;  // EBtype
      0, ;  // nmorestyle
      iif( lmultiline, NIL, nlen + 1 ), ; // nMaxChar
      , )

      nmaxrow := Max( nmaxrow, nrow2 )
      nmincol := Min( nmincol, ncol1 )
   NEXT
   nrow1 := nmaxrow + 2 // Min(nmaxrow+2, MaxRow())
   ncol1 := nmincol // Min(nmincol, MaxCol()-33)
   nOKbutton := wvw_pbCreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "OK", , ;
      {|| SaveVar( nwinnum, @aEBGets, @lDone ), ;
      EndGets( nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton );
      } )

   ncol1 += 10 + 1
   nCancelbutton := wvw_pbCreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "Cancel", , ;
      {|| CancelVar( nwinnum, @aEBGets, @lDone ), ;
      EndGets( nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton );
      } )

   ncol1 += 10 + 1
   nClosebutton := wvw_pbCreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "Close", , ;
      {|| ToCloseWindow( nwinnum, @lClosepermitted ) } )
   wvw_pbEnable( nwinnum, nclosebutton, .F. )

   // register a keyhandler for WVW_INPFOCUS
   inp_handler( nwinnum, {| n, ch | InpKeyHandler( n, ch, aEBGets, nOKbutton, nCancelbutton ) } )

   wvw_ebSetFocus( nwinnum, aEBGets[ 1 ][ __GET_NEBID ] )
   nFocus := 1
   DO WHILE ! lDone
      nKeyStd := hb_keyStd( nKey := Inkey( 0.5, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )
      DO CASE
      CASE HB_ISEVALITEM( SetKey( nKey ) )
         Eval( SetKey( nKey ) )
      CASE HB_ISEVALITEM( SetKey( nKeyStd ) )
         Eval( SetKey( nKeyStd ) )
      CASE nKeyStd != 0
         lchangefocus := .T.
         DO CASE
         CASE nKeyStd == K_TAB .OR. nKeyStd == K_DOWN .OR. nKeyStd == K_ENTER
            IF nFocus < ( nNumGets + 2 )  // incl buttons
               nFocus++
            ELSE
               nFocus := 1
            ENDIF
         CASE nKeyStd == K_SH_TAB .OR. nKeyStd == K_UP
            IF nFocus > 1
               nFocus--
            ELSE
               nFocus := nNumGets + 2
            ENDIF
         OTHERWISE
            lchangefocus := .F. // ! wvw_ebIsFocused(nwinnum, aEBGets[nFocus][__GET_NEBID])
         ENDCASE
         IF lchangefocus
            IF nFocus <= nNumGets
               wvw_ebSetFocus( nwinnum, aEBGets[ nFocus ][ __GET_NEBID ] )
            ELSEIF nFocus == nNumGets + 1
               wvw_pbSetFocus( nwinnum, nOKbutton )
            ELSEIF nFocus == nNumGets + 2
               wvw_pbSetFocus( nwinnum, nCancelbutton )
            ENDIF
         ENDIF
      ENDCASE
      IF wvw_pbIsFocused( nwinnum, nOKbutton )
         nFocus := nNumGets + 1
      ELSEIF wvw_pbIsFocused( nwinnum, nCancelbutton )
         nFocus := nNumGets + 2
      ELSE
         nFocus := nFocused( aEBGets )
      ENDIF
   ENDDO

   // session ended (already ended by OK or Cancel)

   lClosepermitted := ( nwinnum == wvw_nNumWindows() - 1 )
   // wait until user click the close button
   DO WHILE ! lClosepermitted
      Inkey( 0.5 )
   ENDDO

   RETURN

// inp_handler(nwinnum, bhandler)

STATIC PROCEDURE InpKeyHandler( nwinnum, ch, aEBGets, nOKbutton, nCancelbutton )

   LOCAL nNumGets := Len( aEBGets )
   LOCAL nFocus, lchangefocus

   IF HB_ISEVALITEM( SetKey( ch ) )
      Eval( SetKey( ch ) )
      RETURN
   ELSEIF ch == 0
      RETURN
   ENDIF
   DO CASE
   CASE wvw_pbIsFocused( nwinnum, nOKbutton )
      nFocus := nNumGets + 1
   CASE wvw_pbIsFocused( nwinnum, nCancelbutton )
      nFocus := nNumGets + 2
   OTHERWISE
      nFocus := nFocused( aEBGets )
   ENDCASE
   lchangefocus := .T.
   DO CASE
   CASE ch == K_TAB .AND. ! lShiftPressed()
      IF nFocus < ( nNumGets + 2 )  // incl buttons
         nFocus++
      ELSE
         nFocus := 1
      ENDIF
   CASE ch == K_TAB .AND. lShiftPressed()
      IF nFocus > 1
         nFocus--
      ELSE
         nFocus := nNumGets + 2
      ENDIF
   OTHERWISE
      lchangefocus := .F.
   ENDCASE
   IF lchangefocus
      DO CASE
      CASE nFocus <= nNumGets
         wvw_ebSetFocus( nwinnum, aEBGets[ nFocus ][ __GET_NEBID ] )
      CASE nFocus == nNumGets + 1
         wvw_pbSetFocus( nwinnum, nOKbutton )
      CASE nFocus == nNumGets + 2
         wvw_pbSetFocus( nwinnum, nCancelbutton )
      ENDCASE
   ENDIF

   RETURN

STATIC PROCEDURE EndGets( nwinnum, aEBGets, nOKbutton, nCancelbutton, nCloseButton )

   LOCAL aGet

   // session ended
   FOR EACH aGet IN aEBGets
      wvw_ebEnable( nwinnum, aGet[ __GET_NEBID ], .F. )
   NEXT
   wvw_pbEnable( nwinnum, nOKbutton, .F. )
   wvw_pbEnable( nwinnum, nCancelbutton, .F. )

   // clear the GetList
   ASize( aEBGets, 0 )

   // wait until user click the close button
   wvw_pbEnable( nwinnum, nclosebutton, .T. )

   RETURN

// save values into variables
STATIC PROCEDURE SaveVar( nwinnum, aEBGets, lDone )

   LOCAL aGet

   FOR EACH aGet IN aEBGets
      // do some validation if necessary
      Eval( aGet[ __GET_BASSIGN ], ;
         GetValFromText( wvw_ebGetText( nwinnum, aGet[ __GET_NEBID ] ), aGet[ __GET_CVALTYPE ] ) )
   NEXT
   lDone := .T.

   // debugging text
   MyMessageBox( nwinnum, ;
      "Get session in window " + hb_ntos( nwinnum ) + " is ended with confirmation" + hb_eol() + ;
      "Values have been assigned to the respective ebGET variables" )

   RETURN

// restore initial values into variables
STATIC PROCEDURE CancelVar( nwinnum, aEBGets, lDone )

   LOCAL aGet

   FOR EACH aGet IN aEBGets
      Eval( aGet[ __GET_BASSIGN ], aGet[ __GET_XINIT ] )
   NEXT
   lDone := .T.

   // debugging text
   MyMessageBox( nwinnum, ;
      "Get session in window " + hb_ntos( nwinnum ) + " is ended with cancellation" + hb_eol() + ;
      "Values has been assigned to the respective initial ebGET variables" )

   RETURN

STATIC PROCEDURE ToCloseWindow( nwinnum, /* @ */ lPermitted )

   // allow to close topmost window only
   IF !( lPermitted := ( nwinnum == wvw_nNumWindows() - 1 ) )
      MyMessageBox( nwinnum, ;
         "Window " + hb_ntos( nwinnum ) + " is not allowed to be closed, yet" + hb_eol() + ;
         "Please close window " + hb_ntos( wvw_nNumWindows() - 1 ) + " first" )
   ENDIF

   RETURN

// returns index to aEBGets array containing editbox nEBid

STATIC FUNCTION nGetIndex( aEBGets, nEBId )
   RETURN AScan( aEBGets, {| x | x[ __GET_NEBID ] == nEBId } )

// returns index to aEBGets array containing editbox that is/was in focus

STATIC FUNCTION nFocused( aEBGets )
   RETURN AScan( aEBGets, {| x | x[ __GET_LFOCUSED ] } )

// callback function called by GTWVW during some events on editbox
STATIC PROCEDURE MaskEditBox( nWinNum, nId, nEvent, aEBGets )

   STATIC s_bBusy := .F.

   LOCAL ctext
   LOCAL nIndex := nGetIndex( aEBGets, nId )
   LOCAL mcvaltype, mcpict, mlmultiline
   LOCAL nwasfocus

   IF s_bBusy
      RETURN
   ENDIF
   IF nIndex == 0
      RETURN
   ENDIF
   s_bBusy := .T.
   mcvaltype := aEBGets[ nIndex ][ __GET_CVALTYPE ]
   mcpict := aEBGets[ nIndex ][ __GET_CPICT ]
   mlmultiline := aEBGets[ nIndex ][ __GET_LMULTILINE ]

   DO CASE
   CASE nEvent == EN_KILLFOCUS
      IF ! mlmultiline .AND. mcvaltype $ "ND"
         ctext := wvw_ebGetText( nwinnum, nid )
         IF mcvaltype == "D" .AND. IsBadDate( ctext )
            // don't leave it in an invalid state
            wvw_ebSetFocus( nwinnum, nid )
         ELSE
            wvw_ebSetText( nwinnum, nId, Transform( GetValFromText( ctext, mcvaltype ), mcpict ) )
         ENDIF
      ENDIF
   CASE nEvent == EN_SETFOCUS
      IF ! mlmultiline .AND. mcvaltype == "N"
         ctext := wvw_ebGetText( nwinnum, nid )
         wvw_ebSetText( nwinnum, nId, Transform( GetValFromText( ctext, mcvaltype ), GetNumMask( mcpict, mcvaltype ) ) )
      ENDIF
      wvw_ebSetSel( nwinnum, nid, 0, -1 )
      nwasFocus := nFocused( aEBGets )
      IF nwasFocus != 0
         aEBGets[ nwasFocus ][ __GET_LFOCUSED ] := .F.
      ENDIF
      aEBGets[ nIndex ][ __GET_LFOCUSED ] := .T.
   CASE nEvent == EN_CHANGE
      IF ! mlmultiline
         ProcessCharMask( nwinnum, nId, mcvaltype, mcpict )
      ENDIF
   ENDCASE
   s_bBusy := .F.

   RETURN

/* --- borrowed and modified from minigui --- */

// from h_textbox.prg

STATIC PROCEDURE ProcessCharMask( mnwinnum, mnebid, mcvaltype, mcpict )

   LOCAL InBuffer, OutBuffer := "", icp, x, CB, CM, BadEntry, InBufferLeft, InBufferRight, Mask, OldChar, BackInbuffer
   LOCAL pc
   LOCAL fnb := 0
   LOCAL dc := 0
   LOCAL pFlag
   LOCAL ncp
   LOCAL NegativeZero := .F.
   LOCAL Output
   LOCAL ol

   SWITCH mcvaltype
   CASE "N"
      Mask := GetNumMask( mcpict, mcvaltype )
      EXIT
   CASE "D"
      Mask := mcpict  // "9999-99-99"
      EXIT
   OTHERWISE
      Mask := mcpict
   ENDSWITCH

   // Store Initial CaretPos
   wvw_ebGetSel( mnwinnum, mnebid, , @icp )

   // Get Current Content
   InBuffer := wvw_ebGetText( mnwinnum, mnebid )

   pc := 0  // for clarity
   pFlag := .F.  // for clarity
   IF mcvaltype == "N"
      // RL 104
      IF hb_LeftEq( AllTrim( InBuffer ), "-" ) .AND. Val( InBuffer ) == 0
         NegativeZero := .T.
      ENDIF

      IF PCount() > 1
         // Point Count For Numeric InputMask
         FOR x := 1 TO Len( InBuffer )
            IF SubStr( InBuffer, x, 1 ) $ ",."
               pc++
            ENDIF
         NEXT

         // RL 89
         IF hb_LeftEq( InBuffer, "." ) .OR. hb_LeftEq( InBuffer, "," )
            pFlag := .T.
         ENDIF

         // Find First Non-Blank Position
         FOR x := 1 TO Len( InBuffer )
            IF !( SubStr( InBuffer, x, 1 ) == " " )
               fnb := x
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   BackInBuffer := InBuffer

   OldChar := SubStr( InBuffer, icp + 1, 1 )

   IF Len( InBuffer ) < Len( Mask )
      InBufferLeft := Left( InBuffer, icp )
      InBufferRight := Right( InBuffer, Len( InBuffer ) - icp )

      IF CharMaskTekstOK( InBufferLeft + " " + InBufferRight, mcvaltype, Mask ) .AND. ;
         ! CharMaskTekstOK( InBufferLeft + InBufferRight, mcvaltype, Mask )
         InBuffer := InBufferLeft + " " + InBufferRight
      ELSE
         InBuffer := InBufferLeft + InBufferRight
      ENDIF
   ENDIF

   IF Len( InBuffer ) > Len( Mask ) .AND. ! HB_ISNULL( Mask )

      InBufferLeft := Left( InBuffer, icp )
      InBufferRight := Right( InBuffer, Len( InBuffer ) - icp - 1 )

      InBuffer := InBufferLeft + InBufferRight
   ENDIF

   // Process Mask
   OutBuffer := ""  // for clarity
   BadEntry := .F.  // for clarity
   FOR x := 1 TO Len( Mask )
      CB := SubStr( InBuffer, x, 1 )
      CM := SubStr( Mask, x, 1 )

      SWITCH CM
      CASE "A"
      CASE "!"
         IF IsAlpha( CB ) .OR. CB == " "
            IF CM == "!"
               OutBuffer += Upper( CB )
            ELSE
               OutBuffer += CB
            ENDIF
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT

      CASE "9"
         IF IsDigit( CB ) .OR. CB == " " .OR. ;
            ( mcvaltype == "N" .AND. ;
            CB == "-" .AND. x == fnb .AND. PCount() > 1 )

            OutBuffer += CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT

      CASE " "
         IF CB == " "
            OutBuffer += CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer += OldChar
            ELSE
               OutBuffer += " "
            ENDIF
         ENDIF
         EXIT

      CASE "X"
         OutBuffer += CB
         EXIT

      OTHERWISE
         OutBuffer += CM

      ENDSWITCH
   NEXT

   // Replace Content
   IF !( BackInBuffer == OutBuffer )
      wvw_ebSetText( mnwinnum, mnebid, OutBuffer )
   ENDIF

   IF pc > 1
      // pc>1 means we must to JUMP to the decimal point

      // RL 104
      IF NegativeZero
         Output := Transform( GetValFromText( wvw_ebGetText( mnwinnum, mnebid ), mcvaltype ), Mask )

         // better:
         ol := Len( Output )
         Output := PadL( "-" + SubStr( Output, At( ".", OutBuffer ) - 1 ), ol )

         // Replace Text
         wvw_ebSetText( mnwinnum, mnebid, Output )
         wvw_ebSetSel( mnwinnum, mnebid, At( ".", OutBuffer ) + dc, At( ".", OutBuffer ) + dc )
      ELSE
         wvw_ebSetText( mnwinnum, mnebid, Transform( GetValFromText( wvw_ebGetText( mnwinnum, mnebid ), mcvaltype ), Mask ) )
         wvw_ebSetSel( mnwinnum, mnebid, At( ".", OutBuffer ) + dc, At( ".", OutBuffer ) + dc )
      ENDIF

   ELSE
      IF pFlag
         ncp := At( ".", wvw_ebGetText( mnwinnum, mnebid ) )
         wvw_ebSetSel( mnwinnum, mnebid, ncp, ncp )
      ELSE
         // Restore Initial CaretPos
         IF BadEntry
            icp--
         ENDIF

         wvw_ebSetSel( mnwinnum, mnebid, icp, icp )

         // Skip Protected Characters
         FOR x := 1 TO Len( OutBuffer )
            CB := SubStr( OutBuffer, icp + x, 1 )
            CM := SubStr( Mask, icp + x, 1 )

            IF ! IsDigit( CB ) .AND. ! IsAlpha( CB ) .AND. ;
               ( !( CB == " " ) .OR. ( CB == " " .AND. CM == " " ) )
               wvw_ebSetSel( mnwinnum, mnebid, icp + x, icp + x )
            ELSE
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN

// from h_textbox.prg

STATIC FUNCTION CharMaskTekstOK( cString, cValType, cMask )

#if 0
   LOCAL lPassed := .T.
#endif
   LOCAL CB, CM, x

   IF cValType == "D"
      FOR x := 1 TO Min( Len( cString ), Len( cMask ) )
         CB := SubStr( cString, x, 1 )
         CM := SubStr( cMask, x, 1 )
         SWITCH CM
         CASE "9"
            IF IsDigit( CB ) .OR. CB == " "
               // lPassed := .T.
               EXIT
            ELSE
               RETURN .F.
            ENDIF
         OTHERWISE
            // lPassed := .T.
         ENDSWITCH
      NEXT
      RETURN .T.
   ENDIF

   FOR x := 1 TO Min( Len( cString ), Len( cMask ) )
      CB := SubStr( cString, x, 1 )
      CM := SubStr( cMask, x, 1 )
      SWITCH CM
         // JK
      CASE "A"
      CASE "!"
         IF IsAlpha( CB ) .OR. CB == " "
            // lPassed := .T.
            EXIT
         ELSE
            RETURN .F.
         ENDIF
      CASE "9"
         IF IsDigit( CB ) .OR. CB == " "
            // lPassed := .T.
            EXIT
         ELSE
            RETURN .F.
         ENDIF
      CASE " "
         IF CB == " "
            // lPassed := .T.
            EXIT
         ELSE
            RETURN .F.
         ENDIF
      OTHERWISE
         // lPassed := .T.
      ENDSWITCH
   NEXT

   RETURN .T. // lPassed

// from h_textbox.prg

STATIC FUNCTION GetValFromText( Text, mcvaltype )

   // eg. GetValFromText( "999,999.99" ) --> 999999.99
   LOCAL x, c, s

   SWITCH mcvaltype
   CASE "C"
      RETURN Text
   CASE "D"
      RETURN CToD( Text )
   ENDSWITCH

   // ASSUME numeric
   s := ""
   FOR x := 1 TO Len( Text )
      c := SubStr( Text, x, 1 )
      IF c $ "0123456789" .OR. c $ ".-"
         s += c
      ENDIF
   NEXT

   IF hb_LeftEq( AllTrim( Text ), "(" ) .OR. Right( AllTrim( Text ), 2 ) == "DB"
      s := "-" + s
   ENDIF

   // useless!
   // s := Transform( Val( s ), Getnummask( s_cmask, mcvaltype ) )

   RETURN Val( s )

// from h_textbox.prg

STATIC FUNCTION GetNumMask( Text, mcvaltype )

   // eg. GetNumMask( "999,999.99" ) --> "999999.99"
   LOCAL i, c, s

   IF mcvaltype $ "DC"
      RETURN Text
   ENDIF

   s := ""
   FOR i := 1 TO Len( Text )
      c := SubStr( Text, i, 1 )
      IF c $ "9."
         s += c
      ENDIF
      IF c $ "$*"
         s += "9"
      ENDIF
   NEXT

   RETURN s

// from tget.prg

STATIC FUNCTION IsBadDate( cBuffer )

   IF Empty( cBuffer )
      RETURN .F.
   ENDIF

   IF Empty( hb_StrReplace( cBuffer, "/-." ) )
      RETURN .F.
   ENDIF

   IF Empty( CToD( cBuffer ) )
      MyMessageBox( , "'" + cBuffer + "' is not a valid DATE" )
      RETURN .T.
   ENDIF

   RETURN .F.

/* --- WVW_INPUTFOCUS --- */

// this is a simple sample of WVW_INPUTFOCUS
// only handles WIN_WM_CHAR, thus not all input characters are accepted
FUNCTION WVW_INPUTFOCUS( nWinNum, hWnd, message, wParam, lParam )  /* must be a public function */

   LOCAL bhandler

   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )

   // did user perform a menu/toolbar action on Main Window?
#if 0
   IF message == WIN_WM_COMMAND .AND. nWinNum == 0  // menu,toolbar,pushbutton
      RETURN .F.
   ENDIF
#endif

   // now we handle input on other non-topmost windows

   DO CASE
   CASE message == WIN_WM_CHAR
      bhandler := inp_handler( nWinNum )
      IF HB_ISEVALITEM( bhandler )
         Eval( bhandler, nWinNum, wParam )
         RETURN .T.
      ELSE
         RETURN .F.
      ENDIF
   OTHERWISE
      // ignore
      RETURN .T.
   ENDCASE

   RETURN .F.

STATIC FUNCTION inp_handler( nwinnum, bhandler )

   STATIC s_bhandlers := {}

   LOCAL retval := iif( Len( s_bhandlers ) >= nwinnum + 1, s_bhandlers[ nwinnum + 1 ], NIL )

   IF HB_ISEVALITEM( bhandler )
      IF Len( s_bhandlers ) < nwinnum + 1
         ASize( s_bhandlers, nwinnum + 1 )
      ENDIF
      s_bhandlers[ nwinnum + 1 ] := bhandler
   ENDIF

   RETURN retval

/* --- general helpers --- */

STATIC FUNCTION MyMessageBox( nwinnum, cMessage, cCaption, nFlags )
   RETURN wapi_MessageBox( wvw_GetWindowHandle( nwinnum ), cMessage, hb_defaultValue( cCaption, "Debug Message" ), nFlags )

#define VK_SHIFT            16

STATIC FUNCTION lShiftPressed()
   RETURN wapi_GetKeyState( VK_SHIFT ) < 0
