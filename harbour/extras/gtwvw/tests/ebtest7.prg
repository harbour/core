/*
 * $Id$
 */

/*
  copyright (c) Budyanto Dj. <budyanto@centrin.net.id>

  Editbox demo
  multi-field GET session(s) using editboxes
  with a simple sample of @...EBGET command translation

  This sample also shows simple implementation of WVW_INPUTFOCUS
  to handle some keyboard inputs on non topmost window

  (The editbox itself always accept input independent of WVW_INPUTFOCUS)

  Some parts of this sample are modifications from MINIGUI's sourcecode
  to handle "masks" during editbox input session:
    ProcessCharMask()
    CharMaskTekstOK()
    GetValFromText()
    GetNumMask()

 */

#include "inkey.ch"
#include "set.ch"
#include "setcurs.ch"


#command @ <row>, <col> EBGET <var>                                       ;
      [LABEL <label>]                                   ;
      [<multiline: MULTILINE>]                        ;
      [PICTURE <pic>]                                 ;
      ;
      => AddEBGet( aEBGets, <row>, <col>, @<var>, <"var"> , {| x | <var> := x }, <label>, <.multiline.>, <pic> )

//***************************
// constants to aEBGets member,
// according to EBReadGets() convention
// NOTE: a smarter way would be to use CLASS instead of arrays
//***************************
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

//REQUEST HB_NOSTARTUPWINDOW

PROCEDURE Main()

   LOCAL nOpen, nClose
   LOCAL lClosepermitted := .F.
   LOCAL bSetKey := SetKey( K_F8, {|| MyHelp() } )

   SET CENTURY ON
   SET DATE ANSI
   SetMode( 4, 54 )   //a small window
   SetColor( "N/W" )
   Wvw_SetFont( 0, "Courier New", 16, - 7 )
   WVW_EBSetFont( 0, "Arial" )  //font for editbox
   WVW_PBSetFont( 0, "Arial" )  //font for pushbuttons

   Wvw_SetCodePage( 0, 255 )
   wvw_allownontopEvent( .T. )   //this will make pushbuttons to work
   //even on non-topmost window
   wvw_recurseCblock( .T. ) //this will allow recursed execution
   //of control's codeblocks
   //eg. multiple executions of pushbutton's codeblock
   //    invoking "GetSession()"

   SetCursor( SC_NONE ) //we don't need cursor

   CLS
   @ 0, 1 SAY "Click NEW to open a new GET session, CLOSE when done"
   nOpen := wvw_pbcreate( 0, 2, 1, 2, 10, "New", NIL, {|| GetSession() } )
   nClose := wvw_pbcreate( 0, 2, 12, 2, 22, "Close", NIL, {|| ToCloseWindow( 0, @lClosepermitted ) } )

   // activate/show the main window
   wvw_showwindow( 0 )

   // wait until user click the close button
   DO WHILE !lClosepermitted
      Inkey( 0.2 )
   ENDDO
   SetKey( K_F8, bSetKey )

   RETURN

// This is the sample of a GET session.
// This sample is reentrant, can be executed more than once if user clicks
// "NEW" button in the main window multiple times.
PROCEDURE GetSession()

   STATIC s_nsession := 0
   LOCAL aEBGets := {}
   LOCAL cName      := Space( 30 )
   LOCAL cNickName  := Space( 10 )
   LOCAL dBirthdate := SToD()
   LOCAL nBudget    := 125000
   LOCAL cRemark    := "Some notes" + hb_eol() + ;
      "about this person"
   LOCAL nwinnum
   LOCAL nrow1, ncol1, nrow2, ncol2
   LOCAL cdebugreport
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
      nrow1, ncol1, nrow2, ncol2, NIL, 0 )

   cRemark += hb_eol() + "(from Session " + hb_ntos( nwinnum ) + ")"

   @ 1, 15 ebGET cName      LABEL "Name:"
   @ 3, 15 ebGET cNickName  LABEL "Nickname:"   PICTURE Replicate( "!", Len( cNickName ) )
   @ 5, 15 ebGET dBirthDate LABEL "Birth Date:"
   @ 7, 15 ebGET nBudget    PICTURE "999,999.99"   //using default label
   @ 9, 15 ebGET cRemark    LABEL "Remarks:" MULTILINE
   EBReadGets( nwinnum, @aEBGets )   //READ

   // debugging text
   cdebugreport := "Back to GetSession() of window " + hb_ntos( nwinnum ) + " with these values returned:" + hb_eol()
   cdebugreport += "cName:" + cName + hb_eol()
   cdebugreport += "cNickName:" + cNickName + hb_eol()
   cdebugreport += "dBirthDate:" + DToC( dBirthDate ) + hb_eol()
   cdebugreport += "nBudget:" + transform( nBudget, "999,999.99" ) + hb_eol()
   cdebugreport += "cRemark:" + cRemark
   MyMessageBox( nwinnum, cdebugreport )

   wvw_lclosewindow()

   s_nsession--

   RETURN

FUNCTION MyHelp()

   LOCAL ccallstack, i

   ccallstack := ""
   FOR i := 0 TO 8
      ccallstack += hb_ntos( i ) + ". " + ProcName( i ) + "(" + hb_ntos( ProcLine( i ) ) + ")" + hb_eol()
   NEXT

   MyMessageBox( NIL, ;
      "Sorry, this is not really a help :-)" + hb_eol() + ;
      "It is only to show that SETKEY() codeblock is handled by our editboxes" + hb_eol() + ;
      "Call stack:" + hb_eol() + ;
      ccallstack )

   RETURN NIL

FUNCTION WVW_SETFOCUS( nWinNum, hWnd )

   IF nwinnum != 0
      wvw_nsetcurwindow( nwinnum )
   ENDIF

   RETURN NIL

/******************************************************
 Typical application ends here.
 Below are EBReadGets() and its supporters that are
 usable for general needs
 (can be put into separated module)
 ******************************************************/

// 20070525

// adding one EBGet variable into aEBGets array
// returns .T. if successful
FUNCTION AddEBGet( aEBGets, mnrow, mncol, mxValue, mcVarName, mbAssign, mcLabel, mlMultiline, mcPict )

   LOCAL mcVarType, mbText
   mcVarType := ValType( mxValue )
   DO CASE
   CASE mcVarType == "C"
      mcPict := iif( HB_ISSTRING( mcPict ), mcPict, replicate( "X", Len(mxValue ) ) )
      mbText := {|| mxValue }
   CASE mcVarType == "N"
      mcPict := iif( HB_ISSTRING( mcPict ), mcPict, "999,999,999.99" )
      mbText := {|| Transform( mxValue, mcPict ) }
   CASE mcVarType == "D"
      mcPict := iif( HB_ISSTRING( mcPict ), mcPict, "99/99/9999" )
      mbText := {|| DToC( mxValue ) }
   OTHERWISE
      // unsupported valtype
      RETURN .F.
   ENDCASE

   IF ! HB_ISARRAY( aEBGets )
      aEBGEts := {}
   ENDIF

   IF ! HB_ISLOGICAL( mlMultiline ) .OR. ;
      ! HB_ISSTRING( mxValue )
      mlMultiline := .F.
   ENDIF
   IF ! HB_ISSTRING( mcLabel )
      mcLabel := mcVarName + ":"
   ENDIF

   AAdd( aEBGets, { ;
      mlMultiline, ;    //__GET_LMULTILINE
      mcLabel, ;        //__GET_CLABEL
      mnrow, ;          //__GET_NROW
      mncol, ;          //__GET_NCOL
      mxValue, ;        //__GET_XINIT
      mcPict, ;         //__GET_CPICT
      mcVarType, ;      //__GET_CVALTYPE
      mbText, ;         //__GET_BTEXT
      mbAssign, ;       //__GET_BASSIGN
      NIL, ;            //__GET_NEBID
      .F. } )           //__GET_LFOCUSED

   RETURN .T.

// generic procedure to run aEBGets, array of editboxes
PROCEDURE EBReadGets( nwinnum, aEBGets )

   LOCAL nmaxrow, nmincol
   LOCAL i, nlen, lmultiline, clabel, ;
      nrow1, ncol1, nrow2, ncol2
   LOCAL creport, nOKbutton, nCancelbutton, nClosebutton, ldone := .F.
   LOCAL lclosePermitted := .F.
   LOCAL nNumGets := Len( aEBGets )
   LOCAL ch
   LOCAL nfocus, lchangefocus

   IF nNumGets == 0
      RETURN
   ENDIF

   wvw_nsetcurwindow( nwinnum )
   nmaxrow := 0
   nmincol := 99999
   FOR i := 1 TO nNumGets
      lmultiline := aEBGets[i][__GET_LMULTILINE]
      IF !lmultiline
         nlen := Len( aEBGets[i][__GET_CPICT] )
      ELSE
         nlen := 30
      ENDIF
      clabel := aEBGets[i][__GET_CLABEL]
      nrow1 := aEBGets[i][__GET_NROW]
      ncol1 := aEBGets[i][__GET_NCOL]
      nrow2 := iif( aEBGets[i][__GET_LMULTILINE], nrow1 + 3, nrow1 )
      ncol2 := ncol1 + nlen - 1

      @ nrow1, ncol1 - Len( clabel ) - 1 SAY clabel

      aEBGets[i][__GET_NEBID] := wvw_ebcreate( nwinnum, nrow1, ncol1, nrow2, ncol2, ;
         Transform( aEBGets[i][__GET_XINIT], aEBGets[i][__GET_CPICT] ), ;
         {| nWinNum, nId, nEvent | MaskEditBox( nWinNum, nId, nEvent, @aEBGets ) }, ;
         aEBGets[i][__GET_LMULTILINE], ;  //EBtype
      0, ;  //nmorestyle
      iif( lmultiline, NIL, nlen + 1 ), ; //nMaxChar
      NIL, NIL )

      nmaxrow := Max( nmaxrow, nrow2 )
      nmincol := Min( nmincol, ncol1 )
   NEXT
   nrow1 := nmaxrow + 2 //min(nmaxrow+2, maxrow())
   ncol1 := nmincol //min(nmincol, maxcol()-33)
   nOKbutton := wvw_pbcreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "OK", NIL, ;
      {|| SaveVar( nwinnum, @aEBGets, @lDone ), ;
      EndGets( nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton );
      } )

   ncol1 := ncol1 + 10 + 1
   nCancelbutton := wvw_pbcreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "Cancel", NIL, ;
      {|| CancelVar( nwinnum, @aEBGets, @lDone ), ;
      EndGets( nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton );
      } )

   ncol1 := ncol1 + 10 + 1
   nClosebutton := wvw_pbcreate( nwinnum, nrow1, ncol1, nrow1, ncol1 + 10 - 1, "Close", NIL, ;
      {|| ToCloseWindow( nwinnum, @lClosepermitted ) } )
   wvw_pbenable( nwinnum, nclosebutton, .F. )

   // register a keyhandler for WVW_INPFOCUS
   inp_handler( nwinnum, {| n, ch | InpKeyHandler( n, ch, aEBGets, nOKbutton, nCancelbutton ) } )

   i := 1
   wvw_ebsetfocus( nwinnum, aEBGets[1][__GET_NEBID] )
   nFocus := 1
   ch := Inkey( 0.5 )
   DO WHILE !lDone
      IF HB_ISBLOCK( SetKey( ch ) )
         Eval( SetKey( ch ) )
      ELSEIF ch != 0
         lchangefocus := .T.
         DO CASE
         CASE ch == K_TAB .OR. ch == K_DOWN .OR. ch == K_ENTER
            IF nFocus < ( nNumGets + 2 )  //incl buttons
               nFocus++
            ELSE
               nFocus := 1
            ENDIF
         CASE ch == K_SH_TAB .OR. ch == K_UP
            IF nFocus > 1
               nFocus--
            ELSE
               nFocus := nNumGets + 2
            ENDIF
         OTHERWISE
            lchangefocus := .F. //!wvw_ebisfocused(nwinnum, aEBGets[nFocus][__GET_NEBID])
         ENDCASE
         IF lchangefocus
            IF nFocus <= nNumGets
               wvw_ebsetfocus( nwinnum, aEBGets[nFocus][__GET_NEBID] )
            ELSEIF nFocus == nNumGets + 1
               wvw_pbsetfocus( nwinnum, nOKbutton )
            ELSEIF nFocus == nNumGets + 2
               wvw_pbsetfocus( nwinnum, nCancelbutton )
            ENDIF
         ENDIF
      ENDIF
      IF wvw_pbisfocused( nwinnum, nOKbutton )
         nFocus := nNumGets + 1
      ELSEIF wvw_pbisfocused( nwinnum, nCancelbutton )
         nFocus := nNumGets + 2
      ELSE
         nFocus := nFocused( aEBGets )
      ENDIF
      ch := Inkey( 0.5 )
   ENDDO

   // session ended (already ended by OK or Cancel)

   lClosepermitted := ( nwinnum == wvw_nnumwindows() - 1 )
   // wait until user click the close button
   DO WHILE !lClosepermitted
      Inkey( 0.5 )
   ENDDO

   RETURN //EBReadGets()

// inp_handler(nwinnum, bhandler)

STATIC PROCEDURE InpKeyHandler( nwinnum, ch, aEBGets, nOKbutton, nCancelbutton )

   LOCAL nNumGets := Len( aEBGets )
   LOCAL nFocus, lchangefocus

   IF HB_ISBLOCK( SetKey( ch ) )
      Eval( SetKey( ch ) )
      RETURN
   ELSEIF ch == 0
      RETURN
   ENDIF
   IF wvw_pbisfocused( nwinnum, nOKbutton )
      nFocus := nNumGets + 1
   ELSEIF wvw_pbisfocused( nwinnum, nCancelbutton )
      nFocus := nNumGets + 2
   ELSE
      nFocus := nFocused( aEBGets )
   ENDIF
   lchangefocus := .T.
   DO CASE
   CASE ch == K_TAB .AND. !lShiftPressed()
      IF nFocus < ( nNumGets + 2 )  //incl buttons
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
      IF nFocus <= nNumGets
         wvw_ebsetfocus( nwinnum, aEBGets[nFocus][__GET_NEBID] )
      ELSEIF nFocus == nNumGets + 1
         wvw_pbsetfocus( nwinnum, nOKbutton )
      ELSEIF nFocus == nNumGets + 2
         wvw_pbsetfocus( nwinnum, nCancelbutton )
      ENDIF
   ENDIF

   RETURN  //InpKeyHandler()

STATIC PROCEDURE EndGets( nwinnum, aEBGets, nOKbutton, nCancelbutton, nCloseButton )

   LOCAL i

   // session ended
   FOR i := 1 TO Len( aEBGets )
      wvw_ebenable( nwinnum, aEBGets[i][__GET_NEBID], .F. )
   NEXT
   wvw_pbenable( nwinnum, nOKbutton, .F. )
   wvw_pbenable( nwinnum, nCancelbutton, .F. )

   // clear the getlist
   ASize( aEBGets, 0 )

   // wait until user click the close button
   wvw_pbenable( nwinnum, nclosebutton, .T. )

   RETURN

// save values into variables
STATIC PROCEDURE SaveVar( nwinnum, aEBGets, lDone )

   LOCAL i, cdebugreport
   FOR i := 1 TO Len( aEBGets )
      // do some validation if necessary
      Eval( aEBGets[ i ][ __GET_BASSIGN ], ;
         GetValFromText( wvw_ebgettext( nwinnum, aEBGets[ i ][ __GET_NEBID ] ), aEBGets[ i ][ __GET_CVALTYPE ] ) )
   NEXT
   lDone := .T.

   // debugging text
   cdebugreport := "Get session in window " + hb_ntos( nwinnum ) + " is ended with confirmation" + hb_eol() + ;
      "Values have been assigned to the respective ebGET variables"
   MyMessageBox( nwinnum, cdebugreport )

   RETURN

// restore initial values into variables
STATIC PROCEDURE CancelVar( nwinnum, aEBGets, lDone )

   LOCAL i, cdebugreport
   FOR i := 1 TO Len( aEBGets )
      Eval( aEBGets[ i ][ __GET_BASSIGN ], ;
         aEBGets[ i ][ __GET_XINIT ] )
   NEXT
   lDone := .T.

   // debugging text
   cdebugreport := "Get session in window " + hb_ntos( nwinnum ) + " is ended with cancellation" + hb_eol() + ;
      "Values has been assigned to the respective initial ebGET variables"
   MyMessageBox( nwinnum, cdebugreport )

   RETURN

STATIC PROCEDURE ToCloseWindow( nwinnum, lPermitted )

   // allow to close topmost window only
   lPermitted := ( nwinnum == wvw_nnumwindows() - 1 )
   IF !lpermitted
      MyMessageBox( nwinnum, "Window " + hb_ntos( nwinnum ) + " is not allowed to be closed, yet" + hb_eol() + ;
         "Please close window " + hb_ntos( wvw_nnumwindows() - 1 ) + " first" )
   ENDIF

   RETURN

// returns index to aEBGets array containing editbox nEBid

STATIC FUNCTION nGetIndex( aEBGets, nEBId )

   RETURN AScan( aEBGets, {| x | x[__GET_NEBID] == nEBId } )

// returns index to aEBGets array containing editbox that is/was in focus

STATIC FUNCTION nFocused( aEBGets )

   RETURN AScan( aEBGets, {| x | x[__GET_LFOCUSED] == .T. } )

#define EN_SETFOCUS         0x0100

#define EN_KILLFOCUS        0x0200

#define EN_CHANGE           0x0300

#define EN_UPDATE           0x0400

#define EN_ERRSPACE         0x0500

#define EN_MAXTEXT          0x0501

#define EN_HSCROLL          0x0601

#define EN_VSCROLL          0x0602

#define WM_KEYFIRST                     0x0100

#define WM_KEYDOWN                      0x0100

#define WM_KEYUP                        0x0101

#define WM_CHAR                         0x0102

#define WM_DEADCHAR                     0x0103

#define WM_SYSKEYDOWN                   0x0104

#define WM_SYSKEYUP                     0x0105

#define WM_SYSCHAR                      0x0106

#define WM_SYSDEADCHAR                  0x0107

#define WM_KEYLAST                      0x0108

#define WM_INITDIALOG                   0x0110

#define WM_COMMAND                      0x0111

#define WM_SYSCOMMAND                   0x0112

#define WM_TIMER                        0x0113

#define WM_HSCROLL                      0x0114

#define WM_VSCROLL                      0x0115

#define WM_INITMENU                     0x0116

#define WM_INITMENUPOPUP                0x0117

#define WM_MENUSELECT                   0x011F

#define WM_MENUCHAR                     0x0120

#define WM_ENTERIDLE                    0x0121

// callback function called by GTWVW during some events on editbox
STATIC FUNCTION MaskEditBox( nWinNum, nId, nEvent, aEBGets )

   STATIC s_bBusy := .F.
   LOCAL ctext
   LOCAL nIndex := nGetIndex( aEBGets, nId )
   LOCAL mcvaltype, mcpict, mlmultiline
   LOCAL nwasfocus
   IF s_bBusy
      RETURN NIL
   ENDIF
   IF nIndex == 0
      RETURN NIL
   ENDIF
   s_bBusy := .T.
   mcvaltype := aEBGets[ nIndex ][ __GET_CVALTYPE ]
   mcpict := aEBGets[ nIndex ][ __GET_CPICT ]
   mlmultiline := aEBGets[ nIndex ][ __GET_LMULTILINE ]

   DO CASE
   CASE nEvent == EN_KILLFOCUS
      IF !mlmultiline .AND. mcvaltype $ "ND"
         ctext := wvw_ebgettext( nwinnum, nid )
         IF mcvaltype == "D" .AND. IsBadDate( ctext )
            // don't leave it in an invalid state
            wvw_ebsetfocus( nwinnum, nid )
         ELSE
            wvw_ebsettext( nwinnum, nId, Transform( GetValFromText( ctext, mcvaltype ), mcpict ) )
         ENDIF
      ENDIF
   CASE nEvent == EN_SETFOCUS
      IF !mlmultiline .AND. mcvaltype == "N"
         ctext := wvw_ebgettext( nwinnum, nid )
         wvw_ebsettext( nwinnum, nId, Transform( GetValFromText( ctext, mcvaltype ), GetNumMask( mcpict, mcvaltype ) ) )
      ENDIF
      wvw_ebsetsel( nwinnum, nid, 0, -1 )
      nwasFocus := nFocused( aEBGets )
      IF nwasFocus != 0
         aEBGets[ nwasFocus ][ __GET_LFOCUSED ] := .F.
      ENDIF
      aEBGets[ nIndex ][ __GET_LFOCUSED ] := .T.
   CASE nEvent == EN_CHANGE
      IF !mlmultiline
         ProcessCharMask( nwinnum, nId, mcvaltype, mcpict )
      ENDIF
   ENDCASE
   s_bBusy := .F.

   RETURN NIL

/************* borrowed and modified from minigui *************/

//from h_textbox.prg

STATIC PROCEDURE ProcessCharMask( mnwinnum, mnebid, mcvaltype, mcpict )

   LOCAL InBuffer, OutBuffer := "", icp, x, CB, CM, BadEntry := .F., InBufferLeft, InBufferRight, Mask, OldChar, BackInbuffer
   LOCAL pc := 0
   LOCAL fnb := 0
   LOCAL dc := 0
   LOCAL pFlag := .F.
   LOCAL ncp := 0
   LOCAL NegativeZero := .F.
   LOCAL Output := ''
   LOCAL ol := 0

   IF mcvaltype == "N"
      Mask := GetNumMask( mcpict, mcvaltype )
   ELSEIF mcvaltype == "D"
      Mask := mcpict //"99/99/9999"
   ELSE
      Mask := mcpict
   ENDIF

   // Store Initial CaretPos
   wvw_ebgetsel( mnwinnum, mnebid, NIL, @icp )

   // Get Current Content
   InBuffer := wvw_ebgettext( mnwinnum, mnebid )

   pc := 0 //x for clarity
   pFlag := .F. //x for clarity
   IF mcvaltype == "N"
      // RL 104
      IF Left( AllTrim( InBuffer ) , 1 ) == '-' .AND. Val( InBuffer ) == 0
         NegativeZero := .T.
      ENDIF

      IF PCount() > 1
         // Point Count For Numeric InputMask
         FOR x := 1 TO Len( InBuffer )
            CB := SubStr( InBuffer , x , 1 )
            IF CB == '.' .OR. CB == ','
               pc++
            ENDIF
         NEXT x

         // RL 89
         IF Left( InBuffer, 1 ) == '.' .OR. Left( InBuffer, 1 ) == ','
            pFlag := .T.
         ENDIF

         // Find First Non-Blank Position
         FOR x := 1 TO Len( InBuffer )
            CB := SubStr( InBuffer , x , 1 )
            IF !( CB == " " )
               fnb := x
               EXIT
            ENDIF
         NEXT x
      ENDIF //pcount()>1
   ENDIF

   BackInBuffer := InBuffer

   OldChar := SubStr( InBuffer , icp + 1 , 1 )

   IF Len( InBuffer ) < Len( Mask )
      InBufferLeft := Left( InBuffer , icp )

      InBufferRight := Right( InBuffer , Len( InBuffer ) - icp )

      IF CharMaskTekstOK( InBufferLeft + ' ' + InBufferRight, mcvaltype, Mask ) .AND. ;
            !CharMaskTekstOK( InBufferLeft + InBufferRight, mcvaltype, Mask )
         InBuffer := InBufferLeft + ' ' + InBufferRight
      ELSE
         InBuffer := InBufferLeft + InBufferRight
      ENDIF
   ENDIF

   IF Len( InBuffer ) > Len( Mask ) .AND. ;
         Len( Mask ) > 0

      InBufferLeft := Left( InBuffer , icp )

      InBufferRight := Right( InBuffer , Len( InBuffer ) - icp - 1 )

      InBuffer := InBufferLeft + InBufferRight
   ENDIF

   // Process Mask
   OutBuffer := "" //x for clarity
   BadEntry := .F. //x for clarity
   FOR x := 1 TO Len( Mask )
      CB := SubStr( InBuffer , x , 1 )
      CM := SubStr( Mask , x , 1 )

      DO CASE
      CASE ( CM ) == 'A' .OR. ( CM ) == '!'
         IF IsAlpha( CB ) .OR. CB == ' '
            IF ( CM ) == "!"
               OutBuffer := OutBuffer + Upper( CB )
            ELSE
               OutBuffer := OutBuffer + CB
            ENDIF
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer := OutBuffer + OldChar
            ELSE
               OutBuffer := OutBuffer + ' '
            ENDIF
         ENDIF

      CASE CM == '9'
         IF IsDigit( CB ) .OR. CB == ' ' .OR. ;
               ( mcvaltype == "N" .AND. ; //x
            CB == '-' .AND. x == fnb .AND. PCount() > 1 )

            OutBuffer := OutBuffer + CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer := OutBuffer + OldChar
            ELSE
               OutBuffer := OutBuffer + ' '
            ENDIF
         ENDIF

      CASE CM == ' '
         IF CB == ' '
            OutBuffer := OutBuffer + CB
         ELSE
            IF x == icp
               BadEntry := .T.
               OutBuffer := OutBuffer + OldChar
            ELSE
               OutBuffer := OutBuffer + ' '
            ENDIF
         ENDIF

         //x
      CASE CM == 'X'
         OutBuffer := OutBuffer + CB

      OTHERWISE
         OutBuffer := OutBuffer + CM

      ENDCASE
   NEXT x

   // Replace Content
   IF ! ( BackInBuffer == OutBuffer )
      wvw_ebsettext( mnwinnum, mnebid, OutBuffer )
   ENDIF

   IF pc > 1
      //pc>1 means we must to JUMP to the decimal point

      // RL 104
      IF NegativeZero == .T.
         Output := Transform( GetValFromText( wvw_ebgettext(mnwinnum, mnebid ) , mcvaltype ) , Mask )

         //x better:
         ol := Len( Output )
         Output := PadL( "-" + SubStr( Output, At('.',OutBuffer ) - 1 ), ol )

         // Replace Text
         wvw_ebsettext( mnwinnum, mnebid, Output )
         wvw_ebsetsel( mnwinnum, mnebid, At( '.',OutBuffer ) + dc, At( '.',OutBuffer ) + dc )
      ELSE
         wvw_ebsettext( mnwinnum, mnebid, Transform( GetValFromText( wvw_ebgettext(mnwinnum, mnebid ) , mcvaltype ) , Mask ) )
         wvw_ebsetsel( mnwinnum, mnebid, At( '.',OutBuffer ) + dc, At( '.',OutBuffer ) + dc )
      ENDIF

   ELSE
      IF pFlag == .T.
         ncp := At( '.' , wvw_ebgettext( mnwinnum, mnebid ) )
         wvw_ebsetsel( mnwinnum, mnebid, ncp, ncp )
      ELSE
         // Restore Initial CaretPos
         IF BadEntry
            icp--
         ENDIF

         wvw_ebsetsel( mnwinnum, mnebid, icp, icp )

         // Skip Protected Characters
         FOR x := 1 TO Len( OutBuffer )
            CB := SubStr( OutBuffer , icp + x , 1 )
            CM := SubStr( Mask , icp + x , 1 )

            IF !IsDigit( CB ) .AND. !IsAlpha( CB ) .AND. ;
                  ( !( CB == ' ' ) .OR. ( CB == ' ' .AND. CM == ' ' ) )
               wvw_ebsetsel( mnwinnum, mnebid, icp + x, icp + x )
            ELSE
               EXIT
            ENDIF
         NEXT x
      ENDIF
   ENDIF

   RETURN

//from h_textbox.prg

STATIC FUNCTION CharMaskTekstOK( cString, cvaltype, cMask )

   LOCAL lPassed := .T. , CB, CM, x

//x BEGIN
   IF cvaltype == "D"
      FOR x := 1 TO Min( Len( cString ), Len( cMask ) )
         CB := SubStr( cString , x , 1 )
         CM := SubStr( cMask , x , 1 )
         DO CASE
         CASE CM == '9'
            IF IsDigit( CB ) .OR. CB == ' '
               // lPassed:=.T.
            ELSE
               RETURN .F.
            ENDIF
         OTHERWISE
            // lPassed:=.T.
         ENDCASE
      NEXT i
      RETURN .T.
   ENDIF
//x END

   FOR x := 1 TO Min( Len( cString ), Len( cMask ) )
      CB := SubStr( cString , x , 1 )
      CM := SubStr( cMask , x , 1 )
      DO CASE
         // JK
      CASE ( CM ) == 'A' .OR. ( CM ) == '!'
         IF IsAlpha( CB ) .OR. CB == ' '
            // lPassed:=.T.
         ELSE
            RETURN .F.
         ENDIF
      CASE CM == '9'
         IF IsDigit( CB ) .OR. CB == ' '
            // lPassed:=.T.
         ELSE
            RETURN .F.
         ENDIF
      CASE CM == ' '
         IF CB == ' '
            // lPassed:=.T.
         ELSE
            RETURN .F.
         ENDIF
      OTHERWISE
         // lPassed:=.T.
      ENDCASE
   NEXT i

   RETURN .T. //lPassed

//from h_textbox.prg

STATIC FUNCTION GetValFromText( Text , mcvaltype )

   // eg. GetValFromText( "999,999.99" ) --> 999999.99
   LOCAL x , c , s

//x BEGIN
   IF mcvaltype == "C"
      RETURN TEXT
   ENDIF

   IF mcvaltype == "D"
      s := CToD( Text )
      RETURN s
   ENDIF
//x END

   // ASSUME numeric
   s := ''
   FOR x := 1 TO Len( Text )
      c := SubStr( Text, x, 1 )
      //If c='0' .or. c='1' .or. c='2' .or. c='3' .or. c='4' .or. c='5' .or. c='6' .or. c='7' .or. c='8' .or. c='9' .or. c='.' .or. c='-'
      IF c $ '0123456789' .OR. c $ '.-'
         s := s + c
      ENDIF
   NEXT x

   IF Left( AllTrim( Text ) , 1 ) == '(' .OR.  Right( AllTrim( Text ) , 2 ) == 'DB'
      s := '-' + s
   ENDIF

   //useless!
// s := Transform( Val( s ) , Getnummask( s_cmask, mcvaltype ) )

   RETURN Val( s )

//from h_textbox.prg

STATIC FUNCTION GetNumMask( Text, mcvaltype )

   // eg. GetNumMask("999,999.99") --> "999999.99"
   LOCAL i , c , s
//x BEGIN
   IF mcvaltype == "D" .OR. mcvaltype == "C"
      s := Text
      RETURN s
   ENDIF
//x END

   s := ''
   FOR i := 1 TO Len( Text )
      c := SubStr( Text, i, 1 )
      IF c == '9' .OR. c == '.'
         s := s + c
      ENDIF
      IF c == '$' .OR. c == '*'
         s := s + '9'
      ENDIF
   NEXT i

   RETURN s

//from TGET.PRG

STATIC FUNCTION IsBadDate( cBuffer ) //, cPicFunc )

   LOCAL cBuffer2

   IF Empty( cBuffer )
      RETURN .F.
   ENDIF

// IF "E" $ cPicFunc
//    cBuffer := InvertDwM( cBuffer )
// ENDIF

   cBuffer2 := StrTran( cBuffer, "/" )
   cBuffer2 := StrTran( cBuffer2, "-" )
   cBuffer2 := StrTran( cBuffer2, "." )

   IF Empty( cBuffer2 )
      RETURN .F.
   ENDIF

   IF Empty( CToD( cBuffer ) )
      MyMessageBox( NIL, "'" + cBuffer + "' is not a valid DATE" )
      RETURN .T.
   ENDIF

   RETURN .F.

/************ WVW_INPUTFOCUS ************/

// this is a simple sample of WVW_INPUTFOCUS
// only handles WM_CHAR, thus not all input characters are accepted
FUNCTION WVW_INPUTFOCUS( nWinNum, hWnd, message, wParam, lParam )

   LOCAL wParamLow := WVW_LOWORD( wParam )
   LOCAL wParamHi := WVW_HIWORD( wParam )
   LOCAL nCommand, ch
   LOCAL bhandler

   // did user perform a menu/toolbar action on Main Window?
// IF message == WM_COMMAND .AND. nWinNum == 0  //menu,toolbar,pushbutton
//   RETURN .F.
// ENDIF

   // now we handle input on other non-topmost windows

   DO CASE
   CASE message == WM_CHAR
      ch := wParam
      bhandler := inp_handler( nWinNum )
      IF HB_ISBLOCK( bhandler )
         Eval( bhandler, nWinNum, ch )
         RETURN .T.
      ELSE
         RETURN .F.
      ENDIF
   OTHERWISE
      // ignore
      RETURN .T.
   ENDCASE

   RETURN .F. //WVW_INPUTFOCUS()

FUNCTION inp_handler( nwinnum, bhandler )

   STATIC s_bhandlers := {}
   LOCAL i
   LOCAL retval := iif( Len( s_bhandlers ) >= nwinnum + 1, s_bhandlers[nwinnum+1], NIL )

   IF HB_ISBLOCK( bhandler )
      IF Len( s_bhandlers ) < nwinnum + 1
         ASize( s_bhandlers, nwinnum + 1 )
      ENDIF
      s_bhandlers[ nwinnum + 1 ] := bhandler
   ENDIF

   RETURN retval

/********** general helpers ************/

STATIC FUNCTION MyMessageBox( nwinnum, cMessage, cCaption, nFlags )

   LOCAL nParent

   hb_default( @cCaption, "Debug Message" )
   nParent := wvw_getwindowhandle( nwinnum )

   RETURN win_messagebox( nParent, cMessage, cCaption, nFlags )

STATIC FUNCTION lShiftPressed()

// #define VK_SHIFT            16

   RETURN wvw_GETKEYSTATE( 16 ) < 0
