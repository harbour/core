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
#include "common.ch"


#command @ <row>, <col> EBGET <var>                                       ;
                        [LABEL <label>]                                   ;
                        [<multiline: MULTILINE>]                        ;
                        [PICTURE <pic>]                                 ;
                                                                        ;
      => AddEBGet(aEBGets, <row>, <col>, @<var>, <"var">, {|x| <var> := x}, <label>, <.multiline.>, <pic>)

****************************
* constants to aEBGets member,
* according to EBReadGets() convention
* NOTE: a smarter way would be to use CLASS instead of arrays
****************************
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

proc main
local nOpen, nClose
local lClosepermitted := .f.
local bSetKey := SETKEY(K_F8, {|| MyHelp()})
   SET CENTURY ON
   SET DATE BRITISH
   setmode(4,54)   //a small window
   setcolor("N/W")
   Wvw_SetFont(0, "Courier New",16,-7)
   WVW_EBSetFont(0, "Arial")  //font for editbox
   WVW_PBSetFont(0, "Arial")  //font for pushbuttons

   Wvw_SetCodePage(0, 255)
   wvw_allownontopEvent(.t.)   //this will make pushbuttons to work
                               //even on non-topmost window
   wvw_recurseCblock(.t.) //this will allow recursed execution
                          //of control's codeblocks
                          //eg. multiple executions of pushbutton's codeblock
                          //    invoking "GetSession()"

   setcursor(0) //we don't need cursor

   CLS
   @ 0,1 say "Click NEW to open a new GET session, CLOSE when done"
   nOpen := wvw_pbcreate(0, 2, 1, 2, 10, "New", NIL, {|| GetSession()})
   nClose:= wvw_pbcreate(0, 2, 12,2, 22, "Close", NIL, {|| ToCloseWindow(0, @lClosepermitted)})

   * activate/show the main window
   wvw_showwindow(0)

   * wait until user click the close button
   do while !lClosepermitted
      inkey(0.2)
   enddo
   SETKEY(K_F8, bSetKey)
return  //main

procedure GetSession()
* This is the sample of a GET session.
* This sample is reentrant, can be executed more than once if user clicks
* "NEW" button in the main window multiple times.
static snsession := 0
local aEBGets := {}
local cName      := space(30)
local cNickName  := space(10)
local dBirthdate := ctod("")
local nBudget    := 125000
local cRemark    := "Some notes" + chr(13)+chr(10) +;
                    "about this person"
local nwinnum
local nrow1, ncol1, nrow2, ncol2
local cdebugreport
   if snsession>15
      MyMessageBox(0, "Sorry, maximum number of sessions reached")
      return
   endif

   snsession++
   nwinnum := snsession

   nrow1 := 4+(snsession-1)*1
   ncol1 := 10+(snsession-1)*2
   nrow2 := nrow1+15
   ncol2 := ncol1+60
   wvw_nOpenWindow("Session "+alltrim(str(snsession)) + " (press F8 for help)",;
                   nrow1,ncol1,nrow2,ncol2,NIL,0)

   cRemark += chr(13)+chr(10)+"(from Session " + alltrim(str(nwinnum)) + ")"

   @ 1,15 ebGET cName      LABEL "Name:"
   @ 3,15 ebGET cNickName  LABEL "Nickname:"   PICTURE repl("!",len(cNickName))
   @ 5,15 ebGET dBirthDate LABEL "Birth Date:"
   @ 7,15 ebGET nBudget    PICTURE "999,999.99"   //using default label
   @ 9,15 ebGET cRemark    LABEL "Remarks:" MULTILINE
   EBReadGets(nwinnum, @aEBGets)   //READ

   * debugging text
   cdebugreport := "Back to GetSession() of window "+alltrim(str(nwinnum))+" with these values returned:"+ chr(13)+chr(10)
   cdebugreport += "cName:"+cName+ chr(13)+chr(10)
   cdebugreport += "cNickName:"+cNickName+ chr(13)+chr(10)
   cdebugreport += "dBirthDate:"+dtoc(dBirthDate)+ chr(13)+chr(10)
   cdebugreport += "nBudget:"+tran(nBudget,"999,999.99")+ chr(13)+chr(10)
   cdebugreport += "cRemark:"+cRemark
   MyMessageBox(nwinnum, cdebugreport)

   wvw_lclosewindow()

   snsession--
return

function MyHelp()
local ccallstack,i
   ccallstack := ""
   for i := 0 to 8
      ccallstack += alltrim(str(i)) + ". " + procname(i) + "(" + alltrim(str(procline(i))) + ")" + chr(13)+chr(10)
   next

   MyMessageBox(NIL,;
                "Sorry, this is not really a help :-)" + chr(13)+chr(10)+;
                "It is only to show that SETKEY() codeblock is handled by our editboxes" + chr(13)+chr(10)+;
                "Call stack:"+ chr(13)+chr(10)+;
                ccallstack )
return NIL

function WVW_SETFOCUS(nWinNum, hWnd)
   if nwinnum!=0
      wvw_nsetcurwindow(nwinnum)
   endif
return NIL

/******************************************************
 Typical application ends here.
 Below are EBReadGets() and its supporters that are
 usable for general needs
 (can be put into separated module)
 ******************************************************/


* 20070525
function AddEBGet(aEBGets, mnrow, mncol, mxValue, mcVarName, mbAssign, mcLabel, mlMultiline, mcPict)
* adding one EBGet variable into aEBGets array
* returns .t. if successful
local mcVarType, mbText
   mcVarType := valtype(mxValue)
   do case
      case mcVarType=="C"
         mcPict := iif(valtype(mcPict)=="C",mcPict,repl("X", len(mxValue)))
         mbText := {||mxValue}
      case mcVarType=="N"
         mcPict := iif(valtype(mcPict)=="C",mcPict,"999,999,999.99")
         mbText := {||trans(mxValue, mcPict)}
      case mcVarType=="D"
         mcPict := iif(valtype(mcPict)=="C",mcPict,"99/99/9999")
         mbText := {||dtoc(mxValue)}
      otherwise
         * unsupported valtype
         return .f.
   endcase

   if !(valtype(aEBGets)=="A")
      aEBGEts := {}
   endif

   if !(valtype(mlMultiline)=="L") .or.;
      !(valtype(mxValue)=="C")
      mlMultiline := .f.
   endif
   if !(valtype(mcLabel)=="C")
      mcLabel := mcVarName + ":"
   endif

   aadd(aEBGets, { mlMultiline, ;   //__GET_LMULTILINE
                   mcLabel,;        //__GET_CLABEL
                   mnrow,;          //__GET_NROW
                   mncol, ;         //__GET_NCOL
                   mxValue,;        //__GET_XINIT
                   mcPict,;         //__GET_CPICT
                   mcVarType,;      //__GET_CVALTYPE
                   mbText,;         //__GET_BTEXT
                   mbAssign,;       //__GET_BASSIGN
                   NIL,;            //__GET_NEBID
                   .f. })           //__GET_LFOCUSED

return .t.

procedure EBReadGets(nwinnum, aEBGets)
* generic procedure to run aEBGets, array of editboxes
local nmaxrow, nmincol
local i, nlen, lmultiline, clabel, ;
      nrow1,ncol1,nrow2,ncol2
local creport, nOKbutton, nCancelbutton, nClosebutton, ldone := .f.
local lclosePermitted := .f.
local nNumGets := len(aEBGets)
local ch
local nfocus, lchangefocus

   if nNumGets==0
      return
   endif

   wvw_nsetcurwindow(nwinnum)
   nmaxrow := 0
   nmincol := 99999
   for i := 1 to nNumGets
      lmultiline := aEBGets[i][__GET_LMULTILINE]
      if !lmultiline
         nlen := len(aEBGets[i][__GET_CPICT])
      else
         nlen := 30
      endif
      clabel := aEBGets[i][__GET_CLABEL]
      nrow1 := aEBGets[i][__GET_NROW]
      ncol1 := aEBGets[i][__GET_NCOL]
      nrow2 := iif(aEBGets[i][__GET_LMULTILINE],nrow1+3,nrow1)
      ncol2 := ncol1+nlen-1

      @ nrow1, ncol1-len(clabel)-1 say clabel

      aEBGets[i][__GET_NEBID] := wvw_ebcreate(nwinnum, nrow1, ncol1, nrow2, ncol2, ;
                                     trans(aEBGets[i][__GET_XINIT],aEBGets[i][__GET_CPICT]), ;
                                     {|nWinNum,nId,nEvent| MaskEditBox(nWinNum,nId,nEvent,@aEBGets) }, ;
                                     aEBGets[i][__GET_LMULTILINE], ;  //EBtype
                                     0, ;  //nmorestyle
                                     iif(lmultiline,NIL,nlen+1), ; //nMaxChar
                                     NIL, NIL)

      nmaxrow := max(nmaxrow, nrow2)
      nmincol := min(nmincol, ncol1)
   next
   nrow1 := nmaxrow+2 //min(nmaxrow+2, maxrow())
   ncol1 := nmincol //min(nmincol, maxcol()-33)
   nOKbutton := wvw_pbcreate(nwinnum, nrow1, ncol1, nrow1, ncol1+10-1, "OK", NIL, ;
                             {|| SaveVar(nwinnum, @aEBGets, @lDone),;
                                 EndGets(nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton);
                             })

   ncol1 := ncol1+10+1
   nCancelbutton := wvw_pbcreate(nwinnum, nrow1, ncol1, nrow1, ncol1+10-1, "Cancel", NIL, ;
                             {|| CancelVar(nwinnum, @aEBGets, @lDone),;
                                 EndGets(nwinnum, @aEBGets, nOKbutton, nCancelbutton, nCloseButton);
                             })

   ncol1 := ncol1+10+1
   nClosebutton := wvw_pbcreate(nwinnum, nrow1, ncol1, nrow1, ncol1+10-1, "Close", NIL, ;
                             {|| ToCloseWindow(nwinnum, @lClosepermitted)})
   wvw_pbenable(nwinnum, nclosebutton, .f.)

   * register a keyhandler for WVW_INPFOCUS
   inp_handler(nwinnum, {|n,ch| InpKeyHandler(n, ch, aEBGets, nOKbutton, nCancelbutton)})

   i := 1
   wvw_ebsetfocus(nwinnum, aEBGets[1][__GET_NEBID])
   nFocus := 1
   ch := inkey(0.5)
   do while !lDone
      if valtype(setkey(ch))=="B"
         eval(setkey(ch))
      elseif ch!=0
         lchangefocus := .t.
         do case
            case ch==K_TAB .or. ch==K_DOWN .or. ch==K_ENTER
               if nFocus<(nNumGets+2)  //incl buttons
                  nFocus++
               else
                  nFocus:=1
               endif
            case ch==K_SH_TAB .or. ch==K_UP
               if nFocus>1
                  nFocus--
               else
                  nFocus := nNumGets+2
               endif
            otherwise
               lchangefocus := .f. //!wvw_ebisfocused(nwinnum, aEBGets[nFocus][__GET_NEBID])
         endcase
         if lchangefocus
            if nFocus<=nNumGets
               wvw_ebsetfocus(nwinnum, aEBGets[nFocus][__GET_NEBID])
            elseif nFocus==nNumGets+1
               wvw_pbsetfocus(nwinnum, nOKbutton)
            elseif nFocus==nNumGets+2
               wvw_pbsetfocus(nwinnum, nCancelbutton)
            endif
         endif
      endif
      if wvw_pbisfocused(nwinnum, nOKbutton)
         nFocus := nNumGets+1
      elseif wvw_pbisfocused(nwinnum, nCancelbutton)
         nFocus := nNumGets+2
      else
         nFocus := nFocused(aEBGets)
      endif
      ch := inkey(0.5)
   enddo

   * session ended (already ended by OK or Cancel)

   lClosepermitted := (nwinnum==wvw_nnumwindows()-1)
   * wait until user click the close button
   do while !lClosepermitted
      inkey(0.5)
   enddo
return //EBReadGets()

* inp_handler(nwinnum, bhandler)
static procedure InpKeyHandler(nwinnum, ch, aEBGets, nOKbutton, nCancelbutton)
local nNumGets := len(aEBGets)
local nFocus, lchangefocus
   if valtype(setkey(ch))=="B"
      eval(setkey(ch))
      return
   elseif ch==0
      return
   endif
   if wvw_pbisfocused(nwinnum, nOKbutton)
      nFocus := nNumGets+1
   elseif wvw_pbisfocused(nwinnum, nCancelbutton)
      nFocus := nNumGets+2
   else
      nFocus := nFocused(aEBGets)
   endif
   lchangefocus := .t.
   do case
      case ch==K_TAB .and. !lShiftPressed()
         if nFocus<(nNumGets+2)  //incl buttons
            nFocus++
         else
            nFocus:=1
         endif
      case ch==K_TAB .and. lShiftPressed()
         if nFocus>1
            nFocus--
         else
            nFocus := nNumGets+2
         endif
      otherwise
         lchangefocus := .f.
   endcase
   if lchangefocus
      if nFocus<=nNumGets
         wvw_ebsetfocus(nwinnum, aEBGets[nFocus][__GET_NEBID])
      elseif nFocus==nNumGets+1
         wvw_pbsetfocus(nwinnum, nOKbutton)
      elseif nFocus==nNumGets+2
         wvw_pbsetfocus(nwinnum, nCancelbutton)
      endif
   endif
return  //InpKeyHandler()

static procedure EndGets(nwinnum, aEBGets, nOKbutton, nCancelbutton, nCloseButton)
local i
   * session ended
   for i := 1 to len(aEBGets)
      wvw_ebenable(nwinnum, aEBGets[i][__GET_NEBID], .f.)
   next
   wvw_pbenable(nwinnum, nOKbutton, .f.)
   wvw_pbenable(nwinnum, nCancelbutton, .f.)

   * clear the getlist
   asize(aEBGets,0)

   * wait until user click the close button
   wvw_pbenable(nwinnum, nclosebutton, .t.)
return

static procedure SaveVar(nwinnum, aEBGets, lDone)
* save values into variables
local i, cdebugreport
   for i := 1 to len(aEBGets)
      * do some validation if necessary
      eval(aEBGets[i][__GET_BASSIGN], ;
           GetValFromText(wvw_ebgettext(nwinnum, aEBGets[i][__GET_NEBID]), aEBGets[i][__GET_CVALTYPE]))
   next
   lDone := .t.

   * debugging text
   cdebugreport := "Get session in window "+alltrim(str(nwinnum))+" is ended with confirmation"+ chr(13)+chr(10)+;
                   "Values have been assigned to the respective ebGET variables"
   MyMessageBox(nwinnum, cdebugreport)
return

static procedure CancelVar(nwinnum, aEBGets, lDone)
* restore initial values into variables
local i, cdebugreport
   for i := 1 to len(aEBGets)
      eval(aEBGets[i][__GET_BASSIGN], ;
           aEBGets[i][__GET_XINIT])
   next
   lDone := .t.

   * debugging text
   cdebugreport := "Get session in window "+alltrim(str(nwinnum))+" is ended with cancellation"+ chr(13)+chr(10)+;
                   "Values has been assigned to the respective initial ebGET variables"
   MyMessageBox(nwinnum, cdebugreport)
return

static procedure ToCloseWindow(nwinnum, lPermitted)
* allow to close topmost window only
   lPermitted := (nwinnum==wvw_nnumwindows()-1)
   if !lpermitted
      MyMessageBox(nwinnum, "Window " + alltrim(str(nwinnum)) + " is not allowed to be closed, yet" + chr(13)+chr(10) +;
                            "Please close window " + alltrim(str(wvw_nnumwindows()-1)) + " first" )
   endif
return

static function nGetIndex(aEBGets, nEBId)
* returns index to aEBGets array containing editbox nEBid
return ascan(aEBGets, {|x|x[__GET_NEBID]==nEBId})

static function nFocused(aEBGets)
* returns index to aEBGets array containing editbox that is/was in focus
return ascan(aEBGets, {|x|x[__GET_LFOCUSED]==.t.})



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

static function MaskEditBox(nWinNum,nId,nEvent,aEBGets)
* callback function called by GTWVW during some events on editbox
static bBusy := .f.
local ctext
local nIndex := nGetIndex(aEBGets, nId)
local mcvaltype, mcpict, mlmultiline
local nwasfocus
  if bBusy
     return NIL
  endif
  if nIndex==0
     return NIL
  endif
  bBusy := .t.
  mcvaltype := aEBGets[nIndex][__GET_CVALTYPE]
  mcpict := aEBGets[nIndex][__GET_CPICT]
  mlmultiline := aEBGets[nIndex][__GET_LMULTILINE]

  do case
     case nEvent==EN_KILLFOCUS
        if !mlmultiline .and. mcvaltype $ "ND"
           ctext := wvw_ebgettext(nwinnum, nid)
           if mcvaltype=="D" .and. IsBadDate(ctext)
              * don't leave it in an invalid state
              wvw_ebsetfocus(nwinnum, nid)
           else
              wvw_ebsettext(nwinnum, nId, transform(GetValFromText(ctext,mcvaltype), mcpict))
           endif
        endif
     case nEvent==EN_SETFOCUS
        if !mlmultiline .and. mcvaltype=="N"
           ctext := wvw_ebgettext(nwinnum, nid)
           wvw_ebsettext(nwinnum, nId, transform(GetValFromText(ctext,mcvaltype), GetNumMask(mcpict, mcvaltype)))
        endif
        wvw_ebsetsel(nwinnum, nid, 0, -1)
        nwasFocus := nFocused(aEBGets)
        if nwasFocus!=0
           aEBGets[nwasFocus][__GET_LFOCUSED] := .f.
        endif
        aEBGets[nIndex][__GET_LFOCUSED] := .t.
     case nEvent==EN_CHANGE
        if !mlmultiline
           ProcessCharMask(nwinnum, nId, mcvaltype, mcpict)
        endif
  endcase
  bBusy := .f.
return NIL

/************* borrowed and modified from minigui *************/

//from h_textbox.prg
*------------------------------------------------------------------------------*
static PROCEDURE ProcessCharMask ( mnwinnum, mnebid, mcvaltype, mcpict )
*------------------------------------------------------------------------------*
Local InBuffer , OutBuffer := '' , icp , x , CB , CM , BadEntry := .F. , InBufferLeft , InBufferRight , Mask , OldChar , BackInbuffer
Local pc := 0
Local fnb := 0
Local dc := 0
Local pFlag := .F.
Local ncp := 0
Local NegativeZero := .F.
Local Output := ''
Local ol := 0

   if mcvaltype=="N"
      Mask := GetNumMask(mcpict, mcvaltype)
   elseif mcvaltype=="D"
      Mask := mcpict //"99/99/9999"
   else
      Mask := mcpict
   endif

   // Store Initial CaretPos
   wvw_ebgetsel(mnwinnum, mnebid, NIL, @icp)

   // Get Current Content
   InBuffer := wvw_ebgettext(mnwinnum, mnebid)

   pc := 0 //x for clarity
   pFlag := .f. //x for clarity
   if mcvaltype=="N"
      // RL 104
      If Left ( AllTrim(InBuffer) , 1 ) == '-' .And. Val(InBuffer) == 0
         NegativeZero := .T.
      EndIf

      If Pcount() > 1
         // Point Count For Numeric InputMask
         For x := 1 To Len ( InBuffer )
            CB := SubStr (InBuffer , x , 1 )
            If CB == '.' .or. CB == ','
                 pc++
            EndIf
         Next x

         // RL 89
         If left (InBuffer,1) == '.' .or. left (InBuffer,1) == ','
            pFlag := .T.
         EndIf

         // Find First Non-Blank Position
         For x := 1 To Len ( InBuffer )
            CB := SubStr (InBuffer , x , 1 )
            If CB != ' '
               fnb := x
               Exit
            EndIf
         Next x
      EndIf //pcount()>1
   endif

   BackInBuffer := InBuffer

   OldChar := SubStr ( InBuffer , icp+1 , 1 )

   If Len ( InBuffer ) < Len ( Mask )
      InBufferLeft := Left ( InBuffer , icp )

      InBufferRight := Right ( InBuffer , Len (InBuffer) - icp )

      if CharMaskTekstOK(InBufferLeft + ' ' + InBufferRight,mcvaltype, Mask) .and. ;
         !CharMaskTekstOK(InBufferLeft + InBufferRight,mcvaltype,Mask)
         InBuffer := InBufferLeft + ' ' + InBufferRight
      else
         InBuffer := InBufferLeft +InBufferRight
      endif
   EndIf

   If Len ( InBuffer ) > Len ( Mask ) .and.;
      len(Mask)>0

      InBufferLeft := Left ( InBuffer , icp )

      InBufferRight := Right ( InBuffer , Len (InBuffer) - icp - 1 )

      InBuffer := InBufferLeft + InBufferRight
   EndIf

   // Process Mask
   OutBuffer := "" //x for clarity
   BadEntry := .F. //x for clarity
   For x := 1 To Len (Mask)
      CB := SubStr (InBuffer , x , 1 )
      CM := SubStr (Mask , x , 1 )

      Do Case
         Case (CM) == 'A' .or. (CM) == '!'
            If IsAlpha ( CB ) .Or. CB == ' '
               if (CM)=="!"
                  OutBuffer := OutBuffer + UPPER(CB)
               else
                  OutBuffer := OutBuffer + CB
               endif
            Else
               if x == icp
                  BadEntry := .T.
                  OutBuffer := OutBuffer + OldChar
               Else
                  OutBuffer := OutBuffer + ' '
               EndIf
            EndIf

         Case CM == '9'
            If IsDigit ( CB ) .Or. CB == ' ' .Or.;
               ( mcvaltype=="N" .and.; //x
                 CB == '-' .And. x == fnb .And. Pcount() > 1 )

               OutBuffer := OutBuffer + CB
            Else
               if x == icp
                  BadEntry := .T.
                  OutBuffer := OutBuffer + OldChar
               Else
                  OutBuffer := OutBuffer + ' '
               EndIf
            EndIf

         Case CM == ' '
            If CB == ' '
               OutBuffer := OutBuffer + CB
            Else
               if x == icp
                  BadEntry := .T.
                  OutBuffer := OutBuffer + OldChar
               Else
                  OutBuffer := OutBuffer + ' '
               EndIf
            EndIf

         //x
         Case CM == 'X'
            OutBuffer := OutBuffer + CB

         OtherWise
            OutBuffer := OutBuffer + CM

      End Case
   Next x

   // Replace Content
   If ! ( BackInBuffer == OutBuffer )
      wvw_ebsettext(mnwinnum, mnebid, OutBuffer)
   EndIf

   If pc > 1
      //pc>1 means we must to JUMP to the decimal point

      // RL 104
      If NegativeZero == .T.
         Output := Transform ( GetValFromText ( wvw_ebgettext(mnwinnum, mnebid) , mcvaltype ) , Mask )

         //x better:
         ol := len(Output)
         Output := padl("-"+substr(Output, at('.',OutBuffer)-1), ol)

         // Replace Text
         wvw_ebsettext(mnwinnum, mnebid, Output)
         wvw_ebsetsel(mnwinnum, mnebid, at('.',OutBuffer) + dc, at('.',OutBuffer) + dc)
      Else
         wvw_ebsettext(mnwinnum, mnebid, Transform ( GetValFromText ( wvw_ebgettext(mnwinnum, mnebid) , mcvaltype ) , Mask ))
         wvw_ebsetsel(mnwinnum, mnebid, at('.',OutBuffer) + dc, at('.',OutBuffer) + dc)
      EndIf

   Else
      If pFlag == .T.
         ncp := at ( '.' , wvw_ebgettext(mnwinnum, mnebid) )
         wvw_ebsetsel(mnwinnum, mnebid, ncp, ncp)
      Else
         // Restore Initial CaretPos
         If BadEntry
            icp--
         EndIf

         wvw_ebsetsel(mnwinnum, mnebid, icp, icp)

         // Skip Protected Characters
         For x := 1 To Len (OutBuffer)
            CB := SubStr ( OutBuffer , icp+x , 1 )
            CM := SubStr ( Mask , icp+x , 1 )

            If !IsDigit(CB) .And. !IsAlpha(CB) .And.;
               ( !( CB == ' ' ) .or. ( CB == ' ' .and. CM == ' ' ) )
               wvw_ebsetsel(mnwinnum, mnebid, icp+x, icp+x)
            Else
               Exit
            EndIf
         Next x
      EndIf
   EndIf
RETURN

//from h_textbox.prg
*------------------------------------------------------------------------------*
static Function CharMaskTekstOK( cString, cvaltype, cMask )
*------------------------------------------------------------------------------*
Local lPassed:=.t.,CB,CM,x
   //x BEGIN
   if cvaltype=="D"
      For x := 1 To min(Len(cString),Len(cMask))
         CB := SubStr ( cString , x , 1 )
         CM := SubStr ( cMask , x , 1 )
         Do Case
            Case CM == '9'
               If IsDigit ( CB ) .Or. CB == ' '
                  * lPassed:=.t.
               Else
                  Return .f.
               EndIf
            OtherWise
               * lPassed:=.t.
         End Case
      next i
      return .t.
   endif
   //x END

   For x := 1 To min(Len(cString),Len(cMask))
      CB := SubStr ( cString , x , 1 )
      CM := SubStr ( cMask , x , 1 )
      Do Case
      // JK
         Case (CM) == 'A' .or. (CM) == '!'
            If IsAlpha ( CB ) .Or. CB == ' '
               * lPassed:=.t.
            Else
               Return .f.
            EndIf
         Case CM == '9'
            If IsDigit ( CB ) .Or. CB == ' '
               * lPassed:=.t.
            Else
               Return .f.
            EndIf
         Case CM == ' '
            If CB == ' '
               * lPassed:=.t.
            Else
               Return .f.
            EndIf
         OtherWise
            * lPassed:=.t.
      End Case
   next i
Return .t. //lPassed

//from h_textbox.prg
static Function GetValFromText ( Text , mcvaltype )
* eg. GetValFromText( "999,999.99" ) --> 999999.99
Local x , c , s

   //x BEGIN
   if mcvaltype=="C"
      return Text
   endif

   if mcvaltype=="D"
      s := ctod(Text)
      return s
   endif
   //x END

   * ASSUME numeric
   s := ''
   For x := 1 To Len ( Text )
      c := SubStr(Text,x,1)
      //If c='0' .or. c='1' .or. c='2' .or. c='3' .or. c='4' .or. c='5' .or. c='6' .or. c='7' .or. c='8' .or. c='9' .or. c='.' .or. c='-'
      If c $ '0123456789' .or. c $ '.-'
         s := s + c
      EndIf
   Next x

   If Left ( AllTrim(Text) , 1 ) == '(' .OR.  Right ( AllTrim(Text) , 2 ) == 'DB'
      s := '-' + s
   EndIf

   //useless!
   //s := Transform ( Val(s) , Getnummask(s_cmask, mcvaltype) )

Return Val(s)

//from h_textbox.prg
static Function GetNumMask ( Text, mcvaltype )
* eg. GetNumMask("999,999.99") --> "999999.99"
Local i , c , s
   //x BEGIN
   if mcvaltype=="D" .or. mcvaltype=="C"
      s := Text
      return s
   endif
   //x END

   s := ''
   For i := 1 To Len ( Text )
      c := SubStr(Text,i,1)
      If c='9' .or. c='.'
         s := s + c
      EndIf
      if c == '$' .or. c == '*'
         s := s+'9'
      EndIf
   Next i
Return s

//from TGET.PRG
STATIC FUNCTION IsBadDate( cBuffer ) //, cPicFunc )
LOCAL cBuffer2

   If Empty( cBuffer )
      return .F.
   Endif

   //If "E" IN cPicFunc
   //    cBuffer := InvertDwM( cBuffer )
   //Endif

   cBuffer2 := StrTran( cBuffer,"/","")
   cBuffer2 := StrTran( cBuffer2,"-","")
   cBuffer2 := StrTran( cBuffer2,".","")

   If Empty( cBuffer2 )
      return .F.
   Endif

   If Empty( CtoD( cBuffer ) )
      MyMessageBox(NIL, "'" + cBuffer + "' is not a valid DATE")
      return .T.
   Endif

RETURN .F.

/************ WVW_INPUTFOCUS ************/

function WVW_INPUTFOCUS(nWinNum, hWnd, message, wParam, lParam)
* this is a simple sample of WVW_INPUTFOCUS
* only handles WM_CHAR, thus not all input characters are accepted
local wParamLow := WVW_LOWORD(wParam)
local wParamHi := WVW_HIWORD(wParam)
local nCommand, ch
local bhandler

   * did user perform a menu/toolbar action on Main Window?
   //if message==WM_COMMAND .and. nWinNum==0  //menu,toolbar,pushbutton
   //   return .f.
   //endif

   * now we handle input on other non-topmost windows

   do case
      case message==WM_CHAR
         ch := wParam
         bhandler := inp_handler(nWinNum)
         if valtype(bhandler)=="B"
            eval( bhandler, nWinNum, ch )
            return .t.
         else
            return .f.
         endif
      otherwise
         * ignore
         return .t.
   endcase

return .f.//WVW_INPUTFOCUS()

function inp_handler(nwinnum, bhandler)
static sbhandlers := {}
local i
local retval := iif(len(sbhandlers)>=nwinnum+1, sbhandlers[nwinnum+1], NIL)
   if valtype(bhandler)=="B"
      if len(sbhandlers) < nwinnum+1
         asize(sbhandlers, nwinnum+1)
      endif
      sbhandlers[nwinnum+1] := bhandler
   endif
return retval


/********** general helpers ************/

static function MyMessageBox(nwinnum, cMessage, cCaption, nFlags)
local nParent
   default cCaption to "Debug Message"
   nParent := wvw_getwindowhandle(nwinnum)
return win_messagebox(nParent, cMessage, cCaption, nFlags)

static function lShiftPressed()
// #define VK_SHIFT            16
return (wvw_GETKEYSTATE(16) < 0)
