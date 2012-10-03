/*
 * $Id$
 */

/*
   Copyright 2005 Budyanto Dj. <budyanto@centrin.net.id>

   A simple example on how to make use of GTWVW's combobox.

   This program requires GTWVW.LIB.

   Compile: bldwvw cbtest1
*/


#include "inkey.ch"
#include "setcurs.ch"

static s_cQuestion := "Quick survey: What do you think about GTWVW?"
static s_cHint     := "Hint: Please be positive..."

static s_aAnswers  := {"1-GTWVW is a great library!",;
                       "2-GT who? I never heard about him",;
                       "3-Sorry, I don't like it"}

static s_cYourAnswer:="Your response is:"
static s_cThankYou := "Thanks for participating in our survey :-)"

#define _SECRET_KEY 101010

ANNOUNCE HB_NOSTARTUPWINDOW

proc main
local nMaxWidth, nCBid, nPBid, nPos
local ch,ncursor
   setcolor("N/W")
   WVW_SetTitle(NIL,"Quick Survey")
   WVW_NoClose(NIL)
   WVW_SetAltF4Close(.f.)
   WVW_SetLineSpacing(NIL,4)
   //WVW_SetLSpaceColor(NIL,7)
   WVW_cbSetFont(NIL, "Arial", 16)
   WVW_pbSetFont(NIL, "Arial", 16)

   nMaxWidth := 0
   aeval(s_aAnswers, {| x | nMaxWIdth := max(nMaxWidth,len(x))})

   setmode(11, nMaxWidth+1+10+2)

   CLS
   nCursor := setcursor(SC_NONE)
   @ 1,1 say s_cQuestion
   @ 2,1 say s_cHint
   nCBid := wvw_cbCreate(NIL, 4, 1, nMaxWidth, s_aAnswers, ;
                              {| nWinNum, nId, nEvent, nIndex | ;
                               CBhandler( nWinNum, nId, nEvent, nIndex, nPBid ) } )

   nPBid := wvw_pbCreate(NIL, 4,1+nMaxWidth+1, 4,1+nMaxWidth+1+10-1, "OK",NIL,;
                              {|| __keyboard( _SECRET_KEY ) },{0,0,+2,0})

   wvw_cbSetFocus(NIL, nCBid)

   wvw_ShowWindow()

   nPos := 1
   do while (ch:=inkey(0))!=_SECRET_KEY
      do case
      case ch==K_TAB .or. ch==K_ENTER
         if nPos==2 .and. ch==K_ENTER .and. wvw_pbEnable(NIL, nPBid)
            keyboard(_SECRET_KEY)
            loop
         else
            nPos++
         endif
      case ch==K_SH_TAB
         nPos--
      endcase
      if nPos>2
         nPos := 1
      elseif nPos<1
         nPos:=2
      endif
      do case
      case nPos==1
         wvw_cbSetFocus(NIL, nCBid)
      case nPos==2
         wvw_pbSetFocus(NIL, nPBid)
         wvw_pbSetStyle(NIL, nPBid, 1) //BS_DEFPUSHBUTTON
      endcase
   enddo

   wvw_cbEnable(NIL, nCBid, .f.)
   wvw_pbEnable(NIL, nPBid, .f.)
   @ 6,1 say "Your response is:"
   @ 7,1 say wvw_cbGetCurText(NIL, nCBid)
   @ 9,1 say s_cThankYou
   inkey(0)
   setcursor(nCursor)
return  //main

static function CBhandler(nWinNum,nId,nEvent,nIndex, nPBid)
   do case
   case nEvent==3 //CBN_SETFOCUS
      * none
   case nEvent==4 //CBN_KILLFOCUS
      * none
      if nIndex==0
         wvw_pbEnable(nWinNum, nPBid, .t.)
      else
         wvw_pbEnable(nWinNum, nPBid, .f.)
      endif
   case nEvent==1 //CBN_SELCHANGE
      if !wvw_cbIsDropped(nWinNum, nId)
         * nIndex is 0-based
         if nIndex==0
            wvw_pbEnable(nWinNum, nPBid, .t.)
         else
            wvw_pbEnable(nWinNum, nPBid, .f.)
         endif
         wvw_cbSetFocus(nWinNum, nId)
      endif
   endcase

return NIL