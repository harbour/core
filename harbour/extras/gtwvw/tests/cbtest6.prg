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

#include "getexit.ch"
#include "inkey.ch"

/* Two different keyboard handling method
 * (notes: WVW_CB_KBD_CLIPPER doesn't mimic anything from Clipper
 *         it just 'feels like Clipper' to me)
 */
#define WVW_CB_KBD_STANDARD  0
#define WVW_CB_KBD_CLIPPER   1

// our preferred method (choose one of the above)
static s_nCB_Kbd := WVW_CB_KBD_CLIPPER

// list of created comboboxes
static s_aComboList := {}

// create these two as local, otherwise it will be assumed PRIVATE
MEMVAR __nCBid__,__temp__

#xcommand   @ <row>,<col> COMBOBOX <var>                        ;
                          OPTIONS <aOptions>                    ;
                          WIDTH <nWidth>   =>                   ;
                                                                ;
            ;
            __nCBid__ := wvw_cbCreate(NIL, <row>, <col>, <nWidth>,            ;
                                  <aOptions>,                                 ;
                                  {|nWinNum,nId,nEvent,nIndex, temp|          ;
                                    CBhandler(nWinNum,nId,nEvent,nIndex,<"var">,GetList);
                                  },                                          ;
                                  NIL,NIL,s_nCB_Kbd,NIL);                     ;
            AADD(s_aComboList, {__nCBid__, <"var"> });                        ;
            __temp__ := wvw_cbFindString(NIL, __nCBid__, <var> );             ;
            iif(__temp__>=0, wvw_cbSetIndex(NIL, __nCBid__, __temp__),NIL);   ;
            setpos(<row>,<col>);                                              ;
            AAdd(GetList,_GET_(<var>,<"var">,repl("X",<nWidth>),,) ) ;        ;
            ATAIL(GetList):cargo := __nCBid__;                                ;
            ATAIL(GetList):reader := {|get| CBreader(get)}

proc main
local getlist := {}
local mname := padr("Budyanto Dj.",30), msex := "MALE", mage := 17, mstat := "married"
local __nCBid__,__temp__  //these two are temporary var required by CB get creation
   WVW_SetCodePage(NIL,255)
   WVW_SetLineSpacing(NIL,4)
   WVW_SetLSpaceColor(NIL,0)
   WVW_cbSetFont(NIL, "Arial", 14)  //std: 20-2
   //set(_SET_TRACESTACK, 0)

   //wvw_setmousemove(,.t.)

   CLS

   * reset combobox list:
   s_aComboList := {}

   @ 0,0 say     "Name :" get mname pict "@K"
   @ 1,0 say     "Sex  :"
   @ 1,7 COMBOBOX msex OPTIONS {"Male","Female","Unknown"} WIDTH 10
   @ 2,0 say     "Stat :"
   @ 2,7 COMBOBOX mstat OPTIONS {"Married","Unmarried","Widowed"} WIDTH 15
   @ 3,0 say     "age  :" get mage pict "999"
   read

   * disable all comboboxes:
   aeval(s_aComboList, {|x| wvw_cbenable(NIL, x[1], .f.)})

   devpos(5,0)
   ? "name: '" + mname + "'"
   ? "sex : '" + msex + "'"
   ? "stat: '" + mstat + "'"
   ? "age : " + alltrim(str(mage))
   ? "that's what you've got from GET"
   inkey(0)

   * destroy all comboboxes:
   aeval(s_aComboList, {|x| wvw_cbdestroy(NIL, x[1])})
   s_aComboList := {}

   ?
   ? "Comboboxes have now been removed"
   ? "Now press ESC to exit"

   do while inkey(0)!=K_ESC
   enddo
return  //main

function CBhandler(nWinNum,nId,nEvent,nIndex, cVar, GetList)
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

local i, ccursel
local oGetList := __GetListActive()
local oGet := GetActive()

   /* if GetList is empty, then READ session is already ended
    * this should not be happenning!
    */
   if empty(GetList)
      MyAlert("Bad practice: you left an active combobox, but READ already ended")
      return NIL//ignore this event
   endif

   do case
   case nEvent==3 //CBN_SETFOCUS
      i := ascan(GetList, {|x|x:Name==cVar})
      if i>0
         /* !oGet:HasFocus means
          * CBN_SETFOCUS was NOT initiated from mouseclick
          * then we don't need to bother about setting focus to the
          * new GET. GetSys has already done that via CBreader().
          * It is CBreader() that brought us here, so ignore it.
          */
         if oGet:HasFocus
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

            SetWinFocus(nWinNum)
            msetpos(GetList[i]:row, GetList[i]:col+1)
            keyboard(K_LBUTTONDOWN)
         endif //oGet:HasFocus

      else  //i==0
         /* there's no GET object beneath the combobox.
          * This must be a combobox living in the wild.
          * Do what you want with it, we do nothing here.
          */
      endif

   case nEvent==4 //CBN_KILLFOCUS
      // put current content of combobox into GET variable beneath it.
      cCurSel := wvw_cbGetCurText(nWinNum, nId)
      oGet:varput(cCurSel)
      oGet:display() //this is optional

   endcase
return NIL

/************* custom get reader ******************/

FUNCTION CBreader( oGet )
* This is the reader() for oGet, a GET object hidden beneath a combobox.
*
* Some notes:
* oGet:cargo stores combobox id over this oGet
*
   LOCAL nKey, bKeyBlock
   LOCAL nSelected, cSelected
   local oGetList := __GetListActive()

   if !wvw_cbIsFocused(NIL, oGet:cargo)
      wvw_cbSetFocus(NIL, oGet:cargo)
   endif

   oGet:setfocus()
   nKey := INKEY(0)

   IF ( nKey == K_ENTER )
      * NOTE that in WVW_CB_KBD_CLIPPER mode we will never get here
      oGet:exitState := GE_DOWN

   ELSEIF ( nKey == K_UP )
      oGet:exitState := GE_UP

   ELSEIF ( nKey == K_SH_TAB )
      oGet:exitState := GE_UP

   ELSEIF ( nKey == K_DOWN )
      // NOTE that in WVW_CB_KBD_STANDARD mode we will never get here
      oGet:exitState := GE_DOWN

   ELSEIF ( nKey == K_TAB )
      oGet:exitState := GE_DOWN

   ELSEIF ( nKey == K_ESC )
      IF ( Set(_SET_ESCAPE) )
         oGet:exitState := GE_ESCAPE
      ENDIF

   ELSEIF ( nKey == K_PGUP )
      oGet:exitState := GE_WRITE

   ELSEIF ( nKey == K_PGDN )
      oGet:exitState := GE_WRITE

   ELSEIF ( nKey == K_CTRL_HOME )
      oGet:exitState := GE_TOP

   ELSEIF ( nKey == K_LBUTTONDOWN .or. nKey == K_LDBLCLK )
      * is there any GET object hit?
      if !empty(HitTest(oGetList:aGetList, mrow(), mcol(), NIL))
         oGet:exitState := GE_MOUSEHIT
      else
         oGet:exitState := GE_NOEXIT
      endif

   ELSEIF valtype(bKeyBlock := SETKEY(nKey))=="B"
      oGetList:GetDoSetKey( bKeyBlock )  //eval(bKeyBlock)
      oGet:exitState := GE_NOEXIT

   ENDIF

   if oGet:exitState != GE_NOEXIT
      SetWinFocus(NIL)  //assume current window
      oGet:killfocus()
   endif

RETURN NIL  //cbreader()

/* not used:
static function MoveToGet(GetList, nPos)
// move focus to GET object at GetList[nPos]
local i
local oGetList := __GetListActive( )
local oGet
   * leave current active get
   oGet := GetActive()

   if oGet!=NIL .and. oGet:HasFocus
      if oGet:changed
         if GetPostValidate(oGet)
            oGet:updatebuffer()
         else
            //oGet:undo()
            return .f.
         endif
      endif
      oGet:exitstate := GE_MOUSEHIT
      oGetList:nNextGet := nPos
      oGetList:settle(,.f.)
   endif
return .t.
*/

/* miscellaneous **********************************/

static function SetWinFocus(nWinNum)
// Set FOCUS to window nWinNum
local hWnd := wvw_getWindowHandle(nWinNum)
   WIN_SETFOCUS(hWnd)
return NIL

static function MyAlert(cMsg, par2, par3, par4, par5, par6)
local nLineSpacing := WVW_SetLineSpacing(NIL,0)
local retval := Alert(cMsg, par2, par3, par4, par5, par6)
   WVW_SetLineSpacing(NIL,nLineSpacing)
return retval
