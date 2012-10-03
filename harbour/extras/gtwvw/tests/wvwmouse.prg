/*
 * $Id$
 */

/*
  Pseudo mouse object in GTWVW GUI
  copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

  This is a sample of implementation of a pseudo GUI object in GTWVW,
  using GTWVW GUI primitives.

  Example on how to use it is given in WVWTEST9.PRG.
  eg. WVWMouseObject():New("Button1", maxrow()-1, 10, , , {|| tone(660,3) } )

  NOTES:
  This is just a sample. You may not want to see it as a 'model'.
  There are many other ways to handle pseudo GUI objects in GTWVW, just
  use your imagination :-)
*/

#include "inkey.ch"
#include "common.ch"
#include "hbclass.ch"

static s_amouseobjlist := {}
static s_ncurkey := 0
static s_nkeyrepeater := NIL
static s_nrepeatrate  := 0.1
static s_nrepeatdelay := 0.5

#define _DEFAULT_CAPTION_FONT   "Tahoma"
#define _DEFAULT_CAPTION_HEIGHT 16

* CLIPPER COLOR CONSTANTS
#define _IDX_BLACK               0
#define _IDX_BLUE                1
#define _IDX_GREEN               2
#define _IDX_CYAN                3
#define _IDX_RED                 4
#define _IDX_MAGENTA             5
#define _IDX_BROWN               6
#define _IDX_WHITE               7
#define _IDX_LIGHT_GRAY          8
#define _IDX_BRIGHT_BLUE         9
#define _IDX_BRIGHT_GREEN       10
#define _IDX_BRIGHT_CYAN        11
#define _IDX_BRIGHT_RED         12
#define _IDX_BRIGHT_MAGENTA     13
#define _IDX_YELLOW             14
#define _IDX_BRIGHT_WHITE       15


* mouse object types
#define _MOBJECT_BUTTON  0      //mouse button
#define _MOBJECT_HSCROLL 1      //horiz scrollbar: OBSOLETE, NOT USED HERE
#define _MOBJECT_VSCROLL 2      //horiz scrollbar: OBSOLETE, NOT USED HERE

* for Button Types:
#define _BUTTON_NORMAL 0        //normal button
#define _BUTTON_FLAT   1        //'transparent', raised when mouseover
#define _BUTTON_NONE   2        //no sign even when mouseover or clicked
#define _BUTTON_HARD   3        //no recessed when pressed

***************************************************************
* WVWMouseButton
***************************************************************

CLASS WVWMouseButton
   //DATA nId         /* TODO */       //mouse object id
   //DATA nHotKey     /* TODO */       //hotkey associated with this object
   DATA nWinId                       //20040303, parent window's number
   DATA lVisible                     //is the object visible
   DATA lEnable                      //20040303, is the object enable
   DATA lTight                       //allow tight neighboring
   DATA nType                        //20040303, appearance of this button
   DATA nRow1, nCol1, nRow2, nCol2   //mouse object region

   DATA bClickBlock                  //executed on Left Click
   DATA bPressBlock                  //executed on Left Press

   DATA lRepeatPress                 //repeat Left Press when pressed during mouse over?

   DATA cCaption
   DATA cCaptionFont                 //font name for caption
   DATA nCaptionHeight               //height of font for caption, if NIL use current wvw_getfontinfo()
   DATA cImage                       //20040325, image file name

   DATA cNormalColor    //button normal color, pls use single color, eg "W"
   DATA cPressedColor   //button pressed color, pls use single color, eg "B"

   * private DATA, should be protected
   DATA lPressed                     //is it being pressed by Left Button?
   DATA lHover                       //20040303, is mouse over the button?

   * METHODS
   METHOD New( cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId)

   //METHOD nGetId() INLINE ::nId            /* TODO */
   //METHOD SetHotKey(nKey)                  /* TODO */
   //METHOD nGetHotKey() INLINE ::nHotKey    /* TODO */

   METHOD Enable(lEnable)

   METHOD SetClickBlock(bBlock)
   METHOD GetClickBlock() INLINE ::bClickBlock

   METHOD SetPressBlock(bBlock)
   METHOD GetPressBlock() INLINE ::bPressBlock

   METHOD SetRepeatPress(lRepeat)
   METHOD GetRepeatPress() INLINE ::lRepeatPress

   METHOD SetCaption(cCaption)

   METHOD OnClick()
   METHOD OnPress()          //K_LBUTTONDOWN occurs over this mouse object
   METHOD OnRelease()        //K_LBUTTONUP occurs over this mouse object
   METHOD OnReleaseOut()     //K_LBUTTONUP occurs outside of this mouse object
   METHOD OnMouseOut()       //mouse is moved from over the button outside
   METHOD OnMouseOver()    //TODO

   METHOD Draw(nWinNum)

ENDCLASS   //WVWMouseButton

METHOD New(cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId) CLASS WVWMouseButton
   default cCaption to "" //20040325,was: "Button"
   default nRow1 to 0
   default nCol1 to 0
   default nRow2 to nRow1
   default nCol2 to nCol1 + max(10, len(cCaption) + 2) -1
   default nType to _BUTTON_NORMAL         //20040303
   default lDraw to .T.
   default nWinId to wvw_nNumWindows()-1   //20040303

   //TODO: ::nId := iif(empty(s_amouseobjlist), 1, s_amouseobjlist[len(s_amouseobjlist)]:nGetId()+1)
   //TODO: ::nHotKey := NIL
   ::nWinId := nWinId  //20040303

   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   ::bClickBlock   := iif(valtype(bClickBlock)=="B", bClickBlock, NIL)
   ::bPressBlock   := NIL

   ::lRepeatPress  := .F.

   ::lPressed  := .F.
   ::lHover    := .F.  //20040303
   ::cCaption := cCaption
   ::cCaptionFont := _DEFAULT_CAPTION_FONT
   ::nCaptionHeight := _DEFAULT_CAPTION_HEIGHT

   ::cImage    := NIL  //20040325

   //TODO: pls use current color
   ::cNormalColor := "W"
   ::cPressedColor := "W"

   ::lVisible := .T.
   ::lEnable  := .T.
   ::lTight   := .F.
   ::nType := nType

   if lDraw  //20040304
      ::Draw(::nWinId)
   endif

return Self     //WVWMouseButton

METHOD Enable(lEnable) CLASS WVWMouseButton
  ::lEnable := lEnable
  ::draw()
return Self

METHOD SetClickBlock(bBlock) CLASS WVWMouseButton
  ::bClickBlock := bBlock
return Self

METHOD SetPressBlock(bBlock) CLASS WVWMouseButton
  ::bPressBlock := bBlock
return Self

METHOD SetRepeatPress(lRepeat) CLASS WVWMouseButton
  ::lRepeatPress := lRepeat
return Self

METHOD SetCaption(cCaption) CLASS WVWMouseButton
  ::cCaption := cCaption
return Self

METHOD OnPress() CLASS WVWMouseButton
* this is called when LEFT mouse button is pressed on the object
local lWasPressed
  if !::lEnable  //20040303
     return Self
  endif

  lWasPressed := ::lPressed
  ::lPressed := .T.
  ::Draw()

  if ::lRepeatPress //.and. ::lPressed
     if !lWasPressed
        xKeyRepeater(.T.) //init it
     endif
     wvwm_SetKeyRepeater( .T. )   //activate key repeater
  endif

  if valtype(::bPressBlock) == "B"
     eval(::bPressBlock)
  endif
return Self

METHOD OnClick() CLASS WVWMouseButton
* this is called when LEFT mouse button is clicked on the object
* normally (or should it be restricted to be?) called from ::OnRelease()
  if !::lEnable  //20040303
     return Self
  endif

  if valtype(::bClickBlock) == "B"
     eval(::bClickBlock)
  endif
return Self

METHOD OnRelease() CLASS WVWMouseButton
local lWasPressed := ::lPressed
  if !::lEnable  //20040303
     return Self
  endif

  ::lPressed := .F.
  ::Draw()

  if ::lRepeatPress //.and. ::lPressed
     wvwm_SetKeyRepeater( .F. )   //deactivate key repeater
  endif

  if lWasPressed
     ::OnClick()
  endif
return Self

METHOD OnReleaseOut() CLASS WVWMouseButton
* left button is released outside of mouse region
  if !::lEnable  //20040303
     return Self
  endif

  ::lPressed := .F.
  ::Draw()

  //NOTE: no need to do SetKeyRepeater( .F. ),
  //      because it was already handled by onMouseOut
return Self

//20040303
METHOD OnMouseOut() CLASS WVWMouseButton
* mouse is moved from over the button outside
  if !::lEnable  //20040303
     return Self
  endif

  if ::lRepeatPress .and. ::lPressed
     wvwm_SetKeyRepeater( .F. )   //stop key repeater
  endif

  ::lHover := .F.
  ::Draw()
return Self

//20040303
METHOD OnMouseOver() CLASS WVWMouseButton
* mouse is moved to over the button from outside
  if !::lEnable  //20040303
     return Self
  endif

  if ::lRepeatPress .and. ::lPressed
     wvwm_SetKeyRepeater( .T. )   //activate key repeater
  endif

  ::lHover := .T.
  ::Draw()
return Self

METHOD DRAW(nWinNum) CLASS WVWMouseButton
local nROw := row(), nCol := col()
local nOldCursor := setcursor(0)
local lMouseOver := ::lHover //20040303,was: ( mrow()>=::nrow1 .and. mrow()<=::nrow2 .and. mcol()>=::ncol1 .and. mcol()<=::ncol2 )
local lPressed := ::lPressed .and. lMouseOver
local aFontInfo := iif(::nCaptionHeight==NIL,wvw_getFontInfo(nWinNum),NIL)
local nLabelColor := iif(!lPressed, rgb(0,0,0), rgb(96,96,96))
local lUseImage := (valtype(::cImage)=="C") //20040325

  if !::lVisible .or. (::nType == _BUTTON_NONE)
     setcursor(nOldCursor) //20040303
     return Self
  endif

  if ::nrow1>::nrow2 .or. ::ncol1>::ncol2
     setcursor(nOldCursor) //20040303
     return Self
  endif

  default nWinNum to ::nWinId

  if lPressed //::lPressed
     if !(::nType==_BUTTON_HARD)
        WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( HB_ColorToN(::cPressedColor) ), ::lTight)
        Wvw_DrawBoxRecessed( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )  //wvw
     else
        WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( HB_ColorToN(::cNormalColor) ), ::lTight)
        Wvw_DrawBoxRaised(   nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
     endif

     if lUseImage .and. !(::nType==_BUTTON_NONE)
        if !Wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
           win_messagebox(NIL, "Button Failed Wvw_DrawImage(" + ::cImage + ")")
        endif
     endif

     if !empty(::cCaption)
        Wvw_DrawLabel(nWinNum, ::nRow1, nCeiling((::nCol2+::nCol1)/2), ::cCaption, 6,, nLabelColor, rgb(198,198,198), ::cCaptionFont, iif(valtype(afontinfo)=="A",afontinfo[2],::nCaptionHeight), 0, , , , .F., .F. )
     endif
  else
     if lMouseOver .or. (::nType==_BUTTON_NORMAL) .or. (::nType==_BUTTON_HARD)
        WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( HB_ColorToN(::cNormalColor) ), ::lTight)
        Wvw_DrawBoxRaised(nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
     else
        * must undraw the box. ideally GTWVW has this function
        Wvw_DrawBoxGroup(nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2 )
     endif

     if lUseImage .and. !(::nType==_BUTTON_NONE)
        if !Wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
           win_messagebox(NIL, "Button Failed Wvw_DrawImage(" + ::cImage + ")")
        endif
     endif

     if !::lEnable
        nLabelColor := rgb(96,96,96)
     elseif lMouseOver
        nLabelColor := rgb(255,0,0)
     endif

     if !empty(::cCaption)
        Wvw_DrawLabel(nWinNum, ::nRow1, nCeiling((::nCol2+::nCol1)/2), ::cCaption, 6,, nLabelColor, rgb(198,198,198), ::cCaptionFont, iif(valtype(afontinfo)=="A",afontinfo[2],::nCaptionHeight), 0, , , , .F., .F. )
     endif
  endif
  setcursor(nOldCursor)
return Self

//*******************************************************
* interface functions

function wvwm_paint( nWinNum )
* normally called by WVW_Paint()
* redraw every mouse object in window nWinNum
   if len(s_amouseobjlist) >= nWinNum+1
      aeval( s_amouseobjlist[nWinNum+1], {| o | o[2]:draw(nWinNum)} )
   endif
return NIL

function wvwm_ResetMouseObjects( nWinNum )
* clears all mouse objects from window nWinNum
   do while len(s_amouseobjlist) < nWinNum+1
      aadd( s_amouseobjlist, {} )
   enddo
   s_amouseobjlist[ nWinNum+1 ] := {}
return .T.

function wvwm_AddMouseObjects( nWinNum, oMouse, nObjType )
* adds a mouse object oMouse into window nWinNum
   default nObjType to _MOBJECT_BUTTON
   aadd( s_amouseobjlist[ nWinNum+1 ], {nObjType, oMouse} )
return .T.

function wvwm_nNumMouseObjects( nWinNum )
* returns number of mouse objects in window nWinNum
return len(s_amouseobjlist[ nWinNum+1 ])

function wvwm_nObjectType( nWinNum, nObjNum )
* returns type of mouse objects number nObjNum in window nWinNum
return s_amouseobjlist[ nWinNum+1 ][nObjNum][1]

function wvwm_SetKeyRepeater( lSet )
* returns .T. if KeyRepeater is active
* if lSet is supplied, KeyRepeater is enable/disable accordingly
local lWasSet := (s_nkeyrepeater != NIL)
   if !(lSet==NIL)
      if lSet
         if !lWasSet
            s_nkeyrepeater := hb_idleadd( {|| xKeyRepeater() } )
         endif
      else
         if lWasSet
           hb_idledel( s_nkeyrepeater )
           s_nkeyrepeater := NIL
         endif
      endif
   endif
return lWasSet


static function nButtonChecker(nkey, oMouseObj)
local nrow := mrow(), ncol := mcol()
local lMouseOver
local i

     lMouseOver := ( nrow>=oMouseObj:nrow1 .and. nrow<=oMouseObj:nrow2 .and. ncol>=oMouseObj:ncol1 .and. ncol<=oMouseObj:ncol2 )
     if !lMouseOver
        * cursor is somewhere outside of current mouse object area

        if oMouseObj:lHover
           * user has just moved the cursor out of this button
           oMouseObj:OnMouseOut()
        else
           do case
           case nkey == K_LBUTTONUP
              if oMouseObj:lPressed
                 oMouseObj:OnReleaseOut()
              endif
           endcase
        endif

     else
        * cursor is over current mouse object area

        if !oMouseObj:lHover
           * user has just moved the cursor into over this button
           oMouseObj:OnMouseOver()
        else
           do case
           case nkey == K_LDBLCLK
              * currently button not handle this events,
              * so we will treat it as single key press
              oMouseObj:OnPress()
           case nkey == K_LBUTTONDOWN
              oMouseObj:OnPress()
           case nkey == K_LBUTTONUP
              oMouseObj:OnRelease()
           endcase
        endif

     endif

return nkey //nButtonChecker(nkey)

static function nScrollChecker(nkey, cType, oMouseObj)
* cType == "H" or "V"

     nButtonChecker(nkey, oMouseObj:oFirstButton)
     nButtonChecker(nkey, oMouseObj:oRail1Button)
     nButtonChecker(nkey, oMouseObj:oMidButton)
     nButtonChecker(nkey, oMouseObj:oRail2Button)
     nButtonChecker(nkey, oMouseObj:oSecondButton)

return nkey //nHScrollChecker(nkey)

/* HANDLING MULTIPLE MOUSE OBJECTS */
/* called by SETKEYAFTERBLOCK() function */
function wvwm_nMouseChecker(nkey)
* check mouse events in relations with registered mouse objects
* always return inkey codes as if nothing happens here
* (so as to allow GET do something about it)
* NOTE: only cares about current (last) window
local i, oMouseObj
local nCurWindow

  nCurWindow := WVW_nNumWindows()-1

  if len(s_amouseobjlist) < nCurWindow+1
     return nkey
  endif

  s_ncurkey := nkey   //20040303

  for i := 1 to len(s_amouseobjlist[nCurWindow+1])
     oMouseObj := s_amouseobjlist[nCurWindow+1][i][2]

     do case
     case (s_amouseobjlist[nCurWindow+1][i][1]==_MOBJECT_BUTTON)
        nButtonChecker(nkey, oMouseObj)
     case (s_amouseobjlist[nCurWindow+1][i][1]==_MOBJECT_HSCROLL)
        nScrollChecker(nkey, "H", oMouseObj)
     case (s_amouseobjlist[nCurWindow+1][i][1]==_MOBJECT_VSCROLL)
        nScrollChecker(nkey, "V", oMouseObj)
     otherwise
        * runtime error!
     endcase

  next

  s_ncurkey := 0  //20040303
return nKey //wvwm_nMouseChecker(nkey)


static procedure xKeyRepeater(lInit)
static nLastValidCheck := 0
static lFirstRepeat := .T.
local nNow
local nRepeatInterval

  default lInit to .F.
  if lInit
     * simply init the locally static var
     lFirstRepeat := .T.
     nLastValidCheck := seconds()
     return
  endif

  nRepeatInterval := iif(lFirstRepeat, s_nrepeatdelay, s_nrepeatrate)

  nNow := seconds()
  if nNow - nLastValidCheck < nRepeatInterval  //s_nrepeatrate
     * not yet
     return
  endif

  if !MLEFTDOWN()
     * mouse is not pressed
     return
  endif

  * mouse is down long enough since last valid check
  * let's repeat it
  __keyboard( K_LBUTTONDOWN )

  * to be more precise
  nLastValidCheck := seconds()   //nNow

  * next repeat will be quicker
  lFirstRepeat := .F.

return //xKeyRepeater()

**************************** supporters

static function nCeiling(nNumber)
local nTemp
   nTemp := nNumber - INT(nNumber)  //right of dec point
   if nTemp>0
      nNumber := INT(nNumber) + 1
   else
      nNumber := INT(nNumber)
   endif
return nNumber
