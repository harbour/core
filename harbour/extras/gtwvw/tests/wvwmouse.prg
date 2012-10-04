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
#include "setcurs.ch"
#include "hbclass.ch"

STATIC s_amouseobjlist := {}
STATIC s_ncurkey := 0
STATIC s_nkeyrepeater := NIL
STATIC s_nrepeatrate  := 0.1
STATIC s_nrepeatdelay := 0.5

#define _DEFAULT_CAPTION_FONT   "Tahoma"
#define _DEFAULT_CAPTION_HEIGHT 16

// CLIPPER COLOR CONSTANTS
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


// mouse object types
#define _MOBJECT_BUTTON  0      //mouse button
#define _MOBJECT_HSCROLL 1      //horiz scrollbar: OBSOLETE, NOT USED HERE
#define _MOBJECT_VSCROLL 2      //horiz scrollbar: OBSOLETE, NOT USED HERE

// for Button Types:
#define _BUTTON_NORMAL 0        //normal button
#define _BUTTON_FLAT   1        //'transparent', raised when mouseover
#define _BUTTON_NONE   2        //no sign even when mouseover or clicked
#define _BUTTON_HARD   3        //no recessed when pressed

//**************************************************************
// WVWMouseButton
//**************************************************************

CLASS WVWMouseButton

// DATA nId         /* TODO */       //mouse object id
// DATA nHotKey     /* TODO */       //hotkey associated with this object
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

// private DATA, should be protected
   DATA lPressed                     //is it being pressed by Left Button?
   DATA lHover                       //20040303, is mouse over the button?

// METHODS
   METHOD New( cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId )

// METHOD nGetId() INLINE ::nId            /* TODO */
// METHOD SetHotKey( nKey )                /* TODO */
// METHOD nGetHotKey() INLINE ::nHotKey    /* TODO */

   METHOD Enable( lEnable )

   METHOD SetClickBlock( bBlock )
   METHOD GetClickBlock() INLINE ::bClickBlock

   METHOD SetPressBlock( bBlock )
   METHOD GetPressBlock() INLINE ::bPressBlock

   METHOD SetRepeatPress( lRepeat )
   METHOD GetRepeatPress() INLINE ::lRepeatPress

   METHOD SetCaption( cCaption )

   METHOD OnClick()
   METHOD OnPress()          //K_LBUTTONDOWN occurs over this mouse object
   METHOD OnRelease()        //K_LBUTTONUP occurs over this mouse object
   METHOD OnReleaseOut()     //K_LBUTTONUP occurs outside of this mouse object
   METHOD OnMouseOut()       //mouse is moved from over the button outside
   METHOD OnMouseOver()    //TODO

   METHOD Draw( nWinNum )

ENDCLASS   //WVWMouseButton

METHOD New( cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId ) CLASS WVWMouseButton

   DEFAULT cCaption TO "" //20040325,was: "Button"
   DEFAULT nRow1 TO 0
   DEFAULT nCol1 TO 0
   DEFAULT nRow2 TO nRow1
   DEFAULT nCol2 TO nCol1 + Max( 10, Len( cCaption ) + 2 ) - 1
   DEFAULT nType TO _BUTTON_NORMAL         //20040303
   DEFAULT lDraw TO .T.
   DEFAULT nWinId TO wvw_nNumWindows() - 1   //20040303

   //TODO: ::nId := iif( Empty( s_amouseobjlist ), 1, s_amouseobjlist[ Len( s_amouseobjlist ) ]:nGetId() + 1 )
   //TODO: ::nHotKey := NIL
   ::nWinId := nWinId  //20040303

   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   ::bClickBlock   := iif( ValType( bClickBlock ) == "B", bClickBlock, NIL )
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

   IF lDraw  //20040304
      ::Draw( ::nWinId )
   ENDIF

   RETURN Self     //WVWMouseButton

METHOD Enable( lEnable ) CLASS WVWMouseButton

   ::lEnable := lEnable
   ::draw()

   RETURN Self

METHOD SetClickBlock( bBlock ) CLASS WVWMouseButton

   ::bClickBlock := bBlock

   RETURN Self

METHOD SetPressBlock( bBlock ) CLASS WVWMouseButton

   ::bPressBlock := bBlock

   RETURN Self

METHOD SetRepeatPress( lRepeat ) CLASS WVWMouseButton

   ::lRepeatPress := lRepeat

   RETURN Self

METHOD SetCaption( cCaption ) CLASS WVWMouseButton

   ::cCaption := cCaption

   RETURN Self

METHOD OnPress() CLASS WVWMouseButton

   // this is called when LEFT mouse button is pressed on the object
   LOCAL lWasPressed
   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   lWasPressed := ::lPressed
   ::lPressed := .T.
   ::Draw()

   IF ::lRepeatPress // .AND. ::lPressed
      IF ! lWasPressed
         xKeyRepeater( .T. ) //init it
      ENDIF
      wvwm_SetKeyRepeater( .T. )   //activate key repeater
   ENDIF

   IF ValType( ::bPressBlock ) == "B"
      Eval( ::bPressBlock )
   ENDIF

   RETURN Self

METHOD OnClick() CLASS WVWMouseButton

   // this is called when LEFT mouse button is clicked on the object
   // normally (or should it be restricted to be?) called from ::OnRelease()
   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   IF ValType( ::bClickBlock ) == "B"
      Eval( ::bClickBlock )
   ENDIF

   RETURN Self

METHOD OnRelease() CLASS WVWMouseButton

   LOCAL lWasPressed := ::lPressed

   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   ::lPressed := .F.
   ::Draw()

   IF ::lRepeatPress //.AND. ::lPressed
      wvwm_SetKeyRepeater( .F. )   //deactivate key repeater
   ENDIF

   IF lWasPressed
      ::OnClick()
   ENDIF

   RETURN Self

METHOD OnReleaseOut() CLASS WVWMouseButton

   // left button is released outside of mouse region
   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   ::lPressed := .F.
   ::Draw()

   //NOTE: no need to do SetKeyRepeater( .F. ),
   //      because it was already handled by onMouseOut

   RETURN Self

//20040303

METHOD OnMouseOut() CLASS WVWMouseButton

   // mouse is moved from over the button outside
   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   if ::lRepeatPress .AND. ::lPressed
      wvwm_SetKeyRepeater( .F. )   //stop key repeater
   ENDIF

   ::lHover := .F.
   ::Draw()

   RETURN Self

//20040303

METHOD OnMouseOver() CLASS WVWMouseButton

   // mouse is moved to over the button from outside
   IF !::lEnable  //20040303
      RETURN Self
   ENDIF

   if ::lRepeatPress .AND. ::lPressed
      wvwm_SetKeyRepeater( .T. )   //activate key repeater
   ENDIF

   ::lHover := .T.
   ::Draw()

   RETURN Self

METHOD DRAW( nWinNum ) CLASS WVWMouseButton

   LOCAL nROw := Row(), nCol := Col()
   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lMouseOver := ::lHover //20040303,was: ( mrow() >= ::nrow1 .and. mrow() <= ::nrow2 .and. mcol() >= ::ncol1 .and. mcol() <= ::ncol2 )
   LOCAL lPressed := ::lPressed .AND. lMouseOver
   LOCAL aFontInfo := iif( ::nCaptionHeight == NIL, wvw_getFontInfo( nWinNum ), NIL )
   LOCAL nLabelColor := iif( !lPressed, rgb( 0, 0, 0 ), rgb( 96, 96, 96 ) )
   LOCAL lUseImage := ( ValType( ::cImage ) == "C" ) //20040325

   IF !::lVisible .OR. ( ::nType == _BUTTON_NONE )
      SetCursor( nOldCursor ) //20040303
      RETURN Self
   ENDIF

   if ::nrow1 > ::nrow2 .OR. ::ncol1 > ::ncol2
      SetCursor( nOldCursor ) //20040303
      RETURN Self
   ENDIF

   DEFAULT nWinNum to ::nWinId

   IF lPressed //::lPressed
      IF ::nType != _BUTTON_HARD
         WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( hb_ColorToN(::cPressedColor ) ), ::lTight )
         Wvw_DrawBoxRecessed( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )  //wvw
      ELSE
         WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( hb_ColorToN(::cNormalColor ) ), ::lTight )
         Wvw_DrawBoxRaised(   nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
      ENDIF

      IF lUseImage .AND. ::nType != _BUTTON_NONE
         IF !Wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
            win_messagebox( NIL, "Button Failed Wvw_DrawImage(" + ::cImage + ")" )
         ENDIF
      ENDIF

      IF ! Empty( ::cCaption )
         Wvw_DrawLabel( nWinNum, ::nRow1, nCeiling( ( ::nCol2 + ::nCol1 ) / 2 ), ::cCaption, 6, , nLabelColor, rgb( 198,198,198 ), ::cCaptionFont, iif( ValType(afontinfo ) == "A", afontinfo[ 2 ], ::nCaptionHeight ), 0, , , , .F. , .F. )
      ENDIF
   ELSE
      IF lMouseOver .OR. ::nType == _BUTTON_NORMAL .OR. ::nType == _BUTTON_HARD
         WVW_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, WVW_GetRGBcolor( hb_ColorToN(::cNormalColor ) ), ::lTight )
         Wvw_DrawBoxRaised( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
      ELSE
         // must undraw the box. ideally GTWVW has this function
         Wvw_DrawBoxGroup( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2 )
      ENDIF

      IF lUseImage .AND. ::nType != _BUTTON_NONE
         IF !Wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
            win_messagebox( NIL, "Button Failed Wvw_DrawImage(" + ::cImage + ")" )
         ENDIF
      ENDIF

      IF ! ::lEnable
         nLabelColor := rgb( 96, 96, 96 )
      ELSEIF lMouseOver
         nLabelColor := rgb( 255, 0, 0 )
      ENDIF

      IF ! Empty( ::cCaption )
         Wvw_DrawLabel( nWinNum, ::nRow1, nCeiling( ( ::nCol2 + ::nCol1 ) / 2 ), ::cCaption, 6, , nLabelColor, rgb( 198,198,198 ), ::cCaptionFont, iif( ValType( afontinfo ) == "A", afontinfo[ 2 ], ::nCaptionHeight ), 0, , , , .F. , .F. )
      ENDIF
   ENDIF
   SetCursor( nOldCursor )

   RETURN Self

//*******************************************************
// interface functions

FUNCTION wvwm_paint( nWinNum )

   // normally called by WVW_Paint()
   // redraw every mouse object in window nWinNum
   IF Len( s_amouseobjlist ) >= nWinNum + 1
      AEval( s_amouseobjlist[ nWinNum + 1 ], {| o | o[ 2 ]:draw( nWinNum ) } )
   ENDIF

   RETURN NIL

// clears all mouse objects from window nWinNum
FUNCTION wvwm_ResetMouseObjects( nWinNum )

   DO WHILE Len( s_amouseobjlist ) < nWinNum + 1
      AAdd( s_amouseobjlist, {} )
   ENDDO
   s_amouseobjlist[ nWinNum+1 ] := {}

   RETURN .T.

FUNCTION wvwm_AddMouseObjects( nWinNum, oMouse, nObjType )

   // adds a mouse object oMouse into window nWinNum
   DEFAULT nObjType TO _MOBJECT_BUTTON
   AAdd( s_amouseobjlist[ nWinNum + 1 ], { nObjType, oMouse } )

   RETURN .T.

// returns number of mouse objects in window nWinNum
FUNCTION wvwm_nNumMouseObjects( nWinNum )

   RETURN Len( s_amouseobjlist[ nWinNum+1 ] )

// returns type of mouse objects number nObjNum in window nWinNum
FUNCTION wvwm_nObjectType( nWinNum, nObjNum )

   RETURN s_amouseobjlist[ nWinNum+1 ][nObjNum][1]

FUNCTION wvwm_SetKeyRepeater( lSet )

   // returns .T. if KeyRepeater is active
   // if lSet is supplied, KeyRepeater is enable/disable accordingly
   LOCAL lWasSet := ( s_nkeyrepeater != NIL )
   IF lSet != NIL
      IF lSet
         IF !lWasSet
            s_nkeyrepeater := hb_idleAdd( {|| xKeyRepeater() } )
         ENDIF
      ELSE
         IF lWasSet
            hb_idleDel( s_nkeyrepeater )
            s_nkeyrepeater := NIL
         ENDIF
      ENDIF
   ENDIF

   RETURN lWasSet

STATIC FUNCTION nButtonChecker( nkey, oMouseObj )

   LOCAL nrow := MRow(), ncol := MCol()
   LOCAL lMouseOver
   LOCAL i

   lMouseOver := ( nrow >= oMouseObj:nrow1 .AND. nrow <= oMouseObj:nrow2 .AND. ncol >= oMouseObj:ncol1 .AND. ncol <= oMouseObj:ncol2 )
   IF ! lMouseOver
      // cursor is somewhere outside of current mouse object area

      IF oMouseObj:lHover
         // user has just moved the cursor out of this button
         oMouseObj:OnMouseOut()
      ELSE
         DO CASE
         CASE nkey == K_LBUTTONUP
            IF oMouseObj:lPressed
               oMouseObj:OnReleaseOut()
            ENDIF
         ENDCASE
      ENDIF

   ELSE
      // cursor is over current mouse object area

      IF !oMouseObj:lHover
         // user has just moved the cursor into over this button
         oMouseObj:OnMouseOver()
      ELSE
         DO CASE
         CASE nkey == K_LDBLCLK
            // currently button not handle this events,
            // so we will treat it as single key press
            oMouseObj:OnPress()
         CASE nkey == K_LBUTTONDOWN
            oMouseObj:OnPress()
         CASE nkey == K_LBUTTONUP
            oMouseObj:OnRelease()
         ENDCASE
      ENDIF

   ENDIF

   RETURN nkey //nButtonChecker(nkey)

STATIC FUNCTION nScrollChecker( nkey, cType, oMouseObj )

   // cType == "H" or "V"

   nButtonChecker( nkey, oMouseObj:oFirstButton )
   nButtonChecker( nkey, oMouseObj:oRail1Button )
   nButtonChecker( nkey, oMouseObj:oMidButton )
   nButtonChecker( nkey, oMouseObj:oRail2Button )
   nButtonChecker( nkey, oMouseObj:oSecondButton )

   RETURN nkey //nHScrollChecker(nkey)

/* HANDLING MULTIPLE MOUSE OBJECTS */
/* called by SETKEYAFTERBLOCK() function */

FUNCTION wvwm_nMouseChecker( nkey )

   // check mouse events in relations with registered mouse objects
   // always return inkey codes as if nothing happens here
   // (so as to allow GET do something about it)
   // NOTE: only cares about current (last) window
   LOCAL i, oMouseObj
   LOCAL nCurWindow

   nCurWindow := WVW_nNumWindows() - 1

   IF Len( s_amouseobjlist ) < nCurWindow + 1
      RETURN nkey
   ENDIF

   s_ncurkey := nkey   //20040303

   FOR i := 1 TO Len( s_amouseobjlist[ nCurWindow + 1 ] )
      oMouseObj := s_amouseobjlist[ nCurWindow + 1 ][ i ][ 2 ]

      DO CASE
      CASE s_amouseobjlist[ nCurWindow + 1 ][ i ][ 1 ] == _MOBJECT_BUTTON
         nButtonChecker( nkey, oMouseObj )
      CASE s_amouseobjlist[ nCurWindow + 1 ][ i ][ 1 ] == _MOBJECT_HSCROLL
         nScrollChecker( nkey, "H", oMouseObj )
      CASE s_amouseobjlist[ nCurWindow + 1 ][ i ][ 1 ] == _MOBJECT_VSCROLL
         nScrollChecker( nkey, "V", oMouseObj )
      OTHERWISE
         // runtime error!
      ENDCASE

   NEXT

   s_ncurkey := 0  //20040303

   RETURN nKey

STATIC PROCEDURE xKeyRepeater( lInit )

   STATIC s_nLastValidCheck := 0
   STATIC s_lFirstRepeat := .T.

   LOCAL nNow
   LOCAL nRepeatInterval

   DEFAULT lInit TO .F.

   IF lInit
      // simply init the locally static var
      s_lFirstRepeat := .T.
      s_nLastValidCheck := Seconds()
      RETURN
   ENDIF

   nRepeatInterval := iif( s_lFirstRepeat, s_nrepeatdelay, s_nrepeatrate )

   nNow := Seconds()
   IF nNow - s_nLastValidCheck < nRepeatInterval  //s_nrepeatrate
      // not yet
      RETURN
   ENDIF

   IF ! MLeftDown()
      // mouse is not pressed
      RETURN
   ENDIF

   // mouse is down long enough since last valid check
   // let's repeat it
   __Keyboard( K_LBUTTONDOWN )

   // to be more precise
   s_nLastValidCheck := Seconds()   //nNow

   // next repeat will be quicker
   s_lFirstRepeat := .F.

   RETURN //xKeyRepeater()

//*************************** supporters

STATIC FUNCTION nCeiling( nNumber )

   LOCAL nTemp

   nTemp := nNumber - Int( nNumber )  //right of dec point
   IF nTemp > 0
      nNumber := Int( nNumber ) + 1
   ELSE
      nNumber := Int( nNumber )
   ENDIF

   RETURN nNumber
