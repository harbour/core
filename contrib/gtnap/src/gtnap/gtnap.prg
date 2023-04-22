/*
    gtnap bindings to use in Harbour programs
*/

#include "hbclass.ch"

CREATE CLASS NAPListener

   VAR nNumber                       // 20040303, parent window's number
   VAR lVisible                     // is the object visible
   VAR lEnable                      // 20040303, is the object enable
   VAR lTight                       // allow tight neighboring
   VAR nType                        // 20040303, appearance of this button
   VAR nRow1, nCol1, nRow2, nCol2   // mouse object region

   VAR bClickBlock                  // executed on Left Click
   VAR bPressBlock                  // executed on Left Press

   VAR lRepeatPress                 // repeat Left Press when pressed during mouse over?

   VAR cCaption
   VAR cCaptionFont                 // font name for caption
   VAR nCaptionHeight               // height of font for caption, if NIL use current wvw_GetFontInfo()
   VAR cImage                       // 20040325, image file name

   VAR cNormalColor    // button normal color, pls use single color, eg "W"
   VAR cPressedColor   // button pressed color, pls use single color, eg "B"

   // private DATA, should be protected
   VAR lPressed                     // is it being pressed by Left Button?
   VAR lHover                       // 20040303, is mouse over the button?

   // METHODS
   METHOD Create( nNumber, OnClick )

ENDCLASS   // WVWMouseButton

METHOD Create( nNumber, OnClick ) CLASS NAPListener

    ::nNumber := nNumber
    ::bClickBlock := iif( HB_ISBLOCK( OnClick ), OnClick, NIL )
//    hb_default( @cCaption, "" ) // 20040325,was: "Button"
//    hb_default( @nRow1, 0 )
//    hb_default( @nCol1, 0 )
//    hb_default( @nRow2, nRow1 )
//    hb_default( @nCol2, nCol1 + Max( 10, Len( cCaption ) + 2 ) - 1 )
//    hb_default( @nType, _BUTTON_NORMAL )        // 20040303
//    hb_default( @lDraw, .T. )
//    hb_default( @nWinId, wvw_nNumWindows() - 1 )  // 20040303

//    // TODO: ::nId := iif( Empty( s_amouseobjlist ), 1, s_amouseobjlist[ Len( s_amouseobjlist ) ]:nGetId() + 1 )
//    // TODO: ::nHotKey := NIL
//    ::nWinId := nWinId  // 20040303

//    ::nRow1 := nRow1
//    ::nCol1 := nCol1
//    ::nRow2 := nRow2
//    ::nCol2 := nCol2

//    ::bClickBlock   := iif( HB_ISBLOCK( bClickBlock ), bClickBlock, NIL )
//    ::bPressBlock   := NIL

//    ::lRepeatPress  := .F.

//    ::lPressed  := .F.
//    ::lHover    := .F.  // 20040303
//    ::cCaption := cCaption
//    ::cCaptionFont := _DEFAULT_CAPTION_FONT
//    ::nCaptionHeight := _DEFAULT_CAPTION_HEIGHT

//    ::cImage    := NIL  // 20040325

//    // TODO: pls use current color
//    ::cNormalColor := "W"
//    ::cPressedColor := "W"

//    ::lVisible := .T.
//    ::lEnable  := .T.
//    ::lTight   := .F.
//    ::nType := nType

//    IF lDraw  // 20040304
//       ::Draw( ::nWinId )
//    ENDIF

   RETURN Self     // WVWMouseButton
