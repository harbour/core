/*
 * $Id$
 */

/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */
/*----------------------------------------------------------------------*/

#include "inkey.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/
//
//   WvtSetObjects() array structure
//
#define WVT_OBJ_TYPE               1
#define WVT_OBJ_ID                 2
#define WVT_OBJ_ROW                3
#define WVT_OBJ_COL                4
#define WVT_OBJ_ROWTO              5
#define WVT_OBJ_COLTO              6
#define WVT_OBJ_ONDISP             7
#define WVT_OBJ_ONMOUSEOVER        8
#define WVT_OBJ_ONBUTTONDOWN       9
#define WVT_OBJ_ONBUTTONUP        10
#define WVT_OBJ_TOOLTIP           11
#define WVT_OBJ_STATE             12
#define WVT_OBJ_DUMMY             13

#define WVT_OBJ_VRBLS             13

//   WVT_OBJ_TYPE  Constants
//
#define OBJ_TYPE_BUTTON            1

//   WVT_OBJ_STATE
//
#define OBJ_STATE_HIDE             0
#define OBJ_STATE_DISP             1
#define OBJ_STATE_MOUSEOVER        2
#define OBJ_STATE_BUTTONDOWN       3
#define OBJ_STATE_BUTTONUP         4

/*----------------------------------------------------------------------*/

thread static t_keys_:= { , , , , , , , , , , , , , , , , , , , }
thread static t_pic_ := { , , , , , , , , , , , , , , , , , , , }

/*----------------------------------------------------------------------*/

FUNCTION WvtSetKeys( lSet )

   if lSet
      t_keys_[ 2 ] := SetKey( K_F2, {|| WvtNextGets()         } )
      t_keys_[ 3 ] := SetKey( K_F3, {|| WvtWindowExpand( 1 )  } )
      t_keys_[ 4 ] := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
      t_keys_[ 5 ] := SetKey( K_F5, {|| WvtMyBrowse()         } )
      t_keys_[ 6 ] := SetKey( K_F6, {|| Wvt_Minimize()        } )
      t_keys_[ 7 ] := SetKey( K_F7, {|| WvtPartialScreen()    } )
      t_keys_[ 8 ] := SetKey( K_F8, {|| WvtLines()            } )
      t_keys_[ 9 ] := SetKey( K_F9, {|| Wvt_ChooseFont()      } )
      t_keys_[ 10] := SetKey( K_F10,{|| Wvt_ChooseColor()     } )
   else
      SetKey( K_F2,  t_keys_[ 2 ] )
      SetKey( K_F3,  t_keys_[ 3 ] )
      SetKey( K_F4,  t_keys_[ 4 ] )
      SetKey( K_F5,  t_keys_[ 5 ] )
      SetKey( K_F6,  t_keys_[ 6 ] )
      SetKey( K_F7,  t_keys_[ 7 ] )
      SetKey( K_F8,  t_keys_[ 8 ] )
      SetKey( K_F9,  t_keys_[ 9 ] )
      SetKey( K_F10, t_keys_[ 10] )
   endif

   RETURN Nil
//-------------------------------------------------------------------//
//      Wvt_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//-------------------------------------------------------------------//
FUNCTION Wvt_Paint()
   LOCAL aBlocks := WvtSetBlocks()

   aEval( aBlocks, {|e| eval( e ) } )

   WvtPaintObjects()

   RETURN 0
//-------------------------------------------------------------------//
//      Wvt_SetFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_SETFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
#if 0
FUNCTION Wvt_SetFocus()

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, "Focus Gained!", "R/W" )

   DevPos( nRow, nCol )

   RETURN nil
#endif
//-------------------------------------------------------------------//
//      Wvt_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
#if 0
FUNCTION Wvt_KillFocus()

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, "Focus Lost...", "B/W" )

   DevPos( nRow, nCol )

   RETURN nil
#endif

//-------------------------------------------------------------------//
//
//      Wvt_Mouse() must be present if you want to catch and fire
//      mouse call back outside of the inkey() loop.
//
//-------------------------------------------------------------------//

FUNCTION Wvt_Mouse( nKey, nRow, nCol )
   LOCAL nLen, aObjects := WvtSetObjects()
   LOCAL nObj

   STATIC nLastObj := 0
   STATIC nLastKey := 0

   if ( nLen := len( aObjects ) ) == 0
      return nil
   endif

   if !SetMouseCheck()
      return nil
   endif

   if nKey == -1000001
      for nObj :=   1 to nLen
         DO CASE
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_DISP
            eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_MOUSEOVER
            eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONDOWN
            eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONUP
            eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_HIDE

         ENDCASE
      next
      return nil
   endif

   nObj := ascan( aObjects, {|e_| e_[ WVT_OBJ_ROW   ] <= nRow .and. ;
                                  e_[ WVT_OBJ_ROWTO ] >= nRow .and. ;
                                  e_[ WVT_OBJ_COL   ] <= nCol .and. ;
                                  e_[ WVT_OBJ_COLTO ] >= nCol     } )
   if nObj == 0
      if nLastObj > 0
         aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
         eval( aObjects[ nLastObj, WVT_OBJ_ONDISP ] )
         nLastObj := 0
      endif
      return nil
   endif

   if nLastObj == nObj .and. nLastKey == nKey
      return nil
   endif

   nLastObj := nObj
   nLastKey := nKey

   DO CASE
   CASE nKey == K_MOUSEMOVE
      if aObjects[ nLastObj, WVT_OBJ_STATE ] != OBJ_STATE_MOUSEOVER
           aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_MOUSEOVER
         if aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] != nil
            eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
         endif
      endif

   CASE nKey == K_LBUTTONDOWN
        aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_BUTTONDOWN
        if aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] != nil
           eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
      endif

   CASE nKey == K_LBUTTONUP
        aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
        if aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] != nil
           eval( aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] )
      endif

   ENDCASE

   RETURN NIL

//-------------------------------------------------------------------//
//  WvtSetBlocks() is a get/set FUNCTION to be used by Wvt_Paint()
//-------------------------------------------------------------------//

FUNCTION WvtSetBlocks( a_ )

   LOCAL o
   THREAD STATIC t := {}

   o := aclone( t )

   IF a_ != nil
      t := aclone( a_ )
   ENDIF

   RETURN o

//-------------------------------------------------------------------//
//  WvtSetObjects() is a get/set FUNCTION to be used by Wvt_Mouse()
//-------------------------------------------------------------------//

FUNCTION WvtSetObjects( aObject )

   LOCAL oObjects
   THREAD STATIC aObjects := {}

   oObjects := aclone( aObjects )

   if aObject != nil
      if empty( aObject )
         aObjects := {}
      else
         if valtype( aObject[ 1 ] ) == "A"
            aeval( aObject, {|e_| aadd( aObjects, e_ ) } )
         else
            aSize( aObject, WVT_OBJ_VRBLS )

            DEFAULT aObject[ WVT_OBJ_STATE ] TO OBJ_STATE_DISP

            aadd( aObjects, aObject )
         endif
      endif
   endif

   RETURN oObjects

//-------------------------------------------------------------------//

FUNCTION SetMouseCheck( lYes )
   LOCAL lOYes
   STATIC lSYes := .t.

   lOYes := lSYes
   if lYes != nil
      lSYes := lYes
   endif

   RETURN lOYes

//-------------------------------------------------------------------//

FUNCTION WvtWindowExpand( nUnits )

   STATIC sUnits := 18

   sUnits += nUnits

   Wvt_setFont( "Courier New", sUnits )

   RETURN .t.

//-------------------------------------------------------------------//

FUNCTION rgb( r,g,b )
   RETURN r + ( g * 256 ) + ( b * 256 * 256 )

//-------------------------------------------------------------------//

FUNCTION VouChoice( aChoices )
   LOCAL scr, clr, nChoice

   DEFAULT aChoices TO { "One","Two","Three","Four","Five","Six","Seven" }

   scr := SaveScreen( 7,48,13,55 )
   clr := SetColor( "N/W*,GR+/B*,,,GR+/B" )

   nChoice := aChoice( 7, 48, 13, 55, aChoices )

   setColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice

//-------------------------------------------------------------------//

FUNCTION Hb_Clear()
   CLS
   RETURN .f.

//----------------------------------------------------------------------//

FUNCTION MyMenuProcedure( nID )
   do case
   case nID == 101
      alert( 'Procedure 101' )
   case nID == 102
      alert( 'Procedure 102' )
   endcase
   Return .t.

//----------------------------------------------------------------------//

FUNCTION BuildWvgToolBar( oDA, nActiveX )
   LOCAL oTBar

   DEFAULT nActiveX TO 0

   oTBar := WvgToolBar():new( oDA, , { 0,0 }, { oDA:currentSize()[ 1 ], 30 }, , .T. )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT
   oTBar:borderStyle  := WVGFRAME_RECT

   oTBar:buttonWidth  := 40 //28
   oTBar:buttonHeight := 26

   oTBar:imageWidth   := 26
   oTBar:imageHeight  := 24

   oTBar:showToolTips := .t.

   // After setting properties, create toolbar.
   oTBar:create()

   oTBar:addItem( "New"       , hb_DirBase() + 'v_new.bmp'    )
   oTBar:addItem( "Select"    , hb_DirBase() + 'v_selct1.bmp' )
   oTBar:addItem( "Calendar"  , hb_DirBase() + 'v_calend.bmp' )
   oTBar:addItem( "Tools"     , hb_DirBase() + 'v_lock.bmp'   )
   oTBar:addItem( "Index"     , hb_DirBase() + 'v_index.bmp'  )
   oTBar:addItem( "Show"      , hb_DirBase() + 'v_clclt.bmp'  )
   oTBar:addItem( "Hide"      , hb_DirBase() + 'v_notes1.bmp' )

   RETURN oTBar

//----------------------------------------------------------------------//

FUNCTION SetGT( nIndex, pGT )
   LOCAL oldGT
   STATIC pGT_:= { NIL, NIL, NIL }
   oldGT := pGT_[ nIndex ]
   IF PCount() == 2
      pGT_[ nIndex ] := pGT
   ENDIF
   RETURN oldGT

/*----------------------------------------------------------------------*/

FUNCTION SetFonts( hFont )
   LOCAL oldFont
   thread STATIC t_ahFonts := {}
   oldFont := t_ahFonts
   IF !empty( hFont )
      aadd( t_ahFonts, hFont )
   ENDIF
   RETURN oldFont

/*----------------------------------------------------------------------*/

FUNCTION SetIcons( hIcon )
   LOCAL oldIcon
   thread STATIC t_ahIcons := {}
   oldIcon := t_ahIcons
   IF !empty( hIcon )
      aadd( t_ahIcons, hIcon )
   ENDIF
   RETURN oldIcon

/*----------------------------------------------------------------------*/

FUNCTION Popups( nID, lDestroy )
   LOCAL hPop, hPop1
   LOCAL nPrompt := MF_ENABLED+MF_STRING

   THREAD STATIC hPop_:= { , , , , , , , , }

   if nID == nil
      Wvt_SetPopupMenu()
      return nil
   endif

   if lDestroy != nil
      Wvt_DestroyMenu( hPop_[ nID ] )
      return nil
   endif

   hPop := hPop_[ nID ]

   do case
   case nID == 1   //  Data Entry Module

      if hPop == nil
         hPop := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop, nPrompt, K_F2, "Second Get Screen" )
         Wvt_AppendMenu( hPop, nPrompt, K_F3, "Expand Window"     )
         Wvt_AppendMenu( hPop, nPrompt, K_F4, "Shrink Window"     )
         Wvt_AppendMenu( hPop, nPrompt, K_F5, "Browse"            )
         Wvt_AppendMenu( hPop, nPrompt, K_F6, "Minimize"          )
         Wvt_AppendMenu( hPop, nPrompt, K_F7, "Partial Screen"    )
         Wvt_AppendMenu( hPop, nPrompt, K_F8, "Lines"             )
         Wvt_AppendMenu( hPop, nPrompt, K_F9, "Choose Font"       )
         Wvt_AppendMenu( hPop, nPrompt, K_F10,"Choose Color"      )

         Wvt_AppendMenu( hPop, MF_SEPARATOR )

         Wvt_AppendMenu( hPop, nPrompt, K_F5, "Browse"  )

      endif

   case nID == 2   //  Browser

      if hPop == nil
         hPop := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop, nPrompt, K_DOWN     , "Down"      )
         Wvt_AppendMenu( hPop, nPrompt, K_UP       , "Up"        )
         Wvt_AppendMenu( hPop, nPrompt, K_PGDN     , "Page Down" )
         Wvt_AppendMenu( hPop, nPrompt, K_PGUP     , "Page Up"   )
         Wvt_AppendMenu( hPop, nPrompt, K_CTRL_PGUP, "Top"       )
         Wvt_AppendMenu( hPop, nPrompt, K_CTRL_PGDN, "Bottom"    )

         Wvt_AppendMenu( hPop, MF_SEPARATOR )

         hPop1 := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop1, nPrompt, K_RIGHT   , "Right"     )
         Wvt_AppendMenu( hPop1, nPrompt, K_LEFT    , "Left"      )
         Wvt_AppendMenu( hPop1, nPrompt, K_END     , "End"       )
         Wvt_AppendMenu( hPop1, nPrompt, K_HOME    , "Home"      )

         Wvt_AppendMenu( hPop, MF_ENABLED+MF_POPUP, hPop1, "Column Movement" )

      endif

   endcase

   hPop_[ nID ] := hPop

   RETURN Wvt_SetPopupMenu( hPop_[ nID ] )

//-------------------------------------------------------------------//

FUNCTION DispStatusMsg( cMsg )

   Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, rgb(198,198,198), "Arial", 18, , 900 )

   RETURN .t.

//-------------------------------------------------------------------//

FUNCTION ClearStatusMsg()
   LOCAL nRow := Row()
   LOCAL nCol := Col()

   DispOutAt( MaxRow(), 42, space( 37 ), "W/W" )

   SetPos( nRow, nCol )

   RETURN .t.

//-------------------------------------------------------------------//

FUNCTION WvtPictures( nSlot,cFilePic )

   if nSlot != nil .and. nSlot <= 20 .and. file( cFilePic )
      if t_pic_[ nSlot ] != cFilePic
         if Wvt_LoadPicture( cFilePic, nSlot )
            t_pic_[ nSlot ] := cFilePic
         endif
      endif
   endif

   RETURN NIL

//-------------------------------------------------------------------//

FUNCTION WvtExePicture( nTop, nLeft, nBottom, nRight, nSlot, aOffset )

   if t_pic_[ nSlot ] != nil
      Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aOffSet )
   endif

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION GetResource( cName )
   RETURN hb_dirBase() + cName

/*----------------------------------------------------------------------*/

