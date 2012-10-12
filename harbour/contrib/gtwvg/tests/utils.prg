/*
 * $Id$
 */

/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */

//

#include "inkey.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

//
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

//

THREAD STATIC t_keys_ := { , , , , , , , , , , , , , , , , , , , }
THREAD STATIC t_pic_ := { , , , , , , , , , , , , , , , , , , , }

//

FUNCTION WvtSetKeys( lSet )

   IF lSet
      t_keys_[ 2 ] := SetKey( K_F2, {|| WvtNextGets()         } )
      t_keys_[ 3 ] := SetKey( K_F3, {|| WvtWindowExpand( 1 )  } )
      t_keys_[ 4 ] := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
      t_keys_[ 5 ] := SetKey( K_F5, {|| WvtMyBrowse()         } )
      t_keys_[ 6 ] := SetKey( K_F6, {|| Wvt_Minimize()        } )
      t_keys_[ 7 ] := SetKey( K_F7, {|| WvtPartialScreen()    } )
      t_keys_[ 8 ] := SetKey( K_F8, {|| WvtLines()            } )
      t_keys_[ 9 ] := SetKey( K_F9, {|| Wvt_ChooseFont()      } )
      t_keys_[ 10] := SetKey( K_F10, {|| Wvt_ChooseColor()     } )
   ELSE
      SetKey( K_F2,  t_keys_[ 2 ] )
      SetKey( K_F3,  t_keys_[ 3 ] )
      SetKey( K_F4,  t_keys_[ 4 ] )
      SetKey( K_F5,  t_keys_[ 5 ] )
      SetKey( K_F6,  t_keys_[ 6 ] )
      SetKey( K_F7,  t_keys_[ 7 ] )
      SetKey( K_F8,  t_keys_[ 8 ] )
      SetKey( K_F9,  t_keys_[ 9 ] )
      SetKey( K_F10, t_keys_[ 10] )
   ENDIF

   RETURN NIL

//
//      Wvt_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//

FUNCTION Wvt_Paint()

   LOCAL aBlocks := WvtSetBlocks()

   AEval( aBlocks, {| e | Eval( e ) } )

   WvtPaintObjects()

   RETURN 0

//
//      Wvt_SetFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_SETFOCUS message
//      received by the window.
//
#if 0

FUNCTION Wvt_SetFocus()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( 1, 3, "Focus Gained!", "R/W" )

   DevPos( nRow, nCol )

   RETURN NIL

#endif

//
//      Wvt_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//
#if 0

FUNCTION Wvt_KillFocus()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( 1, 3, "Focus Lost...", "B/W" )

   DevPos( nRow, nCol )

   RETURN NIL

#endif

//
//
//      Wvt_Mouse() must be present if you want to catch and fire
//      mouse call back outside of the inkey() loop.
//
//

FUNCTION Wvt_Mouse( nKey, nRow, nCol )

   LOCAL nLen, aObjects := WvtSetObjects()
   LOCAL nObj

   STATIC s_nLastObj := 0
   STATIC s_nLastKey := 0

   IF ( nLen := Len( aObjects ) ) == 0
      RETURN NIL
   ENDIF

   IF ! SetMouseCheck()
      RETURN NIL
   ENDIF

   IF nKey == -1000001
      FOR nObj := 1 TO nLen
         DO CASE
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_DISP
            Eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_MOUSEOVER
            Eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONDOWN
            Eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONUP
            Eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
         CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_HIDE

         ENDCASE
      NEXT
      RETURN NIL
   ENDIF

   nObj := AScan( aObjects, {| e_ | e_[ WVT_OBJ_ROW   ] <= nRow .AND. ;
      e_[ WVT_OBJ_ROWTO ] >= nRow .AND. ;
      e_[ WVT_OBJ_COL   ] <= nCol .AND. ;
      e_[ WVT_OBJ_COLTO ] >= nCol     } )
   IF nObj == 0
      IF s_nLastObj > 0
         aObjects[ s_nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
         Eval( aObjects[ s_nLastObj, WVT_OBJ_ONDISP ] )
         s_nLastObj := 0
      ENDIF
      RETURN NIL
   ENDIF

   IF s_nLastObj == nObj .AND. s_nLastKey == nKey
      RETURN NIL
   ENDIF

   s_nLastObj := nObj
   s_nLastKey := nKey

   DO CASE
   CASE nKey == K_MOUSEMOVE
      IF aObjects[ s_nLastObj, WVT_OBJ_STATE ] != OBJ_STATE_MOUSEOVER
         aObjects[ s_nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_MOUSEOVER
         IF aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] != NIL
            Eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
         ENDIF
      ENDIF

   CASE nKey == K_LBUTTONDOWN
      aObjects[ s_nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_BUTTONDOWN
      IF aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] != NIL
         Eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
      ENDIF

   CASE nKey == K_LBUTTONUP
      aObjects[ s_nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
      IF aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] != NIL
         Eval( aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] )
      ENDIF

   ENDCASE

   RETURN NIL

//
//  WvtSetBlocks() is a get/set FUNCTION to be used by Wvt_Paint()
//

FUNCTION WvtSetBlocks( a_ )

   LOCAL o

   THREAD STATIC t := {}

   o := AClone( t )

   IF a_ != NIL
      t := AClone( a_ )
   ENDIF

   RETURN o

//
//  WvtSetObjects() is a get/set FUNCTION to be used by Wvt_Mouse()
//

FUNCTION WvtSetObjects( aObject )

   LOCAL oObjects

   THREAD STATIC t_aObjects := {}

   oObjects := AClone( t_aObjects )

   IF aObject != NIL
      IF Empty( aObject )
         t_aObjects := {}
      ELSE
         IF HB_ISARRAY( aObject[ 1 ] )
            AEval( aObject, {| e_ | AAdd( t_aObjects, e_ ) } )
         ELSE
            ASize( aObject, WVT_OBJ_VRBLS )

            hb_default( @aObject[ WVT_OBJ_STATE ], OBJ_STATE_DISP )

            AAdd( t_aObjects, aObject )
         ENDIF
      ENDIF
   ENDIF

   RETURN oObjects

//

FUNCTION SetMouseCheck( lYes )

   LOCAL lOYes
   STATIC s_lSYes := .T.

   lOYes := s_lSYes
   IF lYes != NIL
      s_lSYes := lYes
   ENDIF

   RETURN lOYes

//

FUNCTION WvtWindowExpand( nUnits )

   STATIC s_nUnits := 18

   s_nUnits += nUnits

   Wvt_setFont( "Courier New", s_nUnits )

   RETURN .T.

//

FUNCTION rgb( r, g, b )

   RETURN r + ( g * 256 ) + ( b * 256 * 256 )

//

FUNCTION VouChoice( aChoices )

   LOCAL scr, clr, nChoice

   hb_default( @aChoices, { "One", "Two", "Three", "Four", "Five", "Six", "Seven" } )

   scr := SaveScreen( 7, 48, 13, 55 )
   clr := SetColor( "N/W*,GR+/B*,,,GR+/B" )

   nChoice := AChoice( 7, 48, 13, 55, aChoices )

   SetColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice

//

FUNCTION Hb_Clear()

   CLS

   RETURN .F.

//

FUNCTION MyMenuProcedure( nID )

   DO CASE
   CASE nID == 101
      Alert( 'Procedure 101' )
   CASE nID == 102
      Alert( 'Procedure 102' )
   ENDCASE

   RETURN .T.

//

FUNCTION BuildWvgToolBar( oDA, nActiveX )

   LOCAL oTBar

   hb_default( @nActiveX, 0 )

   oTBar := WvgToolBar():new( oDA, , { 0, 0 }, { oDA:currentSize()[ 1 ], 30 }, , .T. )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT
   oTBar:borderStyle  := WVGFRAME_RECT

   oTBar:buttonWidth  := 40 //28
   oTBar:buttonHeight := 26

   oTBar:imageWidth   := 26
   oTBar:imageHeight  := 24

   oTBar:showToolTips := .T.

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

//

FUNCTION SetGT( nIndex, pGT )

   LOCAL oldGT
   STATIC s_pGT_ := { NIL, NIL, NIL }

   oldGT := s_pGT_[ nIndex ]
   IF PCount() == 2
      s_pGT_[ nIndex ] := pGT
   ENDIF

   RETURN oldGT

//

FUNCTION SetFonts( hFont )

   LOCAL oldFont

   THREAD STATIC t_ahFonts := {}
   oldFont := t_ahFonts
   IF !Empty( hFont )
      AAdd( t_ahFonts, hFont )
   ENDIF

   RETURN oldFont

//

FUNCTION SetIcons( hIcon )

   LOCAL oldIcon

   THREAD STATIC t_ahIcons := {}
   oldIcon := t_ahIcons
   IF !Empty( hIcon )
      AAdd( t_ahIcons, hIcon )
   ENDIF

   RETURN oldIcon

//

FUNCTION Popups( nID, lDestroy )

   LOCAL hPop, hPop1
   LOCAL nPrompt := MF_ENABLED + MF_STRING

   THREAD STATIC t_hPop_ := { , , , , , , , , }

   IF nID == NIL
      Wvt_SetPopupMenu()
      RETURN NIL
   ENDIF

   IF lDestroy != NIL
      Wvt_DestroyMenu( t_hPop_[ nID ] )
      RETURN NIL
   ENDIF

   hPop := t_hPop_[ nID ]

   DO CASE
   CASE nID == 1   //  Data Entry Module

      IF hPop == NIL
         hPop := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop, nPrompt, K_F2, "Second Get Screen" )
         Wvt_AppendMenu( hPop, nPrompt, K_F3, "Expand Window"     )
         Wvt_AppendMenu( hPop, nPrompt, K_F4, "Shrink Window"     )
         Wvt_AppendMenu( hPop, nPrompt, K_F5, "Browse"            )
         Wvt_AppendMenu( hPop, nPrompt, K_F6, "Minimize"          )
         Wvt_AppendMenu( hPop, nPrompt, K_F7, "Partial Screen"    )
         Wvt_AppendMenu( hPop, nPrompt, K_F8, "Lines"             )
         Wvt_AppendMenu( hPop, nPrompt, K_F9, "Choose Font"       )
         Wvt_AppendMenu( hPop, nPrompt, K_F10, "Choose Color"      )

         Wvt_AppendMenu( hPop, MF_SEPARATOR )

         Wvt_AppendMenu( hPop, nPrompt, K_F5, "Browse"  )

      ENDIF

   CASE nID == 2   //  Browser

      IF hPop == NIL
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

         Wvt_AppendMenu( hPop, MF_ENABLED + MF_POPUP, hPop1, "Column Movement" )

      ENDIF

   ENDCASE

   t_hPop_[ nID ] := hPop

   RETURN Wvt_SetPopupMenu( t_hPop_[ nID ] )

//

FUNCTION DispStatusMsg( cMsg )

   Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, rgb( 198,198,198 ), "Arial", 18, , 900 )

   RETURN .T.

//

FUNCTION ClearStatusMsg()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( MaxRow(), 42, Space( 37 ), "W/W" )

   SetPos( nRow, nCol )

   RETURN .T.

//

FUNCTION WvtPictures( nSlot, cFilePic )

   IF nSlot != NIL .AND. nSlot <= 20 .AND. hb_FileExists( cFilePic )
      IF !( t_pic_[ nSlot ] == cFilePic )
         IF Wvt_LoadPicture( cFilePic, nSlot )
            t_pic_[ nSlot ] := cFilePic
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

//

FUNCTION WvtExePicture( nTop, nLeft, nBottom, nRight, nSlot, aOffset )

   IF t_pic_[ nSlot ] != NIL
      Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aOffSet )
   ENDIF

   RETURN NIL

//

FUNCTION GetResource( cName )

   RETURN hb_DirBase() + cName

//

FUNCTION uiDebug( ... )

   LOCAL aP := hb_AParams()
   LOCAL s := ""

   AEval( aP, {| e | s += hb_ValToStr( e ) + "   " } )

   WAPI_OutputDebugString( s )

   RETURN NIL

//

FUNCTION MyError( oError )

   ? oError:description
   ? oError:operation

   ? ProcName( 1 ), ProcLine( 1 )
   ? ProcName( 2 ), ProcLine( 2 )
   ? ProcName( 3 ), ProcLine( 3 )
   ? ProcName( 4 ), ProcLine( 4 )
   DO WHILE Inkey() != K_ESC
   ENDDO

   RETURN NIL

//
