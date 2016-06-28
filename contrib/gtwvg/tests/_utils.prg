/* Pritpal Bedi <bedipritpal@hotmail.com> */

#include "inkey.ch"
#include "hbgtinfo.ch"

// WvtSetObjects() array structure
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

// WVT_OBJ_TYPE  Constants
#define OBJ_TYPE_BUTTON            1

// WVT_OBJ_STATE
#define OBJ_STATE_HIDE             0
#define OBJ_STATE_DISP             1
#define OBJ_STATE_MOUSEOVER        2
#define OBJ_STATE_BUTTONDOWN       3
#define OBJ_STATE_BUTTONUP         4

THREAD STATIC t_keys_ := {, , , , , , , , , , , , , , , , , , , }
THREAD STATIC t_pic_ := {, , , , , , , , , , , , , , , , , , , }

PROCEDURE WvtSetKeys( lSet )

   IF lSet
      t_keys_[  2 ] := SetKey( K_F2, {|| WvtNextGets()         } )
      t_keys_[  3 ] := SetKey( K_F3, {|| WvtWindowExpand( 1 )  } )
      t_keys_[  4 ] := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
      t_keys_[  5 ] := SetKey( K_F5, {|| WvtMyBrowse()         } )
      t_keys_[  6 ] := SetKey( K_F6, {|| wvt_Minimize()        } )
      t_keys_[  7 ] := SetKey( K_F7, {|| WvtPartialScreen()    } )
      t_keys_[  8 ] := SetKey( K_F8, {|| WvtLines()            } )
      t_keys_[  9 ] := SetKey( K_F9, {|| wvt_ChooseFont()      } )
      t_keys_[ 10 ] := SetKey( K_F10, {|| wvt_ChooseColor()     } )
   ELSE
      SetKey( K_F2,  t_keys_[ 2 ] )
      SetKey( K_F3,  t_keys_[ 3 ] )
      SetKey( K_F4,  t_keys_[ 4 ] )
      SetKey( K_F5,  t_keys_[ 5 ] )
      SetKey( K_F6,  t_keys_[ 6 ] )
      SetKey( K_F7,  t_keys_[ 7 ] )
      SetKey( K_F8,  t_keys_[ 8 ] )
      SetKey( K_F9,  t_keys_[ 9 ] )
      SetKey( K_F10, t_keys_[ 10 ] )
   ENDIF

   RETURN

// wvt_Paint() must be a FUNCTION in your application
// as it is called when Window gets WIN_WM_PAINT message.

FUNCTION wvt_Paint()  /* must be a public function */

   LOCAL aBlocks := WvtSetBlocks()

   AEval( aBlocks, {| e | Eval( e ) } )

   WvtPaintObjects()

   RETURN 0

// wvt_SetFocus() must be a FUNCTION in your application
// needs to process messages sent through WIN_WM_SETFOCUS message
// received by the window.

#if 0

PROCEDURE wvt_SetFocus()  /* must be a public function */

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( 1, 3, "Focus Gained!", "R/W" )

   DevPos( nRow, nCol )

   RETURN

#endif

// wvt_KillFocus() must be a FUNCTION in your application
// needs to process messages sent through WIN_WM_KILLFOCUS message
// received by the window.

#if 0

PROCEDURE wvt_KillFocus()  /* must be a public function */

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( 1, 3, "Focus Lost...", "B/W" )

   DevPos( nRow, nCol )

   RETURN

#endif

// wvt_Mouse() must be present if you want to catch and fire
// mouse call back outside of the Inkey() loop.

PROCEDURE wvt_Mouse( nKey, nRow, nCol )  /* must be a public function */

   STATIC s_nLastObj := 0
   STATIC s_nLastKey := 0

   LOCAL aObjects := WvtSetObjects()
   LOCAL nObj, oObj

   IF Len( aObjects ) == 0
      RETURN
   ENDIF

   IF ! SetMouseCheck()
      RETURN
   ENDIF

   IF nKey == -1000001
      FOR EACH oObj IN aObjects
         SWITCH oObj[ WVT_OBJ_STATE ]
         CASE OBJ_STATE_DISP
            Eval( oObj[ WVT_OBJ_ONDISP ] )
            EXIT
         CASE OBJ_STATE_MOUSEOVER
            Eval( oObj[ WVT_OBJ_ONMOUSEOVER ] )
            EXIT
         CASE OBJ_STATE_BUTTONDOWN
            Eval( oObj[ WVT_OBJ_ONBUTTONDOWN ] )
            EXIT
         CASE OBJ_STATE_BUTTONUP
            Eval( oObj[ WVT_OBJ_ONDISP ] )
            EXIT
         CASE OBJ_STATE_HIDE
            EXIT
         ENDSWITCH
      NEXT
      RETURN
   ENDIF

   nObj := AScan( aObjects, {| e_ | e_[ WVT_OBJ_ROW ] <= nRow .AND. ;
      e_[ WVT_OBJ_ROWTO ] >= nRow .AND. ;
      e_[ WVT_OBJ_COL   ] <= nCol .AND. ;
      e_[ WVT_OBJ_COLTO ] >= nCol } )
   IF nObj == 0
      IF s_nLastObj > 0
         aObjects[ s_nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
         Eval( aObjects[ s_nLastObj, WVT_OBJ_ONDISP ] )
         s_nLastObj := 0
      ENDIF
      RETURN
   ENDIF

   IF s_nLastObj == nObj .AND. s_nLastKey == nKey
      RETURN
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

   RETURN

//  WvtSetBlocks() is a get/set FUNCTION to be used by wvt_Paint()

FUNCTION WvtSetBlocks( a_ )

   THREAD STATIC t := {}

   LOCAL o := AClone( t )

   IF a_ != NIL
      t := AClone( a_ )
   ENDIF

   RETURN o

// WvtSetObjects() is a get/set FUNCTION to be used by wvt_Mouse()

FUNCTION WvtSetObjects( aObject )

   THREAD STATIC t_aObjects := {}

   LOCAL oObjects := AClone( t_aObjects )

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

FUNCTION SetMouseCheck( lYes )

   STATIC s_lSYes := .T.

   LOCAL lOYes := s_lSYes

   IF lYes != NIL
      s_lSYes := lYes
   ENDIF

   RETURN lOYes

FUNCTION WvtWindowExpand( nUnits )

   STATIC s_nUnits := 18

   s_nUnits += nUnits

   wvt_SetFont( "Courier New", s_nUnits )

   RETURN .T.

FUNCTION VouChoice( aChoices )

   LOCAL scr := SaveScreen( 7, 48, 13, 55 )
   LOCAL clr := SetColor( "N/W*,GR+/B*,,,GR+/B" )

   LOCAL nChoice := AChoice( 7, 48, 13, 55, hb_defaultValue( aChoices, { "One", "Two", "Three", "Four", "Five", "Six", "Seven" } ) )

   SetColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice

FUNCTION Hb_Clear()

   CLS

   RETURN .F.

FUNCTION MyMenuProcedure( nID )

   SWITCH nID
   CASE 101
      Alert( "Procedure 101" )
      EXIT
   CASE 102
      Alert( "Procedure 102" )
      EXIT
   ENDSWITCH

   RETURN .T.

FUNCTION BuildWvgToolBar( oDA )

   LOCAL oTBar := WvgToolBar():new( oDA, , { 0, 0 }, { oDA:currentSize()[ 1 ], 30 }, , .T. )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT
   oTBar:borderStyle  := WVGFRAME_RECT

   oTBar:buttonWidth  := 40 // 28
   oTBar:buttonHeight := 26

   oTBar:imageWidth   := 26
   oTBar:imageHeight  := 24

   oTBar:showToolTips := .T.

   // After setting properties, create toolbar.
   oTBar:create()

   oTBar:addItem( "New"       , hb_DirBase() + "v_new.bmp"    )
   oTBar:addItem( "Select"    , hb_DirBase() + "v_selct1.bmp" )
   oTBar:addItem( "Calendar"  , hb_DirBase() + "v_calend.bmp" )
   oTBar:addItem( "Tools"     , hb_DirBase() + "v_lock.bmp"   )
   oTBar:addItem( "Index"     , hb_DirBase() + "v_index.bmp"  )
   oTBar:addItem( "Show"      , hb_DirBase() + "v_clclt.bmp"  )
   oTBar:addItem( "Hide"      , hb_DirBase() + "v_notes1.bmp" )

   RETURN oTBar

FUNCTION SetGT( nIndex, pGT )

   STATIC s_pGT_ := { NIL, NIL, NIL }

   LOCAL oldGT := s_pGT_[ nIndex ]

   IF PCount() == 2
      s_pGT_[ nIndex ] := pGT
   ENDIF

   RETURN oldGT

FUNCTION SetFonts( hFont )

   THREAD STATIC t_ahFonts := {}

   IF ! Empty( hFont )
      AAdd( t_ahFonts, hFont )
   ENDIF

   RETURN t_ahFonts

FUNCTION SetIcons( hIcon )

   THREAD STATIC t_ahIcons := {}

   IF ! Empty( hIcon )
      AAdd( t_ahIcons, hIcon )
   ENDIF

   RETURN t_ahIcons

FUNCTION Popups( nID, lDestroy )

   THREAD STATIC t_hPop_ := { , , , , , , , , }

   LOCAL hPop, hPop1
   LOCAL nPrompt := WIN_MF_ENABLED + WIN_MF_STRING

   IF nID == NIL
      wvt_SetPopupMenu()
      RETURN NIL
   ENDIF

   IF lDestroy != NIL
      wapi_DestroyMenu( t_hPop_[ nID ] )
      RETURN NIL
   ENDIF

   hPop := t_hPop_[ nID ]

   SWITCH nID
   CASE 1   //  Data Entry Module

      IF hPop == NIL
         hPop := wapi_CreatePopupMenu()
         wapi_AppendMenu( hPop, nPrompt, K_F2, "Second Get Screen" )
         wapi_AppendMenu( hPop, nPrompt, K_F3, "Expand Window"     )
         wapi_AppendMenu( hPop, nPrompt, K_F4, "Shrink Window"     )
         wapi_AppendMenu( hPop, nPrompt, K_F5, "Browse"            )
         wapi_AppendMenu( hPop, nPrompt, K_F6, "Minimize"          )
         wapi_AppendMenu( hPop, nPrompt, K_F7, "Partial Screen"    )
         wapi_AppendMenu( hPop, nPrompt, K_F8, "Lines"             )
         wapi_AppendMenu( hPop, nPrompt, K_F9, "Choose Font"       )
         wapi_AppendMenu( hPop, nPrompt, K_F10, "Choose Color"      )

         wapi_AppendMenu( hPop, WIN_MF_SEPARATOR )

         wapi_AppendMenu( hPop, nPrompt, K_F5, "Browse"  )

      ENDIF
      EXIT

   CASE 2   //  Browser

      IF hPop == NIL
         hPop := wapi_CreatePopupMenu()
         wapi_AppendMenu( hPop, nPrompt, K_DOWN     , "Down"      )
         wapi_AppendMenu( hPop, nPrompt, K_UP       , "Up"        )
         wapi_AppendMenu( hPop, nPrompt, K_PGDN     , "Page Down" )
         wapi_AppendMenu( hPop, nPrompt, K_PGUP     , "Page Up"   )
         wapi_AppendMenu( hPop, nPrompt, K_CTRL_PGUP, "Top"       )
         wapi_AppendMenu( hPop, nPrompt, K_CTRL_PGDN, "Bottom"    )

         wapi_AppendMenu( hPop, WIN_MF_SEPARATOR )

         hPop1 := wapi_CreatePopupMenu()
         wapi_AppendMenu( hPop1, nPrompt, K_RIGHT   , "Right"     )
         wapi_AppendMenu( hPop1, nPrompt, K_LEFT    , "Left"      )
         wapi_AppendMenu( hPop1, nPrompt, K_END     , "End"       )
         wapi_AppendMenu( hPop1, nPrompt, K_HOME    , "Home"      )

         wapi_AppendMenu( hPop, WIN_MF_ENABLED + WIN_MF_POPUP, hPop1, "Column Movement" )

      ENDIF
      EXIT

   ENDSWITCH

   t_hPop_[ nID ] := hPop

   RETURN wvt_SetPopupMenu( t_hPop_[ nID ] )

FUNCTION DispStatusMsg( cMsg )

   wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, WIN_RGB( 198, 198, 198 ), "Arial", 18, , 900 )

   RETURN .T.

FUNCTION ClearStatusMsg()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( MaxRow(), 42, Space( 37 ), "W/W" )

   SetPos( nRow, nCol )

   RETURN .T.

PROCEDURE WvtPictures( nSlot, cFilePic )

   IF HB_ISNUMERIC( nSlot ) .AND. nSlot <= 20 .AND. hb_vfExists( cFilePic )
      IF !( t_pic_[ nSlot ] == cFilePic )
         IF wvt_LoadPicture( cFilePic, nSlot )
            t_pic_[ nSlot ] := cFilePic
         ENDIF
      ENDIF
   ENDIF

   RETURN

PROCEDURE WvtExePicture( nTop, nLeft, nBottom, nRight, nSlot, aOffset )

   IF t_pic_[ nSlot ] != NIL
      wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aOffSet )
   ENDIF

   RETURN

FUNCTION GetResource( cName )
   RETURN hb_DirBase() + cName

PROCEDURE uiDebug( ... )

   LOCAL aP := hb_AParams()
   LOCAL s := ""

   AEval( aP, {| e | s += hb_ValToStr( e ) + "   " } )

   wapi_OutputDebugString( s )

   RETURN

PROCEDURE MyError( oError )

   ? oError:description
   ? oError:operation

   ? ProcName( 1 ), ProcLine( 1 )
   ? ProcName( 2 ), ProcLine( 2 )
   ? ProcName( 3 ), ProcLine( 3 )
   ? ProcName( 4 ), ProcLine( 4 )
   DO WHILE hb_keyStd( Inkey( 0 ) ) != K_ESC
   ENDDO

   RETURN
