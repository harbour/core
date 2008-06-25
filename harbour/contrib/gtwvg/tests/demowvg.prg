/*
 * $Id$
 */

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                   GTWVT Console GUI Interface
//
//               Pritpal Bedi <pritpal@vouchcac.com>
//
//       I have tried to simulate the gui controls through GDI
//        functions and found a way to refresh those controls
//          through WM_PAINT message issued to the Window.
//                               and
//             I feel that IF this functionality is built
//                 into the GT itself, what a wonder!
//
//   This protocol opens up the the distinct possibilities and hope
//            you all will cooperate to enhance it further.
//
//           Thanks Peter Rees! You have laid the foundation!
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include    'inkey.ch'
#include   'common.ch'
#include   'wvtwin.ch'

REQUEST DbfCdx

//-------------------------------------------------------------------//
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

//-------------------------------------------------------------------//

#define  IMAGE_VOUCH          'vouch1.bmp'
#define  IMAGE_BROWSE         'v_browse.ico'
#define  IMAGE_VR             'vr_1.ico'
#define  IMAGE_NOTES          'v_notes.ico'
#define  IMAGE_TOOLS          'v_tools.ico'
#define  IMAGE_HELP           'v_notes.ico'

//-------------------------------------------------------------------//
//
//   Candidate for inkey.ch
//
#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

//-------------------------------------------------------------------//

#ifndef __SQL__
//ANNOUNCE Hb_NoStartUpWindow
#endif

//-------------------------------------------------------------------//

#define CRLF   chr( 13 )+chr( 10 )

//-------------------------------------------------------------------//

MEMVAR cCdxExp, First, Last, City

//-------------------------------------------------------------------//

static wvtScreen := {}
static pic_:= { , , , , , , , , , , , , , , , , , , , }
static keys_:= { , , , , , , , , , , , , , , , , , , , }

static ahFonts := {}
static shIcon, shImage
static aSlides := {}

#ifdef __XCC__
static paint_:= { { '', {} } }
#endif

//-------------------------------------------------------------------//
PROCEDURE Main( cDSN )

   LOCAL aLastPaint, clr, scr, bWhen, bValid, a_:={}
   LOCAL dDate   := ctod( '' )
   LOCAL cName   := Pad( 'Pritpal Bedi', 35 )
   LOCAL cAdd1   := Pad( '60, New Professor Colony', 35 )
   LOCAL cAdd2   := Pad( 'Ludhiana, INDIA', 35 )
   LOCAL cAdd3   := Pad( 'http://www.vouchcac.com', 35 )
   LOCAL nSlry   := 20000
   LOCAL aBlocks := {}
   LOCAL nColGet := 8
   LOCAL GetList := {}
   LOCAL nTop    := 4
   LOCAL nLft    := 4
   LOCAL nBtm    := 20
   LOCAL nRgt    := 75
   LOCAL nMaxRows:= MaxRow()
   LOCAL nBtnRow := nMaxRows - 1
   LOCAL cLabel  := '(x)Harbour simulated GUI.'
   LOCAL aObjects:= WvtSetObjects( {} )
   LOCAL aObj    := {}
   LOCAL hPopup
   LOCAL oMenu
   LOCAL nConxn

   WVT_Core()
   WVT_Utils()

   SET DATE BRITISH

   SET( _SET_EVENTMASK, INKEY_ALL )

   Wvt_SetGui( .t. )
   WvtSetKeys( .t. )
   Popups( 1 )

   Wvt_SetFont( 'Courier New', 18, 0, 0 )

   Wvt_SetMouseMove( .t. )
   Wvt_ShowWindow( SW_RESTORE )

   hPopup  := Wvt_SetPopupMenu()
   oMenu   := CreateMainMenu()

   //  Force mouse pointer right below the xHarbour label
   //
   Wvt_SetMousePos( 2,40 )

   aAdd( aBlocks, {|| Wvt_SetIcon( 'vr_1.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'Vouch' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLabel( 1,40, cLabel,6,, rgb(255,255,255), rgb(198,198,198), 'Arial', 26, , , , , .t., .t. ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) } )
   aAdd( aBlocks, {|| Wvt_DrawImage( 8,62,12,69, IMAGE_VOUCH ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-2,0,maxrow()-2,maxcol(),WVT_LINE_HORZ,WVT_LINE_RECESSED,WVT_LINE_BOTTOM ) } )

   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1, 4,maxrow(), 4,WVT_LINE_VERT,WVT_LINE_RECESSED,WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,41,maxrow(),41,WVT_LINE_VERT,WVT_LINE_RECESSED,WVT_LINE_CENTER ) } )

   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   #define btnFDisp   WVT_BTN_FORMAT_FLAT
   #define btnFMOver  WVT_BTN_FORMAT_RAISED
   #define btnFBDown  WVT_BTN_FORMAT_RECESSED
   #define btnFBUp    WVT_BTN_FORMAT_FLAT

   WvtSetObjects( { OBJ_TYPE_BUTTON, 1,  nBtnRow, 6,nBtnRow+1, 9, ;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,IMAGE_VOUCH,btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,IMAGE_VOUCH,btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,IMAGE_VOUCH,btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,IMAGE_VOUCH,btnFBUp   )  ,;
              eval( SetKey( K_F2 ) ) } ;
                    } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 2,  nBtnRow,11,nBtnRow+1,14, ;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,IMAGE_BROWSE, btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,IMAGE_BROWSE, btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,IMAGE_BROWSE, btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,IMAGE_BROWSE, btnFBUp   )  ,;
              eval( SetKey( K_F5 ) ) } ;
                    } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 3,  nBtnRow,16,nBtnRow+1,19, ;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand',IMAGE_NOTES,btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand',IMAGE_NOTES,btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand',IMAGE_NOTES,btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand',IMAGE_NOTES,btnFBUp   )  ,;
              eval( SetKey( K_F3 ) ) } ;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 4,  nBtnRow,21,nBtnRow+1,24, ;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFDisp , rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFMOver, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFBDown, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFBUp  , rgb( 100,22,241 ), rgb( 0,100,0 ) )  ,;
              eval( SetKey( K_F4 ) ) } ;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 5,  nBtnRow,26,nBtnRow+1,29, ;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize',IMAGE_TOOLS, btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize',IMAGE_TOOLS, btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize',IMAGE_TOOLS, btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize',IMAGE_TOOLS, btnFBUp   )  ,;
              eval( SetKey( K_F6 ) ) },;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 6, nBtnRow,31,nBtnRow+1,34, ;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial',IMAGE_HELP, btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial',IMAGE_HELP, btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial',IMAGE_HELP, btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial',IMAGE_HELP, btnFBUp   )  ,;
              eval( SetKey( K_F7 ) ) },;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 7,  nBtnRow,36,nBtnRow+1,39, ;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines',IMAGE_VR, btnFDisp , rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines',IMAGE_VR, btnFMOver, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines',IMAGE_VR, btnFBDown, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines',IMAGE_VR, btnFBUp  , rgb( 100,22,241 ), rgb( 0,100,0 ) )  ,;
              eval( SetKey( K_F8 ) ) } ;
                   } )

   aAdd( aBlocks, {|| Wvt_Mouse( -1000001 ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   scr := SaveScreen( 0,0,maxrow(),maxcol() )
   clr := SetColor( 'N/W' )
   CLS
   SetColor( 'N/W,N/GR*,,,N/W*' )

   Wvt_SetMenu( oMenu:hMenu )

   SetKey( Wvt_SetMenuKeyEvent(), { || ActivateMenu( oMenu ) } )

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33 ) + '>'
   @ 16, 61      SAY '< Salary >'

   dDate := ctod( '04/01/04' )

   @  7, nColGet GET dDate WHEN  DispStatusMsg( 'Date must be Valid' );
                           VALID ClearStatusMsg()
   @ 10, nColGet GET cName WHEN  DispStatusMsg( 'Must be one of the list!' );
                           VALID ( VouChoice() < 7 .and. ClearStatusMsg() )
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE '@Z 9999999.99'

   READ

   //  Restore Environment
   //
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )
   WvtSetKeys( .f. )
   Wvt_SetPopupMenu( hPopup )

   RETURN

//-------------------------------------------------------------------//

Function HB_GTSYS()
   REQUEST HB_GT_WVG_DEFAULT
   Return nil

//------------------------------------------------------------------//

PROCEDURE WvtNextGets()

   LOCAL aLastPaint, clr
   LOCAL dDate      := ctod( '' )
   LOCAL cName      := Space( 35 )
   LOCAL cAdd1      := Space( 35 )
   LOCAL cAdd2      := Space( 35 )
   LOCAL cAdd3      := Space( 35 )
   LOCAL nSlry      := 0
   LOCAL aBlocks    := {}
   LOCAL nColGet    := 8
   LOCAL GetList    := {}
   LOCAL nTop       := 4
   LOCAL nLft       := 4
   LOCAL nBtm       := 20
   LOCAL nRgt       := 75
   LOCAL kf2        := SetKey( K_F2, {|| WvtGets() } )
   LOCAL kf3        := SetKey( K_F3, {|| WvtWindowExpand(  1 ) } )
   LOCAL kf4        := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
   LOCAL cLabel     := 'VOUCH, that GROWS with you'
   LOCAL aPalette   := Wvt_GetPalette()
   LOCAL aNewPalette:= aclone( aPalette )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL nRow       := Row()
   LOCAL nCol       := Col()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL wvtScr     := Wvt_SaveScreen( 0,0,maxrow(),maxcol() )

   // Change the values of pallatte arbitrarily though yu can fine tune
   // these values with realistic values.
   //
   aNewPalette[ 8 ] := aNewPalette[ 8 ] + 100000

   Wvt_SetPalette( aNewPalette )

   aAdd( aBlocks, {|| Wvt_SetTitle( 'Wvt Gets 2nd Window with Different Palette' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,0,maxrow()-1,maxcol() ) })
   aAdd( aBlocks, {|| Wvt_SetBrush( 0, rgb( 32,255,100 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawEllipse( 6,50,10,58 ) } )
   aAdd( aBlocks, {|| Wvt_SetBrush( 2, rgb( 255,255,100 ),1 ) } )
   aAdd( aBlocks, {|| Wvt_DrawRectangle( 11, 50, 13, 58 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroupRaised( 5, 6, 19, 72 ) } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   aAdd( aBlocks, {|| Wvt_DrawButton( 21, 6,22, 9,'New'   ,'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,11,22,14,'Browse','vouch1.bmp', 1, rgb( 255,255,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,16,22,19, ,'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,21,22,24,'Data',, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,26,22,29,'Flat',IMAGE_VR,2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,31,22,34,'Outline',IMAGE_VR,3 ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 22,36,22,41,'Data',, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   clr := SetColor( 'N/W,N/GR*,,,N/W*' )
   CLS

   @ MaxRow(), 0 SAY PadC( '(x)Harbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33) + '>'
   @ 16, 61      SAY '< Salary >'

   @  7, nColGet GET dDate
   @ 10, nColGet GET cName
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE '@Z 9999999.99'

   READ

   // Restore Environment
   //
   Wvt_SetPalette( aPalette )
   WvtSetObjects( aObjects )
   WvtSetBlocks( aLastPaint )
   SetColor( clr )

   RestScreen( 0, 0,maxrow(), maxcol(), scr )
   Wvt_RestScreen( wvtScr )
   SetPos( nRow, nCol )
   RETURN

//-------------------------------------------------------------------//
FUNCTION WvtSetKeys( lSet )

   if lSet
      keys_[ 2 ] := SetKey( K_F2, {|| WvtNextGets()         } )
      keys_[ 3 ] := SetKey( K_F3, {|| WvtWindowExpand( 1 )  } )
      keys_[ 4 ] := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
      keys_[ 5 ] := SetKey( K_F5, {|| WvtMyBrowse()         } )
      keys_[ 6 ] := SetKey( K_F6, {|| Wvt_Minimize()        } )
      keys_[ 7 ] := SetKey( K_F7, {|| WvtPartialScreen()    } )
      keys_[ 8 ] := SetKey( K_F8, {|| WvtLines()            } )
      keys_[ 9 ] := SetKey( K_F9, {|| Wvt_ChooseFont()      } )
      keys_[ 10] := SetKey( K_F10,{|| Wvt_ChooseColor()     } )
   else
      SetKey( K_F2,  keys_[ 2 ] )
      SetKey( K_F3,  keys_[ 3 ] )
      SetKey( K_F4,  keys_[ 4 ] )
      SetKey( K_F5,  keys_[ 5 ] )
      SetKey( K_F6,  keys_[ 6 ] )
      SetKey( K_F7,  keys_[ 7 ] )
      SetKey( K_F8,  keys_[ 8 ] )
      SetKey( K_F9,  keys_[ 9 ] )
      SetKey( K_F10, keys_[ 10] )
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
FUNCTION Wvt_SetFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Gained!', 'R/W' )

   DevPos( nRow, nCol )

   RETURN nil
//-------------------------------------------------------------------//
//      Wvt_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
FUNCTION Wvt_KillFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Lost...', 'B/W' )

   DevPos( nRow, nCol )

   RETURN nil
//-------------------------------------------------------------------//
//
//      Wvt_Mouse() must be present if you want to catch and fire
//      mouse call back outside of the inkey() loop.
//
//-------------------------------------------------------------------//
FUNCTION Wvt_Mouse( nKey, nRow, nCol )
   LOCAL i, nLen, aObjects := WvtSetObjects()
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

   RETURN nil
//-------------------------------------------------------------------//
//  WvtSetBlocks() is a get/set FUNCTION to be used by Wvt_Paint()
//-------------------------------------------------------------------//
FUNCTION WvtSetBlocks( a_ )

   LOCAL o
   STATIC s := {}

   o := aclone( s )

   IF a_ != nil
      s := aclone( a_ )
   ENDIF

   RETURN o
//-------------------------------------------------------------------//
//  WvtSetObjects() is a get/set FUNCTION to be used by Wvt_Mouse()
//-------------------------------------------------------------------//
FUNCTION WvtSetObjects( aObject )

   LOCAL oObjects
   STATIC aObjects := {}

   oObjects := aclone( aObjects )

   if aObject != nil
      if empty( aObject )
         aObjects := {}
      else
         if valtype( aObject[ 1 ] ) == 'A'
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

   Wvt_setFont( 'Courier New', sUnits )

   RETURN .t.
//-------------------------------------------------------------------//
STATIC FUNCTION rgb( r,g,b )

   RETURN ( r + ( g * 256 ) + ( b * 256 * 256 ) )
//-------------------------------------------------------------------//
FUNCTION VouChoice( aChoices )

   LOCAL scr, clr, nChoice

   DEFAULT aChoices TO { 'One','Two','Three','Four','Five','Six','Seven' }

   scr := SaveScreen( 7,48,13,55 )
   clr := SetColor( 'N/W*,GR+/B*,,,GR+/B' )

   nChoice := aChoice( 7, 48, 13, 55, aChoices )

   setColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice
//-------------------------------------------------------------------//
FUNCTION WvtMyBrowse()

   LOCAL nKey, bBlock, oBrowse , aLastPaint, i
   LOCAL lEnd    := .f.
   LOCAL aBlocks := {}
   LOCAL info_   := {}
   LOCAL nTop    :=  3
   LOCAL nLeft   :=  3
   LOCAL nBottom := maxrow() - 2
   LOCAL nRight  := maxcol() - 3
   LOCAL nCursor := setCursor( 0 )
   LOCAL nRow    := row()
   LOCAL nCol    := col()
   LOCAL cColor  := SetColor( 'N/W*,N/GR*,,,N/W*' )
   LOCAL cScr    := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL aObjects:= WvtSetObjects( {} )
   LOCAL hPopup  := Wvt_SetPopupMenu()
   LOCAL stru_:={}, cDbfFile, cSqlFile, cFileIndex, cFileDbf, cRDD, nIndex

   STATIC nStyle := 0

   cRDD       := 'DBFCDX'
   cFileDbf   := 'TEST.DBF'
   cFileIndex := 'TEST.Z01'

   USE ( cFileDbf ) NEW SHARED VIA ( cRDD )
   if NetErr()
      return nil
   endif
   if fLock()
      INDEX ON Test->FIRST TAG '001' TO ( cFileIndex )
      INDEX ON Test->LAST  TAG '002' TO ( cFileIndex )
      INDEX ON Test->CITY  TAG '003' TO ( cFileIndex )
      dbUnlock()
   endif
   SET INDEX TO
   SET INDEX TO ( cFileIndex )
   SET ORDER TO 1
   DbGoTo( 50 )

   info_:= DbStruct()

   Popups( 2 )

   //oBrowse := TBrowseNew( nTop + 3, nLeft + 2, nBottom - 1, nRight - 2 )
   oBrowse := TBrowseWVG():New( nTop + 3, nLeft + 2, nBottom - 1, nRight - 2 )

   oBrowse:ColSep        := '  '
   oBrowse:HeadSep       := '__'
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i,1 ], bBlock ) )
   next

   oBrowse:configure()

   if nStyle > 5
      nStyle := 0
   endif

   Wvt_SetPen( nStyle, 0, rgb( 210,1210,210 ) )

   nStyle++

   aAdd( aBlocks, {|| Wvt_SetIcon( 'DIA_EXCL.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'WVT Gui TBrowse()' ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( nTop+3, nLeft+2, nBottom-1, nRight-2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridHorz( oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridVert( oBrowse:nTop, oBrowse:nBottom, oBrowse:aColumnsSep, len( oBrowse:aColumnsSep ) ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   DispBox( 0, 0, maxrow(), maxcol(), '         ', 'N/W' )
   DispOutAt( nTop + 1, nleft, padc( CurDrive()+':\'+CurDir()+'\'+'Test.dbf', nRight - nLeft + 1 ), 'W+/W' )

   While !lEnd
      oBrowse:ForceStable()

      nKey := InKey( 0 )

      BrwHandleKey( oBrowse, nKey, @lEnd )

      if nKey == K_F2
         nIndex := IndexOrd()
         nIndex++
         if nIndex > 3
            nIndex := 1
         endif
         Set Order To ( nIndex )
         oBrowse:RefreshAll()
      endif
   end

   Wvt_SetPen( 0 )
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )

   DevPos( nRow, nCol )
   SetColor( cColor )
   SetCursor( nCursor )

   DBCloseArea()
   RestScreen( 0, 0, maxrow(), maxcol(), cScr )
   Wvt_setPopupMenu( hPopup )

   RETURN nil
//-------------------------------------------------------------------//
STATIC FUNCTION DbSkipBlock( n, oTbr )

   LOCAL nSkipped := 0

   if n == 0
      DBSkip( 0 )

   elseif n > 0
      do while nSkipped != n .and. TBNext( oTbr )
         nSkipped++
      enddo
   else
      do while nSkipped != n .and. TBPrev( oTbr )
         nSkipped--
      enddo
   endif

   RETURN  nSkipped
//-------------------------------------------------------------------//
STATIC FUNCTION TBNext( oTbr )

   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   if Eof()
      lMoved := .F.
   else
      DBSkip( 1 )
      if Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      endif
   endif

   RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION TBPrev( oTbr )
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

   RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION VouBlockField( i )

   RETURN  {|| fieldget( i ) }
//-------------------------------------------------------------------//
STATIC FUNCTION BrwHandleKey( oBrowse, nKey, lEnd )
   LOCAL lRet := .t.

   do case
   case nKey == K_ESC
      lEnd := .t.

   case nKey == K_ENTER
      lEnd := .t.

   case nKey == K_DOWN
      oBrowse:Down()

   case nKey == K_UP
      oBrowse:Up()

   case nKey == K_LEFT
      oBrowse:Left()

   case nKey == K_RIGHT
      oBrowse:Right()

   case nKey == K_PGDN
      oBrowse:pageDown()

   case nKey == K_PGUP
      oBrowse:pageUp()

   case nKey == K_CTRL_PGUP
      oBrowse:goTop()

   case nKey == K_CTRL_PGDN
      oBrowse:goBottom()

   case nKey == K_HOME
      oBrowse:home()

   case nKey == K_END
      oBrowse:end()

   case nKey == K_CTRL_LEFT
      oBrowse:panLeft()

   case nKey == K_CTRL_RIGHT
      oBrowse:panRight()

   case nKey == K_CTRL_HOME
      oBrowse:panHome()

   case nKey == K_CTRL_END
      oBrowse:panEnd()

   case nKey == K_MWBACKWARD
      oBrowse:down()

   case nKey == K_MWFORWARD
      oBrowse:up()

   otherwise
      lRet := .f.

   endcase

   RETURN lRet

//-------------------------------------------------------------------//

STATIC FUNCTION BrwOnEvent( oWvtBrw, cPaintID, oBrowse, nKey )
   LOCAL lRet := .t., lRefAll := .f.

   do case
   case nKey == K_DOWN
      oBrowse:Down()

   case nKey == K_UP
      oBrowse:Up()

   case nKey == K_LEFT
      oBrowse:Left()

   case nKey == K_RIGHT
      oBrowse:Right()

   case nKey == K_PGDN
      oBrowse:pageDown()
      lRefAll := .t.

   case nKey == K_PGUP
      oBrowse:pageUp()
      lRefAll := .t.

   case nKey == K_CTRL_PGUP
      oBrowse:goTop()
      lRefAll := .t.

   case nKey == K_CTRL_PGDN
      oBrowse:goBottom()
      lRefAll := .t.

   case nKey == K_HOME
      oBrowse:home()

   case nKey == K_END
      oBrowse:end()

   case nKey == K_CTRL_LEFT
      oBrowse:panLeft()

   case nKey == K_CTRL_RIGHT
      oBrowse:panRight()

   case nKey == K_CTRL_HOME
      oBrowse:panHome()

   case nKey == K_CTRL_END
      oBrowse:panEnd()

   case nKey == K_MWBACKWARD
      oBrowse:down()

   case nKey == K_MWFORWARD
      oBrowse:up()

   case nKey == K_SBTHUMBTRACKVERT
      OrdKeyGoTo( oWvtBrw:oVBar:GetPos() )
      lRefAll := .t.

   case nKey == K_SBTHUMBTRACKHORZ
      oBrowse:ColPos := oWvtBrw:oHBar:GetPos()

   case nKey == K_SBLINEUP
      oBrowse:up()

   case nKey == K_SBLINEDOWN
      oBrowse:down()

   case nKey == K_SBPAGEUP
     oBrowse:PageUp()

   case nKey == K_SBPAGEDOWN
      oBrowse:PageDown()

   case nKey == K_SBLINELEFT
      oBrowse:Left()

   case nKey == K_SBLINERIGHT
      oBrowse:Right()

   case nKey == K_SBPAGELEFT
      oBrowse:Left()

   case nKey == K_SBPAGERIGHT
      oBrowse:right()

   otherwise
      lRet := .f.

   endcase

   if lRet
      if lRefAll
         oBrowse:RefreshAll()
      endif
      oBrowse:ForceStable()

      oWvtBrw:oVBar:SetPos( OrdKeyCount(),OrdKeyNo() )
      oWvtBrw:oHBar:SetPos( oBrowse:ColCount, oBrowse:ColPos )
   endif

   RETURN lRet

//-------------------------------------------------------------------//

STATIC FUNCTION CfgMyBrowse( aFields, cUseAlias, aTLBR, cDesc, oParent, cColorSpec, nID )
   LOCAL info_, oWvtBrw, oBrowse, i, bBlock
   LOCAL aPopup := {}

   aadd( aPopup, { 'Down'     , {|| oBrowse:Down()    , oBrowse:ForceStable() } } )
   aadd( aPopup, { 'Up'       , {|| oBrowse:Up()      , oBrowse:ForceStable() } } )
   aadd( aPopup, { 'Page Down', {|| oBrowse:PageDown(), oBrowse:ForceStable() } } )
   aadd( aPopup, { 'Page Up'  , {|| oBrowse:PageUp()  , oBrowse:ForceStable() } } )
   aadd( aPopup, { 'Top'      , {|| oBrowse:GoTop()   , oBrowse:ForceStable() } } )
   aadd( aPopup, { 'Bottom'   , {|| oBrowse:GoBottom(), oBrowse:ForceStable() } } )

   Select( cUseAlias )
   info_:= DbStruct()

   //oBrowse := TBrowseNew( aTLBR[ 1 ], aTLBR[ 2 ], aTLBR[ 3 ], aTLBR[ 4 ] )
   oBrowse := TBrowseWVG():New( aTLBR[ 1 ], aTLBR[ 2 ], aTLBR[ 3 ], aTLBR[ 4 ] )

   oBrowse:ColSep        := '  '
   oBrowse:HeadSep       := '__'
   oBrowse:ColorSpec     := cColorSpec
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( aFields )
      bBlock := VouBlockField( aFields[ i ] )
      oBrowse:AddColumn( TBColumnNew( info_[ aFields[ i ],1 ], bBlock ) )
   next

   oBrowse:configure()

   oWvtBrw := WvtBrowse():New( oParent,nID )

   oWvtBrw:nTop         := aTLBR[ 1 ]
   oWvtBrw:nLeft        := aTLBR[ 2 ]
   oWvtBrw:nBottom      := aTLBR[ 3 ]
   oWvtBrw:nRight       := aTLBR[ 4 ]
   oWvtBrw:cAlias       := cUseAlias
   oWvtBrw:oBrw         := oBrowse
   oWvtBrw:cDesc        := cDesc
   oWvtBrw:nPointer     := WVT_IDC_HAND
   oWvtBrw:cColorHilite := 'W+/B*'
   oWvtBrw:Tooltip      := cDesc
   oWvtBrw:aPopup       := aPopup

   oWvtBrw:bHandleEvent := {|oWvtBrw,cPaintID,oBrowse,nKey| BrwOnEvent( oWvtBrw,cPaintID,oBrowse,nKey ) }

   RETURN oWvtBrw

//-------------------------------------------------------------------//

FUNCTION WvtPartialScreen()
   LOCAL scr        := SaveScreen( 7,20,15,60 )
   LOCAL wvtScr     := Wvt_SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL wvtScr1
   LOCAL aLastPaint
   LOCAL hPopup     := Wvt_SetPopupMenu()

   aLastPaint := WvtSetBlocks( {} )

   DispBox( 7, 20, 15, 60, '         ', 'W/GR*' )
   @ 10,25 SAY 'Wvt_SaveScreen()' COLOR 'N/GR*'
   @ 11,25 SAY 'Wvt_RestScreen()' COLOR 'N/GR*'
   @ 13,25 SAY 'Press Esc '       COLOR 'N/GR*'
   Wvt_DrawBoxRecessed( 8,22,14,58 )

   wvtScr1 := Wvt_SaveScreen( 7,20,15,60 )

   do while inkey( 0 ) != K_ESC
   enddo

   DispBox( 7, 20, 15, 60, '         ', 'W/B*' )
   @ 10,25 SAY 'Wvt_SaveScreen()' COLOR 'N/B*'
   @ 11,25 SAY 'Wvt_RestScreen()' COLOR 'N/B*'
   @ 13,25 SAY 'Press Esc '       COLOR 'N/B*'
   Wvt_DrawBoxRecessed( 8,22,14,58 )

   do while inkey( 0 ) != K_ESC
   enddo

   Wvt_RestScreen( 7,20,15,60, wvtScr1 )

   do while inkey( 0 ) != K_ESC
   enddo

   RestScreen( 7,20,15,60,scr )
   Wvt_RestScreen( 0, 0, MaxRow(), MaxCol(), wvtScr )
   WvtSetBlocks( aLastPaint )
   Wvt_SetPopupMenu( hPopup )

   RETURN NIL

//-------------------------------------------------------------------//

function WvtLines()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL clr        := SetColor( 'N/W' )
   LOCAL nRows      := maxrow()
   LOCAL nCols      := maxcol()
   LOCAL aLastPaint := WvtSetBlocks( {} )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL hPopup     := Wvt_SetPopupMenu()
   LOCAL aBlocks    := {}

   CLS

   aAdd( aBlocks, {|| Wvt_DrawLine( 0, 0, 0, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 1, 0, 1, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_TOP ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 2, 0, 2, nCols, WVT_LINE_HORZ, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_SOLID, 4, Rgb( 255,255,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 3, 0, 3, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER, WVT_LINE_DASH , 0, Rgb( 255,0,0 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 4, 0, 4, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_BOTTOM ) } )

   @ 0, 1 SAY 'Center Raised'
   @ 1,11 say 'Top Recessed'
   @ 2,21 say 'Center Plain White 3 Pixels'
   @ 3,31 say 'Center Raised Dotted'
   @ 4,41 SAY 'Bottom Recessed'
   @ 5, 1 SAY 'Bottom Checked'

   @ nRows, 0 Say PadC( 'Press ESC to Quit', nCols+1 ) COLOR 'GR+/W'

   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 5,nRows-2, 5, WVT_LINE_VERT, WVT_LINE_RAISED  , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 6,nRows-2, 6, WVT_LINE_VERT, WVT_LINE_RECESSED, WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 7,nRows-2, 7, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_LEFT   ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 8,nRows-2, 8, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 9,nRows-2, 9, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_RIGHT  ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,10,nRows-2,10, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DOT,     0, RGB( 0,0,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,11,nRows-2,11, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASH,    0, RGB( 255,0,0 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,12,nRows-2,12, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASHDOT, 0, RGB( 0,255,0 ) ) } )

   WvtSetBlocks( aBlocks )

   @ 12,5 Say 'A'
   @ 13,6 Say 'B'
   @ 14,7 Say 'C'
   @ 15,8 Say 'D'
   @ 16,9 Say 'E'

   do while ( inkey(0) != 27 )
   enddo

   //  Restore Environments
   //
   SetColor( clr )

   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )
   Wvt_SetPopupMenu( hPopup )

   RestScreen( 0,0,maxrow(),maxcol(), scr )

   RETURN nil

//-------------------------------------------------------------------//

FUNCTION DispStatusMsg( cMsg )

   Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, rgb(198,198,198), 'Arial', 18, , 900 )

   RETURN .t.

//-------------------------------------------------------------------//

FUNCTION ClearStatusMsg()
   LOCAL nRow := Row()
   LOCAL nCol := Col()

   DispOutAt( MaxRow(), 42, space( 37 ), 'W/W' )

   SetPos( nRow, nCol )

   RETURN .t.

//-------------------------------------------------------------------//

FUNCTION Popups( nID, lDestroy )
   LOCAL hPop, hPop1
   LOCAL nPrompt := MF_ENABLED+MF_STRING

   static hPop_:= { , , , , , , , , }

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
         Wvt_AppendMenu( hPop, nPrompt, K_F2, 'Second Get Screen' )
         Wvt_AppendMenu( hPop, nPrompt, K_F3, 'Expand Window'     )
         Wvt_AppendMenu( hPop, nPrompt, K_F4, 'Shrink Window'     )
         Wvt_AppendMenu( hPop, nPrompt, K_F5, 'Browse'            )
         Wvt_AppendMenu( hPop, nPrompt, K_F6, 'Minimize'          )
         Wvt_AppendMenu( hPop, nPrompt, K_F7, 'Partial Screen'    )
         Wvt_AppendMenu( hPop, nPrompt, K_F8, 'Lines'             )
         Wvt_AppendMenu( hPop, nPrompt, K_F9, 'Choose Font'       )
         Wvt_AppendMenu( hPop, nPrompt, K_F10,'Choose Color'      )

         Wvt_AppendMenu( hPop, MF_SEPARATOR )

         Wvt_AppendMenu( hPop, nPrompt, K_F5, 'Browse'  )

      endif

   case nID == 2   //  Browser

      if hPop == nil
         hPop := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop, nPrompt, K_DOWN     , 'Down'      )
         Wvt_AppendMenu( hPop, nPrompt, K_UP       , 'Up'        )
         Wvt_AppendMenu( hPop, nPrompt, K_PGDN     , 'Page Down' )
         Wvt_AppendMenu( hPop, nPrompt, K_PGUP     , 'Page Up'   )
         Wvt_AppendMenu( hPop, nPrompt, K_CTRL_PGUP, 'Top'       )
         Wvt_AppendMenu( hPop, nPrompt, K_CTRL_PGDN, 'Bottom'    )

         Wvt_AppendMenu( hPop, MF_SEPARATOR )

         hPop1 := Wvt_CreatePopupMenu()
         Wvt_AppendMenu( hPop1, nPrompt, K_RIGHT   , 'Right'     )
         Wvt_AppendMenu( hPop1, nPrompt, K_LEFT    , 'Left'      )
         Wvt_AppendMenu( hPop1, nPrompt, K_END     , 'End'       )
         Wvt_AppendMenu( hPop1, nPrompt, K_HOME    , 'Home'      )

         Wvt_AppendMenu( hPop, MF_ENABLED+MF_POPUP, hPop1, 'Column Movement' )

      endif

   endcase

   hPop_[ nID ] := hPop

   RETURN Wvt_SetPopupMenu( hPop_[ nID ] )

//-------------------------------------------------------------------//

FUNCTION WvtPictures( nSlot,cFilePic )

   if nSlot != nil .and. nSlot <= 20 .and. file( cFilePic )
      if pic_[ nSlot ] != cFilePic
         if Wvt_LoadPicture( cFilePic, nSlot )
            pic_[ nSlot ] := cFilePic
         endif
      endif
   endif

   RETURN nil

//-------------------------------------------------------------------//

FUNCTION WvtExePicture( nTop, nLeft, nBottom, nRight, nSlot, aOffset )

   if pic_[ nSlot ] != nil
      Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aOffSet )
   endif

   RETURN nil

//-------------------------------------------------------------------//

FUNCTION CreateMainMenu()
   LOCAL oMenu
   LOCAL g_oMenuBar := wvtMenu():new():create()

   oMenu := WvtMenu():new():create()
   oMenu:Caption:= "Wvt*Classes"
   oMenu:AddItem( "Dialog One", {|| MyDialogOne() } )
   oMenu:AddItem( "Dialog Two", {|| MyDialogTwo() } )
   oMenu:AddItem( "-" )
   oMenu:AddItem( "Exit"      , {|| __keyboard( K_ESC ) } )
   g_oMenuBar:addItem( "",oMenu )

   oMenu := wvtMenu():new():create()
   oMenu:Caption := "Traditional"
   oMenu:AddItem( "Next Gets"     , {|| WvtNextGets()      } )
   oMenu:AddItem( "Browser"       , {|| WvtMyBrowse()      } )
   oMenu:AddItem( "Partial Screen", {|| WvtPartialScreen() } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Wvt Lines"     , {|| WvtLines()         } )
   g_oMenuBar:addItem( "",oMenu )

   oMenu := wvtMenu():new():create()
   oMenu:Caption:= "Common Dialogs"
   oMenu:AddItem( "Fonts" ,{|| Wvt_ChooseFont() } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Colors",{|| Wvt_ChooseColor() } )
   g_oMenuBar:addItem( "",oMenu)

   oMenu := wvtMenu():new():create()
   oMenu:Caption:= "Functionality"
   oMenu:AddItem( "Expand" ,{|| WvtWindowExpand( 1 ) } )
   oMenu:AddItem( "Shrink" ,{|| WvtWindowExpand( -1 ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Minimize",{|| Wvt_Minimize() } )
   g_oMenuBar:addItem( "",oMenu)

   oMenu := wvtMenu():new():create()
   oMenu:Caption:= "Modeless Dialogs"
   oMenu:AddItem( "Dialog First" ,{|| DynDialog_2() } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Slide Show"   ,{|| DlgSlideShow() } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Dialog Scond" ,{|| DynDialog_1() } )

   g_oMenuBar:addItem( "",oMenu)

   RETURN g_oMenuBar

//-------------------------------------------------------------------//

STATIC FUNCTION ActivateMenu( oMenu )
   LOCAL nMenu := Wvt_GetLastMenuEvent()
   LOCAL aMenuItem

   IF !EMPTY( nMenu )

     IF HB_ISOBJECT( oMenu )
       IF !EMPTY( aMenuItem := oMenu:FindMenuItemById( nMenu ) )
         IF HB_ISBLOCK( aMenuItem[ WVT_MENU_ACTION ] )
           EVAL( aMenuItem[ WVT_MENU_ACTION ] )
         ENDIF
       ENDIF
      ENDIF
   ENDIF

   RETURN ( NIL )

//-------------------------------------------------------------------//

STATIC FUNCTION MyDialogOne()
   LOCAL aObjects:= WvtSetBlocks( {} )
   Local nWinRows, nWinCols, cWinTitle, cFont, nHeight, nWidth
   Local oDlg, obj_, oBar, d_, nN, cUseAlias
   Local oText, oTBar, aImg_, oImg, oLine, oBox, oBtn, oBtn2
   Local oBBox, oCon, oGet, oGet2, nRowG, oBBox2, oBnr, oTBx
   Local oBRsd, cTxt, oRct, nGetCol, nSayCol, bBlock, bBlock1
   Local oMnu, oWvtBrw, oWvtBrw1, lOpen, lOpen1, cUseAlias1, oGetArea, oGet1
   LOCAL hPopup, nGetRow, aGets_, lChkMouse
   LOCAL g_oMenuBar, oMenu, oPBar2,oPBar3

   WvtSetKeys( .f. )
   lChkMouse := SetMouseCheck( .f. )

   hPopup := Wvt_SetPopupMenu()
   Popups()

   cTxt := 'GtWvt is capable of designing virtually any preceivable control '
   cTxt := cTxt + 'Windows offers.'
   cTxt := cTxt + CRLF + CRLF
   cTxt := cTxt + 'This text is placed in a WvtTextBox() control with '
   cTxt := cTxt + 'font and alignment adjustments!'
   cTxt := cTxt + CRLF + CRLF
   cTxt := cTxt + 'Enjoy - Pritpal Bedi, INDIA'

   aImg_:={}
   aadd( aImg_, 'v_lock.bmp'   )
   aadd( aImg_, 'v_new.bmp'    )
   aadd( aImg_, 'v_clclt.bmp'  )
   aadd( aImg_, 'v_calend.bmp' )
   aadd( aImg_, 'v_index.bmp'  )
   aadd( aImg_, 'v_notes1.bmp' )
   aadd( aImg_, 'v_selct1.bmp' )

   nWinRows  := 55
   nWinCols  := 185
   cWinTitle := 'WvtGui Dialog One'
   cFont     := 'Courier New'
   nHeight   := 13

   oDlg := WvtDialog():New( nWinRows, nWinCols, cWinTitle, cFont, nHeight )
   oDlg:nTooltipWidth     := 300
   oDlg:nTooltipTextColor := RGB( 255,0,0 )

   oBar := WvtStatusBar():New( oDlg,201 )
   oBar:SetPanels( { 50,100 } )
   oBar:SetText( 1, 'Tab.SH_Tab.Left_Click - Select a Browse' )
   oBar:SetText( 2, 'GtWvt is Fantastic', 'w+/W' )
   oBar:SetText( 3, 'WOW' )
   oBar:nPointer := WVT_IDC_HAND
   oBar:Tooltip  := 'GtWvt Statusbar with 3 panels'
   oDlg:AddObject( oBar )

   oBox := WvtStatic():New( oDlg,110,4,oDlg:MaxCol()-40,7,oDlg:MaxCol()-2 )
   oBox:nStatic := WVT_STATIC_BOXRECESSED
   oDlg:AddObject( oBox )

   oText := WvtLabel():New( oDlg, 101, 4, oDlg:MaxCol()-40, 7,oDlg:MaxCol()-2 )
   oText:Text              := '(x)Harbour'
   oText:nFontHeight       := 36
   oText:nAlignHorz        := 2
   oText:nAlignVert        := 2
   oText:nFontWeight       := 700
   oText:nTextColor        := RGB( 100, 255,  12 )
   oText:nBackColor        := RGB(   0,   0, 255 )
   oText:nTextColorHoverOn := RGB( 255, 255,   0 )
   oText:nBackColorHoverOn := RGB( 255, 100,  12 )
   oText:lItalic           := .t.
   oText:ToolTip           := 'Software that GROWS with you'
   oText:bOnSelect         := {|o,v| .t. }
   oDlg:AddObject( oText )

   oImg := WvtImage():New( oDlg,102,20,oDlg:MaxCol()-40,37,oDlg:MaxCol()-2 )
   oImg:cImage  := aImg_[ 5 ]
   oImg:Tooltip := 'WvtImage():New()'
   oDlg:AddObject( oImg )

   oTBar := WvtToolbar():New( oDlg,103, 0,0,2 )
   oTBar:lFloating := .f.
   oTBar:Tooltip   := 'Toolbar'
   oTBar:AddButton( aImg_[ 1 ], {|| oImg:SetImage( aImg_[ 1 ] ) } , 'Lock' )
   oTBar:AddButton( aImg_[ 2 ], {|| oImg:SetImage( aImg_[ 2 ] ), oText:SetText( '(x)Harbour' ) } , 'New' )
   oTBar:AddButton( aImg_[ 3 ], {|| oImg:SetImage( aImg_[ 3 ] ) } , 'Calculator' )
   oTBar:AddButton()
   oTBar:AddButton( aImg_[ 5 ], {|| oImg:SetImage( aImg_[ 5 ] ) } , 'Restore' )
   oTBar:AddButton( aImg_[ 4 ], {|| oImg:SetImage( aImg_[ 4 ] ), oText:SetText( 'Vouch' )    } , 'Calendar' )
   oTBar:AddButton( aImg_[ 6 ], {|| oImg:SetImage( aImg_[ 6 ] ) } , 'Notes' )
   oTBar:AddButton( aImg_[ 7 ], {|| oImg:SetImage( aImg_[ 7 ] ) } , 'Press to Send Browse on Top' )
   oTBar:AddButton()
   oDlg:AddObject( oTBar )

   oLine := WvtStatic():New( oDlg, 105, 39, 0, 39, oDlg:MaxCol() )
   oLine:nStatic := WVT_STATIC_LINE
   oDlg:AddObject( oLine )

   oBBox := WvtStatic():New( oDlg,125,4,127,37,139 )
   oBBox:nStatic := WVT_STATIC_BOXGROUP
   oDlg:AddObject( oBBox )

   oBtn := WvtPushButton():New(oDlg, 124, 6, 129, 7, 137 )
   oBtn:cCaption  := 'Print'
   oBtn:bOnLeftUp := {|| Wvt_Keyboard( 379 ) }
   oBtn:Tooltip   := 'Open Printing Dialog for the Browser in Focus'
   oDlg:AddObject( oBtn )

   oBtn2 := WvtPushButton():New( oDlg, 124, 9, 129, 12, 137 )
   oBtn2:cFileImage := aImg_[ 3 ]
   oBtn2:block      := {|| ExeProgressBar( oPBar2, oPBar3 ) }
   oBtn2:Tooltip    := 'Execute Progress Bar'
   oDlg:AddObject( oBtn2 )

   oPBar2 := WvtProgressBar():New( oDlg, , 14, 129, 25, 137 )
   oPBar2:nBarColor  := RGB( 240,240,0 )
   oPBar2:cBackColor := 'W/N*'
   oPBar2:lVertical  := .t.
   oPBar2:nDirection := 0
   oPBar2:cImage     := 'vouch1.bmp'
   oDlg:AddObject( oPBar2 )

   oPBar3 := WvtProgressBar():New( oDlg, , 26, 129, 36, 137 )
   oPBar3:nBarColor  := RGB( 240,240,0 )
   oPBar3:cBackColor := 'W/N*'
   oPBar3:lVertical  := .t.
   oPBar3:nDirection := 1
   oPBar3:cImage     := 'vouch1.bmp'
   oDlg:AddObject( oPBar3 )

   oBBox2 := WvtStatic():New( oDlg, , 9, oDlg:MaxCol()-40, 18, oDlg:Maxcol()-2 )
   oBBox2:nStatic := WVT_STATIC_BOXGROUP
   oDlg:AddObject( oBBox2 )

   oCon := WvtConsole():New( oDlg )
   oDlg:AddObject( oCon )

   nGetCol := 158
   bBlock  := {|| oCon:Say( 12, 148, 'Name'  ,'N/W' ),;
                  oCon:Say( 14, 148, 'Date'  ,'N/W' ),;
                  oCon:Say( 16, 148, 'Amount','N/W' ) }

   oGet := WvtGets():New( oDlg, 210, 9, oDlg:Maxcol()-40, 18, oDlg:Maxcol()-2 )
   oGet:AddGets( 12, nGetCol, 'GTWvt               ', '@! ','W+/B*,N/W*' )
   oGet:AddGets( 14, nGetCol, date() )
   oGet:AddGets( 16, nGetCol, 2122.57, '@Z 99999999.99','w+/R,GR+/B' )
   oGet:Tooltip   := 'WvtGets():New() - ReadModal() like Clipper'
   oGet:cDesc     := 'Normal Get Box'
   oGet:bOnCreate := bBlock
   oDlg:AddObject( oGet )

   oBnr := WvtBanner():New( oDlg, 101, 0, 127, 1, oDlg:MaxCol()-2 )
   oBnr:nTimeDelay        := 0.25
   oBnr:cText             := 'the compiler that EXTENDS with you'
   oBnr:nFontHeight       := 24
   oBnr:nFontWeight       := 0
   oBnr:nDirection        := 0
   oBnr:nAlignVert        := 2
   oBnr:nTextColor        := RGB( 253,251,170 )
   oBnr:nBackColor        := RGB( 128,227,142 )
   oBnr:nTextColorHoverOn := RGB( 255,255,  0 )
   oBnr:nBackColorHoverOn := RGB( 255,100, 12 )
   oBnr:Tooltip           := 'WvtBanner():New()'
   oDlg:AddObject( oBnr )

   oBRsd := WvtStatic():New( oDlg, , 41,127,52,oDlg:MaxCol()-2 )
   oBRsd:nStatic := WVT_STATIC_BOXGROUPRAISED
   oDlg:AddObject( oBRsd )

   oRct := WvtStatic():New( oDlg, , 41,127,52,oDlg:MaxCol()-2 )
   oRct:nStatic := WVT_STATIC_SHADEDRECT
   oRct:aRGBb   := { 0xffff, 0x0000, 0x0000, 0x0000 }
   oRct:aRGBe   := { 0x0000, 0xffff, 0xffff, 0x0000 }
   oDlg:AddObject( oRct )

   oTBx := WvtTextBox():New( oDlg, , 42,129,51,oDlg:MaxCol()-4 )
   oTBx:cText       := cTxt
   oTBx:Tooltip     := 'WvtTextBox():New()'
   oTBx:nFontHeight := 16
   oTBx:lItalic     := .t.
   oTBx:lUnderline  := .t.
   oTBx:nAlignHorz  := 2
   oTBx:nTextColor  := RGB( 255,255,255 )
   oTBx:nTextColorHoverOn := RGB( 0,0,255 )
   oTBx:aPopup      := {}
   aadd( oTBx:aPopup, { 'Getsome' , {|| .t. } } )
   aadd( oTBx:aPopup, { 'Getsome2', {|| .t. } } )
   oDlg:AddObject( oTBx )

   oGetArea := WvtStatic():New( oDlg, , 4, 2, 37, 62 )
   oGetArea:nStatic := WVT_STATIC_BOXRAISED
   oDlg:AddObject( oGetArea )

   nGetCol := 20
   nSayCol := 7
   nGetRow := 7
   bBlock1 := {|| oCon:Say( nGetRow+00, nSayCol, 'First Name'  ,'N/W' ),;
                  oCon:Say( nGetRow+02, nSayCol, 'Last Name '  ,'N/W' ),;
                  oCon:Say( nGetRow+04, nSayCol, 'Street'      ,'N/W' ),;
                  oCon:Say( nGetRow+06, nSayCol, 'City'        ,'W+/W'),;
                  oCon:Say( nGetRow+08, nSayCol, 'State'       ,'N/W' ),;
                  oCon:Say( nGetRow+10, nSayCol, 'Zip'         ,'B+/W'),;
                  oCon:Say( nGetRow+12, nSayCol, 'Date Hired'  ,'B+/W'),;
                  oCon:Say( nGetRow+14, nSayCol, 'Married'     ,'B+/W'),;
                  oCon:Say( nGetRow+16, nSayCol, 'Age'         ,'B+/W'),;
                  oCon:Say( nGetRow+18, nSayCol, 'Salary'      ,'B+/W'),;
                  oCon:Say( nGetRow+20, nSayCol, 'Notes',      ,'B+/W') ;
                  }

   aGets_:= { pad('Pritpal',20 ), pad( 'Bedi',20 ), pad( '60, New Professor Colony',30 ), ;
              pad( 'Ludhiana, INDIA',30 ),;
              'PB', pad( '141004',10 ), ctod( '22/06/04' ), .t., 48, 17000, ;
              pad( 'Wvtgui is a classical example of (x)Harbour capabilities...',65 ) }

   oGet1 := WvtGets():New( oDlg, , 4, 2, 37, 62 )
   oGet1:AddGets( nGetRow+00, nGetCol, aGets_[ 1 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+02, nGetCol, aGets_[ 2 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+04, nGetCol, aGets_[ 3 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+06, nGetCol, aGets_[ 4 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+08, nGetCol, aGets_[ 5 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+10, nGetCol, aGets_[ 6 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+12, nGetCol, aGets_[ 7 ], '@ '       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+14, nGetCol, aGets_[ 8 ], '@Y'       , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+16, nGetCol, aGets_[ 9 ], '@Z 99'    , 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+18, nGetCol, aGets_[ 10], '@Z 999999', 'N/W*,N/GR*' )
   oGet1:AddGets( nGetRow+20, nGetCol, aGets_[ 11], '@S35'     , 'N/W*,N/GR*' )
   oGet1:cDesc     := 'Test.dbf Fields'
   oGet1:Tooltip   := 'Double Click to Activate ReadModal()'
   oGet1:bOnCreate := bBlock1
   oDlg:AddObject( oGet1 )

   g_oMenuBar := WvtMenu():new():create()
   oMenu      := WvtMenu():new():create()
   oMenu:Caption := 'Other Dialogs'
   oMenu:AddItem( 'Dialog Two', {|| MyDialogTwo() } )
   oMenu:AddItem( '-' )
   oMenu:AddItem( 'Exit',       {|| Wvt_Keyboard( K_ESC ) } )
   g_oMenuBar:addItem( "",oMenu )

   oDlg:oMenu := g_oMenuBar

   lOpen := .f.
   cUseAlias := 'TEST'
   USE 'TEST' NEW ALIAS ( cUseAlias ) SHARED
   if !NetErr()
      lOpen := .t.
      oWvtBrw := CfgMyBrowse( { 1,7,9,10,8 }, cUseAlias, { 6,67,36,120 }, 'Test.dbf - 1,7,9,10,8', oDlg, 'N/W*,N/GR*',1001 )
      oDlg:AddObject( oWvtBrw )
   endif

   lOpen1 := .f.
   cUseAlias1 := 'TEST1'
   USE 'TEST' NEW ALIAS ( cUseAlias1 ) SHARED
   if !NetErr()
      lOpen1 := .t.
      oWvtBrw1 := CfgMyBrowse( { 1,2,3,4,5,6 }, cUseAlias1, { 43,4,51,120 }, 'Test.dbf - 1,2,3,4,5,6',oDlg, 'N/BG*,N/W*',1002 )
      oDlg:AddObject( oWvtBrw1 )
   endif

   oDlg:Create()
   oDlg:Execute()
   oDlg:Destroy()

   if lOpen
      Select( cUseAlias )
      USE
   endif
   if lOpen1
      Select( cUseAlias1 )
      USE
   endif

   WvtSetBlocks( aObjects )
   WvtSetKeys( .t. )
   Wvt_SetPopupMenu( hPopup )
   SetMouseCheck( lChkMouse )

   RETURN Nil

//-------------------------------------------------------------------//

STATIC FUNCTION MyDialogTwo()
   LOCAL aObjects := WvtSetBlocks( {} )
   LOCAL oDlg     := WvtDialog():New( 30,90,'My Dialog Two' )
   LOCAL g_oMenuBar, oMenu, oPBar
   LOCAL oPBar1, oPBar2, oPBar3, oPBar4

   g_oMenuBar    := WvtMenu():new():create()
   oMenu         := wvtMenu():new():create()
   oMenu:Caption := 'Miscellaneous'
   oMenu:AddItem( 'Progressbar', {|| ExeProgBar( oPBar,oPBar1,oPBar2,oPBar3, oPBar4 ) } )
   oMenu:AddItem( '-' )
   oMenu:AddItem( 'Exit',        {|| Wvt_Keyboard( K_ESC ) } )
   g_oMenuBar:addItem( "",oMenu )

   oDlg:oMenu := g_oMenuBar

   oPBar := WvtProgressBar():New( oDlg, , 3, 10, 5, 80 )
   oPBar:nBarColor   := RGB( 0,240,240 )
   oPBar:cBackColor  := 'W/N*'
   oPBar:nDirection  := 1
   oPBar:cImage      := 'vouch1.bmp'
   oDlg:AddObject( oPBar )

   oPBar1 := WvtProgressBar():New( oDlg, , 7, 10, 8, 80 )
   oPBar1:nBarColor  := RGB( 11,255,196 )
   oPBar1:cBackColor := 'W/N*'
   oPBar1:nDirection := 0
   oDlg:AddObject( oPBar1 )

   oPBar2 := WvtProgressBar():New( oDlg, , 11, 10, 28, 19 )
   oPBar2:nBarColor  := RGB( 240,240,0 )
   oPBar2:cBackColor := 'W/N*'
   oPBar2:lVertical  := .t.
   oPBar2:nDirection := 0
   oPBar2:cImage     := 'v_notes.ico'
   oDlg:AddObject( oPBar2 )

   oPBar3 := WvtProgressBar():New( oDlg, , 11, 77, 28, 80 )
   oPBar3:nBarColor  := RGB( 0,0,255 )
   oPBar3:cBackColor := 'W/N*'
   oPBar3:lVertical  := .t.
   oPBar3:nDirection := 1
   oDlg:AddObject( oPBar3 )

   oPBar4 := WvtProgressBar():New( oDlg, , 22, 22, 28, 74 )
   oPBar4:nBarColor  := RGB( 255,255,0 )
   oPBar4:cBackColor := 'W/N*'
   oPBar4:lVertical  := .t.
   oPBar4:nDirection := 0
   oDlg:AddObject( oPBar4 )

   oDlg:Create()
   oDlg:Execute()
   oDlg:Destroy()

   WvtSetBlocks( aObjects )
   RETURN Nil

//-------------------------------------------------------------------//

STATIC FUNCTION ExeProgBar( oPBar,oPBar1,oPBar2,oPBar3,oPBar4 )
   LOCAL i

   oPBar:Activate()
   oPBar1:Activate()
   oPBar2:Activate()
   oPBar3:Activate()
   oPBar4:Activate()
   for i := 1 to 100
      oPBar:Display( i, 100 )
      oPBar1:Display( i, 100 )
      oPBar2:Display( i, 100 )
      oPBar3:Display( i, 100 )
      oPBar4:Display( i, 100 )
      inkey( 0.3 )
   next
   inkey( 0 )
   oPBar:DeActivate()
   oPBar1:DeActivate()
   oPBar2:DeActivate()
   oPBar3:DeActivate()
   oPBar4:DeActivate()

   RETURN nil
//-------------------------------------------------------------------//

STATIC FUNCTION ExeProgressBar( oPBar, oPBar3 )
   LOCAL i

   oPBar:Activate()
   oPBar3:Activate()
   for i := 1 to 100
      oPBar:Display( i, 100 )
      oPBar3:Display( i, 100 )
      inkey( 0.3 )
   next
   oPBar:DeActivate()
   oPBar3:DeActivate()

   RETURN NIL

//-------------------------------------------------------------------//

Function DynDialog_1()
   Local hDlg, aDlg, nStyle

   Static nInfo := 1
   nInfo++

   nStyle := + WS_CAPTION    + WS_SYSMENU               ;
             + WS_GROUP      + WS_TABSTOP + DS_SETFONT  ;
             + WS_THICKFRAME + WS_VISIBLE + WS_POPUP

   aDlg := Wvt_MakeDlgTemplate( 1, 2, 15, 40, {0,0,0,0},  ;
                     ltrim( str( nInfo,10,0 ) ) + ' - Modeless Dialog', nStyle )

   nStyle := WS_VISIBLE + WS_TABSTOP + ES_AUTOVSCROLL + ES_MULTILINE + ES_WANTRETURN + WS_BORDER + WS_VSCROLL
   aDlg   := Wvt_AddDlgItem( aDlg,  1, 2, 9, 28, {}, 10, 'EDIT'  , nStyle, /* cText, nHelpId, nExStyle */ )

   nStyle := WS_VISIBLE + SS_ETCHEDHORZ
   aDlg   := Wvt_AddDlgItem( aDlg, 12, 2, 1, 36, {}, 12, 'STATIC', nStyle )

   nStyle := WS_VISIBLE + WS_TABSTOP + BS_AUTOCHECKBOX
   aDlg   := Wvt_AddDlgItem( aDlg, 13, 2, 1, 10, {}, 11, 'BUTTON', nStyle, 'Is It Checked?' )

   hDlg := Wvt_CreateDialog( aDlg, .f., 'DynDlgProc_1' )

   Return hDlg

//-------------------------------------------------------------------//

Function DynDlgProc_1( hDlg, nMsg, wParam, lParam )
   Local cText, lClicked

   Switch ( nMsg )

   case WM_INITDIALOG
      Win_SetDlgItemText( hDlg, 10, 'This is multiline text which will be displayed in the edit window!' )
      Win_CheckDlgButton( hDlg, 11, .t. )
      exit

   case WM_DESTROY
      // Do whatevert you want to do with cText
      // Each box will retrieve its own text.
      //
      cText := Win_GetDlgItemText( hDlg, 10 )

      exit

   case WM_TIMER
      // Do some processing

      exit

   case WM_COMMAND
      do case

      case wParam == 11
         lClicked := ( Win_IsDlgButtonChecked( hDlg,11 ) == 1 )
         Win_MessageBox( hDlg, 'Button ' + iif( lClicked, 'Clicked', 'Unclicked' ), 'CheckBoxStatus' )

      endcase
      exit

   end

   Return .f.

//-------------------------------------------------------------------//

#define ID_BTN_OK          1
#define ID_MLE            10
#define ID_CHK_SATIS      11
#define ID_EDT_TIME       51
#define ID_LST_LIST       13
#define ID_CMB_COMBO      31
#define ID_RDO_XH         21
#define ID_RDO_CLIP       22
#define ID_RDO_XBASE      23
#define ID_EDT_TEXT       14
#define ID_EDT_NUMB       15
#define ID_STA_TEXT       71
#define ID_STA_IMAGE      72
#define ID_ICO_VOUCH      81

#define ID_GRP_COMP      113

#define ID_MNU_FILE      201
#define ID_MNU_CONTROL   202

//-------------------------------------------------------------------//

Function DynDialog_2()
   Local hDlg, aDlg, nStyle, nTimerTicks, cDlgIcon, cDlgProc, lOnTop, hMenu, nProc, bDlgProc

   Static nInfo := 0
   nInfo++

   nStyle := DS_SETFONT + WS_VISIBLE + WS_POPUP + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX

   aDlg := Wvt_MakeDlgTemplate( 1, 4, 21, 60, {0,0,0,0},  ;
               'Dialog First [ ' + ltrim( str( nInfo,10,0 ) ) + ' ] '+;
                        iif( nInfo%2==0, 'Modeless', 'Modal' ),' Dialog !', nStyle )

   // Multi line edit control
   //
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOVSCROLL + ES_MULTILINE + ;
             ES_WANTRETURN + WS_BORDER  + WS_VSCROLL
   aDlg   := Wvt_AddDlgItem( aDlg,  1, 2, 15, 35, {}, ID_MLE       , 'EDIT'   , nStyle, /* cText, nHelpId, nExStyle */ )

   // Two Horz and Vert Lines
   //
   nStyle := WS_CHILD + WS_VISIBLE + SS_ETCHEDVERT
   aDlg   := Wvt_AddDlgItem( aDlg, 1, 39,  16, 1, {}, 111          , 'STATIC' , nStyle )
   nStyle := WS_CHILD + WS_VISIBLE + SS_ETCHEDHORZ
   aDlg   := Wvt_AddDlgItem( aDlg, 17, 2,  1, 56, {}, 112          , 'STATIC' , nStyle )

   // Icon
   nStyle := WS_CHILD + WS_VISIBLE + SS_ICON //+ SS_CENTERIMAGE
   aDlg   := Wvt_AddDlgItem( aDlg, 18, 2, 2, 6, {}, ID_ICO_VOUCH  , 'STATIC' , nStyle, '' )
/*
   // Bitmap
   nStyle := WS_CHILD + WS_VISIBLE + SS_BITMAP + SS_REALSIZEIMAGE
   aDlg   := Wvt_AddDlgItem( aDlg, 18, 41, 2,8, {-3,0,3}, ID_STA_IMAGE, 'STATIC' , nStyle, '' )
*/
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTOCHECKBOX
   aDlg   := Wvt_AddDlgItem( aDlg, 18, 15,  1, 10, {}, ID_CHK_SATIS , 'BUTTON' , nStyle, 'Satisfied?' )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_RIGHT + ES_READONLY
   aDlg   := Wvt_AddDlgItem( aDlg, 18, 30, 1,  7, {3}, ID_EDT_TIME , 'EDIT' , nStyle, '' )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_NOTIFY + WS_VSCROLL + WS_BORDER
   aDlg   := Wvt_AddDlgItem( aDlg, 1, 41,  4, 17, {}, ID_LST_LIST  , 'LISTBOX', nStyle, 'ListBox'  )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := Wvt_AddDlgItem( aDlg, 4, 41,  1, 17, {3,0,0,0}, -1    , 'STATIC' , nStyle, 'Degree'     )
   nStyle := WS_VISIBLE + WS_TABSTOP + CBS_DROPDOWNLIST + WS_BORDER + WS_VSCROLL
   aDlg   := Wvt_AddDlgItem( aDlg, 5, 41,  6, 17, {}, ID_CMB_COMBO , 'COMBOBOX' , nStyle, 'Combo' )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_GROUPBOX
   aDlg   := Wvt_AddDlgItem( aDlg, 7, 41,  4, 17, {0,0,4,0},ID_GRP_COMP, 'BUTTON' , nStyle, 'Compiler' )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTORADIOBUTTON
   aDlg   := Wvt_AddDlgItem( aDlg, 8, 43,  1, 14, {}, ID_RDO_XH    , 'BUTTON' , nStyle, '(x)Harbour' )
   aDlg   := Wvt_AddDlgItem( aDlg, 9, 43,  1, 14, {}, ID_RDO_CLIP  , 'BUTTON' , nStyle, 'Clipper'  )
   aDlg   := Wvt_AddDlgItem( aDlg,10, 43,  1, 14, {}, ID_RDO_XBASE , 'BUTTON' , nStyle, 'Xbase++'  )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := Wvt_AddDlgItem( aDlg, 12, 41, 1, 17, {3,0,0,0}, ID_STA_TEXT, 'STATIC' , nStyle, 'Scrollable Text'    )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOHSCROLL + WS_BORDER
   aDlg   := Wvt_AddDlgItem( aDlg, 13, 41, 1, 17, {}, ID_EDT_TEXT  , 'EDIT'   , nStyle, 'This is Text Field' )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := Wvt_AddDlgItem( aDlg, 14, 41, 1, 17, {3,0,0,0}, -1, 'STATIC' , nStyle, 'Right Justified Numerics' )
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOHSCROLL + ES_NUMBER + ES_RIGHT + WS_BORDER
   aDlg   := Wvt_AddDlgItem( aDlg, 15, 41, 1, 17, {}, ID_EDT_NUMB  , 'EDIT'   , nStyle, '1234567' )

   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_PUSHBUTTON
   aDlg   := Wvt_AddDlgItem( aDlg, 18, 50, 1,  8, {-3,0,3,0}, ID_BTN_OK, 'BUTTON' , nStyle, 'OK' )

   hMenu  := Wvt_CreateMenu()
   Wvt_AppendMenu( hMenu, MF_STRING + MF_ENABLED, ID_MNU_FILE   , 'File'     )
   Wvt_AppendMenu( hMenu, MF_STRING + MF_ENABLED, ID_MNU_CONTROL, 'Controls' )

   lOnTop      := .f.
   cDlgProc    := 'DynDlgProc_2'
   bDlgProc    := {|a,b,c,d| DYNDLGPROC_2(a,b,c,d) }
   cDlgIcon    := 'V_Notes.Ico'
   nTimerTicks := 1000  // 1 second

   if nInfo % 2 == 1
      // Modal Dialog
      //
      //hDlg := Wvt_DialogBox( aDlg, bDlgProc, Wvt_GetWindowHandle() )
      hDlg := Wvt_DialogBox( aDlg, cDlgProc, Wvt_GetWindowHandle() )
   else
      // Modeless Dialog
      //
      hDlg := Wvt_CreateDialog( aDlg, lOnTop, cDlgProc, cDlgIcon, /*nTimerTicks*/, hMenu )

      // Using Function name.
      //hDlg  := Wvt_CreateDialog( aDlg, lOnTop, cDlgProc, cDlgIcon, nTimerTicks, hMenu, lModal )
   endif

   Return hDlg

//-------------------------------------------------------------------//

Function DynDlgProc_2( hDlg, nMsg, wParam, lParam )
   Local cText, lClicked, cPrompt, nIndex, hFont

   Switch ( nMsg )

   case WM_TIMER
      Win_SetDlgItemText( hDlg, ID_EDT_TIME, Time() )
      exit

   case WM_COMMAND
      do case

      case wParam == ID_CHK_SATIS
         lClicked := ( Win_IsDlgButtonChecked( hDlg,ID_CHK_SATIS ) == 1 )
         Win_MessageBox( hDlg, iif( lClicked, 'Satisfied', 'UnSatisfied' ), 'CheckBoxStatus' )

      case wParam == ID_RDO_XH
         Win_MessageBox( hDlg, '(x)Harbour', 'Compiler' )

      case wParam == ID_RDO_CLIP
         Win_MessageBox( hDlg, 'Clipper', 'Compiler' )

      case wParam == ID_RDO_XBASE
         Win_MessageBox( hDlg, 'Xbase++', 'Compiler' )

      case wParam == ID_MNU_FILE
         Win_MessageBox( hDlg, 'Execute Menu Action!', 'File' )

      case wParam == ID_MNU_CONTROL
         Win_MessageBox( hDlg, 'Controls are from Windows!', 'Controls' )

      case Win_LoWord( wParam ) == ID_LST_LIST
         if Win_HiWord( wParam ) == LBN_SELCHANGE
            nIndex  := Win_SendMessage( Win_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETCURSEL, 0, 0 )
            cPrompt := space( 20 )
            Win_SendMessage( Win_GetDlgItem( hDlg, ID_LST_LIST ), LB_GETTEXT, nIndex, @cPrompt )
            Win_MessageBox( hDlg, cPrompt, 'ListBox' )
         endif

      case Win_LoWord( wParam ) == ID_CMB_COMBO
         if Win_HiWord( wParam ) == CBN_SELCHANGE
            nIndex  := Win_SendMessage( Win_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETCURSEL, 0, 0 )
            cPrompt := space( 20 )
            Win_SendMessage( Win_GetDlgItem( hDlg, ID_CMB_COMBO ), CB_GETLBTEXT, nIndex, @cPrompt )
            Win_MessageBox( hDlg, cPrompt, 'Combo Box' )
         endif

      endcase
      exit

   case WM_CTLCOLOREDIT
      if ( Win_GetDlgItem( hDlg,ID_MLE ) == lParam )
         Win_SetTextColor( wParam, RGB( 0,0,255 ) )
         Win_SetBkColor( wParam, RGB( 255,255,200 ) )
         return ( 1 )
      elseif ( Win_GetDlgItem( hDlg,ID_EDT_TEXT ) == lParam )
         Win_SetTextColor( wParam, RGB( 255,255,255 ) )
         Win_SetBkColor( wParam, RGB( 10,200,45 ) )
         Return ( 1 )
      endif

      exit

   case WM_CTLCOLORSTATIC
      if ( Win_GetDlgItem( hDlg,ID_STA_TEXT ) == lParam )
         Win_SetTextColor( wParam, RGB( 255,255,255 ) )
         Return ( 1 )
      endif
      exit

   case WM_INITDIALOG
      Win_SetTimer( hDlg, 5001, 1000 ) // 1 sec

      if empty( ahFonts )
         if ( hFont := Wvt_CreateFont( "Times New Roman", 18 ) ) != 0
            aadd( ahFonts, hFont )
         endif
      endif

      if len( ahFonts ) > 0
         Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), WM_SETFONT, ahFonts[ 1 ], 0 )
      endif

      if shIcon == nil
         shIcon := Win_LoadIcon( 'Vr_1.ico' )
      endif
      if shIcon != nil .or. shIcon != 0
         Win_SendMessage( Win_GetDlgItem( hDlg, ID_ICO_VOUCH ), STM_SETIMAGE, IMAGE_ICON, shIcon )
      endif

      /*
      if shImage == nil
         shImage := Win_LoadImage( 'Vouch1.bmp', 2 )
      endif
      if shImage != nil .and. shImage != 0
         Win_SendMessage( Win_GetDlgItem( hDlg, ID_STA_IMAGE ), STM_SETIMAGE, IMAGE_BITMAP, shImage )
      endif
      */
      Win_SetDlgItemText( hDlg, ID_MLE      , GetEditText() )
      Win_CheckDlgButton( hDlg, ID_CHK_SATIS, .t.           )

      Win_CheckRadioButton( hDlg, ID_RDO_XH, ID_RDO_XBASE, ID_RDO_XH )

      Wvt_LBAddString( hDlg, ID_LST_LIST, '(x)Harbour'  )
      Wvt_LBAddString( hDlg, ID_LST_LIST, 'Gtwvt'     )
      Wvt_LBAddString( hDlg, ID_LST_LIST, 'Wvtgui'    )
      Wvt_LBAddString( hDlg, ID_LST_LIST, 'Modeless'  )
      Wvt_LBAddString( hDlg, ID_LST_LIST, 'Dialogs'   )
      Wvt_LBAddString( hDlg, ID_LST_LIST, 'WVT'       )

      Wvt_LBSetCurSel( hDlg, ID_LST_LIST, 1 )

      Wvt_CBAddString( hDlg, ID_CMB_COMBO, 'First'    )
      Wvt_CBAddString( hDlg, ID_CMB_COMBO, 'Second'   )
      Wvt_CBAddString( hDlg, ID_CMB_COMBO, 'Third'    )
      Wvt_CBAddString( hDlg, ID_CMB_COMBO, 'Fourth'   )
      Wvt_CBAddString( hDlg, ID_CMB_COMBO, 'Fifth'    )

      Wvt_CBSetCurSel( hDlg, ID_CMB_COMBO, 1 )

      Win_InvalidateRect( hDlg )

      exit

   case WM_DESTROY
      // Do whatevert you want to do with cText
      // Each box will retrieve its own text.
      //
      cText := Win_GetDlgItemText( hDlg, ID_MLE )
      cText := nil
      exit

   end

   Return ( 0 )

//-------------------------------------------------------------------//

static function GetEditText()
   Local cText := ''

   cText += 'Welcome in the Wonderful World of (x)Harbour!'
   cText += CRLF + CRLF
   cText += 'When Peter Rees first published GTWVT, a Windows '
   cText += 'Terminal Driver, on 22 Dec 2003, everybody took it '
   cText += 'lightly, except for me, as I was aware that what '
   cText += 'wonderful contribution to (x)Harbour he has made, '
   cText += 'what immense possibilities he has opened for (x)Harbour '
   cText += 'developers, what limitations he has cleared for Clipper '
   cText += 'savvy user base.'
   cText += CRLF + CRLF
   cText += 'With a little effort I could extend GTWVT '
   cText += 'to give it a GUI look. I also tried to give it '
   cText += 'an event driven functionality, and up came Wvt*Classes.'
   cText += CRLF + CRLF
   cText += 'And yet another feather is added in the cap of GTWVT '
   cText += 'as it is now capable of firing modeless dialogs like the one '
   cText += 'you are viewing. These dialogs can be constructed dynamically ( Courtesy What32 ) '
   cText += 'at run time or can be one of resources. At present 20 such dialogs '
   cText += 'can be active at any given time. Also note that dialogs created '
   cText += 'dynamically respect Top, Left, Rows, Cols coordinates, which is an '
   cText += 'undisputed productivity boost!'
   cText += CRLF + CRLF
   cText += 'Enjoy!' + CRLF
   cText += 'Pritpal Bedi, INDIA'

   Return cText

//-------------------------------------------------------------------//

EXIT PROCEDURE CleanHandles()
   LOCAL i

   for i := 1 to len( ahFonts )
      Win_DeleteObject( ahFonts[ i ] )
   next

   if shIcon != nil
      Win_DeleteObject( shIcon )
   endif

   if shImage != nil
      Win_DeleteObject( shImage )
   endif

   Return

//-------------------------------------------------------------------//

FUNCTION DlgSlideShow()
   LOCAL hDlg, aDlg, nStyle

   aSlides := { 'Vouch1.bmp', 'V_Notes.ico', '2000.gif', 'V_Lock.bmp', 'V_Help.ico' }

   nStyle  := DS_SETFONT + WS_VISIBLE + WS_POPUP + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX

   aDlg    := Wvt_MakeDlgTemplate( 0, 0, 20, 40, {}, 'Slide Show', nStyle )

   hDlg    := Wvt_CreateDialog( aDlg, .f., 'DlgSlideShowProc', 'Vr_1.ico', 5000 )

   Return hDlg

//-------------------------------------------------------------------//

FUNCTION DlgSlideShowProc( hDlg, nMsg, wParam, lParam )
   LOCAL  aRect, hDC
   STATIC nSlide := 1

   Switch nMsg

   case WM_INITDIALOG
      DrawSlide( hDlg, nSlide )
      exit

   case WM_PAINT
      DrawSlide( hDlg, nSlide )
      exit

   case WM_TIMER
      nSlide++
      if nSlide > len( aSlides )
         nSlide := 1
      endif
      DrawSlide( hDlg, nSlide )

      exit

   end

   Return ( 0 )

//-------------------------------------------------------------------//

FUNCTION DrawSlide( hDlg, nSlide )
   LOCAL hDC, aRect

   hDC   := Win_GetDC( hDlg )
   aRect := Win_GetClientRect( hDlg )

   Win_Rectangle( hDC, aRect[ 1 ]+10, aRect[ 2 ]+10, aRect[ 3 ]-10, aRect[ 4 ]-10 )
   Win_DrawImage( hDC, aRect[ 1 ]+10, aRect[ 2 ]+10, aRect[ 3 ] - aRect[ 1 ] -20, ;
                                  aRect[ 4 ] - aRect[ 2 ] - 20, aSlides[ nSlide ] )

   Win_ReleaseDC( hDlg,hDC )

   Return nil

//----------------------------------------------------------------------//

