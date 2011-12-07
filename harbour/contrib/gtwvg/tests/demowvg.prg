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

#include "inkey.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

REQUEST DbfCdx
REQUEST DbfNtx

//-------------------------------------------------------------------//

#define IMAGE_VOUCH                hb_dirBase() + "vouch1.bmp"
#define IMAGE_BROWSE               hb_dirBase() + "v_browse.ico"
#define IMAGE_VR                   hb_dirBase() + "vr_1.ico"
#define IMAGE_NOTES                hb_dirBase() + "v_notes.ico"
#define IMAGE_TOOLS                hb_dirBase() + "v_tools.ico"
#define IMAGE_HELP                 hb_dirBase() + "v_notes.ico"

#define OBJ_TYPE_BUTTON            1

//-------------------------------------------------------------------//

#ifndef __SQL__
ANNOUNCE Hb_NoStartUpWindow
#endif

//-------------------------------------------------------------------//

MEMVAR cCdxExp, First, Last, City

//-------------------------------------------------------------------//

   THREAD STATIC t_wvtScreen := {}

#ifdef __XCC__
   STATIC s_paint_:= { { "", {} } }
#endif

/*----------------------------------------------------------------------*/

EXIT PROCEDURE CleanHandles()
   LOCAL obj

   FOR EACH obj IN SetFonts()
      WVG_DeleteObject( obj )
      obj := NIL
   NEXT
   FOR EACH obj IN SetIcons()
      WVG_DeleteObject( obj )
      obj := NIL
   NEXT

   RETURN

//-------------------------------------------------------------------//

PROCEDURE Main()
   LOCAL aLastPaint, clr, scr, pGT
   LOCAL hPopup
   LOCAL dDate     := ctod( "" )
   LOCAL cName     := Pad( "Pritpal Bedi", 35 )
   LOCAL cAdd1     := Pad( "60, New Professor Colony", 35 )
   LOCAL cAdd2     := Pad( "Ludhiana, INDIA", 35 )
   LOCAL cAdd3     := Pad( "http://hbide.vouch.info", 35 )
   LOCAL nSlry     := 20000
   LOCAL aBlocks   := {}
   LOCAL nColGet   := 8
   LOCAL GetList   := {}
   LOCAL nTop      := 4
   LOCAL nLft      := 4
   LOCAL nBtm      := 20
   LOCAL nRgt      := 75
   LOCAL cLabel    := "Harbour simulated GUI."
   LOCAL aObjects  := WvtSetObjects( {} )

   LOCAL oError := ErrorBlock( {|o| MyError( o ) } )

   SET DATE BRITISH

   SET( _SET_EVENTMASK, INKEY_ALL + HB_INKEY_GTEVENT )

   Wvt_SetGui( .t. )
   WvtSetKeys( .t. )
   Popups( 1 )
   Wvt_SetMouseMove( .t. )
   Wvt_SetFont( "Courier New", 18, 0, 0 )

   CLS
   Wvt_ShowWindow( SW_RESTORE )

   /* Xbase++ compatible menu protocol */
   BuildMainMenu()
   SetMode( maxrow()+1, maxcol()+1 )  /* Needed to accomodate attached menu */

   SetKey( K_F12        , {|| hb_gtInfo( HB_GTI_ACTIVATESELECTCOPY ) } )
   SetKey( K_CTRL_V     , {|| __KeyBoard( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) ) } )
   SetKey( K_RBUTTONDOWN, {|| __KeyBoard( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) ) } )

   hPopup  := Wvt_SetPopupMenu()

   pGT := SetGT( 1, hb_gtSelect() )

   /*  Force mouse pointer right below the Harbour label */
   Wvt_SetMousePos( 2,40 )

   aAdd( aBlocks, {|| Wvt_SetIcon( GetResource( "vr_1.ico" ) ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( "Vouch" ) } )
   aAdd( aBlocks, {|| Wvt_DrawLabel( 1,40, cLabel,6,, rgb(255,255,255), rgb(198,198,198), "Arial", 26, , , , , .t., .t. ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) } )
   aAdd( aBlocks, {|| Wvt_DrawImage( 8,62,12,69, IMAGE_VOUCH ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-2,0,maxrow()-2,maxcol(),WVT_LINE_HORZ,WVT_LINE_RECESSED,WVT_LINE_BOTTOM ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,41,maxrow(),41,WVT_LINE_VERT,WVT_LINE_RECESSED,WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   aAdd( aBlocks, {|| Wvt_Mouse( -1000001 ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   /* XBase++ compatible pure GUI controls onto CUI console */
   BuildButtons()

   scr := SaveScreen( 0,0,maxrow(),maxcol() )
   clr := SetColor( "N/W" )
   CLS
   SetColor( "N/W,N/GR*,,,N/W*" )

   @  6, nColGet SAY "< Date >"
   @  9, nColGet SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12, nColGet SAY "<" + PadC( "Address", 33 ) + ">"
   @ 16, 61      SAY "< Salary >"

   dDate := ctod( "04/01/04" )

   @  7, nColGet GET dDate WHEN  DispStatusMsg( "Date must be valid" ) VALID ClearStatusMsg()
   @ 10, nColGet GET cName WHEN  DispStatusMsg( "Must be one of the list!" ) VALID ( VouChoice() < 7 .and. ClearStatusMsg() )
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE "@Z 9999999.99"

#if 0   /* Suitable when whole window is subject to this protocol */
   aEval( GetList, {|oGet| Wvg_BoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } )
   SetAppWindow():refresh()
#endif

   READ

   //  Restore Environment
   //
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )
   WvtSetKeys( .f. )
   Wvt_SetPopupMenu( hPopup )

   Popups( 1, .t. )
   SetGT( 1, pGT )

   ErrorBlock( oError )

   RETURN

//-------------------------------------------------------------------//

Function HB_GTSYS()
   REQUEST HB_GT_WVG_DEFAULT
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WGU
   RETURN NIL

//------------------------------------------------------------------//

PROCEDURE WvtConsoleGets( nMode )

   DEFAULT nMode TO 0

   IF hb_mtvm()
      Hb_ThreadStart( {|oCrt|  hb_gtReload( 'WVT' ) , ;
                               oCrt := hb_gtSelect(), ;
                               iif( nMode == 0, WvtNextGetsConsole(), GoogleMap() ) , ;
                               oCrt := NIL        } )
   ENDIF

   RETURN

//----------------------------------------------------------------------//

PROCEDURE WvtNextGetsConsole()
   LOCAL dDate      := ctod( "" )
   LOCAL cName      := Space( 35 )
   LOCAL cAdd1      := Space( 35 )
   LOCAL cAdd2      := Space( 35 )
   LOCAL cAdd3      := Space( 35 )
   LOCAL nSlry      := 0
   LOCAL nColGet    := 8
   LOCAL GetList    := {}

   SetMode( 20,51 )
   SetColor( "N/W,N/GR*,,,N/W*" )
   CLS
   hb_gtInfo( HB_GTI_WINTITLE, "WVT Console in WVG Application" )

   @ MaxRow(), 0 SAY PadC( "GTWVT in GTWVG Console Gets", maxcol()+1 ) COLOR "W+/B*"

   @  2, nColGet SAY "< Date >"
   @  5, nColGet SAY "<" + PadC( "Name", 33 ) + ">"
   @  8, nColGet SAY "<" + PadC( "Address", 33) + ">"
   @ 15, nColGet SAY "< Salary >"

   @  3, nColGet GET dDate
   @  6, nColGet GET cName
   @  9, nColGet GET cAdd1
   @ 11, nColGet GET cAdd2
   @ 13, nColGet GET cAdd3
   @ 16, nColGet GET nSlry PICTURE "@Z 9999999.99"

   READ

   RETURN

//-------------------------------------------------------------------//

PROCEDURE WvtNextGets()

   IF hb_mtvm()
      Hb_ThreadStart( {||  Hb_gtReload( 'WVG' ), Wvt_setFont( 'Terminal',20 ), ;
                           hb_clear(), Wvt_ShowWindow( SW_RESTORE ), WvtNextGets_X() } )
   ELSE
      WvtNextGets_X()
   ENDIF

   RETURN

//----------------------------------------------------------------------//

PROCEDURE WvtNextGets_X()
   LOCAL aLastPaint, clr
   LOCAL dDate      := ctod( "" )
   LOCAL cName      := Space( 35 )
   LOCAL cAdd1      := Space( 35 )
   LOCAL cAdd2      := Space( 35 )
   LOCAL cAdd3      := Space( 35 )
   LOCAL nSlry      := 0
   LOCAL aBlocks    := {}
   LOCAL nColGet    := 8
   LOCAL GetList    := {}
   LOCAL aPalette   := Wvt_GetPalette()
   LOCAL aNewPalette:= aclone( aPalette )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL nRow       := Row()
   LOCAL nCol       := Col()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL wvtScr     := Wvt_SaveScreen( 0,0,maxrow(),maxcol() )

   STATIC nPalletMultiplier := 0

   // Change the values of pallatte arbitrarily though yu can fine tune
   // these values with realistic values.
   //
   aNewPalette[ 8 ] := aNewPalette[ 8 ] + ( 100000 * ++nPalletMultiplier )

   Wvt_SetPalette( aNewPalette )

   aAdd( aBlocks, {|| Wvt_SetTitle( "Wvt Gets 2nd Window with Different Palette" ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,0,maxrow()-1,maxcol() ) } )
   aAdd( aBlocks, {|| Wvt_SetBrush( 0, rgb( 32,255,100 ) )    } )
   aAdd( aBlocks, {|| Wvt_DrawEllipse( 6,50,10,58 )           } )
   aAdd( aBlocks, {|| Wvt_SetBrush( 2, rgb( 255,255,100 ),1 ) } )
   aAdd( aBlocks, {|| Wvt_DrawRectangle( 11, 50, 13, 58 )     } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroupRaised( 5, 6, 19, 72 )  } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   aAdd( aBlocks, {|| Wvt_DrawButton( 21, 6,22, 9,"New"   ,"vouch1.bmp" )                             } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,11,22,14,"Browse","vouch1.bmp", 1, rgb( 255,255,255 ) )      } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,16,22,19, ,"vouch1.bmp" )                                    } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,21,22,24,"Data",, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,26,22,29,"Flat",IMAGE_VR,2 )                                 } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,31,22,34,"Outline",IMAGE_VR,3 )                              } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 22,36,22,41,"Data",, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   clr := SetColor( "N/W,N/GR*,,,N/W*" )
   CLS

   @ MaxRow(), 0 SAY PadC( "Harbour + WVT Console GUI Screen",80 ) COLOR "R+/W"

   @  6, nColGet SAY "< Date >"
   @  9, nColGet SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12, nColGet SAY "<" + PadC( "Address", 33) + ">"
   @ 16, 61      SAY "< Salary >"

   @  7, nColGet GET dDate
   @ 10, nColGet GET cName
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE "@Z 9999999.99"

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

FUNCTION WvtPartialScreen()
   LOCAL scr        := SaveScreen( 7,20,15,60 )
   LOCAL wvtScr     := Wvt_SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL wvtScr1
   LOCAL aLastPaint
   LOCAL hPopup     := Wvt_SetPopupMenu()

   aLastPaint := WvtSetBlocks( {} )

   DispBox( 7, 20, 15, 60, "         ", "W/GR*" )
   @ 10,25 SAY "Wvt_SaveScreen()" COLOR "N/GR*"
   @ 11,25 SAY "Wvt_RestScreen()" COLOR "N/GR*"
   @ 13,25 SAY "Press Esc "       COLOR "N/GR*"
   Wvt_DrawBoxRecessed( 8,22,14,58 )

   wvtScr1 := Wvt_SaveScreen( 7,20,15,60 )

   DO WHILE inkey( 0 ) != K_ESC
   ENDDO

   DispBox( 7, 20, 15, 60, "         ", "W/B*" )
   @ 10,25 SAY "Wvt_SaveScreen()" COLOR "N/B*"
   @ 11,25 SAY "Wvt_RestScreen()" COLOR "N/B*"
   @ 13,25 SAY "Press Esc "       COLOR "N/B*"
   Wvt_DrawBoxRecessed( 8,22,14,58 )

   DO WHILE inkey( 0 ) != K_ESC
   ENDDO

   Wvt_RestScreen( 7,20,15,60, wvtScr1 )

   DO WHILE inkey( 0 ) != K_ESC
   ENDDO

   RestScreen( 7,20,15,60,scr )
   Wvt_RestScreen( 0, 0, MaxRow(), MaxCol(), wvtScr )
   WvtSetBlocks( aLastPaint )
   Wvt_SetPopupMenu( hPopup )

   RETURN NIL

//-------------------------------------------------------------------//

function WvtLines()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL clr        := SetColor( "N/W" )
   LOCAL nRows      := maxrow()
   LOCAL nCols      := maxcol()
   LOCAL aLastPaint := WvtSetBlocks( {} )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL hPopup     := Wvt_SetPopupMenu()
   LOCAL aBlocks    := {}

   CLS

   aAdd( aBlocks, {|| Wvt_DrawLine( 0, 0, 0, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 1, 0, 1, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_TOP )    } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 2, 0, 2, nCols, WVT_LINE_HORZ, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_SOLID, 4, Rgb( 255,255,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 3, 0, 3, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER, WVT_LINE_DASH , 0, Rgb( 255,0,0 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 4, 0, 4, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_BOTTOM ) } )

   @ 0, 1 SAY "Center Raised"
   @ 1,11 say "Top Recessed"
   @ 2,21 say "Center Plain White 3 Pixels"
   @ 3,31 say "Center Raised Dotted"
   @ 4,41 SAY "Bottom Recessed"
   @ 5, 1 SAY "Bottom Checked"

   @ nRows, 0 Say PadC( "Press ESC to Quit", nCols+1 ) COLOR "GR+/W"

   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 5,nRows-2, 5, WVT_LINE_VERT, WVT_LINE_RAISED  , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 6,nRows-2, 6, WVT_LINE_VERT, WVT_LINE_RECESSED, WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 7,nRows-2, 7, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_LEFT   ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 8,nRows-2, 8, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11, 9,nRows-2, 9, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_RIGHT  ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,10,nRows-2,10, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DOT,     0, RGB( 0,0,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,11,nRows-2,11, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASH,    0, RGB( 255,0,0 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( 11,12,nRows-2,12, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASHDOT, 0, RGB( 0,255,0 ) ) } )

   WvtSetBlocks( aBlocks )

   @ 12,5 Say "A"
   @ 13,6 Say "B"
   @ 14,7 Say "C"
   @ 15,8 Say "D"
   @ 16,9 Say "E"

   DO WHILE ( inkey(0) != K_ESC )
   ENDDO

   //  Restore Environments
   //
   SetColor( clr )

   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )
   Wvt_SetPopupMenu( hPopup )

   RestScreen( 0,0,maxrow(),maxcol(), scr )

   RETURN NIL

//-------------------------------------------------------------------//

FUNCTION BuildMainMenu()
   LOCAL oMenu
   LOCAL g_oMenuBar := SetAppWindow():menuBar()

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption:= "Wvt*Classes"
   oMenu:AddItem( "Dialog One . New Window . Threaded"       , {|| DialogWvgClassesOne( 1 ) } )
   oMenu:AddItem( "Dialog One . Main Window . Primary Thread", {|| DialogWvgClassesOne( 2 ) } )
   oMenu:AddItem( "-" )
   oMenu:AddItem( "Dialog Two"                  , {|| DialogWvgClassesTwo()       } )
   oMenu:AddItem( "-" )
   oMenu:AddItem( "Exit"                        , {|| __keyboard( K_ESC ) } )
   g_oMenuBar:addItem( { oMenu, "Wvt*Classes" } )

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:caption := "Traditional"
   oMenu:AddItem( "Gets . GTWVG . Threaded"     , {|| WvtNextGets()       } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Gets . GTWVT . Threaded"     , {|| WvtConsoleGets( 0 ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Browser . GTWVG . Threaded " , {|| WvtMyBrowse()       } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Partial Screen . Main Window", {|| WvtPartialScreen()  } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Wvt Lines . Main Window"     , {|| WvtLines()          } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Google Maps"                 , {|| WvtConsoleGets( 1 ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Wvg Console with GCUI"       , {|| ExecGCUI()          } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Modal Window"                , {|| DoModalWindow()     } )
   g_oMenuBar:addItem( { oMenu, "Traditional" } )

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption:= "Common Dialogs"
   oMenu:AddItem( "Fonts"                       , {|| Wvt_ChooseFont()  } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Colors"                      , {|| Wvt_ChooseColor() } )
   g_oMenuBar:addItem( { oMenu, "Common Dialogs" } )

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption:= "Functionality"
   oMenu:AddItem( "Expand"                      , {|| WvtWindowExpand(  1 ) } )
   oMenu:AddItem( "Shrink"                      , {|| WvtWindowExpand( -1 ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Minimize"                    , {|| Wvt_Minimize()   } )
   oMenu:AddItem( "Maximize"                    , {|| hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED ) } )
   g_oMenuBar:addItem( { oMenu, "Functionality" } )

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption:= "Modeless Dialogs"
   oMenu:AddItem( "Dynamic Dialog . Modeless"   , {|| DynWinDialog( 1 ) } )
   oMenu:AddItem( "Dynamic Dialog . Modal "     , {|| DynWinDialog( 2 ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "Slide Show . Modeless"       , {|| DlgSlideShow()   } )
   g_oMenuBar:addItem( { oMenu, "Modeless Dialogs" } )

   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption := "~XbpDialog()s"
   oMenu:AddItem( "Pure Xbase++"                , {|| Hb_ThreadStart( {|| demoXbp() } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - Internet Explorer" , {|| Hb_ThreadStart( {|| ExecuteActiveX(  1 ) } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - Visualize a PDF"   , {|| Hb_ThreadStart( {|| ExecuteActiveX(  3 ) } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - Explorer . DHTML"  , {|| Hb_ThreadStart( {|| ExecuteActiveX( 11 ) } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - RMChart"           , {|| Hb_ThreadStart( {|| ExecuteActiveX(  4 ) } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - Analog Clock"      , {|| Hb_ThreadStart( {|| ExecuteActiveX(  2 ) } ) } )
   oMenu:AddItem( "-")
   oMenu:AddItem( "ActiveX - Image Viewer"      , {|| Hb_ThreadStart( {|| ExecuteActiveX(  5 ) } ) } )
   g_oMenuBar:addItem( { oMenu, "~XbpDialog()s" } )

   #ifdef __QT__
   oMenu := WvgMenu():new( g_oMenuBar, , .t. ):create()
   oMenu:Caption:= "~QT"
   oMenu:AddItem( "All Widgets"                 , {|| Hb_ThreadStart( {|| ExeQTWidgets() } ) } )
   #endif

   RETURN NIL

//-------------------------------------------------------------------//

STATIC FUNCTION GoogleMap()
   LOCAL mfrom1, mto1, mfrom2, mto2, mfrom3, mto3, mweb
   LOCAL nCursor := setcursor()
   LOCAL getlist := {}

   SetMode( 22,65 )
   setcolor( 'N/W,N/GR*,,,N/W*' )
   cls
   hb_gtInfo( HB_GTI_WINTITLE, 'Google Maps' )

   mfrom1  := mto1  := space(20)
   mfrom2  := mto2  := space(40)
   mfrom3  := mto3  := space(50)

   WHILE .T.
      @ 05, 01 say "FROM :"
      @ 07, 01 say "State ...:" get mfrom1  picture "@!"
      @ 08, 01 say "City ....:" get mfrom2  picture "@!"
      @ 09, 01 say "Street ..:" get mfrom3  picture "@!"
      @ 11, 01 say "TO :"
      @ 13, 01 say "State ...:" get mto1    picture "@!"
      @ 14, 01 say "City ....:" get mto2    picture "@!"
      @ 15, 01 say "Street ..:" get mto3    picture "@!"

      setcursor(1); read; setcursor(nCursor)

      IF lastkey() == K_ESC
         EXIT
      ENDIF

      mweb := "http://maps.google.com/maps?q=from "         +;
              alltrim( mfrom3 ) +" "+ alltrim( mfrom2 ) +" "+ alltrim( mfrom1 ) + " to " +;
              alltrim( mto3 )   +" "+ alltrim( mto2 )   +" "+ alltrim( mto1 )

      Hb_ThreadStart( {|| ExecuteActiveX( 1, mweb ) } )
   ENDDO

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION BuildButtons()
   LOCAL oXbp

   oXbp := WvgPushButton():new()
   oXbp:caption := "Hi"
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -1 }, { -2, -4 } )
   oXbp:activate := {|| Wvg_MessageBox( , "Harbour!" ) }
   oXbp:toolTipText := "Harbour CUI/GUI Console"

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_VOUCH
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -6 }, { -2, -4 } )
   oXbp:activate := {|| Wvt_Keyboard( K_F2 ) }
   oXbp:toolTipText := "Open Another GET Screen"

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_BROWSE
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -11 }, { -2, -4 } )
   oXbp:activate := {|| Wvt_Keyboard( K_F5 ) }
   oXbp:toolTipText := "TBrowse + GUI Controls"

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_NOTES
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -16 }, { -2, -4 } )
   oXbp:activate := {|| Wvt_Keyboard( K_F3 ) }

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_TOOLS
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -21 }, { -2, -4 } )
   oXbp:activate := {|| Wvt_Keyboard( K_F6 ) }

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_HELP
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -26 }, { -2, -4 } )
   oXbp:activate := {|| Wvt_Keyboard( K_F7 ) }

   oXbp := WvgPushButton():new()
   oXbp:caption := IMAGE_VR
   oXbp:border  := .f.
   oXbp:create( , , { {|| -( maxrow()-1 ) }, -31 }, { -2, -4 } )
   oXbp:activate := {|| Hb_ThreadStart( {|| demoXbp() } ) } // {|| Wvt_Keyboard( K_F8 ) }
   oXbp:toolTipText := "Flat Button . Lines: press ESC when finished."

   RETURN NIL

/*----------------------------------------------------------------------*/



