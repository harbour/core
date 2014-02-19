//                   GTWVT Console GUI Interface
//
//         Copyright (c) Pritpal Bedi <pritpal@vouchcac.com>

#require "gtwvg"

#include "inkey.ch"
#include "hbgtinfo.ch"

#define IMAGE_VOUCH                hb_DirBase() + "vouch1.bmp"
#define IMAGE_BROWSE               hb_DirBase() + "v_browse.ico"
#define IMAGE_VR                   hb_DirBase() + "vr_1.ico"
#define IMAGE_NOTES                hb_DirBase() + "v_notes.ico"
#define IMAGE_TOOLS                hb_DirBase() + "v_tools.ico"
#define IMAGE_HELP                 hb_DirBase() + "v_notes.ico"

MEMVAR GetList

PROCEDURE Main()

   LOCAL aPaint

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVG" )
#endif

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_ALL, HB_INKEY_GTEVENT ) )

   Wvt_SetGUI( .T. )
   Wvt_SetFont( "Courier New", 18, 0, 0 )
   Wvt_SetMouseMove( .T. )

   SetColor( "N/W" )
   CLS
   Wvt_ShowWindow( SW_RESTORE )
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour's GTWVG Demo ( Simplified )" )
   Wvt_SetIcon( hb_DirBase() +  "vr_1.ico" )

   SetGT( 1, hb_gtSelect() )

   aPaint := {}

   AAdd( aPaint, { "Label", {|| Wvt_DrawLabel( 1, 40, "Harbour Console GUI Demo", 6, , RGB( 255, 255, 255 ), RGB( 198, 198, 198 ), "Arial", 26, , , , , .T., .T. ) }, { WVT_BLOCK_LABEL, 1, 10, 3, 50 } } )
   AAdd( aPaint, { "Box_1", {|| Wvt_DrawBoxRaised( 4, 4, 20, 75 ) }, { WVT_BLOCK_BOX, 4, 4, 20, 75 } } )
   AAdd( aPaint, { "Box_2", {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) }, { WVT_BLOCK_BOX, 7, 61, 13, 70 } } )
   AAdd( aPaint, { "Box_3", {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) }, { WVT_BLOCK_BOX, 15, 59, 18, 72 } } )
   AAdd( aPaint, { "Box_4", {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) }, { WVT_BLOCK_BOX, 5, 6, 19, 44 } } )
   AAdd( aPaint, { "Image", {|| Wvt_DrawImage( 8, 62, 12, 69, IMAGE_VOUCH ) }, { WVT_BLOCK_IMAGE, 8, 62, 12, 69 } } )
   AAdd( aPaint, { "Box_5", {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) }, { WVT_BLOCK_BOX, 7, 48, 13, 55 } } )
   AAdd( aPaint, { "Line_1", {|| Wvt_DrawLine( MaxRow() - 2, 0, MaxRow() - 2, MaxCol(), WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_BOTTOM ) }, NIL } )
   AAdd( aPaint, { "Gets", {|| AEval( GetList, {| oGet | Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) }, NIL } )

   ExecForm( aPaint )

   RETURN

/* This function must be linked with the application */

FUNCTION Wvt_Paint()  /* must be a public function */

   WvtPaintObjects()

   RETURN NIL

STATIC PROCEDURE ExecForm( aPaint )

   LOCAL cColor    := SetColor()
   LOCAL aPnt
   LOCAL dDate     := Date()
   LOCAL cName     := PadR( "Pritpal Bedi", 35 )
   LOCAL cAdd1     := PadR( "60, New Professor Colony", 35 )
   LOCAL cAdd2     := PadR( "Ludhiana, INDIA", 35 )
   LOCAL cAdd3     := PadR( "http://hbide.vouch.info", 35 )
   LOCAL nSlry     := 20000
   LOCAL nColGet   := 8

   aPnt := WvtSetPaint( aPaint )

   SetColor( "N/W" )
   CLS
   SetColor( "N/W,N/GR*,,,N/W*" )

   SetKey( K_F2, {|| DoModalDialog() } )

   @  6, nColGet SAY "< Date >"
   @  9, nColGet SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12, nColGet SAY "<" + PadC( "Address", 33 ) + ">"
   @ 16, 61      SAY "< Salary >"
   @  7, nColGet GET dDate WHEN  DispStatusMsg( "Date must be Valid" )
   @ 10, nColGet GET cName WHEN  DispStatusMsg( "Must be one of the list!" ) VALID ( MyChoice() < 7 )
   @ 13, nColGet GET cAdd1 WHEN  DispStatusMsg( "Press F2 to get modal window" )
   @ 15, nColGet GET cAdd2 WHEN  DispStatusMsg( "Press F2 to activate modal window" )
   @ 17, nColGet GET cAdd3 WHEN  DispStatusMsg( "Press F2 to bring in front a modal window" )
   @ 17, 61      GET nSlry PICTURE "@Z 9999999.99" WHEN ClearStatusMsg()


   READ

   SetColor( cColor )
   WvtSetPaint( aPnt )

   RETURN

STATIC FUNCTION SetGT( nIndex, pGT )

   LOCAL oldGT
   STATIC s_pGT_ := { NIL, NIL, NIL }

   oldGT := s_pGT_[ nIndex ]
   IF PCount() == 2
      s_pGT_[ nIndex ] := pGT
   ENDIF

   RETURN oldGT

STATIC FUNCTION MyChoice( aChoices )

   LOCAL scr, clr, nChoice

   hb_default( @aChoices, { "One", "Two", "Three", "Four", "Five", "Six", "Seven" } )

   scr := SaveScreen( 7, 48, 13, 55 )
   clr := SetColor( "N/W*,GR+/B*,,,GR+/B" )

   nChoice := AChoice( 7, 48, 13, 55, aChoices )

   SetColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice

STATIC FUNCTION DispStatusMsg( cMsg )

   ClearStatusMsg()

   /* NOTE: The GUI function used as such is not subject to autopainting */
   Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, RGB( 198, 198, 198 ), "Arial", 18, , 900 )

   RETURN .T.

STATIC FUNCTION ClearStatusMsg()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   hb_DispOutAt( MaxRow(), 42, Space( 37 ), "W/W" )
   SetPos( nRow, nCol )

   RETURN .T.

STATIC PROCEDURE DoModalDialog()

   LOCAL oCrt, nSel
   LOCAL aPnt   := WvtSetPaint( {} )
   LOCAL aPaint := {}

   oCrt := WvgCrt():New( , , { 4, 8 }, { 12, 49 }, , .T. )

   oCrt:lModal      := .T.
   oCrt:resizable   := .F.
   oCrt:closable    := .F.
   oCrt:title       := "Modal Dialog!"
   oCrt:icon        := hb_DirBase() + "vr_1.ico"

   oCrt:Create()
   oCrt:show()

   AAdd( aPaint, { "Box_V", {|| Wvt_DrawBoxRaised( 1, 2, 11, 47 ) }, NIL, { WVT_BLOCK_BOX, 0, 0, MaxRow(), MaxCol() } } )
   WvtSetPaint( aPaint )

   SetColor( "N/W" )
   CLS
   DO WHILE .T.
      nSel := Alert( "A modal window !;Click on parent window;Move this window", { "OK" } )
      IF nSel == 0 .OR. nSel == 1
         EXIT
      ENDIF
   ENDDO

   oCrt:Destroy()

   WvtSetPaint( aPnt )

   RETURN

#if ! defined( __HBSCRIPT__HBSHELL )

PROCEDURE hb_GTSYS()  /* must be a public function */

   REQUEST HB_GT_WVG_DEFAULT
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WGU

   RETURN

#endif
