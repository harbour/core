// Harbour Extended GT Features Demo
// Pritpal Bedi <pritpal@vouchcac.com>

#include "hbgtinfo.ch"
#include "dbstruct.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define RGB( r, g, b )  ( r + ( g * 256 ) + ( b * 256 * 256 ) )

//

STATIC s_nRows := 20
STATIC s_nCols := 60
STATIC s_nColorIndex := 1

//

STATIC s_nFont := 1
STATIC sc_aFont := { ;
   "Lucida Console", ;
   "Consolas", ;
   "Courier New", ;
   "Terminal", ;
   "DejaVu Sans Mono", ;
   "Monaco" }

//

PROCEDURE Main()

   LOCAL nKey, nKeyStd, lMark, lResize, lClose, aKeys[ 50 ], nI
   LOCAL nHeight := 20
   LOCAL nWidth  := Int( nHeight / 2 )
   LOCAL GetList := {}
   LOCAL nModeCols, nModeRows, nWndHeight, nWndWidth, nMaxWHeight, nMaxWWidth, aWndSize := { 0, 0 }

   LOCAL nMSec

#if defined( __HBSCRIPT__HBSHELL )
   #if defined( __PLATFORM__WINDOWS )
      hbshell_gtSelect( "GTWVT" )
   #elif defined( __PLATFORM__UNIX )
      hbshell_gtSelect( "GTXWC" )
   #endif
#endif

   AFill( aKeys, { 0, 0 } )

   Set( _SET_SCOREBOARD, .F. )

   hb_gtInfo( HB_GTI_FONTNAME , sc_aFont[ s_nFont ] )
   hb_gtInfo( HB_GTI_FONTWIDTH, nWidth  )
   hb_gtInfo( HB_GTI_FONTSIZE , nHeight )
   hb_gtInfo( HB_GTI_WINTITLE , "GT-Test (Resizable by FONT)" )
   hb_gtInfo( HB_GTI_ALTENTER, .T. )  // allow <Alt-Enter> for full screen
   SetCursor( SC_NONE )

   hb_gtInfo( HB_GTI_CLOSABLE, .F. )

   DispScreen()

   DO WHILE .T.
      FOR nI := 1 TO Len( aKeys )
         IF aKeys[ nI ][ 1 ] == 0 .OR. nI > MaxRow() - 1
            EXIT
         ENDIF
         hb_DispOutAt( nI, MaxCol() - 18, Str( aKeys[ nI ][ 2 ], 10 ) + " " + hb_NumToHex( aKeys[ nI ][ 1 ], 8 ) )
      NEXT

      IF nMSec != NIL .AND. hb_MilliSeconds() > nMSec + 2000
         hb_DispOutAt( MaxRow(), 0, Space( MaxCol() + 1 ), "N/G*" )
         nMSec := NIL
      ENDIF

      nKey := Inkey( 0.1, hb_bitOr( hb_bitAnd( INKEY_ALL, hb_bitNot( INKEY_MOVE ) ), HB_INKEY_GTEVENT, HB_INKEY_EXT ) )

      IF nKey == 0
         LOOP
      ENDIF

      nKeyStd := hb_keyStd( nKey )

      hb_AIns( aKeys, 1, { nKey, nKeyStd } )
      FOR nI := 1 TO Len( aKeys )
         IF aKeys[ nI ][ 1 ] == 0 .OR. nI > MaxRow() - 1
            EXIT
         ENDIF
         hb_DispOutAt( nI, MaxCol() - 18, Str( aKeys[ nI ][ 2 ], 10 ) + " " + hb_NumToHex( aKeys[ nI ][ 1 ], 8 ) )
      NEXT

      IF nKeyStd == K_ESC
         EXIT
      ENDIF

      DO CASE
      CASE nKeyStd == K_ENTER
         Alert( "<Enter> Pressed" )

      CASE nKeyStd == hb_keyCode( "0" )  // setmode
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         DO WHILE .T.
            nModeCols := MaxCol() + 1
            nModeRows := MaxRow() + 1
            hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
            hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
            hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
            hb_DispOutAt( MaxRow() / 2    , 2, "SetMode( 99, 999 )  <Esc>-Cancels" )
            @ MaxRow() / 2, 11 GET nModeRows PICTURE "99" RANGE 10, 99
            @ MaxRow() / 2, 16 GET nModeCols PICTURE "999" RANGE 20, 300
            READ
            IF LastKey() == K_ESC
               EXIT
            ENDIF
            IF SetMode( nModeRows, nModeCols )
               EXIT
            ELSE
               Alert( "SetMode() Failed!" )
            ENDIF
         ENDDO
         DispScreen()

      CASE nKeyStd == hb_keyCode( "1" )  // "1" get/set Window-Height
         nWndHeight := hb_gtInfo( HB_GTI_SCREENHEIGHT )
         nMaxWHeight := hb_gtInfo( HB_GTI_DESKTOPHEIGHT )
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 2, "Get/Set Window Height: 9999 (Max: " + hb_ntos( nMaxWHeight ) + ")" )
         @ MaxRow() / 2, 25 GET nWndHeight PICTURE "9999" RANGE 100, nMaxWHeight
         READ
         IF LastKey() != K_ESC
            hb_gtInfo( HB_GTI_SCREENHEIGHT, nWndHeight )
         ENDIF
         DispScreen()

      CASE nKeyStd == hb_keyCode( "2" )  // get/set window width
         nWndWidth := hb_gtInfo( HB_GTI_SCREENWIDTH )
         nMaxWWidth := hb_gtInfo( HB_GTI_DESKTOPWIDTH )
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 2, "Get/Set Window Width: 9999 (Max: " + hb_ntos( nMaxWWidth ) + ")" )
         @ MaxRow() / 2, 24 GET nWndWidth PICTURE "9999" RANGE 100, nMaxWWidth
         READ
         IF LastKey() != K_ESC
            hb_gtInfo( HB_GTI_SCREENWIDTH, nWndWidth )
         ENDIF
         DispScreen()

      CASE nKeyStd == hb_keyCode( "3" )  // get/set Window-Size
         aWndSize := hb_gtInfo( HB_GTI_SCREENSIZE )
         nMaxWWidth := hb_gtInfo( HB_GTI_DESKTOPWIDTH )
         nMaxWHeight := hb_gtInfo( HB_GTI_DESKTOPHEIGHT )
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 2, "Get/Set Window Size(WxH): 9999 x 9999 (Max: " + hb_ntos( nMaxWWidth ) + " x " + hb_ntos( nMaxWHeight ) + ")" )
         @ MaxRow() / 2, 28 GET aWndSize[ 1 ] PICTURE "9999" RANGE 100, nMaxWWidth
         @ MaxRow() / 2, 35 GET aWndSize[ 2 ] PICTURE "9999" RANGE 100, nMaxWHeight
         READ
         IF LastKey() != K_ESC
            hb_gtInfo( HB_GTI_SCREENSIZE, aWndSize )
         ENDIF
         DispScreen()

      CASE nKeyStd == hb_keyCode( "4" )  // set Window-Position by pixels
         aWndSize := hb_gtInfo( HB_GTI_SETPOS_XY )
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 2, "Get/Set Window Position in pixels(Left/Top): 9999 / 9999 " )
         @ MaxRow() / 2, 47 GET aWndSize[ 1 ] PICTURE "9999"
         @ MaxRow() / 2, 54 GET aWndSize[ 2 ] PICTURE "9999"
         READ
         IF LastKey() != K_ESC
            hb_gtInfo( HB_GTI_SETPOS_XY, aWndSize )
         ENDIF
         DispScreen()

      CASE nKeyStd == hb_keyCode( "5" )  // set Window-Position by row/col
         aWndSize := hb_gtInfo( HB_GTI_SETPOS_ROWCOL )
         SetColor( "W+/B,GR+/N,W/B,B/B,G+/N" )
         hb_DispOutAt( MaxRow() / 2 - 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2 + 1, 0, Space( MaxCol() ) )
         hb_DispOutAt( MaxRow() / 2    , 2, "Get/Set Window Position by Row/Col: 999 / 999 " )
         @ MaxRow() / 2, 38 GET aWndSize[ 1 ] PICTURE "999"
         @ MaxRow() / 2, 44 GET aWndSize[ 2 ] PICTURE "999"
         READ
         IF LastKey() != K_ESC
            hb_gtInfo( HB_GTI_SETPOS_ROWCOL, aWndSize[ 1 ], aWndSize[ 2 ] )
         ENDIF
         DispScreen()

      CASE nKeyStd == K_F1
         Alert( "Additional Hot-Key Test Settings:;;" + ;
            "0 - SetMode( nRows, nCols ) test     ;" + ;
            "1 - Get/Set HB_GTI_SCREENHEIGHT test ;" + ;
            "2 - Get/Set HB_GTI_SCREENWIDTH test  ;" + ;
            "3 - Get/Set HB_GTI_SCREENSIZE test   ;" + ;
            "4 - Get/Set HB_GTI_SETPOS_XY test    ;" + ;
            "5 - Get/Set HB_GTI_SETPOS_ROWCOL test", , "W+/B" )

      CASE nKeyStd == K_F2
         lMark := hb_gtInfo( HB_GTI_SELECTCOPY )
         hb_gtInfo( HB_GTI_SELECTCOPY, ! lMark )

      CASE nKeyStd == K_F3
         lResize := hb_gtInfo( HB_GTI_RESIZABLE )
         hb_gtInfo( HB_GTI_RESIZABLE, ! lResize )
         DispScreen()

      CASE nKeyStd == K_F4
         lClose := hb_gtInfo( HB_GTI_CLOSABLE )
         hb_gtInfo( HB_GTI_CLOSABLE, ! lClose )
         DispScreen()

      CASE nKeyStd == K_F5
         SetPalette( 1 )

      CASE nKeyStd == K_F6
         SetPalette( 0 )

      CASE nKeyStd == K_F7
         SetPaletteIndex()

      CASE nKeyStd == K_F8
         Alert( "Menu text changed. Was: " + hb_gtInfo( HB_GTI_SELECTCOPY, hb_TToS( hb_DateTime() ) ) )

      CASE nKeyStd == K_F9
         hb_gtInfo( HB_GTI_RESIZEMODE, iif( hb_gtInfo( HB_GTI_RESIZEMODE ) == HB_GTI_RESIZEMODE_ROWS, HB_GTI_RESIZEMODE_FONT, HB_GTI_RESIZEMODE_ROWS ) )
         hb_gtInfo( HB_GTI_WINTITLE , "GT-Test (Resizable by " + iif( hb_gtInfo( HB_GTI_RESIZEMODE ) == HB_GTI_RESIZEMODE_ROWS, "ROWS", "FONT" ) + ")" )
         DispScreen()

      CASE nKeyStd == K_F10
         IF hb_mtvm()
            hb_threadStart( @thFunc() )
         ELSE
            Alert( "MT mode not available. Rebuild this program with -mt switch and try again." )
         ENDIF

      CASE nKeyStd == K_F11
         hb_gtInfo( HB_GTI_ALTENTER, ! hb_gtInfo( HB_GTI_ALTENTER ) )
         DispScreen()

      CASE nKeyStd == K_F12
         IF ++s_nFont > Len( sc_aFont )
            s_nFont := 1
         ENDIF
         hb_gtInfo( HB_GTI_FONTNAME, sc_aFont[ s_nFont ] )

         IF hb_gtInfo( HB_GTI_RESIZEMODE ) == HB_GTI_RESIZEMODE_ROWS
            hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
            SetMode( MaxRow(), MaxCol() )
            SetMode( MaxRow() + 2, MaxCol() + 2 )
            hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
         ELSE
            SetMode( MaxRow(), MaxCol() )
            SetMode( MaxRow() + 2, MaxCol() + 2 )
         ENDIF
         DispScreen()
         hb_DispOutAt( MaxRow(), 2, "< Font changed to " + hb_gtInfo( HB_GTI_FONTNAME ) + " >", "B/G*" )
         nMSec := hb_MilliSeconds()

      CASE nKeyStd == HB_K_RESIZE
         DispScreen()
         hb_DispOutAt( MaxRow(), 33, "Resized      ", "B/G*" )
         nMSec := hb_MilliSeconds()

      CASE nKeyStd == HB_K_GOTFOCUS
         ChgPalette( .T. )
         hb_DispOutAt( MaxRow(), 33, "We got focus ", "B/G*" )
         nMSec := hb_MilliSeconds()

      CASE nKeyStd == HB_K_LOSTFOCUS
         ChgPalette( .F. )
         hb_DispOutAt( MaxRow(), 33, "We lost focus", "B/G*" )
         nMSec := hb_MilliSeconds()

      CASE nKeyStd == HB_K_CLOSE
         IF Alert( "Close Application", { "Yes", "No" } ) == 1
            RETURN
         ENDIF

      ENDCASE
   ENDDO

   RETURN

//

STATIC PROCEDURE DispScreen()

   LOCAL nRow := 9
   LOCAL cColor := "N/W"
   LOCAL nMaxCol := MaxCol() + 1

   DispBegin()

   SetColor( "N/W" )
   CLS
   hb_DispOutAt( 0, 0, hb_UPadC( "Harbour GT - New Features", nMaxCol ), "N/GR*" )

   // Contributed by Massimo Belgrano
   hb_DispOutAt( 2, 0, hb_UPadC( "______  __             ______________________                        ", nMaxCol ), "W+/W" )
   hb_DispOutAt( 3, 0, hb_UPadC( "___  / / /_____ ___________ /___________  _________    __  ____/____/", nMaxCol ), "W+/W" )
   hb_DispOutAt( 4, 0, hb_UPadC( "__  /_/ /_  __ `/_  ___/_  __ \  __ \  / / /_  ___/    _  / __ __/   ", nMaxCol ), "W+/W" )
   hb_DispOutAt( 5, 0, hb_UPadC( "_  __  / / /_/ /_  /   _  /_/ / /_/ / /_/ /_  /        / /_/ / _  /  ", nMaxCol ), "W+/W" )
   hb_DispOutAt( 6, 0, hb_UPadC( "/_/ /_/  \__,_/ /_/    /_.___/\____/\__,_/ /_/         \____/  /_/   ", nMaxCol ), "W+/W" )

   hb_DispOutAt( 8, 0, hb_UPadC( "MODE: " + hb_ntos( MaxRow() + 1 ) + " Rows and " + hb_ntos( nMaxCol ) + " Columns", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F2 MarkCopy    Toggle >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F3 Resizable   Toggle > " + iif( hb_gtInfo( HB_GTI_RESIZABLE ), "On ", "Off" ), nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F4 Closable    Toggle > " + iif( hb_gtInfo( HB_GTI_CLOSABLE ), "On ", "Off" ), nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F5 Palette L   Repeat >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F6 Palette D   Repeat >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F7 Palette By Index R >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F8 MarkCopy menu text >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "<    Click Other Window >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "<    Click X Button     >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F9 Resize Mode Toggle > " + iif( hb_gtInfo( HB_GTI_RESIZEMODE ) == HB_GTI_RESIZEMODE_ROWS, "ROWS", "FONT" ), nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F10 Open New Window   >    ", nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F11 Alt-Enter  Toggle > " + iif( hb_gtInfo( HB_GTI_ALTENTER ), "On ", "Off" ), nMaxCol ), cColor )
   hb_DispOutAt( ++nRow, 0, hb_UPadC( "< F12 Change Font Test  > " + hb_gtInfo( HB_GTI_FONTNAME ) + " " + hb_ntos( hb_gtInfo( HB_GTI_FONTWIDTH ) ) + "x" + hb_ntos( hb_gtInfo( HB_GTI_FONTSIZE ) ), nMaxCol ), cColor )

   hb_DispOutAt( MaxRow(), 0, Space( MaxCol() + 1 ), "N/G*" )

   hb_DispOutAt( 0, 0                  , "TL", "N/GR*" )
   hb_DispOutAt( 0, MaxCol() - 1       , "TR", "N/GR*" )
   hb_DispOutAt( MaxRow(), 0           , "BL", "N/G*"  )
   hb_DispOutAt( MaxRow(), MaxCol() - 1, "BR", "N/G*"  )

   DispEnd()

   RETURN

//

#if ! defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )

PROCEDURE hb_GTSYS()  /* must be a public function */

   REQUEST HB_GT_WVT_DEFAULT
   REQUEST HB_GT_WIN

   RETURN

#endif

//

STATIC PROCEDURE SetPalette( nMode )

   LOCAL aPalette := hb_gtInfo( HB_GTI_PALETTE )

   THREAD STATIC t_nR := 198
   THREAD STATIC t_nG := 198
   THREAD STATIC t_nB := 198

   t_nR += iif( nMode == 0, -5, 5 )
   t_nG += iif( nMode == 0, -5, 5 )
   t_nB += iif( nMode == 0, -5, 5 )

   // Change "W" to slightly gray everytime you press F5
   //
   aPalette[ 8 ] := RGB( t_nR, t_nG, t_nB )

   hb_gtInfo( HB_GTI_PALETTE, aPalette )
   DispScreen()

   RETURN

//

STATIC PROCEDURE SetPaletteIndex()

   hb_gtInfo( HB_GTI_PALETTE, 8, RGB( 120, 200, 240 ) )
   DispScreen()

   RETURN

//

STATIC PROCEDURE thFunc()

   STATIC s_nBrowser := 0
   STATIC s_nZx := 0
   STATIC s_nZy := 0

   LOCAL oBrowse, lEnd, nKey, i
   LOCAL aColor := { "W+/N", "W+/B", "W+/G", "W+/BG", "W+/N*", "W+/RB", "N/W*", "N/GR*" }

   s_nBrowser++
   s_nZx += 20
   s_nZy += 20

   /* allocate own GT driver */
   hb_gtReload( hb_gtVersion() )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_PALETTE, 8, RGB( 120, 200, 240 ) )

   IF s_nBrowser % 2 != 0
      hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   ENDIF
   hb_gtInfo( HB_GTI_FONTNAME , sc_aFont[ 1 ] )
   hb_gtInfo( HB_GTI_WINTITLE, "test.dbf    [" + iif( s_nBrowser % 2 != 0, "RESIZABLE_BY_ROWS", "RESIZABLE_BY_FONT" ) + "]" )
   hb_gtInfo( HB_GTI_ALTENTER, .T. )  // allow alt-enter for full screen

   SetCursor( SC_NONE )

   s_nColorIndex++
   IF s_nColorIndex > Len( aColor )
      s_nColorIndex := 1
   ENDIF

   s_nRows++
   s_nCols += 2
   SetMode( s_nRows, s_nCols )

   SetColor( aColor[ s_nColorIndex ] )

   hb_DispOutAt( 0, 0, hb_UPadC( "New Window with " + hb_ntos( MaxRow() ) + " Rows and " + hb_ntos( MaxCol() ) + " Columns", MaxCol() + 1 ), "N/GR*" )

   hb_gtInfo( HB_GTI_SETPOS_XY, s_nZx, s_nZy )  // this does not work until something is displayed

   USE test.dbf READONLY SHARED NEW

   oBrowse := TBrowse():New( 1, 0, MaxRow(), MaxCol() )

   oBrowse:HeadSep       := hb_UTF8ToStrBox( "┬─" )
   oBrowse:ColSep        := hb_UTF8ToStrBox( "│" )
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip ) }

   FOR EACH i IN dbStruct()
      oBrowse:AddColumn( TBColumnNew( i[ DBS_NAME ], FieldBlock( i[ DBS_NAME ] ) ) )
   NEXT

   oBrowse:configure()

   lEnd := .F.
   DO WHILE ! lEnd
      oBrowse:ForceStable()

      nKey := Inkey( 0, hb_bitOr( HB_INKEY_ALL, HB_INKEY_EXT ) )

      IF ! BrwHandleKey( oBrowse, nKey, @lEnd )

         SWITCH hb_keyStd( nKey )
         CASE HB_K_GOTFOCUS
            ChgPalette( .T. )
            EXIT

         CASE HB_K_LOSTFOCUS
            ChgPalette( .F. )
            EXIT

         CASE HB_K_RESIZE
            hb_DispOutAt( 0, 0, hb_UPadC( "New Window with " + hb_ntos( MaxRow() ) + " Rows and " + hb_ntos( MaxCol() ) + " Columns", MaxCol() + 1 ), "N/GR*" )

            oBrowse:nBottom := MaxRow()
            oBrowse:nRight := MaxCol()
            oBrowse:Configure()
            oBrowse:RefreshAll()
            EXIT

         ENDSWITCH
      ENDIF
   ENDDO

   dbCloseArea()

   RETURN

//

STATIC FUNCTION DbSkipBlock( n )

   LOCAL nSkipped := 0

   DO CASE
   CASE n == 0
      dbSkip( 0 )
   CASE n > 0
      DO WHILE nSkipped != n .AND. TBNext()
         nSkipped++
      ENDDO
   OTHERWISE
      DO WHILE nSkipped != n .AND. TBPrev()
         nSkipped--
      ENDDO
   ENDCASE

   RETURN nSkipped

//

STATIC FUNCTION TBNext()

   LOCAL nSaveRecNum := RecNo()

   IF Eof()
      RETURN .F.
   ELSE
      dbSkip()
      IF Eof()
         dbGoto( nSaveRecNum )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

//

STATIC FUNCTION TBPrev()

   LOCAL nSaveRecNum := RecNo()

   dbSkip( -1 )

   IF Bof()
      dbGoto( nSaveRecNum )
      RETURN .F.
   ENDIF

   RETURN .T.

//

STATIC FUNCTION BrwHandleKey( oBrowse, nKey, /* @ */ lEnd )

   nKey := hb_keyStd( nKey )

   DO CASE
   CASE nKey == K_ESC        ; lEnd := .T.
#if 0
   CASE nKey == K_ENTER      ; lEnd := .T.
#endif
   CASE nKey == K_DOWN       ; oBrowse:Down()
   CASE nKey == K_UP         ; oBrowse:Up()
   CASE nKey == K_LEFT       ; oBrowse:Left()
   CASE nKey == K_RIGHT      ; oBrowse:Right()
   CASE nKey == K_PGDN       ; oBrowse:pageDown()
   CASE nKey == K_PGUP       ; oBrowse:pageUp()
   CASE nKey == K_CTRL_PGUP  ; oBrowse:goTop()
   CASE nKey == K_CTRL_PGDN  ; oBrowse:goBottom()
   CASE nKey == K_HOME       ; oBrowse:home()
   CASE nKey == K_END        ; oBrowse:end()
   CASE nKey == K_CTRL_LEFT  ; oBrowse:panLeft()
   CASE nKey == K_CTRL_RIGHT ; oBrowse:panRight()
   CASE nKey == K_CTRL_HOME  ; oBrowse:panHome()
   CASE nKey == K_CTRL_END   ; oBrowse:panEnd()
   CASE nKey == K_MWBACKWARD ; oBrowse:down()
   CASE nKey == K_MWFORWARD  ; oBrowse:up()
   OTHERWISE                 ; RETURN .F.
   ENDCASE

   RETURN .T.

//

STATIC PROCEDURE ChgPalette( lFocus )

   THREAD STATIC t_aSavePalette

   LOCAL aPalette := hb_gtInfo( HB_GTI_PALETTE )
   LOCAL cSaveScreen := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL nR, nG, nB, nColor, nI, nDimFactor := 1.5

   IF t_aSavePalette == NIL
      t_aSavePalette := AClone( aPalette )
   ENDIF

   IF lFocus
      aPalette := t_aSavePalette
   ELSE
      FOR nI := 2 TO Len( aPalette )
         nColor := aPalette[ nI ]
         IF nColor >= 0x10000
            nB := Int( nColor / 0x10000 )
            nColor -= nB * 0x10000
            nB := Int( nB / nDimFactor )
         ELSE
            nB := 0
         ENDIF
         IF nColor >= 0x100
            nG := Int( nColor / 0x100 )
            nColor -= nG * 0x100
            nG := Int( nG / nDimFactor )
         ELSE
            nG := 0
         ENDIF
         IF nColor > 0
            nR := Int( nColor / nDimFactor )
         ELSE
            nR := 0
         ENDIF
         aPalette[ nI ] := RGB( nR, nG, nB )
      NEXT
   ENDIF

   hb_gtInfo( HB_GTI_PALETTE, aPalette )

   RestScreen( 0, 0, MaxRow(), MaxCol(), cSaveScreen )

   RETURN
