/*
 * $Id$
 */

//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                  [x]Harbour Extended Features Deno
//                                    .
//                 Pritpal Bedi <pritpal@vouchcac.com>
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include "hbgtinfo.ch"
#include "inkey.ch"
#include "box.ch"

#define RGB(r,g,b) ( r + ( g * 256 ) + ( b * 256 * 256 ) )

//----------------------------------------------------------------------//

// To be in hbgtinfo.ch
//
#define HB_GTI_SETPARENT         101
#define HB_GTI_WINHANDLE         102

REQUEST HB_ZipOpen

//----------------------------------------------------------------------//

STATIC nRows := 20, nCols := 60, nColorIndex := 1

//----------------------------------------------------------------------//

FUNCTION Main()
   Local nKey, lMark, lResize, lClose
   LOCAL cVar    := '               '
   LOCAL cVar1   := '               '
   LOCAL getlist := {}
   Local nHeight := 20
   Local nWidth  := Int( nHeight/2 )
   Local cFont   := 'Courier New'
   Local nn      := 0
   Local cc      := 'abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmn'

   SET SCOREBOARD OFF

   #if 0
   Hb_GtInfo( HB_GTI_FONTNAME , cFont   )
   Hb_GtInfo( HB_GTI_FONTWIDTH, nWidth  )
   Hb_GtInfo( HB_GTI_FONTSIZE , nHeight )
   #endif

   Hb_GtInfo( HB_GTI_CODEPAGE, 255 )
   SetCursor( 0 )
   SetColor( "n/w" )

   DispScreen()

   Hb_GtInfo( HB_GTI_ICONFILE, 'test.ico' )

   DO WHILE .T.
      nKey := Inkey(0.1, INKEY_ALL )
      if nKey == K_ESC
         exit
      ENDIF
#if 0
IF nKey != 0
   hb_ToOutDebug( "nKey %i %s", nKey, chr( nKey ) )
ENDIF
#endif
      IF nn < 80
         @ maxrow()-1, nn SAY substr( cc, nn+1, 1 )
         nn++
      endif

      DO CASE
      CASE nKey == K_ENTER
         Alert( "<Enter> Pressed" )

      CASE nKey == K_F2
         lMark := Hb_GtInfo( HB_GTI_SELECTCOPY )
         Hb_GtInfo( HB_GTI_SELECTCOPY, !lMark )

      CASE nKey == K_F3
         lResize := Hb_GtInfo( HB_GTI_RESIZABLE )
         Hb_GtInfo( HB_GTI_RESIZABLE, !lResize )

      CASE nKey == K_F4
         lClose := Hb_GtInfo( HB_GTI_CLOSABLE )
         hb_GtInfo( HB_GTI_CLOSABLE, !lClose )

      CASE nKey == K_F5
         SetPalette( 1 )

      CASE nKey == K_F6
         SetPalette( 0 )

      CASE nKey == K_F7
         SetPaletteIndex()

      CASE nKey == K_F8
         #include 'setcurs.ch'
         SetCursor( 2 )
         SetKey( K_F12, {||  SetCursor( 2 ) } )
         @ maxrow()-6, 5 GET cVar  COLOR 'W+/B'
         @ maxrow()-5, 5 GET cVar1 COLOR 'W+/R'
         READ
         @ maxrow()-3, 5 SAY cVar COLOR 'W+/W'

      ENDCASE
   ENDDO

   RETURN NIL
//----------------------------------------------------------------------//
STATIC FUNCTION MyNotifier( nEvent, ... )

   DO CASE

   CASE nEvent == HB_GTE_SETFOCUS
      DispScreen()
      DispOutAt( maxrow(), 35, "In  Focus", "B/G*" )

   CASE nEvent == HB_GTE_KILLFOCUS
      DispScreen()
      DispOutAt( maxrow(), 35, "Out Focus", "B/G*" )

   CASE nEvent == HB_GTE_CLOSE
      DispScreen()
      if Alert( "Close Application", {"Yes","No" } ) == 2
         Return ( 1 )
      endif

   ENDCASE

   RETURN 0
//----------------------------------------------------------------------//
STATIC FUNCTION DispScreen()
   Local nRow := 12, nCol := 28
   Local cColor := "N/W"
   Local nMaxCol := MaxCol()+1

   DispBegin()

   CLS
   DispOutAt( 0, 0,padc( "Harbour GTQTC - QT based Terminal Driver", maxcol()+1 ), "N/GR*" )

   // Contributed by Massimo Belgrano
   DispOutAt( 2, 0, padc( "______  __             ______________________                        ",nMaxCol ), "W+/W" )
   DispOutAt( 3, 0, padc( "___  / / /_____ ___________ /___________  _________    __  ____/____/",nMaxCol ), "W+/W" )
   DispOutAt( 4, 0, padc( "__  /_/ /_  __ `/_  ___/_  __ \  __ \  / / /_  ___/    _  / __ __/   ",nMaxCol ), "W+/W" )
   DispOutAt( 5, 0, padc( "_  __  / / /_/ /_  /   _  /_/ / /_/ / /_/ /_  /        / /_/ / _  /  ",nMaxCol ), "W+/W" )
   DispOutAt( 6, 0, padc( "/_/ /_/  \__,_/ /_/    /_.___/\____/\__,_/ /_/         \____/  /_/   ",nMaxCol ), "W+/W" )

   #if 1
   DispOutAt( ++nRow, nCol, "< F2 MarkCopy    Toggle >", cColor )
   DispOutAt( ++nRow, nCol, "< F3 Resize      Toggle >", cColor )
   DispOutAt( ++nRow, nCol, "< F4 Closable    Toggle >", cColor )
   DispOutAt( ++nRow, nCol, "< F5 Palette L   Repeat >", cColor )
   DispOutAt( ++nRow, nCol, "< F6 Palette D   Repeat >", cColor )
   DispOutAt( ++nRow, nCol, "< F7 Palette By Index R >", cColor )
   //DispOutAt( ++nRow, nCol, "< F8 MarkCopy menu text >", cColor )
   DispOutAt( ++nRow, nCol, "<        F8  GET        >", cColor )
   DispOutAt( ++nRow, nCol, "<    Enter - Alert()    >", cColor )
   DispOutAt( ++nRow, nCol, "<    Click X Button     >", cColor )
   #endif

   DispOutAt( maxrow(), 0, Space( maxcol()+1 ), "N/G*" )

   DispOutAt( 0, 0                  , "TL", "N/GR*" )
   DispOutAt( 0, MaxCol() - 1       , "TR", "N/GR*" )
   DispOutAt( MaxRow(), 0           , "BL", "N/G*"  )
   DispOutAt( MaxRow(), MaxCol() - 1, "BR", "N/G*"  )

   DispEnd()
   RETURN NIL
//----------------------------------------------------------------------//
PROCEDURE HB_GTSYS()
   REQUEST HB_GT_QTC_DEFAULT
   RETURN
//----------------------------------------------------------------------//
FUNCTION SetPalette( nMode )
   Local aPalette := Hb_GtInfo( HB_GTI_PALETTE )

   static nR := 198
   static nG := 198
   static nB := 198

   nR += iif( nMode == 0, -5, 5 )
   nG += iif( nMode == 0, -5, 5 )
   nB += iif( nMode == 0, -5, 5 )

   // Change "W" to slightly gray everytime you press F5
   //
   aPalette[ 8 ] := RGB( nR, nG, nB )

   Hb_GtInfo( HB_GTI_PALETTE, aPalette )
   DispScreen()

   RETURN NIL
//----------------------------------------------------------------------//
FUNCTION SetPaletteIndex()

   Hb_GtInfo( HB_GTI_PALETTE, 8, RGB( 120, 200, 240 ) )
   DispScreen()

   RETURN NIL
//----------------------------------------------------------------------//
PROCEDURE thFunc()
   Local cTitle, oBrowse, lEnd, nKey, i, aStruct, pGT1, pGT, hWnd
   Local aColor := { 'W+/N', 'W+/B', 'W+/G', 'W+/BG', 'W+/N*', 'W+/RB', 'N/W*', 'N/GR*' }

   static nBrowser := 0
   static nZx := 0
   static nZy := 0

   ErrorBlock( {|oErr| MyErrorSys( oErr ) } )

   nBrowser++
   nZx += 20
   nZy += 20

   /* allocate own GT driver */
   hb_gtReload( 'WVT' )
   Hb_GtInfo( HB_GTI_PALETTE, 8, RGB( 120, 200, 240 ) )

   if ( nBrowser % 2 ) != 0
      Hb_GtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   endif
   Hb_GtInfo( HB_GTI_WINTITLE, 'Test.dbf    ['+if( ( nBrowser % 2 ) != 0, 'RESIZABLE_BY_ROWS', 'RESIZABLE_BY_FONT' )+']' )

   SetCursor( 0 )

   nColorIndex++
   if nColorIndex > len( aColor )
      nColorIndex := 1
   endif

   nRows++
   nCols += 2

   SetMode( nRows,nCols )
   SetColor( aColor[ nColorIndex ] )
   Hb_GtInfo( HB_GTI_WINTITLE, cTitle )
   CLS
   Hb_GtInfo( HB_GTI_SETPOS_XY, nZx, nZy )

   cTitle := 'New Window with '+ltrim( str( MaxRow() ) )+;
                          ' Rows and '+ltrim( str( MaxCol() ) )+' Columns'
   DispOutAt( 0, 0, padc( cTitle, maxcol()+1 ), 'N/GR*' )
   cTitle := '<F11> Modal Dialog  <ReSize> <Maximize> <ESC>'
   DispOutAt( MaxRow(), 0, padc( cTitle, maxcol()+1 ), 'W+/R*' )

   use test shared
   aStruct := DbStruct()

   oBrowse := TBrowse():New( 1, 0, maxrow()-1, maxcol() )

   oBrowse:ColSep        := " ³ "
   oBrowse:HeadSep       := "ÄÂÄ"
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( aStruct )
      oBrowse:AddColumn( TBColumnNew( aStruct[ i,1 ], BlockField( i ) ) )
   next

   oBrowse:configure()

   lEnd := .f.
   While !lEnd
      oBrowse:ForceStable()

      nKey := InKey( 0, INKEY_ALL )

      if BrwHandleKey( oBrowse, nKey, @lEnd )
         //
      else
         if nKey == HB_K_RESIZE
            cTitle := 'New Window with '+ltrim( str( MaxRow() ) )+;
                          ' Rows and '+ltrim( str( MaxCol() ) )+' Columns'
            DispOutAt( 0, 0, padc( cTitle, maxcol()+1 ), 'N/GR*' )
            cTitle := '<F11> Modal Dialog  <ReSize> <Maximize> <ESC>'
            DispOutAt( MaxRow(), 0, padc( cTitle, maxcol()+1 ), 'W+/R*' )

            oBrowse:nBottom := MaxRow()-1
            oBrowse:nRight  := MaxCol()
            oBrowse:Configure()
            oBrowse:RefreshAll()

         elseif nKey == K_F11
            // Initialization Phase . Can be a Dialog Class
            // LIKE in Xbase++:
            // oWnd := gtDialog():New( oParent, oOwner, aSize, aPos, aPresParam, lVisible )
            //
            hWnd := hb_gtInfo( HB_GTI_WINHANDLE )

            pGT1 := hb_gtCreate( 'WVT' )
            pGT  := hb_gtSelect( pGT1 )
            SetMode( 7,40 )
            hb_gtInfo( HB_GTI_SETPARENT     , hWnd )
            hb_gtInfo( HB_GTI_SETPOS_ROWCOL , 4, 8 )
            hb_gtInfo( HB_GTI_WINTITLE      , 'Modal Dialog [ Row:4 Col:8 ]' )
            hb_gtInfo( HB_GTI_RESIZABLE     , .F.  )

            // Program Flow
            SetColor( 'N/W' )
            CLS
            Alert( 'I am in Child Window', {'Good','Bad'} )

            // Cleanup
            pGT1 := NIL
            hb_gtSelect( pGT )

         elseif nKey == K_F12
            pGT1 := NIL

         endif
      endif
   end

   DbCloseArea()

   RETURN
//----------------------------------------------------------------------//
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
STATIC FUNCTION BlockField( i )
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
PROCEDURE MyErrorSys( oError )

   ? oError:Description
   ? oError:args
   ? oError:genCode
   ? oError:operation

   ? procname(1), procline(1)
   ? procname(2), procline(2)
   ? procname(3), procline(3)

   RETURN
//----------------------------------------------------------------------//
