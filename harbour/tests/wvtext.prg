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

#include 'hbgtinfo.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//

FUNCTION Main()
   Local nKey, lMark, lResize, lClose
   Local nHeight := 20
   Local nWidth  := Int( nHeight/2 )
   Local cFont

   //cFont := 'Courier New' // Harbour default
   //cFont := 'Times New Roman'
   //cFont := 'Lucida Console'

   Hb_GtInfo( HB_GTI_FONTNAME , cFont   )
   Hb_GtInfo( HB_GTI_FONTWIDTH, nWidth  )
   Hb_GtInfo( HB_GTI_FONTSIZE , nHeight )

   SetCursor( 0 )
   SetColor( 'n/w' )

   HB_GtInfo( HB_GTI_NOTIFIERBLOCK, {|nEvent, ...| MyNotifier( nEvent, ... ) } )

   DispScreen()

   DO WHILE .T.
      nKey := Inkey()
      if nKey == K_ESC
         exit
      endif

      DO CASE
      CASE nKey == K_ENTER
         Alert( '<Enter> Pressed' )

      CASE nKey == K_F2
         lMark := Hb_GtInfo( HB_GTI_SELECTCOPY )
         Hb_GtInfo( HB_GTI_SELECTCOPY, !lMark )

      CASE nKey == K_F3
         lResize := Hb_GtInfo( HB_GTI_RESIZABLE )
         Hb_GtInfo( HB_GTI_RESIZABLE, !lResize )

      CASE nKey == K_F4
         lClose := Hb_GtInfo( HB_GTI_CLOSABLE )
         hb_GtInfo( HB_GTI_CLOSABLE, !lClose )

      ENDCASE
   ENDDO

   RETURN NIL
//----------------------------------------------------------------------//

STATIC FUNCTION MyNotifier( nEvent, ... )

   DO CASE

   CASE nEvent == HB_GTE_SETFOCUS
      DispScreen()
      DispOutAt( maxrow(), 33, "We got focus", 'B/G*' )

   CASE nEvent == HB_GTE_CLOSE
      DispScreen()
      if Alert( 'Close Application', {'Yes','No' } ) == 2
         Return ( 1 )
      endif

   ENDCASE

   RETURN 0

//----------------------------------------------------------------------//

STATIC FUNCTION DispScreen()
   Local nRow := 18, nCol := 28
   Local cColor := 'N/W*'
   Local nMaxCol := MaxCol()+1

   CLS
   DispOutAt( 0, 0,padc( 'Harbour GT - New Features', maxcol()+1 ), 'N/GR*' )

   // Contributed by Massimo Belgrano
   DispOutAt( 2, 0, padc( "______  __             ______________________                        ",nMaxCol ), 'W+/W' )
   DispOutAt( 3, 0, padc( "___  / / /_____ __________  /___________  _________    __  ____/____/",nMaxCol ), 'W+/W' )
   DispOutAt( 4, 0, padc( "__  /_/ /_  __ `/_  ___/_  __ \  __ \  / / /_  ___/    _  / __ __/   ",nMaxCol ), 'W+/W' )
   DispOutAt( 5, 0, padc( "_  __  / / /_/ /_  /   _  /_/ / /_/ / /_/ /_  /        / /_/ / _  /  ",nMaxCol ), 'W+/W' )
   DispOutAt( 6, 0, padc( "/_/ /_/  \__,_/ /_/    /_.___/\____/\__,_/ /_/         \____/  /_/   ",nMaxCol ), 'W+/W' )

   DispOutAt( nRow+0, nCol, '< F2 MarkCopy    Toggle >', cColor )
   DispOutAt( nRow+1, nCol, '< F3 Resize      Toggle >', cColor )
   DispOutAt( nRow+2, nCol, '< F4 Closable    Toggle >', cColor )
   DispOutAt( nRow+3, nCol, '<    Click Other Window >', cColor )
   DispOutAt( nRow+4, nCol, '<    Click X Button     >', cColor )

   DispOutAt( maxrow(), 0, Space( maxcol()+1 ), "N/G*" )

   DispOutAt( 0, 0                  , "TL", "N/GR*" )
   DispOutAt( 0, MaxCol() - 1       , "TR", "N/GR*" )
   DispOutAt( MaxRow(), 0           , "BL", "N/G*" )
   DispOutAt( MaxRow(), MaxCol() - 1, "BR", "N/G*" )

   RETURN NIL

//----------------------------------------------------------------------//

PROCEDURE HB_GTSYS()
     REQUEST HB_GT_WVT
     REQUEST HB_GT_WIN
     RETURN

//----------------------------------------------------------------------//

PROCEDURE HB_GT_WVT_DEFAULT()
     RETURN

//----------------------------------------------------------------------//
   ?"______  __             ______________________                        "
   ?"___  / / /_____ __________  /___________  _________    __  ____/____/"
   ?"__  /_/ /_  __ `/_  ___/_  __ \  __ \  / / /_  ___/    _  / __ __/   "
   ?"_  __  / / /_/ /_  /   _  /_/ / /_/ / /_/ /_  /        / /_/ / _  /  "
   ?"/_/ /_/  \__,_/ /_/    /_.___/\____/\__,_/ /_/         \____/  /_/   "
