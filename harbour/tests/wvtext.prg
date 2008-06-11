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
   Local nKey, lMark, lResize
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

      ENDCASE
   ENDDO

   RETURN NIL
//----------------------------------------------------------------------//

STATIC FUNCTION MyNotifier( nEvent, ... )

   DO CASE

   CASE nEvent == HB_GTE_SETFOCUS
      DispScreen()
      Alert( "We got focus" )

   CASE nEvent == HB_GTE_CLOSE
      DispScreen()
      if Alert( 'Close Application', {'Yes','No' } ) == 1
         QUIT
      endif

   ENDCASE

   RETURN 0

//----------------------------------------------------------------------//

STATIC FUNCTION DispScreen()

   CLS
   DispOutAt( 0, 0,padc( 'Harbour GT - New Features', maxcol()+1 ), 'N/GR*' )
   DispOutAt( maxrow() - 1, 0, padc( '<F2 MarkCopy:Toggle> <F3 Resize:Toggle> <Click Other Window> <Click X Button>', maxcol()+1 ), 'N/G*' )
   DispOutAt( maxrow(), 0, Space( maxcol()+1 ), 'N/G*' )
   DispOutAt( 0, 0, "TL" )
   DispOutAt( 0, MaxCol() - 1, "TR" )
   DispOutAt( MaxRow(), 0, "BL" )
   DispOutAt( MaxRow(), MaxCol() - 1, "BR" )

   RETURN NIL

//----------------------------------------------------------------------//

PROCEDURE HB_GTSYS()
     REQUEST HB_GT_WVT
     REQUEST HB_GT_WIN
     RETURN

PROCEDURE HB_GT_WVT_DEFAULT()
     RETURN

//----------------------------------------------------------------------//
