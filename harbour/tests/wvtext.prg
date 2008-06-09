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

#include 'HbGtInfo.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//

FUNCTION Main()
   Local nKey, lMark, lResize
   Local nHeight := 20
   Local nWidth  := Int( nHeight/2 )
   Local cFont

   //cFont := 'Courier New'
   //cFont := 'Times New Roman'
   cFont := 'Lucida Console'

   Hb_GtInfo( HB_GTI_FONTNAME , cFont   )
   Hb_GtInfo( HB_GTI_FONTWIDTH, nWidth  )
   Hb_GtInfo( HB_GTI_FONTSIZE , nHeight )

   SetMode( 25,80 )
   SetCursor( 0 )
   SetColor( 'n/w' )

   // Any CALLBACK function receive 5 parameters PLUS any additional parameters
   // supplied with the CALLBACK Block.
   //
   HB_GtInfo( HB_GTI_CALLBACK, { HB_GTE_SETFOCUS, {|a,b,c,d,e| MyCallBacks( a,b,c,d,e,'MyParam' ) }, { 'MyCargo' } } )
   HB_GtInfo( HB_GTI_CALLBACK, { HB_GTE_CLOSE   , {|a,b,c,d,e| MyCallBacks( a,b,c,d,e ) } } )

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
         lMark := Hb_GtInfo( HB_GTI_MARKCOPY )
         Hb_GtInfo( HB_GTI_MARKCOPY, !lMark )

      CASE nKey == K_F3
         lResize := Hb_GtInfo( HB_GTI_RESIZEABLE )
         Hb_GtInfo( HB_GTI_RESIZEABLE, !lResize )

      ENDCASE
   ENDDO

   RETURN NIL
//----------------------------------------------------------------------//

STATIC FUNCTION MyCallBacks( nEvent, iGT, xCargo, wParam, lParam, xSentByMe )

   DO CASE

   CASE nEvent == HB_GTE_SETFOCUS
      DispOutAt( 5,10, xCargo[ 1 ], 'N/W' )  // We have sent { 'MyCargo' }
      DispOutAt( 6,10, xSentByMe  , 'R/W' )  // We are sending 'MyParam'

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
   DispOutAt( 0,0,padc( '[x]Harbour GT - New Features', maxcol()+1 ), 'N/GR*' )
   DispOutAt( maxrow(),0,padc( '<F2 MarkCopy> <F3 Resize> <Click Other Window> <Click X Button>', maxcol()+1 ), 'N/G*' )

   RETURN NIL

//----------------------------------------------------------------------//
// Comment out this function if you do not want "Mark and Copy" prompt
// available in SysMenu at the left of Title Bar of the application window.
//
//FUNCTION Hb_NoCopyConsole() ; RETURN NIL

//----------------------------------------------------------------------//

FUNCTION HB_GTSYS()

   REQUEST HB_GT_WVT_DEFAULT

   RETURN nil

//----------------------------------------------------------------------//
