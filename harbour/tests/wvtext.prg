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

//----------------------------------------------------------------------//

FUNCTION Main()
   Local nKey

   SetMode( 25,80 )
   SetCursor( 0 )
   SetColor( 'n/w' )


   // Any CALLBACK function receive 5 parameters PLUS any additional parameters
   // supplied with the CALLBACK Block.
   //
   HB_GtInfo( GTI_CALLBACK, { GTI_CB_SETFOCUS, {|a,b,c,d,e| MyCallBacks( a,b,c,d,e,'MyParam' ) }, { 'MyCargo' } } )
   HB_GtInfo( GTI_CALLBACK, { GTI_CB_CLOSE   , {|a,b,c,d,e| MyCallBacks( a,b,c,d,e ) } } )

   DispScreen()

   DO WHILE .T.
      nKey := Inkey()
      if nKey == 27
         exit
      endif

      DO CASE
      CASE nKey == 13
         Alert( '<Enter> Pressed' )

      ENDCASE
   ENDDO

   RETURN NIL
//----------------------------------------------------------------------//

STATIC FUNCTION MyCallBacks( nEvent, iGT, xCargo, wParam, lParam, xSentByMe )

   DO CASE

   CASE nEvent == GTI_CB_SETFOCUS
      DispOutAt( 5,10, xCargo[ 1 ], 'N/W' )  // We have sent { 'MyCargo' }
      DispOutAt( 6,10, xSentByMe  , 'R/W' )  // We are sending 'MyParam'

   CASE nEvent == GTI_CB_CLOSE
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
   DispOutAt( maxrow(),0,padc( '<Click Other Window>   <Click X Button>   <ESC Quit>', maxcol()+1 ), 'N/G*' )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION HB_GTSYS()

   REQUEST HB_GT_WVT_DEFAULT

   RETURN nil

//----------------------------------------------------------------------//

