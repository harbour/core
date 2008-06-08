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
// Comment out this function if you do not want console window be resizable with mouse
//
//FUNCTION Hb_NoResizeableWindow() ; RETURN NIL

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
/*
Private Sub SetWindowStyle(ByVal hwnd As Long, ByVal _
    extended_style As Boolean, ByVal style_value As Long, _
    ByVal new_value As Boolean, ByVal brefresh As Boolean)
Dim style_type As Long
Dim style As Long

    If extended_style Then
        style_type = GWL_EXSTYLE
    Else
        style_type = GWL_STYLE
    End If

    ' Get the current style.
    style = GetWindowLong(hwnd, style_type)

    ' Add or remove the indicated value.
    If new_value Then
        style = style Or style_value
    Else
        style = style And Not style_value
    End If

    ' Hide Window if Changing ShowInTaskBar
    If brefresh Then
        ShowWindow hwnd, SW_HIDE
    End If

    ' Set the style.
    SetWindowLong hwnd, style_type, style

    ' Show Window if Changing ShowInTaskBar
    If brefresh Then
        ShowWindow hwnd, SW_SHOW
    End If

    ' Make the window redraw.
    SetWindowPos hwnd, 0, 0, 0, 0, 0, _
        SWP_FRAMECHANGED Or _
        SWP_NOMOVE Or _
        SWP_NOSIZE Or _
        SWP_NOZORDER
End Sub
*/
//----------------------------------------------------------------------//
