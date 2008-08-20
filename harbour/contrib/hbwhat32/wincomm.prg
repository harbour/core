/*
 * $Id$
 */

// WHAT32

// common controls and common dialogs

#include "winuser.ch"
#include "commctrl.ch"
#include "what32.ch"


*------------------------------------------------------------------------------

FUNCTION CreateStatusBar(nStyle, cText, hParent, nId  )
LOCAL hSBWnd
LOCAL nProc
   IF ( hSBWnd := CreateStatusWindow(nStyle, cText,hParent, nId )) != 0
      nProc:=SetProcedure(hParent, {|hWnd, nMsg, nwParam, nlParam| ;
             _SBMove( nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd ) }, WM_SIZE )
   ENDIF
RETURN(hSBWnd)

*------------------------------------------------------------------------------

// internal use

Static FUNCTION  _SBMove(  nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd )
   LOCAL aRect
   IF nMsg == WM_SIZE
      If IsWindow( hSBWnd )
         aRect := GetWindowRect( hSBWnd )
         MoveWindow( hSBWnd, 0, HiWord( nlParam ) - ( aRect[ 4 ] - aRect[ 2 ] ) , ;
                     LoWord( nlParam ) , aRect[ 4 ] - aRect[ 2 ] , .T. )

      Endif
   EndIf
   Return CallWindowProc( nProc, hWnd, nMsg, nwParam, nlParam )


*------------------------------------------------------------------------------

FUNCTION SetStatusBarParts( hSBWnd, aParts )
   LOCAL bSizes := ""
   AEVAL(aParts,{|x| bSizes+=L2BIN(x)})
   return SendMessage( hSBWnd, SB_SETPARTS, LEN( aParts ), bSizes )


*------------------------------------------------------------------------------

FUNCTION SetStatusBarText( hSBWnd, nPart, cText, nBorder )
   nBorder:=IFNIL(nBorder,0,nBorder)
   return SendMessage( hSBWnd, SB_SETTEXT + nBorder, nPart, cText )


*------------------------------------------------------------------------------

FUNCTION SetStatusBkColor( hSBWnd, nPart, nColor )
   return SendMessage( hSBWnd, SB_SETBKCOLOR, nPart, nColor )


*------------------------------------------------------------------------------

FUNCTION SetStatusIcon( hSBWnd, nPart, hIcon )
   return SendMessage( hSBWnd, SB_SETICON, nPart, hIcon )

*------------------------------------------------------------------------------

FUNCTION SetStatusToolTip( hSBWnd, nPart, cTTip )
   return SendMessage( hSBWnd, SB_SETTIPTEXT, nPart, cTTip )


