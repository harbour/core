/*
 * $Id$
 */

//----------------------------------------------------------------------//

// hbwhat
// common controls and common dialogs

#include "winuser.ch"
#include "commctrl.ch"
#include "hbwhat.ch"

//----------------------------------------------------------------------//
FUNCTION WHT_CreateStatusBar( nStyle, cText, hParent, nId  )
   LOCAL hSBWnd
   LOCAL nProc

   IF ( hSBWnd := VWN_CreateStatusWindow( nStyle, cText,hParent, nId )) != 0
      nProc := VWN_SetProcedure( hParent, {|hWnd, nMsg, nwParam, nlParam| ;
             _SBMove( nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd ) }, WM_SIZE )
   ENDIF

   RETURN(hSBWnd)
//----------------------------------------------------------------------//
// internal use
STATIC FUNCTION  _SBMove( nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd )
   LOCAL aRect

   IF nMsg == WM_SIZE
      If VWN_IsWindow( hSBWnd )
         aRect := VWN_GetWindowRect( hSBWnd )
         VWN_MoveWindow( hSBWnd, 0, VWN_HiWord( nlParam ) - ( aRect[ 4 ] - aRect[ 2 ] ) , ;
                         VWN_LoWord( nlParam ) , aRect[ 4 ] - aRect[ 2 ] , .T. )

      Endif
   EndIf

   RETURN VWN_CallWindowProc( nProc, hWnd, nMsg, nwParam, nlParam )
//----------------------------------------------------------------------//
FUNCTION WHT_SetStatusBarParts( hSBWnd, aParts )
   LOCAL bSizes := ""

   AEVAL(aParts,{|x| bSizes+=L2BIN(x)})

   RETURN VWN_SendMessage( hSBWnd, SB_SETPARTS, LEN( aParts ), bSizes )
//----------------------------------------------------------------------//
FUNCTION WHT_SetStatusBarText( hSBWnd, nPart, cText, nBorder )

   nBorder := IFNIL( nBorder,0,nBorder )

   RETURN VWN_SendMessage( hSBWnd, SB_SETTEXT + nBorder, nPart, cText )
//----------------------------------------------------------------------//
FUNCTION WHT_SetStatusBkColor( hSBWnd, nPart, nColor )

   RETURN VWN_SendMessage( hSBWnd, SB_SETBKCOLOR, nPart, nColor )
//----------------------------------------------------------------------//
FUNCTION WHT_SetStatusIcon( hSBWnd, nPart, hIcon )

   RETURN VWN_SendMessage( hSBWnd, SB_SETICON, nPart, hIcon )
//----------------------------------------------------------------------//
FUNCTION WHT_SetStatusToolTip( hSBWnd, nPart, cTTip )

   RETURN VWN_SendMessage( hSBWnd, SB_SETTIPTEXT, nPart, cTTip )
//----------------------------------------------------------------------//
