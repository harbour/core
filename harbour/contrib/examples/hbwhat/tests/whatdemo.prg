/*
 * $Id$
 */

//----------------------------------------------------------------------//
//
//                               WhatDemo.prg
//
//                   Pritpal Bedi <pritpal@vouchcac.com>
//
//----------------------------------------------------------------------//

#include "inkey.ch"
#include "winuser.ch"

//----------------------------------------------------------------------//

Function Main()
   Local nKey

   SetColor( "N/W" )
   CLS

   DispOutAt( Maxrow(),0, padc( "F2 New Dialog    ESC Exit", maxcol()+1 ), "W+/R" )

   do while .t.
      nKey := inkey()

      if nKey == K_ESC
         Exit

      elseif nKey == K_F2
         TestDyn( VWN_GetActiveWindow() )

      endif
   enddo

   Return( NIL )

//----------------------------------------------------------------------//

Function HB_GtSys()

   REQUEST HB_GT_WVT_DEFAULT

   Return nil

//----------------------------------------------------------------------//

Function TestDyn( hWnd )
   Local aDlg

   aDlg := WHT_MakeDlgTemplate( "Sample Dynamic Dialog",            ;
                           WS_CAPTION + 4 + WS_SYSMENU              ;
                           + WS_GROUP + WS_TABSTOP + DS_SETFONT     ;
                           + WS_THICKFRAME + WS_VISIBLE             ;
                           + WS_POPUP, 24, 12, 180, 160 )

   aDlg := WHT_AddDlgItem( aDlg, 101, 128 , ; //"BUTTON",           ;
                           BS_AUTOCHECKBOX + WS_TABSTOP + WS_CHILD  ;
                           + WS_VISIBLE,                            ;
                           60, 90, 50, 12,                          ;
                           "&Check box" )

   aDlg := WHT_AddDlgItem( aDlg, 1, "BUTTON",                       ;
                           BS_DEFPUSHBUTTON + WS_TABSTOP            ;
                           + WS_CHILD + WS_VISIBLE,                 ;
                           106, 112, 44, 14,                        ;
                           "Ok" )

   aDlg := WHT_AddDlgItem( aDlg, 2, 128, ;//"BUTTON",               ;
                           BS_PUSHBUTTON + WS_TABSTOP               ;
                               + WS_CHILD + WS_VISIBLE,             ;
                           36, 112, 44, 14,                         ;
                           "&Cancel" )

   #ifdef __MODELESS__
   WHT_CreateDialog( , aDlg, hWnd, { | h, m, w, l | MyDlgProc2( h, m, w, l ) } )
   #else
   WHT_DialogBox( , aDlg, hWnd, { | h, m, w, l | MyDlgProc2( h, m, w, l ) } )
   #endif

   Return( NIL )

//----------------------------------------------------------------------//

Function MyDlgProc2( hDlg, nMsg, nwParam, nlParam )

   Do Case
   Case nMsg == WM_INITDIALOG
      VWN_ShowWindow( hDlg, SW_SHOW )
      Return( 1 )

   Case nMsg == WM_SYSCOMMAND
      If nwParam == SC_CLOSE
         #ifdef __MODELESS__
         VWN_DestroyWindow( hDlg )
         #else
         VWN_EndDialog( hDlg, IDOK )
         #endif

         Return( 1 )
      EndIf

   Case nMsg == WM_COMMAND
      If nwParam == IDOK
         #ifdef __MODELESS__
         VWN_DestroyWindow( hDlg )
         #else
         VWN_EndDialog( hDlg, IDOK )
         #endif

         Return( 1 )
      EndIf

   EndCase

   Return( 0 )

//----------------------------------------------------------------------//
