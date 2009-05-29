/*
 * $Id$
 */

//----------------------------------------------------------------------//
/*
                       W A R N I N G   !!!

    *******************************************************
      Do not alter this code, unless you understand fully
    how it works, and, if you really know what you're doing
    *******************************************************

*/
//----------------------------------------------------------------------//

#xCommand ? ? <x> => VWN_OutputDebugString( asString( <x> ) )
#xCommand ? <x>   => VWN_OutputDebugString( asString( <x> ) + chr( 13 ) )

#Define WT_DIALOG     0      // used internally (user custom dialog class - advanced option)
#Define WT_WINDOW     1      // use DefWindowProc()
#Define WT_MDIFRAME   2      // use DefFrameProc()
#Define WT_MDICHILD   4      // use DefMDIChildProc()

#include "common.ch"
#include "hbwhat.ch"
#include "winuser.ch"
#include "hboo.ch"

//PUBLIC lPrevInstance
//PUBLIC hThisInstance

THREAD STATIC t_hWndActive := 0

STATIC s_aClass:={}  // cClass,nType,{{anWM,bAction,nProc,0}}
STATIC s_aWindow:={} // hWnd, nType, {{anWM,bAction,nProc,nOldProc}}
STATIC s_aDialog:={} // hDlg, {{anWM.bAction}} // maybe rather add them to s_aWindow as type 0 - dialog ?
STATIC s_aProc       // array of possible windows procedures (10)
                     // for subclassing ???

//----------------------------------------------------------------------//
/*
INIT PROCEDURE _CheckMultiInst

   lPrevInstance:=(empty(CreateMutex( , .T., strtran(GetModuleFileName(),"\","_") )) ;
                   .or. (GetLastError() > 0) )

   hThisInstance:=_getinstance()

   RETURN
*/
//----------------------------------------------------------------------//

FUNCTION WhatVersion(dDate)

 dDate := hb_stod( "20020821" )

 RETURN "0.g"

//----------------------------------------------------------------------//
//
//SYNTAX: RegisterClass( WNDCLASS, [<nType>], [<bAction>], [< anWM >] )
// where ntype: is one of RFC_* values
//     bAction: event procedure codeblock
//        anWM: selective messages to send to codeblock ( -1 == All )

Function WHT_RegisterClass( wndclass, nType, bAction, anWM, oObj, xCargo )
   Local aAction

   // wndclass:cbSize        := LEN( wndclass:value )
   wndclass:lpfnWndProc   := 0
   wndclass:style         := iif( wndclass:style==NIL,(CS_HREDRAW + CS_VREDRAW + CS_OWNDC + CS_DBLCLKS), wndclass:style )
   wndclass:cbClsExtra    := iif( wndclass:cbClsExtra==NIL, 0, wndclass:cbClsExtra )
   wndclass:cbWndExtra    := iif( wndclass:cbWndExtra==NIL, 0, wndclass:cbWndExtra )
   wndclass:hInstance     := iif( wndclass:hInstance==NIL, VWN_GetModuleHandle(), wndclass:hInstance )
   wndclass:hIcon         := iif( wndclass:hIcon==NIL, VWN_LoadIcon( VWN_GetModuleHandle(),""), wndclass:hIcon )
   wndclass:hCursor       := iif( wndclass:hCursor==NIL, VWN_LoadCursor(, IDC_ARROW), wndclass:hCursor)
   wndclass:hbrBackground := iif( wndclass:hbrBackground==NIL, COLOR_WINDOW  + 1, wndclass:hbrBackground )
   wndclass:lpszMenuName  := iif( wndclass:lpszMenuName==NIL, "", wndclass:lpszMenuName ) ;

   IF !ISCHARACTER( wndclass:lpszClassName ) .OR. EMPTY( wndclass:lpszClassName ) .OR. ;
      ! WHT__RegisterClass( wndclass:value )
      Return( .F. )
   ENDIF

   // note : WHT__RegisterClass() function MUST add our
   //        default "C" window procedure address
   //        which will call our  __ProcessMessage() below

   If Empty( anWM )
      anWM := { 0 }
   ElseIf ValType( anWM ) == "N"
      anWM := { anWM }
   EndIf

   If !(ValType( bAction ) $ "BN")
      bAction := NIL
   EndIf

   aAction := { anWM, bAction, WHT_GetWndProc( 1 ) , 0, oObj, xCargo }
   aAdd( s_aClass, { WNDCLASS:lpszClassName, nType, aAction } )

   Return( .T. )

//----------------------------------------------------------------------//

Function WHT_UnregisterClass( cClass, hInst )
   Local n

   If ! WHT__UnregisterClass( cClass, hInst )
      Return( .F. )
   EndIf

   If ( n := aScan( s_aClass, { | x | x[ 1 ] == cClass } ) ) > 0
      aDel( s_aClass, n )
      aSize( s_aClass, Len( s_aClass ) - 1 )
   EndIf

   Return( .T. )

//----------------------------------------------------------------------//

Function WHT_CreateWindowEx( nExStyle, cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                             hWndParent, hMenu, hInst , cParam )

   Return WHT_CreateWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                                 hWndParent, hMenu, hInst , cParam, nExStyle )

//----------------------------------------------------------------------//
//
// Uses CreateWindowEx  !!!!!!!!
// Note extra params
//
Function WHT_CreateWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                           hWndParent, hMenu, hInst, cParam, nExStyle )
   Local hWnd
   Local n
   Local nIndex

   // prepare a slot in s_aWindow array

   If ( nIndex := aScan( s_aWindow, { | x | x[ 1 ] == NIL } ) ) == 0  // waiting
      aAdd( s_aWindow, )
      nIndex := Len( s_aWindow )
   EndIf

   // add default class procedure address and block

   If ( n := aScan( s_aClass, { | x | cClass == x[ 1 ] } ) ) > 0 // own window class
      s_aWindow[ nIndex ] := { 0, s_aClass[ n, 2 ] , {} }
      If ! Empty( s_aClass[ n, 3 ] )                     // if default user codeblock exists
         aAdd( s_aWindow[ nIndex, 3 ] , s_aClass[ n, 3 ] )
      EndIf
   Else
      s_aWindow[ nIndex ] := { 0, WT_WINDOW, {} }         // no default codeblock
   EndIf

   // create a window
   If ( hWnd := WHT__CreateWindowEx( nExStyle, cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                                       hWndParent, hMenu, hInst , cParam ) ) > 0
      If s_aWindow[ nIndex, 1 ] == 0
         s_aWindow[ nIndex, 1 ] := hWnd
      EndIf
   Else
      s_aWindow[ nIndex ] := { NIL, NIL, { } }
      __KillWindow( )
   EndIf

   Return( hWnd )

//----------------------------------------------------------------------//
//
// Creates MDI child window
//
Function WHT_CreateMDIWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                       hWndParent, hInst , lParam )
   Local hWnd
   Local n
   Local nIndex

   // prepare a slot in s_aWindow array
   If ( nIndex := aScan( s_aWindow, { | x | x[ 1 ] == NIL } ) ) == 0  // waiting
      aAdd( s_aWindow, )
      nIndex := Len( s_aWindow )
   EndIf

   // add default class procedure address and block
   If ( n := aScan( s_aClass, { | x | cClass == x[ 1 ] } ) ) > 0 // own window class
      s_aWindow[ nIndex ] := { 0, s_aClass[ n, 2 ] , { } }
      If ! Empty( s_aClass[ n, 3 ] )                     // if default user codeblock exists
         aAdd( s_aWindow[ nIndex, 3 ] , s_aClass[ n, 3 ] )
      EndIf
   Else
      s_aWindow[ nIndex ] := { 0, WT_MDICHILD, { } }         // no default codeblock
   EndIf

   // create a window
   If ( hWnd := WHT__CreateMDIWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                                 hWndParent, hInst , lParam ) ) > 0

      If s_aWindow[ nIndex, 1 ] == 0
         s_aWindow[ nIndex, 1 ] := hWnd
      EndIf
   Else
      s_aWindow[ nIndex ] := { NIL, NIL, { } }
      __KillWindow( )
   EndIf

   Return( hWnd )

//----------------------------------------------------------------------//

Function WHT__ProcessMsg( hWnd, nMsg, nwParam, nlParam, nIndex )
   Local n
   Local i := 0
   Local anWM
   Local bProc
   Local nType := WT_WINDOW
   Local nRet
   Local nProc //:=s_aProc[nIndex]
   Local oObj
   Local xCargo

   // setup list of possible procedures (max 10 per window )

   If s_aProc == NIL
      s_aProc := { ;
                 WHT_GetWndProc( 1 ) , ;
                 WHT_GetWndProc( 2 ) , ;
                 WHT_GetWndProc( 3 ) , ;
                 WHT_GetWndProc( 4 ) , ;
                 WHT_GetWndProc( 5 ) , ;
                 WHT_GetWndProc( 6 ) , ;
                 WHT_GetWndProc( 7 ) , ;
                 WHT_GetWndProc( 8 ) , ;
                 WHT_GetWndProc( 9 ) , ;
                 WHT_GetWndProc( 10 ) ;
               }
   EndIf

   // still in creation process ?

   If ( n := aScan( s_aWindow, { | x | hWnd == x[ 1 ] } ) ) == 0 //find the window
      If ( ( n := aScan( s_aWindow, { | x | 0 == x[ 1 ] .AND. WT_DIALOG != x[ 2 ] } ) ) > 0 )
         s_aWindow[ n, 1 ] := hWnd
      EndIf
   EndIf

   // find the procedure corresponding to the subclass index
   // bypass Windows procedure chain, where applicable

   nProc := s_aProc[ nIndex ]
   If !Empty( n )
      nType := s_aWindow[ n, 2 ]
      Do While ( i := aScan( s_aWindow[ n, 3 ] , { | x | nProc == x[ 3 ] } ) ) > 0 // does custom procedure exist ?
         anWM  := s_aWindow[ n, 3, i, 1 ]
         bProc := s_aWindow[ n, 3, i, 2 ]
         oObj  := s_aWindow[ n, 3, i, 5 ]
         xCargo:= s_aWindow[ n, 3, i, 6 ]
         If ! ( ValType( bProc ) $ "BN" .AND.  ( nMsg >= WM_USER .OR. anWM[ 1 ] == - 1 .OR. aScan( anWM, nMsg ) > 0 ) )
            If ( nProc := s_aWindow[ n, 3, i, 4 ] ) != 0 // old procedure exists
               If aScan( s_aProc, nProc ) == 0  // not our procedure
                  Return VWN_CallWindowProc( nProc, hWnd, nMsg, nwParam, nlParam ) // external
               EndIf
            Else
               //i := 0 // end of the road, call default
            EndIf
         Else
            Exit  // ok, we got it
         EndIf
      EndDo
   EndIf

   // process message

   If i == 0 // no subclassed proc
      If nType == WT_MDICHILD
         nRet := VWN_DefMDIChildProc( hWnd, nMsg, nwParam, nlParam )
      ElseIf nType == WT_MDIFRAME
         nRet := VWN_DefFrameProc( hWnd, nMsg, nwParam, nlParam )
      ElseIf nType == WT_DIALOG
         nRet := VWN_DefDlgProc( hWnd, nMsg, nwParam, nlParam )
      Else  //WT_WINDOW
         nRet := VWN_DefWindowProc( hWnd, nMsg, nwParam, nlParam )
      EndIf
   Else
      If Valtype( bProc ) == "N"
         nRet := HB_ExecFromArray( bProc, { oObj, hWnd, nMsg, nwParam, nlParam, xCargo } )
      Else
         nRet := Eval( bProc, hWnd, nMsg, nwParam, nlParam )
      Endif
   EndIf

   // remove the window from our internal list

   If nMsg == WM_NCDESTROY // last message to the window
      __KillWindow( hWnd )
   EndIf

   Return( nRet )

//----------------------------------------------------------------------//
//
// must be called on WM_NCDESTROY, which is irreversable !
// but only after processing our codeblock chain
//
Function __KillWindow( hWnd )
   Local n

   If hWnd != NIL .AND. ( n := aScan( s_aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0
      s_aWindow[ n ] := { NIL, NIL, { } }
   EndIf

   If aScan( s_aWindow, { | x | ! Empty( x[ 1 ] ) } ) == 0
      VWN_PostQuitMessage( 0 )
   EndIf

   Return( NIL )

//----------------------------------------------------------------------//
//
// must create a mechanism for initial locking up of the dialog window
// to the codeblock
//
Function WHT__ProcessDlgMsg( hDlg, nMsg, nwParam, nlParam )
   Local nIndex
   Local nResult
   Local n

   If ( ( nIndex := aScan( s_aDialog, { | x | hDlg == x[ 1 ] } ) ) == 0 )
      If ( ( nIndex := aScan( s_aDialog, { | x | 0 == x[ 1 ] } ) ) == 0 )
         Return( 0 )
      Else
         s_aDialog[ nIndex, 1 ] := hDlg
         If ( ( n := aScan( s_aWindow, { | x | 0 == x[ 1 ] .AND. WT_DIALOG == x[ 2 ] } ) ) > 0 )
            s_aWindow[ n ] := { hDlg, WT_DIALOG, { } }
         EndIf
      EndIf
   EndIf

   nResult := iif( ValType(s_aDialog[ nIndex, 2 ]) == "B", ;
                  eval( s_aDialog[ nIndex, 2 ] , hDlg, nMsg, nwParam, nlParam ),;
                  iif( Valtype( s_aDialog[ nIndex, 2 ] ) == "N", ;
                     HB_ExecFromArray( s_aDialog[ nIndex,2 ], { s_aDialog[ nIndex, 4], hDlg, nMsg, nwParam, nlParam, s_aDialog[ nIndex, 5 ] } ),;
                     0 );
                )

   If nMsg == WM_NCDESTROY
      s_aDialog[ nIndex ] := { NIL , NIL , NIL, NIL, NIL }
      If aScan( s_aWindow, { | x | hDlg == x[ 1 ] .AND. WT_DIALOG == x[ 2 ] .AND. Empty( x[ 3 ] ) } ) > 0
         __KillWindow( hDlg )
      EndIf
   EndIf

   Return( nResult )

//----------------------------------------------------------------------//

Function WHT_DialogBox( hInst, acnDlg, hWnd, bAction, oObj, xCargo )
   Local nResult
   Local nIndex
   Local cTemplate

   If !(ValType( bAction ) $ "BN")
      Return( - 1 )
   EndIf

   // register the dialog

   If  ( nIndex := aScan( s_aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( s_aDialog, { 0, bAction, 1, oObj, xCargo } )
      nIndex := Len( s_aDialog )

   Else
      s_aDialog[ nIndex ] := { 0, bAction, 1, oObj, xCargo }  // 0 means waiting...
                                                            // 1 means modal
   EndIf

   // create the template from the array
   //
   If ValType( acnDlg ) == "A"
      cTemplate := WHT__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[ 2 ] , acnDlg[ 3 ] , acnDlg[ 4 ] , ;
                                     acnDlg[ 5 ] , acnDlg[ 6 ] , acnDlg[ 7 ] , acnDlg[ 8 ] , ;
                                     acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )
      nResult := WHT__DialogBoxIndirect( hInst, cTemplate, hWnd, WHT__GetDlgProc( ) )

   Else
      nResult := WHT__DialogBox( hInst, acnDlg, hWnd, WHT__GetDlgProc( ) )

   EndIf

   s_aDialog[ nIndex ] := { NIL , NIL , NIL, NIL, NIL }    // unused

   Return( nResult )

//----------------------------------------------------------------------//
//
// internal to access setting dialog procedures as codeblocks
// for external/common dialogs.
//
FUNCTION _Get_aDialog()

   RETURN(s_aDialog)

//----------------------------------------------------------------------//
//
// internal to access setting window procedures as codeblocks
// for external/common dialogs.
//
FUNCTION _Get_aWindow()

   RETURN(s_aWindow)

//----------------------------------------------------------------------//

FUNCTION WHT_MakeDlgTemplate( cTitle, nStyle , x, y, nWidth, nHeight, nPointSize, ;
                                   cFaceName, nWeight, lItalic, nHelpId, nExStyle )
   // Prepare the template array
   // Element 1: dialog template
   // Elements 2-12: Properties of an item (each elemet - different property)

   LOCAL aDlg := { { } , { } , { } , { } , { } , { } , { } , { } , { } , { } , { } , { } }

   //aAdd(aDlg[1],1)       // add in C
   //aAdd(aDlg[1],0xFFFF)  // add in C

   // style
   If !ISNUMERIC( nStyle ) // nStyle
      nStyle := 0
      //   acnDlg:=DS_SETFONT
      //elseif AND(acnDlg,DS_SETFONT)==0
      //  acnDlg+=DS_SETFONT
   EndIf

   // But the programming interface and the result is the same.

   aAdd( aDlg[ 1 ] , iif( Empty( nHelpId ) , 0, nHelpId ) )   // new
   aAdd( aDlg[ 1 ] , iif( Empty( nExStyle ) , 0, nExStyle ) ) // new
   aAdd( aDlg[ 1 ] , nStyle ) // nStyle
   aAdd( aDlg[ 1 ] , 0 ) // no. of items
   aAdd( aDlg[ 1 ] , x )
   aAdd( aDlg[ 1 ] , y )
   aAdd( aDlg[ 1 ] , nWidth )
   aAdd( aDlg[ 1 ] , nHeight )
   aAdd( aDlg[ 1 ] , 0 ) // no menu ? pity, maybe I'll add later
   aAdd( aDlg[ 1 ] , 0 ) // default windows class
   aAdd( aDlg[ 1 ] , iif( ValType( cTitle ) == "C", cTitle, "" ) )

   If VWN_AND( nStyle, DS_SETFONT ) == DS_SETFONT
      aAdd( aDlg[ 1 ] , iif( ValType( nPointSize ) == "N", nPointSize, 8 ) )
      aAdd( aDlg[ 1 ] , iif( ValType( nWeight    ) == "N", nWeight, 400 ) )
      aAdd( aDlg[ 1 ] , iif( ValType( lItalic    ) == "L", lItalic, .F. ) )
      aAdd( aDlg[ 1 ] , iif( ValType( cFaceName  ) == "C", cFaceName, "MS Sans Serif" ) )
   EndIf

   Return( aDlg )

//----------------------------------------------------------------------//

Function WHT_CreateDialog( hInst, acnDlg , hWnd, bAction, oObj, xCargo )
   Local nIndex
   Local hDlg
   Local cTemplate
   Local n

   If !( ValType( bAction ) $ "BN" )
      Return( 0 )
   EndIf

   // prepare s_aDialog entry

   If ( nIndex := aScan( s_aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( s_aDialog, { 0 , bAction, 0, oObj, xCargo } )    // must add before CreateDialog
      nIndex := Len( s_aDialog )
   Else
      s_aDialog[ nIndex ] := { 0, bAction, 0, oObj, xCargo }  // window 0 means waiting ...
   EndIf                                                    // type 0 means modeless

   // we need to add it here too, to QUIT on the last window !!!
   // note type 0

   If ( n := aScan( s_aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( s_aWindow, { 0, WT_DIALOG, { } } )
      n := Len( s_aWindow )
   Else
      s_aWindow[ n ] := { 0, WT_DIALOG, { } }  // window 0 means waiting ...
   EndIf

   // create the dialog

   If ValType( acnDlg ) == "A"
      // create the template from the array
      cTemplate := WHT__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[ 2 ] , acnDlg[ 3 ] , acnDlg[ 4 ] , ;
                                     acnDlg[ 5 ] , acnDlg[ 6 ] , acnDlg[ 7 ] , acnDlg[ 8 ] , ;
                                     acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )

      hDlg := WHT__CreateDialogIndirect( hInst, cTemplate, hWnd, WHT__GetDlgProc() )

   Else
      hDlg := WHT__CreateDialog( hInst, acnDlg, hWnd, WHT__GetDlgProc() )

   EndIf

   // if failed to create
   If hDlg == 0
      s_aDialog[ nIndex ] := { NIL , NIL, NIL, NIL, NIL }
      s_aWindow[ n ] := { NIL , NIL , { } }
      __KillWindow( )
   EndIf

   Return( hDlg )

//----------------------------------------------------------------------//

Function WHT_AddDlgItem( aDlg, cnId, cnDlgClass, nStyle, nX, nY, ;
                       nWidth, nHeight, cText, nHelpId, nExStyle, cData )

   HB_SYMBOL_UNUSED( cData )

   aDlg[ 1, 4 ] ++ // item count

   aAdd( aDlg[ 2 ] ,  iif( ValType( nHelpId ) == "N", nHelpId, 0 ) )
   aAdd( aDlg[ 3 ] ,  iif( ValType( nExStyle ) == "N", nExStyle, 0 ) )
   aAdd( aDlg[ 4 ] ,  iif( ValType( nStyle ) == "N", nStyle, WS_CHILD + WS_VISIBLE ) )
   aAdd( aDlg[ 5 ] ,  nX )
   aAdd( aDlg[ 6 ] ,  nY )
   aAdd( aDlg[ 7 ] ,  nWidth )
   aAdd( aDlg[ 8 ] ,  nHeight )
   aAdd( aDlg[ 9 ] ,  cnId )
   aAdd( aDlg[ 10 ] , cnDlgClass )
   aAdd( aDlg[ 11 ] , iif( ISCHARACTER( cText ), cText, iif( ISNUMERIC( cText ), cText, "" ) ) )
   aAdd( aDlg[ 12 ] , 0 )    // cData

   Return aDlg

//----------------------------------------------------------------------//

Function WHT_SetProcedure( hWnd, bAction, anWM, oObj, xCargo )
   Local nProc := 0
   Local i, n
   Local nOldProc := 0

   // setup list of possible procedures (max 10 per window )

   If s_aProc == NIL
      s_aProc := { ;
                 WHT_GetWndProc( 1 ) , ;
                 WHT_GetWndProc( 2 ) , ;
                 WHT_GetWndProc( 3 ) , ;
                 WHT_GetWndProc( 4 ) , ;
                 WHT_GetWndProc( 5 ) , ;
                 WHT_GetWndProc( 6 ) , ;
                 WHT_GetWndProc( 7 ) , ;
                 WHT_GetWndProc( 8 ) , ;
                 WHT_GetWndProc( 9 ) , ;
                 WHT_GetWndProc( 10 ) ;
               }
   EndIf

   // make sure the window is in the array

   If VWN_IsWindow( hWnd )
      If ( n := aScan( s_aWindow, { | x | hWnd == x[ 1 ] } ) ) == 0
         If ( n := aScan( s_aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
            aAdd( s_aWindow, )
            n := Len( s_aWindow )
         EndIf
         s_aWindow[ n ] := { hWnd, WT_WINDOW, { } }
      EndIf

      // get a unique procedure address

      nOldProc := VWN_GetWindowLong( hWnd, GWL_WNDPROC )
      For i := 1 To 10
         If s_aProc[ i ] != nOldProc
            If aScan( s_aWindow[ n, 3 ] , { | x | x[ 3 ] == s_aProc[ i ] .OR. x[ 4 ] == s_aProc[ i ] } ) == 0
               nProc := s_aProc[ i ]
               Exit
            EndIf
         EndIf
      Next

      If !Empty( nProc )
         VWN_SetWindowLongPtr( hWnd, GWL_WNDPROC, nProc )
         If Empty( anWM )
            anWM := { 0 }
         ElseIf ValType( anWM ) == "N"
            anWM := { anWM }
         EndIf

         If !(ValType( bAction ) $ "BN")
            bAction := NIL
         EndIf
         aAdd( s_aWindow[ n, 3 ] , { anWM, bAction, nProc, nOldProc, oObj, xCargo } )
      EndIf

   EndIf

   Return( nOldProc )

//----------------------------------------------------------------------//

Function WHT_ResetProcedure( hWnd, nProc )
   Local n, i
   Local lRet := .F.

   If ( n := aScan( s_aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0 //find the window
      If !ISNUMERIC( nProc ) .OR. nProc == 0  // unsubclass all

         If Len( s_aWindow[ n, 3 ] ) > 0 // is subclassed
            If s_aWindow[ n, 3, 1, 4 ] == 0
               nProc := s_aWindow[ n, 3, 1, 3 ]
               aSize( s_aWindow[ n, 3 ] , 1 ) // class procedure must stay
            Else
               nProc := s_aWindow[ n, 3, 1, 4 ]
               aSize( s_aWindow[ n, 3 ] , 0 )
            EndIf
            If nProc != 0
               VWN_SetWindowLongPtr( hWnd, GWL_WNDPROC, nProc )
               lRet := .T.
            EndIf
         EndIf

      Else

         If ( i := aScan( s_aWindow[ n, 3 ] , { | x | x[ 4 ] == nProc } ) ) > 0
            Do While Len( s_aWindow[ n, 3 ] ) >= i
               aDel( s_aWindow[ n, 3 ] , i )
               aSize( s_aWindow[ n, 3 ] , Len( s_aWindow[ n, 3 ] ) - 1 )
            EndDo
            VWN_SetWindowLongPtr( hWnd, GWL_WNDPROC, nProc )
            lRet := .T.
         EndIf

      EndIf
   EndIf

   Return( lRet )

//----------------------------------------------------------------------//

Function WHT_WinProcCount( hWnd, nProc )
   Local n, i
   Local nRet := 0

   If ( n := aScan( s_aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0 //find the window
      If !ISNUMERIC( nProc ) .OR. nProc == 0
         nRet := Max( 0, Len( s_aWindow[ n, 3 ] ) - 1 )
      Else
         If ( i := aScan( s_aWindow[ n, 3 ] , { | x | x[ 3 ] == nProc } ) ) > 0
            nRet := Max( 0, i - 1 )
         EndIf
      EndIf
   EndIf

   Return( nRet )

//----------------------------------------------------------------------//

Function WHT_SelectWindow( hNewWnd )

   Local hOldActive := t_hWndActive

   If hNewWnd != NIL .AND. VWN_IsWindow( hNewWnd )
      t_hWndActive := hNewWnd
   EndIf

   Return( hOldActive )

//----------------------------------------------------------------------//

Function WHT_IsDialogMessage( hDlg, cMsg )

   If hDlg == NIL
     Return ( aScan( s_aDialog, {|x| 0 == x[ 3 ] .AND. VWN_IsDialogMessage( x[ 1 ], cMsg ) } ) > 0 )
   Endif

   Return( WHT_IsDialogMessage( hDlg, cMsg ) )

//----------------------------------------------------------------------//
