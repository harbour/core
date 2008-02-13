/*
 * $Id$
 */


/*
                       W A R N I N G   !!!

    *******************************************************
      Do not alter this code, unless you understand fully
    how it works, and, if you really know what you're doing
    *******************************************************

*/

#xCommand ? ? < x > = > OutputDebugString( asString( < x > ) )
#xCommand ? < x > = > OutputDebugString( asString( < x > ) + chr( 13 ) )

#Define WT_DIALOG     0      // used internally (user custom dialog class - advanced option)
#Define WT_WINDOW     1      // use DefWindowProc()
#Define WT_MDIFRAME   2      // use DefFrameProc()
#Define WT_MDICHILD   4      // use DefMDIChildProc()

#include "common.ch"

#Include "winuser.ch"
#Include "hboo.ch"

//PUBLIC lPrevInstance
//PUBLIC hThisInstance

Static hWndActive := 0
Static aClass:={}  //cClass,nType,{{anWM,bAction,nProc,0}}
Static aWindow:={} //hWnd, nType, {{anWM,bAction,nProc,nOldProc}}
Static aDialog:={} //hDlg, {{anWM.bAction}} // maybe rather add them to aWindow as type 0 - dialog ?
Static aProc       // array of possible windows procedures (10)
                   // for subclassing ???


*-----------------------------------------------------------------------------*
/*
INIT PROCEDURE _CheckMultiInst


   lPrevInstance:=(empty(CreateMutex( , .T., strtran(GetModuleFileName(),'\','_') )) ;
                   .or. (GetLastError() > 0) )

   hThisInstance:=_getinstance()

   RETURN
*/
*-----------------------------------------------------------------------------*
FUNCTION WhatVersion(dDate)

 dDate:=stod("20020821")

 RETURN ("0.g")

*-----------------------------------------------------------------------------*

//SYNTAX: RegisterClass( WNDCLASS, [<nType>], [<bAction>], [< anWM >] )
// where ntype: is one of RFC_* values
//     bAction: event procedure codeblock
//        anWM: selective messages to send to codeblock ( -1 == All )

Function RegisterClass( wndclass, nType, bAction, anWM, oObj, xCargo)

   Local aAction

  // wndclass:cbSize        := LEN( wndclass:value )
   wndclass:lpfnWndProc   := 0
   wndclass:style         := if( wndclass:style==NIL,(CS_HREDRAW + CS_VREDRAW + CS_OWNDC + CS_DBLCLKS), wndclass:style )
   wndclass:cbClsExtra    := if( wndclass:cbClsExtra==NIL, 0, wndclass:cbClsExtra )
   wndclass:cbWndExtra    := if( wndclass:cbWndExtra==NIL, 0, wndclass:cbWndExtra )
   wndclass:hInstance     := if( wndclass:hInstance==NIL, GetModuleHandle(), wndclass:hInstance )
   wndclass:hIcon         := if( wndclass:hIcon==NIL, LoadIcon(GetModuleHandle(),""), wndclass:hIcon )
   wndclass:hCursor       := if( wndclass:hCursor==NIL, LoadCursor(, IDC_ARROW), wndclass:hCursor)
   wndclass:hbrBackground := if( wndclass:hbrBackground==NIL, COLOR_WINDOW  + 1, wndclass:hbrBackground )
   wndclass:lpszMenuName  := if( wndclass:lpszMenuName==NIL, "", wndclass:lpszMenuName ) ;

   IF VALTYPE(wndclass:lpszClassName)<> "C" .OR. EMPTY(wndclass:lpszClassName) .OR. ;
      ! _RegisterClass( wndclass:value )
      Return( .F. )
   ENDIF

   // note : _RegisterClass() function MUST add our
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

   aAction := { anWM, bAction, GetWndProc( 1 ) , 0, oObj, xCargo }
   aAdd( aClass, { WNDCLASS:lpszClassName, nType, aAction } )

   Return( .T. )


*-----------------------------------------------------------------------------*

Function UnregisterClass( cClass, hInst )

   Local n

   If ! _UnregisterClass( cClass, hInst )
      Return( .F. )
   EndIf

   If ( n := aScan( aClass, { | x | x[ 1 ] == cClass } ) ) > 0
      aDel( aClass, n )
      aSize( aClass, Len( aClass ) - 1 )
   EndIf

   Return( .T. )


*-----------------------------------------------------------------------------*

Function CreateWindowEx( nExStyle, cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                         hWndParent, hMenu, hInst , cParam )

   Return CreateWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                        hWndParent, hMenu, hInst , cParam, nExStyle )


*-----------------------------------------------------------------------------*

   // Uses CreateWindowEx  !!!!!!!!
   // Note extra params

Function CreateWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                       hWndParent, hMenu, hInst , cParam, nExStyle )

   Local hWnd
   Local n
   Local nIndex

   // prepare a slot in aWindow array

   If ( nIndex := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0  // waiting
      aAdd( aWindow, )
      nIndex := Len( aWindow )
   EndIf

   // add default class procedure address and block

   If ( n := aScan( aClass, { | x | cClass == x[ 1 ] } ) ) > 0 // own window class
      aWindow[ nIndex ] := { 0, aClass[ n, 2 ] , { } }
      If ! Empty( aClass[ n, 3 ] )                     // if default user codeblock exists
         aAdd( aWindow[ nIndex, 3 ] , aClass[ n, 3 ] )
      EndIf
   Else
      aWindow[ nIndex ] := { 0, WT_WINDOW, { } }         // no default codeblock
   EndIf

   // create a window


   If ( hWnd := _CreateWindowEx( nExStyle, cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                                 hWndParent, hMenu, hInst , cParam ) ) > 0


      If aWindow[ nIndex, 1 ] == 0
         aWindow[ nIndex, 1 ] := hWnd
      EndIf
   Else
      aWindow[ nIndex ] := { NIL, NIL, { } }
      __KillWindow( )
   EndIf

   Return( hWnd )

*-----------------------------------------------------------------------------*

   // Creates MDI child window

Function CreateMDIWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                       hWndParent, hInst , lParam )

   Local hWnd
   Local n
   Local nIndex

   // prepare a slot in aWindow array

   If ( nIndex := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0  // waiting
      aAdd( aWindow, )
      nIndex := Len( aWindow )
   EndIf

   // add default class procedure address and block

   If ( n := aScan( aClass, { | x | cClass == x[ 1 ] } ) ) > 0 // own window class
      aWindow[ nIndex ] := { 0, aClass[ n, 2 ] , { } }
      If ! Empty( aClass[ n, 3 ] )                     // if default user codeblock exists
         aAdd( aWindow[ nIndex, 3 ] , aClass[ n, 3 ] )
      EndIf
   Else
      aWindow[ nIndex ] := { 0, WT_MDICHILD, { } }         // no default codeblock
   EndIf

   // create a window

   If ( hWnd := _CreateMDIWindow( cClass, cTitle, nStyle, nX, nY, nWidth, nHeight, ;
                                 hWndParent, hInst , lParam ) ) > 0

      If aWindow[ nIndex, 1 ] == 0
         aWindow[ nIndex, 1 ] := hWnd
      EndIf
   Else
      aWindow[ nIndex ] := { NIL, NIL, { } }
      __KillWindow( )
   EndIf

   Return( hWnd )

*-----------------------------------------------------------------------------*

Function _ProcessMsg( hWnd, nMsg, nwParam, nlParam, nIndex )

   Local n
   Local i := 0
   Local anWM
   Local bProc
   Local nType := WT_WINDOW
   Local nRet := 0
   Local nProc //:=aProc[nIndex]
   Local oObj
   Local xCargo

   // setup list of possible procedures (max 10 per window )

   If aProc == NIL
      aProc := { ;
                 GetWndProc( 1 ) , ;
                 GetWndProc( 2 ) , ;
                 GetWndProc( 3 ) , ;
                 GetWndProc( 4 ) , ;
                 GetWndProc( 5 ) , ;
                 GetWndProc( 6 ) , ;
                 GetWndProc( 7 ) , ;
                 GetWndProc( 8 ) , ;
                 GetWndProc( 9 ) , ;
                 GetWndProc( 10 ) ;
               }
   EndIf

   // still in creation process ?

   If ( n := aScan( aWindow, { | x | hWnd == x[ 1 ] } ) ) == 0 //find the window
      If ( ( n := aScan( aWindow, { | x | 0 == x[ 1 ] .AND. WT_DIALOG <> x[ 2 ] } ) ) > 0 )
         aWindow[ n, 1 ] := hWnd
      EndIf
   EndIf

   // find the procedure corresponding to the subclass index
   // bypass Windows procedure chain, where applicable

   nProc := aProc[ nIndex ]
   If n > 0
      nType := aWindow[ n, 2 ]
      Do While ( i := aScan( aWindow[ n, 3 ] , { | x | nProc == x[ 3 ] } ) ) > 0 // does custom procedure exist ?
         anWM  := aWindow[ n, 3, i, 1 ]
         bProc := aWindow[ n, 3, i, 2 ]
         oObj  := aWindow[ n, 3, i, 5 ]
         xCargo:= aWindow[ n, 3, i, 6 ]
         If ! ( ValType( bProc ) $ "BN" .AND.  ( nMsg >= WM_USER .OR. anWM[ 1 ] == - 1 .OR. aScan( anWM, nMsg ) > 0 ) )
            If ( nProc := aWindow[ n, 3, i, 4 ] ) <> 0 // old procedure exists
               If aScan( aProc, nProc ) == 0  // not our procedure
                  Return CallWindowProc( nProc, hWnd, nMsg, nwParam, nlParam ) // external
               EndIf
            Else
               i := 0 // end of the road, call default
            EndIf
         Else
            Exit  // ok, we got it
         EndIf
      EndDo

   EndIf

   // process message

   If i == 0 // no subclassed proc
      If nType == WT_MDICHILD
         nRet := DefMDIChildProc( hWnd, nMsg, nwParam, nlParam )
      ElseIf nType == WT_MDIFRAME
         nRet := DefFrameProc( hWnd, nMsg, nwParam, nlParam )
      ElseIf nType == WT_DIALOG
         nRet := DefDlgProc( hWnd, nMsg, nwParam, nlParam )
      Else  //WT_WINDOW
         nRet := DefWindowProc( hWnd, nMsg, nwParam, nlParam )
      EndIf
   Else
      If Valtype(bProc)=="N"
         nRet := HB_Exec( bProc, oObj, hWnd, nMsg, nwParam, nlParam, xCargo )
      Else
         nRet := Eval( bProc, hWnd, nMsg, nwParam, nlParam )
      Endif
   EndIf

   // remove the window from our internal list

   If nMsg == WM_NCDESTROY // last message to the window
      __KillWindow( hWnd )
   EndIf

   Return( nRet )


*-----------------------------------------------------------------------------*

   // must be called on WM_NCDESTROY, which is irreversable !
   // but only after processing our codeblock chain

Function __KillWindow( hWnd )

   Local n

   If hWnd <> NIL .AND. ( n := aScan( aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0
      aWindow[ n ] := { NIL, NIL, { } }
   EndIf

   If aScan( aWindow, { | x | ! Empty( x[ 1 ] ) } ) == 0
      PostQuitMessage( 0 )
   EndIf

   Return( NIL )


*-----------------------------------------------------------------------------*

   // must create a mechanism for initial locking up of the dialog window
   // to the codeblock

Function _ProcessDlgMsg( hDlg, nMsg, nwParam, nlParam )

   Local nIndex := 0
   Local nResult
   Local n := 0

   If ( ( nIndex := aScan( aDialog, { | x | hDlg == x[ 1 ] } ) ) == 0 )
      If ( ( nIndex := aScan( aDialog, { | x | 0 == x[ 1 ] } ) ) == 0 )
         Return( 0 )
      Else
         aDialog[ nIndex, 1 ] := hDlg
         If ( ( n := aScan( aWindow, { | x | 0 == x[ 1 ] .AND. WT_DIALOG == x[ 2 ] } ) ) > 0 )
            aWindow[ n ] := { hDlg, WT_DIALOG, { } }
         EndIf
      EndIf
   EndIf

   nResult := if( ValType(aDialog[ nIndex, 2 ]) == "B", ;
                  eval( aDialog[ nIndex, 2 ] , hDlg, nMsg, nwParam, nlParam ),;
                  if(Valtype(aDialog[ nIndex, 2 ])=="N", ;
                     HB_Exec( aDialog[ nIndex,2 ], aDialog[ nIndex, 4], hDlg, nMsg, nwParam, nlParam, aDialog[ nIndex, 5 ]  ),;
                     0 );
                )

   If nMsg == WM_NCDESTROY
      aDialog[ nIndex ] := { NIL , NIL , NIL, NIL, NIL }
      If ( n := aScan( aWindow, { | x | hDlg == x[ 1 ] .AND. WT_DIALOG == x[ 2 ] .AND. Empty( x[ 3 ] ) } ) ) > 0
         __KillWindow( hDlg )
      EndIf
   EndIf

   Return( nResult )


*-----------------------------------------------------------------------------*

Function DialogBox( hInst, acnDlg, hWnd, bAction, oObj, xCargo )

   Local nResult := 0
   Local nIndex
   Local cTemplate

   If !(ValType( bAction ) $ "BN")
      Return( - 1 )
   EndIf

   // register the dialog

   If  ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( aDialog, { 0, bAction, 1, oObj, xCargo } )
      nIndex := Len( aDialog )
   Else
      aDialog[ nIndex ] := { 0, bAction, 1, oObj, xCargo }  // 0 means waiting...
   EndIf                                                    // 1 means modal

   // create the template from the array

   If ValType( acnDlg ) == "A"

      cTemplate := _MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[ 2 ] , acnDlg[ 3 ] , acnDlg[ 4 ] , ;
                                     acnDlg[ 5 ] , acnDlg[ 6 ] , acnDlg[ 7 ] , acnDlg[ 8 ] , ;
                                     acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )

      nResult := _DialogBoxIndirect( hInst, cTemplate, hWnd, _GetDlgProc( ) )
   Else
      nResult := _DialogBox( hInst, acnDlg, hWnd, _GetDlgProc( ) )
   EndIf

   aDialog[ nIndex ] := { NIL , NIL , NIL, NIL, NIL }    // unused

   Return( nResult )



*-----------------------------------------------------------------------------*
// internal to access setting dialog procedures as codeblocks
// for external/common dialogs.

FUNCTION _Get_aDialog()

  RETURN(aDialog)

*-----------------------------------------------------------------------------*
// internal to access setting window procedures as codeblocks
// for external/common dialogs.

FUNCTION _Get_aWindow()

  RETURN(aWindow)

*-----------------------------------------------------------------------------*

FUNCTION MakeDlgTemplate( cTitle, nStyle , x, y, nWidth, nHeight, nPointSize, ;
                          cFaceName, nWeight, lItalic, nHelpId, nExStyle )


   // Prepare the template array
   // Element 1: dialog template
   // Elements 2-12: Properties of an item (each elemet - different property)


  LOCAL aDlg := { { } , { } , { } , { } , { } , { } , { } , { } , { } , { } , { } , { } }

   //aAdd(aDlg[1],1)       // add in C
   //aAdd(aDlg[1],0xFFFF)  // add in C


   // style
   If ValType( nStyle ) <> "N" // nStyle
      nStyle := 0
      //   acnDlg:=DS_SETFONT
      //elseif AND(acnDlg,DS_SETFONT)==0
      //  acnDlg+=DS_SETFONT
   EndIf

   // But the programming interface and the result is the same.

   aAdd( aDlg[ 1 ] , If( Empty( nHelpId ) , 0, nHelpId ) )   // new
   aAdd( aDlg[ 1 ] , If( Empty( nExStyle ) , 0, nExStyle ) ) // new
   aAdd( aDlg[ 1 ] , nStyle ) // nStyle
   aAdd( aDlg[ 1 ] , 0 ) // no. of items
   aAdd( aDlg[ 1 ] , x )
   aAdd( aDlg[ 1 ] , y )
   aAdd( aDlg[ 1 ] , nWidth )
   aAdd( aDlg[ 1 ] , nHeight )
   aAdd( aDlg[ 1 ] , 0 ) // no menu ? pity, maybe I'll add later
   aAdd( aDlg[ 1 ] , 0 ) // default windows class
   aAdd( aDlg[ 1 ] , If( ValType( cTitle ) == "C", cTitle, "" ) )

   If AND( nStyle, DS_SETFONT ) == DS_SETFONT
      aAdd( aDlg[ 1 ] , If( ValType( nPointSize ) == "N", nPointSize, 8 ) )
      aAdd( aDlg[ 1 ] , If( ValType( nWeight ) == "N", nWeight, 400 ) )
      aAdd( aDlg[ 1 ] , If( ValType( lItalic ) == "L", lItalic, .F. ) )
      aAdd( aDlg[ 1 ] , If( ValType( cFaceName ) == "C", cFaceName, "MS Sans Serif" ) )
   EndIf

   Return( aDlg )

*-----------------------------------------------------------------------------*

Function CreateDialog( hInst, acnDlg , hWnd, bAction, oObj, xCargo )

   Local nIndex
   Local hDlg
   Local cTemplate
   Local n

      If !(ValType( bAction ) $ "BN")
         Return( 0 )
      EndIf

      // prepare aDialog entry

      If ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
         aAdd( aDialog, { 0 , bAction, 0, oObj, xCargo } )    // must add before CreateDialog
         nIndex := Len( aDialog )
      Else
         aDialog[ nIndex ] := { 0, bAction, 0, oObj, xCargo }  // window 0 means waiting ...
      EndIf                                                    // type 0 means modeless

      // we need to add it here too, to QUIT on the last window !!!
      // note type 0

      If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
         aAdd( aWindow, { 0, WT_DIALOG, { } } )
         n := Len( aWindow )
      Else
         aWindow[ n ] := { 0, WT_DIALOG, { } }  // window 0 means waiting ...
      EndIf

      // create the dialog

      If ValType( acnDlg ) == "A"
         // create the template from the array
         cTemplate := _MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[ 2 ] , acnDlg[ 3 ] , acnDlg[ 4 ] , ;
                                        acnDlg[ 5 ] , acnDlg[ 6 ] , acnDlg[ 7 ] , acnDlg[ 8 ] , ;
                                        acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )

         hDlg := _CreateDialogIndirect( hInst, cTemplate, hWnd, _GetDlgProc( ) )
      Else
         hDlg := _CreateDialog( hInst, acnDlg, hWnd, _GetDlgProc( ) )
      EndIf

      // if failed to create
      If hDlg == 0
         aDialog[ nIndex ] := { NIL , NIL, NIL, NIL, NIL }
         aWindow[ n ] := { NIL , NIL , { } }
         __KillWindow( )
      EndIf

      Return( hDlg )


*-----------------------------------------------------------------------------*

Function AddDlgItem( aDlg, cnId, cnDlgClass, nStyle, nX, nY, ;
                       nWidth, nHeight, cText, nHelpId, nExStyle, cData )

   HB_SYMBOL_UNUSED( cData )

   aDlg[ 1, 4 ] ++ // item count

   aAdd( aDlg[ 2 ] ,  If( ValType( nHelpId ) == "N", nHelpId, 0 ) )
   aAdd( aDlg[ 3 ] ,  If( ValType( nExStyle ) == "N", nExStyle, 0 ) )
   aAdd( aDlg[ 4 ] ,  If( ValType( nStyle ) == "N", nStyle, WS_CHILD + WS_VISIBLE ) )
   aAdd( aDlg[ 5 ] ,  nX )
   aAdd( aDlg[ 6 ] ,  nY )
   aAdd( aDlg[ 7 ] ,  nWidth )
   aAdd( aDlg[ 8 ] ,  nHeight )
   aAdd( aDlg[ 9 ] ,  cnId )
   aAdd( aDlg[ 10 ] , cnDlgClass )
   aAdd( aDlg[ 11 ] , If( ValType( cText ) <> "C", If( ValType( cText ) == "N", cText, "" ) , cText ) )
   aAdd( aDlg[ 12 ] , 0 )    // cData

   Return aDlg


*-----------------------------------------------------------------------------*

Function SetProcedure( hWnd, bAction, anWM, oObj, xCargo )

   Local nProc := 0
   Local i, n
   Local nOldProc := 0

   // setup list of possible procedures (max 10 per window )

   If aProc == NIL
      aProc := { ;
                 GetWndProc( 1 ) , ;
                 GetWndProc( 2 ) , ;
                 GetWndProc( 3 ) , ;
                 GetWndProc( 4 ) , ;
                 GetWndProc( 5 ) , ;
                 GetWndProc( 6 ) , ;
                 GetWndProc( 7 ) , ;
                 GetWndProc( 8 ) , ;
                 GetWndProc( 9 ) , ;
                 GetWndProc( 10 ) ;
               }
   EndIf

   // make sure the window is in the array

   If IsWindow( hWnd )
      If ( n := aScan( aWindow, { | x | hWnd == x[ 1 ] } ) ) == 0
         If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
            aAdd( aWindow, )
            n := Len( aWindow )
         EndIf
         aWindow[ n ] := { hWnd, WT_WINDOW, { } }
      EndIf

      // get a unique procedure address

      nOldProc := GetWindowLong( hWnd, GWL_WNDPROC )
      For i := 1 To 10
         If aProc[ i ] <> nOldProc
            If aScan( aWindow[ n, 3 ] , { | x | x[ 3 ] == aProc[ i ] .OR. x[ 4 ] == aProc[ i ] } ) == 0
               nProc := aProc[ i ]
               Exit
            EndIf
         EndIf
      Next

      If nProc <> 0
         SetWindowLong( hWnd, GWL_WNDPROC, nProc )
         If Empty( anWM )
            anWM := { 0 }
         ElseIf ValType( anWM ) == "N"
            anWM := { anWM }
         EndIf

         If !(ValType( bAction ) $ "BN")
            bAction := NIL
         EndIf
         aAdd( aWindow[ n, 3 ] , { anWM, bAction, nProc, nOldProc, oObj, xCargo } )
      EndIf

   EndIf

   Return( nOldProc )


*-----------------------------------------------------------------------------*

Function ResetProcedure( hWnd, nProc )

   Local n, i
   Local lRet := .F.

   If ( n := aScan( aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0 //find the window
      If ValType( nProc ) <> "N" .OR. nProc == 0  // unsubclass all

         If Len( aWindow[ n, 3 ] ) > 0 // is subclassed
            If aWindow[ n, 3, 1, 4 ] == 0
               nProc := aWindow[ n, 3, 1, 3 ]
               aSize( aWindow[ n, 3 ] , 1 ) // class procedure must stay
            Else
               nProc := aWindow[ n, 3, 1, 4 ]
               aSize( aWindow[ n, 3 ] , 0 )
            EndIf
            If nProc <> 0
               SetWindowLong( hWnd, GWL_WNDPROC, nProc )
               lRet := .T.
            EndIf
         EndIf

      Else

         If ( i := aScan( aWindow[ n, 3 ] , { | x | x[ 4 ] == nProc } ) ) > 0
            Do While Len( aWindow[ n, 3 ] ) >= i
               aDel( aWindow[ n, 3 ] , i )
               aSize( aWindow[ n, 3 ] , Len( aWindow[ n, 3 ] ) - 1 )
            EndDo
            SetWindowLong( hWnd, GWL_WNDPROC, nProc )
            lRet := .T.
         EndIf

      EndIf
   EndIf

   Return( lRet )


*-----------------------------------------------------------------------------*

Function WinProcCount( hWnd, nProc )

   Local n, i
   Local nRet := 0

   If ( n := aScan( aWindow, { | x | hWnd == x[ 1 ] } ) ) > 0 //find the window
      If ValType( nProc ) <> "N" .OR. nProc == 0
         nRet := Max( 0, Len( aWindow[ n, 3 ] ) - 1 )
      Else
         If ( i := aScan( aWindow[ n, 3 ] , { | x | x[ 3 ] == nProc } ) ) > 0
            nRet := Max( 0, i - 1 )
         EndIf
      EndIf
   EndIf

   Return( nRet )


*-----------------------------------------------------------------------------*

Function SelectWindow( hNewWnd )

   Local hOldActive := hWndActive

   If hNewWnd <> NIL .AND. IsWindow( hNewWnd )
      hWndActive := hNewWnd
   EndIf

   Return( hOldActive )


*------------------------------------------------------------------------------

Function isDialogMessage( hDlg, cMsg )

   If hDlg == NIL
     Return ( aScan( aDialog, {|x| 0 == x[ 3 ] .AND. _isDialogMessage( x[ 1 ], cMsg ) } ) > 0 )
   Endif

   Return( isDialogMessage(hDlg, cMsg ) )



//HARBOUR C FUNCTIONS:

*------------------------------------------------------------------------------
* Low Level C Routines
*------------------------------------------------------------------------------
#pragma BEGINDUMP


#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

LRESULT CALLBACK __WndProc   (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc2  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc3  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc4  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc5  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc6  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc7  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc8  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc9  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;
LRESULT CALLBACK __WndProc10 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;

BOOL CALLBACK __DlgProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) ;

int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn);
LPWORD lpwAlign ( LPWORD lpIn);

//-----------------------------------------------------------------------------

// note: specifying (HBRUSH) COLOR_WINDOW+1 makes it not working
//       (the window background stays black)
//       but not specifying it generates a WARNING, but it works OK.

HB_FUNC( _REGISTERCLASS )
{
   WNDCLASS *wndclass = ( WNDCLASS *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;
   wndclass->lpfnWndProc   = __WndProc ;

   /*
   wndclass.style         = (ISNIL(1) ? (CS_HREDRAW | CS_VREDRAW | CS_OWNDC | CS_DBLCLKS)  : hb_parni(1) );
   wndclass.lpfnWndProc   = __WndProc ;
   wndclass.cbClsExtra    = ( ISNIL(2)  ? 0 : hb_parni(2)) ;
   wndclass.cbWndExtra    = ( ISNIL(3)  ? 0 : hb_parni(3)) ;
   wndclass.hInstance     = ( ISNIL(4)  ? GetModuleHandle(NULL) : (HANDLE) hb_parnl(4) ) ;
   wndclass.hIcon         = ( ISNIL(5)  ? LoadIcon(GetModuleHandle(NULL),"") : (HICON) hb_parnl(5) ) ;
   wndclass.hCursor       = ( ISNIL(6)  ? LoadCursor (NULL, IDC_ARROW) : (HCURSOR) hb_parnl(6) ) ;
   wndclass.hbrBackground = ( ISNIL(7)  ? (INT) COLOR_WINDOW  + 1 :  (HBRUSH) hb_parnl(7) ) ;
   wndclass.lpszMenuName  = (LPCSTR) ( !ISNIL(8) ? hb_parc(8) : NULL ) ;
   wndclass.lpszClassName = (LPCSTR) hb_parc(9) ;
   */

   hb_retl( RegisterClass (wndclass));
}


//-----------------------------------------------------------------------------

HB_FUNC( _UNREGISTERCLASS )
{
   HANDLE hInst = ( ISNIL(2) ? GetModuleHandle(NULL) : (HANDLE) hb_parnl(2) ) ;

   hb_retl( UnregisterClass( hb_parc(1), (HINSTANCE) hInst ) ) ;
}

//-----------------------------------------------------------------------------

// hMenu or Window ID are interchangable
// casting Id to hMenu stops it from working correctly

HB_FUNC( _CREATEWINDOWEX )
{
   DWORD  dwExStyle  = (ISNIL(1)  ? 0 : hb_parnl(1)) ;
   LPCSTR cClass     = (LPCSTR) hb_parc(2);
   LPCSTR cTitle     = (LPCSTR) hb_parc(3);
   DWORD  nStyle     = (ISNIL(4)  ? 0 : (DWORD) hb_parnd(4) );
   int    x          = (ISNIL(5)  ? CW_USEDEFAULT : hb_parni(5));
   int    y          = (ISNIL(6)  ? CW_USEDEFAULT : hb_parni(6));
   int    nWidth     = (ISNIL(7)  ? CW_USEDEFAULT : hb_parni(7));
   int    nHeight    = (ISNIL(8)  ? CW_USEDEFAULT : hb_parni(8));
   HWND   hWndParent = (ISNIL(9)  ? (HWND) NULL : (HWND) hb_parnl(9)) ;
   HMENU  hMenu      = (ISNIL(10) ? (HMENU) NULL : (HMENU) hb_parni(10));
   HANDLE hInstance  = (ISNIL(11) ? GetModuleHandle( NULL ) : (HANDLE) hb_parnl(11));
   LPVOID lParam     = (ISNIL(12) ? NULL : (LPVOID) hb_parnl(12));

   HWND hWnd = CreateWindowEx( dwExStyle, cClass, cTitle,
                               nStyle, x, y, nWidth, nHeight,
                               hWndParent, hMenu, (HINSTANCE) hInstance, lParam )  ;

   hb_retnl( (LONG) hWnd );
}

//-----------------------------------------------------------------------------

//  Creates child MDI window

HB_FUNC( _CREATEMDIWINDOW )
{
   LPCSTR cClass     = (LPCSTR) hb_parc(1);
   LPCSTR cTitle     = (LPCSTR) hb_parc(2);
   DWORD  nStyle     = (ISNIL(3)  ? WS_MAXIMIZE : (DWORD) hb_parnd(3) );
   int    x          = (ISNIL(4)  ? CW_USEDEFAULT : hb_parni(4));
   int    y          = (ISNIL(5)  ? CW_USEDEFAULT : hb_parni(5));
   int    nWidth     = (ISNIL(6)  ? CW_USEDEFAULT : hb_parni(6));
   int    nHeight    = (ISNIL(7)  ? CW_USEDEFAULT : hb_parni(7));
   HWND   hWndParent = (ISNIL(8)  ? (HWND) NULL : (HWND) hb_parnl(8)) ;
   HANDLE hInstance  = (ISNIL(9)  ? GetModuleHandle( NULL ) : (HANDLE) hb_parnl(9));
   LPARAM lParam     = (ISNIL(10) ? 0 : (LPARAM) hb_parnl(10));

   HWND hWnd = CreateMDIWindow( cClass, cTitle,nStyle,
                                x, y, nWidth, nHeight,
                                hWndParent, (HINSTANCE) hInstance, lParam ) ;
   hb_retnl( (LONG) hWnd );
}


//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 1 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc2 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 2 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc3 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 3 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc4 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 4 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc5 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 5 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc6 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 6 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc7 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 7 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc8 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 8 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc9 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 9 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}

//-----------------------------------------------------------------------------

LRESULT CALLBACK __WndProc10 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmPushLong( 10 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}


//-----------------------------------------------------------------------------

// called once, on start-up

HB_FUNC( GETWNDPROC )
{

  switch ( hb_parni(1) )
  {
    case 10:
      hb_retnl( (ULONG) __WndProc10 ) ;
      return ;

    case 9:
      hb_retnl( (ULONG) __WndProc9 ) ;
      return ;

    case 8:
      hb_retnl( (ULONG) __WndProc8 ) ;
      return ;

    case 7:
      hb_retnl( (ULONG) __WndProc7 ) ;
      return ;

    case 6:
      hb_retnl( (ULONG) __WndProc6 ) ;
      return ;

    case 5:
      hb_retnl( (ULONG) __WndProc5 ) ;
      return ;

    case 4:
      hb_retnl( (ULONG) __WndProc4 ) ;
      return ;

    case 3:
      hb_retnl( (ULONG) __WndProc3 ) ;
      return ;

    case 2:
      hb_retnl( (ULONG) __WndProc2 ) ;
      return ;

    case 1:
      hb_retnl( (ULONG) __WndProc ) ;
      return ;

    default:
      hb_retnl( (ULONG) 0 ) ;
      return ;

  }

}


//----------------------------------------------------------------------------

HB_FUNC( _GETDLGPROC )
{
    hb_retnl( (ULONG) __DlgProc ) ;
}

//-----------------------------------------------------------------------------

BOOL CALLBACK __DlgProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   BOOL res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "_PROCESSDLGMSG" );

   if ( pSymTest )
   {
      //hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushSymbol( hb_itemGetSymbol( pSymTest ) );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return FALSE ;
}



//----------------------------------------------------------------------------

HB_FUNC( _DIALOGBOX )
{


  hb_retni( DialogBox( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) )  ,
                       (hb_parinfo(2)==HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))) ,
                       (ISNIL(3) ?  NULL : (HWND) hb_parnl(3) )        ,
                       (DLGPROC) hb_parnl(4)
                     ));

}


//----------------------------------------------------------------------------

HB_FUNC( _DIALOGBOXINDIRECT )
{

   hb_retni( DialogBoxIndirect( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) )   ,
                               (LPDLGTEMPLATE) hb_parc(2)  ,
                               (ISNIL(3) ?  NULL : (HWND) hb_parnl(3) )        ,
                               (DLGPROC) hb_parnl(4)
                             ));
}


//----------------------------------------------------------------------------

HB_FUNC( _CREATEDIALOG )
{

  hb_retnl( (ULONG) CreateDialog( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) )  ,
                                  (hb_parinfo(2)==HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))) ,
                                  (ISNIL(3) ?  NULL : (HWND) hb_parnl(3) )        ,
                                  (DLGPROC) hb_parnl(4)
                                ) );
}


//----------------------------------------------------------------------------

HB_FUNC( _CREATEDIALOGINDIRECT )
{
  hb_retnl(
   (ULONG) CreateDialogIndirect(
            (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) )   ,
            (LPDLGTEMPLATE) hb_parc(2) ,
            (ISNIL(3) ?  NULL : (HWND) hb_parnl(3) )        ,
            (DLGPROC) hb_parnl(4)
          ));
}


//-----------------------------------------------------------------------------

// Create dynamic dialog from the Harbour array

HB_FUNC( _MAKEDLGTEMPLATE )

{
   WORD *p, *pdlgtemplate ;
   WORD  nItems = hb_parni( 1, 4 ) ;
   int   i, nchar ;
   DWORD lStyle ;

   // Parameters: 12 arrays
   // 1 for DLG template
   // 11 for item properties

   pdlgtemplate = p = (PWORD) LocalAlloc (LPTR, 65534)  ; // 64k allow to build up to 255 items on the dialog

   //---------------

    lStyle = hb_parnl(1,3) ;

    // start to fill in the dlgtemplate information.  addressing by WORDs

    *p++ = 1                   ; // version
    *p++ = 0xFFFF                   ; // signature
    *p++ = LOWORD ( hb_parnl(1,1) ) ; // Help Id
    *p++ = HIWORD ( hb_parnl(1,1) ) ;

    *p++ = LOWORD ( hb_parnl(1,2) ) ; // ext. style
    *p++ = HIWORD ( hb_parnl(1,2) ) ;

    *p++ = LOWORD (lStyle)          ;
    *p++ = HIWORD (lStyle)          ;

    *p++ = (WORD)   nItems        ;  // NumberOfItems
    *p++ = (short)  hb_parni(1,5) ;  // x
    *p++ = (short)  hb_parni(1,6) ;  // y
    *p++ = (short)  hb_parni(1,7) ;  // cx
    *p++ = (short)  hb_parni(1,8) ;  // cy
    *p++ = (short)  0             ;  // Menu (ignored for now.)
    *p++ = (short)  0x00          ;  // Class also ignored

    if ( hb_parinfa(1,11) == HB_IT_STRING ) {
        nchar = nCopyAnsiToWideChar( p, TEXT( hb_parc(1,11) ) ) ;
        p += nchar   ;
      }
    else
      *p++ =0 ;

    // add in the wPointSize and szFontName here iff the DS_SETFONT bit on

    if ( (lStyle & DS_SETFONT ) ) {
      *p++ = (short) hb_parni(1,12) ;
      *p++ = (short) hb_parni(1,13) ;
      *p++ = (short) hb_parni(1,14) ;

      nchar = nCopyAnsiToWideChar( p, TEXT( hb_parc(1,15) ) ) ;
      p += nchar ;

    } ;

    //---------------
    // Now, for the items

   for ( i = 1 ; i <= nItems ; i++ ) {

      // make sure each item starts on a DWORD boundary
      p = lpwAlign (p) ;


      *p++ = LOWORD ( hb_parnl(2,i) ) ;    // help id
      *p++ = HIWORD ( hb_parnl(2,i) ) ;

      *p++ = LOWORD ( hb_parnl(3,i) ) ; // ext. style
      *p++ = HIWORD ( hb_parnl(3,i) ) ;

      *p++ = LOWORD ( hb_parnl(4,i) ) ; // style
      *p++ = HIWORD ( hb_parnl(4,i) ) ;

      *p++ = (short)  hb_parni(5,i)   ;  // x
      *p++ = (short)  hb_parni(6,i)   ;  // y
      *p++ = (short)  hb_parni(7,i)   ;  // cx
      *p++ = (short)  hb_parni(8,i)   ;  // cy

      *p++ = LOWORD ( hb_parnl(9,i) ) ;  // id
      *p++ = HIWORD ( hb_parnl(9,i) ) ;  // id   // 0;

      if ( hb_parinfa(10,i) == HB_IT_STRING ) {
          nchar = nCopyAnsiToWideChar(p, TEXT ( hb_parc(10,i)) ) ; // class
          p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parni(10,i) ;
         }

      if ( hb_parinfa(11,i) == HB_IT_STRING ) {
         nchar = nCopyAnsiToWideChar(p, (LPSTR) hb_parc(11,i) ) ;  // text
         p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parni(11,i) ;
         }


      *p++ = 0x00 ;  // extras ( in array 12 )


    } ;
    p = lpwAlign (p)  ;


    hb_retclen( (LPSTR) pdlgtemplate, ( (ULONG) p - (ULONG) pdlgtemplate ) ) ;

    LocalFree (LocalHandle (pdlgtemplate) ) ;

}

#pragma ENDDUMP
