/*
 * $Id$
 */

//----------------------------------------------------------------------//
//
//  hbwhat ErrorSys
//  A.J. Wos 08/06/2002
//  Scaled down and adapted for Harbour + hbwhat
//
//----------------------------------------------------------------------//
#include "common.ch"

#include "hbwhat.ch"
#include "winuser.ch"
#include "error.ch"
#include "debug.ch"

#xtranslate NTRIM( <n> ) => lTrim( Str( <n> ) )
//#define LOGFILE  error.log // don't use quotes
//----------------------------------------------------------------------//
PROCEDURE WHT_ErrorSys( )

   ErrorBlock( { | e | DefError( e ) } )

   RETURN
//----------------------------------------------------------------------//
STATIC FUNCTION DefError( e )

   LOCAL cMessage, aOptions, nChoice
   LOCAL cErr
   LOCAL cProcStack := ''
   LOCAL i

   IF e:genCode == EG_PRINT
      RETURN PrintError( )
   ENDIF
   IF ( e:genCode == EG_ZERODIV )
      RETURN ( 0 )
   ENDIF
   IF ( e:genCode == EG_OPEN .AND. e:osCode == 32 .AND. e:canDefault )
      NetErr( .T. )
      RETURN ( .F. ) // NOTE
   ENDIF
   IF ( e:genCode == EG_APPENDLOCK .AND. e:canDefault )
      NetErr( .T. )
      RETURN ( .F. ) // NOTE
   ENDIF

   i := 2
   DO WHILE ( ! Empty( ProcName( i ) ) )
      cProcStack += ( CRLF + ProcFile( i ) + "->" + ProcName( i ) + "(" + NTRIM( ProcLine( i ++ ) ) + ")" )
      IF ProcName( i ) == 'DEFERROR' // Oops, recursive arror, cannot continue !
         VWN_OutputDebugString( "" )
         VWN_OutputDebugString( "===============" + CRLF )
         VWN_OutputDebugString( "RECURSIVE ERROR" + CRLF )
         VWN_OutputDebugString( "===============" + CRLF )
         VWN_OutputDebugString( e:description + CHR( 13 ) + "Procedure Stack Depth:" + ntrim( getprocstack( ) ) + CRLF )
         VWN_OutputDebugString( cProcStack + CRLF )
         VWN_PostQuitMessage( 0 )
         Errorlevel( 1 )
         // quit
         RETURN( .F. )
      ENDIF
   ENDDO

   //OutputDebugString( cProcStack + CRLF )

   cErr := LogError( e, cProcStack )
   VWN_OutputDebugString( cErr )
   cMessage := ErrorMessage( e )

   aOptions := { "Quit" }
   IF ( e:canRetry )
      aAdd( aOptions, "Retry" )
   END
   IF ( e:canDefault )
      aAdd( aOptions, "Default" )
   END

   IF ( Empty( e:osCode ) )
      nChoice := eAlert( cMessage, aOptions, cErr )
   ELSE
      nChoice := eAlert( cMessage + ;
                         ";(OS Error " + NTRIM( e:osCode ) + ")", ;
                         aOptions, cErr )
   END

   IF ( ! Empty( nChoice ) )

      // do as instructed
      IF ( aOptions[ nChoice ] == "Break" )
         SET DELETED ON
         BREAK( e )
         RETURN( .F. )
      ELSEIF ( aOptions[ nChoice ] == "Retry" )
         RETURN ( .T. )
      ELSEIF ( aOptions[ nChoice ] == "Default" )
         SET DELETED ON
         RETURN ( .F. )
      END

   END

   VWN_PostQuitMessage( 0 )
   ErrorLevel( 1 )
   Quit
   //CLOSE ALL
   //PGMEXIT()

   RETURN ( .F. )
//----------------------------------------------------------------------//
STATIC FUNCTION ErrorMessage( e )
   LOCAL cMessage

   // start error message
   cMessage := iif( e:severity > ES_WARNING, "Error", "Warning" )

   // add error description if available
   IF ( ValType( e:description ) == "C" )
      cMessage += ';' + e:description
   END

   // add either filename or operation
   IF ( ! Empty( e:filename ) )
      cMessage += ( ';' + e:filename )
   ELSEIF ( ! Empty( e:operation ) )
      cMessage += ( ';' + e:operation )
   END

   // add subsystem name if available
   IF ( ValType( e:subsystem ) == "C" )
      cMessage += ';ERROR: ' + e:subsystem( ) + ' '
   ELSE
      cMessage += ";ERROR: ??? "
   END

   // add subsystem's error code if available
   IF ( ValType( e:subCode ) == "N" )
      cMessage += ( NTRIM( e:subCode ) )
   END
   cMessage += ';Called from ' + ProcFile(3) + "->" + procname( 3 ) + ' (' + AllTrim( Str( procline( 3 ) ) ) + '),  ' + ;
               + ProcFile(4) + "->" + procname( 4 ) + ' (' + AllTrim( Str( procline( 4 ) ) ) + ')'
   cMessage += ';Error logged in file '+ VWN_GetModuleFileName()+'\error.log'

   RETURN ( cMessage )
//----------------------------------------------------------------------//
STATIC FUNCTION LogError( e, cProcStack )
   LOCAL Args := ConvertArgs( e:args )
   LOCAL cErr := ''
   LOCAL dVer

   cErr += 'SYSTEM'
   cErr += ( CRLF + '------' )
   cErr += ( CRLF + 'Error date:' + dtoc( date( ) ) + ' time:' + time( ) )
   cErr += ( CRLF + 'Application: ' + VWN_GetModuleFileName( ) )
   cErr += ( CRLF + 'hbwhat library ver.' + WhatVersion( @dVer ) + ", " + DTOC( dVer ) )

   // T.B.D.:
   // add here  Windows version, memory info, diskspace info, free resources info
   // add computer name and operator name

   cErr += ( CRLF )
   cErr += ( CRLF + "ERROR INFORMATION" )
   cErr += ( CRLF + "-----------------" )
   cErr += ( CRLF + "Arguments     " + Args )
   cErr += ( CRLF + "Description   " + e:description )
   cErr += ( CRLF + "Filename      " + IfEmpty( e:filename ) )
   cErr += ( CRLF + "GenCode       " + gencodetext( e:genCode ) )
   cErr += ( CRLF + "Operation     " + IfEmpty( e:operation ) )
   cErr += ( CRLF + "Severity      " + NTRIM( e:severity ) )
   cErr += ( CRLF + "SubCode       " + NTRIM( e:subCode ) )
   cErr += ( CRLF + "SubSystem     " + e:subSystem )
   cErr += ( CRLF + "Tries         " + NTRIM( e:tries ) )
   cErr += ( CRLF + "Alias()       " + IfEmpty( ALIAS( ) ) )
   cErr += ( CRLF + "Open DBFs     " + ntrim( GetAliasCount( ) ) )
   cErr += ( CRLF + "DOS Error     " + DosErrCode( e ) )
   cErr += ( CRLF + "Windows Error " + NTRIM( GetLastError( ) ) )
   cErr += ( CRLF )
   cErr += ( CRLF )
   cErr += ( CRLF + "PROCEDURE STACK" )
   cErr += ( CRLF + "---------------" )

   cErr += cProcStack

   SET PRINTER TO  "error.log" ADDITIVE
   SET CONSOLE OFF
   SET PRINTER ON

   QOut( "        Please mail or fax this error report to:" )
   /*
   ? '             +---------------------------+'
   ? '             |  YOUR BUSINESS NAME HERE  |'
   ? '             |        P.O.Box 123        |'
   ? '             |Some Prestigeous Town, 1234|'
   ? '             |    Fax: (01) 1234 1234    |'
   */
   QOut( "             +---------------------------+" )
   QOut( cErr )

   QOut(Replicate( "=", 70 ))

   EJECT
   SET PRINTER OFF
   SET PRINTER TO
   SET CONSOLE ON

   RETURN cErr
//----------------------------------------------------------------------//
STATIC FUNCTION IfEmpty( Msg )
   LOCAL Ret_Val := "<none>"

   IF ! Empty( msg )
      Ret_Val := Left( msg, 68 )
   ENDIF

   RETURN Ret_Val
//----------------------------------------------------------------------//
STATIC PROCEDURE PrintError

   BREAK

   //RETURN
//----------------------------------------------------------------------//
STATIC FUNCTION ConvertArgs( a )
   LOCAL Ret_Val
   LOCAL x, cType
   LOCAL NumArgs := iif( ValType( a ) == "A", Len( a ) , iif( ValType( a ) == "C", ( a := { a } , 1 ) , 0 ) )

   IF NumArgs > 0
      Ret_Val := '{ '
      FOR x := 1 TO NumArgs
         cType := ValType( a[ x ] )
         DO CASE
         CASE cType == "C"
            Ret_Val += a[ x ]
         CASE cType == "N"
            Ret_Val += NTRIM( a[ x ] )
         CASE cType == "D"
            Ret_Val += dtoc( a[ x ] )
         CASE cType == "L"
            Ret_Val += iif( a[ x ] , ".T.", ".F." )
         CASE cType == "O"
            Ret_Val += a[ x ] :className + " Object"
         CASE cType == "U"
            Ret_Val += "NIL"
         ENDCASE
         //ÄÄÄÄÄ Next block added 1/8/92 To separate arguments
         IF x < NumArgs
            Ret_Val += ', '
         ENDIF
      NEXT
      Ret_Val += ' }'
   ENDIF

   RETURN ifempty( Ret_Val )
//----------------------------------------------------------------------//
STATIC FUNCTION GetAliasCount()
   LOCAL Counter
   LOCAL nCounter := 0

   FOR Counter := 1 TO 255
      IF !Empty( alias( Counter ) )
         nCounter++
      ENDIF
   NEXT

   RETURN( nCounter )
//----------------------------------------------------------------------//
STATIC FUNCTION GetProcStack( )
   LOCAL i := 2

   DO WHILE ! Empty( ProcName( i ) )
      i ++
   ENDDO

   RETURN( i - 3 )
//----------------------------------------------------------------------//
STATIC FUNCTION DosErrCode( e )
   LOCAL Msg

   IF e:osCode > 0
      Msg := NTRIM( e:osCode ) + ": " + Left( DosErrText( e:osCode ) , 37 )
   ELSE
      Msg := "(not an operating system error)"
   ENDIF

   RETURN Msg
//----------------------------------------------------------------------//
/*
 Function: DosErrText( )
 Author: Craig Yellick
 Purpose: Provide full description of DOS error code ( see table D - 1
 in the Clipper 5.0 "Programming & Utilities Guide" )
 Returns: character string
*/
STATIC FUNCTION DosErrText( n )

   LOCAL desc_ := { "Invalid function number"                 , ; // 1
                    "File not found"                          , ; // 2
                    "Path not found"                          , ; // 3
                    "Too many files open (no handles left)"   , ; // 4
                    "Access denied"                           , ; // 5
                    "Invalid handle"                          , ; // 6
                    "Memory control blocks destroyed (oh, my)", ; // 7
                    "Insufficient memory"                     , ; // 8
                    "Invalid memory block address"            , ; // 9
                    "Invalid environment"                     , ; // 10
                    "Invalid format"                          , ; // 11
                    "Invalid access code"                     , ; // 12
                    "Invalid data"                            , ; // 13
                    , ; // 14
                    "Invalid drive was specified"             , ; // 15
                    "Attempt to remove the current directory" , ; // 16
                    "Not same device"                         , ; // 17
                    "No more files"                           , ; // 18
                    "Attempt to write on write-protected diskette", ; // 19
                    "Unknown unit"                            , ; // 20
                    "Drive not ready"                         , ; // 21
                    "Unknown command"                         , ; // 22
                    "Data error (CRC)"                        , ; // 23
                    "Bad request structure length"            , ; // 24
                    "Seek error"                              , ; // 25
                    "Unknown media type"                      , ; // 26
                    "Sector not found"                        , ; // 27
                    "Printer out of paper"                    , ; // 28
                    "Write fault"                             , ; // 29
                    "Read fault"                              , ; // 30
                    "General failure"                         , ; // 31
                    "Sharing violation"                       , ; // 32
                    "Lock violation"                          , ; // 33
                    "Invalid disk change"                     , ; // 34
                    "FCB unavailable"                         , ; // 35
                    "Sharing buffer overflow"                 , ; // 36
                    , , , , , , , , , , , , ,                   ; // 37-49
                    "Network request not supported"           , ; // 50
                    "Remote computer not listening"           , ; // 51
                    "Duplicate name on network"               , ; // 52
                    "Network name not found"                  , ; // 53
                    "Network busy"                            , ; // 54
                    "Network device no longer exists"         , ; // 55
                    "Network BIOS command limit exceeded"     , ; // 56
                    "Network adapter hardware error"          , ; // 57
                    "Incorrect response from network"         , ; // 58
                    "Unexpected network error"                , ; // 59
                    "Incompatible remote adapter"             , ; // 60
                    "Print queue full"                        , ; // 61
                    "Not enough space for print file"         , ; // 62
                    "Print file deleted (not enough space)"   , ; // 63
                    "Network name deleted"                    , ; // 64
                    "Access denied"                           , ; // 65
                    "Network device type incorrect"           , ; // 66
                    "Network name not found"                  , ; // 67
                    "Network name limit exceeded"             , ; // 68
                    "Network BIOS session limit exceeded"     , ; // 69
                    "Temporarily paused"                      , ; // 70
                    "Network request not accepted"            , ; // 71
                    "Print or disk redirection paused"        , ; // 72
                    , , , , , , ,                               ; // 73-79
                    "File already exists"                     , ; // 80
                    , ; // 81
                    "Cannot make directory entry"             , ; // 82
                    "Fail on INT 24h"                         , ; // 83
                    "Too many redirections"                   , ; // 84
                    "Duplicate redirection"                   , ; // 85
                    "Invalid password"                        , ; // 86
                    "Invalid parameter"                       , ; // 87
                    "Network device fault"                    , ; // 88
                    ;
                    "Undefined or reserved error code!"         ; // +1
                  }

   /*
     Check that code number is within known upper limit,
     AND that a description is available For it.
   */
/*
   IF ( n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF
*/
   IF ( ( n < 1 ) .OR. n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF

   RETURN desc_[ n ]
//----------------------------------------------------------------------//
STATIC FUNCTION GenCodeText( n )

   LOCAL desc_ := { "EG_ARG", ; // 1
                    "EG_BOUND"               , ; // 2
                    "EG_STROVERFLOW"         , ; // 3
                    "EG_NUMOVERFLOW"         , ; // 4
                    "EG_ZERODIV"             , ; // 5
                    "EG_NUMERR"              , ; // 6
                    "EG_SYNTAX"              , ; // 7
                    "EG_COMPLEXITY"          , ; // 8
                    , ,                        ; // 9-10
                    "EG_MEM"                 , ; // 11
                    "EG_NOFUNC"              , ; // 12
                    "EG_NOMETHOD"            , ; // 13
                    "EG_NOVAR"               , ; // 14
                    "EG_NOALIAS"             , ; // 15
                    "EG_NOVARMETHOD"         , ; // 16
                    "EG_BADALIAS"            , ; // 17 (new w/ 5.01a)
                    "EG_DUPALIAS"            , ; // 18 (new w/ 5.01a)
                    ,                          ; // 19
                    "EG_CREATE"              , ; // 20
                    "EG_OPEN"                , ; // 21
                    "EG_CLOSE"               , ; // 22
                    "EG_READ"                , ; // 23
                    "EG_WRITE"               , ; // 24
                    "EG_PRINT"               , ; // 25
                    , , , ,                    ; // 26-29
                    "EG_UNSUPPORTED"         , ; // 30
                    "EG_LIMIT"               , ; // 31
                    "EG_CORRUPTION"          , ; // 32
                    "EG_DATATYPE"            , ; // 33
                    "EG_DATAWIDTH"           , ; // 34
                    "EG_NOTABLE"             , ; // 35
                    "EG_NOORDER"             , ; // 36
                    "EG_SHARED"              , ; // 37
                    "EG_UNLOCKED"            , ; // 38
                    "EG_READONLY"            , ; // 39
                    "EG_APPENDLOCK"          , ; // 40
                    ;
                    "Unknown or reserved"      ; // +1
                  }

   /*
     Check that code number is within known upper limit,
     AND that a description is available For it.
   */
/*
   IF ( n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF
*/
   IF ( ( n < 1 ) .OR. n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF

   RETURN NTRIM( n ) + ": " + desc_[ n ]
//----------------------------------------------------------------------//
STATIC FUNCTION eAlert( cMsg, aChoices, cDetail )

   LOCAL aDlg, i, n, aChoose, aMsg
   LOCAL hWnd, hDC
   LOCAL w , h, t := 0, cTitle, Msgh, ButWidth
   LOCAL crPos, txth
   LOCAL isDetail := .F.

   IF !ISCHARACTER( cMsg )
      cMsg := WHT_asString( cMsg )
   ENDIF
   cTitle := 'Alert'

   IF aChoices == NIL
      aChoices := { "&Ok" }
   ENDIF

   aAdd( achoices, "&Details >>" )

   cMsg := StrTran( cMsg, ";", CR )

   IF ( crPos := at( CR, cMsg ) ) > 0
      cTitle := Left( cMsg, crPos - 1 )
      cMsg := SubStr( cMsg, crPos + 1 )
   ENDIF

   hWnd := VWN_GetDesktopWindow( ) // default parent
   hDC  := VWN_GetDC( hWnd )

   //------------- total width without buttons

   w := VWN_GetTextExtentPoint32( hDC, AllTrim( cTitle ) ) [ 1 ]
   aMsg := WHT_str2a( cMsg, CR )
   AEVAL( aMsg, { | x | w := Max( w, VWN_GetTextExtentPoint32( hDC, AllTrim( x ) ) [ 1 ] ) } )
   w += 20

   //--------- total width of choices, also add "&" to the choices (if needed)

   n := Len( aChoices )
   aChoose := array( n )

   txth := 8 //ATM[TM_Height]
   Msgh := Len( aMsg ) * txth
   FOR i := 1 TO n
      ButWidth := Max( 20, VWN_GetTextExtentpoint32( hDC, aChoices[ i ] ) [ 1 ] + 6 )
      t := Max( t, ButWidth )
      aChoose[ i ] := iif( at( "&", aChoices[ i ] ) == 0, "&" + aChoices[ i ] , aChoices[ i ] )
   NEXT i

   VWN_ReleaseDC( , hDC )

   ButWidth := t / 2
   t *= ( n + 1 )
   w := Max( Max( w, t ) + 40, 500 ) // minimum dlg width
   h := Msgh + 33
   w /= 2

   //----------- create dialog

   aDlg := WHT_MakeDlgTemplate( cTitle, ;
                           WS_CAPTION + DS_MODALFRAME + WS_VISIBLE + 4 + WS_POPUP + DS_SETFONT , ;
                           0, 0, w, h, 8, 'MS Sans Serif' )

   FOR i := 1 TO n
      aDlg := WHT_AddDlgItem( aDlg, i, "BUTTON", ;
                         BS_PUSHBUTTON + WS_TABSTOP + WS_CHILD + WS_VISIBLE, ;
                         w - ( n - i ) * ( ButWidth + 5 ) - ButWidth - 5, h - 18, ButWidth, 14, ;
                         aChoose[ i ] )
   NEXT i

   aDlg := WHT_AddDlgItem( aDlg, - 1, "STATIC", ;
                      SS_CENTER + WS_CHILD + WS_VISIBLE, ;
                      10, 8, w - 20, Msgh, ;
                      cMsg )

   aDlg := WHT_AddDlgItem( aDlg, - 1, "BUTTON", ;
                      BS_GROUPBOX + WS_CHILD + WS_VISIBLE, ;
                      5, 1, w - 10, Msgh + 10, ;
                      "" )

   aDlg := WHT_AddDlgItem( aDlg, 101, "EDIT", ;
                      WS_CHILD + WS_VISIBLE + WS_BORDER + ES_MULTILINE + ES_READONLY + WS_VSCROLL + WS_TABSTOP, ;
                      5, h + 1, w - 10, 115, ;
                      cDetail )

   VWN_MessageBeep( MB_ICONHAND )
   i := WHT_DialogBox( , aDlg, hWnd, { | hDlg, nMsg, nwParam, nlParam | ;
                                  eAlertProc( hDlg, nMsg, nwParam, nlParam, @isDetail, hWnd, n ) } )
   VWN_SetFocus( hWnd )

   RETURN i
//----------------------------------------------------------------------//
STATIC FUNCTION eAlertProc( hDlg, nMsg, nwParam, nlParam, isDetail, hWnd, n )
   LOCAL aRect

   HB_SYMBOL_UNUSED( nlParam )

   DO CASE
   CASE nMsg == WM_INITDIALOG
      WHT_CenterWindow( hDlg, , , hWnd )
      WHT_SetOnTop( hDlg, HWND_TOPMOST )
      RETURN( 1 )

   CASE nMsg == WM_COMMAND
      IF 'Detail' $ VWN_GetDlgItemText( hDlg, nwParam )
         aRect := VWN_GetWindowRect( hDlg )
         IF isDetail
            VWN_SetDlgItemText( hDlg, nwParam, '&Detail >>' )
            VWN_MoveWindow( hDlg, aRect[ 1 ] , aRect[ 2 ] , aRect[ 3 ] - aRect[ 1 ] , aRect[ 4 ] - aRect[ 2 ] - 200, .T. )
            isDetail := .F.
         ELSE
            VWN_SetDlgItemText( hDlg, nwParam, '<< &Detail' )
            VWN_MoveWindow( hDlg, aRect[ 1 ] , aRect[ 2 ] , aRect[ 3 ] - aRect[ 1 ] , aRect[ 4 ] - aRect[ 2 ] + 200, .T. )
            isDetail := .T.
         ENDIF
      ELSE
         IF nwParam > 0 .AND. nwParam < n
            VWN_EndDialog( hDlg, nwParam )
         ENDIF
      ENDIF

   ENDCASE

   RETURN 0
//----------------------------------------------------------------------//
