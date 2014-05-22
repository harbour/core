/*
 * Harbour Project source code:
 * xHarbour default error handler and error functions:
 *    xhb_ErrorSys(), __BreakBlock(), __ErrorBlock(),
 *    __MinimalErrorHandler(), xhb_ErrorNew()
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Ron Pinkas <ron @ xHarbour.com>
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 *    TraceLog()
 * Copyright 2002 Luiz Rafael Culik <culikr@uol.com.br>
 *    strvalue()
 *    LogError()
 */

#include "error.ch"
#include "fileio.ch"

#include "hbver.ch"

STATIC s_cErrorLog := "error.log"
STATIC s_lErrorLogAppend := .T.

FUNCTION xhb_ErrorLog( cErrorLog, lErrorLogAppend )

   LOCAL aValueOld := { s_cErrorLog, s_lErrorLogAppend }

   IF HB_ISSTRING( cErrorLog )
      s_cErrorLog := cErrorLog
   ENDIF
   IF HB_ISLOGICAL( lErrorLogAppend )
      s_lErrorLogAppend := lErrorLogAppend
   ENDIF

   RETURN aValueOld

PROCEDURE xhb_ErrorSys()

   ErrorBlock( {| oError | xhb_DefError( oError ) } )

   RETURN

STATIC FUNCTION xhb_DefError( oError )

   LOCAL cMessage
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n

   n := 0
   DO WHILE ! Empty( ProcName( ++n ) )
      IF ProcName( n ) == ProcName()
         n := 3
         TraceLog( "Error system failure!", ProcLine( n ), ProcLine( n ), ProcFile( n ), oError:description )
         Alert( "Error system failure!;Please correct error handler:;" + ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) +  ") in module: " + ProcFile( n ) )
         ErrorLevel( 1 )
         QUIT
      ENDIF
   ENDDO

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oError:genCode == EG_LOCK .AND. oError:canRetry
      // oError:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Making sure we display the error info!
   DO WHILE DispCount() > 0
      DispEnd()
   ENDDO

   cMessage := ErrorMessage( oError )
   IF ! Empty( oError:osCode )
      cDOSError := "(DOS Error " + hb_ntos( oError:osCode ) + ")"
   ENDIF

   IF HB_ISARRAY( oError:Args )
      cMessage += " Arguments: (" + Arguments( oError ) + ")"
   ENDIF

   // Build buttons

   IF MaxCol() > 0
      aOptions := {}

      // AAdd( aOptions, "Break" )
      AAdd( aOptions, "Quit" )

      IF oError:canRetry
         AAdd( aOptions, "Retry" )
      ENDIF

      IF oError:canDefault
         AAdd( aOptions, "Default" )
      ENDIF

      // Show alert box
      // TraceLog( cMessage )

      DO WHILE ( nChoice := Alert( cMessage + ;
         iif( cDOSError == NIL, "", ";" + cDOSError ), aOptions ) ) == 0
      ENDDO

      IF ! Empty( nChoice )
         SWITCH aOptions[ nChoice ]
         CASE "Break"
            Break( oError )
         CASE "Retry"
            RETURN .T.
         CASE "Default"
            RETURN .F.
         ENDSWITCH
      ENDIF
   ELSE
      IF Empty( oError:osCode )
         Alert( cMessage + ";" + ProcLine( 3 ) + "(" + hb_ntos( ProcLine( 3 ) ) +  ") in module: " + ProcFile( 3 ) )
      ELSE
         Alert( cMessage + ";" + cDOSError + ";" + ProcLine( 3 ) + "(" + hb_ntos( ProcLine( 3 ) ) +  ") in module: " + ProcFile( 3 ) )
      ENDIF
   ENDIF

   // "Quit" selected

   IF ! Empty( oError:osCode )
      cMessage += " " + cDOSError
   ENDIF

   ? cMessage

   ?
   ? "Error at...:", ProcName() + "(" + hb_ntos( ProcLine() ) + ") in module:", ProcFile()
   n := 2
   DO WHILE ! Empty( ProcName( ++n ) )
      ? "Called from:", ProcName( n ) + ;
         "(" + hb_ntos( ProcLine( n ) ) + ") in module:", ProcFile( n )
   ENDDO

   /* For some strange reason, the DOS prompt gets written on the first line
      *of* the message instead of on the first line *after* the message after
      the program quits, unless the screen has scrolled. [dgh] */
   LogError( oError )

   ErrorLevel( 1 )
   ?
   QUIT

   RETURN .F.

STATIC FUNCTION ErrorMessage( oError )

   LOCAL cMessage

   // start error message
   cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF HB_ISSTRING( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF HB_ISNUMERIC( oError:subCode )
      cMessage += "/" + hb_ntos( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF HB_ISSTRING( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE ! Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE ! Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage

STATIC PROCEDURE LogError( oerr )

   LOCAL cLogFile   := s_cErrorLog        // error log file name
   LOCAL lAppendLog := s_lErrorLogAppend  // .F. create a new error log (default), .T. append to a existing one.

   LOCAL nHandle

   LOCAL nCount

   LOCAL nCols
   LOCAL nCol
   LOCAL cScreen
   LOCAL cOutString

   LOCAL cReport := ""

   AddLine( @cReport, PadC( " Harbour Error Log ", 80, "-" ) )
   AddLine( @cReport, "" )

   AddLine( @cReport, "Date, time ........: " + hb_TToC( hb_DateTime(), "yyyy-mm-dd" ) )
   AddLine( @cReport, "" )
   AddLine( @cReport, "Application name ..: " + hb_CmdArgArgV() )
   AddLine( @cReport, "Workstation name ..: " + NetName() )
   AddLine( @cReport, "Available memory ..: " + strvalue( Memory( 0 ) ) )
   AddLine( @cReport, "Current directory .: " + hb_cwd() )
   AddLine( @cReport, "Free disk space ...: " + strvalue( DiskSpace() ) )
   AddLine( @cReport, "" )
   AddLine( @cReport, "Operating system ..: " + OS() )
   AddLine( @cReport, "Harbour version ...: " + Version() )
   AddLine( @cReport, "Harbour built on ..: " + hb_Version( HB_VERSION_BUILD_DATE_STR ) )
   AddLine( @cReport, "C/C++ compiler ....: " + hb_Compiler() )
   AddLine( @cReport, "Multithreading ....: " + iif( hb_mtvm(), "Yes", "No" ) )
   AddLine( @cReport, "VM optimization ...: " + strvalue( hb_VMMode() ) )
   AddLine( @cReport, "" )
   AddLine( @cReport, "Current area ......: " + strvalue( Select() ) )

   AddLine( @cReport, "" )
   AddLine( @cReport, PadC( " SETs ", 80, "-" ) )
   AddLine( @cReport, "" )
   AddLine( @cReport, "SET ALTERNATE .....: " + strvalue( Set( _SET_ALTERNATE ), .T. ) )
   AddLine( @cReport, "SET ALTFILE .......: " + strvalue( Set( _SET_ALTFILE ) ) )
   AddLine( @cReport, "SET AUTOPEN .......: " + strvalue( Set( _SET_AUTOPEN ), .T. ) )
   AddLine( @cReport, "SET AUTORDER ......: " + strvalue( Set( _SET_AUTORDER ) ) )
   AddLine( @cReport, "SET AUTOSHARE .....: " + strvalue( Set( _SET_AUTOSHARE ) ) )
   AddLine( @cReport, "SET BELL ..........: " + strvalue( Set( _SET_BELL ), .T. ) )
   AddLine( @cReport, "SET BLINK .........: " + strvalue( SetBlink() ) )
   AddLine( @cReport, "SET CANCEL ........: " + strvalue( Set( _SET_CANCEL ), .T. ) )
   AddLine( @cReport, "SET CENTURY .......: " + strvalue( __SetCentury(), .T. ) )
   AddLine( @cReport, "SET CODEPAGE ......: " + strvalue( Set( _SET_CODEPAGE ) ) )
   AddLine( @cReport, "SET COLOR .........: " + strvalue( Set( _SET_COLOR ) ) )
   AddLine( @cReport, "SET CONFIRM .......: " + strvalue( Set( _SET_CONFIRM ), .T. ) )
   AddLine( @cReport, "SET CONSOLE .......: " + strvalue( Set( _SET_CONSOLE ), .T. ) )
   AddLine( @cReport, "SET COUNT .........: " + strvalue( Set( _SET_COUNT ) ) )
   AddLine( @cReport, "SET CURSOR ........: " + strvalue( Set( _SET_CURSOR ) ) )
   AddLine( @cReport, "SET DATE FORMAT ...: " + strvalue( Set( _SET_DATEFORMAT ) ) )
   AddLine( @cReport, "SET DBCODEPAGE ....: " + strvalue( Set( _SET_DBCODEPAGE ) ) )
   AddLine( @cReport, "SET DBFLOCKSCHEME .: " + strvalue( Set( _SET_DBFLOCKSCHEME ) ) )
   AddLine( @cReport, "SET DEBUG .........: " + strvalue( Set( _SET_DEBUG ), .T. ) )
   AddLine( @cReport, "SET DECIMALS ......: " + strvalue( Set( _SET_DECIMALS ) ) )
   AddLine( @cReport, "SET DEFAULT .......: " + strvalue( Set( _SET_DEFAULT ) ) )
   AddLine( @cReport, "SET DEFEXTENSIONS .: " + strvalue( Set( _SET_DEFEXTENSIONS ), .T. ) )
   AddLine( @cReport, "SET DELETED .......: " + strvalue( Set( _SET_DELETED ), .T. ) )
   AddLine( @cReport, "SET DELIMCHARS ....: " + strvalue( Set( _SET_DELIMCHARS ) ) )
   AddLine( @cReport, "SET DELIMETERS ....: " + strvalue( Set( _SET_DELIMITERS ), .T. ) )
   AddLine( @cReport, "SET DEVICE ........: " + strvalue( Set( _SET_DEVICE ) ) )
   AddLine( @cReport, "SET DIRCASE .......: " + strvalue( Set( _SET_DIRCASE ) ) )
   AddLine( @cReport, "SET DIRSEPARATOR ..: " + strvalue( Set( _SET_DIRSEPARATOR ) ) )
   AddLine( @cReport, "SET EOF ...........: " + strvalue( Set( _SET_EOF ), .T. ) )
   AddLine( @cReport, "SET EOL ...........: " + strvalue( hb_StrToHex( Set( _SET_EOL ) ) ) )
   AddLine( @cReport, "SET EPOCH .........: " + strvalue( Set( _SET_EPOCH ) ) )
   AddLine( @cReport, "SET ERRORLOG ......: " + strvalue( cLogFile ) + ", " + strvalue( lAppendLog ) )
   AddLine( @cReport, "SET ESCAPE ........: " + strvalue( Set( _SET_ESCAPE ), .T. ) )
   AddLine( @cReport, "SET EVENTMASK .....: " + strvalue( Set( _SET_EVENTMASK ) ) )
   AddLine( @cReport, "SET EXACT .........: " + strvalue( Set( _SET_EXACT ), .T. ) )
   AddLine( @cReport, "SET EXCLUSIVE .....: " + strvalue( Set( _SET_EXCLUSIVE ), .T. ) )
   AddLine( @cReport, "SET EXIT ..........: " + strvalue( Set( _SET_EXIT ), .T. ) )
   AddLine( @cReport, "SET EXTRA .........: " + strvalue( Set( _SET_EXTRA ), .T. ) )
   AddLine( @cReport, "SET EXTRAFILE .....: " + strvalue( Set( _SET_EXTRAFILE ) ) )
   AddLine( @cReport, "SET FILECASE ......: " + strvalue( Set( _SET_FILECASE ) ) )
   AddLine( @cReport, "SET FIXED .........: " + strvalue( Set( _SET_FIXED ), .T. ) )
   AddLine( @cReport, "SET FORCEOPT ......: " + strvalue( Set( _SET_FORCEOPT ), .T. ) )
   AddLine( @cReport, "SET HARDCOMMIT ....: " + strvalue( Set( _SET_HARDCOMMIT ), .T. ) )
   AddLine( @cReport, "SET HBOUTLOG ......: " + strvalue( Set( _SET_HBOUTLOG ) ) )
   AddLine( @cReport, "SET HBOUTLOGINFO ..: " + strvalue( Set( _SET_HBOUTLOGINFO ) ) )
   AddLine( @cReport, "SET IDLEREPEAT ....: " + strvalue( Set( _SET_IDLEREPEAT ), .T. ) )
   AddLine( @cReport, "SET INSERT ........: " + strvalue( Set( _SET_INSERT ), .T. ) )
   AddLine( @cReport, "SET INTENSITY .....: " + strvalue( Set( _SET_INTENSITY ), .T. ) )
   AddLine( @cReport, "SET LANGUAGE ......: " + strvalue( Set( _SET_LANGUAGE ) ) )
   AddLine( @cReport, "SET MARGIN ........: " + strvalue( Set( _SET_MARGIN ) ) )
   AddLine( @cReport, "SET MBLOCKSIZE ....: " + strvalue( Set( _SET_MBLOCKSIZE ) ) )
   AddLine( @cReport, "SET MCENTER .......: " + strvalue( Set( _SET_MCENTER ), .T. ) )
   AddLine( @cReport, "SET MESSAGE .......: " + strvalue( Set( _SET_MESSAGE ) ) )
   AddLine( @cReport, "SET MFILEEXT ......: " + strvalue( Set( _SET_MFILEEXT ) ) )
   AddLine( @cReport, "SET OPTIMIZE ......: " + strvalue( Set( _SET_OPTIMIZE ), .T. ) )
   AddLine( @cReport, "SET OSCODEPAGE ....: " + strvalue( Set( _SET_OSCODEPAGE ) ) )
   AddLine( @cReport, "SET PATH ..........: " + strvalue( Set( _SET_PATH ) ) )
   AddLine( @cReport, "SET PRINTER .......: " + strvalue( Set( _SET_PRINTER ), .T. ) )
   AddLine( @cReport, "SET PRINTFILE .....: " + strvalue( Set( _SET_PRINTFILE ) ) )
   AddLine( @cReport, "SET SCOREBOARD ....: " + strvalue( Set( _SET_SCOREBOARD ), .T. ) )
   AddLine( @cReport, "SET SCROLLBREAK ...: " + strvalue( Set( _SET_SCROLLBREAK ), .T. ) )
   AddLine( @cReport, "SET SOFTSEEK ......: " + strvalue( Set( _SET_SOFTSEEK ), .T. ) )
   AddLine( @cReport, "SET STRICTREAD ....: " + strvalue( Set( _SET_STRICTREAD ), .T. ) )
   AddLine( @cReport, "SET TIMEFORMAT ....: " + strvalue( Set( _SET_TIMEFORMAT ) ) )
   AddLine( @cReport, "SET TRIMFILENAME ..: " + strvalue( Set( _SET_TRIMFILENAME ) ) )
   AddLine( @cReport, "SET TYPEAHEAD .....: " + strvalue( Set( _SET_TYPEAHEAD ) ) )
   AddLine( @cReport, "SET UNIQUE ........: " + strvalue( Set( _SET_UNIQUE ), .T. ) )
   AddLine( @cReport, "SET VIDEOMODE .....: " + strvalue( Set( _SET_VIDEOMODE ) ) )
   AddLine( @cReport, "SET WRAP ..........: " + strvalue( Set( _SET_WRAP ), .T. ) )

   AddLine( @cReport, "" )
   AddLine( @cReport, PadC( " Detailed Work Area Items ", 80, "-" ) )
   AddLine( @cReport, "" )

   hb_WAEval( {||
      AddLine( @cReport, "Work area no ......: " + strvalue( Select() ) )
      AddLine( @cReport, "Alias .............: " + Alias() )
      AddLine( @cReport, "Current recno .....: " + strvalue( RecNo() ) )
      AddLine( @cReport, "Current filter ....: " + dbFilter() )
      AddLine( @cReport, "Relation exp. .....: " + dbRelation() )
      AddLine( @cReport, "Index order .......: " + strvalue( IndexOrd() ) )
      AddLine( @cReport, "Active key ........: " + strvalue( IndexKey( 0 ) ) )
      AddLine( @cReport, "" )
      RETURN .T.
      } )

   AddLine( @cReport, "" )
   AddLine( @cReport, PadC( " Internal Error Handling Information ", 80, "-" ) )
   AddLine( @cReport, "" )
   AddLine( @cReport, "Subsystem call ....: " + oErr:subsystem() )
   AddLine( @cReport, "System code .......: " + strvalue( oErr:subCode() ) )
   AddLine( @cReport, "Default status ....: " + strvalue( oerr:canDefault() ) )
   AddLine( @cReport, "Description .......: " + oErr:description() )
   AddLine( @cReport, "Operation .........: " + oErr:operation() )
   AddLine( @cReport, "Arguments .........: " + Arguments( oErr ) )
   AddLine( @cReport, "Involved file .....: " + oErr:filename() )
   AddLine( @cReport, "OS error code .....: " + strvalue( oErr:oscode() ) )
   IF hb_mtvm()
      AddLine( @cReport, "VM thread ID ......: " + strvalue( hb_threadID() ) )
   ENDIF

   AddLine( @cReport, "" )
   AddLine( @cReport, PadC( " Call Stack ", 80, "-" ) )
   AddLine( @cReport, "" )

   nCount := 3
   DO WHILE ! Empty( ProcName( ++nCount ) )
      AddLine( @cReport, PadR( ProcName( nCount ), 21 ) + " : " + Transform( ProcLine( nCount ), "999,999" ) + " in module: " + ProcFile( nCount ) )
   ENDDO

   AddLine( @cReport, "" )
   AddLine( @cReport, "" )

   IF MaxCol() > 0
      nCols := MaxCol()
      cScreen := SaveScreen()
      AddLine( @cReport, PadC( " Screen Dump ", nCols + 1 + 2, "#" ) )
      AddLine( @cReport, "" )
      AddLine( @cReport, "+" + Replicate( "-", nCols + 1 ) + "+" )
      FOR nCount := 0 TO MaxRow()
         cOutString := ""
         FOR nCol := 0 TO nCols
            cOutString += __XSaveGetChar( cScreen, nCount * ( nCols + 1 ) + nCol )
         NEXT
         AddLine( @cReport, "|" + cOutString + "|" )
      NEXT
      AddLine( @cReport, "+" + Replicate( "-", nCols + 1 ) + "+" )
   ELSE
      AddLine( @cReport, "Screen Dump Not Available" )
   ENDIF

   AddLine( @cReport, "" )
   AddLine( @cReport, "" )

   IF lAppendLog .AND. hb_FileExists( cLogFile )
      nHandle := FOpen( cLogFile, FO_WRITE )
   ELSE
      nHandle := FCreate( cLogFile )
   ENDIF

   IF nHandle == F_ERROR .AND. !( Lower( cLogFile ) == "error.log" )
      // Force creating error.log in case supplied log file cannot be created for any reason
      nHandle := FCreate( "error.log" )
   ENDIF

   IF nHandle != F_ERROR
      FSeek( nHandle, 0, FS_END )
      FWrite( nHandle, cReport )
      FClose( nHandle )
   ENDIF

   RETURN

STATIC FUNCTION strvalue( c, l )

   SWITCH ValType( c )
   CASE "C"
   CASE "M" ; RETURN c
   CASE "N" ; RETURN hb_ntos( c )
   CASE "D" ; RETURN DToC( c )
   CASE "L" ; RETURN iif( hb_defaultValue( l, .F. ), iif( c, "ON", "OFF" ), iif( c, ".T.", ".F." ) )
   ENDSWITCH

   RETURN ""

STATIC PROCEDURE AddLine( cReport, c )

   cReport += c + hb_eol()

   RETURN

STATIC FUNCTION Arguments( oErr )

   LOCAL xArg, cArguments := ""

   IF HB_ISARRAY( oErr:Args )
      FOR EACH xArg IN oErr:Args
         cArguments += "[" + hb_ntos( xArg:__enumIndex() ) + "] == Type: " + ValType( xArg )
         IF xArg != NIL
            cArguments += " Val: " + hb_ValToExp( xArg )
         ENDIF
         cArguments += " "
      NEXT
   ENDIF

   RETURN RTrim( cArguments )

FUNCTION __BreakBlock()
   RETURN {| e | Break( e ) }

FUNCTION __ErrorBlock()
   RETURN {| e | __MinimalErrorHandler( e ) }

PROCEDURE __MinimalErrorHandler( oError )

   LOCAL cError := "Error"

   IF HB_ISNUMERIC( oError:SubCode )
      cError += ": " + hb_ntos( oError:SubCode )
   ENDIF
   cError += "!" + hb_eol()

   IF HB_ISSTRING( oError:Operation )
      cError += "Operation: " + oError:Operation + hb_eol()
   ENDIF
   IF HB_ISSTRING( oError:Description )
      cError += "Description: " + oError:Description + hb_eol()
   ENDIF
   cError += "Source: " + ProcFile( 3 ) + hb_eol()
   cError += "Procedure: " + ProcName( 3 ) + hb_eol()
   cError += "Line: " + hb_ntos( ProcLine( 3 ) ) + hb_eol()

   OutStd( cError )

   QUIT

   RETURN

FUNCTION xhb_ErrorNew( cSubSystem, nGenCode, nSubCode, ;
      cOperation, cDescription, aArgs )

   LOCAL oError := ErrorNew()

   IF HB_ISSTRING( cSubSystem )
      oError:SubSystem := cSubSystem
   ENDIF
   IF HB_ISNUMERIC( nGenCode )
      oError:GenCode := nGenCode
   ENDIF
   IF HB_ISNUMERIC( nSubCode )
      oError:SubCode := nSubCode
   ENDIF
   IF HB_ISSTRING( cOperation )
      oError:Operation := cOperation
   ENDIF
   IF HB_ISSTRING( cDescription )
      oError:Description := cDescription
   ENDIF
   IF HB_ISARRAY( aArgs )
      oError:Args := aArgs
   ENDIF

   RETURN oError
