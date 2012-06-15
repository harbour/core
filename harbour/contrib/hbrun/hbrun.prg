/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    "DOt Prompt" Console and .prg/.hrb runner for the Harbour Language
 *
 * Copyright 2008-2012 Viktor Szakats (harbour syenar.net)
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "color.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "hbgtinfo.ch"
#include "hbhrb.ch"

/* NOTE: use hbextern library instead of #include "hbextern.ch"
 *       in dynamic builds it will greatly reduce the size because
 *       all function symbols will be registered by harbour shared
 *       library (.dll, .so, .sl, .dyn, ...) not by this code
 */

REQUEST __HB_EXTERN__

REQUEST HB_GT_CGI
REQUEST HB_GT_PCA
REQUEST HB_GT_STD
#if defined( __PLATFORM__WINDOWS )
REQUEST HB_GT_WVT
#endif

#define HB_HISTORY_LEN 500
#define HB_LINE_LEN    256

STATIC s_nRow
STATIC s_nCol := 0
STATIC s_aHistory := {}
STATIC s_lPreserveHistory := .T.
STATIC s_lWasLoad := .F.

STATIC s_cDirBase
STATIC s_cProgName

/* ********************************************************************** */

#if defined( __PLATFORM__DOS )
#  define _EXT_FILE_ "hb_ext.ini"
#else
#  define _EXT_FILE_ "hb_extension"
#endif
#define _EXT_ENV_  "HB_EXTENSION"

PROCEDURE _APPMAIN( cFile, ... )
   LOCAL cExt
   LOCAL hHeaders

   LOCAL aDynamic

   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   aDynamic := {}

   LoadExtDynamicFromFile( aDynamic, __hbrun_ConfigDir() + _EXT_FILE_ )
   LoadExtDynamicFromString( aDynamic, GetEnv( _EXT_ENV_ ) )

   /* TODO: Rework parameter handling */
   IF PCount() > 0
      SWITCH Lower( cFile )
      CASE "-?"
      CASE "-h"
      CASE "--help"
      CASE "/?"
      CASE "/h"
         hbrun_Usage()
         EXIT
      CASE "-v"
      CASE "/v"
         hbrun_Prompt( hb_AParams(), "? hb_version()" )
         EXIT
#if defined( __PLATFORM__WINDOWS )
      CASE "-r"
      CASE "-ra"
      CASE "/r"
      CASE "/ra"
         IF win_reg_self( .T., Right( Lower( cFile ), 1 ) == "a" )
            OutStd( "hbrun: Harbour Script File registered" + hb_eol() )
         ELSE
            OutErr( "hbrun: Error: Registering Harbour Script File" + hb_eol() )
         ENDIF
         EXIT
      CASE "-u"
      CASE "-ua"
      CASE "/u"
      CASE "/ua"
         IF win_reg_self( .F., Right( Lower( cFile ), 1 ) == "a" )
            OutStd( "hbrun: Harbour Script File unregistered" + hb_eol() )
         ELSE
            OutErr( "hbrun: Error: Unregistering Harbour Script File" + hb_eol() )
         ENDIF
         EXIT
#endif
      CASE "-p"
      CASE "/p"
         s_lPreserveHistory := .F.
         hbrun_extensionlist_init( aDynamic )
         hbrun_Prompt( hb_AParams() )
         EXIT
      OTHERWISE
         IF Left( cFile, 2 ) == "--"
            hbrun_extensionlist_init( aDynamic )
            hbrun_Prompt( hb_AParams() )
            EXIT
         ELSE
            cFile := __hbrun_FindInPath( cFile )
            IF ! Empty( cFile )
               hb_FNameSplit( cFile, NIL, NIL, @cExt )
               cExt := Lower( cExt )
               SWITCH cExt
                  CASE ".hb"
                  CASE ".prg"
                  CASE ".hrb"
                  CASE ".dbf"
                     EXIT
                  OTHERWISE
                     cExt := hbrun_FileSig( cFile )
               ENDSWITCH
               SWITCH cExt
                  CASE ".dbf"
                     hbrun_extensionlist_init( aDynamic )
                     hbrun_Prompt( hb_AParams(), "USE " + cFile + " SHARED" )
                     EXIT
                  CASE ".hb"
                  CASE ".prg"
                     IF Empty( GetEnv( "HBRUN_NOHEAD" ) )
                        hHeaders := __hbrun_CoreHeaderFiles() /* add core header files */
                     ENDIF

                     LoadExtDynamicFromSource( aDynamic, cFile )

                     cFile := hb_compileBuf( hHeaders, hb_ProgName(), "-n2", "-w", "-es2", "-q0", ;
                                             "-I" + hb_FNameDir( cFile ), "-D" + "__HBSCRIPT__HBSHELL", cFile )
                     IF cFile == NIL
                        ErrorLevel( 1 )
                        EXIT
                     ENDIF
                  OTHERWISE
                     hbrun_extensionlist_init( aDynamic )
                     s_cDirBase := hb_DirBase()
                     s_cProgName := hb_ProgName()
                     hb_argShift( .T. )
                     hb_hrbRun( cFile, ... )
                     EXIT
               ENDSWITCH
            ENDIF
         ENDIF
      ENDSWITCH
   ELSE
      hbrun_extensionlist_init( aDynamic )
      hbrun_Prompt( hb_AParams() )
   ENDIF

   RETURN

/* Public hbshell API */
FUNCTION hbshell_DirBase()
   RETURN s_cDirBase

FUNCTION hbshell_ProgName()
   RETURN s_cProgName

INIT PROCEDURE ClipInit()

   hb_cdpSelect( "UTF8EX" )

   RETURN

EXIT PROCEDURE hbrun_exit()

   hbrun_HistorySave()

   RETURN

FUNCTION __hbrun_ConfigDir()
   LOCAL cEnvVar
   LOCAL cDir

#if defined( __PLATFORM__WINDOWS )
   cEnvVar := "APPDATA"
#else
   cEnvVar := "HOME"
#endif

   IF ! Empty( GetEnv( cEnvVar ) )
#if defined( __PLATFORM__DOS )
      cDir := GetEnv( cEnvVar ) + hb_ps() + "~harbour"
#else
      cDir := GetEnv( cEnvVar ) + hb_ps() + ".harbour"
#endif
   ELSE
      cDir := hb_DirBase()
   ENDIF

   IF ! hb_DirExists( cDir )
      hb_DirCreate( cDir )
   ENDIF

   RETURN cDir + hb_ps()

STATIC PROCEDURE LoadExtDynamicFromFile( aDynamic, cFileName )
   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( StrTran( MemoRead( cFileName ), Chr( 13 ) ), Chr( 10 ) )
      IF "#" $ cItem
         cItem := Left( cItem, At( "#", cItem ) - 1 )
      ENDIF
      IF ! Empty( cItem )
         AAdd( aDynamic, cItem )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE LoadExtDynamicFromString( aDynamic, cString )
   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( cString,, .T. )
      IF ! Empty( cItem )
         AAdd( aDynamic, cItem )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE LoadExtDynamicFromSource( aDynamic, cFileName )
   LOCAL cFile := MemoRead( cFileName )
   LOCAL pRegex
   LOCAL tmp

   tmp := hb_cdpSelect( "EN" )
   pRegex := hb_regexComp( '(^|;)[[:blank:]]*#[[:blank:]]*(require)[[:blank:]]*(\".+?\"'+"|'.+?'"+')',;
      .F. /* lCaseSensitive */,;
      .T. /* lNewLine */ )
   hb_cdpSelect( tmp )

   FOR EACH tmp IN hb_regexAll( pRegex, cFile, ;
                                NIL /* lCaseSensitive */, ;
                                NIL /* lNewLine */, NIL, ;
                                NIL /* nGetMatch */, ;
                                .T. /* lOnlyMatch */ )
      AAdd( aDynamic, SubStr( ATail( tmp ), 2, Len( ATail( tmp ) ) - 2 ) /* Last group in match marker */ )
   NEXT

   RETURN

STATIC PROCEDURE hbrun_extensionlist_init( aDynamic )
   STATIC s_lInit := .F.

   IF ! s_lInit
      IF hb_IsFunction( "__hbrun_extensions_static_init" )
         Do( "__hbrun_extensions_static_init" )
      ENDIF
      __hbrun_extensions_dynamic_init( aDynamic )
      s_lInit := .T.
   ENDIF

   RETURN

STATIC FUNCTION hbrun_FileSig( cFile )
   LOCAL hFile
   LOCAL cBuff, cSig, cExt

   cExt := ".hb"
   hFile := FOpen( cFile, FO_READ )
   IF hFile != F_ERROR
      cSig := hb_hrbSignature()
      cBuff := Space( hb_BLen( cSig ) )
      FRead( hFile, @cBuff, hb_BLen( cBuff ) )
      FClose( hFile )
      IF cBuff == cSig
         cExt := ".hrb"
      ENDIF
   ENDIF

   RETURN cExt

#define _PLUGIN_hHRB                1
#define _PLUGIN_hMethods            2
#define _PLUGIN_ctx                 3
#define _PLUGIN_cID                 4
#define _PLUGIN_MAX_                4

STATIC FUNCTION plugins_load( hPlugins, aParams )
   LOCAL hConIO := {;
      "displine"  => {| c | hbrun_ToConsole( c ) } ,;
      "gethidden" => {|| hbrun_GetHidden() } }

   LOCAL plugin
   LOCAL plugins := {}
   LOCAL hHRBEntry
   LOCAL cFile

   FOR EACH cFile IN hPlugins

      plugin := Array( _PLUGIN_MAX_ )
      plugin[ _PLUGIN_hHRB ] := NIL

      SWITCH Lower( hb_FNameExt( cFile:__enumKey() ) )
      CASE ".hb"
      CASE ".prg"
         cFile := hb_compileFromBuf( cFile, __hbrun_CoreHeaderFiles(), hb_ProgName(), "-n2", "-w", "-es2", "-q0" )
         IF cFile == NIL
            EXIT
         ENDIF
      CASE ".hrb"
         BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
            plugin[ _PLUGIN_hHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
            IF Empty( hHRBEntry := hb_hrbGetFunSym( plugin[ _PLUGIN_hHRB ], "__hbrun_plugin" ) )
               plugin[ _PLUGIN_hHRB ] := NIL
            ENDIF
         RECOVER
            plugin[ _PLUGIN_hHRB ] := NIL
         END SEQUENCE
         EXIT
      ENDSWITCH

      IF ! Empty( plugin[ _PLUGIN_hHRB ] )
         plugin[ _PLUGIN_hMethods ] := Do( hHRBEntry )
         IF ! Empty( plugin[ _PLUGIN_hMethods ] )
            plugin[ _PLUGIN_ctx ] := Eval( plugin[ _PLUGIN_hMethods ][ "init" ], hConIO, aParams )
            IF ! Empty( plugin[ _PLUGIN_ctx ] )
               plugin[ _PLUGIN_cID ] := plugin[ _PLUGIN_hMethods ][ "id" ]
               IF ! Empty( plugin[ _PLUGIN_cID ] )
                  AAdd( plugins, plugin )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN plugins

STATIC FUNCTION plugins_command( plugins, cCommand, cDomain )
   LOCAL plugin

   FOR EACH plugin IN plugins
      IF Left( cCommand, Len( plugin[ _PLUGIN_cID ] ) + 1 ) == plugin[ _PLUGIN_cID ] + "."
         IF Eval( plugin[ _PLUGIN_hMethods ][ "cmd" ], plugin[ _PLUGIN_ctx ], SubStr( cCommand, Len( plugin[ _PLUGIN_cID ] ) + 2 ) )
            RETURN .T.
         ENDIF
      ELSEIF cDomain == plugin[ _PLUGIN_cID ]
         IF Eval( plugin[ _PLUGIN_hMethods ][ "cmd" ], plugin[ _PLUGIN_ctx ], cCommand )
            RETURN .T.
         ENDIF
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION plugins_valid_id( plugins, cID )
   LOCAL plugin

   FOR EACH plugin IN plugins
      IF plugin[ _PLUGIN_cID ] == cID
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION plugins_valid_id_list( plugins )
   LOCAL plugin
   LOCAL aList := {}

   FOR EACH plugin IN plugins
      AAdd( aList, plugin[ _PLUGIN_cID ] )
   NEXT

   RETURN aList

STATIC PROCEDURE plugins_unload( plugins )
   LOCAL plugin

   FOR EACH plugin IN plugins
      Eval( plugin[ _PLUGIN_hMethods ][ "exit" ], plugin[ _PLUGIN_ctx ] )
   NEXT

   RETURN

STATIC PROCEDURE hbrun_Prompt( aParams, cCommand )
   LOCAL GetList
   LOCAL cLine
   LOCAL nMaxRow, nMaxCol
   LOCAL nHistIndex
   LOCAL bKeyUP, bKeyDown, bKeyIns, bKeyResize
   LOCAL lResize := .F.
   LOCAL plugins

   LOCAL cDomain := ""
   LOCAL tmp

   IF ! hb_gtInfo( HB_GTI_ISSCREENPOS )
      OutErr( "hbrun: Error: Interactive session not possible with " + hb_gtVersion( 0 ) + " terminal driver" + hb_eol() )
      RETURN
   ENDIF

   hb_gtInfo( HB_GTI_ICONRES, 1 )

   CLEAR SCREEN
   SET SCOREBOARD OFF
   GetList := {}

   hbrun_HistoryLoad()

   AAdd( s_aHistory, PadR( "quit", HB_LINE_LEN ) )
   nHistIndex := Len( s_aHistory ) + 1

   IF HB_ISSTRING( cCommand )
      AAdd( s_aHistory, PadR( cCommand, HB_LINE_LEN ) )
      hbrun_Info( cCommand )
      hbrun_Exec( cCommand )
   ELSE
      cCommand := ""
   ENDIF

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )

   s_nRow := 2 + iif( Empty( __hbrun_extensions_get_list() ), 0, 1 )

   plugins := plugins_load( __hbrun_plugins(), aParams )

   DO WHILE .T.

      IF cLine == NIL
         cLine := Space( HB_LINE_LEN )
      ENDIF

      hbrun_Info( cCommand )

      nMaxRow := MaxRow()
      nMaxCol := MaxCol()
      @ nMaxRow, 0 SAY cDomain + "."
      @ nMaxRow, Col() GET cLine ;
                       PICTURE "@KS" + hb_ntos( nMaxCol - Col() + 1 )

      SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

      bKeyIns  := SetKey( K_INS, ;
         {|| SetCursor( iif( ReadInsert( !ReadInsert() ), ;
                          SC_NORMAL, SC_INSERT ) ) } )
      bKeyUp   := SetKey( K_UP, ;
         {|| iif( nHistIndex > 1, ;
                  cLine := s_aHistory[ --nHistIndex ], ) } )
      bKeyDown := SetKey( K_DOWN, ;
         {|| cLine := iif( nHistIndex < Len( s_aHistory ), ;
             s_aHistory[ ++nHistIndex ], ;
             ( nHistIndex := Len( s_aHistory ) + 1, Space( HB_LINE_LEN ) ) ) } )
      bKeyResize := SetKey( HB_K_RESIZE,;
         {|| lResize := .T., hb_KeyPut( K_ENTER ) } )

      READ

      SetKey( K_DOWN, bKeyDown )
      SetKey( K_UP, bKeyUp )
      SetKey( K_INS, bKeyIns )
      SetKey( HB_K_RESIZE, bKeyResize )

      IF LastKey() == K_ESC .OR. Empty( cLine ) .OR. ;
         ( lResize .AND. LastKey() == K_ENTER )
         IF lResize
            lResize := .F.
         ELSE
            cLine := NIL
         ENDIF
         IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
            @ nMaxRow, 0 CLEAR
         ENDIF
         LOOP
      ENDIF

      IF Empty( s_aHistory ) .OR. ! ATail( s_aHistory ) == cLine
         IF Len( s_aHistory ) < HB_HISTORY_LEN
            AAdd( s_aHistory, cLine )
         ELSE
            ADel( s_aHistory, 1 )
            s_aHistory[ Len( s_aHistory ) ] := cLine
         ENDIF
      ENDIF
      nHistIndex := Len( s_aHistory ) + 1

      cCommand := AllTrim( cLine, " " )
      cLine := NIL
      @ nMaxRow, 0 CLEAR
      hbrun_Info( cCommand )

      IF ! Empty( cCommand )

         IF Left( cCommand, 1 ) == "."
            IF cCommand == "."
               cDomain := ""
            ELSEIF plugins_valid_id( plugins, SubStr( cCommand, 2 ) )
               cDomain := SubStr( cCommand, 2 )
            ELSE
               FOR EACH tmp IN plugins_valid_id_list( plugins )
                  hbrun_ToConsole( "." + tmp )
               NEXT
            ENDIF
         ELSE
            IF ! plugins_command( plugins, cCommand, cDomain )
               hbrun_Exec( cCommand )
            ENDIF

            IF s_nRow >= MaxRow()
               Scroll( 2 + iif( Empty( __hbrun_extensions_get_list() ), 0, 1 ), 0, MaxRow(), MaxCol(), 1 )
               s_nRow := MaxRow() - 1
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   plugins_unload( plugins )

   RETURN

/* ********************************************************************** */

STATIC PROCEDURE hbrun_ToConsole( cText )
   QQOut( cText + hb_eol() )
   RETURN

STATIC FUNCTION hbrun_GetHidden()
   LOCAL GetList := {}
   LOCAL cPassword := Space( 128 )
   LOCAL nSavedRow
   LOCAL bKeyPaste

   QQOut( "Enter password: " )

   nSavedRow := Row()

   AAdd( GetList, hb_Get():New( Row(), Col(), {| v | iif( PCount() == 0, cPassword, cPassword := v ) }, "cPassword", "@S" + hb_ntos( MaxCol() - Col() + 1 ), hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD ) ) )
   ATail( GetList ):hideInput( .T. )
   ATail( GetList ):postBlock := {|| ! Empty( cPassword ) }
   ATail( GetList ):display()

   SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
   bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

   READ

   /* Positions the cursor on the line previously saved */
   SetPos( nSavedRow, MaxCol() - 1 )
   SetKey( K_ALT_V, bKeyPaste )

   QQOut( hb_eol() )

   RETURN AllTrim( cPassword )

/* ********************************************************************** */

STATIC PROCEDURE hbrun_Usage()

   OutStd( 'Harbour "DOt Prompt" Console / runner ' + HBRawVersion() + hb_eol() +;
           "Copyright (c) 1999-2012, Przemyslaw Czerpak, Viktor Szakats" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
           hb_eol() +;
           "Syntax:  hbrun [<file[.hb|.prg|.hrb]> [<parameters,...>]]" + hb_eol() )

   RETURN

/* ********************************************************************** */

STATIC PROCEDURE hbrun_Info( cCommand )

   IF cCommand != NIL
      hb_DispOutAt( 0, 0, "PP: " )
      hb_DispOutAt( 0, 4, PadR( cCommand, MaxCol() - 3 ), "N/R" )
   ENDIF
   IF Used()
      hb_DispOutAt( 1, 0, ;
         PadR( "RDD: " + PadR( rddName(), 6 ) + ;
               " | Area:" + Str( Select(), 3 ) + ;
               " | Dbf: " + PadR( Alias(), 10 ) + ;
               " | Index: " + PadR( ordName( IndexOrd() ), 8 ) + ;
               " | # " + Str( RecNo(), 7 ) + "/" + Str( RecCount(), 7 ), ;
               MaxCol() + 1 ), "N/BG" )
   ELSE
      hb_DispOutAt( 1, 0, ;
         PadR( "RDD: " + Space( 6 ) + ;
               " | Area:" + Space( 3 ) + ;
               " | Dbf: " + Space( 10 ) + ;
               " | Index: " + Space( 8 ) + ;
               " | # " + Space( 7 ) + "/" + Space( 7 ), ;
               MaxCol() + 1 ), "N/BG" )
   ENDIF
   IF s_lPreserveHistory
      hb_DispOutAt( 1, MaxCol(), "o", "R/BG" )
   ENDIF

   hb_DispOutAt( 2, 0, PadR( "Ext: " + ArrayToList( __hbrun_extensions_get_list() ), MaxCol() + 1 ), "W/B" )

   RETURN

STATIC FUNCTION ArrayToList( array )
   LOCAL cString := ""
   LOCAL tmp

   FOR tmp := 1 TO Len( array )
      cString += array[ tmp ]
      IF tmp < Len( array )
         cString += ", "
      ENDIF
   NEXT

   RETURN cString

/* ********************************************************************** */

STATIC PROCEDURE hbrun_Err( oErr, cCommand )

   LOCAL xArg, cMessage

   cMessage := "Sorry, could not execute:;;" + cCommand + ";;"
   IF oErr:ClassName == "ERROR"
      cMessage += oErr:Description
      IF !Empty( oErr:Operation )
         cMessage += " " + oErr:Operation
      ENDIF
      IF HB_ISARRAY( oErr:Args ) .AND. Len( oErr:Args ) > 0
         cMessage += ";Arguments:"
         FOR EACH xArg IN oErr:Args
            cMessage += ";" + hb_CStr( xArg )
         NEXT
      ENDIF
   ELSEIF HB_ISSTRING( oErr )
      cMessage += oErr
   ENDIF
   cMessage += ";;" + ProcName( 2 ) + "(" + hb_ntos( ProcLine( 2 ) ) + ")"

   Alert( cMessage )

   Break( oErr )

/* ********************************************************************** */

STATIC PROCEDURE hbrun_Exec( cCommand )
   LOCAL pHRB, cHRB, cFunc, bBlock, cEol, nRowMin

   cEol := hb_eol()
   cFunc := "STATIC FUNCTION __HBDOT()" + cEol + ;
            "RETURN {||" + cEol + ;
            "   " + cCommand + cEol + ;
            "   RETURN __MVSETBASE()" + cEol + ;
            "}" + cEol

   BEGIN SEQUENCE WITH {|oErr| hbrun_Err( oErr, cCommand ) }

      cHRB := hb_compileFromBuf( cFunc, hb_ProgName(), "-n2", "-q2" )
      IF cHRB == NIL
         Eval( ErrorBlock(), "Syntax error." )
      ELSE
         pHRB := hb_hrbLoad( cHRB )
         IF pHrb != NIL
            bBlock := hb_hrbDo( pHRB )
            DevPos( s_nRow, s_nCol )
            Eval( bBlock )
            s_nRow := Row()
            s_nCol := Col()
            nRowMin := 2 + iif( Empty( __hbrun_extensions_get_list() ), 0, 1 )
            IF s_nRow < nRowMin
               s_nRow := nRowMin
            ENDIF
         ENDIF
      ENDIF

   ENDSEQUENCE

   __mvSetBase()

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

/* ********************************************************************** */

#define _HISTORY_DISABLE_LINE "no"

#if defined( __PLATFORM__DOS )
#  define _FNAME_HISTORY_ "hbhist.ini"
#else
#  define _FNAME_HISTORY_ ".hb_history"
#endif

STATIC PROCEDURE hbrun_HistoryLoad()
   LOCAL cHistory
   LOCAL cLine

   s_lWasLoad := .T.

   IF s_lPreserveHistory
      cHistory := StrTran( MemoRead( __hbrun_ConfigDir() + _FNAME_HISTORY_ ), Chr( 13 ) )
      IF Left( cHistory, Len( _HISTORY_DISABLE_LINE + Chr( 10 ) ) ) == _HISTORY_DISABLE_LINE + Chr( 10 )
         s_lPreserveHistory := .F.
      ELSE
         FOR EACH cLine IN hb_ATokens( StrTran( cHistory, Chr( 13 ) ), Chr( 10 ) )
            IF ! Empty( cLine )
               AAdd( s_aHistory, PadR( cLine, HB_LINE_LEN ) )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE hbrun_HistorySave()
   LOCAL cHistory
   LOCAL cLine
   LOCAL cDir

   IF s_lWasLoad .AND. s_lPreserveHistory
      cHistory := ""
      FOR EACH cLine IN s_aHistory
         IF !( Lower( AllTrim( cLine ) ) == "quit" )
            cHistory += AllTrim( cLine ) + hb_eol()
         ENDIF
      NEXT
      IF ! hb_DirExists( cDir := __hbrun_ConfigDir() )
         hb_DirCreate( cDir )
      ENDIF
      hb_MemoWrit( cDir + _FNAME_HISTORY_, cHistory )
   ENDIF

   RETURN

FUNCTION __hbrun_FindInPath( cFileName, xPath )
   LOCAL cDir
   LOCAL cName
   LOCAL cExt
   LOCAL cFullName
   LOCAL aExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
   aExt := iif( Empty( cExt ), { ".hb", ".hrb" }, { cExt } )

   FOR EACH cExt IN aExt
      /* Check original filename (in supplied path or current dir) */
      IF hb_FileExists( cFullName := hb_FNameMerge( cDir, cName, cExt ) )
         RETURN cFullName
      ENDIF
   NEXT

   IF Empty( cDir )
      IF ! Empty( cDir := hb_DirBase() )
         /* Check in the dir of this executable. */
         FOR EACH cExt IN aExt
            IF hb_FileExists( cFullName := hb_FNameMerge( cDir, cName, cExt ) )
               RETURN cFullName
            ENDIF
         NEXT
      ENDIF

      IF ! HB_ISSTRING( xPath ) .AND. ;
         ! HB_ISARRAY( xPath )
         xPath := GetEnv( "PATH" )
      ENDIF

      IF HB_ISSTRING( xPath )
         #if defined( __PLATFORM__WINDOWS ) .OR. ;
             defined( __PLATFORM__DOS ) .OR. ;
             defined( __PLATFORM__OS2 )
            xPath := hb_ATokens( xPath, hb_osPathListSeparator(), .T., .T. )
         #else
            xPath := hb_ATokens( xPath, hb_osPathListSeparator() )
         #endif
      ENDIF

      FOR EACH cExt IN aExt
         /* Check in the PATH. */
         FOR EACH cDir IN xPath
            IF Left( cDir, 1 ) == '"' .AND. Right( cDir, 1 ) == '"'
               cDir := SubStr( cDir, 2, Len( cDir ) - 2 )
            ENDIF
            IF ! Empty( cDir )
               IF hb_FileExists( cFullName := hb_FNameMerge( cDir, cName, cExt ) )
                  RETURN cFullName
               ENDIF
            ENDIF
         NEXT
      NEXT
   ENDIF

   RETURN NIL

#if defined( __PLATFORM__WINDOWS )

STATIC FUNCTION win_reg_self( lRegister, lAllUser )
   RETURN win_reg_app( lRegister, lAllUser, hb_ProgName() )

STATIC FUNCTION win_reg_app( lRegister, lAllUser, cAppPath )
   LOCAL cHive := iif( HB_ISLOGICAL( lAllUser ) .AND. lAllUser, "HKEY_CLASSES_ROOT", "HKEY_CURRENT_USER\Software\Classes" )
   LOCAL lSuccess := .T.
   LOCAL tmp

   LOCAL aEntries := {;
      cHive + '\'                                , ""                     ,;
      cHive + '\.hb\'                            , "HarbourScript"        ,;
      cHive + '\HarbourScript\'                  , "Harbour Script File"  ,;
      cHive + '\HarbourScript\DefaultIcon\'      , cAppPath + ",-1"       ,;
      cHive + '\HarbourScript\Shell\'            , "Run"                  ,;
      cHive + '\HarbourScript\Shell\Run\'        , ""                     ,;
      cHive + '\HarbourScript\Shell\Run\Command\', cAppPath + ' "%1"'     }

   IF lRegister
      FOR tmp := 1 TO Len( aEntries ) STEP 2
         lSuccess := lSuccess .AND. win_regWrite( aEntries[ tmp ], aEntries[ tmp + 1 ] )
      NEXT
   ELSE
      FOR tmp := Len( aEntries ) - 1 TO 3 STEP -2
         lSuccess := win_regDelete( aEntries[ tmp ] )
      NEXT
   ENDIF

   RETURN lSuccess

#endif
