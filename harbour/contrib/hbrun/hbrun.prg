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

PROCEDURE _APPMAIN( cFile, ... )
   LOCAL cExt
   LOCAL hHeaders

   LOCAL aDynamic

   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hbmk_auto_cp_term()
   hbmk_auto_cp_os()

   aDynamic := {}

   LoadExtDynamicFromFile( aDynamic, hb_DirBase() + "hbrun.ext" )
   LoadExtDynamicFromString( aDynamic, GetEnv( "HBRUN_EXT" ) )

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
                  CASE ".prg"
                  CASE ".hb"
                  CASE ".hbs"
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
                  CASE ".prg"
                  CASE ".hb"
                  CASE ".hbs"
                     IF Empty( GetEnv( "HBRUN_NOHEAD" ) )
                        hHeaders := __hbrun_CoreHeaderFiles() /* add core header files */
                     ENDIF

                     LoadExtDynamicFromSource( aDynamic, cFile )

                     cFile := hb_compileBuf( hHeaders, hb_ProgName(), "-n2", "-w", "-es2", "-q0", ;
                                             "-I" + hb_FNameDir( cFile ), "-D" + "__HBSCRIPT__HBRUN", cFile )
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

/* Public hbrun API */
FUNCTION hbrun_DirBase()
   RETURN s_cDirBase

/* Public hbrun API */
FUNCTION hbrun_ProgName()
   RETURN s_cProgName

INIT PROCEDURE ClipInit()

   hb_cdpSelect( "UTF8EX" )

   RETURN

EXIT PROCEDURE hbrun_exit()

   hbrun_HistorySave()

   RETURN

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
      IF Type( "__hbrun_extensions_static_init()" ) == "UI"
         Do( "__hbrun_extensions_static_init" )
      ENDIF
      __hbrun_extensions_dynamic_init( aDynamic )
      s_lInit := .T.
   ENDIF

   RETURN

STATIC FUNCTION hbrun_FileSig( cFile )
   LOCAL hFile
   LOCAL cBuff, cSig, cExt

   cExt := ".prg"
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
      CASE ".hbs"
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
           "Syntax:  hbrun [<file[.prg|.hb|.hbs|.hrb]> [<parameters,...>]]" + hb_eol() )

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
   IF ! Empty( __hbrun_extensions_get_list() )
      hb_DispOutAt( 2, 0, PadR( "Ext: " + ArrayToList( __hbrun_extensions_get_list() ), MaxCol() + 1 ), "W/B" )
   ENDIF

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
   cFunc := "STATIC FUNC __HBDOT()" + cEol + ;
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

STATIC PROCEDURE hbrun_HistoryLoad()
   LOCAL cHistory
   LOCAL cLine

   s_lWasLoad := .T.

   IF s_lPreserveHistory
      cHistory := StrTran( MemoRead( hbrun_HistoryFileName() ), Chr( 13 ) )
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

   IF s_lWasLoad .AND. s_lPreserveHistory
      cHistory := ""
      FOR EACH cLine IN s_aHistory
         IF !( Lower( AllTrim( cLine ) ) == "quit" )
            cHistory += AllTrim( cLine ) + hb_eol()
         ENDIF
      NEXT
      hb_MemoWrit( hbrun_HistoryFileName(), cHistory )
   ENDIF

   RETURN

STATIC FUNCTION hbrun_HistoryFileName()
   LOCAL cEnvVar
   LOCAL cDir
   LOCAL cFileName

#if defined( __PLATFORM__WINDOWS )
   cEnvVar := "APPDATA"
#else
   cEnvVar := "HOME"
#endif

#if defined( __PLATFORM__DOS )
   cFileName := "hbrunhst.ini"
#else
   cFileName := ".hbrun_history"
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

   RETURN cDir + hb_ps() + cFileName

FUNCTION __hbrun_FindInPath( cFileName, xPath )
   LOCAL cDir
   LOCAL cName
   LOCAL cExt
   LOCAL cFullName
   LOCAL aExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
   aExt := iif( Empty( cExt ), { ".hb", ".hbs", ".hrb" }, { cExt } )

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
      cHive + '\.hbs\'                           , "HarbourScript"        ,;
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

STATIC FUNCTION hbmk_auto_cp_term()
   LOCAL cCP
   LOCAL cLang

   #if defined( __PLATFORM__WINDOWS )
      cCP := __CPWinToCPStd( wapi_GetOEMCP() )
      cLang := hb_UserLang()
   #elif defined( __PLATFORM__UNIX )
      IF ! Empty( GetEnv( "LANG" ) )
         __UnixParseLangCP( GetEnv( "LANG" ), @cCP, @cLang )
      ELSE
         __UnixParseLangCP( GetEnv( "LC_CTYPE" ), @cCP, @cLang )
      ENDIF
   #elif defined( __PLATFORM__DOS )
      /* TODO */
   #elif defined( __PLATFORM__OS2 )
      /* TODO */
   #endif

   IF ! Empty( cCP := __CPStdToHarbour( cCP, cLang ) )
      hb_SetTermCP( cCP )
   ENDIF

   RETURN cCP

STATIC FUNCTION hbmk_auto_cp_os()
   LOCAL cCP
   LOCAL cLang

   #if defined( __PLATFORM__WINDOWS )
      cCP := __CPWinToCPStd( wapi_GetACP() )
      cLang := hb_UserLang()
   #elif defined( __PLATFORM__UNIX )
      IF ! Empty( GetEnv( "LANG" ) )
         __UnixParseLangCP( GetEnv( "LANG" ), @cCP, @cLang )
      ELSE
         __UnixParseLangCP( GetEnv( "LC_CTYPE" ), @cCP, @cLang )
      ENDIF
   #elif defined( __PLATFORM__DOS )
      /* TODO */
   #elif defined( __PLATFORM__OS2 )
      /* TODO */
   #endif

   IF ! Empty( cCP := __CPStdToHarbour( cCP, cLang ) )
      Set( _SET_OSCODEPAGE, cCP )
   ENDIF

   RETURN cCP

/* language[_territory][.codeset] */
/* [language[_territory][.codeset][@modifier]] */
/* TODO: handle "C"/"POSIX" values and values starting with "/" */
STATIC FUNCTION __UnixParseLangCP( cString, /* @ */ cCP, /* @ */ cLang )
   LOCAL tmp

   IF ( tmp := At( ".", cString ) ) > 0
      cLang := Left( cString, tmp - 1 )
      cCP := SubStr( cString, tmp + 1 )
      IF ( tmp := At( "@", cString ) ) > 0
         cCP := Left( cString, tmp - 1 )
      ENDIF
   ELSE
      cLang := cString
      cCP := "UTF-8"
   ENDIF

   RETURN NIL

STATIC FUNCTION __CPUnixToCPStd( cCPUnix )

   IF HB_ISSTRING( cCPUnix )

      cCPUnix := StrTran( cCPUnix, "_" )
      cCPUnix := StrTran( cCPUnix, "-" )

      /* TOFIX: update the list of std unix cp names */
      SWITCH Lower( cCPUnix )
      CASE "ibm437"
      CASE "cp437"       ; RETURN "cp437"
      CASE "ibm737"
      CASE "cp737"       ; RETURN "cp737"
      CASE "ibm775"
      CASE "cp775"       ; RETURN "cp775"
      CASE "ibm850"
      CASE "cp850"       ; RETURN "cp850"
      CASE "ibm852"
      CASE "cp852"       ; RETURN "cp852"
      CASE "ibm857"
      CASE "cp857"       ; RETURN "cp857"
      CASE "ibm860"
      CASE "cp860"       ; RETURN "cp860"
      CASE "ibm861"
      CASE "cp861"       ; RETURN "cp861"
      CASE "ibm865"
      CASE "cp865"       ; RETURN "cp865"
      CASE "ibm866"
      CASE "cp866"       ; RETURN "cp866"
      CASE "windows1250" ; RETURN "cp1250"
      CASE "windows1251" ; RETURN "cp1251"
      CASE "windows1252" ; RETURN "cp1252"
      CASE "windows1253" ; RETURN "cp1253"
      CASE "windows1254" ; RETURN "cp1254"
      CASE "windows1257" ; RETURN "cp1257"
      CASE "koi8r"       ; RETURN "koi-8"
      CASE "koi8u"       ; RETURN "koi-8u"
      CASE "iso88591"    ; RETURN "iso8859-1"
      CASE "iso88592"    ; RETURN "iso8859-2"
      CASE "iso88595"    ; RETURN "iso8859-5"
      CASE "iso88597"    ; RETURN "iso8859-7"
      CASE "iso88599"    ; RETURN "iso8859-9"
      CASE "utf8"        ; RETURN "utf8"
      ENDSWITCH
   ENDIF

   RETURN ""

STATIC FUNCTION __CPWinToCPStd( nCPWin )

   IF HB_ISNUMERIC( nCPWin )
      SWITCH nCPWin
      CASE 437   ; RETURN "cp437"
      CASE 737   ; RETURN "cp737"
      CASE 775   ; RETURN "cp775"
      CASE 850   ; RETURN "cp850"
      CASE 852   ; RETURN "cp852"
      CASE 857   ; RETURN "cp857"
      CASE 860   ; RETURN "cp860"
      CASE 861   ; RETURN "cp861"
      CASE 865   ; RETURN "cp865"
      CASE 866   ; RETURN "cp866"
      CASE 1200  ; RETURN "utf16"
      CASE 1250  ; RETURN "cp1250"
      CASE 1251  ; RETURN "cp1251"
      CASE 1252  ; RETURN "cp1252"
      CASE 1253  ; RETURN "cp1253"
      CASE 1254  ; RETURN "cp1254"
      CASE 1257  ; RETURN "cp1257"
      CASE 20866 ; RETURN "koi-8"
      CASE 21866 ; RETURN "koi-8u"
      CASE 28591 ; RETURN "iso8859-1"
      CASE 28592 ; RETURN "iso8859-2"
      CASE 28595 ; RETURN "iso8859-5"
      CASE 28597 ; RETURN "iso8859-7"
      CASE 28599 ; RETURN "iso8859-9"
      CASE 65001 ; RETURN "utf8"
      ENDSWITCH
   ENDIF

   RETURN ""

STATIC FUNCTION __CPStdToHarbour( cCPStd, cCtryStd )
   LOCAL cCP
   LOCAL cCtryHb
   LOCAL cdp

   IF ! Empty( cCPStd )
      IF Lower( cCPStd ) == "utf8"
         cCP := "UTF8"
      ELSEIF Lower( cCPStd ) == "utf16"
         cCP := "UTF16LE"
      ELSE
         IF ! Empty( cCtryHb := __CtryStdToCtry( cCtryStd ) )
            FOR EACH cdp IN hb_cdpList()
               IF Left( cdp, 2 ) == cCtryHb
                  IF Lower( cCPStd ) == hb_cdpUniID( cdp )
                     cCP := cdp
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ENDIF
         IF Empty( cCP )
            FOR EACH cdp IN hb_cdpList()
               IF Lower( cCPStd ) == hb_cdpUniID( cdp )
                  cCP := cdp
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN cCP

STATIC FUNCTION __CtryStdToCtry( cCtryStd )
   LOCAL cCtryHb := Left( hb_cdpSelect(), 2 )

   IF HB_ISSTRING( cCtryStd )
      SWITCH Lower( cCtryStd )
      CASE "af-za"      ; EXIT
      CASE "af"         ; EXIT
      CASE "ar-ae"      ; EXIT
      CASE "ar-bh"      ; EXIT
      CASE "ar-dz"      ; EXIT
      CASE "ar-eg"      ; EXIT
      CASE "ar-iq"      ; EXIT
      CASE "ar-jo"      ; EXIT
      CASE "ar-kw"      ; EXIT
      CASE "ar-lb"      ; EXIT
      CASE "ar-ly"      ; EXIT
      CASE "ar-ma"      ; EXIT
      CASE "ar-om"      ; EXIT
      CASE "ar-qa"      ; EXIT
      CASE "ar-sa"      ; EXIT
      CASE "ar-sy"      ; EXIT
      CASE "ar-tn"      ; EXIT
      CASE "ar-ye"      ; EXIT
      CASE "ar"         ; EXIT
      CASE "az-az-cyrl" ; EXIT
      CASE "az-az-latn" ; EXIT
      CASE "az"         ; EXIT
      CASE "be-by"      ; EXIT
      CASE "be"         ; EXIT
      CASE "bg-bg"
      CASE "bg"         ; cCtryHb := "BG" ; EXIT
      CASE "ca-es"      ; EXIT
      CASE "ca"         ; EXIT
      CASE "cy-gb"      ; EXIT
      CASE "cs-cz"
      CASE "cs"         ; cCtryHb := "CS" ; EXIT
      CASE "da-dk"
      CASE "da"         ; cCtryHb := "DK" ; EXIT
      CASE "de-at"
      CASE "de-ch"
      CASE "de-de"
      CASE "de-li"
      CASE "de-lu"
      CASE "de"         ; cCtryHb := "DE" ; EXIT
      CASE "div-mv"     ; EXIT
      CASE "div"        ; EXIT
      CASE "el-gr"
      CASE "el"         ; cCtryHb := "EL" ; EXIT
      CASE "en-au"
      CASE "en-bz"
      CASE "en-ca"
      CASE "en-cb"
      CASE "en-gb"
      CASE "en-ie"
      CASE "en-jm"
      CASE "en-nz"
      CASE "en-ph"
      CASE "en-tt"
      CASE "en-us"
      CASE "en-za"
      CASE "en-zw"
      CASE "en"         ; cCtryHb := "EN" ; EXIT
      CASE "es-ar"
      CASE "es-bo"
      CASE "es-cl"
      CASE "es-co"
      CASE "es-cr"
      CASE "es-do"
      CASE "es-ec"
      CASE "es-es"
      CASE "es-gt"
      CASE "es-hn"
      CASE "es-mx"
      CASE "es-ni"
      CASE "es-pa"
      CASE "es-pe"
      CASE "es-pr"
      CASE "es-py"
      CASE "es-sv"
      CASE "es-uy"
      CASE "es-ve"
      CASE "es"         ; cCtryHb := "ES" ; EXIT
      CASE "et-ee"      ; EXIT
      CASE "et"         ; EXIT
      CASE "eu-es"      ; EXIT
      CASE "eu"         ; EXIT
      CASE "fa-ir"      ; EXIT
      CASE "fa"         ; EXIT
      CASE "fi-fi"
      CASE "fi"         ; cCtryHb := "FI" ; EXIT
      CASE "fo-fo"      ; EXIT
      CASE "fo"         ; EXIT
      CASE "fr-be"
      CASE "fr-ca"
      CASE "fr-ch"
      CASE "fr-fr"
      CASE "fr-lu"
      CASE "fr-mc"
      CASE "fr"         ; cCtryHb := "FR" ; EXIT
      CASE "gl-es"      ; EXIT
      CASE "gl"         ; EXIT
      CASE "gu-in"      ; EXIT
      CASE "gu"         ; EXIT
      CASE "he-il"      ; EXIT
      CASE "he"         ; EXIT
      CASE "hi-in"      ; EXIT
      CASE "hi"         ; EXIT
      CASE "hr-hr"
      CASE "hr"         ; cCtryHb := "HR" ; EXIT
      CASE "hu-hu"
      CASE "hu"         ; cCtryHb := "HU" ; EXIT
      CASE "hy-am"      ; EXIT
      CASE "hy"         ; EXIT
      CASE "id-id"      ; EXIT
      CASE "id"         ; EXIT
      CASE "is-is"
      CASE "is"         ; cCtryHb := "IS" ; EXIT
      CASE "it-ch"
      CASE "it-it"
      CASE "it"         ; cCtryHb := "IT" ; EXIT
      CASE "ja-jp"      ; EXIT
      CASE "ja"         ; EXIT
      CASE "ka-ge"      ; EXIT
      CASE "ka"         ; EXIT
      CASE "kk-kz"      ; EXIT
      CASE "kk"         ; EXIT
      CASE "kn-in"      ; EXIT
      CASE "kn"         ; EXIT
      CASE "ko-kr"      ; EXIT
      CASE "ko"         ; EXIT
      CASE "kok-in"     ; EXIT
      CASE "kok"        ; EXIT
      CASE "ky-kz"      ; EXIT
      CASE "ky"         ; EXIT
      CASE "lt-lt"
      CASE "lt"         ; cCtryHb := "LT" ; EXIT
      CASE "lv-lv"      ; EXIT
      CASE "lv"         ; EXIT
      CASE "mk-mk"      ; EXIT
      CASE "mk"         ; EXIT
      CASE "mn-mn"      ; EXIT
      CASE "mn"         ; EXIT
      CASE "mr-in"      ; EXIT
      CASE "mr"         ; EXIT
      CASE "ms-bn"      ; EXIT
      CASE "ms-my"      ; EXIT
      CASE "ms"         ; EXIT
      CASE "nb-no"      ; EXIT
      CASE "nl-be"
      CASE "nl-nl"
      CASE "nl"         ; cCtryHb := "NL" ; EXIT
      CASE "nn-no"      ; EXIT
      CASE "no"         ; cCtryHb := "NO" ; EXIT
      CASE "pa-in"      ; EXIT
      CASE "pa"         ; EXIT
      CASE "pl-pl"
      CASE "pl"         ; cCtryHb := "PL" ; EXIT
      CASE "pt-br"
      CASE "pt-pt"
      CASE "pt"         ; cCtryHb := "PT" ; EXIT
      CASE "ro-ro"
      CASE "ro"         ; cCtryHb := "RO" ; EXIT
      CASE "ru-ru"
      CASE "ru"         ; cCtryHb := "RU" ; EXIT
      CASE "sa-in"      ; EXIT
      CASE "sa"         ; EXIT
      CASE "sk-sk"
      CASE "sk"         ; cCtryHb := "SK" ; EXIT
      CASE "sl-si"
      CASE "sl"         ; cCtryHb := "SL" ; EXIT
      CASE "sq-al"      ; EXIT
      CASE "sq"         ; EXIT
      CASE "sr-sp-cyrl"
      CASE "sr-sp-latn" ; cCtryHb := "SR" ; EXIT
      CASE "sv-fi"
      CASE "sv-se"
      CASE "sv"         ; cCtryHb := "SV" ; EXIT
      CASE "sw-ke"      ; EXIT
      CASE "sw"         ; EXIT
      CASE "syr-sy"     ; EXIT
      CASE "syr"        ; EXIT
      CASE "ta-in"      ; EXIT
      CASE "ta"         ; EXIT
      CASE "te-in"      ; EXIT
      CASE "te"         ; EXIT
      CASE "th-th"      ; EXIT
      CASE "th"         ; EXIT
      CASE "tr-tr"
      CASE "tr"         ; cCtryHb := "TR" ; EXIT
      CASE "tt-ru"      ; EXIT
      CASE "tt"         ; EXIT
      CASE "uk-ua"
      CASE "uk"         ; cCtryHb := "UA" ; EXIT
      CASE "ur-pk"      ; EXIT
      CASE "ur"         ; EXIT
      CASE "uz-uz-cyrl" ; EXIT
      CASE "uz-uz-latn" ; EXIT
      CASE "uz"         ; EXIT
      CASE "vi-vn"      ; EXIT
      CASE "vi"         ; EXIT
      CASE "zh-chs"     ; EXIT
      CASE "zh-cht"     ; EXIT
      CASE "zh-cn"      ; EXIT
      CASE "zh-hk"      ; EXIT
      CASE "zh-mo"      ; EXIT
      CASE "zh-sg"      ; EXIT
      CASE "zh-tw"      ; EXIT
      ENDSWITCH
   ENDIF

   RETURN cCtryHb
