/*
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
 * Copyright 2003-2006 Phil Krylov <phil@newstar.rinet.ru>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

#include "hbdebug.ch"   /* for "nMode" of __dbgEntry */
#include "hbgtinfo.ch"
#include "hbmemvar.ch"

#include "box.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"


/* Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX */
#define HB_DBG_VAR_MVALUE       HB_DBG_VAR_INDEX

/* Information structure stored in ::aWatch (watchpoints) */
#define WP_TYPE                 1  // wp: watchpoint, tr: tracepoint
#define WP_EXPR                 2  // source of an expression

/* The dimensions of the debugger window */
#define DEBUGGER_MINROW         0
#define DEBUGGER_MINCOL         0
#define DEBUGGER_MAXROW         22
#define DEBUGGER_MAXCOL         77

/* command window scroll history */
#define DEBUGGER_CMDHIST_SIZE   128

THREAD STATIC t_oDebugger

/* debugger entry point */
PROCEDURE __dbgEntry( nMode, uParam1, uParam2, uParam3, uParam4 )

   LOCAL lStartup

   SWITCH nMode
   CASE HB_DBG_ACTIVATE

      IF ( lStartup := ( t_oDebugger == NIL ) )
         t_oDebugger := HBDebugger():New()
         t_oDebugger:pInfo := uParam1
      ENDIF
      t_oDebugger:nProcLevel := uParam2
      t_oDebugger:aCallStack := uParam3
      t_oDebugger:aModules := uParam4
      IF lStartup
         IF t_oDebugger:lRunAtStartup
            __dbgSetGo( uParam1 )
            RETURN
         ENDIF
      ENDIF
      t_oDebugger:lGo := .F.
      t_oDebugger:Activate()
      RETURN

   CASE HB_DBG_GETENTRY

      __dbgSetEntry()

   ENDSWITCH

   RETURN

CREATE CLASS HBDebugger

   VAR pInfo
   VAR aWindows          INIT {}
   VAR nCurrentWindow    INIT 1
   VAR oPullDown

   VAR oWndCode
   VAR oWndCommand
   VAR oWndStack
   VAR oWndVars

   VAR oBrwText
   VAR cPrgName
   VAR oBrwStack
   VAR oBrwVars
   VAR aVars             INIT {}

   VAR nAppDispCount

   VAR nAppDirCase
   VAR nAppFileCase
   VAR nAppTypeAhead
   VAR nAppLastKey

   VAR nMaxRow
   VAR nMaxCol

   VAR hUserWindow
   VAR hDebuggerWindow
   VAR lDebuggerWindowIsOpen INIT .F.

   VAR aCallStack        INIT {}    // stack of procedures with debug info
   VAR aProcStack        INIT {}    // stack of all procedures
   VAR nProcLevel                   // procedure level where the debugger is currently
   VAR aModules          INIT {}    // array of modules with static and GLOBAL variables
   VAR aWatch            INIT {}
   VAR aColors           INIT { "W+/BG", "N/BG", "R/BG", "N+/BG", "W+/B", "GR+/B", "W/B", "N/W", "R/W", "N/BG", "R/BG" }

   VAR aHistCommands
   VAR aLastCommands
   VAR nCommand
   VAR oGetCommand

   VAR lAnimate          INIT .F.
   VAR lEnd              INIT .F.
   VAR lCaseSensitive    INIT .F.
   VAR lMonoDisplay      INIT .F.
   VAR lSortVars         INIT .F.

   VAR cSearchString     INIT ""
   VAR cPathForFiles
   VAR cSettingsFileName INIT "init.cld"
   VAR aPathDirs

   VAR nTabWidth         INIT 4
   VAR nSpeed            INIT 0

   VAR lShowPublics      INIT .F.
   VAR lShowPrivates     INIT .F.
   VAR lShowStatics      INIT .F.
   VAR lShowLocals       INIT .F.
   VAR lShowGlobals      INIT .F.
   VAR lShowAllGlobals   INIT .F.
   VAR lAll              INIT .F.
   VAR lShowCallStack    INIT .F.
   VAR lGo                          // stores if GO was requested
   VAR lActive           INIT .F.
   VAR lCBTrace          INIT .T.   // stores if codeblock tracing is allowed
   VAR oBrwPnt
   VAR oWndPnt
   VAR lPPO              INIT .F.
   VAR lRunAtStartup     INIT .T.   // Clipper compatible
   VAR lLineNumbers      INIT .T.
   VAR nHelpPage         INIT 1
   VAR nWaFocus          INIT 1
   VAR nCmdWndHight      INIT 3
   VAR lWindowsAutoSized INIT .T.

   METHOD New()
   METHOD Activate()

   METHOD All()

   METHOD Animate() INLINE iif( ::lAnimate, ::Step(), NIL )

   METHOD BarDisplay()
   METHOD BuildCommandWindow()
   METHOD BuildBrowseStack()
   METHOD ResizeCmdWnd( nLines )

   METHOD CallStackProcessKey( nKey )
   METHOD ClrModal() INLINE iif( ::lMonoDisplay, "N/W, W+/W, W/N, W+/N", ;
      "N/W, R/W, N/BG, R/BG" )
   METHOD GetColors() INLINE iif( ! ::lMonoDisplay, ::aColors, { ;
      "W+/N", "W+/N", "N/W", "N/W", ;
      "N/W", "N/W", "W+/N", "N/W", ;
      "W+/W", "W/N", "W+/N" } )
   METHOD CodeblockTrace()
   METHOD CodeWindowProcessKey( nKey )
   METHOD Colors()
   METHOD CommandWindowProcessKey( nKey )
   METHOD CommandWindowDisplay( cLine, lCmd )
   METHOD DoCommand( cCommand )
   METHOD DoScript( cFileName )
   METHOD EditColor( nColor, oBrwColors )
   METHOD EditSet( nSet, oBrwSets )
   METHOD EditVar( nVar )
   METHOD Exit() INLINE ::lEnd := .T.
   METHOD FindNext()
   METHOD FindPrevious()
   METHOD GetExprValue( xExpr, lValid )
   METHOD GetSourceFiles()
   METHOD ModuleMatch( cModuleName1, cModuleName2 )

   METHOD Global()

   METHOD Go()
   METHOD GoToLine( nLine )
   METHOD HandleEvent()
   METHOD Hide()
   METHOD HideCallStack()
   METHOD HideVars()
   METHOD InputBox( cMsg, uValue, bValid, lEditable )
   METHOD Inspect( uValue, cValueName )
   METHOD IsValidStopLine( cName, nLine )
   METHOD ListBox( cCaption, aItems )
   METHOD LoadColors()
   METHOD LoadSettings()
   METHOD LoadVars()
   METHOD LoadCallStack()

   METHOD Local()

   METHOD Locate( nMode, cValue )

   METHOD MonoDisplay()
   METHOD NextWindow()
   METHOD Open( cFileName )
   METHOD OpenMenu( cName )
   METHOD OpenPPO()
   METHOD Resume() INLINE ::ShowCodeLine( 1 )
   METHOD OSShell()
   METHOD PathForFiles( cPathForFiles )

   METHOD PrevWindow()
   METHOD Private()
   METHOD Public()
   METHOD Quit()
   METHOD RefreshVars()
   METHOD RestoreAppScreen()
   METHOD RestoreAppState()
   METHOD RestoreSettings( cFileName )
   METHOD RunAtStartup( lRunAtStartup )
   METHOD SaveAppScreen()
   METHOD SaveAppState()
   METHOD SaveSettings( cFileName )
   METHOD Show()
   METHOD ShowAllGlobals()
   METHOD ShowAppScreen()
   METHOD ShowCallStack()
   METHOD ShowCodeLine( nProc )
   METHOD ShowHelp( cTopic )
   METHOD ShowVars()
   METHOD LocatePrgPath( cPrgName )
   METHOD Sort() INLINE ASort( ::aVars,,, {| x, y | x[ HB_DBG_VAR_NAME ] < y[ HB_DBG_VAR_NAME ] } ), ;
      ::lSortVars := .T., ;
      iif( ::oBrwVars != NIL, ::oBrwVars:RefreshAll(), NIL ), ;
      iif( ::oWndVars != NIL .AND. ::oWndVars:lVisible, iif( ! ::lGo .AND. ::oBrwVars != NIL, ::oBrwVars:ForceStable(), NIL ), NIL )

   METHOD Speed() INLINE ::nSpeed := ::InputBox( "Step delay (in tenths of a second)", ::nSpeed )

   METHOD Stack( cParam )
   METHOD Static()

   METHOD Step()

   METHOD TabWidth() INLINE ;
      ::nTabWidth := ::InputBox( "Tab width", ::nTabWidth ), ;
      ::oBrwText:nTabWidth := ::nTabWidth, ::oBrwText:RefreshAll()

   METHOD BreakPointToggle( nLine, cFileName )
   METHOD BreakPointDelete( cPos )
   METHOD BreakPointFunc( cFuncName )
   METHOD BreakPointList()

   METHOD Trace()

   METHOD ToCursor()
   METHOD NextRoutine()
   METHOD ViewSets()
   METHOD WndVarsLButtonDown( nMRow, nMCol )
   METHOD LineNumbers( lLineNumbers ) // Toggles numbering of source code lines
   METHOD RemoveWindow( oWnd )
   METHOD SearchLine( cLine )
   METHOD ToggleAnimate() INLINE ::oPullDown:GetItemByIdent( "ANIMATE" ):checked := ::lAnimate := ! ::lAnimate
   METHOD ToggleCaseSensitive() INLINE ::oPullDown:GetItemByIdent( "CASE" ):checked := ::lCaseSensitive := ! ::lCaseSensitive
   METHOD ShowWorkAreas() INLINE __dbgShowWorkAreas( Self )

   METHOD TracepointAdd( cExpr )
   METHOD WatchpointAdd( cExpr )
   METHOD WatchpointDel( xPos )
   METHOD WatchpointsShow()
   METHOD WatchpointsHide()
   METHOD WatchpointEdit( nPos )
   METHOD WatchpointInspect( nPos )
   METHOD WatchGetInfo( nWatch )
   METHOD WatchpointList()

   METHOD VarGetInfo( aVar )
   METHOD VarGetValue( aVar )
   METHOD VarSetValue( aVar, uValue )

   METHOD ResizeWindows( oWindow )
   METHOD NotSupported() INLINE __dbgAlert( "Not implemented yet!" )

   METHOD OpenDebuggerWindow()
   METHOD CloseDebuggerWindow()

ENDCLASS


METHOD New() CLASS HBDebugger

   t_oDebugger := Self

   /* default the search path for files to the current directory
      that way if the source is in the same directory it will still be found even if the application
      changes the current directory with the SET DEFAULT command. */
   ::cPathForFiles := GetEnv( "HB_DBG_PATH" )
   IF Empty( ::cPathForFiles )
      ::cPathForFiles := GetEnv( "PATH" )
   ENDIF
   ::aPathDirs := PathToArray( ::cPathForFiles )

   ::lGo := ::lRunAtStartup

   /* Store the initial screen dimensions for now */
   ::nMaxRow := MaxRow()
   ::nMaxCol := MaxCol()

   ::oPullDown := __dbgBuildMenu( Self )

   ::oWndCode             := HBDbWindow():New( 1, 0, ;
                                               ::nMaxRow - ::nCmdWndHight - 3, ;
                                               ::nMaxCol )
   ::oWndCode:bKeyPressed := {| nKey | ::CodeWindowProcessKey( nKey ) }

   AAdd( ::aWindows, ::oWndCode )

   ::BuildCommandWindow()
   ::BuildBrowseStack()

   IF hb_FileExists( ::cSettingsFileName )
      ::LoadSettings()
      ::lGo := ::lRunAtStartup // Once again after settings file is loaded
   ENDIF

   RETURN Self

METHOD PROCEDURE OpenDebuggerWindow() CLASS HBDebugger

   IF ! ::lDebuggerWindowIsOpen
      ::hUserWindow := hb_gtInfo( HB_GTI_GETWIN )
      IF ::hDebuggerWindow == NIL
         ::hDebuggerWindow := hb_gtInfo( HB_GTI_GETWIN, ;
            { "Debugger", DEBUGGER_MINROW, DEBUGGER_MINCOL, ;
            DEBUGGER_MAXROW, DEBUGGER_MAXCOL } )
      ELSE
         hb_gtInfo( HB_GTI_SETWIN, ::hDebuggerWindow )
      ENDIF
      ::lDebuggerWindowIsOpen := .T.
   ENDIF

   RETURN


METHOD PROCEDURE CloseDebuggerWindow() CLASS HBDebugger

   IF ::lDebuggerWindowIsOpen
      ::hDebuggerWindow := hb_gtInfo( HB_GTI_GETWIN )
      hb_gtInfo( HB_GTI_SETWIN, ::hUserWindow )
      ::lDebuggerWindowIsOpen := .F.
   ENDIF

   RETURN


METHOD PROCEDURE Activate() CLASS HBDebugger

   LOCAL lFirst := .F.

   ::LoadCallStack()
   ::SaveAppState()

   IF ::lActive
      ::SaveAppScreen()
   ELSE
      ::lActive := .T.
      ::Show()
      IF ::lShowCallStack
         ::ShowCallStack()
      ENDIF
      lFirst := .T.
   ENDIF
   IF ::lShowCallStack
      ::oWndStack:Show()
   ENDIF

   ::LoadVars()
   ::ShowVars()

   IF ::oWndPnt != NIL
      ::WatchpointsShow()
   ENDIF

   // show the topmost procedure
   ::ShowCodeLine( 1 ) // ::aCallStack[ 1 ][ HB_DBG_CS_LINE ], ::aCallStack[ 1 ][ HB_DBG_CS_MODULE ] )

   // Most commands can be executed only after activation
   IF lFirst
      ::LoadSettings()
   ENDIF

   ::HandleEvent()

   RETURN


METHOD PROCEDURE All() CLASS HBDebugger

   ::lShowPublics := ::lShowPrivates := ::lShowStatics := ;
      ::lShowLocals := ::lShowGlobals := ::lAll := ! ::lAll

   ::RefreshVars()

   RETURN

METHOD PROCEDURE BarDisplay() CLASS HBDebugger

   LOCAL cClrItem   := __dbgColors()[ 8 ]
   LOCAL cClrHotKey := __dbgColors()[ 9 ]

   DispBegin()

   hb_Scroll( ::nMaxRow, 0, ::nMaxRow, ::nMaxCol,,, cClrItem )
   hb_DispOutAt( ::nMaxRow,  0, "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt F10-Trace", cClrItem )
   hb_DispOutAt( ::nMaxRow,  0, "F1", cClrHotKey )
   hb_DispOutAt( ::nMaxRow,  8, "F2", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 16, "F3", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 26, "F4", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 34, "F5", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 40, "F6", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 46, "F7", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 54, "F8", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 62, "F9", cClrHotKey )
   hb_DispOutAt( ::nMaxRow, 70, "F10", cClrHotKey )

   DispEnd()

   RETURN


METHOD PROCEDURE BuildBrowseStack() CLASS HBDebugger

   LOCAL aColors

   IF ::oBrwStack == NIL
      aColors := __dbgColors()
      ::oBrwStack := HBDbBrowser():New( 2, ::nMaxCol - 14, ::nMaxRow - ::nCmdWndHight - 4, ::nMaxCol - 1 )
      ::oBrwStack:ColorSpec := aColors[ 3 ] + "," + aColors[ 4 ] + "," + aColors[ 5 ] + "," + aColors[ 6 ]
      ::oBrwStack:goTopBlock := {|| ::oBrwStack:Cargo := 1 }
      ::oBrwStack:goBottomBlock := {|| ::oBrwStack:Cargo := Len( ::aProcStack ) }
      ::oBrwStack:skipBlock := {| nSkip, nOld | nOld := ::oBrwStack:Cargo, ;
         ::oBrwStack:Cargo += nSkip, ;
         ::oBrwStack:Cargo := Min( Max( ::oBrwStack:Cargo, 1 ), ;
         Len( ::aProcStack ) ), ::oBrwStack:Cargo - nOld }

      ::oBrwStack:Cargo := 1 // Actual highligthed row

      ::oBrwStack:AddColumn( HBDbColumnNew( "", {|| iif( Len( ::aProcStack ) > 0, ;
         PadC( ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_FUNCTION ], 14 ), Space( 14 ) ) } ) )
   ENDIF

   RETURN


METHOD PROCEDURE BuildCommandWindow() CLASS HBDebugger

   LOCAL nSize

   ::oWndCommand := HBDbWindow():New( ::nMaxRow - ::nCmdWndHight - 2, 0, ;
                                      ::nMaxRow - 1, ::nMaxCol, "Command" )

   ::oWndCommand:bKeyPressed := {| nKey | ::CommandWindowProcessKey( nKey ) }
   ::oWndCommand:bPainted    := {|| ::CommandWindowDisplay(), ;
       hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ;
                     "> ", __dbgColors()[ 2 ] ), ;
      ::oGetCommand:SetColor( __dbgColors()[ 2 ] ):display() }
   AAdd( ::aWindows, ::oWndCommand )

   ::aHistCommands := { "" }
   ::aLastCommands := { "" }
   ::nCommand := 1

   nSize := ::oWndCommand:nRight - ::oWndCommand:nLeft - 3
   ::oGetCommand := HbDbInput():new( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3, ;
                                     nSize, "", __dbgColors()[ 2 ], Max( nSize, 256 ) )

   RETURN


METHOD PROCEDURE CallStackProcessKey( nKey ) CLASS HBDebugger

   LOCAL n
   LOCAL nSkip
   LOCAL lUpdate := .F.

   SWITCH nKey
   CASE K_HOME
   CASE K_CTRL_PGUP
   CASE K_CTRL_HOME

      IF ::oBrwStack:Cargo > 1
         ::oBrwStack:GoTop()
         ::oBrwStack:ForceStable()
         lUpdate := .T.
      ENDIF
      EXIT

   CASE K_END
   CASE K_CTRL_PGDN
   CASE K_CTRL_END

      IF ::oBrwStack:Cargo < Len( ::aProcStack )
         ::oBrwStack:GoBottom()
         ::oBrwStack:ForceStable()
         lUpdate := .T.
      ENDIF
      EXIT

   CASE K_UP

      IF ::oBrwStack:Cargo > 1
         ::oBrwStack:Up()
         ::oBrwStack:ForceStable()
         lUpdate := .T.
      ENDIF
      EXIT

   CASE K_DOWN

      IF ::oBrwStack:Cargo < Len( ::aProcStack )
         ::oBrwStack:Down()
         ::oBrwStack:ForceStable()
         lUpdate := .T.
      ENDIF
      EXIT

   CASE K_PGUP

      ::oBrwStack:PageUp()
      ::oBrwStack:ForceStable()
      lUpdate := .T.
      EXIT

   CASE K_PGDN

      ::oBrwStack:PageDown()
      ::oBrwStack:ForceStable()
      lUpdate := .T.
      EXIT

   CASE K_LBUTTONDOWN

      IF ( nSkip := MRow() - ::oWndStack:nTop - ::oBrwStack:RowPos ) != 0
         IF nSkip > 0
            FOR n := 1 TO nSkip
               ::oBrwStack:Down()
               ::oBrwStack:Stabilize()
            NEXT
         ELSE
            FOR n := 1 TO nSkip + 2 STEP -1
               ::oBrwStack:Up()
               ::oBrwStack:Stabilize()
            NEXT
         ENDIF
         ::oBrwStack:ForceStable()
      ENDIF
      lUpdate := .T.
      EXIT

   ENDSWITCH

   IF lUpdate
      IF ::oWndVars != NIL .AND. ::oWndVars:lVisible
         ::LoadVars()
         ::ShowVars()
      ENDIF

      // jump to source line for a function

#if 0
      IF ::aCallStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_LINE ] != NIL
         ::ShowCodeLine( ::aCallStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_LINE ], ::aCallStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_MODULE ] )
      ELSE
         ::GotoLine( 1 )
      ENDIF
#endif

      ::ShowCodeLine( ::oBrwStack:Cargo )
   ENDIF

   RETURN


METHOD PROCEDURE CodeblockTrace() CLASS HBDebugger

   ::oPullDown:GetItemByIdent( "CODEBLOCK" ):checked := ::lCBTrace := ! ::lCBTrace
   __dbgSetCBTrace( ::pInfo, ::lCBTrace )

   RETURN


METHOD PROCEDURE CodeWindowProcessKey( nKey ) CLASS HBDebugger

   IF ::oBrwText != NIL

      SWITCH nKey
      CASE K_CTRL_PGUP
      CASE K_CTRL_HOME

         ::oBrwText:GoTop()
         EXIT

      CASE K_CTRL_PGDN
      CASE K_CTRL_END

         ::oBrwText:GoBottom()
         ::oBrwText:nCol := ::oWndCode:nLeft + 1
         ::oBrwText:nFirstCol := ::oWndCode:nLeft + 1
         EXIT

      CASE K_HOME
         ::oBrwText:Home()
         EXIT

      CASE K_END
         ::oBrwText:End()
         EXIT

      CASE K_LEFT
         ::oBrwText:Left()
         EXIT

      CASE K_RIGHT
         ::oBrwText:Right()
         EXIT

      CASE K_UP
         ::oBrwText:Up()
         EXIT

      CASE K_DOWN
         ::oBrwText:Down()
         EXIT

      CASE K_PGUP
         ::oBrwText:PageUp()
         EXIT

      CASE K_PGDN
         ::oBrwText:PageDown()
         EXIT

      ENDSWITCH
   ENDIF

   RETURN


METHOD PROCEDURE Colors() CLASS HBDebugger

   LOCAL oWndColors := HBDbWindow():New( 4, 5, 16, ::nMaxCol - 5, ;
      "Debugger Colors[1..11]", ::ClrModal() )
   LOCAL aColors := { ;
      "Border", "Text", "Text High", "Text PPO", "Text Selected", ;
      "Text High Sel.", "Text PPO Sel.", "Menu", "Menu High", ;
      "Menu Selected", "Menu High Sel." }

   LOCAL oBrwColors := HBDbBrowser():New( oWndColors:nTop + 1, oWndColors:nLeft + 1, ;
      oWndColors:nBottom - 1, oWndColors:nRight - 1 )
   LOCAL nWidth := oWndColors:nRight - oWndColors:nLeft - 1
   LOCAL oCol

   IF ::lMonoDisplay
      __dbgAlert( "Monochrome display" )
      RETURN
   ENDIF

   oBrwColors:Cargo := { 1, {} }  // Actual highligthed row
   oBrwColors:ColorSpec := ::ClrModal()
   oBrwColors:goTopBlock := {|| oBrwColors:cargo[ 1 ] := 1 }
   oBrwColors:goBottomBlock := {|| oBrwColors:cargo[ 1 ] := Len( oBrwColors:cargo[ 2 ][ 1 ] ) }
   oBrwColors:skipBlock := {| nPos | ( nPos := ArrayBrowseSkip( nPos, oBrwColors ), oBrwColors:cargo[ 1 ] := ;
      oBrwColors:cargo[ 1 ] + nPos, nPos ) }

   oBrwColors:AddColumn( oCol := HBDbColumnNew( "", {|| aColors[ oBrwColors:Cargo[ 1 ] ] } ) )
   oCol:Width := 14
   oCol:defColor := { 1, 2 }
   AAdd( oBrwColors:Cargo[ 2 ], aColors )
   oBrwColors:AddColumn( oCol := HBDbColumnNew( "", ;
      {|| PadR( '"' + ::aColors[ oBrwColors:Cargo[ 1 ] ] + '"', nWidth - 15 ) } ) )
   AAdd( oBrwColors:Cargo[ 2 ], aColors )
   oCol:defColor := { 1, 3 }
   oCol:Width := 50
   oBrwColors:autolite := .F.

   oWndColors:bPainted    := {|| oBrwColors:ForceStable(), RefreshVarsS( oBrwColors ) }

   oWndColors:bKeyPressed := {| nKey | SetsKeyPressed( nKey, oBrwColors, ;
      Len( aColors ), oWndColors, "Debugger Colors", ;
      {|| ::EditColor( oBrwColors:Cargo[ 1 ], oBrwColors ) } ) }

   oWndColors:ShowModal()

   ::LoadColors()

   RETURN


METHOD PROCEDURE CommandWindowDisplay( cLine, lCmd ) CLASS HBDebugger

   LOCAL n, nRow, nSize

   IF cLine != NIL
      cLine := iif( lCmd, "> ", "      " ) + cLine
      IF Len( ::aHistCommands ) >= DEBUGGER_CMDHIST_SIZE
         ADel( ::aHistCommands, 1 )
         ::aHistCommands[ Len( ::aHistCommands ) ] := cLine
      ELSE
         AAdd( ::aHistCommands, cLine )
      ENDIF
   ENDIF

   n := Len( ::aHistCommands )
   nRow := ::oWndCommand:nBottom
   nSize := ::oWndCommand:nRight - ::oWndCommand:nLeft - 1
   hb_DispOutAt( --nRow, ::oWndCommand:nLeft + 1, PadR( "> ", nSize ), ;
                 __dbgColors()[ 2 ] )
   DO WHILE --nRow > ::oWndCommand:nTop
      hb_DispOutAt( nRow, ::oWndCommand:nLeft + 1, ;
                    PadR( iif( n > 0, ::aHistCommands[ n-- ], "" ), nSize ), ;
                    __dbgColors()[ 2 ] )
   ENDDO

   RETURN


METHOD PROCEDURE CommandWindowProcessKey( nKey ) CLASS HBDebugger

   LOCAL cCommand
   LOCAL n

   SWITCH nKey
   CASE K_UP
   CASE K_F3
      IF ::nCommand > 1
         ::aLastCommands[ ::nCommand ] := RTrim( ::oGetCommand:getValue() )
         ::oGetCommand:setValue( ::aLastCommands[ --::nCommand ] ):display()
      ENDIF
      EXIT
   CASE K_DOWN
      IF ::nCommand < Len( ::aLastCommands )
         ::aLastCommands[ ::nCommand ] := RTrim( ::oGetCommand:getValue() )
         ::oGetCommand:setValue( ::aLastCommands[ ++::nCommand ] ):display()
      ENDIF
      EXIT
   CASE K_ENTER
      cCommand := RTrim( ::oGetCommand:getValue() )
      IF ! Empty( cCommand )
         IF ( n := hb_AScan( ::aLastCommands, cCommand, , , .T. ) ) > 0 .AND. n < Len( ::aLastCommands )
            hb_ADel( ::aLastCommands, n, .T. )
         ENDIF
         ::nCommand := Len( ::aLastCommands )
         ::aLastCommands[ ::nCommand ] := cCommand
         AAdd( ::aLastCommands, "" )
         ::nCommand := Len( ::aLastCommands )
         ::CommandWindowDisplay( cCommand, .T. )
         ::DoCommand( cCommand )
      ENDIF
      hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ;
                    PadR( "> ", ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ), ;
                    __dbgColors()[ 2 ] )
      ::oGetCommand:setValue( "" ):display()
      EXIT
   OTHERWISE
      ::oGetCommand:applyKey( nKey )
   ENDSWITCH

   RETURN


/*
 * ?? <expr>
 *      displays inspect window with value or display nothing on error
 * ? <expr>
 *      displays either result or error description in command window
 */
METHOD DoCommand( cCommand ) CLASS HBDebugger

   LOCAL aCmnd[ 3 ]
   LOCAL cParam1 := ""
   LOCAL cParam2 := ""
   LOCAL cParams := ""
   LOCAL cResult
   LOCAL lValid
   LOCAL oWindow
   LOCAL n

   cCommand := AllTrim( cCommand )

   DO CASE
   CASE Empty( cCommand )
      RETURN ""

   CASE hb_LeftEq( cCommand, "??" )
      cParams := AllTrim( SubStr( cCommand, 3 ) )
      cCommand := "??"

   CASE hb_LeftEq( cCommand, "?" )
      cParams := SubStr( cCommand, 2 )
      cCommand := "?"

   OTHERWISE
      IF ( n := At( " ", cCommand ) ) > 0
         cParam1 := cParams := AllTrim( SubStr( cCommand, n + 1 ) )
         cCommand := Left( cCommand, n - 1 )
         IF ( n := At( " ", cParam1 ) ) > 0
            cParam2 := AllTrim( SubStr( cParam1, n + 1 ) )
            cParam1 := Left( cParam1, n - 1 )
         ENDIF
      ENDIF
      cCommand := Upper( cCommand )
      cParam1 := Upper( cParam1 )
   ENDCASE

   DO CASE
   CASE cCommand == "??" .OR. cCommand == "?"
      aCmnd[ WP_TYPE ] := cCommand
      aCmnd[ WP_EXPR ] := cParams

      ::RestoreAppState()
      cResult := ::GetExprValue( cParams, @lValid )
      ::SaveAppState()

      IF aCmnd[ WP_TYPE ] == "??"
         IF lValid
            ::Inspect( aCmnd[ WP_EXPR ], cResult )
            cResult := ""  // discard result
         ENDIF
      ELSEIF lValid
         cResult := __dbgValToStr( cResult )
      ENDIF
      ::RefreshVars()

   CASE hb_LeftEqN( "ANIMATE", cCommand, 4 )
      IF ::lActive
         ::lAnimate := .T.
         ::Animate()
      ENDIF

   CASE cCommand == "BP"
      IF Empty( cParam1 )
         ::BreakPointToggle()
      ELSEIF IsDigit( cParam1 )
         ::BreakPointToggle( Val( cParam1 ), ;
                             iif( Empty( cParam2 ), ::cPrgName, cParam2 ) )
      ELSEIF hb_asciiIsAlpha( cParam1 ) .OR. hb_LeftEq( cParam1, "_" )
         ::BreakPointFunc( cParam1 )
      ENDIF

   CASE hb_LeftEqN( "CALLSTACK", cCommand, 4 )
      ::Stack( cParam1 )

   CASE hb_LeftEqN( "DELETE", cCommand, 3 )
      DO CASE
      CASE cParam1 == "BP"
         ::BreakPointDelete( cParam2 )
      CASE cParam1 == "WP" .OR. cParam2 == "TP"
         ::WatchpointDel( cParam2 )
      CASE cParam1 == "ALL"
         DO CASE
         CASE Empty( cParam2 )
            ::BreakPointDelete( cParam1 )
            ::WatchpointDel( cParam1 )
         CASE hb_LeftEqI( "BP", cParam2 )
            ::BreakPointDelete( cParam1 )
         CASE hb_LeftEqI( "WP", cParam2 ) .OR. hb_LeftEqI( "TP", cParam2 )
            ::WatchpointDel( cParam1 )
         OTHERWISE
            /* Cl*pper clears break and watch points in such case */
            cResult := "Command error"
         ENDCASE
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "DOS"
      ::OsShell()

   CASE hb_LeftEq( "FILE", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "OPEN", cParam1 )
         ::Open( cParam2 )
      CASE hb_LeftEq( "RESUME", cParam1 )
         ::Resume()
      CASE hb_LeftEq( "OS", cParam1 ) .OR. hb_LeftEq( "DOS", cParam1 )
         ::OSShell()
      CASE hb_LeftEq( "EXIT", cParam1 )
         ::Quit()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "FIND"
      ::Locate( 0, cParams )

   CASE hb_LeftEqN( "GOTO", cCommand, 4 ) .AND. ( n := Val( cParam1 ) ) > 0
      ::GoToLine( n )

   CASE hb_LeftEq( "GO", cCommand )
      ::Go()

   CASE cCommand == "HELP"
      ::ShowHelp( cParam1 )

   CASE hb_LeftEqN( "INPUT", cCommand, 4 )
      IF Empty( cParams )
         cParams := AllTrim( ::InputBox( "File name",, ;
                                         {| cFile | hb_FileExists( cFile ) .OR. ;
                                                    ( __dbgAlert( "File unavailable" ), .F. ) } ) )
         IF LastKey() == K_ESC
            cParams := ""
         ENDIF
      ENDIF
      IF ! Empty( cParams )
         ::DoScript( cParams )
      ENDIF

   CASE cCommand == "LIST"
      SWITCH cParam1
      CASE "BP"
         ::BreakPointList()
         EXIT
      CASE "WP"
      CASE "TP"
         ::WatchpointList()
         EXIT
      OTHERWISE
         cResult := "Command error"
      ENDSWITCH

   CASE hb_LeftEq( "LOCATE", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "FIND", cParam1 )
         ::Locate( 0, cParam2 )
      CASE hb_LeftEq( "NEXT", cParam1 )
         ::FindNext()
      CASE hb_LeftEq( "PREVIOUS", cParam1 )
         ::FindPrevious()
      CASE hb_LeftEq( "GOTOLINE", cParam1 )
         ::SearchLine( cParam2 )
      CASE hb_LeftEq( "CASESENSITIVE", cParam1 )
         ::ToggleCaseSensitive()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE hb_LeftEq( "MONITOR", cCommand )

      /* Here the order of CASEs makes sense: M P is Public, while M Pr is
       * Private, etc. */
      DO CASE
      CASE hb_LeftEq( "PUBLIC", cParam1 )
         ::Public()
      CASE hb_LeftEq( "PRIVATE", cParam1 )
         ::Private()
      CASE hb_LeftEq( "STATIC", cParam1 )
         ::Static()
      CASE hb_LeftEq( "LOCAL", cParam1 )
         ::Local()
      CASE hb_LeftEq( "GLOBAL", cParam1 )
         ::Global()
      CASE hb_LeftEq( "ALL", cParam1 )
         ::All()
      CASE hb_LeftEq( "SORT", cParam1 )
         ::Sort()
      CASE hb_LeftEq( "SHOWALLGLOBALS", cParam1 )
         ::ShowAllGlobals()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "NEXT"
      ::FindNext()

   CASE cCommand == "NUM"
      SWITCH cParam1
      CASE "OFF"
         ::LineNumbers( .F. ) ; EXIT
      CASE "ON"
         ::LineNumbers( .T. ) ; EXIT
      OTHERWISE
         ::LineNumbers() ; EXIT
      ENDSWITCH

   CASE hb_LeftEq( "OPTIONS", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "PREPROCESSEDCODE", cParam1 )
         ::OpenPPO()
      CASE hb_LeftEq( "LINENUMBERS", cParam1 )
         ::LineNumbers()
      CASE hb_LeftEq( "EXCHANGESCREENS", cParam1 ) .OR. ;
           hb_LeftEq( "SWAPONINPUT", cParam1 ) .OR. ;
           hb_LeftEq( "MENUBAR", cParam1 )
         ::NotSupported()
      CASE hb_LeftEq( "CODEBLOCKTRACE", cParam1 )
         ::CodeblockTrace()
      CASE hb_LeftEq( "MONODISPLAY", cParam1 )
         ::MonoDisplay()
      CASE hb_LeftEq( "COLORS", cParam1 )
         IF Empty( cParam2 )
            ::Colors()
         ELSE
            cParam2 := SubStr( cParam2, At( "{", cParam2 ) + 1 )
            FOR n := 1 TO 11
               IF "," $ cParam2
                  ::aColors[ n ] := ;
                     StrTran( Left( cParam2, At( ",", cParam2 ) - 1 ), '"' )
                  cParam2 := SubStr( cParam2, At( ",", cParam2 ) + 1 )
               ELSE
                  ::aColors[ n ] := ;
                     StrTran( Left( cParam2, At( "}", cParam2 ) - 1 ), '"' )
               ENDIF
            NEXT
            ::LoadColors()
         ENDIF
      CASE hb_LeftEq( "TABWIDTH", cParam1 )
         IF IsDigit( cParam2 )
            ::nTabWidth := Min( Val( cParam2 ), 16 )
         ELSE
            ::TabWidth()
         ENDIF
      CASE hb_LeftEq( "PATHFORFILES", cParam1 )
         ::PathForFiles( AllTrim( cParam2 ) )
      CASE hb_LeftEq( "RUNATSTARTUP", cParam1 )
         ::RunAtStartup( .T. )
      CASE hb_LeftEq( "NORUNATSTARTUP", cParam1 )
         ::RunAtStartup( .F. )
      CASE hb_LeftEq( "SAVESETTINGS", cParam1 )
         ::SaveSettings( AllTrim( cParam2 ) )
      CASE hb_LeftEq( "RESTORESETTINGS", cParam1 )
         ::RestoreSettings( AllTrim( cParam2 ) )
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE hb_LeftEqN( "OUTPUT", cCommand, 4 )
      ::ShowAppScreen()

   CASE hb_LeftEq( "POINT", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "WATCHPOINT", cParam1 )
         ::WatchpointAdd()
      CASE hb_LeftEq( "TRACEPOINT", cParam1 )
         ::TracepointAdd()
      CASE hb_LeftEq( "BREAKPOINT", cParam1 )
         ::BreakPointToggle()
      CASE hb_LeftEq( "DELETE", cParam1 )
         ::WatchpointDel()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "PREV"
      ::FindPrevious()

   CASE hb_LeftEq( "QUIT", cCommand )
      ::Quit()

   /* TODO: Support RESTART */

   CASE hb_LeftEqN( "RESUME", cCommand, 4 )
      ::Resume()

   CASE hb_LeftEq( "RUN", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "ANIMATE", cParam1 )
         ::ToggleAnimate()
         ::Animate()
      CASE hb_LeftEq( "STEP", cParam1 )
         ::Step()
      CASE hb_LeftEq( "TRACE", cParam1 )
         ::Trace()
      CASE hb_LeftEq( "GO", cParam1 )
         ::Go()
      CASE hb_LeftEq( "TOCURSOR", cParam1 )
         ::ToCursor()
      CASE hb_LeftEq( "NEXTROUTINE", cParam1 )
         ::NextRoutine()
      CASE hb_LeftEq( "SPEED", cParam1 )
         IF IsDigit( cParam2 )
            ::nSpeed := Min( Val( cParam2 ), 65534 )
         ELSE
            ::Speed()
         ENDIF
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE hb_LeftEqN( "SPEED", cCommand, 4 )
      IF IsDigit( cParam1 )
         ::nSpeed := Min( Val( cParam1 ), 65534 )
      ELSE
         ::Speed()
      ENDIF

   CASE cCommand == "STEP"
      ::Step()

   CASE cCommand == "TP"
      ::TracepointAdd( cParams )

   CASE hb_LeftEq( "VIEW", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "SETS", cParam1 )
         ::ViewSets()
      CASE hb_LeftEq( "WORKAREAS", cParam1 )
         ::ShowWorkAreas()
      CASE hb_LeftEq( "APPSCREEN", cParam1 )
         ::ShowAppScreen()
      CASE hb_LeftEq( "CALLSTACK", cParam1 )
         ::Stack()
      OTHERWISE
         ::Open( cParams )
      ENDCASE

   CASE hb_LeftEq( "WINDOW", cCommand )
      DO CASE
      CASE Empty( cParam1 )
         ::OpenMenu( cCommand )
      CASE hb_LeftEq( "NEXT", cParam1 )
         ::NextWindow()
      CASE hb_LeftEq( "PREV", cParam1 )
         ::PrevWindow()
      CASE hb_LeftEq( "MOVE", cParam1 )
         IF Empty( cParam2 )
            ::NotSupported()
         ELSE
            oWindow := ::aWindows[ ::nCurrentWindow ]
            IF ( n := At( " ", cParam2 ) ) > 0
               n := Val( SubStr( cParam2, n ) )
            ENDIF
            oWindow:Resize( Val( cParam2 ), n, ;
               oWindow:nBottom + Val( cParam2 ) - oWindow:nTop, ;
               oWindow:nRight + n - oWindow:nLeft )
            ::lWindowsAutoSized := .F.
         ENDIF
      CASE hb_LeftEq( "SIZE", cParam1 )
         IF Empty( cParam2 )
            ::NotSupported()
         ELSE
            IF Val( cParam2 ) >= 2 .AND. ;
              ( n := At( " ", cParam2 ) ) > 0 .AND. Val( SubStr( cParam2, n ) ) > 0
               oWindow := ::aWindows[ ::nCurrentWindow ]
               oWindow:Resize( oWindow:nTop, oWindow:nLeft, ;
                               Val( cParam2 ) - 1 + oWindow:nTop, ;
                               Val( SubStr( cParam2, n ) ) - 1 + oWindow:nLeft )
               ::lWindowsAutoSized := .F.
            ENDIF
         ENDIF
      CASE hb_LeftEq( "ZOOM", cParam1 ) .OR. ;
           hb_LeftEq( "ICONIZE", cParam1 ) .OR. ;
           hb_LeftEq( "TILE", cParam1 )
         ::NotSupported()
      OTHERWISE
         cResult := "Command error"
      ENDCASE

   CASE cCommand == "WP"
      ::WatchpointAdd( cParams )

   OTHERWISE
      cResult := "Command error"

   ENDCASE

   IF ::lActive

      hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ;
                    Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ), ;
                    __dbgColors()[ 2 ] )
      IF ! Empty( cResult )
         ::CommandWindowDisplay( cResult, .F. )
      ENDIF
   ENDIF

   RETURN cResult


METHOD PROCEDURE DoScript( cFileName ) CLASS HBDebugger

   LOCAL nPos
   LOCAL cLine

   IF hb_FileExists( cFileName )
      FOR EACH cLine IN __dbgTextToArray( MemoRead( cFileName ) )
         cLine := AllTrim( cLine )
         IF ::lActive .OR. ( ( nPos := At( " ", cLine ) ) > 0 .AND. ;
            hb_LeftEqI( "OPTIONS", Left( cLine, nPos - 1 ) ) )
            // In inactive debugger, only "OPTIONS" commands can be executed safely
            ::DoCommand( cLine )
         ENDIF
      NEXT
   ENDIF

   RETURN


METHOD PROCEDURE EditColor( nColor, oBrwColors ) CLASS HBDebugger

   LOCAL cColor := '"' + ::aColors[ nColor ] + '"'

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   IF __dbgInput( Row(), oBrwColors:nLeft + oBrwColors:GetColumn( 1 ):width + 1, ;
                  oBrwColors:getColumn( 2 ):Width, ;
                  @cColor, __dbgExprValidBlock( "C" ), ;
                  SubStr( ::ClrModal(), 5 ) )
      ::aColors[ nColor ] := &cColor
   ENDIF

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   RETURN

METHOD PROCEDURE EditSet( nSet, oBrwSets ) CLASS HBDebugger

   LOCAL cSet  := PadR( __dbgValToExp( Set( nSet ) ), ;
                        oBrwSets:getColumn( 2 ):Width )
   LOCAL cType := ValType( Set( nSet ) )

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

   IF __dbgInput( Row(), oBrwSets:nLeft + oBrwSets:GetColumn( 1 ):width + 1, ;
                  oBrwSets:getColumn( 2 ):Width, ;
                  @cSet, __dbgExprValidBlock( cType ), ;
                  SubStr( ::ClrModal(), 5 ), 256 )
      Set( nSet, &cSet )
   ENDIF

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

   RETURN


METHOD PROCEDURE EditVar( nVar ) CLASS HBDebugger

   LOCAL cVarName := ::aVars[ nVar ][ 1 ]
   LOCAL cVarStr
   LOCAL oErr

   LOCAL uVarValue := ::VarGetValue( ::aVars[ nVar ] )

   IF ValType( uVarValue ) $ "AHOPB"
      ::InputBox( cVarName, uVarValue,, .F. )
   ELSE
      cVarStr := ::InputBox( cVarName, __dbgValToExp( uVarValue ), __dbgExprValidBlock() )

      IF LastKey() != K_ESC
         BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
            ::VarSetValue( ::aVars[ nVar ], &cVarStr )
         RECOVER USING oErr
            __dbgAlert( oErr:description )
         END SEQUENCE
      ENDIF
   ENDIF

   ::oBrwVars:RefreshCurrent()
   ::oBrwVars:ForceStable()

   RETURN


METHOD FindNext() CLASS HBDebugger
   RETURN ::Locate( 1, ::cSearchString )


METHOD FindPrevious() CLASS HBDebugger
   RETURN ::Locate( 2, ::cSearchString )


METHOD GetExprValue( xExpr, lValid ) CLASS HBDebugger

   LOCAL xResult
   LOCAL oErr

   lValid := .F.

   xResult := __dbgGetExprValue( ::pInfo, xExpr, @lValid )
   IF ! lValid
      oErr := xResult
      IF oErr:ClassName() == "ERROR"
         xResult := oErr:operation + ": " + oErr:description
         IF HB_ISARRAY( oErr:args )
            xResult += "; arguments:"
            AEval( oErr:args, {| x, i | xResult += iif( i == 1, " ", ", " ) + ;
                                                   __dbgValToStr( x ) } )
         ENDIF
      ELSE
         xResult := "Syntax error"
      ENDIF
   ENDIF

   RETURN xResult


METHOD GetSourceFiles() CLASS HBDebugger
   RETURN __dbgGetSourceFiles( ::pInfo )


METHOD ModuleMatch( cModuleName1, cModuleName2 ) CLASS HBDebugger
   RETURN __dbgModuleMatch( ::pInfo, cModuleName1, cModuleName2 )


METHOD PROCEDURE Global() CLASS HBDebugger

   ::lShowGlobals := ! ::lShowGlobals
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Go() CLASS HBDebugger

   // we are starting to run again so reset to the deepest call if
   // displaying stack
   IF ::oBrwStack != NIL
      ::oBrwStack:GoTop()
   ENDIF
   ::RestoreAppScreen()
   ::RestoreAppState()
   __dbgSetGo( ::pInfo )
   ::Exit()

   RETURN


METHOD PROCEDURE GotoLine( nLine ) CLASS HBDebugger

   ::oBrwText:GotoLine( nLine )

   RETURN


METHOD PROCEDURE HandleEvent() CLASS HBDebugger

   LOCAL nPopup
   LOCAL oWnd
   LOCAL nKey
   LOCAL nMRow
   LOCAL nMCol

   IF ::lAnimate
      IF ::nSpeed != 0
         Inkey( ::nSpeed / 10 )
      ENDIF
      IF __dbgInvokeDebug()  // NextKey() == K_ALT_D
         ::lAnimate := .F.
      ELSE
         ::Step()
         RETURN
      ENDIF
   ENDIF

   ::lEnd := .F.

   DO WHILE ! ::lEnd

      IF ::oWndCommand:lFocused
         ::oGetCommand:showCursor()
      ELSEIF ::oWndCode:lFocused .AND. ::oBrwText != NIL
         ::oBrwText:ForceStable()
         SetCursor( SC_SPECIAL1 )
      ELSE
         SetCursor( SC_NONE )
      ENDIF
      nKey := __dbgInkey()
      SetCursor( SC_NONE )

      IF nKey == K_ALT_X
         t_oDebugger:Quit()
      ELSEIF ::oPullDown:IsOpen()
         ::oPullDown:ProcessKey( nKey )
         IF ::oPullDown:nOpenPopup == 0 // Closed
            ::aWindows[ ::nCurrentWindow ]:Show( .T. )
         ENDIF
      ELSE
         SWITCH nKey
         CASE K_LDBLCLK

            IF MRow() != 0 .AND. MRow() != ::nMaxRow

               nMRow := MRow()
               nMCol := MCol()
               FOR EACH oWnd IN ::aWindows
                  IF oWnd:IsOver( nMRow, nMCol )
                     IF ! oWnd:lFocused
                        ::aWindows[ ::nCurrentWindow ]:Show( .F. )
                        ::nCurrentWindow := oWnd:__enumIndex()
                        oWnd:Show( .T. )
                     ENDIF
                     oWnd:LDblClick( nMRow, nMCol )
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            EXIT

         CASE K_LBUTTONDOWN

            IF MRow() == 0

               IF ( nPopup := ::oPullDown:GetItemOrdByCoors( 0, MCol() ) ) != 0
                  ::oPullDown:ShowPopup( nPopup )
               ENDIF

            ELSEIF MRow() != ::nMaxRow

               nMRow := MRow()
               nMCol := MCol()
               FOR EACH oWnd IN ::aWindows
                  IF oWnd:IsOver( nMRow, nMCol )
                     IF ! oWnd:lFocused
                        ::aWindows[ ::nCurrentWindow ]:Show( .F. )
                        ::nCurrentWindow := oWnd:__enumIndex()
                        oWnd:Show( .T. )
                     ENDIF
                     oWnd:LButtonDown( nMRow, nMCol )
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            EXIT

         CASE K_RBUTTONDOWN
            EXIT
#if 0
         CASE K_ESC
            ::RestoreAppStatus()
            t_oDebugger := NIL
            s_lExit := .T.
            DispEnd()
            ::Exit()
            EXIT
#endif

         CASE K_ENTER
            IF ! Empty( ::oGetCommand:getValue() )
               ::oWndCommand:KeyPressed( nKey )
               EXIT
            ENDIF
         CASE K_UP
         CASE K_DOWN
         CASE K_LEFT
         CASE K_RIGHT
         CASE K_PGUP
         CASE K_PGDN
         CASE K_HOME
         CASE K_END
         CASE K_DEL
         CASE K_CTRL_PGUP
         CASE K_CTRL_PGDN
         CASE K_CTRL_HOME
         CASE K_CTRL_END
         CASE K_CTRL_ENTER
            ::aWindows[ ::nCurrentWindow ]:KeyPressed( nKey )
            EXIT

         CASE K_ALT_U /* Move the border between Command and Code windows Up */
            ::ResizeCmdWnd( 1 )
            EXIT

         CASE K_ALT_D /* Move the border between Command and Code windows Down */
            ::ResizeCmdWnd( -1 )
            EXIT

         CASE K_ALT_G /* Grow active window */
         CASE K_ALT_S /* Shrink active window */
            ::NotSupported()
            EXIT

         CASE K_F1
            ::ShowHelp()
            EXIT

         CASE K_F2
            ::NotSupported()
            EXIT

         CASE K_F4
            ::ShowAppScreen()
            EXIT

         CASE K_F5
            ::Go()
            EXIT

         CASE K_CTRL_F5
            ::NextRoutine()
            EXIT

         CASE K_F6
            ::ShowWorkAreas()
            EXIT

         CASE K_F7
            ::ToCursor()
            EXIT

         CASE K_F8
            ::Step()
            EXIT

         CASE K_F9
            ::BreakPointToggle()
            EXIT

         CASE K_F10
            ::Trace()
            EXIT

         CASE K_TAB
            ::NextWindow()
            EXIT

         CASE K_SH_TAB
            ::PrevWindow()
            EXIT

         OTHERWISE
            IF ! ::OpenMenu( __dbgAltToKey( nKey ) )
               ::oWndCommand:KeyPressed( nKey )
            ENDIF
         ENDSWITCH
      ENDIF
   ENDDO

   RETURN


METHOD PROCEDURE Hide() CLASS HBDebugger

   ::CloseDebuggerWindow()

   RETURN


METHOD PROCEDURE HideCallStack() CLASS HBDebugger

   ::lShowCallStack := .F.

   IF ::oWndStack != NIL
      DispBegin()
      ::oWndStack:Hide()
      IF ::aWindows[ ::nCurrentWindow ] == ::oWndStack
         ::NextWindow()
      ENDIF
      ::RemoveWindow( ::oWndStack )
      ::oWndStack := NIL

      ::oWndCode:Resize( ,,, ::oWndCode:nRight + 16 )
      IF ::oWndVars != NIL
         ::oWndVars:Resize( ,,, ::oWndVars:nRight + 16 )
      ENDIF
      IF ::oWndPnt != NIL
         ::oWndPnt:Resize( ,,, ::oWndPnt:nRight + 16 )
      ENDIF
      DispEnd()
   ENDIF

   RETURN


METHOD PROCEDURE HideVars() CLASS HBDebugger

   LOCAL nTop

   IF ::oWndVars == NIL
      RETURN
   ENDIF

   ::oWndVars:Hide()
   IF ::oWndPnt == NIL
      nTop := 1
   ELSE
      ::oWndPnt:Resize( 1, , ::oWndPnt:nBottom - ( ::oWndPnt:nTop - 1 ) )
      ::oBrwPnt:Resize( 2, , ::oWndPnt:nBottom - 1 )
      nTop := ::oWndPnt:nBottom + 1
   ENDIF
   ::oWndCode:Resize( nTop )
   IF ::oBrwText != NIL
      ::oBrwText:Resize( ::oWndCode:nTop + 1 )
   ENDIF

   IF ::aWindows[ ::nCurrentWindow ] == ::oWndVars
      ::NextWindow()
   ENDIF

   RETURN


METHOD InputBox( cMsg, uValue, bValid, lEditable ) CLASS HBDebugger

   LOCAL nTop    := Int( ( ::nMaxRow / 2 ) - 5 )
   LOCAL nLeft   := Int( ( ::nMaxCol / 2 ) - 25 )
   LOCAL nBottom := nTop + 2
   LOCAL nRight  := nLeft + 50
   LOCAL cType   := ValType( uValue )
   LOCAL nWidth  := nRight - nLeft - 1
   LOCAL uTemp
   LOCAL lExit
   LOCAL oWndInput := HBDbWindow():New( nTop, nLeft, nBottom, nRight, cMsg, ;
      ::oPullDown:cClrPopup )

   oWndInput:lShadow := .T.
   oWndInput:Show()

   uTemp := uValue

   IF hb_defaultValue( lEditable, .T. )

      IF ! cType == "C" .OR. Len( uValue ) < nWidth
         uTemp := PadR( iif( cType == "N", hb_ntos( uValue ), ;
                                           uValue ), nWidth )
      ENDIF
      IF bValid == NIL .AND. cType $ "N"
         bValid := __dbgExprValidBlock( cType )
      ENDIF
      __dbgInput( nTop + 1, nLeft + 1, nWidth, @uTemp, bValid, ;
                  __dbgColors()[ 5 ], Max( Max( nWidth, Len( uTemp ) ), 256 ) )
      SWITCH cType
      CASE "C" ; uTemp := AllTrim( uTemp ) ; EXIT
      CASE "N" ; uTemp := Val( uTemp )     ; EXIT
      ENDSWITCH

   ELSE

      hb_DispOutAt( nTop + 1, nLeft + 1, left( __dbgValToStr( uValue ), nRight - nLeft - 1 ), ;
                    __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] )
      SetPos( nTop + 1, nLeft + 1 )

      lExit := .F.

      DO WHILE ! lExit

         SWITCH __dbgInkey()
         CASE K_ESC
            lExit := .T.
            EXIT

         CASE K_ENTER

            SWITCH cType
            CASE "A"
               IF Len( uValue ) == 0
                  __dbgAlert( "Array is empty" )
               ELSE
                  __dbgArrays( uValue, cMsg )
               ENDIF
               LOOP
            CASE "H"
               IF Len( uValue ) == 0
                  __dbgAlert( "Hash is empty" )
               ELSE
                  __dbgHashes( uValue, cMsg )
               ENDIF
               LOOP
            CASE "O"
               __dbgObject( uValue, cMsg )
               LOOP
            ENDSWITCH

         OTHERWISE
            __dbgAlert( "Value cannot be edited" )

         ENDSWITCH
      ENDDO
   ENDIF

   oWndInput:Hide()

   RETURN uTemp


METHOD PROCEDURE Inspect( uValue, cValueName ) CLASS HBDebugger

   ::InputBox( uValue, cValueName,, .F. )

   RETURN


METHOD IsValidStopLine( cName, nLine ) CLASS HBDebugger
   RETURN __dbgIsValidStopLine( ::pInfo, cName, nLine )


METHOD RunAtStartup( lRunAtStartup ) CLASS HBDebugger

   ::lRunAtStartup := hb_defaultValue( lRunAtStartup, ! ::lRunAtStartup )
   ::oPulldown:GetItemByIdent( "ALTD" ):checked := ::lRunAtStartup

   RETURN Self


METHOD LineNumbers( lLineNumbers ) CLASS HBDebugger

   ::lLineNumbers := hb_defaultValue( lLineNumbers, ! ::lLineNumbers )
   ::oPulldown:GetItemByIdent( "LINE" ):checked := ::lLineNumbers
   IF ::oBrwText != NIL
      ::oBrwText:lLineNumbers := ::lLineNumbers
      ::oBrwText:RefreshAll()
   ENDIF

   RETURN Self


METHOD ListBox( cCaption, aItems ) CLASS HBDebugger

   LOCAL nItems
   LOCAL nMaxWid
   LOCAL nLeft
   LOCAL nTop
   LOCAL nBottom
   LOCAL nRight
   LOCAL oWndList
   LOCAL aColors
   LOCAL n

   nItems := Len( aItems )
   nMaxWid := Len( cCaption ) + 2
   AEval( aItems, {| x | nMaxWid := Max( Len( x ), nMaxWid ) } )
   nMaxWid += 2

   nTop    := Int( ( ::nMaxRow / 2 ) - Min( nItems, ::nMaxRow - 5 ) / 2 )
   nBottom := Int( ( ::nMaxRow / 2 ) + Min( nItems, ::nMaxRow - 5 ) / 2 + 1 )
   nLeft   := Int( ( ::nMaxCol / 2 ) - Min( nMaxWid, ::nMaxCol * 3 / 2 ) / 2 )
   nRight  := Int( ( ::nMaxCol / 2 ) + Min( nMaxWid, ::nMaxCol * 3 / 2 ) / 2 )
   oWndList := HBDbWindow():new( nTop, nLeft, nBottom, nRight, cCaption, ;
      ::oPullDown:cClrPopup )
   oWndList:lShadow := .T.
   oWndList:Show()

   aColors := __dbgColors()
   n := __dbgAChoice( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1, aItems, ;
      aColors[ 8 ] + "," + aColors[ 10 ] )
   oWndList:Hide()

   RETURN n


METHOD PROCEDURE LoadCallStack() CLASS HBDebugger

   LOCAL i
   LOCAL nDebugLevel
   LOCAL nCurrLevel
   LOCAL nLevel
   LOCAL nPos

   ::aProcStack := Array( ::nProcLevel )

   nCurrLevel := __dbgProcLevel() - 1
   nDebugLevel := nCurrLevel - ::nProcLevel + 1

   FOR i := nDebugLevel TO nCurrLevel
      nLevel := nCurrLevel - i + 1
      IF ( nPos := AScan( ::aCallStack, {| a | a[ HB_DBG_CS_LEVEL ] == nLevel } ) ) > 0
         // a procedure with debug info
         ::aProcStack[ i - nDebugLevel + 1 ] := ::aCallStack[ nPos ]
      ELSE
         ::aProcStack[ i - nDebugLevel + 1 ] := iif( ProcLine( i ) != 0, ;
            { ProcFile( i ), ProcName( i ), ProcLine( i ), nLevel, {}, {} }, ;
            {, ProcName( i ) + "(" + hb_ntos( ProcLine( i ) ) + ")", , nLevel, , } )
      ENDIF
   NEXT

   RETURN


METHOD PROCEDURE LoadColors() CLASS HBDebugger

   LOCAL oWnd

   ::oPullDown:LoadColors()
   IF ::lActive
      ::oPullDown:Refresh()
      ::BarDisplay()
   ENDIF
   FOR EACH oWnd IN ::aWindows
      oWnd:LoadColors()
      IF ::lActive
         oWnd:Refresh()
      ENDIF
   NEXT

   RETURN


METHOD PROCEDURE LoadSettings() CLASS HBDebugger

   ::DoScript( ::cSettingsFileName )

   RETURN


METHOD PROCEDURE LoadVars() CLASS HBDebugger  // updates monitored variables

   LOCAL nCount
   LOCAL n
   LOCAL m
   LOCAL xValue
   LOCAL cName
   LOCAL hSkip

   LOCAL aBVars := {}

   IF ::lShowPublics
      nCount := __mvDbgInfo( HB_MV_PUBLIC )
      FOR n := nCount TO 1 STEP -1
         xValue := __mvDbgInfo( HB_MV_PUBLIC, n, @cName )
         AAdd( aBVars, { cName, xValue, "Public" } )
      NEXT
   ENDIF

   IF ::lShowPrivates
      /* CA-Cl*pper shows only local private variables in monitor
       * We are marking non local private variables with "^" character
       */
      nCount := __mvDbgInfo( HB_MV_PRIVATE )
      IF nCount > 0
         m := __mvDbgInfo( HB_MV_PRIVATE_LOCAL, ::nProcLevel )
         hSkip := { => }
         hb_HAllocate( hSkip, nCount )
         FOR n := nCount TO 1 STEP -1
            xValue := __mvDbgInfo( HB_MV_PRIVATE, n, @cName )
            IF ! cName $ hSkip
               AAdd( aBVars, { cName, xValue, iif( m > 0, "Private", "Private^" ) } )
               hSkip[ cName ] := NIL
            ENDIF
            --m
         NEXT
      ENDIF
   ENDIF

   IF ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_LINE ] != NIL
      IF ::lShowGlobals
         cName := ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_MODULE ]
         FOR EACH n IN ::aModules
            IF ! ::lShowAllGlobals
               IF ! ::ModuleMatch( n[ HB_DBG_MOD_NAME ], cName )
                  LOOP
               ENDIF
            ENDIF
            FOR EACH m IN n[ HB_DBG_MOD_GLOBALS ]
               AAdd( aBVars, m )
            NEXT
            IF ! ::lShowAllGlobals
               FOR EACH m IN n[ HB_DBG_MOD_EXTGLOBALS ]
                  AAdd( aBVars, m )
               NEXT
            ENDIF
         NEXT
      ENDIF

      IF ::lShowStatics
         cName := ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_MODULE ]
         IF ( n := AScan( ::aModules, {| a | ::ModuleMatch( a[ HB_DBG_MOD_NAME ], cName ) } ) ) > 0
            FOR EACH m IN ::aModules[ n ][ HB_DBG_MOD_STATICS ]
               AAdd( aBVars, m )
            NEXT
         ENDIF
         FOR EACH n IN ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_STATICS ]
            AAdd( aBVars, n )
         NEXT
      ENDIF

      IF ::lShowLocals
         FOR EACH n IN ::aProcStack[ ::oBrwStack:Cargo ][ HB_DBG_CS_LOCALS ]
            cName := n[ HB_DBG_VAR_NAME ]
            // Is there another var with this name ?
            IF ( m := AScan( aBVars, {| aVar | aVar[ HB_DBG_VAR_NAME ] == cName .AND. aVar[ HB_DBG_VAR_TYPE ] == "S" } ) ) > 0
               aBVars[ m ] := n
            ELSE
               AAdd( aBVars, n )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF ::oBrwVars != NIL .AND. ::oBrwVars:cargo[ 1 ] > Len( aBVars )
      ::oBrwVars:GoTop()
   ENDIF
   ::aVars := aBVars
   IF ::lSortVars .AND. ! Empty( ::aVars )
      ::Sort()
   ENDIF

   RETURN


METHOD PROCEDURE Local() CLASS HBDebugger

   ::lShowLocals := ! ::lShowLocals
   ::RefreshVars()

   RETURN


METHOD Locate( nMode, cValue ) CLASS HBDebugger

   LOCAL lFound

   IF Empty( cValue )
      cValue := ::InputBox( "Search string", ::cSearchString )
      IF Empty( cValue )
         RETURN NIL
      ENDIF
   ENDIF

   ::cSearchString := cValue

   lFound := ::oBrwText:Search( ::cSearchString, ::lCaseSensitive, nMode )

   RETURN lFound


METHOD LocatePrgPath( cPrgName ) CLASS HBDebugger

   LOCAL cRetPrgName
   LOCAL cDir

   FOR EACH cDir IN ::aPathDirs
      cRetPrgName := cDir + hb_ps() + cPrgName
      IF hb_FileExists( cRetPrgName )
         RETURN cRetPrgName
      ENDIF
   NEXT

   RETURN NIL


METHOD PROCEDURE MonoDisplay() CLASS HBDebugger

   ::lMonoDisplay := ! ::lMonoDisplay
   ::oPullDown:GetItemByIdent( "MONO" ):checked := ::lMonoDisplay
   ::LoadColors()

   RETURN


METHOD NextRoutine() CLASS HBDebugger

   ::RestoreAppScreen()
   ::RestoreAppState()
   __dbgSetNextRoutine( ::pInfo )
   ::Exit()

   RETURN Self


METHOD PROCEDURE NextWindow() CLASS HBDebugger

   LOCAL oWnd

   IF Len( ::aWindows ) > 0

      oWnd := ::aWindows[ ::nCurrentWindow++ ]
      oWnd:Show( .F. )
      IF ::nCurrentWindow > Len( ::aWindows )
         ::nCurrentWindow := 1
      ENDIF
      DO WHILE ! ::aWindows[ ::nCurrentWindow ]:lVisible
         ::nCurrentWindow++
         IF ::nCurrentWindow > Len( ::aWindows )
            ::nCurrentWindow := 1
         ENDIF
      ENDDO
      oWnd := ::aWindows[ ::nCurrentWindow ]
      oWnd:Show( .T. )
   ENDIF

   RETURN


METHOD PROCEDURE Open( cFileName ) CLASS HBDebugger

   LOCAL nFileName
   LOCAL cRealName
   LOCAL aFiles

   IF Empty( cFileName )
      aFiles := ::GetSourceFiles()
      ASort( aFiles )
      hb_AIns( aFiles, 1, "(Another file)", .T. )

      nFileName := ::ListBox( "Please choose a source file", aFiles )
      SWITCH nFileName
      CASE 0
         RETURN
      CASE 1
         cFileName := AllTrim( ::InputBox( "Please enter the filename" ) )
         EXIT
      OTHERWISE
         cFileName := aFiles[ nFileName ]
      ENDSWITCH
   ENDIF

   IF ! Empty( cFileName ) .AND. ;
      ( ! HB_ISSTRING( ::cPrgName ) .OR. ! hb_FileMatch( cFileName, ::cPrgName ) )

      IF ! hb_FileExists( cFileName ) .AND. ! Empty( ::cPathForFiles )
         cRealName := ::LocatePrgPath( cFileName )
         IF Empty( cRealName )
            __dbgAlert( "File '" + cFileName + "' not found!" )
            RETURN
         ENDIF
         cFileName := cRealName
      ENDIF
      ::cPrgName := cFileName
      ::lPPO := ( Lower( hb_FNameExt( cFileName ) ) == ".ppo" )
      ::oPulldown:GetItemByIdent( "PPO" ):Checked := ::lPPO
      ::oBrwText := HBBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1, ;
         ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, cFileName, ;
         __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] + "," + ;
         __dbgColors()[ 3 ] + "," + __dbgColors()[ 6 ], ;
         ::lLineNumbers, ::nTabWidth )
      ::oWndCode:Browser := ::oBrwText
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()       // to force the window caption to update
   ENDIF

   RETURN

METHOD OpenMenu( cName ) CLASS HBDebugger

   LOCAL nPopup := ::oPullDown:GetHotKeyPos( Left( cName, 1 ) )

   IF nPopup == 0
      RETURN .F.
   ENDIF
   IF ::oPullDown:nOpenPopup != nPopup
      ::oPullDown:ShowPopup( nPopup )
   ENDIF

   RETURN .T.

METHOD OpenPPO() CLASS HBDebugger

   LOCAL lSuccess

   IF Empty( ::cPrgName )
      RETURN .F.
   ENDIF

   IF Lower( hb_FNameExt( ::cPrgName ) ) == ".ppo"
      ::cPrgName := hb_FNameExtSet( ::cPrgName, ".prg" )
      lSuccess := hb_FileExists( ::cPrgName )
      ::lPPO := ! lSuccess
   ELSE
      ::cPrgName := hb_FNameExtSet( ::cPrgName, ".ppo" )
      lSuccess := hb_FileExists( ::cPrgName )
      ::lPPO := lSuccess
   ENDIF

   IF lSuccess
      ::oBrwText := HBBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1, ;
         ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, ::cPrgName, ;
         __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] + "," + ;
         __dbgColors()[ 3 ] + "," + __dbgColors()[ 6 ], ::lLineNumbers, ::nTabWidth )
      ::oWndCode:Browser := ::oBrwText
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()  // to force the window caption to update
   ENDIF

   ::oPullDown:GetItemByIdent( "PPO" ):checked := ::lPPO

   RETURN lSuccess


METHOD PROCEDURE OSShell() CLASS HBDebugger

   LOCAL cImage := __dbgSaveScreen()
   LOCAL cColors := SetColor()
   LOCAL oE

   SetColor( "W/N" )
   CLS
   QOut( "Type 'exit' to RETURN to the Debugger" )
   SetCursor( SC_NORMAL )     // standard cursor for OS shell

   BEGIN SEQUENCE WITH {| objErr | Break( objErr ) }

#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
      hb_run( GetEnv( "COMSPEC" ) )
#elif defined( __PLATFORM__UNIX )
      hb_run( GetEnv( "SHELL" ) )
#else
      ::NotSupported()
#endif

   RECOVER USING oE

      __dbgAlert( "Error: " + oE:description )

   END SEQUENCE

   SetCursor( SC_NONE )
   __dbgRestScreen( ,,,, cImage )
   SetColor( cColors )

   RETURN


METHOD PROCEDURE Quit() CLASS HBDebugger

   ::Exit()
   ::Hide()
   __dbgSetQuit( ::pInfo )
   t_oDebugger := NIL

   __Quit()

   RETURN


METHOD PathForFiles( cPathForFiles ) CLASS HBDebugger

   IF cPathForFiles == NIL
      cPathForFiles := ::InputBox( "Search path for source files:", ::cPathForFiles )
   ENDIF
   ::cPathForFiles := cPathForFiles
   ::aPathDirs := PathToArray( ::cPathForFiles )

   ::Resume()

   RETURN Self


METHOD PROCEDURE PrevWindow() CLASS HBDebugger

   LOCAL oWnd

   IF Len( ::aWindows ) > 0

      oWnd := ::aWindows[ ::nCurrentWindow-- ]
      oWnd:Show( .F. )
      IF ::nCurrentWindow < 1
         ::nCurrentWindow := Len( ::aWindows )
      ENDIF
      DO WHILE ! ::aWindows[ ::nCurrentWindow ]:lVisible
         ::nCurrentWindow--
         IF ::nCurrentWindow < 1
            ::nCurrentWindow := Len( ::aWindows )
         ENDIF
      ENDDO
      oWnd := ::aWindows[ ::nCurrentWindow ]
      oWnd:Show( .T. )
   ENDIF

   RETURN


METHOD PROCEDURE Private() CLASS HBDebugger

   ::lShowPrivates := ! ::lShowPrivates
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Public() CLASS HBDebugger

   ::lShowPublics := ! ::lShowPublics
   ::RefreshVars()

   RETURN


METHOD PROCEDURE RefreshVars() CLASS HBDebugger

   ::oPulldown:GetItemByIdent( "GLOBAL" ):checked := ::lShowGlobals
   ::oPulldown:GetItemByIdent( "LOCAL" ):checked := ::lShowLocals
   ::oPulldown:GetItemByIdent( "PRIVATE" ):checked := ::lShowPrivates
   ::oPulldown:GetItemByIdent( "PUBLIC" ):checked := ::lShowPublics
   ::oPulldown:GetItemByIdent( "STATIC" ):checked := ::lShowStatics
   ::oPulldown:GetItemByIdent( "ALL" ):checked := ::lAll
   ::oPulldown:GetItemByIdent( "SHOWALLGLOBALS" ):checked := ::lShowAllGlobals

   IF ::lActive
      IF ::lShowGlobals .OR. ::lShowPublics .OR. ::lShowPrivates .OR. ::lShowStatics .OR. ::lShowLocals
         ::LoadVars()
         ::ShowVars()
      ELSE
         ::HideVars()
      ENDIF
   ENDIF

   RETURN


METHOD PROCEDURE RemoveWindow( oWnd ) CLASS HBDebugger

   LOCAL n

   IF ( n := hb_AScan( ::aWindows, oWnd,,, .T. ) ) > 0
      ::aWindows := hb_ADel( ::aWindows, n, .T. )
   ENDIF

   ::nCurrentWindow := 1

   RETURN


METHOD ResizeWindows( oWindow ) CLASS HBDebugger

   LOCAL oWindow2
   LOCAL nTop
   LOCAL lVisible2 := .F.

   DO CASE
   CASE oWindow == ::oWndVars
      oWindow2 := ::oWndPnt
   CASE oWindow == ::oWndPnt
      oWindow2 := ::oWndVars
   ENDCASE

   DispBegin()
   IF oWindow2 == NIL
      nTop := oWindow:nBottom + 1
   ELSE
      lVisible2 := oWindow2:lVisible
      IF oWindow2:lVisible
         IF oWindow:nTop < oWindow2:nTop
            nTop := oWindow2:nBottom - oWindow2:nTop + 1
            oWindow2:Resize( oWindow:nBottom + 1,, oWindow:nBottom + nTop )
         ELSE
            nTop := oWindow:nBottom - oWindow:nTop + 1
            oWindow:Resize( oWindow2:nBottom + 1,, oWindow2:nBottom + nTop )
         ENDIF
         nTop := Max( oWindow:nBottom, oWindow2:nBottom ) + 1
      ELSE
         IF oWindow:nTop > 1
            nTop := oWindow:nBottom - oWindow:nTop + 1
            oWindow:Resize( 1,, nTop )
         ENDIF
         nTop := oWindow:nBottom + 1
      ENDIF
   ENDIF

   oWindow:hide()
   IF oWindow2 != NIL
      oWindow2:hide()
   ENDIF

   ::oWndCode:Resize( nTop )

   IF oWindow2 != NIL .AND. lVisible2
      oWindow2:Show()
   ENDIF
   oWindow:Show()
   DispEnd()

   RETURN Self


METHOD PROCEDURE RestoreAppScreen() CLASS HBDebugger

   LOCAL i

   ::CloseDebuggerWindow()

   FOR i := 1 TO ::nAppDispCount
      DispBegin()
   NEXT

   RETURN


METHOD PROCEDURE RestoreAppState() CLASS HBDebugger

   Set( _SET_DIRCASE, ::nAppDirCase )
   Set( _SET_FILECASE, ::nAppFileCase )
   Set( _SET_TYPEAHEAD, ::nAppTypeAhead )
   hb_keySetLast( ::nAppLastKey )

   RETURN


METHOD PROCEDURE RestoreSettings( cFileName ) CLASS HBDebugger

   IF Empty( cFileName )
      ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )
      IF LastKey() == K_ESC
         RETURN
      ENDIF
   ELSE
      ::cSettingsFileName := cFileName
   ENDIF

   ::LoadSettings()
   ::ShowVars()

   RETURN


METHOD PROCEDURE SaveAppScreen() CLASS HBDebugger

   LOCAL nRight
   LOCAL nTop
   LOCAL i

   ::nAppDispCount := DispCount()
   FOR i := 1 TO ::nAppDispCount
      DispEnd()
   NEXT

   ::OpenDebuggerWindow()

   IF ::nMaxRow != MaxRow() .OR. ::nMaxCol != MaxCol()
      DispBegin()
      ::nMaxRow := MaxRow()
      ::nMaxCol := MaxCol()
      nTop := 1
      nRight := ::nMaxCol
      ::oWndCommand:Resize( ::nMaxRow - ::nCmdWndHight - 2, 0, ::nMaxRow - 1, ::nMaxCol )
      ::oGetCommand:newPos( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3 )
      ::oBrwStack:nTop := 2
      ::oBrwStack:nLeft := ::nMaxCol - 14
      ::oBrwStack:nRight := ::nMaxCol - 1
      ::oBrwStack:nBottom := ::nMaxRow - ::nCmdWndHight - 4
      IF ::oWndStack != NIL
         nRight -= 16
         ::oWndStack:Resize( , nRight + 1, ::nMaxRow - ::nCmdWndHight - 3, ::nMaxCol )
      ENDIF
      IF ::oWndVars != NIL
         ::oWndVars:Resize( , , , nRight )
         nTop := Max( nTop, ::oWndVars:nBottom + 1 )
      ENDIF
      IF ::oWndPnt != NIL
         ::oWndPnt:Resize( , , , nRight )
         nTop := Max( nTop, ::oWndPnt:nBottom + 1 )
      ENDIF
      ::oWndCode:Resize( nTop, 0, ::nMaxRow - ::nCmdWndHight - 3, nRight )
      ::oPullDown:Refresh()
      ::BarDisplay()
      DispEnd()
   ENDIF

   RETURN


METHOD PROCEDURE SaveAppState() CLASS HBDebugger

   ::nAppDirCase := Set( _SET_DIRCASE, 0 )
   ::nAppFileCase := Set( _SET_FILECASE, 0 )
   ::nAppTypeAhead := Set( _SET_TYPEAHEAD, 16 )
   ::nAppLastKey := LastKey()

   RETURN


METHOD PROCEDURE SaveSettings( cFileName ) CLASS HBDebugger

   LOCAL cInfo := ""
   LOCAL n
   LOCAL oWnd
   LOCAL aBreak, aWatch

   IF Empty( cFileName )
      ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )
      IF LastKey() == K_ESC
         RETURN
      ENDIF
   ELSE
      ::cSettingsFileName := cFileName
   ENDIF

   IF ! Empty( ::cPathForFiles )
      cInfo += "Options Path " + ::cPathForFiles + hb_eol()
   ENDIF

   cInfo += "Options Colors {"
   FOR EACH n IN ::aColors
      cInfo += '"' + n + '"'
      IF ! n:__enumIsLast()
         cInfo += ","
      ENDIF
   NEXT
   cInfo += "}" + hb_eol()

   IF ::lMonoDisplay
      cInfo += "Options mono " + hb_eol()
   ENDIF

   IF ! ::lRunAtStartup
      cInfo += "Options NoRunAtStartup " + hb_eol()
   ENDIF

   IF ::nSpeed != 0
      cInfo += "Run Speed " + hb_ntos( ::nSpeed ) + hb_eol()
   ENDIF

   IF ::nTabWidth != 4
      cInfo += "Options Tab " + hb_ntos( ::nTabWidth ) + hb_eol()
   ENDIF

   IF ::lShowStatics
      cInfo += "Monitor Static" + hb_eol()
   ENDIF

   IF ::lShowPublics
      cInfo += "Monitor Public" + hb_eol()
   ENDIF

   IF ::lShowLocals
      cInfo += "Monitor Local" + hb_eol()
   ENDIF

   IF ::lShowPrivates
      cInfo += "Monitor Private" + hb_eol()
   ENDIF

   IF ::lShowGlobals
      cInfo += "Monitor Global" + hb_eol()
   ENDIF

   IF ::lSortVars
      cInfo += "Monitor Sort" + hb_eol()
   ENDIF

   IF ::lShowCallStack
      cInfo += "View CallStack" + hb_eol()
   ENDIF

   IF ! ::lLineNumbers
      cInfo += "Num Off" + hb_eol()
   ENDIF

   FOR EACH aBreak IN __dbgGetBreakPoints( ::pInfo )
      cInfo += "BP " + hb_ntos( aBreak[ HB_DBG_BP_LINE ] ) + " " + ;
               aBreak[ HB_DBG_BP_MODULE ] + hb_eol()
   NEXT

   FOR EACH aWatch IN ::aWatch
      cInfo += Upper( aWatch[ 1 ] ) + " " + aWatch[ 2 ] + hb_eol()
   NEXT

   IF ! ::lWindowsAutoSized
      /* This part of the script must be executed after all windows are created */
      FOR EACH oWnd IN ::aWindows
         cInfo += ;
            "Window Size " + hb_ntos( oWnd:nBottom - oWnd:nTop + 1 ) + " " + ;
            hb_ntos( oWnd:nRight - oWnd:nLeft + 1 ) + hb_eol() + ;
            "Window Move " + hb_ntos( oWnd:nTop ) + " " + ;
            hb_ntos( oWnd:nLeft ) + hb_eol() + ;
            "Window Next" + hb_eol()
      NEXT
   ENDIF

   hb_MemoWrit( ::cSettingsFileName, cInfo )

   RETURN


METHOD ResizeCmdWnd( nLines ) CLASS HBDebugger

   LOCAL nRight
   LOCAL nTop
   LOCAL aShow
   LOCAL oWnd

   nTop := 1
   nRight := ::nMaxCol
   IF ::oWndVars != NIL
      nTop := Max( nTop, ::oWndVars:nBottom + 1 )
   ENDIF
   IF ::oWndPnt != NIL
      nTop := Max( nTop, ::oWndPnt:nBottom + 1 )
   ENDIF
   IF ::oWndStack != NIL
      nRight -= 16
   ENDIF

   IF ::nCmdWndHight + nLines >= 2 .AND. ;
      ::nMaxRow - ( ::nCmdWndHight + nLines + 4 ) > nTop

      DispBegin()
      aShow := {}
      IF ::oWndCode:lVisible
         ::oWndCode:Hide()
         AAdd( aShow, ::oWndCode )
      ENDIF
      IF ::oWndPnt != NIL .AND. ::oWndPnt:lVisible
         ::oWndPnt:Hide()
         AAdd( aShow, ::oWndPnt )
      ENDIF
      IF ::oWndVars != NIL .AND. ::oWndVars:lVisible
         ::oWndVars:Hide()
         AAdd( aShow, ::oWndVars )
      ENDIF
      IF ::oWndStack != NIL .AND. ::oWndStack:lVisible
         ::oWndStack:Hide()
         AAdd( aShow, ::oWndStack )
      ENDIF
      IF ::oWndCommand:lVisible
         ::oWndCommand:Hide()
         AAdd( aShow, ::oWndCommand )
      ENDIF

      ::nCmdWndHight += nLines
      ::oWndCommand:Resize( ::nMaxRow - ::nCmdWndHight - 2, 0, ::nMaxRow - 1, ::nMaxCol )
      ::oGetCommand:newPos( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3 )
      ::oBrwStack:nTop := 2
      ::oBrwStack:nLeft := ::nMaxCol - 14
      ::oBrwStack:nRight := ::nMaxCol - 1
      ::oBrwStack:nBottom := ::nMaxRow - ::nCmdWndHight - 4
      ::oBrwStack:Configure()
      IF ::oWndStack != NIL
         ::oWndStack:Resize( , nRight + 1, ::nMaxRow - ::nCmdWndHight - 3, ::nMaxCol )
      ENDIF
      IF ::oWndVars != NIL
         ::oWndVars:Resize( , , , nRight )
      ENDIF
      IF ::oWndPnt != NIL
         ::oWndPnt:Resize( , , , nRight )
      ENDIF
      ::oWndCode:Resize( nTop, 0, ::nMaxRow - ::nCmdWndHight - 3, nRight )
      FOR EACH oWnd IN aShow DESCEND
         oWnd:Show()
      NEXT
      DispEnd()

   ENDIF

   RETURN Self


METHOD PROCEDURE SearchLine( cLine ) CLASS HBDebugger

   ::GotoLine( Max( 1, iif( HB_ISSTRING( cLine ) .AND. IsDigit( cLine ), ;
                            Val( cLine ), ::InputBox( "Line number", 1 ) ) ) )
   RETURN


METHOD PROCEDURE Show() CLASS HBDebugger

   ::SaveAppScreen()
   ::oPullDown:Display()
   ::oWndCode:Show( .T. )
   ::oWndCommand:Show()
   hb_DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ">", __dbgColors()[ 2 ] )

   ::BarDisplay()

   RETURN


METHOD PROCEDURE ShowAllGlobals() CLASS HBDebugger

   ::lShowAllGlobals := ! ::lShowAllGlobals
   ::RefreshVars()

   RETURN


METHOD PROCEDURE ShowAppScreen() CLASS HBDebugger

   ::CloseDebuggerWindow()

   IF LastKey() == K_LBUTTONDOWN
      __dbgInkey()
   ENDIF
   DO WHILE __dbgInkey() == K_MOUSEMOVE
   ENDDO

   ::OpenDebuggerWindow()

   RETURN


METHOD PROCEDURE ShowCallStack() CLASS HBDebugger

   ::lShowCallStack := .T.

   IF ::oWndStack == NIL

      DispBegin()
      // Resize code window
      ::oWndCode:Resize( ,,, ::oWndCode:nRight - 16 )
      // Resize vars window
      IF ::oWndVars != NIL
         ::oWndVars:Resize( ,,, ::oWndVars:nRight - 16 )
      ENDIF
      // Resize watchpoints window
      IF ::oWndPnt != NIL
         ::oWndPnt:Resize( ,,, ::oWndPnt:nRight - 16 )
      ENDIF
      DispEnd()

      IF ::aWindows[ ::nCurrentWindow ]:lFocused
         ::aWindows[ ::nCurrentWindow ]:Show( .F. )
      ENDIF

      ::oWndStack := HBDbWindow():New( 1, ::nMaxCol - 15, ;
                                       ::nMaxRow - ::nCmdWndHight - 3, ;
                                       ::nMaxCol, "Calls" )
      ::oWndStack:bKeyPressed  := {| nKey | ::CallStackProcessKey( nKey ) }
      ::oWndStack:bLButtonDown := {|| ::CallStackProcessKey( K_LBUTTONDOWN ) }

      // Maintain fixed window array order: code, monitor, watch, callstack, command
      IF ::nCurrentWindow >= Len( ::aWindows )
         ::nCurrentWindow++
      ENDIF
      hb_AIns( ::aWindows, Len( ::aWindows ), ::oWndStack, .T. )

      IF ::oBrwStack == NIL
         ::BuildBrowseStack()
      ENDIF

      ::oWndStack:bPainted := {|| ::oBrwStack:ColorSpec := __dbgColors()[ 2 ] + "," + ;
         __dbgColors()[ 5 ] + "," + __dbgColors()[ 4 ] + "," + __dbgColors()[ 6 ], ;
         ::oBrwStack:RefreshAll(), ::oBrwStack:ForceStable() }

      ::oWndStack:Show( .F. )
   ENDIF

   RETURN


METHOD PROCEDURE ShowCodeLine( nProc ) CLASS HBDebugger

   LOCAL nLine
   LOCAL cPrgName

   // we only update the stack window and up a new browse
   // to view the code if we have just broken execution
   IF ! ::lGo
      IF ::oWndStack != NIL
         ::oBrwStack:RefreshAll()
      ENDIF

      nLine := ::aProcStack[ nProc ][ HB_DBG_CS_LINE ]
      cPrgName := ::aProcStack[ nProc ][ HB_DBG_CS_MODULE ]
      IF nLine == NIL
         ::oBrwText := NIL
         ::oWndCode:Browser := NIL
         ::oWndCode:SetCaption( ::aProcStack[ nProc ][ HB_DBG_CS_FUNCTION ] + ;
            ": Code not available" )
         ::oWndCode:Refresh() // to force the window caption to update
         RETURN
      ENDIF

      IF ::lPPO
         cPrgName := hb_FNameExtSet( cPrgName, ".ppo" )
      ENDIF

      IF ! Empty( cPrgName )

         IF ! ::ModuleMatch( cPrgName, ::cPrgName ) .OR. ;
            ::oBrwText == NIL

            IF ! hb_FileExists( cPrgName ) .AND. ! Empty( ::cPathForFiles )
               cPrgName := ::LocatePrgPath( cPrgName )
            ENDIF

            ::cPrgName := cPrgName

            IF ! hb_FileExists( cPrgName )
               ::oBrwText := NIL
               ::oWndCode:Browser := NIL
               ::oWndCode:SetCaption( ::aProcStack[ nProc ][ HB_DBG_CS_MODULE ] + ;
                  "  File not found" )
               ::oWndCode:Refresh()
               RETURN
            ENDIF

            IF ::oBrwText == NIL
               ::oBrwText := HBBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1, ;
                  ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, cPrgName, ;
                  __dbgColors()[ 2 ] + "," + __dbgColors()[ 5 ] + "," + ;
                  __dbgColors()[ 3 ] + "," + __dbgColors()[ 6 ], ;
                  ::lLineNumbers, ::nTabWidth )

               ::oWndCode:Browser := ::oBrwText
            ELSE
               ::oBrwText:LoadFile( cPrgName )
            ENDIF

            ::oWndCode:bPainted := {|| iif( ::oBrwText != NIL, ::oBrwText:RefreshAll():ForceStable(), ::oWndCode:Clear() ) }
            ::oWndCode:SetCaption( ::cPrgName )
            ::oWndCode:Refresh()       // to force the window caption to update
         ENDIF
         ::oBrwText:SetActiveLine( nLine )
         ::GotoLine( nLine )
      ENDIF
   ENDIF

   RETURN


METHOD PROCEDURE ShowHelp( cTopic ) CLASS HBDebugger

   __dbgHelp( cTopic )

   RETURN


#define MAX_VARS_HEIGHT  7

METHOD PROCEDURE ShowVars() CLASS HBDebugger

   LOCAL oCol
   LOCAL lRepaint := .F.
   LOCAL nTop
   LOCAL nBottom
   LOCAL lWindowCreated := .F.
   LOCAL aColors

   IF ::lGo
      RETURN
   ENDIF

   IF ! ( ::lShowLocals .OR. ::lShowStatics .OR. ::lShowPrivates .OR. ;
      ::lShowPublics .OR. ::lShowGlobals )
      RETURN
   ENDIF

   DispBegin()

   IF ::oWndVars == NIL

      nTop := iif( ::oWndPnt != NIL .AND. ::oWndPnt:lVisible, ::oWndPnt:nBottom + 1, 1 )
      nBottom := nTop + Min( MAX_VARS_HEIGHT, Len( ::aVars ) + 1 )

      ::oWndVars := HBDbWindow():New( nTop, 0, nBottom, ;
         ::nMaxCol - iif( ::oWndStack != NIL, ::oWndStack:nWidth(), 0 ), ;
         "Monitor:" + ;
         iif( ::lShowGlobals, " Global", "" ) + iif( ::lShowLocals, " Local", "" ) + ;
         iif( ::lShowStatics, " Static", "" ) + iif( ::lShowPrivates, " Private", "" ) + ;
         iif( ::lShowPublics, " Public", "" ) )

      ::oWndVars:bLButtonDown := {| nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
      ::oWndVars:bLDblClick   := {|| ::EditVar( ::oBrwVars:Cargo[ 1 ] ) }
      ::oWndVars:bPainted     := {|| iif( Len( ::aVars ) > 0, ( ::oBrwVars:RefreshAll():ForceStable(), RefreshVarsS( ::oBrwVars ) ), ) }

      ::oWndVars:bKeyPressed := {| nKey | iif( Len( ::aVars ) == 0, NIL, ( ;
         iif( nKey == K_DOWN, ::oBrwVars:Down(), NIL ), ;
         iif( nKey == K_UP, ::oBrwVars:Up(), NIL ), ;
         iif( nKey == K_PGDN, ::oBrwVars:PageDown(), NIL ), ;
         iif( nKey == K_PGUP, ::oBrwVars:PageUp(), NIL ), ;
         iif( nKey == K_HOME, ::oBrwVars:GoTop(), NIL ), ;
         iif( nKey == K_END, ::oBrwVars:GoBottom(), NIL ), ;
         iif( nKey == K_ENTER, ::EditVar( ::oBrwVars:Cargo[ 1 ] ), NIL ), ;
         iif( Len( ::aVars ) > 0, ::oBrwVars:ForceStable(), NIL ) ) ) }

      // Maintain fixed window array order: code, monitor, watch, callstack, command
      hb_AIns( ::aWindows, 2, ::oWndVars, .T. )
      IF ::nCurrentWindow >= 2
         ::nCurrentWindow++
      ENDIF

      lWindowCreated := .T.
   ELSE

      nTop := ::oWndVars:nTop
      ::oWndVars:cCaption := "Monitor:" + ;
         iif( ::lShowGlobals, " Global", "" ) + ;
         iif( ::lShowLocals, " Local", "" ) + ;
         iif( ::lShowStatics, " Static", "" ) + ;
         iif( ::lShowPrivates, " Private", "" ) + ;
         iif( ::lShowPublics, " Public", "" )

      nBottom := ::oWndVars:nBottom
      DO CASE
      CASE Len( ::aVars ) == 0
         IF ::oWndVars:nBottom - ::oWndVars:nTop > 1
            nBottom := nTop + 1
         ENDIF
      CASE Len( ::aVars ) > ::oWndVars:nBottom - ::oWndVars:nTop - 1
         nBottom := nTop + Min( Len( ::aVars ) + 1, MAX_VARS_HEIGHT )
      CASE Len( ::aVars ) < ::oWndVars:nBottom - ::oWndVars:nTop - 1
         nBottom := nTop + Len( ::aVars ) + 1
      OTHERWISE
         nBottom := ::oWndVars:nBottom
      ENDCASE
   ENDIF

   IF Len( ::aVars ) > 0 .AND. ::oBrwVars == NIL
      ::oBrwVars := HBDbBrowser():New( nTop + 1, 1, nBottom - 1, ;
         ::nMaxCol - iif( ::oWndStack != NIL, ::oWndStack:nWidth(), 0 ) - 1 )
      aColors := __dbgColors()
      ::oBrwVars:Cargo := { 1, {} } // Actual highlighted row
      ::oBrwVars:ColorSpec := aColors[ 2 ] + "," + aColors[ 5 ] + "," + aColors[ 3 ] + "," + aColors[ 6 ]
      ::oBrwVars:goTopBlock := {|| ::oBrwVars:cargo[ 1 ] := Min( 1, Len( ::aVars ) ) }
      ::oBrwVars:goBottomBlock := {|| ::oBrwVars:cargo[ 1 ] := Max( 1, Len( ::aVars ) ) }
      ::oBrwVars:skipBlock := {| nSkip, nOld | ;
         nOld := ::oBrwVars:Cargo[ 1 ], ;
         ::oBrwVars:Cargo[ 1 ] += nSkip, ;
         ::oBrwVars:Cargo[ 1 ] := Min( Max( ::oBrwVars:Cargo[ 1 ], 1 ), Len( ::aVars ) ), ;
         ::oBrwVars:Cargo[ 1 ] - nOld }

      oCol := HBDbColumnNew( "", ;
         {|| PadR( hb_ntos( ::oBrwVars:Cargo[ 1 ] - 1 ) + ") " + ;
         ::VarGetInfo( ::aVars[ Max( ::oBrwVars:Cargo[ 1 ], 1 ) ] ), ;
         ::oWndVars:nWidth() - 2 ) } )
      ::oBrwVars:AddColumn( oCol )
      AAdd( ::oBrwVars:Cargo[ 2 ], ::aVars )
      oCol:DefColor := { 1, 2 }
      ::oBrwVars:ForceStable()
   ELSEIF Len( ::aVars ) == 0
      ::oBrwVars := NIL
      ::oWndVars:Browser := NIL
   ENDIF

   ::oWndVars:Browser := ::oBrwVars

   IF lWindowCreated
      ::oWndVars:Show()
      ::ResizeWindows( ::oWndVars )

   ELSE

      IF ::oBrwVars != NIL
         IF ::oBrwVars:cargo[ 1 ] <= 0
            ::oBrwVars:cargo[ 1 ] := 1
         ENDIF
      ENDIF

      IF Len( ::aVars ) == 0
         IF nBottom == ::oWndVars:nBottom
            /* We still need to redraw window caption, it could have changed */
            ::oWndVars:Refresh()
         ENDIF
      ENDIF
      IF nBottom != ::oWndVars:nBottom
         ::oWndVars:Resize( ,, nBottom )
         lRepaint := .T.
      ELSE
         IF ::oBrwVars != NIL
            ::oBrwVars:RefreshAll():ForceStable()
         ENDIF
         ::oWndVars:Refresh()
      ENDIF
      IF ! ::oWndVars:lVisible .OR. lRepaint
         ::ResizeWindows( ::oWndVars )
      ENDIF
   ENDIF

   DispEnd()

   RETURN


METHOD PROCEDURE Stack( cParam ) CLASS HBDebugger

   SWITCH iif( HB_ISSTRING( cParam ), cParam, "" )
   CASE "ON"
      ::lShowCallStack := .T.
      EXIT
   CASE "OFF"
      ::lShowCallStack := .F.
      EXIT
   OTHERWISE
      ::lShowCallStack := ! ::lShowCallStack
   ENDSWITCH

   ::oPulldown:GetItemByIdent( "CALLSTACK" ):checked := ::lShowCallStack

   IF ::lActive
      IF ::lShowCallStack
         ::ShowCallStack()
      ELSE
         ::HideCallStack()
      ENDIF
   ENDIF

   RETURN


METHOD PROCEDURE Static() CLASS HBDebugger

   ::lShowStatics := ! ::lShowStatics
   ::RefreshVars()

   RETURN


METHOD PROCEDURE Step() CLASS HBDebugger

   // we are starting to run again so reset to the deepest call if displaying stack
   IF ::oBrwStack != NIL
      ::oBrwStack:GoTop()
   ENDIF

   ::RestoreAppScreen()
   ::RestoreAppState()
   ::Exit()

   RETURN


METHOD ToCursor() CLASS HBDebugger

   IF ::IsValidStopLine( ::cPrgName, ::oBrwText:RowPos )
      __dbgSetToCursor( ::pInfo, ::cPrgName, ::oBrwText:RowPos )
      ::RestoreAppScreen()
      ::RestoreAppState()
      ::Exit()
   ENDIF

   RETURN Self


// Toggle a breakpoint at the cursor position in the currently viewed file
// which may be different from the file in which execution was broken
METHOD PROCEDURE BreakPointToggle( nLine, cFileName ) CLASS HBDebugger

   // look for a breakpoint which matches both line number and module name

   LOCAL nAt

   IF ! ::lActive
      RETURN
   ENDIF

   IF nLine == NIL
      cFileName := ::cPrgName
      nLine := ::oBrwText:RowPos
   ENDIF

   IF ( nAt := __dbgIsBreak( ::pInfo, cFileName, nLine ) ) >= 0
      __dbgDelBreak( ::pInfo, nAt )
   ELSEIF ::IsValidStopLine( cFileName, nLine )
      __dbgAddBreak( ::pInfo, cFileName, nLine )
   ENDIF

   ::oBrwText:RefreshAll():ForceStable()

   RETURN


METHOD BreakPointDelete( cPos ) CLASS HBDebugger

   LOCAL nAt

   IF Empty( cPos )
      cPos := AllTrim( ::InputBox( "Item number to delete", "0" ) )
      IF LastKey() == K_ESC
         cPos := ""
      ENDIF
   ENDIF

   IF IsDigit( cPos )
      __dbgDelBreak( ::pInfo, Val( cPos ) )
   ELSEIF Upper( cPos ) == "ALL"
      FOR nAt := Len( __dbgGetBreakPoints( ::pInfo ) ) - 1 TO 0 STEP -1
         __dbgDelBreak( ::pInfo, nAt )
      NEXT
   ENDIF

   ::oBrwText:RefreshAll():ForceStable()

   RETURN Self


METHOD BreakPointFunc( cFuncName ) CLASS HBDebugger

   __dbgAddBreak( ::pInfo,,, cFuncName )

   RETURN Self


METHOD BreakPointList() CLASS HBDebugger

   LOCAL aBreak, cType

   FOR EACH aBreak IN __dbgGetBreakPoints( ::pInfo )
      cType := iif( aBreak[ HB_DBG_BP_FUNC ] != NIL, ;
                    aBreak[ HB_DBG_BP_FUNC ], ;
                    hb_ntos( aBreak[ HB_DBG_BP_LINE ] ) + " " + ;
                    aBreak[ HB_DBG_BP_MODULE ] )
      ::CommandWindowDisplay( hb_ntos( aBreak:__enumIndex() - 1 ) + ") " + ;
                              cType, .F. )
   NEXT

   RETURN Self


METHOD Trace() CLASS HBDebugger

   __dbgSetTrace( ::pInfo )
   ::Step() // forces a Step()

   RETURN Self


METHOD TracepointAdd( cExpr ) CLASS HBDebugger

   LOCAL aWatch

   IF cExpr == NIL
      cExpr := AllTrim( ::InputBox( "Enter Tracepoint",, __dbgExprValidBlock() ) )
      IF LastKey() == K_ESC
         RETURN Self
      ENDIF
   ENDIF
   cExpr := AllTrim( cExpr )
   IF Empty( cExpr )
      RETURN Self
   ENDIF
   aWatch := { "tp", cExpr, NIL }
   ::RestoreAppState()
   __dbgAddWatch( ::pInfo, cExpr, .T. )
   ::SaveAppState()
   AAdd( ::aWatch, aWatch )
   ::WatchpointsShow()

   RETURN Self


METHOD VarGetInfo( aVar ) CLASS HBDebugger

   LOCAL uValue := ::VarGetValue( aVar )
   LOCAL cType

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
   CASE "G"  ; cType := "Global" ; EXIT
   CASE "L"  ; cType := "Local" ; EXIT
   CASE "S"  ; cType := "Static" ; EXIT
   OTHERWISE ; cType := aVar[ HB_DBG_VAR_TYPE ]
   ENDSWITCH

   RETURN aVar[ HB_DBG_VAR_NAME ] + " <" + cType + ", " + ValType( uValue ) + ">: " + __dbgValToStr( uValue )


METHOD VarGetValue( aVar ) CLASS HBDebugger

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
   CASE "G" ; RETURN __dbgVMVarGGet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
   CASE "L" ; RETURN __dbgVMVarLGet( __dbgProcLevel() - aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
   CASE "S" ; RETURN __dbgVMVarSGet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ] )
   ENDSWITCH

   // Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX
   RETURN aVar[ HB_DBG_VAR_MVALUE ]


METHOD VarSetValue( aVar, uValue ) CLASS HBDebugger

   LOCAL nProcLevel

   SWITCH aVar[ HB_DBG_VAR_TYPE ]
   CASE "G"
      __dbgVMVarGSet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   CASE "L"
      nProcLevel := __dbgProcLevel() - aVar[ HB_DBG_VAR_FRAME ]  // skip debugger stack
      __dbgVMVarLSet( nProcLevel, aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   CASE "S"
      __dbgVMVarSSet( aVar[ HB_DBG_VAR_FRAME ], aVar[ HB_DBG_VAR_INDEX ], uValue )
      EXIT
   OTHERWISE
      // Public or Private created in ::LoadVars(), value stored in HB_DBG_VAR_INDEX
      aVar[ HB_DBG_VAR_MVALUE ] := uValue
      &( aVar[ HB_DBG_VAR_NAME ] ) := uValue
   ENDSWITCH

   RETURN Self


METHOD PROCEDURE ViewSets() CLASS HBDebugger

   LOCAL aSets := __dbgGetSETs()
   LOCAL oWndSets := HBDbWindow():New( 1, 7, ::nMaxRow - 2, ::nMaxCol - 7, ;
      "System Settings[1.." + hb_ntos( aTail( aSets )[ HB_DBG_SET_POS ] ) + "]", ::ClrModal() )
   LOCAL oBrwSets := HBDbBrowser():new( oWndSets:nTop + 1, oWndSets:nLeft + 1, ;
      oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   LOCAL nWidth := oWndSets:nRight - oWndSets:nLeft - 1
   LOCAL oCol

   oBrwSets:Cargo := { 1, {} } // Actual highlighted row
   oBrwSets:autolite := .F.
   oBrwSets:ColorSpec := ::ClrModal()
   oBrwSets:goTopBlock := {|| oBrwSets:cargo[ 1 ] := 1 }
   oBrwSets:goBottomBlock := {|| oBrwSets:cargo[ 1 ] := Len( oBrwSets:cargo[ 2 ][ 1 ] ) }
   oBrwSets:skipBlock := {| nPos | ( nPos := ArrayBrowseSkip( nPos, oBrwSets ), oBrwSets:cargo[ 1 ] := ;
      oBrwSets:cargo[ 1 ] + nPos, nPos ) }
   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| aSets[ oBrwSets:cargo[ 1 ] ][ HB_DBG_SET_NAME ] } ) )
   AAdd( oBrwSets:Cargo[ 2 ], aSets )
   oCol:Width := 14
   oCol:defcolor := { 1, 2 }
   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", ;
      {|| PadR( __dbgValToExp( Set( aSets[ oBrwSets:cargo[ 1 ] ][ HB_DBG_SET_POS ] ) ), nWidth - 13 ) } ) )
   oCol:defcolor := { 1, 3 }
   oCol:width := 40
   oWndSets:bPainted := {|| oBrwSets:ForceStable(), RefreshVarsS( oBrwSets ) }
   oWndSets:bKeyPressed := {| nKey | SetsKeyPressed( nKey, oBrwSets, Len( aSets ), ;
      oWndSets, "System Settings", ;
      {|| ::EditSet( aSets[ oBrwSets:Cargo[ 1 ] ][ HB_DBG_SET_POS ], oBrwSets ) } ) }

   oWndSets:ShowModal()

   RETURN


METHOD WatchGetInfo( nWatch ) CLASS HBDebugger

   LOCAL xVal
   LOCAL cType
   LOCAL lValid
   LOCAL aWatch := ::aWatch[ nWatch ]

   ::RestoreAppState()
   xVal := ::GetExprValue( nWatch, @lValid )
   ::SaveAppState()

   IF lValid
      cType := ValType( xVal )
      xVal  := __dbgValToStr( xVal )
   ELSE
      cType := "U"
      xVal := "Undefined"
   ENDIF

   RETURN aWatch[ WP_EXPR ] + " <" + aWatch[ WP_TYPE ] + ", " + cType + ">: " + xVal


METHOD WatchpointAdd( cExpr ) CLASS HBDebugger

   LOCAL aWatch

   IF cExpr == NIL
      cExpr := ::InputBox( "Enter Watchpoint",, __dbgExprValidBlock() )
      IF LastKey() == K_ESC
         RETURN Self
      ENDIF
   ENDIF

   cExpr := AllTrim( cExpr )

   IF Empty( cExpr )
      RETURN Self
   ENDIF

   aWatch := { "wp", cExpr }
   __dbgAddWatch( ::pInfo, cExpr, .F. )
   AAdd( ::aWatch, aWatch )
   ::WatchpointsShow()

   RETURN Self


METHOD WatchpointDel( xPos ) CLASS HBDebugger

   LOCAL nPos := -1, lAll := .F.

   IF ::oWndPnt != NIL .AND. ::oWndPnt:lVisible
      IF Empty( xPos )
         nPos := ::InputBox( "Enter item number to delete", ::oBrwPnt:cargo[ 1 ] - 1 )
         IF LastKey() == K_ESC
            nPos := -1
         ENDIF
      ELSEIF HB_ISSTRING( xPos )
         IF Upper( xPos ) == "ALL"
            lAll := .T.
         ELSEIF IsDigit( xPos )
            nPos := Val( xPos )
         ENDIF
      ELSEIF HB_ISNUMERIC( xPos )
         nPos := xPos
      ENDIF

      IF lAll .OR. ( nPos >= 0 .AND. nPos < Len( ::aWatch ) )
         ::oBrwPnt:gotop()
         IF lAll
            FOR nPos := Len( ::aWatch ) - 1 TO 0 STEP -1
               __dbgDelWatch( ::pInfo, nPos )
            NEXT
            ASize( ::aWatch, 0 )
         ELSE
            __dbgDelWatch( ::pInfo, nPos )
            hb_ADel( ::aWatch, nPos + 1, .T. )
         ENDIF
         IF Len( ::aWatch ) == 0
            ::WatchpointsHide()
         ELSE
            ::WatchpointsShow()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self


METHOD WatchpointEdit( nPos ) CLASS HBDebugger

   LOCAL cExpr := ::InputBox( "Enter Watchpoint", ::aWatch[ nPos ][ WP_EXPR ], __dbgExprValidBlock() )
   LOCAL aWatch

   IF LastKey() == K_ESC
      RETURN Self
   ENDIF

   cExpr := AllTrim( cExpr )

   IF Empty( cExpr )
      RETURN Self
   ENDIF

   aWatch := { "wp", cExpr }

   __dbgSetWatch( ::pInfo, nPos - 1, cExpr, .F. )
   ::aWatch[ nPos ] := aWatch
   ::WatchpointsShow()

   RETURN Self


METHOD WatchpointInspect( nPos ) CLASS HBDebugger

   LOCAL xValue
   LOCAL lValid

   ::RestoreAppState()
   xValue := ::GetExprValue( ::aWatch[ nPos ][ WP_EXPR ], @lValid )
   ::SaveAppState()

   ::InputBox( ::aWatch[ nPos ][ WP_EXPR ], xValue,, .F. )
   ::RefreshVars()

   RETURN Self


METHOD PROCEDURE WatchpointsHide() CLASS HBDebugger

   ::oWndPnt:Hide()
   ::oWndCode:nTop := iif( ::oWndVars != NIL .AND. ::oWndVars:lVisible, ::oWndVars:nBottom + 1, 1 )
   ::oBrwText:Resize( ::oWndCode:nTop + 1 )
   IF ::aWindows[ ::nCurrentWindow ] == ::oWndPnt
      ::NextWindow()
   ENDIF

   RETURN


METHOD PROCEDURE WatchpointsShow() CLASS HBDebugger

   LOCAL oCol
   LOCAL lRepaint := .F.
   LOCAL nTop
   LOCAL aColors
   LOCAL nPos

   IF ::lGo
      RETURN
   ENDIF

   IF Len( ::aWatch ) == 0
      RETURN
   ENDIF

   IF ::oWndPnt == NIL

      nTop := iif( ::oWndVars != NIL .AND. ::oWndVars:lVisible, ::oWndVars:nBottom, 0 ) + 1

      ::oWndPnt := HBDbWindow():New( nTop, ;
         0, ;
         nTop + Min( 4, Len( ::aWatch ) ) + 1, ;
         ::nMaxCol - iif( ::oWndStack != NIL, ::oWndStack:nWidth(), 0 ), ;
         "Watch" )

#if 0
      ::oBrwText:Resize( ::oWndPnt:nBottom + 1 )
      ::oWndCode:nTop := ::oWndPnt:nBottom + 1
      ::oBrwText:Resize( ::oWndCode:nTop + 1 )
      ::oBrwText:RefreshAll()
      ::oWndCode:SetFocus( .T. )

      ::oWndPnt:bLButtonDown := {| nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
      ::oWndPnt:bLDblClick   := {| nMRow, nMCol | ::EditVar( ::oBrwPnt:Cargo[ 1 ] ) }
#endif

      ::oBrwPnt := HBDbBrowser():New( nTop + 1, 1, ::oWndPnt:nBottom - 1, ::nMaxCol - iif( ::oWndStack != NIL, ;
         ::oWndStack:nWidth(), 0 ) - 1 )

      ::oWndPnt:Browser := ::oBrwPnt

      ::oBrwPnt:Cargo := { 1, {} } // Actual highlighted row
      aColors := __dbgColors()
      ::oBrwPnt:ColorSpec := aColors[ 2 ] + "," + aColors[ 5 ] + "," + aColors[ 3 ] + "," + aColors[ 6 ]
      ::oBrwPnt:goTopBlock := {|| ::oBrwPnt:cargo[ 1 ] := Min( 1, Len( ::aWatch ) ) }
      ::oBrwPnt:goBottomBlock := {|| ::oBrwPnt:cargo[ 1 ] := Len( ::aWatch ) }
      ::oBrwPnt:skipBlock := {| nSkip, nOld | nOld := ::oBrwPnt:Cargo[ 1 ], ;
         ::oBrwPnt:Cargo[ 1 ] += nSkip, ;
         ::oBrwPnt:Cargo[ 1 ] := Min( Max( ::oBrwPnt:Cargo[ 1 ], 1 ), ;
         Len( ::aWatch ) ), ;
         iif( Len( ::aWatch ) > 0, ::oBrwPnt:Cargo[ 1 ] - nOld, 0 ) }

      oCol := HBDbColumnNew( "", ;
         {|| PadR( iif( Len( ::aWatch ) > 0, ;
                        hb_ntos( ::oBrwPnt:Cargo[ 1 ] - 1 ) + ") " + ;
                           ::WatchGetInfo( Max( ::oBrwPnt:Cargo[ 1 ], 1 ) ), ;
                        " " ), ::oWndPnt:nWidth() - 2 ) } )
      ::oBrwPnt:AddColumn( oCol )
      AAdd( ::oBrwPnt:Cargo[ 2 ], ::aWatch )
      oCol:DefColor := { 1, 2 }

      ::oWndPnt:bPainted := {|| iif( Len( ::aWatch ) > 0, ( ::oBrwPnt:RefreshAll():ForceStable(), RefreshVarsS( ::oBrwPnt ) /*, ::RefreshVars()*/ ) , ) }

      ::oWndPnt:bKeyPressed := {| nKey | ( ;
         iif( nKey == K_DOWN, ::oBrwPnt:Down(), NIL ), ;
         iif( nKey == K_UP, ::oBrwPnt:Up(), NIL ), ;
         iif( nKey == K_PGDN, ::oBrwPnt:PageDown(), NIL ), ;
         iif( nKey == K_PGUP, ::oBrwPnt:PageUp(), NIL ), ;
         iif( nKey == K_HOME, ::oBrwPnt:GoTop(), NIL ), ;
         iif( nKey == K_END, ::oBrwPnt:GoBottom(), NIL ), ;
         iif( nKey == K_DEL, ::WatchpointDel( ::oBrwPnt:Cargo[ 1 ] - 1 ), NIL ), ;
         iif( nKey == K_ENTER, ::WatchpointEdit( ::oBrwPnt:Cargo[ 1 ] ), NIL ), ;
         iif( nKey == K_CTRL_ENTER, ::WatchpointInspect( ::oBrwPnt:Cargo[ 1 ] ), NIL ), ;
         ::oBrwPnt:ForceStable() ) }

      // Maintain fixed window array order: code, monitor, watch, callstack, command
      nPos := iif( ::aWindows[ 2 ] == ::oWndVars, 3, 2 )
      hb_AIns( ::aWindows, nPos, ::oWndPnt, .T. )
      IF ::nCurrentWindow >= nPos
         ::nCurrentWindow++
      ENDIF

      ::oWndPnt:Show()
      ::ResizeWindows( ::oWndPnt )
   ELSE
      IF ::oBrwPnt:cargo[ 1 ] <= 0
         ::oBrwPnt:cargo[ 1 ] := 1
      ENDIF
      DispBegin()
      DO CASE
      CASE Len( ::aWatch ) > ::oWndPnt:nBottom - ::oWndPnt:nTop - 1
         // Resize( top, left, bottom, right )
         ::oWndPnt:Resize( ,, ::oWndPnt:nTop + Min( Len( ::aWatch ) + 1, 4 ) )
         lRepaint := .T.
      CASE Len( ::aWatch ) < ::oWndPnt:nBottom - ::oWndPnt:nTop - 1
         ::oWndPnt:Resize( ,, ::oWndPnt:nTop + Len( ::aWatch ) + 1 )
         lRepaint := .T.
      OTHERWISE
         ::oBrwPnt:RefreshAll():ForceStable()
      ENDCASE
      IF ! ::oWndPnt:lVisible .OR. lRepaint
         ::ResizeWindows( ::oWndPnt )
      ENDIF
      DispEnd()
   ENDIF

   RETURN


METHOD PROCEDURE WatchpointList() CLASS HBDebugger

   LOCAL aWatch, cType

   FOR EACH aWatch IN ::aWatch
      SWITCH aWatch[ WP_TYPE ]
      CASE "wp"
         cType := "WatchPoint"
         EXIT
      CASE "tp"
         cType := "TracePoint"
         EXIT
      OTHERWISE
         cType := aWatch[ WP_TYPE ]
      ENDSWITCH
      ::CommandWindowDisplay( hb_ntos( aWatch:__enumIndex() - 1 ) + ") " + ;
                              cType + " " + ;
                              AllTrim( aWatch[ WP_EXPR ] ), .F. )
   NEXT

   RETURN


METHOD PROCEDURE WndVarsLButtonDown( nMRow, nMCol ) CLASS HBDebugger

   IF nMRow > ::oWndVars:nTop .AND. ;
      nMRow < ::oWndVars:nBottom .AND. ;
      nMCol > ::oWndVars:nLeft .AND. ;
      nMCol < ::oWndVars:nRight

      IF nMRow - ::oWndVars:nTop >= 1 .AND. ;
         nMRow - ::oWndVars:nTop <= Len( ::aVars )

         DO WHILE ::oBrwVars:RowPos > nMRow - ::oWndVars:nTop
            ::oBrwVars:Up()
            ::oBrwVars:ForceStable()
         ENDDO

         DO WHILE ::oBrwVars:RowPos < nMRow - ::oWndVars:nTop
            ::oBrwVars:Down()
            ::oBrwVars:ForceStable()
         ENDDO

      ENDIF
   ENDIF

   RETURN


STATIC PROCEDURE SetsKeyPressed( nKey, oBrwSets, nSets, oWnd, cCaption, bEdit )

   SWITCH nKey
   CASE K_UP
      oBrwSets:up()
      EXIT

   CASE K_DOWN
      oBrwSets:down()
      EXIT

   CASE K_HOME
   CASE K_CTRL_PGUP
   CASE K_CTRL_HOME
      oBrwSets:goTop()
      EXIT

   CASE K_END
   CASE K_CTRL_PGDN
   CASE K_CTRL_END
      oBrwSets:goBottom()
      EXIT

   CASE K_PGDN

      oBrwSets:pageDown()
      EXIT

   CASE K_PGUP

      oBrwSets:pageUp()
      EXIT

   CASE K_ENTER

      IF bEdit != NIL
         Eval( bEdit )
      ENDIF

      IF LastKey() == K_ENTER
         hb_keyPut( K_DOWN )
      ENDIF
      EXIT

   ENDSWITCH

   RefreshVarsS( oBrwSets )

   oWnd:SetCaption( cCaption + "[" + hb_ntos( oBrwSets:Cargo[ 1 ] ) + ".." + hb_ntos( nSets ) + "]" )

   RETURN


FUNCTION __dbgColors()
   RETURN t_oDebugger:GetColors()


FUNCTION __dbg()
   RETURN t_oDebugger


STATIC PROCEDURE RefreshVarsS( oBrowse )

   LOCAL nLen := oBrowse:colCount

   IF nLen == 2
      oBrowse:deHilite():colPos := 2
   ENDIF
   oBrowse:deHilite():forceStable()

   IF nLen == 2
      oBrowse:hilite():colPos := 1
   ENDIF
   oBrowse:hilite()

   RETURN


STATIC FUNCTION ArrayBrowseSkip( nPos, oBrwSets )
   RETURN iif( oBrwSets:cargo[ 1 ] + nPos < 1, 0 - oBrwSets:cargo[ 1 ] + 1, ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len( oBrwSets:cargo[ 2 ][ 1 ] ), ;
      Len( oBrwSets:cargo[ 2 ][ 1 ] ) - oBrwSets:cargo[ 1 ], nPos ) )


STATIC FUNCTION PathToArray( cList )

   LOCAL aList := {}
   LOCAL cSep := hb_osPathListSeparator()
   LOCAL cDirSep := hb_osPathDelimiters()
   LOCAL nPos

   IF cList != NIL

      DO WHILE ( nPos := At( cSep, cList ) ) > 0
         AAdd( aList, Left( cList, nPos - 1 ) )        // Add a new element
         cList := SubStr( cList, nPos + 1 )
      ENDDO

      AAdd( aList, cList )              // Add final element

      /* Strip ending delimiters */
      AEval( aList, {| x, i | iif( Right( x, 1 ) $ cDirSep, aList[ i ] := hb_StrShrink( x ), ) } )
   ENDIF

   RETURN aList


/* Check if a string starts with another string with a min length */
STATIC FUNCTION hb_LeftEqN( cLine, cStart, nMin )
   RETURN Len( cStart ) >= nMin .AND. hb_LeftEq( cLine, cStart )


FUNCTION __dbgExprValidBlock( cType )
   LOCAL cTypeName

   IF HB_ISSTRING( cType )
      SWITCH cType
      CASE "N"
         cTypeName := "numeric"
         EXIT
      CASE "C"
         cTypeName := "srtring"
         EXIT
      CASE "L"
         cTypeName := "logical"
         EXIT
      CASE "D"
         cTypeName := "date"
         EXIT
      CASE "T"
         cTypeName := "timestamp"
         EXIT
      CASE "S"
         cTypeName := "symbol"
         EXIT
      CASE "A"
         cTypeName := "array"
         EXIT
      CASE "H"
         cTypeName := "hash"
         EXIT
      CASE "P"
         cTypeName := "pointer"
         EXIT
      ENDSWITCH
   ENDIF

   IF cTypeName != NIL
      RETURN {| u | iif( Type( u ) == "UE", ;
                         ( __dbgAlert( "Expression error" ), .F. ), ;
                         Type( u ) == cType .OR. ;
                         ( __dbgAlert( "Must be " + cTypeName ), .F. ) ) }
   ENDIF

   RETURN {| u | ! Type( u ) == "UE" .OR. ;
                 ( __dbgAlert( "Expression error" ), .F. ) }


FUNCTION __dbgInput( nRow, nCol, nWidth, cValue, bValid, cColor, nSize )

   LOCAL lOK := .F.
   LOCAL nKey
   LOCAL oGet

   IF ! HB_ISNUMERIC( nWidth )
      nWidth := Len( cValue )
   ENDIF
   oGet := HbDbInput():new( nRow, nCol, nWidth, cValue, cColor, nSize )

   oGet:display()

   DO WHILE .T.
      oGet:showCursor()
      nKey := __dbgInkey()
      DO CASE
      CASE nKey == K_ESC
         EXIT
      CASE nKey == K_ENTER
         IF bValid == NIL .OR. Eval( bValid, oGet:getValue() )
            cValue := oGet:getValue()
            lOK := .T.
            EXIT
         ENDIF
      OTHERWISE
         oGet:applyKey( nKey )
      ENDCASE
   ENDDO

   SetCursor( SC_NONE )

   RETURN lOK


FUNCTION __dbgAChoice( nTop, nLeft, nBottom, nRight, aItems, cColors )

   LOCAL oBrw
   LOCAL oCol
   LOCAL nRow
   LOCAL nLen

   oBrw := HBDbBrowser():New( nTop, nLeft, nBottom, nRight )
   oBrw:colorSpec := cColors
   nLen := nRight - nLeft + 1
   nRow := 1
   oCol := HBDbColumnNew( "", {|| PadR( aItems[ nRow ], nLen ) } )
   oBrw:AddColumn( oCol )
   oBrw:goTopBlock := {|| nRow := 1 }
   oBrw:goBottomBlock := {|| nRow := Len( aItems ) }
   oBrw:skipBlock := {| n | n := iif( n < 0, Max( n, 1 - nRow ), ;
      Min( Len( aItems ) - nRow, n ) ), ;
      nRow += n, n }
   DO WHILE .T.
      oBrw:forceStable()
      SWITCH __dbgInkey()
      CASE K_UP;     oBrw:up();        EXIT
      CASE K_DOWN;   oBrw:down();      EXIT
      CASE K_PGUP;   oBrw:pageUp();    EXIT
      CASE K_PGDN;   oBrw:pageDown();  EXIT
      CASE K_HOME;   oBrw:goTop();     EXIT
      CASE K_END;    oBrw:goBottom();  EXIT
      CASE K_ESC;    nRow := 0
      CASE K_ENTER;  RETURN nRow
      ENDSWITCH
   ENDDO

   RETURN 0


FUNCTION __dbgAlert( cMessage )
   RETURN hb_gtAlert( cMessage, { "Ok" }, "W+/R", "W+/B" )


FUNCTION __dbgInkey()

   LOCAL nKey
   LOCAL lDebug, lCancel

   lDebug := Set( _SET_DEBUG, .F. )
   lCancel := Set( _SET_CANCEL, .F. )
   nKey := Inkey( 0, INKEY_ALL )
   Set( _SET_CANCEL, lCancel )
   Set( _SET_DEBUG, lDebug )

   RETURN nKey


FUNCTION __dbgSaveScreen( ... )

   LOCAL lAppCompatBuffer := hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   LOCAL cScreen := SaveScreen( ... )

   hb_gtInfo( HB_GTI_COMPATBUFFER, lAppCompatBuffer )

   RETURN cScreen


FUNCTION __dbgRestScreen( ... )

   LOCAL lAppCompatBuffer := hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )

   RestScreen( ... )
   hb_gtInfo( HB_GTI_COMPATBUFFER, lAppCompatBuffer )

   RETURN NIL


FUNCTION __dbgTextToArray( cString )
   RETURN hb_ATokens( StrTran( cString, Chr( 13 ) ), Chr( 10 ) )

FUNCTION __dbgValToStr( uVal )

   SWITCH ValType( uVal )
#ifdef HB_CLP_STRICT
   CASE "C"
   CASE "M" ; RETURN '"' + uVal + '"'
   CASE "D" ; RETURN DToC( uVal )
   CASE "T" ; RETURN hb_TToC( uVal )
   CASE "O" ; RETURN "{ ... }"
#else
   CASE "C"
   CASE "M" ; RETURN hb_StrToExp( uVal )
   CASE "D" ; RETURN Left( hb_TSToStr( uVal, .F. ), 10 )
   CASE "T" ; RETURN hb_TSToStr( uVal, .T. )
   CASE "O" ; RETURN "Class " + uVal:ClassName() + " object"
#endif
   CASE "N" ; RETURN Str( uVal )
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + uVal:name + "()"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ ... }"
   CASE "H" ; RETURN "{ => }"
   CASE "P" ; RETURN "<pointer>"
   OTHERWISE
      IF uVal == NIL
         RETURN "NIL"
      ENDIF
   ENDSWITCH

   RETURN "U"


FUNCTION __dbgValToExp( uVal )

   SWITCH ValType( uVal )
#ifdef HB_CLP_STRICT
   CASE "C"
   CASE "M" ; RETURN '"' + uVal + '"'
   CASE "D" ; RETURN 'CToD("' + DToC( uVal ) + '")'
   CASE "T" ; RETURN 'hb_CToT("' + hb_TToC( uVal ) + '")'
   CASE "O" ; RETURN "Object"
#else
   CASE "C"
   CASE "M" ; RETURN hb_StrToExp( uVal )
   CASE "D" ; RETURN 'd"' + Left( hb_TSToStr( uVal, .F. ), 10 ) + '"'
   CASE "T" ; RETURN 't"' + hb_TSToStr( uVal, .T. ) + '"'
   CASE "O" ; RETURN "{ " + uVal:className() + " Object }"
#endif
   CASE "N" ; RETURN hb_ntos( uVal )
   CASE "L" ; RETURN iif( uVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + uVal:name + "()"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ ... }"
   CASE "H" ; RETURN "{ => }"
   CASE "P" ; RETURN "<pointer>"
   OTHERWISE
      IF uVal == NIL
         RETURN "NIL"
      ENDIF
   ENDSWITCH

   RETURN "U"
