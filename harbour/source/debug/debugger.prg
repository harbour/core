/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
 * www - http://www.harbour-project.org
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */


//#pragma -es2

#include "hbclass.ch"
#include "hbmemvar.ch"
#include "box.ch"
#include "inkey.ch"
#include "common.ch"
#include "setcurs.ch"
#include "hbdebug.ch"   //for "nMode" of __dbgEntry

#define  NTRIM(x)    (ALLTRIM(STR(x)))

#define ALTD_DISABLE   0
#define ALTD_ENABLE    1


/* Information structure hold by DATA aCallStack
*/
#define CSTACK_FUNCTION    1  //function name
#define CSTACK_LOCALS      2  //an array with local variables
#define CSTACK_LINE        3  //start line
#define CSTACK_MODULE      4  //module name (.PRG file)
#define CSTACK_STATICS     5  //an array with static variables
#define CSTACK_LEVEL       6  //eval stack level of the function

/* Information structure hold by aCallStack[n][ CSTACK_LOCALS ]
{ cLocalName, nLocalIndex, "Local", ProcName( 1 ) }
*/
#define VAR_NAME        1
#define VAR_POS         2
#define VAR_TYPE        3
#define VAR_FUNCNAME    4
#define VAR_LEVEL       5  //eval stack level of the function

/* Information structure hold by ::aWatch (watchpoints)
*/
#define WP_TYPE      1  //wp = watchpoint, tr = tracepoint
#define WP_EXPR      2  //source of an expression
#define WP_BLOCK     3  //codeblock to retrieve a value

/* Information structure hold by ::aTrace (tracepoints)
*/
#define TR_IDX       1  //index into ::aWatch item storing expression
#define TR_VALUE     2  //the current value of the expression

static s_oDebugger
static s_lExit := .F.
memvar __DbgStatics


procedure __dbgEntry( nMode, uParam1, uParam2, uParam3 )  // debugger entry point

   local cProcName
   local nVarIndex, cVarName
   local nAt, nSFrame
   LOCAL aTrace, uValue, lSuccess, nLen


   IF( __MVSCOPE( "__DBGSTATICS" ) != HB_MV_PUBLIC )   
      public __DbgStatics
      __DbgStatics := {}
   ENDIF
   
   do case
   case nMode == HB_DBG_SHOWLINE
      IF( s_lExit )
         RETURN
      ENDIF
      IF( s_oDebugger:lTracepoints )
         nLen := LEN(s_oDebugger:aTrace)
         FOR nAt:=1 TO nLen
            aTrace := s_oDebugger:aTrace[ nAt ]
            uValue := GetWatchValue( s_oDebugger:aWatch[ aTrace[TR_IDX] ], @lSuccess, hb_dbg_ProcLevel()-1 )
            IF( !lSuccess )
               uValue := NIL
            ENDIF
            IF( (VALTYPE(uValue) != VALTYPE(aTrace[TR_VALUE])) .OR. ;
                (uValue != aTrace[TR_VALUE]) )
               aTrace[TR_VALUE] := uValue
               s_oDebugger:lTrace :=.F.
               s_oDebugger:lCodeblock :=.F.
               s_oDebugger:lGo :=.F.
               s_oDebugger:lToCursor :=.F.
               s_oDebugger:lNextRoutine :=.F.
               s_oDebugger:aCallStack[ 1 ][CSTACK_LINE] := uParam1  
               s_oDebugger:nProcLevel := hb_dbg_Proclevel()-IIF(PROCNAME(1)=="ALTD",2,1)
               s_oDebugger:Activate()
               RETURN
            ENDIF
         NEXT
      ENDIF
      
      // set the current line number on the CallStack
      if s_oDebugger:lTrace
         //In TRACE mode (step over procedure)
         IF( s_oDebugger:nTraceLevel < Len( s_oDebugger:aCallStack ) )
            s_oDebugger:lTrace := (! s_oDebugger:IsBreakPoint( uParam1, s_oDebugger:aCallStack[1][ CSTACK_MODULE ] )) .AND.;
                                  (! HB_DBG_INVOKEDEBUG())
            if s_oDebugger:lTrace
               RETURN
            ENDIF
         ELSE
            //Return back into a current procedure
            s_oDebugger:lTrace := .f.
         ENDIF
      endif

      IF( s_oDebugger:lToCursor )
         IF( s_oDebugger:aToCursor[1] == uParam1 .AND. ;
             s_oDebugger:aToCursor[2] == s_oDebugger:aCallStack[1][ CSTACK_MODULE ] )
            s_oDebugger:lToCursor := .F.
         ELSE
            s_oDebugger:lToCursor := (! s_oDebugger:IsBreakPoint( uParam1, s_oDebugger:aCallStack[1][ CSTACK_MODULE ] )) .AND.;
                                  (! HB_DBG_INVOKEDEBUG())
            if s_oDebugger:lToCursor
               RETURN
            ENDIF
         ENDIF
      ENDIF
      
      IF( s_oDebugger:lNextRoutine .AND. !HB_DBG_INVOKEDEBUG() )
         s_oDebugger:lNextRoutine := (! s_oDebugger:IsBreakPoint( uParam1, s_oDebugger:aCallStack[1][ CSTACK_MODULE ] )) .AND.;
                                  (! HB_DBG_INVOKEDEBUG())
         if s_oDebugger:lNextRoutine
            RETURN
         ENDIF
      ENDIF
      
      if s_oDebugger:lGo
         s_oDebugger:lGo := ! s_oDebugger:IsBreakPoint( uParam1, s_oDebugger:aCallStack[1][ CSTACK_MODULE ] )
      endif

      IF( s_oDebugger:lCodeblock )
         s_oDebugger:lCodeblock := .F.
         IF( !s_oDebugger:lCBTrace )
            RETURN
         ENDIF
      ENDIF
      
      s_oDebugger:aCallStack[ 1 ][CSTACK_LINE] := uParam1  
      if !s_oDebugger:lGo .or. HB_DBG_INVOKEDEBUG()
         s_oDebugger:lGo := .F.
         s_oDebugger:nProcLevel := hb_dbg_Proclevel()-IIF(PROCNAME(1)=="ALTD",2,1)
         s_oDebugger:Activate()
      endif

   case nMode == HB_DBG_MODULENAME
      // add a call to the stack but don't try to show the code yet
      cProcName := ProcName( 1 )

      if cProcName == "(_INITSTATICS)"
         //module wide static variable
         AADD( __dbgStatics, { uParam1, {} } )
         return  // We can not use s_oDebugger yet, so we return
      endif

      IF( s_lExit )
         RETURN
      ENDIF

      IF( s_oDebugger == NIL )
         s_oDebugger := TDebugger():New()
      ENDIF
      if cProcName == "__EVAL" .OR. cProcName == "EVAL"
         s_oDebugger:lCodeblock := .T.
      ELSE
         IF( s_oDebugger:lNextRoutine )
            s_oDebugger:lNextRoutine :=.F.
         ENDIF
      endif
      s_oDebugger:StackProc( uParam1, hb_dbg_ProcLevel()-1 )

   case nMode == HB_DBG_ENDPROC
      if ProcName( 1 ) == "(_INITSTATICS)"
         return
      endif
      IF( s_lExit )
         RETURN
      ENDIF
      if s_oDebugger:lCodeblock
         s_oDebugger:lCodeblock := .F.
      endif
      s_oDebugger:EndProc()
         
   case nMode == HB_DBG_LOCALNAME
      IF( s_lExit )
         RETURN
      ENDIF
      cProcName := IIF(s_oDebugger:lCodeblock, s_oDebugger:aCallStack[1][CSTACK_FUNCTION], ProcName( 1 ))
      nVarIndex := uParam1
      cVarName  := IIF(valtype(uParam2)=='C',uParam2,'NIL')
      nAt       := hb_dbg_ProcLevel()-1
      AAdd( s_oDebugger:aCallStack[ 1 ][ CSTACK_LOCALS ], ;
            { cVarName, nVarIndex, "Local", cProcName, nAt } )
      if s_oDebugger:lShowLocals
         if ( nAt := AScan( s_oDebugger:aVars,; // Is there another var with this name ?
                            { | aVar | aVar[ 1 ] == cVarName } ) ) != 0
            s_oDebugger:aVars[ nAt ] := ATAIL( s_oDebugger:aCallStack[ 1 ][ CSTACK_LOCALS ] )
         else
            AAdd( s_oDebugger:aVars, ATAIL( s_oDebugger:aCallStack[ 1 ][ CSTACK_LOCALS ] ) )
         endif
      endif
      
   case nMode == HB_DBG_STATICNAME
      nSFrame   := uParam1
      nVarIndex := uParam2
      cVarName  := uParam3
      cProcName := ProcName( 1 )
      if cProcName == "(_INITSTATICS)"
         //module wide static variable
         AAdd( ATAIL(__DbgStatics)[2], { cVarName, nVarIndex, "Static",, nSFrame } )
         return  // We can not use s_oDebugger yet, so we return
      endif
           
      IF( s_lExit )
         RETURN
      ENDIF

      AAdd( s_oDebugger:aCallStack[ 1 ][ CSTACK_STATICS ], { cVarName, nVarIndex, "Static",, nSFrame } )

      if s_oDebugger:lShowStatics
         if ( nAt := AScan( s_oDebugger:aVars,; // Is there another var with this name ?
                             { | aVar | aVar[ VAR_NAME ] == cVarName } ) ) != 0
            s_oDebugger:aVars[ nAt ] := ATAIL( s_oDebugger:aCallStack[ 1 ][ CSTACK_STATICS ] )
         else
            AAdd( s_oDebugger:aVars, ATAIL( s_oDebugger:aCallStack[ 1 ][ CSTACK_STATICS ] ) )
         endif
      endif

   endcase

return

CLASS TDebugger

   DATA   aWindows, nCurrentWindow
   DATA   oPullDown
   DATA   oWndCode, oWndCommand, oWndStack, oWndVars
   DATA   oBar, oBrwText, cPrgName, oBrwStack, oBrwVars, aVars
   DATA   cImage
   DATA   cAppImage, nAppRow, nAppCol, cAppColors, nAppCursor
   DATA   aBreakPoints
   DATA   aCallStack    //stack of procedures with debug info
   DATA   aProcStack    //stack of all procedures
   DATA   nProcLevel    //procedure level where the debugger is currently
   DATA   aColors
   DATA   aWatch, aTrace, lTracepoints
   DATA   aLastCommands, nCommand, oGetListCommand
   DATA   lAnimate, lEnd, lCaseSensitive, lMonoDisplay, lSortVars
   DATA   cSearchString, cPathForFiles, cSettingsFileName, aPathDirs
   DATA   nTabWidth, nSpeed
   DATA   lShowPublics, lShowPrivates, lShowStatics, lShowLocals, lAll
   DATA   lShowCallStack
   DATA   lGo           //stores if GO was requested
   DATA   lTrace        //stores if TRACE over procedure was requested
   DATA   nTraceLevel      //procedure level where TRACE was requested
   DATA   lCodeblock INIT .F.
   DATA   lActive INIT .F.
   DATA   lCBTrace INIT .T.   //stores if codeblock tracing is allowed
   DATA   lToCursor INIT .F.
   DATA   aToCursor
   DATA   lNextRoutine INIT .F.
   DATA   oBrwPnt, oWndPnt
   DATA   lppo INIT .F.    //view preprocessed output
   DATA   lRunAtStartup

   METHOD New()
   METHOD Activate()

   METHOD All()

   METHOD Animate() INLINE If( ::lAnimate, ::Step(), nil )

   METHOD BarDisplay()
   METHOD BuildCommandWindow()
   METHOD BuildBrowseStack()

   METHOD CallStackProcessKey( nKey )
   METHOD ClrModal() INLINE iif( ::lMonoDisplay, "N/W, W+/W, W/N, W+/N",;
                                "N/W, R/W, N/BG, R/BG" )

   METHOD CodeWindowProcessKey( nKey )
   METHOD Colors()
   METHOD CommandWindowProcessKey( nKey )
   METHOD EditColor( nColor, oBrwColors )
   METHOD EditSet( nSet, oBrwSets )
   METHOD EditVar( nVar )
   METHOD EndProc()
   METHOD Exit() INLINE ::lEnd := .t.
   METHOD Go() INLINE ::RestoreAppStatus(), ::lGo := .t., ::Exit()
   METHOD GoToLine( nLine )
   METHOD HandleEvent()
   METHOD Hide()
   METHOD HideCallStack()
   METHOD HideVars()
   METHOD InputBox( cMsg, uValue, bValid, lEditable )
   METHOD Inspect( uValue, cValueName )
   METHOD IsBreakPoint( nLine, cPrgName)
   METHOD LoadSettings()
   METHOD LoadVars()
   METHOD LoadCallStack()

   METHOD Local()

   METHOD MonoDisplay()
   METHOD NextWindow()
   METHOD Open()
   METHOD OpenPPO()
//   METHOD Resume() INLINE IIF( LEN(::aCallStack[1])>0, ::ShowCodeLine( ::aCallStack[1][ CSTACK_LINE ], ::aCallStack[1][ CSTACK_MODULE ] ), NIL) 
   METHOD Resume() INLINE ::ShowCodeLine( 1 )
   METHOD OSShell()
   METHOD PathForFiles()

   METHOD PrevWindow()
   METHOD Private()
   METHOD Public()
   METHOD Quit() INLINE ::Exit(), ::Hide(), s_lExit:=.T., s_oDebugger:=NIL, __QUIT()
   METHOD RestoreAppStatus()
   METHOD RestoreSettings()
   METHOD RunAtStartup() INLINE ::lRunAtStartup:=!::lRunAtStartup
   METHOD SaveAppStatus()
   METHOD SaveSettings()
   METHOD Show()
   METHOD ShowAppScreen()
   METHOD ShowCallStack()
   METHOD ShowCodeLine( nLine, cPrgName )
   METHOD StackProc(cModuleName, nProcLevel)
   METHOD ShowHelp( nTopic )
   METHOD ShowVars()
   METHOD RedisplayBreakpoints()
   METHOD LocatePrgPath( cPrgName )
   METHOD Sort() INLINE ASort( ::aVars,,, {|x,y| x[1] < y[1] } ),;
                        ::lSortVars := .t.,;
                        iif( ::oBrwVars != nil, ::oBrwVars:RefreshAll(), nil ),;
          iif( ::oWndVars != nil .and. ::oWndVars:lVisible, iif(!::lGo,::oBrwVars:ForceStable(),),)

   METHOD Speed() INLINE ;
          ::nSpeed := ::InputBox( "Step delay (in tenths of a second)",;
                                  ::nSpeed )

   METHOD Stack()
   METHOD Static()

   METHOD Step() INLINE ::RestoreAppStatus(), ::Exit()

   METHOD TabWidth() INLINE ;
          ::nTabWidth := ::InputBox( "Tab width", ::nTabWidth )

   METHOD ToggleBreakPoint()

   METHOD Trace() INLINE ::lTrace := .t., ::nTraceLevel := Len( ::aCallStack ),;
                         __Keyboard( Chr( 255 ) ) //forces a Step()

   METHOD ToCursor()
   METHOD NextRoutine()
   METHOD CodeblockTrace() INLINE ::lCBTrace := ! ::lCBTrace
   METHOD ViewSets()
   METHOD WndVarsLButtonDown( nMRow, nMCol )
   METHOD LineNumbers()          // Toggles numbering of source code lines
   METHOD Locate()
   METHOD FindNext()
   METHOD FindPrevious()
   METHOD RemoveWindow()
   METHOD SearchLine()
   METHOD ToggleAnimate() INLINE ::lAnimate := ! ::lAnimate
   METHOD ToggleCaseSensitive() INLINE ::lCaseSensitive := ! ::lCaseSensitive
   METHOD ShowWorkAreas() INLINE __dbgShowWorkAreas( Self )

   METHOD TracepointAdd( cExpr )
   METHOD WatchpointAdd( cExpr )
   METHOD WatchpointDel( nPos )
   METHOD WatchpointsShow()   
   METHOD WatchpointsHide()   
   METHOD WatchpointEdit( nVar )
   METHOD WatchGetInfo( aVar )
   
   METHOD VarGetInfo( aVar )
   METHOD VarGetValue( aVar )
   METHOD VarSetValue( aVar, uValue )

   METHOD ResizeWindows( oWindow )
   METHOD NotSupported() INLINE Alert( "Not implemented yet!" )
   
ENDCLASS


METHOD New() CLASS TDebugger

   s_oDebugger := Self

   ::aColors := {"W+/BG","N/BG","R/BG","N+/BG","W+/B","GR+/B","W/B","N/W","R/W","N/BG","R/BG"}
   ::lMonoDisplay      := .f.
   ::aWindows          := {}
   ::nCurrentWindow    := 1
   ::lAnimate          := .f.
   ::lEnd              := .f.
   ::lTrace            := .f.
   ::aBreakPoints      := {}
   ::aWatch            := {}
   ::aTrace            := {}
   ::lTracepoints      := .F.
   ::aCallStack        := {}
   ::aProcStack        := {}
   ::aVars             := {}
   ::lCaseSensitive    := .f.
   ::cSearchString     := ""

   // default the search path for files to the current directory
   // that way if the source is in the same directory it will still be found even if the application
   // changes the current directory with the SET DEFAULT command
   ::cPathForFiles     := getenv( "PATH" )
   ::nTabWidth         := 4
   ::nSpeed            := 0
   ::lShowCallStack    := .f.
   ::lShowPublics      := .f.
   ::lShowPrivates     := .f.
   ::lShowStatics      := .f.
   ::lShowLocals       := .f.
   ::lAll              := .f.
   ::lSortVars         := .f.
   ::cSettingsFileName := "init.cld"
   ::lRunAtStartup     := .t. //Clipper compatible

   if File( ::cSettingsFileName )
      ::LoadSettings()
   endif
   ::lGo            := ::lRunAtStartup

   ::oPullDown      := __dbgBuildMenu( Self )
   ::oPulldown:GetItemByIdent( "ALTD" ):Checked := ::lRunAtStartup

   ::oWndCode       := TDbWindow():New( 1, 0, MaxRow() - 6, MaxCol() )
   ::oWndCode:Cargo       := { ::oWndCode:nTop, ::oWndCode:nLeft }
   ::oWndCode:bKeyPressed := { | nKey | ::CodeWindowProcessKey( nKey ) }
   ::oWndCode:bGotFocus   := { || ::oGetListCommand:SetFocus(), SetCursor( SC_SPECIAL1 ), ;
                              SetPos( ::oWndCode:Cargo[1],::oWndCode:Cargo[2] ) }
   ::oWndCode:bLostFocus  := { || ::oWndCode:Cargo[1] := Row(), ::oWndCode:Cargo[2] := Col(), ;
                              SetCursor( SC_NONE ) }

   AAdd( ::aWindows, ::oWndCode )

   ::BuildCommandWindow()
   ::BuildBrowseStack()

return Self

METHOD PathForFiles() CLASS TDebugger

   ::cPathForFiles := ::InputBox( "Search path for source files:", ::cPathForFiles )
   IF ! RIGHT(::cPathForFiles,1) $ HB_OSPATHDELIMITERS()
     ::cPathForFiles:=::cPathForFiles + HB_OSPATHSEPARATOR()
   ENDIF

return Self

METHOD Activate() CLASS TDebugger

   ::LoadCallStack()
   IF( ! ::lActive )
      ::lActive := .T.
      ::Show()
      if ::lShowCallStack
         ::ShowCallStack()
      endif
   ELSE
      ::SaveAppStatus()
   ENDIF
   ::loadVars()
   ::ShowVars()
   IF( ::oWndPnt != NIL )
      ::WatchpointsShow()
   ENDIF
   // show the topmost procedure
   ::ShowCodeLine( 1 )  //::aCallStack[1][ CSTACK_LINE ], ::aCallStack[1][ CSTACK_MODULE ] )
   ::HandleEvent()

return nil

METHOD All() CLASS TDebugger

   ::lShowPublics := ::lShowPrivates := ::lShowStatics := ;
   ::lShowLocals := ::lAll := ! ::lAll

   iif( ::lAll, (::LoadVars(),::ShowVars()), ::HideVars() )

return nil

METHOD BarDisplay() CLASS TDebugger

   local cClrItem   := __DbgColors()[ 8 ]
   local cClrHotKey := __DbgColors()[ 9 ]

   DispBegin()
   SetColor( cClrItem )
   @ MaxRow(), 0 CLEAR TO MaxRow(), MaxCol()

   DispOutAt( MaxRow(),  0,;
   "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt F10-Trace",;
   cClrItem )
   DispOutAt( MaxRow(),  0, "F1", cClrHotKey )
   DispOutAt( MaxRow(),  8, "F2", cClrHotKey )
   DispOutAt( MaxRow(), 16, "F3", cClrHotKey )
   DispOutAt( MaxRow(), 26, "F4", cClrHotKey )
   DispOutAt( MaxRow(), 34, "F5", cClrHotKey )
   DispOutAt( MaxRow(), 40, "F6", cClrHotKey )
   DispOutAt( MaxRow(), 46, "F7", cClrHotKey )
   DispOutAt( MaxRow(), 54, "F8", cClrHotKey )
   DispOutAt( MaxRow(), 62, "F9", cClrHotKey )
   DispOutAt( MaxRow(), 70, "F10", cClrHotKey )
   DispEnd()

return nil

METHOD BuildCommandWindow() CLASS TDebugger

   local GetList := {}, oGet
   local cCommand

   ::oWndCommand := TDbWindow():New( MaxRow() - 5, 0, MaxRow() - 1, MaxCol(),;
                                    "Command" )

   ::oWndCommand:bGotFocus   := { || ::oGetListCommand:SetFocus(), SetCursor( SC_NORMAL ) }
   ::oWndCommand:bLostFocus  := { || SetCursor( SC_NONE ) }
   ::oWndCommand:bKeyPressed := { | nKey | ::CommandWindowProcessKey( nKey ) }
   ::oWndCommand:bPainted    := { || DispOutAt( ::oWndCommand:nBottom - 1,;
                             ::oWndCommand:nLeft + 1, "> ", __DbgColors()[ 2 ] ),;
                        oGet:ColorDisp( Replicate( __DbgColors()[ 2 ] + ",", 5 ) ),;
                        hb_ClrArea( ::oWndCommand:nTop + 1, ::oWndCommand:nLeft + 1,;
                        ::oWndCommand:nBottom - 2, ::oWndCommand:nRight - 1,;
                        iif( ::lMonoDisplay, 15, HB_ColorToN( __DbgColors()[ 2 ] ) ) ) }
   AAdd( ::aWindows, ::oWndCommand )

   ::aLastCommands := {}
   ::nCommand := 0

   cCommand := Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 )
   // We don't use the GET command here to avoid the painting of the GET
   AAdd( GetList, oGet := Get():New( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3,;
         { | u | iif( PCount() > 0, cCommand := u, cCommand ) }, "cCommand" ) )
   oGet:ColorSpec := Replicate( __DbgColors()[ 2 ] + ",", 5 )
   ::oGetListCommand := HBGetList():New( GetList )

return nil

METHOD BuildBrowseStack() CLASS TDebugger

   if ::oBrwStack == nil
      ::oBrwStack := TBrowseNew( 2, MaxCol() - 14, MaxRow() - 7, MaxCol() - 1 )
      ::oBrwStack:ColorSpec := ::aColors[ 3 ] + "," + ::aColors[ 4 ] + "," + ::aColors[ 5 ]
      ::oBrwStack:GoTopBlock := { || ::oBrwStack:Cargo := 1 }
      ::oBrwStack:GoBottomBlock := { || ::oBrwStack:Cargo := Len( ::aProcStack ) }
      ::oBrwStack:SkipBlock = { | nSkip, nOld | nOld := ::oBrwStack:Cargo,;
                              ::oBrwStack:Cargo += nSkip,;
                              ::oBrwStack:Cargo := Min( Max( ::oBrwStack:Cargo, 1 ),;
                              Len( ::aProcStack ) ), ::oBrwStack:Cargo - nOld }

      ::oBrwStack:Cargo := 1 // Actual highligthed row

      ::oBrwStack:AddColumn( TBColumnNew( "", { || If( Len( ::aProcStack ) > 0,;
            PadC( ::aProcStack[ ::oBrwStack:Cargo ][1], 14 ), Space( 14 ) ) } ) )
   endif

return nil

METHOD CallStackProcessKey( nKey ) CLASS TDebugger

   local n, nSkip, lUpdate := .f.

   do case
      case nKey == K_HOME
           if ::oBrwStack:Cargo > 1
              ::oBrwStack:GoTop()
              ::oBrwStack:ForceStable()
              lUpdate = .t.
           endif

      case nKey == K_END
           if ::oBrwStack:Cargo < Len( ::aProcStack )
              ::oBrwStack:GoBottom()
              ::oBrwStack:ForceStable()
              lUpdate = .t.
           endif

      case nKey == K_UP
           if ::oBrwStack:Cargo > 1
              ::oBrwStack:Up()
              ::oBrwStack:ForceStable()
              lUpdate = .t.
           endif

      case nKey == K_DOWN
           if ::oBrwStack:Cargo < Len( ::aProcStack )
              ::oBrwStack:Down()
              ::oBrwStack:ForceStable()
              lUpdate = .t.
           endif

      case nKey == K_PGUP
           ::oBrwStack:PageUp()
           ::oBrwStack:ForceStable()
           lUpdate = .t.

      case nKey == K_PGDN
           ::oBrwStack:PageDown()
           ::oBrwStack:ForceStable()
           lUpdate = .t.

      case nKey == K_LBUTTONDOWN
           if ( nSkip := MRow() - ::oWndStack:nTop - ::oBrwStack:RowPos ) != 0
              if nSkip > 0
                 for n = 1 to nSkip
                    ::oBrwStack:Down()
                    ::oBrwStack:Stabilize()
                 next
              else
                 for n = 1 to nSkip + 2 step -1
                    ::oBrwStack:Up()
                    ::oBrwStack:Stabilize()
                 next
              endif
              ::oBrwStack:ForceStable()
           endif
           lUpdate = .t.
   endcase

   if lUpdate
      if ::oWndVars != nil .AND. ::oWndVars:lVisible
         ::LoadVars()
         ::ShowVars()
      endif

      // jump to source line for a function
/*      
      if ::aCallStack[ ::oBrwStack:Cargo ][ CSTACK_LINE ] != nil
         ::ShowCodeLine( ::aCallStack[ ::oBrwStack:Cargo ][ CSTACK_LINE ], ::aCallStack[ ::oBrwStack:Cargo ][ CSTACK_MODULE ] )
      else
         ::GotoLine( 1 )
      endif
*/
      ::ShowCodeLine( ::oBrwStack:Cargo )
   endif

return nil

METHOD CodeWindowProcessKey( nKey ) CLASS TDebugger

   do case
      case nKey == K_HOME
           ::oBrwText:GoTop()
           if ::oWndCode:lFocused
              SetCursor( SC_SPECIAL1 )
           endif

      case nKey == K_END
           ::oBrwText:GoBottom()
           ::oBrwText:nCol = ::oWndCode:nLeft + 1
           ::oBrwText:nFirstCol = ::oWndCode:nLeft + 1
           SetPos( Row(), ::oWndCode:nLeft + 1 )
           if ::oWndCode:lFocused
              SetCursor( SC_SPECIAL1 )
           endif

      case nKey == K_LEFT
           ::oBrwText:Left()

      case nKey == K_RIGHT
           ::oBrwText:Right()

      case nKey == K_UP
           ::oBrwText:Up()

      case nKey == K_DOWN
           ::oBrwText:Down()

      case nKey == K_PGUP
           ::oBrwText:PageUp()

      case nKey == K_PGDN
           ::oBrwText:PageDown()

   endcase

return nil

METHOD Colors() CLASS TDebugger

   local oWndColors := TDbWindow():New( 4, 5, 16, MaxCol() - 5,;
                                        "Debugger Colors[1..11]", ::ClrModal() )
   local aColors := { "Border", "Text", "Text High", "Text PPO", "Text Selected",;
                      "Text High Sel.", "Text PPO Sel.", "Menu", "Menu High",;
                      "Menu Selected", "Menu High Sel." }

   local oBrwColors := TBrowseNew( oWndColors:nTop + 1, oWndColors:nLeft + 1,;
                                 oWndColors:nBottom - 1, oWndColors:nRight - 1 )
   local n := 1
   local nWidth := oWndColors:nRight - oWndColors:nLeft - 1
   local oCol

   if ::lMonoDisplay
      Alert( "Monochrome display" )
      return nil
   endif
   oBrwColors:Cargo :={ 1,{}} // Actual highligthed row
   oBrwColors:ColorSpec := ::ClrModal()
   oBrwColors:GOTOPBLOCK := { || oBrwColors:cargo[ 1 ]:= 1 }
   oBrwColors:GoBottomBlock := { || oBrwColors:cargo[ 1 ]:= Len(oBrwColors:cargo[ 2 ][ 1 ])}
   oBrwColors:SkipBlock := { |nPos| ( nPos:= ArrayBrowseSkip(nPos, oBrwColors), oBrwColors:cargo[ 1 ]:= ;
   oBrwColors:cargo[ 1 ] + nPos,nPos ) }

   oBrwColors:AddColumn( ocol := TBColumnNew( "", { || PadR( aColors[ oBrwColors:Cargo[1] ], 14 ) } ) )
   oCol:DefColor:={1,2}
   aadd(oBrwColors:Cargo[2],acolors)
   oBrwColors:AddColumn( oCol := TBColumnNew( "",;
                       { || PadR( '"' + ::aColors[ oBrwColors:Cargo[1] ] + '"', nWidth - 15 ) } ) )
   aadd(oBrwColors:Cargo[2],acolors)
   oCol:DefColor:={1,3}
   ocol:width:=50
   oBrwColors:autolite:=.f.

   oWndColors:bPainted    := { || oBrwColors:ForceStable(),RefreshVarsS(oBrwColors)}

   oWndColors:bKeyPressed := { | nKey | SetsKeyPressed( nKey, oBrwColors,;
                               Len( aColors ), oWndColors, "Debugger Colors",;
                               { || ::EditColor( oBrwColors:Cargo[1], oBrwColors ) } ) }
   oWndColors:ShowModal()

   ::oPullDown:LoadColors()
   ::oPullDown:Refresh()
   ::BarDisplay()

   for n := 1 to Len( ::aWindows )
      ::aWindows[ n ]:LoadColors()
      ::aWindows[ n ]:Refresh()
   next

return nil

METHOD CommandWindowProcessKey( nKey ) CLASS TDebugger

   local cCommand, cResult, oE
   local bLastHandler
   local lDisplay

   do case
      case nKey == K_UP
           if ::nCommand > 0
              ::oGetListCommand:oGet:VarPut( ::aLastCommands[ ::nCommand ] )
              ::oGetListCommand:oGet:Buffer := ::aLastCommands[ ::nCommand ]
              ::oGetListCommand:oGet:Pos := 1
              ::oGetListCommand:oGet:Display()
              if ::nCommand > 1
                 ::nCommand--
              endif
           endif

      case nKey == K_DOWN
           if ::nCommand > 0 .AND. ::nCommand <= Len( ::aLastCommands )
              ::oGetListCommand:oGet:VarPut( ::aLastCommands[ ::nCommand ] )
              ::oGetListCommand:oGet:Buffer := ::aLastCommands[ ::nCommand ]
              ::oGetListCommand:oGet:Pos := 1
              ::oGetListCommand:oGet:Display()
              if ::nCommand < Len( ::aLastCommands )
                 ::nCommand++
              endif
           endif

      case nKey == K_ENTER
           cCommand := ::oGetListCommand:oGet:VarGet()

           if ! Empty( cCommand )
              AAdd( ::aLastCommands, cCommand )
              ::nCommand++
              ::oWndCommand:ScrollUp( 1 )
           endif

           do case
              case Empty( cCommand )
                 lDisplay = .f.

              case SubStr( LTrim( cCommand ), 1, 2 ) == "??" .or. ;
                       SubStr( LTrim( cCommand ), 1, 1 ) == "?"
                 lDisplay := !Empty( cResult := DoCommand( Self,cCommand ) )

              case Upper( SubStr( LTrim( cCommand ), 1, 4 ) ) == "ANIM" .or. ;
                   Upper( SubStr( LTrim( cCommand ), 1, 7 ) ) == "ANIMATE"
                 ::lAnimate = .t.
                 ::Animate()
                 SetCursor( SC_NORMAL )
                 lDisplay = .f.

              case Upper( SubStr( LTrim( cCommand ), 1, 3 ) ) == "DOS"
                 ::OsShell()
                 SetCursor( SC_NORMAL )
                 lDisplay = .f.

              case Upper( SubStr( LTrim( cCommand ), 1, 4 ) ) == "HELP"
                 ::ShowHelp()
                 lDisplay = .f.

              case Upper( SubStr( LTrim( cCommand ), 1, 4 ) ) == "QUIT"
                 ::Quit()

              case Upper( SubStr( LTrim( cCommand ), 1, 6 ) ) == "OUTPUT"
                 SetCursor( SC_NONE )
                 ::ShowAppScreen()
                 SetCursor( SC_NORMAL )
                 lDisplay = .f.

              case Upper( SubStr( LTrim( cCommand ), 1, 3 ) ) == "WP "
                  //add watchpoint
                  ::WatchpointAdd( SUBSTR( LTRIM(cCommand), 3 ) )
                 lDisplay = .f.

              case Upper( SubStr( LTrim( cCommand ), 1, 3 ) ) == "TP "
                  //add tracepoint
                  ::TracepointAdd( SUBSTR( LTRIM(cCommand), 3 ) )
                 lDisplay = .f.

              otherwise
                 cResult = "Command error"
                 lDisplay = .t.

           endcase

           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1,;
              Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ),;
              __DbgColors()[ 2 ] )
           if lDisplay
              DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3,;
                         cResult, __DbgColors()[ 2 ] )
              ::oWndCommand:ScrollUp( 1 )
           endif
           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, "> ",;
              __DbgColors()[ 2 ] )
           cCommand := Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 )
           ::oGetListCommand:oGet:VarPut( cCommand )
           ::oGetListCommand:oGet:Buffer := cCommand
           ::oGetListCommand:oGet:Pos := 1
           ::oGetListCommand:oGet:Display()

      otherwise
          ::oGetListCommand:GetApplyKey( nKey )
   endcase

return nil

METHOD EditColor( nColor, oBrwColors ) CLASS TDebugger

   local GetList    := {}
   local lPrevScore := Set( _SET_SCOREBOARD, .f. )
   local lPrevExit  := Set( _SET_EXIT, .t. )
   local cColor     := PadR( '"' + ::aColors[ nColor ] + '"',;
                             oBrwColors:getColumn(2):Width )

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   SetCursor( SC_NORMAL )
   @ Row(), Col() + 15 GET cColor COLOR SubStr( ::ClrModal(), 5 ) ;
      VALID iif( Type( cColor ) != "C", ( Alert( "Must be string" ), .f. ), .t. )

   READ
   SetCursor( SC_NONE )

   Set( _SET_SCOREBOARD, lPrevScore )
   Set( _SET_EXIT, lPrevExit )

   if LastKey() != K_ESC
      ::aColors[ nColor ] := &cColor
   endif

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

return nil

METHOD EditSet( nSet, oBrwSets ) CLASS TDebugger

   local GetList    := {}
   local lPrevScore := Set( _SET_SCOREBOARD, .f. )
   local lPrevExit  := Set( _SET_EXIT, .t. )
   local cSet       := PadR( ValToStr( Set( nSet ) ), oBrwSets:getColumn(2):Width )
   local cType      := VALTYPE(SET(nSet))

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

   SetCursor( SC_NORMAL )
   @ Row(), Col()+13 GET cSet COLOR SubStr( ::ClrModal(), 5 ) ;
     VALID iif( Type(cSet) != cType, (Alert( "Must be of type '"+cType+"'" ), .f. ), .t. )


   READ
   SetCursor( SC_NONE )

   Set( _SET_SCOREBOARD, lPrevScore )
   Set( _SET_EXIT, lPrevExit )

   if LastKey() != K_ESC
      Set( nSet, &cSet )
   endif

   oBrwSets:RefreshCurrent()
   oBrwSets:ForceStable()

return nil

METHOD VarGetValue( aVar ) CLASS TDebugger
LOCAL nProcLevel, uValue
LOCAL cProc

   IF( aVar[ VAR_TYPE ] = "L" )
      nProcLevel := hb_dbg_procLevel() - aVar[ VAR_LEVEL ]
      cProc := aVar[ VAR_FUNCNAME ]
      uValue := hb_dbg_vmVarLGet( nProcLevel, aVar[ VAR_POS ] )
      
   ELSEIF( aVar[ VAR_TYPE ] = "S" )
      uValue := hb_dbg_vmVarSGet( aVar[VAR_LEVEL], aVar[ VAR_POS ] )

   ELSE
      //Public or Private
      uValue := aVar[ VAR_POS ]
   ENDIF

RETURN uValue


METHOD VarSetValue( aVar, uValue ) CLASS TDebugger
LOCAL nProcLevel
LOCAL cProc

   IF( aVar[ VAR_TYPE ] = "L" )
      nProcLevel := hb_dbg_procLevel() - aVar[VAR_LEVEL]   //skip debugger stack
      cProc := aVar[ VAR_FUNCNAME ]
      hb_dbg_vmVarLSet( nProcLevel, aVar[ VAR_POS ], uValue )
      
   ELSEIF( aVar[ VAR_TYPE ] = "S" )
      hb_dbg_vmVarSSet( aVar[VAR_LEVEL], aVar[ VAR_POS ], uValue )

   ELSE
      //Public or Private
      aVar[ VAR_POS ] := uValue
      &( aVar[ VAR_NAME ] ) := uValue

   ENDIF

RETURN self


METHOD EditVar( nVar ) CLASS TDebugger

   local cVarName   := ::aVars[ nVar ][ 1 ]
   local uVarValue  := ::aVars[ nVar ][ 2 ]
   local cVarType   := ::aVars[ nVar ][ 3 ]
   local aArray
   local cVarStr

   uVarValue := ::VarGetValue( ::aVars[ nVar ] )

   do case
      case ValType( uVarValue ) == "A"
           ::InputBox( cVarName, uVarValue,, .f. )

      case ValType( uVarValue ) == "O"
           ::InputBox( cVarName, uVarValue,, .f. )

      otherwise
           cVarStr := ::InputBox( cVarName, ValToStr( uVarValue ),;
       { | u | If( Type( u ) == "UE", ( Alert( "Expression error" ), .f. ), .t. ) } )
   endcase

   if LastKey() != K_ESC
      do case
      case cVarStr == "{ ... }"
      //    aArray := ::VarGetValue( ::aVars[ nVar ] )
          if Len( aArray ) > 0
              __DbgArrays( uVarValue, cVarName )
          else
              Alert( "Array is empty" )
          endif

      case Upper( SubStr( cVarStr, 1, 5 ) ) == "CLASS"
          __DbgObject( uVarValue, cVarName )

      otherwise
          ::VarSetValue( ::aVars[ nVar ], &cVarStr )
      endcase
   endif

   ::oBrwVars:RefreshCurrent()
   ::oBrwVars:ForceStable()

return nil

METHOD EndProc() CLASS TDebugger

   if Len( ::aCallStack ) > 0
      ADel( ::aCallStack, 1 )
      ASize( ::aCallStack, Len( ::aCallStack ) - 1 )
      if ::oBrwStack != nil .and. ! ::lTrace
         ::oBrwStack:RefreshAll()
      endif
   endif

return nil

METHOD HandleEvent() CLASS TDebugger

   local nPopup, oWnd
   local nKey, nMRow, nMCol, n

   if ::lAnimate
      if ::nSpeed != 0
         Inkey( ::nSpeed / 10 )
      endif
      if HB_DBG_INVOKEDEBUG()  //NextKey() == K_ALT_D
         ::lAnimate := .f.
      else
         KEYBOARD Chr( 255 ) // Forces a Step(). Only 0-255 range is supported
      endif
   endif

   ::lEnd := .f.

   while ! ::lEnd

      nKey := InKey( 0, INKEY_ALL )

      do case
         case nKey == K_ALT_X
              s_oDebugger:Quit()

         case ::oPullDown:IsOpen()
              ::oPullDown:ProcessKey( nKey )
              if ::oPullDown:nOpenPopup == 0 // Closed
                 ::aWindows[ ::nCurrentWindow ]:SetFocus( .t. )
              endif

         case nKey == K_LDBLCLK
              if MRow() == 0

              elseif MRow() == MaxRow()

              else
                 nMRow := MRow()
                 nMCol := MCol()
                 for n := 1 to Len( ::aWindows )
                    if ::aWindows[ n ]:IsOver( nMRow, nMCol )
                       if ! ::aWindows[ n ]:lFocused
                          ::aWindows[ ::nCurrentWindow ]:SetFocus( .f. )
                          ::nCurrentWindow := n
                          ::aWindows[ n ]:SetFocus( .t. )
                       endif
                       ::aWindows[ n ]:LDblClick( nMRow, nMCol )
                       exit
                    endif
                 next
              endif

         case nKey == K_LBUTTONDOWN
              if MRow() == 0
                 if ( nPopup := ::oPullDown:GetItemOrdByCoors( 0, MCol() ) ) != 0
                    if ! ::oPullDown:IsOpen()
                       if ::oWndCode:lFocused
                          Eval( ::oWndCode:bLostFocus )
                       endif
                       SetCursor( SC_NONE )
                    endif
                    ::oPullDown:ShowPopup( nPopup )
                 endif

              elseif MRow() == MaxRow()

              else
                 nMRow := MRow()
                 nMCol := MCol()
                 for n := 1 to Len( ::aWindows )
                    if ::aWindows[ n ]:IsOver( nMRow, nMCol )
                       if ! ::aWindows[ n ]:lFocused
                          ::aWindows[ ::nCurrentWindow ]:SetFocus( .f. )
                          ::nCurrentWindow := n
                          ::aWindows[ n ]:SetFocus( .t. )
                       endif
                       ::aWindows[ n ]:LButtonDown( nMRow, nMCol )
                       exit
                    endif
                 next
              endif

         case nKey == K_RBUTTONDOWN
/*
         case nKey == K_ESC
              ::RestoreAppStatus()
              s_oDebugger := nil
              s_lExit := .T.
              DispEnd()
              ::Exit()
*/
         case nKey == K_UP .or. nKey == K_DOWN .or. nKey == K_HOME .or. ;
              nKey == K_END .or. nKey == K_ENTER .or. nKey == K_PGDN .or. ;
              nKey == K_PGUP .OR. nKey == K_DEL
              oWnd := ::aWindows[ ::nCurrentWindow ]
              oWnd:KeyPressed( nKey )

         case nKey == K_F1
              ::ShowHelp()

         case nKey == K_F4
              ::ShowAppScreen()

         case nKey == K_F5
              // we are starting to run again so reset to the deepest call if
              // displaying stack
              if ! ::oBrwStack == nil
                 ::oBrwStack:GoTop()
              endif
              ::Go()

         case nKey == K_CTRL_F5
              ::NextRoutine()
              
         case nKey == K_F6
              ::ShowWorkAreas()

         case nKey == K_F7
              ::ToCursor()

         case nKey == K_F8 .or. nKey == 255
              // we are starting to run again so reset to the deepest call if
              // displaying stack
              if ! ::oBrwStack == nil
                 ::oBrwStack:GoTop()
              endif
              ::Step()

         case nKey == K_F9
              ::ToggleBreakPoint()

         case nKey == K_F10
              ::Trace()

         case nKey == K_TAB
              ::NextWindow()

         case nKey == K_SH_TAB
              ::PrevWindow()

         case ::oWndCommand:lFocused .and. nKey < 272 // Alt
              ::oWndCommand:KeyPressed( nKey )

         otherwise
              if ( nPopup := ::oPullDown:GetHotKeyPos( __dbgAltToKey( nKey ) ) ) != 0
                 if ::oPullDown:nOpenPopup != nPopup
                    SetCursor( SC_NONE )
                    ::oPullDown:ShowPopup( nPopup )
                 endif
              endif
      endcase
   end

return nil

METHOD Hide() CLASS TDebugger

   RestScreen( ,,,, ::cAppImage )
   ::cAppImage := nil
   SetColor( ::cAppColors )
   SetCursor( ::nAppCursor )

return nil

METHOD MonoDisplay() CLASS TDebugger

   local n

   ::lMonoDisplay := ! ::lMonoDisplay

   ::oPullDown:LoadColors()
   ::oPullDown:Refresh()

   ::BarDisplay()

   for n := 1 to Len( ::aWindows )
      ::aWindows[ n ]:LoadColors()
      ::aWindows[ n ]:Refresh()
   next

return nil

METHOD NextWindow() CLASS TDebugger

   local oWnd

   if Len( ::aWindows ) > 0
      oWnd := ::aWindows[ ::nCurrentWindow++ ]
      oWnd:SetFocus( .f. )
      if ::nCurrentWindow > Len( ::aWindows )
         ::nCurrentWindow := 1
      endif
      while ! ::aWindows[ ::nCurrentWindow ]:lVisible
         ::nCurrentWindow++
         if ::nCurrentWindow > Len( ::aWindows )
            ::nCurrentWindow := 1
         endif
      end
      oWnd := ::aWindows[ ::nCurrentWindow ]
      oWnd:SetFocus( .t. )
   endif

return nil

METHOD PrevWindow() CLASS TDebugger

   local oWnd

   if Len( ::aWindows ) > 0
      oWnd := ::aWindows[ ::nCurrentWindow-- ]
      oWnd:SetFocus( .f. )
      if ::nCurrentWindow < 1
         ::nCurrentWindow := Len( ::aWindows )
      endif
      while ! ::aWindows[ ::nCurrentWindow ]:lVisible
         ::nCurrentWindow--
         if ::nCurrentWindow < 1
            ::nCurrentWindow := Len( ::aWindows )
         endif
      end
      oWnd := ::aWindows[ ::nCurrentWindow ]
      oWnd:SetFocus( .t. )
   endif

return nil

METHOD Show() CLASS TDebugger

   ::cAppImage  := SaveScreen()
   ::nAppRow    := Row()
   ::nAppCol    := Col()
   ::cAppColors := SetColor()
   ::nAppCursor := SetCursor( SC_NONE )

   ::oPullDown:Display()
   ::oWndCode:Show( .t. )
   ::oWndCommand:Show()
   DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ">" )

   ::BarDisplay()

return nil

METHOD ShowAppScreen() CLASS TDebugger

   ::cImage := SaveScreen()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )

   if LastKey() == K_LBUTTONDOWN
      InKey( 0, INKEY_ALL )
      InKey( 0, INKEY_ALL )
   else
      InKey( 0, INKEY_ALL )
   endif

   while LastKey() == K_MOUSEMOVE
      InKey( 0, INKEY_ALL )
   end
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )

return nil

METHOD ShowCallStack() CLASS TDebugger

   local n := 1
   local oCol
   LOCAL i

   ::lShowCallStack = .t.

   if ::oWndStack == nil

      SetCursor( SC_NONE )

      DispBegin()
      // Resize code window
      ::oWndCode:Resize(,,,::oWndCode:nRight-16)
      // Resize vars window
      if ::oWndVars != nil
         ::oWndVars:Resize(,,, ::oWndVars:nRight - 16 )
      endif
      // Resize watchpoints window
      if ::oWndPnt != nil
         ::oWndPnt:Resize(,,, ::oWndPnt:nRight - 16 )
      endif
      DispEnd()

      if ::aWindows[ ::nCurrentWindow ]:lFocused
         ::aWindows[ ::nCurrentWindow ]:SetFocus( .f. )
      endif

      ::oWndStack := TDbWindow():New( 1, MaxCol() - 15, MaxRow() - 6, MaxCol(),;
                                     "Calls" )
      ::oWndStack:bKeyPressed  := { | nKey | ::CallStackProcessKey( nKey ) }
      ::oWndStack:bLButtonDown := { | nKey | ::CallStackProcessKey( K_LBUTTONDOWN ) }

      AAdd( ::aWindows, ::oWndStack )
//      ::nCurrentWindow = Len( ::aWindows )

      if ::oBrwStack == nil
         ::BuildBrowseStack()
      endif

      ::oWndStack:bPainted := { || ::oBrwStack:ColorSpec := __DbgColors()[ 2 ] + "," + ;
                                  __DbgColors()[ 5 ] + "," + __DbgColors()[ 4 ],;
                                  ::oBrwStack:RefreshAll(), ::oBrwStack:ForceStable() }
      ::oWndStack:bGotFocus = { || SetCursor( SC_NONE ) }

      ::oWndStack:Show( .f. )
   endif

return nil

METHOD LoadCallStack() CLASS TDebugger
LOCAL i
LOCAL nDebugLevel
LOCAL nCurrLevel
LOCAL nlevel, nPos

   ::aProcStack := ARRAY( ::nProcLevel )
   nCurrLevel := hb_dbg_ProcLevel() - 1
   nDebugLevel := nCurrLevel - ::nProcLevel +1
   FOR i:=nDebugLevel TO nCurrLevel
      nLevel := nCurrLevel - i +1
      nPos := ASCAN( ::aCallStack, {|a| a[CSTACK_LEVEL]==nLevel} )
      IF( nPos > 0 )
         //a procedure with debug info
         ::aProcStack[i-nDebugLevel+1] := ::aCallStack[ nPos ]
      ELSE
         ::aProcStack[i-nDebugLevel+1] := { PROCNAME( i ), ,,,, nLevel }
      ENDIF
   NEXT

RETURN NIL

METHOD LoadSettings() CLASS TDebugger

   local cInfo := MemoRead( ::cSettingsFileName )
   local n, cLine, nColor, nLen

   nLen := MLCount( cInfo )
   for n := 1 to nLen
      cLine := MemoLine( cInfo, 120, n )
      do case
         case Upper( SubStr( cLine, 1, 14 ) ) == "OPTIONS COLORS"
            cLine := SubStr( cLine, At( "{", cLine ) + 1 )
            nColor := 1
            while nColor < 12
               if At( ",", cLine ) != 0
                  ::aColors[ nColor ] := ;
                     StrTran( SubStr( cLine, 1, At( ",", cLine ) - 1 ), '"', "" )
                  cLine := SubStr( cLine, At( ",", cLine ) + 1 )
               else
                  ::aColors[ nColor ] := ;
                     StrTran( SubStr( cLine, 1, At( "}", cLine ) - 1 ), '"', "" )
               endif
               nColor++
            end

         case Upper( SubStr( cLine, 1, 11 ) ) == "OPTIONS TAB"
            cLine = SubStr( cLine, 12, 3 )
            ::nTabWidth = Val( cLine )

         case Upper( SubStr( cLine, 1, 12 ) ) == "OPTIONS PATH"
            cLine = SubStr( cLine, 13, 120 )
            ::cPathForFiles = AllTrim( cLine )

         case Upper( SubStr( cLine, 1, 22 ) ) == "OPTIONS NORUNATSTARTUP"
            ::lRunAtStartup := .F.

         case Upper( SubStr( cLine, 1, 14 ) ) == "MONITOR STATIC"
            ::lShowStatics = .t.

         case Upper( SubStr( cLine, 1, 14 ) ) == "MONITOR PUBLIC"
            ::lShowPublics = .t.

         case Upper( SubStr( cLine, 1, 13 ) ) == "MONITOR LOCAL"
            ::lShowLocals = .t.

         case Upper( SubStr( cLine, 1, 15 ) ) == "MONITOR PRIVATE"
            ::lShowPrivates = .t.

         case Upper( SubStr( cLine, 1, 12 ) ) == "MONITOR SORT"
            ::lSortVars = .t.

         case Upper( SubStr( cLine, 1, 14 ) ) == "VIEW CALLSTACK"
            ::lShowCallStack = .t.

         case Upper( SubStr( cLine, 1,  2 ) ) == "BP"
            AAdd( ::aBreakPoints,;
               { Val( SubStr( cLine, 4, RAt( " ", cLine ) - 4 ) ),;
                 SubStr( cLine, RAt( " ", cLine ) ) } )

      endcase
   next

return nil

METHOD LoadVars() CLASS TDebugger // updates monitored variables

   local nCount, n, m, xValue, cName
   local cStaticName, nStaticIndex, nStaticsBase
   LOCAL aVars
   LOCAL aBVars
   
   aBVars := {}

   if ::lShowPublics
      nCount := __mvDbgInfo( HB_MV_PUBLIC )
      for n := nCount to 1 step -1
         xValue := __mvDbgInfo( HB_MV_PUBLIC, n, @cName )
         if cName != "__DBGSTATICS"  // reserved public used by the debugger
            AAdd( aBVars, { cName, xValue, "Public" } )
         endif
      next
   endif

   if ::lShowPrivates
      nCount := __mvDbgInfo( HB_MV_PRIVATE )
      for n := nCount to 1 step -1
         xValue := __mvDbgInfo( HB_MV_PRIVATE, n, @cName )
         AAdd( aBVars, { cName, xValue, "Private" } )
      next
   endif

   IF( ::aProcStack[::oBrwStack:Cargo ][ CSTACK_LINE ] != nil )
      if ::lShowStatics
         if Type( "__DbgStatics" ) == "A"
            cName := ::aProcStack[ ::oBrwStack:Cargo ][ CSTACK_MODULE ]
            n := ASCAN( __dbgStatics, {|a| a[1]==cName} )
            IF( n > 0 )
               aVars := __DbgStatics[ n ][ 2 ]
               for m := 1 to Len( aVars )
                  AAdd( aBVars, aVars[ m ] )
               next
            ENDIF
            aVars := ::aProcStack[ ::oBrwStack:Cargo ][ CSTACK_STATICS ]
            for n := 1 to Len( aVars )
               AAdd( aBVars, aVars[ n ] )
            next
         endif
      endif

      if ::lShowLocals
         aVars := ::aProcStack[ ::oBrwStack:Cargo ][ CSTACK_LOCALS ]
         for n := 1 to Len( aVars )
            cName := aVars[ n ][ VAR_NAME ]
            m := AScan( aBVars,; // Is there another var with this name ?
                        { | aVar | aVar[ VAR_NAME ] == cName .AND. aVar[VAR_TYPE]=='S'} )
            IF( m > 0 )
               aBVars[ m ] := aVars[ n ]
            ELSE
               AAdd( aBVars, aVars[ n ] )
            ENDIF
         next
      endif
   ENDIF

   IF( ::oBrwVars != NIL .AND. ::oBrwVars:cargo[1] > LEN(aBVars) )
      ::oBrwVars:gotop()
   ENDIF
   ::aVars := aBVars
   if ::lSortVars
      ::Sort()
   endif

return nil

METHOD ShowHelp( nTopic ) CLASS TDebugger

   local nCursor := SetCursor( SC_NONE )

   __dbgHelp( nTopic )
   SetCursor( nCursor )

return nil

METHOD ShowVars() CLASS TDebugger

   local nWidth, n := 1
   Local oCol
   local lRepaint := .f.
   local nTop

   if ::lGo
      return nil
   endif

   if ! ( ::lShowLocals .or. ::lShowStatics .or. ::lShowPrivates .or. ;
          ::lShowPublics )
      return nil
   endif

   if ::oWndVars == nil

      nTop := IIF(::oWndPnt!=NIL .AND. ::oWndPnt:lVisible,::oWndPnt:nBottom+1,1) 

      ::oWndVars := TDbWindow():New( nTop, 0, nTop+Min( 5, Len( ::aVars )+1 ),;
         MaxCol() - iif( ::oWndStack != nil, ::oWndStack:nWidth(), 0 ),;
         "Monitor:" + iif( ::lShowLocals, " Local", "" ) + ;
         iif( ::lShowStatics, " Static", "" ) + iif( ::lShowPrivates, " Private", "" ) + ;
         iif( ::lShowPublics, " Public", "" ) )

      ::oWndVars:bLButtonDown := { | nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
      ::oWndVars:bLDblClick   := { | nMRow, nMCol | ::EditVar( ::oBrwVars:Cargo[ 1 ] ) }

      ::oBrwVars := TDbgBrowser():New( nTop+1, 1, ::oWndVars:nBottom - 1, MaxCol() - iif( ::oWndStack != nil,;
                               ::oWndStack:nWidth(), 0 ) - 1 )

      ::oWndVars:Browser := ::oBrwVars
      
      ::oBrwVars:Cargo :={ 1,{}} // Actual highligthed row
      ::oBrwVars:ColorSpec := ::aColors[ 2 ] + "," + ::aColors[ 5 ] + "," + ::aColors[ 3 ]
      ::oBrwVars:GOTOPBLOCK := { || ::oBrwVars:cargo[ 1 ] := Min( 1, Len( ::aVars ) ) }
      ::oBrwVars:GoBottomBlock := { || ::oBrwVars:cargo[ 1 ] := MAX(1,Len( ::aVars )) }
      ::oBrwVars:SkipBlock = { | nSkip, nOld | nOld := ::oBrwVars:Cargo[ 1 ],;
                               ::oBrwVars:Cargo[ 1 ] += nSkip,;
                               ::oBrwVars:Cargo[ 1 ] := Min( Max( ::oBrwVars:Cargo[ 1 ], 1 ),;
                                                             Len( ::aVars ) ),;
                               If( Len( ::aVars ) > 0, ::oBrwVars:Cargo[ 1 ] - nOld, 0 ) }

      nWidth := ::oWndVars:nWidth() - 1
      oCol:=TBColumnNew( "", ;
         { || PadR( If( Len( ::aVars ) > 0, ;
                       AllTrim( Str( ::oBrwVars:Cargo[1] -1 ) ) + ") " + ;
                       ::VarGetInfo( ::aVars[ Max( ::oBrwVars:Cargo[1], 1 ) ] ), ;
                       " " ), ;
                   ::oWndVars:nWidth() - 2 ) } )
      ::oBrwVars:AddColumn( oCol )
      AAdd(::oBrwVars:Cargo[2],::aVars)
      oCol:DefColor:={1,2}
      if Len( ::aVars ) > 0
         ::oBrwVars:ForceStable()
      endif

      ::oWndVars:bPainted     := { || if(Len( ::aVars ) > 0, ( ::obrwVars:refreshAll():ForceStable(),RefreshVarsS(::oBrwVars) ),) }

      ::oWndVars:bKeyPressed := { | nKey | ( iif( nKey == K_DOWN ;
      , ::oBrwVars:Down(), nil ), iif( nKey == K_UP, ::oBrwVars:Up(), nil ) ;
      , iif( nKey == K_PGDN, ::oBrwVars:PageDown(), nil ) ;
      , iif( nKey == K_PGUP, ::oBrwVars:PageUp(), nil ) ;
      , iif( nKey == K_HOME, ::oBrwVars:GoTop(), nil ) ;
      , iif( nKey == K_END, ::oBrwVars:GoBottom(), nil ) ;
      , iif( nKey == K_ENTER, ::EditVar( ::oBrwVars:Cargo[1] ), nil ), IIF(LEN(::aVars)>0,::oBrwVars:ForceStable(),nil) ) }
      
      AAdd( ::aWindows, ::oWndVars )
      ::oWndVars:Show()
      ::ResizeWindows( ::oWndVars )

   else
      ::oWndVars:cCaption := "Monitor:" + ;
      iif( ::lShowLocals, " Local", "" ) + ;
      iif( ::lShowStatics, " Static", "" ) + ;
      iif( ::lShowPrivates, " Private", "" ) + ;
      iif( ::lShowPublics, " Public", "" )

      DispBegin()
      if( ::oBrwVars:cargo[1] <= 0 )
         ::oBrwVars:cargo[1] := 1
      endif

      if Len( ::aVars ) == 0
         if ::oWndVars:nBottom - ::oWndVars:nTop > 1
            ::oWndVars:Resize( ,, ::oWndVars:nTop + 1 )
            lRepaint := .t.
         endif
      elseif Len( ::aVars ) > ::oWndVars:nBottom - ::oWndVars:nTop - 1
         ::oWndVars:Resize( ,, ::oWndVars:nTop + Min( Len( ::aVars ) + 1, 7 ) )
         lRepaint := .t.
      elseif Len( ::aVars ) < ::oWndVars:nBottom - ::oWndVars:nTop - 1
         ::oWndVars:Resize( ,, ::oWndVars:nTop + Len( ::aVars ) + 1 )
         lRepaint := .t.
      else
         ::oBrwVars:refreshAll():ForceStable()
      endif
      if ! ::oWndVars:lVisible .OR. lRepaint
         ::ResizeWindows( ::oWndVars )
      endif
      DispEnd()
   endif

return nil

METHOD VarGetInfo( aVar ) CLASS TDebugger
LOCAL uValue

   uValue := ::VarGetValue( aVar )
   do case
   case aVar[ VAR_TYPE ] = "L"
      return aVar[ VAR_NAME ] + " <Local, " + ;
                  ValType( uValue ) + ;
                  ">: " + ValToStr( uValue )

   case aVar[ VAR_TYPE ] = "S"
      return aVar[ VAR_NAME ] + " <Static, " + ;
                  ValType( uValue ) + ;
                  ">: " + ValToStr( uValue )
                  
   OTHERWISE
      return aVar[ VAR_NAME ] + " <" + aVar[ VAR_TYPE ] + ", " + ;
                  ValType( uValue ) + ;
                  ">: " + ValToStr( uValue )

   endcase

return ""

static function CompareLine( Self )

return { | a | a[ 1 ] == Self:oBrwText:nRow }  // it was nLine


METHOD StackProc( cModuleName, nProcLevel ) CLASS TDebugger
   // always treat filename as lower case - we need it consistent for comparisons   
   LOCAL nPos:=RAT( ":", cModuleName )

   ASize( ::aCallStack, Len( ::aCallStack ) + 1 )
   AIns( ::aCallStack, 1 )
   
   // nil means that no line number is stored yet
   ::aCallStack[1]:= { IIF(::lCodeBlock,"(b)","")+SubStr( cModuleName, nPos + 1 ),;    //function name
                       {},;   //local vars
                       nil,;  //line no
                       lower(LEFT( cModuleName, nPos - 1 )),; // and the module name
                       {},;   // static vars
                       nProcLevel }
return nil

//METHOD ShowCodeLine( nLine, cPrgName ) CLASS TDebugger
METHOD ShowCodeLine( nProc ) CLASS TDebugger
LOCAL nPos, nLevel
LOCAL nLine, cPrgName

   // we only update the stack window and up a new browse
   // to view the code if we have just broken execution
   if !::lGo .AND. !::lTrace
      if ::oWndStack != nil
         ::oBrwStack:RefreshAll()
      endif
      

      nLine := ::aProcStack[ nProc ][ CSTACK_LINE ]
      cPrgName := ::aProcStack[ nProc ][ CSTACK_MODULE ]
      IF( nLine == NIL )
         ::oBrwText := nil
         ::oWndCode:Browser := nil
         ::oWndCode:SetCaption( ::aProcStack[ nProc ][ CSTACK_FUNCTION ] +;
                        ": Code not available" )
         ::oWndCode:Refresh()			// to force the window caption to update
         RETURN nil
      ENDIF
      
      if( ::lppo )
         nPos :=RAT(".PRG", UPPER(cPrgName) ) 
         IF( nPos > 0 )
            cPrgName := LEFT( cPrgName, nPos-1 ) + ".ppo"
         ELSE
            cPrgName += cPrgName +".ppo"
         ENDIF
      endif
      if( cPrgName != ::cPrgName .OR. ::oBrwText == NIL )
         if ! File( cPrgName ) .and. ! Empty( ::cPathForFiles )
            cPrgName := ::LocatePrgPath( cPrgName )
         endif
         ::cPrgName := cPrgName
         ::oBrwText := nil
         ::oBrwText := TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                      ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, cPrgName,;
                      __DbgColors()[ 2 ] + "," + __DbgColors()[ 5 ] + "," + ;
                      __DbgColors()[ 3 ] + "," + __DbgColors()[ 6 ] )
         
         ::oWndCode:Browser := ::oBrwText
         ::oWndCode:bPainted :={|| IIF(::oBrwText!=nil,::oBrwText:refreshAll():forceStable(),::oWndCode:Clear()) }
         ::RedisplayBreakpoints()               // check for breakpoints in this file and display them
         ::oWndCode:SetCaption( ::cPrgName )
         ::oWndCode:Refresh()			// to force the window caption to update
      endif
      ::GoToLine( nLine )
   endif
   
return nil

METHOD Open() CLASS TDebugger
LOCAL cFileName := ::InputBox( "Please enter the filename", Space( 255 ) )
LOCAL cPrgName

  cFileName:= ALLTRIM( cFileName )
  if !EMPTY(cFileName) .AND. (cFileName != ::cPrgName .OR. valtype(::cPrgName)=='U')
      if ! File( cFileName ) .and. ! Empty( ::cPathForFiles )
          cFileName := ::LocatePrgPath( cFileName )
      endif
      ::cPrgName := cFileName
      ::lppo := RAT(".PPO", UPPER(cFileName)) > 0
      ::oPulldown:GetItemByIdent( "PPO" ):Checked := ::lppo
      ::oBrwText := nil
      ::oBrwText := TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                   ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, cFileName,;
                   __DbgColors()[ 2 ] + "," + __DbgColors()[ 5 ] + "," + ;
                   __DbgColors()[ 3 ] + "," + __DbgColors()[ 6 ] )
      ::oWndCode:Browser := ::oBrwText
      ::RedisplayBreakpoints()               // check for breakpoints in this file and display them
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()			// to force the window caption to update
   endif
   
return nil

METHOD OpenPPO() CLASS TDebugger
LOCAL nPos
LOCAL lSuccess:=.F.

   nPos := RAT(".PPO", UPPER(::cPrgName))
   IF( nPos == 0 )
      nPos := RAT(".PRG", UPPER(::cPrgName))
      IF( nPos > 0 )
         ::cPrgName := LEFT(::cPrgName,nPos-1) + ".ppo"
      ELSE
         ::cPrgName += ".ppo"
      ENDIF
      lSuccess := FILE(::cPrgName)
      ::lppo := lSuccess
   ELSE
      ::cPrgName := LEFT(::cPrgName,nPos-1) + ".prg"
      lSuccess := FILE( ::cPrgName )
      ::lppo := !lSuccess
   ENDIF
   
   IF( lSuccess )
      ::oBrwText := nil
      ::oBrwText := TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                   ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, ::cPrgName,;
                   __DbgColors()[ 2 ] + "," + __DbgColors()[ 5 ] + "," + ;
                   __DbgColors()[ 3 ] + "," + __DbgColors()[ 6 ] )
      ::oWndCode:Browser := ::oBrwText
      ::RedisplayBreakpoints()               // check for breakpoints in this file and display them
      ::oWndCode:SetCaption( ::cPrgName )
      ::oWndCode:Refresh()			// to force the window caption to update
   endif
   
return lSuccess

// check for breakpoints in the current file and display them
METHOD RedisplayBreakPoints() CLASS TDebugger

   local n
   for n := 1 to Len( ::aBreakpoints )
      if ::aBreakpoints[ n ] [ 2 ] == ::cPrgName
        ::oBrwText:ToggleBreakPoint(::aBreakpoints[ n ] [ 1 ], .T.)
      Endif
   next
return nil

METHOD OSShell() CLASS TDebugger

   local cImage := SaveScreen()
   local cColors := SetColor()
   local cOs    := Upper( OS() )
   local cShell
   local bLastHandler := ErrorBlock({ |objErr| BREAK (objErr) })
   local oE

   SET COLOR TO "W/N"
   CLS
   ? "Type 'exit' to return to the Debugger"
   SetCursor( SC_NORMAL )

   begin sequence
      if At("WINDOWS", cOs) != 0 .OR. At("DOS", cOs) != 0 .OR. At("OS/2", cOs) != 0
         cShell := GetEnv("COMSPEC")
         RUN ( cShell )
      elseif At("LINUX", cOs) != 0
         cShell := GetEnv("SHELL")
         RUN ( cShell )
      else
         Alert( "Not implemented yet!" )
      endif

   recover using oE
      Alert("Error: " + oE:description)

   end sequence

   ErrorBlock(bLastHandler)

   SetCursor( SC_NONE )
   RestScreen( ,,,, cImage )
   SetColor( cColors )

return nil

METHOD HideCallStack() CLASS TDebugger

   ::lShowCallStack = .f.

   if ::oWndStack != nil
      DispBegin()
      ::oWndStack:Hide()
      if ::aWindows[ ::nCurrentWindow ] == ::oWndStack
         ::NextWindow()
      ENDIF
      ::RemoveWindow( ::oWndStack )
      ::oWndStack = nil
      
      ::oWndCode:Resize(,,, ::oWndCode:nRight + 16 )
      if ::oWndVars != nil
         ::oWndVars:Resize(,,, ::oWndVars:nRight + 16 )
      endif
      if ::oWndPnt != nil
         ::oWndPnt:Resize(,,, ::oWndPnt:nRight + 16 )
      endif
      DispEnd()
   endif

return nil

METHOD HideVars() CLASS TDebugger

   ::oWndVars:Hide()
   ::oWndCode:nTop := 1
   ::oBrwText:Resize( ::oWndCode:nTop+1 )
   if ::aWindows[ ::nCurrentWindow ] == ::oWndVars
      ::NextWindow()
   ENDIF

return nil

METHOD InputBox( cMsg, uValue, bValid, lEditable ) CLASS TDebugger

   local nTop    := ( MaxRow() / 2 ) - 5
   local nLeft   := ( MaxCol() / 2 ) - 25
   local nBottom := ( MaxRow() / 2 ) - 3
   local nRight  := ( MaxCol() / 2 ) + 25
   local cType   := ValType( uValue )
   local uTemp   := PadR( uValue, nRight - nLeft - 1 )
   local GetList := {}
   local nOldCursor
   local lScoreBoard := Set( _SET_SCOREBOARD, .f. )
   local lExit
   local oWndInput := TDbWindow():New( nTop, nLeft, nBottom, nRight, cMsg,;
                                       ::oPullDown:cClrPopup )

   DEFAULT lEditable TO .t.

   oWndInput:lShadow := .t.
   oWndInput:Show()

   if lEditable
      if bValid == nil
         @ nTop + 1, nLeft + 1 GET uTemp COLOR "," + __DbgColors()[ 5 ]
      else
         @ nTop + 1, nLeft + 1 GET uTemp VALID Eval( bValid, uTemp ) ;
           COLOR "," + __DbgColors()[ 5 ]
      endif

      nOldCursor := SetCursor( SC_NORMAL )
      READ
      SetCursor( nOldCursor )
   else
      @ nTop + 1, nLeft + 1 SAY ValToStr( uValue ) COLOR "," + __DbgColors()[ 5 ]
      SetPos( nTop + 1, nLeft + 1 )
      nOldCursor := SetCursor( SC_NONE )

      lExit = .f.

      while ! lExit
         Inkey( 0 )

         do case
            case LastKey() == K_ESC
               lExit = .t.

            case LastKey() == K_ENTER
               if cType == "A"
                  if Len( uValue ) == 0
                     Alert( "Array is empty" )
                  else
                     __DbgArrays( uValue, cMsg )
                  endif

               elseif cType == "O"
                  __DbgObject( uValue, cMsg )

               else
                  Alert( "Value cannot be edited" )
               endif

            otherwise
               Alert( "Value cannot be edited" )
         endcase
      end

      SetCursor( nOldCursor )
   endif

   nOldCursor := SetCursor( SC_NORMAL )
   READ
   SetCursor( nOldCursor )
   oWndInput:Hide()
   Set( _SET_SCOREBOARD, lScoreBoard )

   do case
      case cType == "C"
           uTemp := AllTrim( uTemp )

      case cType == "D"
           uTemp := CToD( uTemp )

      case cType == "N"
           uTemp := Val( uTemp )

   endcase

return iif( LastKey() != K_ESC, uTemp, uValue )

METHOD Inspect( uValue, cValueName ) CLASS TDebugger

   uValue = ::InputBox( uValue, cValueName,, .f. )

return nil

METHOD IsBreakPoint( nLine, cPrgName ) CLASS TDebugger
return AScan( ::aBreakPoints, { | aBreak | (aBreak[ 1 ] == nLine) .AND. (aBreak [ 2 ] == cPrgName) } ) != 0


METHOD GotoLine( nLine ) CLASS TDebugger

   local nRow, nCol
/*
   if ::oBrwVars != nil
      ::ShowVars()
   endif
*/
   ::oBrwText:GotoLine( nLine )
   nRow = Row()
   nCol = Col()

   // no source code line stored yet
/*
   if ::oBrwStack != nil .and. Len( ::aCallStack ) > 0 .and. ;
      ::aCallStack[ ::oBrwStack:Cargo ][ CSTACK_LINE ] == nil
      ::aCallStack[ ::oBrwStack:Cargo ][ CSTACK_LINE ] = nLine
   endif
*/
   if ::oWndStack != nil .and. ! ::oBrwStack:Stable
      ::oBrwStack:ForceStable()
   endif

   if ::oWndCode:lFocused .and. SetCursor() != SC_SPECIAL1
      SetPos( nRow, nCol )
      SetCursor( SC_SPECIAL1 )
   endif

return nil

METHOD Local() CLASS TDebugger

   ::lShowLocals := ! ::lShowLocals

   if ::lShowPublics .or. ::lShowPrivates .or. ::lShowStatics .or. ::lShowLocals
      ::LoadVars()
      ::ShowVars()
   else
      ::HideVars()
   endif

return nil

METHOD Private() CLASS TDebugger

   ::lShowPrivates := ! ::lShowPrivates

   if ::lShowPublics .or. ::lShowPrivates .or. ::lShowStatics .or. ::lShowLocals
      ::LoadVars()
      ::ShowVars()
   else
      ::HideVars()
   endif

return nil

METHOD Public() CLASS TDebugger

   ::lShowPublics := ! ::lShowPublics

   if ::lShowPublics .or. ::lShowPrivates .or. ::lShowStatics .or. ::lShowLocals
      ::LoadVars()
      ::ShowVars()
   else
      ::HideVars()
   endif

return nil

METHOD RestoreAppStatus() CLASS TDebugger

   ::cImage := SaveScreen()
   DispBegin()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
   SetPos( ::nAppRow, ::nAppCol )
   SetColor( ::cAppColors )
   SetCursor( ::nAppCursor )
   DispEnd()
   
return nil

METHOD RestoreSettings() CLASS TDebugger

   local n

   ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )

   if LastKey() != K_ESC
      ::LoadSettings()

      ::oPullDown:LoadColors()
      ::oPullDown:Refresh()
      ::BarDisplay()
      ::ShowVars()
      for n := 1 to Len( ::aWindows )
        ::aWindows[ n ]:LoadColors()
        ::aWindows[ n ]:Refresh()
      next
   endif

return nil

METHOD SaveAppStatus() CLASS TDebugger

   DispBegin()
   ::cAppImage  := SaveScreen()
   ::nAppRow    := Row()
   ::nAppCol    := Col()
   ::cAppColors := SetColor()
   ::nAppCursor := SetCursor( SC_NONE )
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )
   DispEnd()

return nil

METHOD SaveSettings() CLASS TDebugger

   local cInfo := "", n, oWnd

   ::cSettingsFileName := ::InputBox( "File name", ::cSettingsFileName )

   if LastKey() != K_ESC

      if ! Empty( ::cPathForFiles )
         cInfo += "Options Path " + ::cPathForFiles + HB_OsNewLine()
      endif

      cInfo += "Options Colors {"
      for n := 1 to Len( ::aColors )
         cInfo += '"' + ::aColors[ n ] + '"'
         if n < Len( ::aColors )
            cInfo += ","
         endif
      next
      cInfo += "}" + HB_OsNewLine()

      if ::lMonoDisplay
         cInfo += "Options mono " + HB_OsNewLine()
      endif

      if !::lRunAtStartup
         cInfo += "Options NoRunAtStartup " + HB_OsNewLine()
      endif

      if ::nSpeed != 0
         cInfo += "Run Speed " + AllTrim( Str( ::nSpeed ) ) + HB_OsNewLine()
      endif

      for n := 1 to Len( ::aWindows )
         oWnd := ::aWindows[ n ]
         cInfo += "Window Size " + AllTrim( Str( oWnd:nBottom - oWnd:nTop + 1 ) ) + " "
         cInfo += AllTrim( Str( oWnd:nRight - oWnd:nLeft + 1 ) ) + HB_OsNewLine()
         cInfo += "Window Move " + AllTrim( Str( oWnd:nTop ) ) + " "
         cInfo += AllTrim( Str( oWnd:nLeft ) ) + HB_OsNewLine()
         cInfo += "Window Next" + HB_OsNewLine()
      next

      if ::nTabWidth != 4
         cInfo += "Options Tab " + AllTrim( Str( ::nTabWidth ) ) + HB_OsNewLine()
      endif

      if ::lShowStatics
         cInfo += "Monitor Static" + HB_OsNewLine()
      endif

      if ::lShowPublics
         cInfo += "Monitor Public" + HB_OsNewLine()
      endif

      if ::lShowLocals
         cInfo += "Monitor Local" + HB_OsNewLine()
      endif

      if ::lShowPrivates
         cInfo += "Monitor Private" + HB_OsNewLine()
      endif

      if ::lSortVars
         cInfo += "Monitor Sort" + HB_OsNewLine()
      endif

      if ::lShowCallStack
         cInfo += "View CallStack" + HB_OsNewLine()
      endif

      if ! Empty( ::aBreakPoints )
         for n := 1 to Len( ::aBreakPoints )
            cInfo += "BP " + AllTrim( Str( ::aBreakPoints[ n ][ 1 ] ) ) + " " + ;
                     AllTrim( ::aBreakPoints[ n ][ 2 ] ) + HB_OsNewLine()
         next
      endif

      MemoWrit( ::cSettingsFileName, cInfo )
   endif

return nil

METHOD Stack() CLASS TDebugger

   if ::lShowCallStack := ! ::lShowCallStack
      ::ShowCallStack()
   else
      ::HideCallStack()
   endif

return nil

METHOD Static() CLASS TDebugger

   ::lShowStatics := ! ::lShowStatics

   ::LoadVars()
   if ::lShowPublics .or. ::lShowPrivates .or. ::lShowStatics .or. ::lShowLocals
      ::ShowVars()
   else
      ::HideVars()
   endif

return nil

// Toggle a breakpoint at the cursor position in the currently viewed file
// which may be different from the file in which execution was broken
METHOD ToggleBreakPoint() CLASS TDebugger
   // look for a breakpoint which matches both line number and program name
   local nAt
   LOCAL cLine
   
   cLine := ::oBrwText:GetLine( ::oBrwText:nRow )
   IF( ::oBrwText:lLineNumbers )
      cLine := SUBSTR( cLine, AT(":",cLine)+1 )
   ENDIF
   IF( IsValidStopLine( cLine ) )
      nAt := AScan( ::aBreakPoints, { | aBreak | aBreak[ 1 ] == ;
                       ::oBrwText:nRow ;
                       .AND. aBreak [ 2 ] == ::cPrgName} ) // it was nLine

      if nAt == 0
         AAdd( ::aBreakPoints, { ::oBrwText:nRow, ::cPrgName } )     // it was nLine
         ::oBrwText:ToggleBreakPoint(::oBrwText:nRow, .T.)
      else
         ADel( ::aBreakPoints, nAt )
         ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
         ::oBrwText:ToggleBreakPoint(::oBrwText:nRow, .F.)
      endif

      ::oBrwText:RefreshCurrent()
   ENDIF

return nil

METHOD ViewSets() CLASS TDebugger

   local oWndSets := TDbWindow():New( 1, 8, MaxRow() - 2, MaxCol() - 8,;
                                      "System Settings[1..47]", ::ClrModal() )
   local aSets := { "Exact", "Fixed", "Decimals", "DateFormat", "Epoch", "Path",;
                    "Default", "Exclusive", "SoftSeek", "Unique", "Deleted",;
                    "Cancel", "Debug", "TypeAhead", "Color", "Cursor", "Console",;
                    "Alternate", "AltFile", "Device", "Extra", "ExtraFile",;
                    "Printer", "PrintFile", "Margin", "Bell", "Confirm", "Escape",;
                    "Insert", "Exit", "Intensity", "ScoreBoard", "Delimeters",;
                    "DelimChars", "Wrap", "Message", "MCenter", "ScrollBreak",;
                    "EventMask", "VideoMode", "MBlockSize", "MFileExt",;
                    "StrictRead", "Optimize", "Autopen", "Autorder", "AutoShare" }

   local oBrwSets := TBrowseNew( oWndSets:nTop + 1, oWndSets:nLeft + 1,;
                                 oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   local n := 1
   local nWidth := oWndSets:nRight - oWndSets:nLeft - 1
   local oCol
   oBrwSets:Cargo :={ 1,{}} // Actual highligthed row
   oBrwSets:autolite:=.f.
   oBrwSets:ColorSpec := ::ClrModal()
   oBrwSets:GOTOPBLOCK := { || oBrwSets:cargo[ 1 ]:= 1 }
   oBrwSets:GoBottomBlock := { || oBrwSets:cargo[ 1 ]:= Len(oBrwSets:cargo[ 2 ][ 1 ])}
   oBrwSets:SKIPBLOCK := { |nPos| ( nPos:= ArrayBrowseSkip(nPos, oBrwSets), oBrwSets:cargo[ 1 ]:= ;
   oBrwSets:cargo[ 1 ] + nPos,nPos ) }
   oBrwSets:AddColumn( ocol := TBColumnNew( "", { || PadR( aSets[ oBrwSets:cargo[ 1 ] ], 12 ) } ) )
   aadd(oBrwSets:Cargo[2],asets)
   ocol:defcolor:={1,2}
   oBrwSets:AddColumn( oCol := TBColumnNew( "",;
                       { || PadR( ValToStr( Set( oBrwSets:cargo[ 1 ]  ) ), nWidth - 13 ) } ) )
   ocol:defcolor:={1,3}
   ocol:width:=40
   oWndSets:bPainted    := { || oBrwSets:ForceStable(),RefreshVarsS(oBrwSets)}
   oWndSets:bKeyPressed := { | nKey | SetsKeyPressed( nKey, oBrwSets, Len( aSets ),;
                            oWndSets, "System Settings",;
                            { || ::EditSet( oBrwSets:Cargo[1], oBrwSets ) } ) }

   SetCursor( SC_NONE )
   oWndSets:ShowModal()

return nil

METHOD WndVarsLButtonDown( nMRow, nMCol ) CLASS TDebugger

   if nMRow > ::oWndVars:nTop .and. ;
      nMRow < ::oWndVars:nBottom .and. ;
      nMCol > ::oWndVars:nLeft .and. ;
      nMCol < ::oWndVars:nRight
      if nMRow - ::oWndVars:nTop >= 1 .and. ;
         nMRow - ::oWndVars:nTop <= Len( ::aVars )
         while ::oBrwVars:RowPos > nMRow - ::oWndVars:nTop
            ::oBrwVars:Up()
            ::oBrwVars:ForceStable()
         end
         while ::oBrwVars:RowPos < nMRow - ::oWndVars:nTop
            ::oBrwVars:Down()
            ::oBrwVars:ForceStable()
         end
      endif
   endif

return nil

static procedure SetsKeyPressed( nKey, oBrwSets, nSets, oWnd, cCaption, bEdit )


   local nSet := oBrwSets:cargo[1]
   local cTemp:=str(nSet,4)

   Local nRectoMove
   do case
      case nKey == K_UP
              oBrwSets:Up()
      case nKey == K_DOWN
              oBrwSets:Down()
      case nKey == K_HOME .or. (nKey == K_CTRL_PGUP) .or. (nKey == K_CTRL_HOME)
              oBrwSets:GoTop()
      case nKey == K_END .or. (nkey == K_CTRL_PGDN) .or. (nkey == K_CTRL_END )
              oBrwSets:GoBottom()
      Case nKey == K_PGDN
              oBrwSets:pageDown()
      Case nKey == K_PGUP
              OBrwSets:PageUp()

      case nKey == K_ENTER
           if bEdit != nil
              Eval( bEdit )
           endif
           if LastKey() == K_ENTER
              KEYBOARD Chr( K_DOWN )
           endif

   endcase
      RefreshVarsS(oBrwSets)

      oWnd:SetCaption( cCaption + "[" + AllTrim( Str( oBrwSets:Cargo[1] ) ) + ;
                       ".." + AllTrim( Str( nSets ) ) + "]" )

return

static procedure SetsKeyVarPressed( nKey, oBrwSets, nSets, oWnd, bEdit )
   Local nRectoMove
   local nSet := oBrwSets:Cargo[1]
   do case
      case nKey == K_UP
              oBrwSets:Up()

      case nKey == K_DOWN
              oBrwSets:Down()

      case nKey == K_HOME .or. (nKey == K_CTRL_PGUP) .or. (nKey == K_CTRL_HOME)
              oBrwSets:GoTop()
      case nKey == K_END .or. (nkey == K_CTRL_PGDN) .or. (nkey == K_CTRL_END )
              oBrwSets:GoBottom()

      Case nKey == K_PGDN
              oBrwSets:pageDown()
      Case nKey == K_PGUP
              OBrwSets:PageUp()

      case nKey == K_ENTER
           if bEdit != nil
              Eval( bEdit )
           endif
           if LastKey() == K_ENTER
              KEYBOARD Chr( K_DOWN )
           endif

   endcase

return

static function ValToStr( uVal )

   local cType := ValType( uVal )
   local cResult := "U"

   do case
      case uVal == nil
           cResult := "NIL"

      case cType == "A"
           cResult := "{ ... }"

      Case cType  =="B"
         cResult:= "{ || ... }"

      case cType $ "CM"
           cResult := '"' + uVal + '"'

      case cType == "L"
           cResult := iif( uVal, ".T.", ".F." )

      case cType == "D"
           cResult := DToC( uVal )

      case cType == "N"
           cResult := AllTrim( Str( uVal ) )

      case cType == "O"
           cResult := "Class " + uVal:ClassName() + " object"
   endcase

return cResult

METHOD LineNumbers() CLASS TDebugger

   ::oBrwText:lLineNumbers := !::oBrwText:lLineNumbers
   ::oBrwText:RefreshAll()

return Self

METHOD Locate( nMode ) CLASS TDebugger

   local cValue

   DEFAULT nMode TO 0

   cValue := ::InputBox( "Search string", ::cSearchString )

   if empty( cValue )
      return nil
   endif

   ::cSearchString := cValue

return ::oBrwText:Search( ::cSearchString, ::lCaseSensitive, 0 )

METHOD FindNext() CLASS TDebugger

   local lFound

   if Empty( ::cSearchString )
      lFound := ::Locate( 1 )
   else
      lFound := ::oBrwText:Search( ::cSearchString, ::lCaseSensitive, 1 )
   endif

return lFound

METHOD FindPrevious() CLASS TDebugger

   local lFound

   if Empty( ::cSearchString )
      lFound := ::Locate( 2 )
   else
      lFound := ::oBrwText:Search( ::cSearchString, ::lCaseSensitive, 2 )
   endif

return lFound

METHOD RemoveWindow( oWnd ) CLASS TDebugger

  local n := AScan( ::aWindows, { | o | o == oWnd } )

  if n != 0
     ::aWindows = ADel ( ::aWindows, n )
     ::aWindows = ASize( ::aWindows, Len( ::aWindows ) - 1 )
  endif

  ::nCurrentWindow = 1

return nil

METHOD SearchLine() CLASS TDebugger

   local cLine

   cLine := ::InputBox( "Line number", "1" )

   if Val( cLine ) > 0
      ::oBrwText:GotoLine ( Val( cLine ) )
   endif

return nil

METHOD LocatePrgPath( cPrgName ) CLASS TDebugger

   local i
   local iMax
   local aPaths
   local cRetPrgName
   local cSep
   
   if empty( ::aPathDirs )
      ::aPathDirs := PathToArray( ::cPathForFiles )
   endif

   cSep := HB_OsPathSeparator()

   aPaths := ::aPathDirs

   iMax := len( aPaths )

   for i := 1 to iMax
       cRetPrgName := aPaths[i] + cSep + cPrgName
       if file( cRetPrgName )
          exit
       else
          cRetPrgName := nil
       endif
   next i

   return cRetPrgName

METHOD ToCursor() CLASS TDebugger
LOCAL cLine

   cLine := ::oBrwText:GetLine( ::oBrwText:nRow )
   IF( ::oBrwText:lLineNumbers )
      cLine := SUBSTR( cLine, AT(":",cLine)+1 )
   ENDIF
   IF( IsValidStopLine( cLine ) )
      ::aToCursor := { ::oBrwText:nRow, ::cPrgName }
      ::RestoreAppStatus()
      ::lToCursor := .t.
      ::Exit()
   ENDIF

RETURN self

METHOD NextRoutine() CLASS TDebugger

   ::RestoreAppStatus()
   ::lNextRoutine := .t.
   ::Exit()

RETURN self


METHOD WatchpointAdd( cExpr ) CLASS TDebugger
LOCAL cErr
LOCAL aWatch

   IF( cExpr == NIL )
      cExpr:=SPACE(255)
      cExpr := ALLTRIM( ::InputBox( "Enter Watchpoint", cExpr ) )
      IF( LASTKEY() == K_ESC )
         RETURN self
      ENDIF
   ENDIF
   cExpr := ALLTRIM( cExpr )
   IF( EMPTY(cExpr) )
      RETURN self
   ENDIF
   aWatch := {"wp", cExpr, NIL}
   cErr := CreateExpression( cExpr, aWatch )
   IF( !EMPTY(cErr) )
      ALERT( cErr )
      RETURN self
   ENDIF
   AADD( ::aWatch, aWatch )
   ::WatchpointsShow()
      
RETURN self

METHOD TracepointAdd( cExpr ) CLASS TDebugger
LOCAL cErr
LOCAL aWatch
LOCAL lSuccess
LOCAL uValue

   IF( cExpr == NIL )
      cExpr:=SPACE(255)
      cExpr := ALLTRIM( ::InputBox( "Enter Tracepoint", cExpr ) )
      IF( LASTKEY() == K_ESC )
         RETURN self
      ENDIF
   ENDIF
   cExpr := ALLTRIM( cExpr )
   IF( EMPTY(cExpr) )
      RETURN self
   ENDIF
   aWatch := {"tp", cExpr, NIL}
   cErr := CreateExpression( cExpr, aWatch )
   IF( !EMPTY(cErr) )
      ALERT( cErr )
      RETURN self
   ENDIF
   AADD( ::aWatch, aWatch )
   uValue := GetWatchValue( aWatch, @lSuccess )
   AADD( ::aTrace, { LEN(::aWatch), IIF(lSuccess, uValue, NIL )} )
   ::lTracepoints :=.T.
   ::WatchpointsShow()
      
RETURN self


METHOD WatchPointsShow() CLASS TDebugger

   local nWidth, n := 1
   Local oCol
   local lRepaint := .f.
   local nTop
   LOCAL lFocused

   if ::lGo
      return nil
   endif

   if LEN(::aWatch) == 0
      return nil
   endif

   if ::oWndPnt == nil
      nTop := IIF(::oWndVars!=NIL .AND. ::oWndVars:lVisible,::oWndVars:nBottom,0) + 1
      ::oWndPnt := TDbWindow():New( nTop,;
         0, ;
         nTop +Min( 4, Len( ::aWatch ) ) + 1,;
         MaxCol() - iif( ::oWndStack != nil, ::oWndStack:nWidth(), 0 ),;
         "Watch" )

//      ::oBrwText:Resize( ::oWndPnt:nBottom + 1 )
//      ::oWndCode:nTop := ::oWndPnt:nBottom + 1
//      ::oBrwText:Resize( ::oWndCode:nTop + 1 )
//      ::oBrwText:RefreshAll()
//      ::oWndCode:SetFocus( .t. )

//      ::oWndPnt:bLButtonDown := { | nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
//      ::oWndPnt:bLDblClick   := { | nMRow, nMCol | ::EditVar( ::oBrwPnt:Cargo[ 1 ] ) }

      ::oBrwPnt := TDbgBrowser():New( nTop+1, 1, ::oWndPnt:nBottom - 1, MaxCol() - iif( ::oWndStack != nil,;
                               ::oWndStack:nWidth(), 0 ) - 1 )

      ::oWndPnt:Browser := ::oBrwPnt

      ::oBrwPnt:Cargo :={ 1,{}} // Actual highligthed row
      ::oBrwPnt:ColorSpec := ::aColors[ 2 ] + "," + ::aColors[ 5 ] + "," + ::aColors[ 3 ]
      ::oBrwPnt:GOTOPBLOCK := { || ::oBrwPnt:cargo[ 1 ] := Min( 1, Len(::aWatch) ) }
      ::oBrwPnt:GoBottomBlock := { || ::oBrwPnt:cargo[ 1 ] := Len( ::aWatch ) }
      ::oBrwPnt:SkipBlock = { | nSkip, nOld | nOld := ::oBrwPnt:Cargo[ 1 ],;
                               ::oBrwPnt:Cargo[ 1 ] += nSkip,;
                               ::oBrwPnt:Cargo[ 1 ] := Min( Max( ::oBrwPnt:Cargo[ 1 ], 1 ),;
                                                             Len( ::aWatch ) ),;
                               IIF( LEN(::aWatch) > 0, ::oBrwPnt:Cargo[ 1 ] - nOld, 0 ) }

      nWidth := ::oWndPnt:nWidth() - 1
      oCol:=TBColumnNew( "", ;
         { || PadR( IIF( LEN( ::aWatch ) > 0, ;
                       AllTrim( Str( ::oBrwPnt:Cargo[1] -1 ) ) + ") " + ;
                       ::WatchGetInfo( ::aWatch[ Max( ::oBrwPnt:Cargo[1], 1 ) ] ), ;
                       " " ), ;
                   ::oWndPnt:nWidth() - 2 ) } )
      ::oBrwPnt:AddColumn( oCol )
      AAdd(::oBrwPnt:Cargo[2], ::aWatch)
      oCol:DefColor:={1,2}

      ::oWndPnt:bPainted := { || if(Len(::aWatch) > 0, ( ::oBrwPnt:refreshAll():ForceStable(),RefreshVarsS(::oBrwPnt) ),) }

      ::oWndPnt:bKeyPressed := { | nKey | ( iif( nKey == K_DOWN ;
      , ::oBrwPnt:Down(), nil ), iif( nKey == K_UP, ::oBrwPnt:Up(), nil ) ;
      , iif( nKey == K_PGDN, ::oBrwPnt:PageDown(), nil ) ;
      , iif( nKey == K_PGUP, ::oBrwPnt:PageUp(), nil ) ;
      , iif( nKey == K_HOME, ::oBrwPnt:GoTop(), nil ) ;
      , iif( nKey == K_END, ::oBrwPnt:GoBottom(), nil ) ;
      , iif( nKey == K_DEL, ::WatchpointDel( ::oBrwPnt:Cargo[1] ), nil ) ;
      , iif( nKey == K_ENTER, ::WatchpointEdit( ::oBrwPnt:Cargo[1] ), nil ), ::oBrwPnt:ForceStable() ) }

      AAdd( ::aWindows, ::oWndPnt )
      ::oWndPnt:Show()
      ::ResizeWindows( ::oWndPnt )
   else
      if( ::oBrwPnt:cargo[1] <= 0 )
         ::oBrwPnt:cargo[1] := 1
      endif
      DispBegin()
      if Len( ::aWatch ) > ::oWndPnt:nBottom - ::oWndPnt:nTop - 1
         //Resize( top, left, bottom, right)
         ::oWndPnt:Resize( ,, ::oWndPnt:nTop + Min( Len( ::aWatch ) + 1, 4 ) )
         lRepaint :=.T.
      elseif Len( ::aWatch ) < ::oWndPnt:nBottom - ::oWndPnt:nTop - 1
         ::oWndPnt:Resize( ,, ::oWndPnt:nTop + Len( ::aWatch ) + 1 )
         lRepaint :=.T.
      endif
      if ! ::oWndPnt:lVisible .OR. lRepaint
         ::ResizeWindows( ::oWndPnt )
      endif
      DispEnd()
   endif

return nil

METHOD WatchpointEdit( nPos ) CLASS TDebugger
LOCAL cExpr
LOCAL aWatch
LOCAL cErr

   cExpr:=PADR( ::aWatch[nPos][WP_EXPR], 255 )
   cExpr := ALLTRIM( ::InputBox( "Enter Watchpoint", cExpr ) )
   IF( LASTKEY() == K_ESC )
      RETURN self
   ENDIF
   cExpr := ALLTRIM( cExpr )
   IF( EMPTY(cExpr) )
      RETURN self
   ENDIF
   aWatch := {"wp", cExpr, NIL}
   cErr := CreateExpression( cExpr, aWatch )
   IF( !EMPTY(cErr) )
      ALERT( cErr )
      RETURN self
   ENDIF
   ::aWatch[ nPos ] := aWatch
   ::WatchpointsShow()
   
RETURN self


METHOD WatchpointDel( nPos ) CLASS TDebugger
LOCAL nIdx

   IF( ::oWndPnt != NIL .AND. ::oWndPnt:lVisible )
      IF( nPos == NIL )
         //called from the menu
         nPos := ::InputBox( "Enter item number to delete", ::oBrwPnt:cargo[1]-1 ) 
      ELSE
         nPos--
      ENDIF
      IF( LastKey() != K_ESC )
         IF( nPos >=0 .AND. nPos < LEN(::aWatch) )
            ::oBrwPnt:gotop()
            IF( ::aWatch[nPos+1][WP_TYPE] == "tp" )
               nIdx := ASCAN( ::aTrace, {|a| a[TR_IDX]==nPos+1} )
               IF( nIdx > 0 )
                  ADEL( ::aTrace[nIdx] )
                  ASIZE( ::aTrace, LEN(::aTrace)-1 )
                  ::lTracepoints := LEN(::aTrace) > 0
               ENDIF
            ENDIF
            ADEL( ::aWatch, nPos+1 )
            ASIZE( ::aWatch, LEN(::aWatch)-1 )
            IF( LEN(::aWatch) == 0 )
               ::WatchpointsHide()
            ELSE
               ::WatchpointsShow()
            ENDIF
         ENDIF 
      ENDIF
   ENDIF
   
RETURN self


METHOD WatchpointsHide() CLASS TDebugger

   ::oWndPnt:Hide()
   ::oWndCode:nTop := IIF(::oWndVars!=NIL .AND. ::oWndVars:lVisible, ::oWndVars:nBottom+1,1)
   ::oBrwText:Resize( ::oWndCode:nTop+1 )
   if ::aWindows[ ::nCurrentWindow ] == ::oWndPnt
      ::NextWindow()
   ENDIF

return nil


STATIC FUNCTION GetWatchValue( aWatch, plSuccess )
LOCAL aVars, i, j
LOCAL nLen, aLocVars
LOCAL cVar, nPos, cName
LOCAL xVal
LOCAL oErr
LOCAL bEBlock

   plSuccess := .F.
   bEblock := ErrorBlock( {|o| BREAK(o)} )
   BEGIN SEQUENCE
      IF( aWatch[WP_BLOCK] != NIL )
         nLen :=LEN(aWatch)-WP_BLOCK
         IF( nLen > 0 )
            aVars := ARRAY( nLen )
            FOR i:=1 TO nLen
               cVar := aWatch[ i + WP_BLOCK ]
               //search local variables in current procedure
               aLocVars := s_oDebugger:aProcStack[1][CSTACK_LOCALS]
               nPos := ASCAN( aLocVars, {|a| a[VAR_NAME]==cVar} )
               IF( nPos > 0 )
                  j :=hb_dbg_ProcLevel() - aLocVars[ nPos ][ VAR_LEVEL ]
                  aVars[i] := hb_dbg_vmVarLGet( j, aLocVars[ nPos ][ VAR_POS ] )
               ELSE
                  //search local statics
                  aLocVars := s_oDebugger:aProcStack[1][CSTACK_STATICS]
                  nPos := ASCAN( aLocVars, {|a| a[VAR_NAME]==cVar} )
                  IF( nPos > 0 )
                     aVars[i] := hb_dbg_vmVarSGet( aLocVars[ nPos ][VAR_LEVEL], aLocVars[ nPos ][VAR_POS] )
                  ELSE
                     //search global statics
                     cName := s_oDebugger:aProcStack[1][CSTACK_MODULE]
                     nPos := ASCAN( __dbgStatics, {|a| a[1]==cName} )
                     IF( nPos > 0 )
                        aLocVars := __dbgStatics[nPos][ 2 ]
                        nPos := ASCAN( aLocVars, {|a| a[VAR_NAME]==cVar} )
                        IF( nPos > 0 )
                           aVars[i] :=hb_dbg_vmVarSGet( aLocVars[ nPos ][VAR_LEVEL], aLocVars[ nPos ][VAR_POS] )
                           EXIT
                        ENDIF
                     ENDIF
                     IF( nPos == 0 )
                        aVars[i] := &cVar
                     ENDIF
                  ENDIF
               ENDIF
            NEXT
         ENDIF
         
         xVal := EVAL( aWatch[WP_BLOCK], aVars )
         plSuccess :=.T.
      ENDIF
      
   RECOVER USING oErr
      xVal := oErr:description
   END SEQUENCE
   ErrorBlock( bEBlock )
   
RETURN xVal

METHOD WatchGetInfo( aWatch ) CLASS TDebugger
LOCAL xVal
LOCAL ctype
LOCAL lValid

   xVal := GetWatchValue( aWatch, @lValid )
   IF( lValid )
      cType := VALTYPE( xVal )
      xVal  := ValToStr( xVal )
   ELSE
      //xVal contains error description
      cType := 'U'
//      xVal  := "Undefined"
   ENDIF
   
RETURN aWatch[WP_EXPR]+" <"+aWatch[WP_TYPE]+", " +cType+">: " +xVal

METHOD ResizeWindows( oWindow ) CLASS TDebugger
LOCAL oWindow2, nTop, i

   IF( oWindow == ::oWndVars )
      oWindow2 := ::oWndPnt
   ELSEIF( oWindow == ::oWndPnt )
      oWindow2 := ::oWndVars
   ENDIF
   
   DispBegin()
   IF( oWindow2 == NIL )
      nTop := oWindow:nBottom +1
   ELSE
      IF( oWindow2:lVisible )
         IF( oWindow:nTop < oWindow2:nTop )
            nTop := oWindow2:nBottom - oWindow2:nTop + 1
            oWindow2:Resize( oWindow:nBottom+1,, oWindow:nBottom+nTop)
         ELSE
            nTop := oWindow:nBottom - oWindow:nTop + 1
            oWindow:Resize( oWindow2:nBottom+1,, oWindow2:nBottom+nTop)
         ENDIF
         nTop := MAX( oWindow:nBottom, oWindow2:nBottom ) + 1
      ELSE
         IF( oWindow:nTop > 1 )
            nTop := oWindow:nBottom - oWindow:nTop + 1
            oWindow:Resize( 1, , nTop )
         ENDIF
         nTop := oWindow:nBottom + 1
      ENDIF
   ENDIF
   
   oWindow:hide()
   IF( oWindow2 != NIL )
      oWindow2:hide()
   ENDIF
   ::oWndCode:Resize( nTop )
   IF( oWindow2 != NIL )
      oWindow2:show()
   ENDIF
   oWindow:show()
   DispEnd()
   
RETURN self


STATIC FUNCTION CreateExpression( cExpr, aWatch )
LOCAL nLen
LOCAL i,j
LOCAL c, lSpace
LOCAL cID, cBV, nPos
LOCAL oErr, oEBlock
LOCAL cRet

   nLen := LEN(cExpr)
   i := j := 1
   lSpace :=.T.
   DO WHILE( i <= nLen )
      c := SUBSTR( cExpr, i, 1 )
      IF( c == '_' .OR. ISALPHA(c) )
         i++
         DO WHILE( i<=nLen .AND. IsIdentChar(c:=SUBSTR(cExpr,i,1)) )
            i++
         ENDDO
         cID := UPPER( SUBSTR( cExpr, j, i-j ) )
         IF( i < nLen )
            DO WHILE( SUBSTR(cExpr,i,1)==" ")
               i++
            ENDDO
            IF( SUBSTR(cExpr,i,1) = '(' )
               //function call
               j := i+1
               LOOP
            ENDIF
            IF( SUBSTR(cExpr,i,2) = "->" )
               //alias expressions are not expanded
               i += 2
               DO WHILE( i<=nLen .AND. IsIdentChar(SUBSTR(cExpr,i,1)," ()") )
                  i++
               ENDDO
               j := i+1
               LOOP
            ENDIF
         ENDIF
         nPos := ASCAN( aWatch, {|c| c==cID}, WP_BLOCK+1 )
         IF( nPos == 0 )
            AADD( aWatch, cID )
            nPos := LEN( aWatch )
         ENDIF
         
         cBV := "__dbg[" +NTRIM(nPos - WP_BLOCK) +"]"
         cExpr := LEFT( cExpr, j-1 ) + cBV + SUBSTR( cExpr, i )
         nLen := LEN(cExpr)
         i := j + LEN(cBV)
         lSpace := .F.

      ELSEIF( c $ " +-*/^!=<>(" )
         lSpace := .T.
         i++
                  
      ELSEIF( c = '&' )    //skip macro expression
         i++
         DO WHILE( i<=nLen .AND. IsIdentChar(SUBSTR(cExpr,i,1)," ()") )
            i++
         ENDDO
      
      ELSEIF( c = ':' )    //skip send operator
         i++
         DO WHILE( i<=nLen .AND. IsIdentChar(SUBSTR(cExpr,i,1)) )
            i++
         ENDDO
      
      ELSEIF( c = "'" .OR. c = '"' )   //STRING
         i++
         DO WHILE( i<=nLen .AND. SUBSTR(cExpr,i,1)!=c )
            i++
         ENDDO
         i++

      ELSEIF( c = "[" )
         IF( lSpace )   
            //STRING
            i++
            DO WHILE( i<=nLen .AND. SUBSTR(cExpr,i,1)!="]" )
               i++
            ENDDO
         ELSE
            //array index
            lSpace := .T.
            i++
         ENDIF
      ELSE
         i++
      ENDIF
      j := i
   ENDDO
   
//      s_oDebugger:InputBox("AFTER", cExpr )
   oEBlock := ErrorBlock( {|o| BREAK(o)} )
   BEGIN SEQUENCE
      aWatch[WP_BLOCK] := &( "{|__dbg|"+ cExpr +"}" ) 
      cRet := NIL
   RECOVER USING oErr
      cRet := "Expression error: " +oErr:description
      aWatch[WP_BLOCK] := NIL
   END SEQUENCE
   ErrorBlock( oEBlock )

RETURN cRet  

STATIC FUNCTION IsIdentChar( cChar, cSeeAlso )

   IF( ISALPHA(cChar) .OR. ISDIGIT(cChar) .OR. cChar = '_' )
      RETURN .T.
   ENDIF
   
RETURN IIF(cSeeAlso!=NIL, cChar $ cSeeAlso, .F. )

STATIC PROCEDURE StripUntil( pcLine, i, cChar )
LOCAL j, n
LOCAL nLen:=LEN(pcLine)

   n := LEN(cChar)
   j := i+n
   DO WHILE( j<=nLen .AND. SUBSTR(pcLine, j, n) != cChar )
      j++
   ENDDO
   IF( j <= nLen )
      pcLine := LEFT( pcLine, i-1 ) + SUBSTR(pcLine, j+n)
   ENDIF

RETURN

STATIC FUNCTION IsValidStopLine( cLine )
LOCAL i, c, c2
   
   cLine := UPPER( ALLTRIM( cLine ) )
   i := 1
   DO WHILE( i <= LEN(cLine) )
      c := SUBSTR( cLine, i, 1 )
      c2 := SUBSTR( cLine, i, 2 )
      DO CASE
      CASE c == '"'
         StripUntil( @cLine, i, c )
          
      CASE c == "'"
         StripUntil( @cLine, i, c )

      CASE c == "["
         StripUntil( @cLine, i, "]" )

      CASE c2 == "//"
         cLine := LEFT( cLine, i-1 )
         
      CASE c2 == "/*"
         StripUntil( @cLine, i, "*/" )

      OTHERWISE
         i++      
      ENDCASE
   ENDDO
   
   cLine := ALLTRIM( cLine )
   IF( EMPTY(cLine) )
      RETURN .F.
   ENDIF
   
   IF( cLine = 'FUNC' .OR.;
       cLine = 'PROC' .OR.;
       cLine = 'NEXT' .OR.;
       cLine = 'END'  .OR.;
       cLine = 'ELSE' .OR.;
       cLine = 'LOCA' .OR.;
       cLine = 'STAT' .OR.;
       cLine = 'MEMV' )
       RETURN .F.
   ENDIF
   
RETURN .T.

function __DbgColors()

return iif( ! s_oDebugger:lMonoDisplay, s_oDebugger:aColors,;
           { "W+/N", "W+/N", "N/W", "N/W", "N/W", "N/W", "W+/N",;
             "N/W", "W+/W", "W/N", "W+/N" } )

function __Dbg()

return s_oDebugger

static function myColors( oBrowse, aColColors )
   local i
   local nColPos := oBrowse:colpos

   for i := 1 to len( aColColors )
      oBrowse:colpos := aColColors[i]
      oBrowse:hilite()
   next

   oBrowse:colpos := nColPos

return nil

static procedure RefreshVarsS( oBrowse )

   local nLen := Len(oBrowse:aColumns)

   if ( nLen == 2 )
      oBrowse:dehilite():colpos:=2
   endif
   oBrowse:dehilite():forcestable()
   if ( nLen == 2 )
      oBrowse:hilite():colpos:=1
   endif
   oBrowse:hilite()

return

static function ArrayBrowseSkip( nPos, oBrwSets, n )

return iif( oBrwSets:cargo[ 1 ] + nPos < 1, 0 - oBrwSets:cargo[ 1 ] + 1 , ;
       iif( oBrwSets:cargo[ 1 ] + nPos > Len(oBrwSets:cargo[ 2 ][ 1 ]), ;
       Len(oBrwSets:cargo[ 2 ][ 1 ]) - oBrwSets:cargo[ 1 ], nPos ) )

/*
 ?? <expr>
   displays inspect window with value or display nothing on error
 ? <expr>
   displays either result or error description in command window
*/      
static function DoCommand( o,cCommand )
LOCAL aCmnd
LOCAL cResult
LOCAL lValid

   cCommand := ALLTRIM( cCommand )
   aCmnd := { NIL, NIL, NIL }
   IF( cCommand = "??" )
      cCommand := SUBSTR( cCommand, 3 )
      aCmnd[WP_TYPE] := "??"
      
   ELSEIF( cCommand = "?" )
      cCommand := SUBSTR( cCommand, 2 )
      aCmnd[WP_TYPE] := "?"
   ENDIF
   aCmnd[WP_EXPR] := cCommand
   cResult := CreateExpression( cCommand, aCmnd )
   IF( EMPTY(cResult) )
      //valid syntax
      cResult := GetWatchValue( aCmnd, @lValid )
      IF( aCmnd[WP_TYPE] == "??" )
         IF( lValid )
            o:Inspect( aCmnd[WP_EXPR], cResult )
         ENDIF
         cResult := ''  //discard result
      ELSE
         IF( lValid )
            cResult := ValToStr( cResult )
         ENDIF
      ENDIF
   ELSE
      IF( aCmnd[WP_TYPE] == "??" )
         cResult := ''  //ignore error
      ENDIF
   ENDIF

RETURN cResult


static function PathToArray( cList )

   local nPos
   local aList := {}
   local cSep

   cSep := HB_OsPathListSeparator()

   if ( cList <> NIL )

      do while ( nPos := at( cSep, cList ) ) <> 0
         aadd( aList, substr( cList, 1, nPos - 1 ) )        // Add a new element
         cList := substr( cList, nPos + 1 )
      enddo

      aadd( aList, cList )              // Add final element

   endif

   return aList 
