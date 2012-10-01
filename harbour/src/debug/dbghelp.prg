/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Help
 *
 * Copyright 2002 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
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

#pragma DEBUGINFO=OFF

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#include "box.ch"
#include "inkey.ch"

PROCEDURE __dbgHelp( nTopic )

   LOCAL oDlg
   LOCAL cColor := iif( __Dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N", "N/W, N/BG, R/W, R/BG" )
   LOCAL oBrw
   LOCAL aTopics := GetTopics()

   hb_default( @nTopic, 1 )

   oDlg := HBDbWindow():New( 2, 2, MaxRow() - 2, MaxCol() - 2, "Help", cColor )

   oBrw := HBDbBrowser():New( oDlg:nTop + 1, oDlg:nLeft + 1, oDlg:nBottom - 1, oDlg:nLeft + 12 )
   oBrw:Cargo := 1
   oBrw:AddColumn( HBDbColumnNew( "", {|| aTopics[ oBrw:Cargo ][ 1 ] }, 12 ) )
   oBrw:ColorSpec := StrTran( __Dbg():ClrModal(), ", R/W", "" )
   oBrw:SkipBlock := {| nSkip, nOld | nOld := oBrw:Cargo, oBrw:Cargo += nSkip,;
                  oBrw:Cargo := Min( Max( oBrw:Cargo, 1 ), Len( aTopics ) ),;
                  oBrw:Cargo - nOld }
   oBrw:GoTopBlock := {|| oBrw:Cargo := 1 }
   oBrw:GoBottomBlock := {|| oBrw:Cargo := Len( aTopics ) }

   IF nTopic > 1
      Eval( oBrw:SkipBlock, nTopic - 1 )
   ENDIF

   oDlg:bPainted := {|| PaintWindow( oDlg, oBrw, aTopics ) }
   oDlg:bKeyPressed := {| nKey | ProcessKey( nKey, oDlg, oBrw, aTopics, oDlg:cColor ) }

   oDlg:ShowModal()

   RETURN

STATIC PROCEDURE PaintWindow( oDlg, oBrw, aTopics )

   hb_dispBox( oDlg:nTop + 1, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 13, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_dispOutAt( oDlg:nTop , oDlg:nLeft + 13 , Chr( 194 ), oDlg:cColor )
   hb_dispOutAt( oDlg:nBottom , oDlg:nLeft + 13 , Chr( 193 ), oDlg:cColor )

   oBrw:ForceStable()
   ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 ) // Start on page 1

   RETURN

STATIC PROCEDURE ProcessKey( nKey, oDlg, oBrw, aTopics )

   LOCAL n
   LOCAL nSkip

   DO CASE
   CASE nKey == K_UP

      IF oBrw:Cargo > 1
         oBrw:Up()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF

   CASE nKey == K_DOWN

      IF oBrw:Cargo < Len( aTopics )
         oBrw:Down()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF

   CASE nKey == K_HOME

      IF oBrw:Cargo > 1
         oBrw:GoTop()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF

   CASE nKey == K_END

      IF oBrw:Cargo < Len( aTopics )
         oBrw:GoBottom()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF

   CASE nKey == K_PGUP .OR. nKey == K_CTRL_B

      ShowTopic( oDlg, aTopics, oBrw:Cargo, -1 ) // Skip to prev page

   CASE nKey == K_PGDN .OR. nKey == K_CTRL_F .OR. nKey == K_SPACE

      ShowTopic( oDlg, aTopics, oBrw:Cargo, 1 )  // Skip to next page

   CASE nKey == K_LBUTTONDOWN

      IF ( nSkip := MRow() - oDlg:nTop - oBrw:RowPos ) != 0
         IF nSkip > 0
            FOR n := 1 TO nSkip
               oBrw:Down()
               oBrw:Stabilize()
            NEXT
         ELSE
            FOR n := 1 TO nSkip + 2 STEP -1
               oBrw:Up()
               oBrw:Stabilize()
            NEXT
         ENDIF
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF

   ENDCASE

   RETURN

STATIC PROCEDURE ShowTopic( oDlg, aTopics, nTopic, nPageOp )

   local oDebug := __Dbg()
   LOCAL nRows  := oDlg:nBottom - oDlg:nTop - 1
   LOCAL nPages := Len( aTopics[ nTopic ][ 2 ] ) / nRows
   LOCAL nRowsToPaint
   LOCAL n

   IF nPages > 1 .AND. Int( nPages ) < nPages
      nPages := Int( nPages ) + 1
   ENDIF

   IF nPages == 1
      IF nPageOp == -1 .OR. nPageOp == 1
         RETURN
      ENDIF
      oDebug:nHelpPage := 1
   ELSE
      DO CASE
      CASE nPageOp == 0 // Show first page

         oDebug:nHelpPage := 1

      CASE nPageOp == 1 // Show next page

         IF oDebug:nHelpPage < nPages
            oDebug:nHelpPage++
         ELSE
            RETURN
         ENDIF

      CASE nPageOp == -1 // Show prev page

         IF oDebug:nHelpPage > 1
            oDebug:nHelpPage--
         ELSE
            RETURN
         ENDIF

      ENDCASE
   ENDIF

   Scroll( oDlg:nTop + 1, oDlg:nLeft + 14, oDlg:nBottom - 1, oDlg:nRight - 1 )

   nRowsToPaint := Min( nRows, Len( aTopics[ nTopic ][ 2 ] ) - ( ( oDebug:nHelpPage - 1 ) * nRows ) )

   FOR n := 1 TO nRowsToPaint
      hb_dispOutAt( 2 + n, 16, aTopics[ nTopic ][ 2 ][ ( ( oDebug:nHelpPage - 1 ) * nRows ) + n ] )
   NEXT

   IF Len( aTopics[ nTopic ][ 2 ] ) <= nRows
      hb_dispOutAt( oDlg:nBottom, oDlg:nRight - 16, " Page 1 of 1 " )
   ELSE
      hb_dispOutAt( oDlg:nBottom, oDlg:nRight - 16, " Page " + Str( oDebug:nHelpPage, 1 ) + " of " + Str( nPages, 1 ) + " " )
   ENDIF

   RETURN

STATIC FUNCTION GetTopics()

   LOCAL aTopics := { { "About Help  ", },;
                      { "Keys        ", },;
                      { "   Function ", },;
                      { "   Window   ", },;
                      { "   Other    ", },;
                      { "Windows     ", },;
                      { "   Command  ", },;
                      { "   Code     ", },;
                      { "   Watch    ", },;
                      { "   Monitor  ", },;
                      { "   CallStack", },;
                      { "Menus       ", },;
                      { "   File     ", },;
                      { "   Locate   ", },;
                      { "   View     ", },;
                      { "   Run      ", },;
                      { "   Point    ", },;
                      { "   Monitor  ", },;
                      { "   Options  ", },;
                      { "   Window   ", },;
                      { "Commands    ", },;
                      { "Script files", } }

   aTopics[ 1 ][ 2 ] := ;
      { " " + Chr( 24 ) + Chr( 25 ) + "             Select help topic.",;
        " PageUp         Page help text down.",;
        " PageDn         Page help text down.",;
        " Esc            Returns to debugger." }

   aTopics[ 2 ][ 2 ] := ;
      { "Special debugger keys fall into the following",;
        "categories:",;
        "",;
        "    Function Keys",;
        "        Keys that execute debugger functions",;
        "",;
        "    Window keys",;
        "        Keys that operate on the active window",;
        "",;
        "    Others",;
        "        Keys for window navigation and sizing",;
        "",;
        "",;
        "Other keys (typeable characters) are sent to",;
        "the Command window and treated as input text." }

   aTopics[ 3 ][ 2 ] := ;
      { "F1     Help",;
        "F2     Zoom active window",;
        "",;
        "F3     Retype last command",;
        "F4     View Application (User) screen",;
        "",;
        "F5     Go (Run application)",;
        "F6     View Workareas screen",;
        "",;
        "F7     Run to cursor line",;
        "F8     Step",;
        "",;
        "F9     Set breakpoint on cursor line",;
        "F10    Trace" }

   aTopics[ 4 ][ 2 ] := ;
      { "Enter        If input is pending in the Command window,",;
        "             <Enter> will execute the command, regardless",;
        "             of which window is active. Otherwise, if the",;
        "             Monitor or Watch window is active, ENTER will",;
        "             inspect the selected window item.",;
        "",;
        "Up           In Code window, moves cursor line up.",;
        "             In Command window, recalls previous command.",;
        "             In other windows, moves selected item up.",;
        "",;
        "Down         In Code window, moves cursor line down.",;
        "             In Command window, recalls previous command.",;
        "             In other windows, moves selected item down.",;
        "",;
        "PageUp       In Code window, pages source up.",;
        "             In Command window, does nothing.",;
        "             In other windows, pages item list up.",;
        "",;
        "PageDn       In Code window, pages source down.",;
        "             In Command window, does nothing.",;
        "             In other windows, pages item list down.",;
        "",;
        "Ctrl PageUp  In Code window, moves cursor line to top.",;
        "             of source.",;
        "             In Command window, does nothing.",;
        "             In other windows, selects first item on list.",;
        "",;
        "Ctrl PageDn  In Code window, moves cursor line to bottom.",;
        "             of source.",;
        "             In Command window, does nothing",;
        "             In other windows, selects last item on list.",;
        "",;
        "Left         In Code window, scrolls left 1 column.",;
        "             In Command window, moves cursor left.",;
        "             In other windows, does nothing",;
        "",;
        "Right        In Code window, scrolls right 1 column.",;
        "             In Command window, moves cursor right.",;
        "             In other windows, does nothing.",;
        "",;
        "Home         In Code window, scrolls hard left.",;
        "             In Command window, moves cursor to beginning",;
        "             of line.",;
        "             In other windows, does nothing.",;
        "",;
        "End          In Code window, scrolls hard right.",;
        "             In Command window, moves cursor to end",;
        "             of line.",;
        "             In other windows, does nothing.",;
        "",;
        "Esc          In Command window, clears command line.",;
        "             In other windows, does nothing." }

   aTopics[ 5 ][ 2 ] := ;
      { "TAB         Next window",;
        "",;
        "SHIFT-TAB   Previous window",;
        "",;
        "ALT-G       Grow active window",;
        "",;
        "ALT-S       Shrink active window",;
        "",;
        "ALT-U       Move the border between Command and Code",;
        "            windows Up",;
        "",;
        "ALT-D       Move the border between Command and Code",;
        "            windows Down",;
        "",;
        "ALT-X       Exit" }

   aTopics[ 6 ][ 2 ] := ;
      { "The Debugger display consists of the following five",;
        "windows:",;
        "",;
        "    Command Window",;
        "        Accepts and displays debugger commands.",;
        "        Always open.",;
        "",;
        "    Code Window",;
        "        Displays program source code.",;
        "        Always open.",;
        "",;
        "    Watch Window",;
        "        Displays Watchpoints and Tracepoints, and inspects",;
        "        their values.",;
        "        Open when any Watchpoints or Tracepoints are",;
        "        defined. These are set and deleted via the",;
        "        Point menu.",;
        "",;
        "    Monitor Window",;
        "        Displays monitored variables, and inspects their",;
        "        values.",;
        "        Open when any classes of variables are being",;
        "        monitored, via commands in the Monitor menu.",;
        "",;
        "    CallStack Window",;
        "        Displays program call stack.",;
        "        Opened via the View:CallStack menu option.",;
        "",;
        "        If this window is active, the Code, Watch and",;
        "        Monitor windows will display information pertaining",;
        "        to the selected call on the CallStack.",;
        "",;
        "",;
        "One debugger window is active at a time. The active window",;
        "is displayed with a hilighted border. TAB and SHIFT-TAB",;
        "navigate among open windows.",;
        "",;
        "The Window menu contains options to Move, Size, Zoom and",;
        "Iconize the active window.",;
        "",;
        "After a long session of moving and sizing, the Window:Tile",;
        "menu option will restore the windows to their original",;
        "size and location." }

   aTopics[ 7 ][ 2 ] := ;
      { "The Command window accepts debugger commands as line",;
        "input, and displays the response from an executed",;
        "command, if any.",;
        "",;
        "Commands are entered simply by typing in the command",;
        "text, then pressing ENTER.",;
        "",;
        "Commands may be entered and executed while any",;
        "window is active. However, the entry cursor is",;
        "only visible within the Command window when it is",;
        "active.",;
        "",;
        "When the Command window is active, the UP and DOWN",;
        "arrow keys can be used to recall previous commands." }

   aTopics[ 8 ][ 2 ] := ;
      { "The Code window displays Clipper source code for",;
        "the program being debugged.",;
        "",;
        "",;
        "What file the Code window displays may be controlled",;
        "in the following ways:",;
        "",;
        "    1.  By default, the Code window will contain the line",;
        "        of Clipper code currently being executed.",;
        "",;
        "    2.  If the CallStack window is open, the code being",;
        "        viewed is that of the selected call in the CallStack",;
        "        window.",;
        "",;
        "        NOTE: By default this will be the same code as 1,",;
        "        but code for the other calls may be viewed by",;
        "        making the CallStack window active and using UP",;
        "        and DOWN to traverse the call stack.",;
        "",;
        "    3.  A specific file may be viewed by issuing a VIEW",;
        "        command, or selecting the File:View menu option.",;
        "",;
        "",;
        "If the Code window contains the line of Clipper code",;
        "currently being executed, that line will be hilighted.",;
        "Any lines which have Breakpoints set on them will also",;
        "be marked.",;
        "",;
        "",;
        "In addition to standard navigation keys, you can search",;
        "the viewed file for a specific string, or go to a particular",;
        "line within it, using options found in the Locate menu." }

   aTopics[ 9 ][ 2 ] := ;
      { "The Watch window displays Watchpoint and Tracepoint",;
        "expressions, and their current values.",;
        "",;
        "Watchpoints and Tracepoints may be defined and deleted via",;
        "options on the Point menu. You can edit a Watchpoint or",;
        "Tracepoint expression by selecting it in the Watch window",;
        "and pressing CTRL-ENTER.",;
        "",;
        "If the Watch window is active, pressing ENTER will inspect",;
        "the value of the selected expression. The navigation keys",;
        "described in 'Window Keys' may be used to change the",;
        "selected item.",;
        "",;
        "If the CallStack window is active, the Watch window will",;
        "display the values for Watchpoints and Tracepoints at the",;
        "activation level represented by the selected call in the",;
        "CallStack window." }

   aTopics[ 10 ][ 2 ] := ;
      { "The Monitor window displays monitored variables.",;
        "",;
        "Classes of variables may be monitored via options in the",;
        "Monitor menu.",;
        "",;
        "If the Monitor window is active, pressing ENTER will",;
        "inspect the value of the selected variable. The navigation",;
        "keys described in 'Window Keys' may be used to change the",;
        "selected item.",;
        "",;
        "If the CallStack window is active, the Monitor window will",;
        "display the values of variables at the point of the",;
        "activation level represented by the selected call in the",;
        "CallStack window." }

   aTopics[ 11 ][ 2 ] := ;
      { "The CallStack window displays the program's call stack.",;
        "It is opened and closed via the View:CallStack menu",;
        "option.",;
        "",;
        "By default, the selected call within the CallStack window",;
        "is the top one -- i.e., the call currently being executed.",;
        "When the CallStack window is active, the other call levels",;
        "may be selected using the navigation keys described in",;
        "'Window Keys'.",;
        "",;
        "All other windows except the Command window are synchronized",;
        "with the CallStack window. The code viewed in the Code",;
        "window, the values of Watchpoints and Tracepoints in the",;
        "Watch window, and the values of variables in the Monitor",;
        "window are all in the context of the activation level",;
        "selected in the CallStack window." }

   aTopics[ 12 ][ 2 ] := ;
      { "The debugger menus contain various debugger functions.",;
        "",;
        "Each menu may be accessed at any time by pressing the",;
        "ALT key, and the first letter in the menu's name.",;
        "",;
        "Once in a menu, the UP and DOWN arrow keys navigate",;
        "the list of options. An option may be selected by",;
        "pressing ENTER, or by typing the first uppercase",;
        "letter within the name of the desired option.",;
        "",;
        "",;
        "Some menu options toggle a debugger setting. These",;
        "options will have a checkmark displayed to their left",;
        "if the setting they refer to is currently ON.",;
        "",;
        "",;
        "Each menu option is also available as a command,",;
        "made up of the Menu name, followed by the first",;
        "word of the option name. For instance, the",;
        "View:CallStack menu option may also be accessed via",;
        "the command:",;
        "",;
        "    View Call",;
        "",;
        "Words within these commands may be shortened in most",;
        "cases to one letter.",;
        "",;
        "For more information on this class of commands, see",;
        "the 'Commands' section of this help." }

   aTopics[ 13 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Open...",;
        "    Specify a file to be opened in the Code window",;
        "",;
        "    DOS Access",;
        "    Shell to the DOS environment",;
        "",;
        "    Exit    Alt-X",;
        "    Exit the debugger" }

   aTopics[ 14 ][ 2 ] := ;
      { "Facilites for navigating the file in the Code window",;
        "",;
        "Options:",;
        "",;
        "    Find...",;
        "    Search for a specified string, from the beginning",;
        "    of the file onward.",;
        "",;
        "    Next",;
        "    Search for the next occurence of the Find string,",;
        "    from the cursor line onward.",;
        "",;
        "    Prev",;
        "    Search for the previous occurence of the Find string,",;
        "    from the cursor line backward.",;
        "",;
        "    Goto Line...",;
        "    Go to a specific line in the file being viewed.",;
        "",;
        "    Case Sensitivity",;
        "    Toggles case sensitivity in searches. Default is",;
        "    OFF." }

   aTopics[ 15 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Sets",;
        "    View Set status information",;
        "",;
        "    Work Areas   F6",;
        "    View Database status information",;
        "",;
        "    App. screen  F4",;
        "    Displays application screen, until key is pressed",;
        "",;
        "    CallStack",;
        "    Toggles the CallStack window. Default is OFF" }

   aTopics[ 16 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Restart",;
        "    Terminate program and re-execute, leaving debugger",;
        "    settings in place",;
        "",;
        "    Animate",;
        "    Execute program in Animate mode",;
        "",;
        "    Step              F8",;
        "    Execute one line of program code",;
        "",;
        "    Trace            F10",;
        "    Trace over function call in program code",;
        "",;
        "    Go                F5",;
        "    Execute program",;
        "",;
        "    to Cursor         F7",;
        "    Execute program, breaking at the current cursor",;
        "    line in Code window",;
        "",;
        "    Next routine Ctrl-F5",;
        "    Execute program, breaking at line 1 of the next",;
        "    procedure or function call.,",;
        "",;
        "    sPeed...",;
        "    Set step speed for Animate mode execution" }

   aTopics[ 17 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Watchpoint...",;
        "",;
        "    Add Watchpoint. A Watchpoint is an expression which",;
        "    the debugger tracks during program execution -- the",;
        "    current value of a Watchpoint is displayed in the",;
        "    Watch window during debugging.",;
        "",;
        "    A Watchpoint may be any valid Clipper expression,",;
        "    i.e.:",;
        "",;
        "        s                   // variable",;
        "        a[n]                // array element",;
        "        g:buffer            // object instance variable",;
        "        At(s, t)            // return value of function call",;
        "        ValType(s) == 'C'   // value of expression",;
        "",;
        "",;
        "    Tracepoint...",;
        "",;
        "    Add Tracepoint. A Tracepoint is similar to a Watchpoint,",;
        "    with the additional property that if the value of a",;
        "    Tracepoint expression changes, the debugger will be",;
        "    invoked as if a Breakpoint had been hit.",;
        "",;
        "",;
        "    Breakpoint   F9",;
        "",;
        "    Set Breakpoint on current cursor line in",;
        "    Code window",;
        "",;
        "",;
        "    Delete",;
        "",;
        "    Delete Tracepoint or Watchpoint." }

   aTopics[ 18 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Public",;
        "    Monitor Public variables.",;
        "",;
        "    Private",;
        "    Monitor Private variables.",;
        "",;
        "    Local",;
        "    Monitor Local variables.",;
        "",;
        "    Static",;
        "    Monitor Static variables.",;
        "",;
        "    Global",;
        "    Monitor Global variables.",;
        "",;
        "    All",;
        "    Monitor All variables.",;
        "",;
        "    Sort",;
        "    Toggles whether monitored variables are sorted by",;
        "    name. Default is OFF." }

   aTopics[ 19 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Preprocessed code",;
        "    Toggles the display of preprocessed code (from",;
        "    PPO file) within the Code window. Default is OFF.",;
        "",;
        "    Line numbers",;
        "    Toggles the display of line numbers in the Code",;
        "    window. Default is OFF.",;
        "",;
        "    Exchange screens",;
        "    Toggles whether debugger screen is swapped with",;
        "    application screen during debugger execution.",;
        "    Default is ON.",;
        "",;
        "    Swap on Input",;
        "    Toggles whether debugger screen is swapped with",;
        "    application screen when the program being debugged",;
        "    is waiting for input. Default is ON.",;
        "    This setting is only meaningful when the Exchange",;
        "    Screens setting is OFF.",;
        "",;
        "    Codeblock Trace",;
        "    Toggles whether the debugger will trace into",;
        "    code blocks when tracing (i.e., when in Trace",;
        "    mode). Defaults to ON.",;
        "",;
        "    Menu Bar",;
        "    Toggles display of the debugger menu bar.",;
        "    Default is ON.",;
        "",;
        "    mono Display",;
        "    Toggles display between monochrome and color.",;
        "    Default is OFF.",;
        "",;
        "    Colors...",;
        "    Inspects debugger colors.",;
        "",;
        "    Tab width...",;
        "    Set tab width in Code window. Default is 4.",;
        "",;
        "    pAth for Files...",;
        "    Specify search path for source files.",;
        "    The debugger will use this path to search for",;
        "    files, if not found in the current directory.",;
        "    NOTE: If not found in the debugger path, the",;
        "    directories specified in the environment's PATH",;
        "    will be searched.",;
        "",;
        "",;
        "    Save Settings",;
        "    Save debugger settings to a script file.",;
        "",;
        "    Restore Settings",;
        "    Restore debugger settings from a previously",;
        "    saved script file." }

   aTopics[ 20 ][ 2 ] := ;
      { "Options:",;
        "",;
        "    Next       Tab",;
        "    Make next window active.",;
        "",;
        "    Prev    Sh-Tab",;
        "    Make previous window active.",;
        "",;
        "    Move",;
        "    Move active window. UP, DOWN, LEFT, RIGHT,",;
        "    PGUP, PGDN, HOME, END move ENTER finishes,",;
        "    While Moving, top left corner of window is marked.",;
        "",;
        "    Size",;
        "    Size active window. UP, DOWN, LEFT, RIGHT,",;
        "    PGUP, PGDN, HOME, END size ENTER finishes,",;
        "    While Sizing, bottom right corner of window is marked.",;
        "",;
        "    Zoom        F2",;
        "    Toggles whether active window is Zoomed. When",;
        "    Zoomed, window will fill entire display area.",;
        "",;
        "    Iconize",;
        "    Toggles whether active window is Iconized. When",;
        "    Iconized, window will be one row high and a few",;
        "    columns wide.",;
        "",;
        "    Tile",;
        "    Restore all windows to original size and position." }

   aTopics[ 21 ][ 2 ] := ;
      { "There are two sets of debugger commands:",;
        "",;
        "1. Menu option commands. These commands are formed",;
        "from the menu name, followed by the (first word of)",;
        "the option name. For instance, the Monitor:Public",;
        "menu option may be invoked via the command:",;
        "",;
        "    Monitor Public",;
        "",;
        "These commands may be abbreviated down to one letter",;
        "per word. However in some cases a second letter will",;
        "be required in the second word, as in the case of",;
        "Monitor Private:",;
        "",;
        "    M P     // invokes Monitor Public",;
        "    M Pr    // invokes Monitor Private",;
        "",;
        "",;
        "",;
        "2. Other commands. Listed below.",;
        "",;
        "",;
        "? <exp>",;
        "    Display the value of a variable or expression.",;
        "",;
        "?? <exp>",;
        "    Inspect the value of a variable or expression.",;
        "",;
        "ANIMATE",;
        "    Execute application in Animate Mode.",;
        "",;
        "BP [<nLineNum> [ <cFileName> ]]|[<cFuncName>]",;
        "",;
        "   BP",;
        "      Toggle breakpoint at current line in current",;
        "      source file.",;
        "",;
        "   BP <nLineNum>",;
        "      Toggle breakpoint at <nLineNum> in current source",;
        "      file.",;
        "",;
        "   BP <nLineNum> <cFileName>",;
        "      Toggle breakpoint at <nLineNum> in <cFileName>",;
        "      source file.",;
        "   BP <cFuncName>",;
        "     Toggle breakpoint on function.",;
        "",;
        "CALLSTACK on|OFF",;
        "    Toggle display of CallStack window",;
        "",;
        "DELETE ALL [WP|TP|BP]",;
        "DELETE WP|TP|BP <nNumber>",;
        "    Delete all or particular Watchpoint, Tracepoint",;
        "    or Breakpoint.",;
        "",;
        "DOS",;
        "    Visit the operating system.",;
        "",;
        "FIND <cSearchString>",;
        "    Search currently viewed file for specified",;
        "    character string.",;
        "",;
        "GO",;
        "    Execute application in Run Mode.",;
        "",;
        "GOTO <nLineNum>",;
        "    Move cursor to specified line in currently viewed",;
        "    file.",;
        "",;
        "HELP",;
        "    Get advice in the form of the Help window.",;
        "",;
        "INPUT <cFileName>",;
        "    Read commands from specified Script File.",;
        "",;
        "LIST BP|WP|TP",;
        "    List Breakpoints, Watchpoints or Tracepoints in",;
        "    the Command Window.",;
        "",;
        "NEXT",;
        "    Search for next occurence of FIND string.",;
        "",;
        "NUM ON|off",;
        "    Toggle display of line numbers in Code window.",;
        "",;
        "OUTPUT",;
        "    View application screen.",;
        "",;
        "",;
        "PREV",;
        "    Search for previous occurence of FIND string.",;
        "",;
        "QUIT",;
        "    Quit.",;
        "",;
        "RESTART",;
        "    Restart application",;
        "",;
        "RESUME",;
        "    Resume viewing the currently executing program",;
        "    code in the Code Window, after VIEWing another",;
        "    file.",;
        "",;
        "SPEED <nSpeed>",;
        "    Set Animate mode step speed. <nSpeed> designates",;
        "    the number of tenths of a second to delay.",;
        "    <nSpeed> must be greater than or equal to 0.",;
        "",;
        "STEP",;
        "    Execute one line of program code.",;
        "",;
        "TP <exp>",;
        "    Establish <exp> as a Tracepoint. <exp> may be a",;
        "    variable or expression.",;
        "",;
        "VIEW <cFileName>",;
        "   View specified file in Code window.",;
        "",;
        "WP <exp>",;
        "    Establish <exp> as a Watchpoint. <exp> may be a",;
        "    variable or expression." }

   aTopics[ 22 ][ 2 ] := ;
      { "Script files contain debugger commands, in the same",;
        "form they would take as input in the Command window.",;
        "By default, script files use the extension CLD, as in",;
        "'myscript.cld'.",;
        "",;
        "",;
        "Creating a script file:",;
        "",;
        "A script file containing all the debugger's current",;
        "settings may be created via the Options:Save menu",;
        "option. A script file may also be written by hand,",;
        "in a text editor.",;
        "",;
        "",;
        "Reading a script file:",;
        "",;
        "A script file may be read into the debugger at any",;
        "time using the Options:Restore menu option.",;
        "",;
        "When using CLD.EXE, a script file may also be",;
        "specified on the command line, before the name of",;
        "the program to be debugged, i.e.:",;
        "",;
        "    CLD @<ScriptName> <ProgName>",;
        "",;
        "In both of these, the extension '.cld' will be assumed",;
        "if no extension is supplied.",;
        "",;
        "When reading a script file, the debugger will look",;
        "for the file in the current directory first. If the",;
        "script is not found there, the debugger will search",;
        "all directories in the PATH environment variable.",;
        "",;
        "",;
        "init.cld:",;
        "",;
        "On startup (or, if it is linked into a program, when",;
        "it is first invoked), the debugger will look for a",;
        "script file called init.cld, in the current directory",;
        "and then, if not found, in the directories specified",;
        "by the PATH environment variable.",;
        "",;
        "If init.cld is found, the debugger will read it",;
        "automatically. It is useful to place general",;
        "preferences in init.cld -- specifying colors,",;
        "turning on the CallStack window, and so on." }

   RETURN aTopics
