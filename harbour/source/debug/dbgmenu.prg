/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Menu
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbclass.ch"

#xcommand MENU [<oMenu>] => [ <oMenu> := ] TDbMenu():New()
#xcommand MENUITEM [ <oMenuItem> PROMPT ] <cPrompt> [ ACTION <uAction,...> ] ;
   [ <checked: CHECK, CHECKED> ] => ;
   [ <oMenuItem> := ] TDbMenu():AddItem( TDbMenuItem():New( <cPrompt>,;
   [{|Self|<uAction>}] ,[<.checked.>] ) )
#xcommand SEPARATOR => TDbMenu():AddItem( TDbMenuItem():New( "-" ) )
#xcommand ENDMENU => ATail( TDbMenu():aMenus ):Build()

function __dbgBuildMenu( oDebugger )  // Builds the debugger pulldown menu

   local oMenu
   local oLineNumbers

   MENU oMenu
      MENUITEM " ~File "
      MENU
         MENUITEM " ~Open..."         ACTION oDebugger:Open()
         MENUITEM " ~Resume"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~OS Shell"        ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " e~Xit    Alt-X "  ACTION oDebugger:Exit(), oDebugger:Hide()
      ENDMENU

      MENUITEM " ~Locate "
      MENU
         MENUITEM " ~Find"            ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Next"            ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Previous"        ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Goto line..."    ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " ~Case sensitive " ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~View "
      MENU
         MENUITEM " ~Sets"            ACTION oDebugger:ViewSets()
         MENUITEM " ~WorkAreas   F6"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~App Screen  F4 " ACTION oDebugger:ShowAppScreen()
         SEPARATOR
         MENUITEM " ~CallStack"       ACTION oDebugger:ShowCallStack()
      ENDMENU

      MENUITEM " ~Run "
      MENU
         MENUITEM " ~Restart"               ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Animate"               ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Step              F8 " ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Trace            F10"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Go                F5"  ACTION oDebugger:Go()
         MENUITEM " to ~Cursor         F7"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Next routine Ctrl-F5"  ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " s~Peed..."              ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~Point "
      MENU
         MENUITEM " ~Watchpoint..."         ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Tracepoint..."         ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Breakpoint   F9 "      ACTION oDebugger:ToggleBreakPoint()
         MENUITEM " ~Delete..."             ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~Monitor "
      MENU
         MENUITEM " ~Public"                ACTION oDebugger:ShowVars()
         MENUITEM " pri~Vate "              ACTION oDebugger:ShowVars()
         MENUITEM " ~Static"                ACTION oDebugger:ShowVars()
         MENUITEM " ~Local"                 ACTION oDebugger:ShowVars()
         SEPARATOR
         MENUITEM " ~All"                   ACTION Alert( "Not implemented yet!" )
         MENUITEM " s~Ort"                  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~Options "
      MENU
         MENUITEM " ~Preprocessed Code"     ACTION Alert( "Not implemented yet!" )
         MENUITEM oLineNumbers PROMPT " ~Line Numbers" ;
            ACTION ( oDebugger:LineNumbers(), oLineNumbers:Toggle() ) CHECKED
         MENUITEM " ~Exchange Screens"      ACTION Alert( "Not implemented yet!" )
         MENUITEM " swap on ~Input"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " code~Block Trace"       ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Menu Bar"              ACTION Alert( "Not implemented yet!" )
         MENUITEM " mono ~Display"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Colors..."             ACTION oDebugger:SelColors()
         MENUITEM " ~Tab Width..."          ACTION Alert( "Not implemented yet!" )
         MENUITEM " path for ~Files..."     ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " ~Save Settings..."      ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Restore Settings... "  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~Window "
      MENU
         MENUITEM " ~Next      Tab "        ACTION oDebugger:NextWindow()
         MENUITEM " ~Prev   Sh-Tab"         ACTION oDebugger:PrevWindow()
         MENUITEM " ~Move"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Size"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Zoom       F2"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Iconize"               ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " ~Tile"                  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " ~Help "
      MENU
         MENUITEM " ~About Help "           ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " ~Keys"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Windows"               ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Menus"                 ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Commands"              ACTION Alert( "Not implemented yet!" )
      ENDMENU

   ENDMENU

return oMenu