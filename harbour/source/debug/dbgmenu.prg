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
   local oCaseSensitive
   local oMonoDisplay
   local oPublic, oPrivate, oStatic, oLocal, oAll, oSort

   MENU oMenu
      MENUITEM " ~File "
      MENU
         MENUITEM " ~Open..."         ACTION oDebugger:Open()
         MENUITEM " ~Resume"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " O~S Shell"        ACTION oDebugger:OSShell()
         SEPARATOR
         MENUITEM " e~Xit    Alt-X "  ACTION oDebugger:Exit(), oDebugger:Hide(), __Quit()
      ENDMENU

      MENUITEM " ~Locate "
      MENU
         MENUITEM " ~Find"            ACTION oDebugger:Locate()
         MENUITEM " ~Next"            ACTION oDebugger:FindNext()
         MENUITEM " ~Previous"        ACTION oDebugger:FindPrevious()
         MENUITEM " ~Goto line..."    ACTION oDebugger:SearchLine()
         SEPARATOR
         MENUITEM oCaseSensitive PROMPT " ~Case sensitive " ;
            ACTION ( oDebugger:ToggleCaseSensitive(), oCaseSensitive:Toggle() )
      ENDMENU

      MENUITEM " ~View "
      MENU
         MENUITEM " ~Sets"            ACTION oDebugger:ViewSets()
         MENUITEM " ~WorkAreas   F6"  ACTION oDebugger:ShowWorkAreas()
         MENUITEM " ~App Screen  F4 " ACTION oDebugger:ShowAppScreen()
         SEPARATOR
         MENUITEM " ~CallStack"       ACTION oDebugger:ShowCallStack()
      ENDMENU

      MENUITEM " ~Run "
      MENU
         MENUITEM " ~Animate"               ACTION oDebugger:Animate()
         MENUITEM " ~Step              F8 " ACTION oDebugger:Step()
         MENUITEM " ~Trace            F10"  ACTION oDebugger:Trace()
         MENUITEM " ~Go                F5"  ACTION oDebugger:Go()
         MENUITEM " to ~Cursor         F7"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " ~Next routine Ctrl-F5"  ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " s~Peed..."              ACTION oDebugger:Speed()
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
         MENUITEM oPublic PROMPT " ~Public" ;
            ACTION ( oDebugger:Public(), oPublic:Toggle() )

         MENUITEM oPrivate PROMPT " pri~Vate " ;
            ACTION ( oDebugger:Private(), oPrivate:Toggle() )

         MENUITEM oStatic PROMPT " ~Static" ;
            ACTION ( oDebugger:Static(), oStatic:Toggle() )

         MENUITEM oLocal PROMPT " ~Local" ;
            ACTION ( oDebugger:Local(), oLocal:Toggle() )

         SEPARATOR
         MENUITEM oAll PROMPT " ~All" ;
            ACTION ( oDebugger:All(), oAll:Toggle(),;
                     oPublic:lChecked := oPrivate:lChecked := oStatic:lChecked := ;
                     oLocal:lChecked := oAll:lChecked )

         MENUITEM " s~Ort" ACTION oDebugger:Sort()
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
         MENUITEM oMonoDisplay PROMPT " mono ~Display" ;
            ACTION ( oDebugger:MonoDisplay(), oMonoDisplay:Toggle() )
         MENUITEM " ~Colors..."             ACTION oDebugger:Colors()
         MENUITEM " ~Tab Width..."          ACTION oDebugger:TabWidth()
         MENUITEM " path for ~Files..."     ACTION oDebugger:PathForFiles()
         SEPARATOR
         MENUITEM " ~Save Settings..."      ACTION oDebugger:SaveSettings()
         MENUITEM " ~Restore Settings... "  ACTION oDebugger:RestoreSettings()
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