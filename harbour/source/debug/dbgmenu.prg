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

#include "hbclass.ch"

#xcommand MENU [<oMenu>] => [ <oMenu> := ] TDbMenu():New()
#xcommand MENUITEM [ <oMenuItem> PROMPT ] <cPrompt> ;
          [ IDENT <nIdent> ] [ ACTION <uAction,...> ] ;
          [ CHECKED <bChecked> ] => ;
   [ <oMenuItem> := ] TDbMenu():AddItem( TDbMenuItem():New( <cPrompt>,;
   [{|Self|<uAction>}], [<bChecked>], [<nIdent>] ) )
#xcommand SEPARATOR => TDbMenu():AddItem( TDbMenuItem():New( "-" ) )
#xcommand ENDMENU => ATail( TDbMenu():aMenus ):Build()

function __dbgBuildMenu( oDebugger )  // Builds the debugger pulldown menu

   local oMenu
   local oLineNumbers
   local oCaseSensitive
   local oMonoDisplay
   local oPublic, oPrivate, oStatic, oLocal, oAll
   local oCallStack
   local oCBTrace
   local oPPo
   local oRunAtStartup

   MENU oMenu
      MENUITEM " ~File "
      MENU
         MENUITEM " ~Open..."         ACTION oDebugger:Open()
         MENUITEM " ~Resume"          ACTION oDebugger:Resume()
         MENUITEM " O~S Shell"        ACTION oDebugger:OSShell()
         SEPARATOR
         MENUITEM " e~Xit    Alt-X "  ACTION oDebugger:Quit()
      ENDMENU

      MENUITEM " ~Locate "
      MENU
         MENUITEM " ~Find"            ACTION oDebugger:Locate()
         MENUITEM " ~Next"            ACTION oDebugger:FindNext()
         MENUITEM " ~Previous"        ACTION oDebugger:FindPrevious()
         MENUITEM " ~Goto line..."    ACTION oDebugger:SearchLine()
         SEPARATOR
         MENUITEM oCaseSensitive PROMPT " ~Case sensitive " IDENT "CASE" ;
            ACTION oDebugger:ToggleCaseSensitive() ;
            CHECKED oDebugger:lCaseSensitive
      ENDMENU

      MENUITEM " ~View "
      MENU
         MENUITEM " ~Sets"            ACTION oDebugger:ViewSets()
         MENUITEM " ~WorkAreas   F6"  ACTION oDebugger:ShowWorkAreas()
         MENUITEM " ~App Screen  F4 " ACTION oDebugger:ShowAppScreen()
         SEPARATOR
         MENUITEM oCallStack PROMPT " ~CallStack" IDENT "CALLSTACK";
            ACTION oDebugger:Stack() ;
            CHECKED oDebugger:lShowCallStack
      ENDMENU

      MENUITEM " ~Run "
      MENU
         MENUITEM " ~Animate" IDENT "ANIMATE" ;
            ACTION ( oDebugger:ToggleAnimate(), oDebugger:Animate() ) ;
            CHECKED oDebugger:lAnimate
         MENUITEM " ~Step              F8 " ACTION oDebugger:Step()
         MENUITEM " ~Trace            F10"  ACTION oDebugger:Trace()
         MENUITEM " ~Go                F5"  ACTION oDebugger:Go()
         MENUITEM " to ~Cursor         F7"  ACTION oDebugger:ToCursor()
         MENUITEM " ~Next routine Ctrl-F5"  ACTION oDebugger:NextRoutine()
         SEPARATOR
         MENUITEM " s~Peed..."              ACTION oDebugger:Speed()
      ENDMENU

      MENUITEM " ~Point "
      MENU
         MENUITEM " ~Watchpoint..."         ACTION oDebugger:WatchpointAdd()
         MENUITEM " ~Tracepoint..."         ACTION oDebugger:TracepointAdd()
         MENUITEM " ~Breakpoint   F9 "      ACTION oDebugger:ToggleBreakPoint()
         MENUITEM " ~Delete..."             ACTION oDebugger:WatchpointDel()
      ENDMENU

      MENUITEM " ~Monitor "
      MENU
         MENUITEM oPublic PROMPT " ~Public" IDENT "PUBLIC" ;
            ACTION oDebugger:Public() ;
            CHECKED oDebugger:lShowPublics

         MENUITEM oPrivate PROMPT " pri~Vate " IDENT "PRIVATE" ;
            ACTION oDebugger:Private() ;
            CHECKED oDebugger:lShowPrivates
            
         MENUITEM oStatic PROMPT " ~Static" IDENT "STATIC" ;
            ACTION oDebugger:Static() ;
            CHECKED oDebugger:lShowStatics
           
         MENUITEM oLocal PROMPT " ~Local" IDENT "LOCAL" ;
            ACTION oDebugger:Local() ;
            CHECKED oDebugger:lShowLocals
            
         SEPARATOR
         MENUITEM oAll PROMPT " ~All" IDENT "ALL" ;
            ACTION oDebugger:All() ;
            CHECKED oDebugger:lAll

         MENUITEM " s~Ort" ACTION oDebugger:Sort()
      ENDMENU

      MENUITEM " ~Options "
      MENU
         MENUITEM oPPo PROMPT " ~Preprocessed Code" IDENT "PPO" ;
            ACTION oDebugger:OpenPPO() ;
            CHECKED oDebugger:lPPO
         MENUITEM oLineNumbers PROMPT " ~Line Numbers" IDENT "LINE" ;
            ACTION oDebugger:LineNumbers() ;
            CHECKED oDebugger:lLineNumbers
         MENUITEM " ~Exchange Screens"      ACTION oDebugger:NotSupported()
         MENUITEM " swap on ~Input"         ACTION oDebugger:NotSupported()
         MENUITEM oCBTrace PROMPT " code~Block Trace" IDENT "CODEBLOCK" ;
            ACTION oDebugger:CodeblockTrace() ;
            CHECKED oDebugger:lCBTrace
         MENUITEM " ~Menu Bar"              ACTION oDebugger:NotSupported()
         MENUITEM oMonoDisplay PROMPT " mono ~Display" IDENT "MONO";
            ACTION oDebugger:MonoDisplay() ;
            CHECKED oDebugger:lMonoDisplay
         MENUITEM " ~Colors..."             ACTION oDebugger:Colors()
         MENUITEM " ~Tab Width..."          ACTION oDebugger:TabWidth()
         MENUITEM " path for ~Files..."     ACTION oDebugger:PathForFiles()
         MENUITEM oRunAtStartup PROMPT " R~un at startup" IDENT "ALTD" ;
            ACTION oDebugger:RunAtStartup() ;
            CHECKED oDebugger:lRunAtStartup
         SEPARATOR
         MENUITEM " ~Save Settings..."      ACTION oDebugger:SaveSettings()
         MENUITEM " ~Restore Settings... "  ACTION oDebugger:RestoreSettings()
      ENDMENU

      MENUITEM " ~Window "
      MENU
         MENUITEM " ~Next      Tab "        ACTION oDebugger:NextWindow()
         MENUITEM " ~Prev   Sh-Tab"         ACTION oDebugger:PrevWindow()
         MENUITEM " ~Move"                  ACTION oDebugger:NotSupported()
         MENUITEM " ~Size"                  ACTION oDebugger:NotSupported()
         MENUITEM " ~Zoom       F2"         ACTION oDebugger:NotSupported()
         MENUITEM " ~Iconize"               ACTION oDebugger:NotSupported()
         SEPARATOR
         MENUITEM " ~Tile"                  ACTION oDebugger:NotSupported()
      ENDMENU

      MENUITEM " ~Help "
      MENU
         MENUITEM " ~About Help "           ACTION oDebugger:ShowHelp( 0 )
         SEPARATOR
         MENUITEM " ~Keys"                  ACTION oDebugger:ShowHelp( 2 )
         MENUITEM " ~Windows"               ACTION oDebugger:ShowHelp( 6 )
         MENUITEM " ~Menus"                 ACTION oDebugger:ShowHelp( 12 )
         MENUITEM " ~Commands"              ACTION oDebugger:ShowHelp( 21 )
      ENDMENU

   ENDMENU

return oMenu