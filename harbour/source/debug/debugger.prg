/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszel] */

#include "hbclass.ch"
#include "hbmemvar.ch"
#include "box.ch"
#include "inkey.ch"
#include "common.ch"
#include "setcurs.ch"

#define ALTD_DISABLE   0
#define ALTD_ENABLE    1

static s_oDebugger
static s_lExit := .F.
static s_lEnabled := .t.

function AltD( nAction )

   do case
      case nAction == nil
           if s_lEnabled
              s_lExit = .f.
              __dbgEntry( ProcLine( 2 ) )
           endif

      case nAction == ALTD_DISABLE
           s_lEnabled = .f.

      case nAction == ALTD_ENABLE
           s_lEnabled = .t.
   endcase

return nil

function __dbgEntry( uParam1, uParam2 )  // debugger entry point

   do case
      case ValType( uParam1 ) == "C"   // called from hvm.c hb_vmModuleName()
           if ! s_lExit
              if s_oDebugger == nil
                 s_oDebugger = TDebugger():New()
                 s_oDebugger:Activate( uParam1 )
              else
                 s_oDebugger:ShowCode( uParam1 )
              endif
           endif

      case ValType( uParam1 ) == "N"   // called from hvm.c hb_vmDebuggerShowLines()
           if s_oDebugger != nil
              if PCount() == 2 // called from hvm.c hb_vmDebuggerLocalName()
                 AAdd( s_oDebugger:aVars, { uParam2, "Local", uParam1 } )
                 if s_oDebugger:oBrwVars != nil
                    s_oDebugger:oBrwVars:RefreshAll()
                 endif
                 return nil
              endif
              if s_oDebugger:lGo
                 s_oDebugger:lGo = ! s_oDebugger:IsBreakPoint( uParam1 )
              endif
              if s_oDebugger:lGo
                 DispBegin()
                 DispBegin()
                 s_oDebugger:SaveAppStatus()
                 s_oDebugger:RestoreAppStatus()
                 DispEnd()
                 DispEnd()
              else
                 s_oDebugger:SaveAppStatus()
                 s_oDebugger:GoToLine( uParam1 )
                 s_oDebugger:HandleEvent()
              endif
           endif

      otherwise   // called from hvm.c hb_vmDebuggerEndProc()
         if s_oDebugger != nil
            s_oDebugger:EndProc()
         endif
   endcase

return nil

CLASS TDebugger

   DATA   aWindows, nCurrentWindow
   DATA   oPullDown
   DATA   oWndCode, oWndCommand, oWndStack, oWndVars
   DATA   oBar, oBrwText, cPrgName, oBrwStack, oBrwVars, aVars
   DATA   cImage
   DATA   lEnd
   DATA   cAppImage, nAppRow, nAppCol, cAppColors, nAppCursor
   DATA   aBreakPoints, aCallStack
   DATA   aLastCommands, nCommand, oGetListCommand
   DATA   lGo
   DATA   cClrDialog

   METHOD New()
   METHOD Activate( cModuleName )
   METHOD BuildCommandWindow()
   METHOD CodeWindowProcessKey( nKey )
   METHOD CommandWindowProcessKey( nKey )
   METHOD EditVar( nVar )
   METHOD EndProc()
   METHOD Exit() INLINE ::lEnd := .t.
   METHOD Go() INLINE ::RestoreAppStatus(), ::lGo := .t., DispEnd(), ::Exit()
   METHOD GoToLine( nLine )
   METHOD HandleEvent()
   METHOD Hide()
   METHOD InputBox( cMsg, uValue, bValid )
   METHOD IsBreakPoint( nLine )
   METHOD LoadVars()
   METHOD NextWindow()
   METHOD Open()
   METHOD PrevWindow()
   METHOD RestoreAppStatus()
   METHOD SaveAppStatus()
   METHOD Show()
   METHOD ShowAppScreen()
   METHOD ShowCallStack()
   METHOD ShowCode( cModuleName )
   METHOD ShowVars()
   METHOD ToggleBreakPoint()
   METHOD ViewSets()

ENDCLASS

METHOD New() CLASS TDebugger

   ::aWindows       = {}
   ::nCurrentWindow = 1
   ::cClrDialog     = "N/W"
   ::oPullDown      = BuildMenu( Self )

   ::oWndCode       = TDbWindow():New( 1, 0, MaxRow() - 6, MaxCol(),, "BG+/B" )
   ::oWndCode:bKeyPressed = { | nKey | ::CodeWindowProcessKey( nKey ) }
   AAdd( ::aWindows, ::oWndCode )

   ::BuildCommandWindow()

   ::lEnd           = .f.
   ::aBreakPoints   = {}
   ::aCallStack     = {}
   ::lGo            = .f.
   ::aVars          = {}

return Self

METHOD Activate( cModuleName ) CLASS TDebugger

   ::Show()
   ::ShowCode( cModuleName )
   ::ShowCallStack()
   ::ShowVars()
   ::RestoreAppStatus()

return nil

METHOD BuildCommandWindow() CLASS TDebugger

   local GetList := {}
   local cCommand

   ::oWndCommand = TDbWindow():New( MaxRow() - 5, 0, MaxRow() - 1, MaxCol(),;
                                    "Command", "BG+/B" )
   ::oWndCommand:bGotFocus   = { || ::oGetListCommand:SetFocus(), SetCursor( SC_NORMAL ) }
   ::oWndCommand:bLostFocus  = { || SetCursor( SC_NONE ) }
   ::oWndCommand:bKeyPressed = { | nKey | ::CommandWindowProcessKey( nKey ) }
   ::oWndCommand:bPainted    = { || DispOutAt( ::oWndCommand:nBottom - 1,;
                             ::oWndCommand:nLeft + 1, "> ", ::oWndCommand:cColor ) }
   AAdd( ::aWindows, ::oWndCommand )

   ::aLastCommands = {}
   ::nCommand = 0

   cCommand = Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 )
   // We don't use the GET command here to avoid the painting of the GET
   AAdd( GetList, TGet():New( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3,;
         { | u | If( PCount() > 0, cCommand := u, cCommand ) }, "cCommand" ) )
   ATail( GetList ):ColorSpec = Replicate( ::oWndCommand:cColor + ",", 5 )
   ::oGetListCommand = TGetList():New( GetList )

return nil

METHOD CodeWindowProcessKey( nKey ) CLASS TDebugger

   do case
      case nKey == K_HOME
           ::oBrwText:GoTop()
           ::oBrwText:ForceStable()

      case nKey == K_END
           ::oBrwText:GoBottom()
           ::oBrwText:ForceStable()

      case nKey == K_UP
           ::oBrwText:Up()
           ::oBrwText:ForceStable()

      case nKey == K_DOWN
           ::oBrwText:Down()
           ::oBrwText:ForceStable()

      case nKey == K_PGUP
           ::oBrwText:PageUp()
           ::oBrwText:ForceStable()

      case nKey == K_PGDN
           ::oBrwText:PageDown()
           ::oBrwText:ForceStable()
   endcase

return nil

METHOD CommandWindowProcessKey( nKey ) CLASS TDebugger

   local cCommand, cResult

   do case
      case nKey == K_UP
           if ::nCommand > 0
              ::oGetListCommand:oGet:VarPut( ::aLastCommands[ ::nCommand ] )
              ::oGetListCommand:oGet:Buffer = ::aLastCommands[ ::nCommand ]
              ::oGetListCommand:oGet:Pos = 1
              ::oGetListCommand:oGet:Display()
              if ::nCommand > 1
                 ::nCommand--
              endif
           endif

      case nKey == K_DOWN
           if ::nCommand <= Len( ::aLastCommands )
              ::oGetListCommand:oGet:VarPut( ::aLastCommands[ ::nCommand ] )
              ::oGetListCommand:oGet:Buffer = ::aLastCommands[ ::nCommand ]
              ::oGetListCommand:oGet:Pos = 1
              ::oGetListCommand:oGet:Display()
              if ::nCommand < Len( ::aLastCommands )
                 ::nCommand++
              endif
           endif

      case nKey == K_ENTER
           cCommand = ::oGetListCommand:oGet:VarGet()
           AAdd( ::aLastCommands, cCommand )
           ::nCommand++
           ::oWndCommand:ScrollUp( 1 )
           if SubStr( LTrim( cCommand ), 1, 2 ) == "? "
              cResult = ValToStr( &( AllTrim( SubStr( LTrim( cCommand ), 3 ) ) ) )
           else
              cResult = "Command error"
           endif
           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1,;
              Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ),;
              ::oWndCommand:cColor )
           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3, cResult,;
              ::oWndCommand:cColor )
           ::oWndCommand:ScrollUp( 1 )
           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, "> ",;
              ::oWndCommand:cColor )
           cCommand = Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 )
           ::oGetListCommand:oGet:VarPut( cCommand )
           ::oGetListCommand:oGet:Buffer = cCommand
           ::oGetListCommand:oGet:Pos = 1
           ::oGetListCommand:oGet:Display()

      otherwise
          ::oGetListCommand:GetApplyKey( nKey )
   endcase

return nil

METHOD EditVar( nVar ) CLASS TDebugger

   local cVarName  := ::aVars[ nVar ][ 1 ]
   local uVarValue := ::aVars[ nVar ][ 2 ]

   uVarValue := ::InputBox( cVarName, ValToStr( uVarValue ) )

   do case
      case uVarValue == "{ ... }"
           // It is an array, don't do anything

      case Upper( SubStr( uVarValue, 1, 5 ) ) == "CLASS"
           // It is an object, don't do anything

      otherwise
         ::aVars[ nVar ][ 2 ] = &uVarValue
         &( ::aVars[ nVar ][ 1 ] ) = ::aVars[ nVar ][ 2 ]
   endcase

   ::oBrwVars:RefreshCurrent()
   ::oBrwVars:ForceStable()

return nil

METHOD EndProc() CLASS TDebugger

   if Len( ::aCallStack ) > 1
      ADel( ::aCallStack, 1 )
      ASize( ::aCallStack, Len( ::aCallStack ) - 1 )
      if ::oBrwStack != nil
         ::oBrwStack:RefreshAll()
      endif
   endif

return nil

METHOD HandleEvent() CLASS TDebugger

   local nPopup, oWnd
   local nKey, nMRow, nMCol, n

   ::lEnd = .f.

   while ! ::lEnd

      nKey = InKey( 0, INKEY_ALL )

      do case
         case ::oPullDown:IsOpen()
              ::oPullDown:ProcessKey( nKey )
              if ::oPullDown:nOpenPopup == 0 // Closed
                 ::aWindows[ ::nCurrentWindow ]:SetFocus( .t. )
              endif

         case nKey == K_LDBLCLK
              Alert( "Mouse Left button doble click" )

         case nKey == K_LBUTTONDOWN
              if MRow() == 0
                 if ( nPopup := ::oPullDown:GetItemOrdByCoors( 0, MCol() ) ) != 0
                    SetCursor( SC_NONE )
                    ::oPullDown:ShowPopup( nPopup )
                 endif

              elseif MRow() == MaxRow()

              else
                 nMRow = MRow()
                 nMCol = MCol()
                 for n = 1 to Len( ::aWindows )
                    if ::aWindows[ n ]:IsOver( nMRow, nMCol ) .and. ;
                       ! ::aWindows[ n ]:lFocused
                       ::aWindows[ ::nCurrentWindow ]:SetFocus( .f. )
                       ::nCurrentWindow = n
                       ::aWindows[ n ]:SetFocus( .t. )
                       n = Len( ::aWindows ) + 1
                    endif
                 next
              endif

         case nKey == K_RBUTTONDOWN

         case nKey == K_ESC
              ::RestoreAppStatus()
              s_oDebugger := nil
              s_lExit := .T.
              DispEnd()
              ::Exit()

         case nKey == K_UP .or. nKey == K_DOWN .or. nKey == K_HOME .or. ;
              nKey == K_END .or. nKey == K_ENTER
              oWnd = ::aWindows[ ::nCurrentWindow ]
              oWnd:KeyPressed( nKey )

         case nKey == K_F4
              ::ShowAppScreen()

         case nKey == K_F5
              ::Go()

         case nKey == K_F8
              ::RestoreAppStatus()
              ::Exit()

         case nKey == K_F9
              ::ToggleBreakPoint()

         case nKey == K_TAB
              ::NextWindow()

         case nKey == K_SH_TAB
              ::PrevWindow()

         case ::oWndCommand:lFocused .and. nKey < 272 // Alt
              ::oWndCommand:KeyPressed( nKey )

         otherwise
              if ( nPopup := ::oPullDown:GetHotKeyPos( AltToKey_debugger( nKey ) ) ) != 0
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
   ::cAppImage = nil
   SetColor( ::cAppColors )
   SetCursor( ::nAppCursor )

return nil

METHOD NextWindow() CLASS TDebugger

   local oWnd

   if Len( ::aWindows ) > 0
      oWnd = ::aWindows[ ::nCurrentWindow++ ]
      oWnd:SetFocus( .f. )
      if ::nCurrentWindow > Len( ::aWindows )
         ::nCurrentWindow = 1
      endif
      oWnd = ::aWindows[ ::nCurrentWindow ]
      oWnd:SetFocus( .t. )
   endif

return nil

METHOD PrevWindow() CLASS TDebugger

   local oWnd

   if Len( ::aWindows ) > 0
      oWnd = ::aWindows[ ::nCurrentWindow-- ]
      oWnd:SetFocus( .f. )
      if ::nCurrentWindow < 1
         ::nCurrentWindow = Len( ::aWindows )
      endif
      oWnd = ::aWindows[ ::nCurrentWindow ]
      oWnd:SetFocus( .t. )
   endif

return nil

METHOD Show() CLASS TDebugger

   ::cAppImage  = SaveScreen()
   ::nAppRow    = Row()
   ::nAppCol    = Col()
   ::cAppColors = SetColor()
   ::nAppCursor = SetCursor( SC_NONE )

   ::oPullDown:Display()
   ::oWndCode:Show( .t. )
   ::oWndCommand:Show()
   DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1, ">" )

   SetColor( "N/BG" )
   @ MaxRow(), 0 CLEAR TO MaxRow(), MaxCol()

   DispOutAt( MaxRow(),  0, "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt F10-Trace", "N/BG" )
   DispOutAt( MaxRow(),  0, "F1", "GR+/BG" )
   DispOutAt( MaxRow(),  8, "F2", "GR+/BG" )
   DispOutAt( MaxRow(), 16, "F3", "GR+/BG" )
   DispOutAt( MaxRow(), 26, "F4", "GR+/BG" )
   DispOutAt( MaxRow(), 34, "F5", "GR+/BG" )
   DispOutAt( MaxRow(), 40, "F6", "GR+/BG" )
   DispOutAt( MaxRow(), 46, "F7", "GR+/BG" )
   DispOutAt( MaxRow(), 54, "F8", "GR+/BG" )
   DispOutAt( MaxRow(), 62, "F9", "GR+/BG" )
   DispOutAt( MaxRow(), 70, "F10", "GR+/BG" )

return nil

METHOD ShowAppScreen() CLASS TDebugger

   ::cImage = SaveScreen()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
   InKey( 0 )
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )

return nil

METHOD ShowCallStack() CLASS TDebugger

   local n := 1

   if ::oWndStack == nil
      ::oWndCode:nRight -= 16
      ::oBrwText:nRight -= 16
      ::oBrwText:aColumns[ 1 ]:Width -= 16
      ::oWndCode:SetFocus( .t. )
      ::oWndStack = TDbWindow():New( 1, MaxCol() - 15, MaxRow() - 6, MaxCol(),;
                                     "Stack", "BG+/B" )
      ::oWndStack:Show( .f. )
      AAdd( ::aWindows, ::oWndStack )
      ::oBrwStack = TBrowseNew( 2, MaxCol() - 14, MaxRow() - 7, MaxCol() - 1 )
      ::oBrwStack:ColorSpec = "BG+/B, N/BG"
      ::oBrwStack:GoTopBlock = { || n := 1 }
      ::oBrwStack:GoBottomBlock = { || n := Len( ::aCallStack ) }
      ::oBrwStack:SkipBlock = { | nSkip, nPos | nPos := n,;
                             n := If( nSkip > 0, Min( Len( ::aCallStack ), n + nSkip ),;
                             Max( 1, n + nSkip ) ), n - nPos }

      ::oBrwStack:AddColumn( TBColumnNew( "",  { || PadC( ::aCallStack[ n ], 14 ) } ) )
      ::oBrwStack:ForceStable()
   endif

return nil

METHOD LoadVars() CLASS TDebugger // updates monitored variables

   local nCount, n, xValue, cName

   ::aVars = {}

   nCount = __mvDbgInfo( HB_MV_PUBLIC )
   for n = nCount to 1 step -1
      xValue = __mvDbgInfo( HB_MV_PUBLIC, n, @cName )
      AAdd( ::aVars, { cName, xValue, "Public" } )
   next
   nCount = __mvDbgInfo( HB_MV_PRIVATE )
   for n = nCount to 1 step -1
      xValue = __mvDbgInfo( HB_MV_PRIVATE, n, @cName )
      AAdd( ::aVars, { cName, xValue, "Private" } )
   next

return nil

METHOD ShowVars() CLASS TDebugger

   local n
   local nWidth

   if ::oWndVars == nil

      n := 1

      ::oWndCode:nTop += 5
      ::oBrwText:nTop += 5
      ::oBrwText:RefreshAll()
      ::oWndCode:SetFocus( .t. )
      ::oWndVars = TDbWindow():New( 1, 0, 5,;
         MaxCol() - If( ::oWndStack != nil, ::oWndStack:nWidth(), 0 ),;
         "Monitor", "BG+/B" )
      ::oWndVars:Show( .f. )
      AAdd( ::aWindows, ::oWndVars )
      ::oWndVars:bKeyPressed = { | nKey | If( nKey == K_DOWN, ( ::oBrwVars:Down(),;
      ::oBrwVars:ForceStable() ), nil ), If( nKey == K_UP, ( ::oBrwVars:Up(),;
      ::oBrwVars:ForceStable() ), nil ), If( nKey == K_ENTER, ::EditVar( n ), nil ) }

      ::oBrwVars = TBrowseNew( 2, 1, 4, MaxCol() - If( ::oWndStack != nil,;
                               ::oWndStack:nWidth(), 0 ) - 1 )
      ::oBrwVars:ColorSpec = "BG+/B, N/BG"
      ::LoadVars()
      ::oBrwVars:GoTopBlock = { || n := 1 }
      ::oBrwVars:GoBottomBlock = { || n := Len( ::aVars ) }
      ::oBrwVars:SkipBlock = { | nSkip, nPos | nPos := n,;
                             n := If( nSkip > 0, Min( Len( ::aVars ), n + nSkip ),;
                             Max( 1, n + nSkip ) ), n - nPos }

      nWidth = ::oWndVars:nWidth() - 1
      ::oBrwVars:AddColumn( TBColumnNew( "",  { || AllTrim( Str( n ) ) + ") " + ;
         PadR( GetVarInfo( ::aVars[ n ] ), ::oWndVars:nWidth() - 5 ) } ) )
      ::oBrwVars:ForceStable()
   endif

return nil

static function GetVarInfo( aVar )

   do case
      case aVar[ 3 ] == "Public" .or. aVar[ 3 ] == "Private"
           return aVar[ 1 ] + " <" + aVar[ 3 ] + ", " + ValType( aVar[ 2 ] ) + ;
                  ">: " + ValToStr( aVar[ 2 ] )

      case aVar[ 3 ] == "Local"
           return aVar[ 1 ] + " <" + aVar[ 2 ] + ", " + ;
                  ValType( __vmVarLGet( 7, aVar[ 3 ] ) ) + ;
                  ">: " + ValToStr( __vmVarLGet( 7, aVar[ 3 ] ) )
   endcase

return ""

static function CompareLine( Self )

return { | a | a[ 1 ] == Self:oBrwText:nLine }

METHOD ShowCode( cModuleName ) CLASS TDebugger

   local cFunction := SubStr( cModuleName, At( ":", cModuleName ) + 1 )
   local cPrgName  := SubStr( cModuleName, 1, At( ":", cModuleName ) - 1 )

   ASize( ::aCallStack, Len( ::aCallStack ) + 1 )
   AIns( ::aCallStack, 1 )
   if Len( ::aCallStack ) == 1
      ::aCallStack[ 1 ] = ProcName( 3 ) // cFunction
   else
      ::aCallStack[ 1 ] = ProcName( 2 ) // cFunction
   endif

   if ::oWndStack != nil
      ::oBrwStack:RefreshAll()
   endif

   if cPrgName != ::cPrgName
      ::cPrgName := cPrgName
      ::oBrwText = TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                   ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, ::cPrgName, "BG+/B, N/BG, W+/R, W+/BG" )

      ::oBrwText:aColumns[ 1 ]:ColorBlock = { || If( AScan( ::aBreakPoints,;
         CompareLine( Self ) ) != 0, { 3, 4 }, { 1, 2 } ) }

      ::oBrwText:ForceStable()
      ::oWndCode:SetCaption( ::cPrgName )
   endif

return nil

METHOD Open() CLASS TDebugger

   local cFileName := ::InputBox( "Please enter the filename", Space( 30 ) )

return nil

METHOD InputBox( cMsg, uValue, bValid ) CLASS TDebugger

   local nTop    := ( MaxRow() / 2 ) - 5
   local nLeft   := ( MaxCol() / 2 ) - 20
   local nBottom := ( MaxRow() / 2 ) - 3
   local nRight  := ( MaxCol() / 2 ) + 20
   local uTemp   := PadR( uValue, nRight - nLeft - 1 )
   local GetList := {}
   local nOldCursor
   local lScoreBoard := Set( _SET_SCOREBOARD, .f. )
   local oWndInput := TDbWindow():New( nTop, nLeft, nBottom, nRight, cMsg,;
                                       ::oPullDown:cClrPopup )

   oWndInput:lShadow = .t.
   oWndInput:Show()

   if bValid == nil
      @ nTop + 1, nLeft + 1 GET uTemp
   else
      @ nTop + 1, nLeft + 1 GET uTemp VALID bValid
   endif

   nOldCursor = SetCursor( SC_NORMAL )
   READ
   SetCursor( nOldCursor )
   oWndInput:Hide()
   Set( _SET_SCOREBOARD, lScoreBoard )

return If( LastKey() != K_ESC, AllTrim( uTemp ), uValue )

METHOD IsBreakPoint( nLine ) CLASS TDebugger

return AScan( ::aBreakPoints, { | aBreak | aBreak[ 1 ] == nLine } ) != 0

METHOD GotoLine( nLine ) CLASS TDebugger

   ::oBrwText:GotoLine( nLine )

   if ::oBrwVars != nil
      ::LoadVars()
      ::oBrwVars:RefreshAll()
      ::oBrwVars:ForceStable()
   endif

   if ::oBrwStack != nil .and. ! ::oBrwStack:Stable
      ::oBrwStack:ForceStable()
   endif

return nil

METHOD RestoreAppStatus() CLASS TDebugger

   ::cImage = SaveScreen()
   DispBegin()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
   SetPos( ::nAppRow, ::nAppCol )
   SetColor( ::cAppColors )
   SetCursor( ::nAppCursor )

return nil

METHOD SaveAppStatus() CLASS TDebugger

   ::cAppImage  = SaveScreen()
   ::nAppRow    = Row()
   ::nAppCol    = Col()
   ::cAppColors = SetColor()
   ::nAppCursor = SetCursor()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )
   SetCursor( SC_NONE )
   DispEnd()

return nil

METHOD ToggleBreakPoint() CLASS TDebugger

   local nAt := AScan( ::aBreakPoints, { | aBreak | aBreak[ 1 ] == ;
                       ::oBrwText:nLine } )

   if nAt == 0
      AAdd( ::aBreakPoints, { ::oBrwText:nLine, ::cPrgName } )
   else
      ADel( ::aBreakPoints, nAt )
      ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
   endif

   ::oBrwText:RefreshCurrent()
   ::oBrwText:ForceStable()

return nil

METHOD ViewSets() CLASS TDebugger

   local oWndSets := TDbWindow():New( 1, 8, MaxRow() - 2, MaxCol() - 8,;
                                      "System Settings[1..47]", ::cClrDialog )
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

   oBrwSets:ColorSpec = "N/W, W+/W, N/BG"
   oBrwSets:GoTopBlock = { || n := 1 }
   oBrwSets:GoBottomBlock = { || n := Len( aSets ) }
   oBrwSets:SkipBlock = { | nSkip, nPos | nPos := n,;
                          n := If( nSkip > 0, Min( Len( aSets ), n + nSkip ),;
                          Max( 1, n + nSkip ) ), n - nPos }
   oBrwSets:AddColumn( TBColumnNew( "", { || PadR( aSets[ n ], 12 ) } ) )
   oBrwSets:AddColumn( oCol := TBColumnNew( "",;
                       { || PadR( ValToStr( Set( n ) ), nWidth - 13 ) } ) )
   oBrwSets:Cargo = 1 // Actual highligthed row
   oCol:ColorBlock = { || { If( n == oBrwSets:Cargo, 3, 1 ), 3 } }

   oWndSets:bPainted    = { || oBrwSets:ForceStable() }
   oWndSets:bKeyPressed = { | nKey | SetsKeyPressed( nKey, oBrwSets, Len( aSets ),;
                            oWndSets ) }

   SetCursor( 0 )
   oWndSets:ShowModal()

return nil

static function SetsKeyPressed( nKey, oBrwSets, nSets, oWnd )

   local nSet := oBrwSets:Cargo

   do case
      case nKey == K_UP
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo--
              SetsUp( oBrwSets )
           endif

      case nKey == K_DOWN
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo++
              SetsDown( oBrwSets )
           endif

      case nKey == K_HOME
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo = 1
              oBrwSets:GoTop()
              oBrwSets:RefreshAll()
              oBrwSets:ForceStable()
           endif

      case nKey == K_END
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo = nSets
              oBrwSets:GoBottom()
              oBrwSets:RefreshAll()
              oBrwSets:ForceStable()
           endif
   endcase

   if nSet != oBrwSets:Cargo
      oWnd:SetCaption( "System Settings[" + AllTrim( Str( oBrwSets:Cargo ) ) + ;
                       "..47]" )
   endif

return nil

static function SetsUp( oBrw )

   local nRow := oBrw:RowPos
   local nSetPos

   if oBrw:RowPos = 1
      nSetPos = oBrw:Cargo
      oBrw:Cargo = 0
      oBrw:RefreshCurrent()
      oBrw:ForceStable()
      oBrw:Cargo = nSetPos
   endif
   oBrw:Up()
   oBrw:RefreshCurrent()

   if nRow != oBrw:Cargo
      oBrw:aReDraw[ nRow ] = .f.
   endif
   oBrw:ForceStable()

return nil

static function SetsDown( oBrw )

   local nRow := oBrw:RowPos
   local nSetPos

   if oBrw:RowPos = oBrw:RowCount
      nSetPos = oBrw:Cargo
      oBrw:Cargo = 0
      oBrw:RefreshCurrent()
      oBrw:ForceStable()
      oBrw:Cargo = nSetPos
   endif
   oBrw:Down()
   oBrw:RefreshCurrent()

   if nRow != oBrw:Cargo
      oBrw:aReDraw[ nRow ] = .f.
   endif
   oBrw:ForceStable()

return nil

static function ValToStr( uVal )

   local cType := ValType( uVal )
   local cResult := "U"

   do case
      case uVal == nil
           cResult = "NIL"

      case cType == "A"
           cResult = "{ ... }"

      case cType == "C"
           cResult = '"' + uVal + '"'

      case cType == "L"
           cResult = If( uVal, ".T.", ".F." )

      case cType == "D"
           cResult = DToC( uVal )

      case cType == "N"
           cResult = AllTrim( Str( uVal ) )

      case cType == "O"
           cResult = "Class " + uVal:ClassName() + " object"
   endcase

return cResult
