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
#pragma -es0
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

memvar __DbgStatics

procedure AltD( nAction )
   static s_lEnabled := .t.

   do case
      case nAction == nil
           if s_lEnabled
              s_lExit := .f.
              __dbgEntry( ProcLine( 2 ) )
           endif

      case nAction == ALTD_DISABLE
           s_lEnabled := .f.

      case nAction == ALTD_ENABLE
           s_lEnabled := .t.
   endcase

return

procedure __dbgEntry( uParam1, uParam2, uParam3 )  // debugger entry point

   local cModuleName
   local nStaticsBase, nStaticIndex, cStaticName
   local cLocalName, nLocalIndex
   local nAt

   do case
      case ValType( uParam1 ) == "C"   // called from hvm.c hb_vmModuleName()
           cModuleName = uParam1
           if ! s_lExit
              if s_oDebugger == nil
                 s_oDebugger := TDebugger():New()
                 s_oDebugger:Activate( cModuleName )
              else
                 s_oDebugger:ShowCode( cModuleName )
                 s_oDebugger:LoadVars()
              endif
           endif

      case ValType( uParam1 ) == "N"   // called from hvm.c both hb_vmDebuggerShowLines()
           public __DbgStatics         // hb_vmStaticName() and hb_vmLocalName()
           if Type( "__DbgStatics" ) == "L"
              __DbgStatics := {}
           endif

           if ProcName( 1 ) == "(_INITSTATICS)"
              nStaticsBase = uParam1
              cStaticName  = uParam2
              if AScan( __DbgStatics, { | a | a[ 1 ] == nStaticsBase } ) == 0
                 AAdd( __DbgStatics, { nStaticsBase, { cStaticName } } )
              else
                 AAdd( ATail( __DbgStatics )[ 2 ], cStaticName )
              endif
              return  // We can not use s_oDebugger yet, so we return
           endif

           if s_oDebugger != nil
              if PCount() == 3 // called from hvm.c hb_vmLocalName() and hb_vmStaticName()
                 if uParam3 == 1 // in-function static variable
                    cStaticName  = uParam2
                    nStaticIndex = uParam1
                    if ( nAt := AScan( s_oDebugger:aVars,; // Is there another var with this name ?
                         { | aVar | aVar[ 1 ] == cStaticName } ) ) != 0
                       s_oDebugger:aVars[ nAt ] = { cStaticName, nStaticIndex, "Static" }
                    else
                       AAdd( s_oDebugger:aVars, { cStaticName, nStaticIndex, "Static" } )
                    endif
                 else            // local variable
                    cLocalName  = uParam2
                    nLocalIndex = uParam1
                    if ( nAt := AScan( s_oDebugger:aVars,; // Is there another var with this name ?
                         { | aVar | aVar[ 1 ] == cLocalName } ) ) != 0
                       s_oDebugger:aVars[ nAt ] = { cLocalName, nLocalIndex, "Local", ProcName( 1 ) }
                    else
                       AAdd( s_oDebugger:aVars, { cLocalName, nLocalIndex, "Local", ProcName( 1 ) } )
                    endif
                 endif
                 if s_oDebugger:oBrwVars != nil
                    s_oDebugger:oBrwVars:RefreshAll()
                 endif
                 return
              endif
              if s_oDebugger:lGo
                 s_oDebugger:lGo := ! s_oDebugger:IsBreakPoint( uParam1 )
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
         if Empty( ProcName( 1 ) ) // ending (_INITSTATICS)
            return
         endif
         if s_oDebugger != nil
            s_oDebugger:EndProc()
            s_oDebugger:LoadVars()
         endif
   endcase

return

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
   DATA   aColors
   DATA   lCaseSensitive
   DATA   cSearchString
   DATA   cPathForFiles
   DATA   nTabWidth
   DATA   lMonoDisplay

   METHOD New()
   METHOD Activate( cModuleName )
   METHOD BarDisplay()
   METHOD BuildCommandWindow()

   METHOD ClrModal() INLINE If( ::lMonoDisplay, "N/W, W+/W, W/N, W+/N",;
                                "N/W, R/W, N/BG, R/BG" )

   METHOD CodeWindowProcessKey( nKey )
   METHOD Colors()
   METHOD CommandWindowProcessKey( nKey )
   METHOD EditColor( nColor, oBrwColors )
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
   METHOD MonoDisplay()
   METHOD NextWindow()
   METHOD Open()
   METHOD OSShell()

   METHOD PathForFiles() INLINE ;
          ::cPathForFiles := ::InputBox( "Search path for source files:",;
                                         ::cPathForFiles )

   METHOD PrevWindow()
   METHOD RestoreAppStatus()
   METHOD SaveAppStatus()
   METHOD Show()
   METHOD ShowAppScreen()
   METHOD ShowCallStack()
   METHOD ShowCode( cModuleName )
   METHOD ShowVars()

   METHOD Step() INLINE ::RestoreAppStatus(), ::Exit()

   METHOD TabWidth() INLINE ;
          ::nTabWidth := ::InputBox( "Tab width", ::nTabWidth )

   METHOD ToggleBreakPoint()
   METHOD ViewSets()
   METHOD WndVarsLButtonDown( nMRow, nMCol )
   METHOD LineNumbers()          // Toggles numbering of source code lines
   METHOD Locate()
   METHOD FindNext()
   METHOD FindPrevious()
   METHOD SearchLine()
   METHOD ToggleCaseSensitive() INLINE ::lCaseSensitive := !::lCaseSensitive
   METHOD ShowWorkAreas() INLINE __dbgShowWorkAreas( Self )

ENDCLASS


METHOD New() CLASS TDebugger

   ::aColors := { "BG+/B", "BG+/B", "N/BG", "N/BG", "N/W", "N/BG", "GR+/B",;
                  "N/BG", "GR+/BG", "W+/N", "GR+/N" }
   ::lMonoDisplay   := .f.
   s_oDebugger := Self

   ::aWindows       := {}
   ::nCurrentWindow := 1
   ::oPullDown      := __dbgBuildMenu( Self )

   ::oWndCode       := TDbWindow():New( 1, 0, MaxRow() - 6, MaxCol() )
   ::oWndCode:bKeyPressed := { | nKey | ::CodeWindowProcessKey( nKey ) }
   ::oWndCode:bGotFocus   := { || ::oGetListCommand:SetFocus(), SetCursor( SC_SPECIAL1 ) }
   ::oWndCode:bLostFocus  := { || SetCursor( SC_NONE ) }
   ::oWndCode:bPainted = { || ::oBrwText:SetColor( __DbgColors()[ 2 ] + "," + ;
                              __DbgColors()[ 3 ] + "," + __DbgColors()[ 4 ] ),;
                              If( ::oBrwText != nil, ::oBrwText:RefreshWindow(), nil ) }

   AAdd( ::aWindows, ::oWndCode )

   ::BuildCommandWindow()

   ::lEnd           := .f.
   ::aBreakPoints   := {}
   ::aCallStack     := {}
   ::lGo            := .f.
   ::aVars          := {}
   ::lCaseSensitive := .f.
   ::cSearchString  := ""
   ::cPathForFiles  := ""
   ::nTabWidth      := 4

return Self

METHOD Activate( cModuleName ) CLASS TDebugger

   ::Show()
   ::ShowCode( cModuleName )
   ::ShowCallStack()
   ::ShowVars()
   ::RestoreAppStatus()

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
                        If( ::lMonoDisplay, 15, HB_ColorToN( __DbgColors()[ 2 ] ) ) ) }
   AAdd( ::aWindows, ::oWndCommand )

   ::aLastCommands := {}
   ::nCommand := 0

   cCommand := Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 3 )
   // We don't use the GET command here to avoid the painting of the GET
   AAdd( GetList, oGet := Get():New( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3,;
         { | u | iif( PCount() > 0, cCommand := u, cCommand ) }, "cCommand" ) )
   oGet:ColorSpec := Replicate( __DbgColors()[ 2 ] + ",", 5 )
   ::oGetListCommand := TGetList():New( GetList )

return nil

METHOD CodeWindowProcessKey( nKey ) CLASS TDebugger

   do case
      case nKey == K_HOME
           ::oBrwText:GoTop()

      case nKey == K_END
           ::oBrwText:GoBottom()

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

   oBrwColors:ColorSpec := ::ClrModal()
   oBrwColors:GoTopBlock := { || n := 1 }
   oBrwColors:GoBottomBlock := { || n := Len( aColors ) }
   oBrwColors:SkipBlock := { | nSkip, nPos | nPos := n,;
                          n := iif( nSkip > 0, Min( Len( aColors ), n + nSkip ),;
                          Max( 1, n + nSkip ) ), n - nPos }
   oBrwColors:AddColumn( ocol := TBColumnNew( "", { || PadR( aColors[ n ], 14 ) } ) )
   oCol:colorblock :=   { || { iif( n == oBrwColors:Cargo, 2, 1 ), 2 } }
   oBrwColors:AddColumn( oCol := TBColumnNew( "",;
                       { || PadR( '"' + ::aColors[ n ] + '"', nWidth - 15 ) } ) )
   oBrwColors:Cargo := 1 // Actual highligthed row
   oBrwColors:colPos:=2
   oBrwColors:Freeze:=1
   oCol:ColorBlock := { || { iif( n == oBrwColors:Cargo, 3, 1 ), 3 } }


   oWndColors:bPainted    := { || oBrwColors:ForceStable() }

   oWndColors:bKeyPressed := { | nKey | SetsKeyPressed( nKey, oBrwColors,;
                               Len( aColors ), oWndColors, "Debugger Colors",;
                               { || ::EditColor( n, oBrwColors ) } ) }
   oWndColors:ShowModal()

   ::oPullDown:LoadColors()
   ::oPullDown:Refresh()
   ::BarDisplay()

   for n = 1 to Len( ::aWindows )
      ::aWindows[ n ]:LoadColors()
      ::aWindows[ n ]:Refresh()
   next

return nil

METHOD CommandWindowProcessKey( nKey ) CLASS TDebugger

   local cCommand, cResult, oE
   local bLastHandler

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
           AAdd( ::aLastCommands, cCommand )
           ::nCommand++
           ::oWndCommand:ScrollUp( 1 )

           bLastHandler := ErrorBlock({ |objErr| BREAK (objErr) })

           if SubStr( LTrim( cCommand ), 1, 2 ) == "? "
              begin sequence
                  cResult := ValToStr( &( AllTrim( SubStr( LTrim( cCommand ), 3 ) ) ) )

              recover using oE
                  cResult := "Command error: " + oE:description

              end sequence

           else
              cResult := "Command error"

           endif

           ErrorBlock(bLastHandler)

           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 1,;
              Space( ::oWndCommand:nRight - ::oWndCommand:nLeft - 1 ),;
              __DbgColors()[ 2 ] )
           DispOutAt( ::oWndCommand:nBottom - 1, ::oWndCommand:nLeft + 3, cResult,;
              __DbgColors()[ 2 ] )
           ::oWndCommand:ScrollUp( 1 )
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
                             oBrwColors:aColumns[ 2 ]:Width )

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

   SetCursor( SC_NORMAL )
   @ Row(), Col() GET cColor COLOR SubStr( ::ClrModal(), 5 ) ;
      VALID If( ValType( &cColor ) != "C", ( Alert( "Must be string" ), .f. ), .t. )

   READ
   SetCursor( SC_NONE )

   Set( _SET_SCOREBOARD, lPrevScore )
   Set( _SET_EXIT, lPrevExit )

   if LastKey() != K_ESC
      ::aColors[ nColor ] = &cColor
   endif

   oBrwColors:RefreshCurrent()
   oBrwColors:ForceStable()

return nil

METHOD EditVar( nVar ) CLASS TDebugger

   local cVarName   := ::aVars[ nVar ][ 1 ]
   local uVarValue  := ::aVars[ nVar ][ 2 ]
   local cVarType   := ::aVars[ nVar ][ 3 ]
   local nProcLevel := 1
   local aArray

   if ::aVars[ nVar ][ 3 ] == "Local"
      while ProcName( nProcLevel ) != ::aVars[ nVar ][ 4 ]
         nProcLevel++
      end
      uVarValue = __vmVarLGet( nProcLevel, ::aVars[ nVar ][ 2 ] )
   endif

   if ::aVars[ nVar ][ 3 ] == "Static"
      uVarValue = __vmVarSGet( ::aVars[ nVar ][ 2 ] )
   endif

   uVarValue := ::InputBox( cVarName, ValToStr( uVarValue ) )

   if LastKey() != K_ESC
      do case
         case uVarValue == "{ ... }"
               cVarType = ::aVars[ nVar ][ 3 ]

               do case
                  case cVarType == "Local"
                     aArray = __vmVarLGet( nProcLevel, ::aVars[ nVar ][ 2 ] )

                  case cVarType == "Static"
                     aArray = __vmVarSGet( ::aVars[ nVar ][ 2 ] )

                  otherwise
                     aArray = ::aVars[ nVar ][ 2 ]
               endcase

               if Len( aArray ) > 0
                  __DbgArrays( aArray, cVarName )
               else
                  Alert( "Array is empty" )
               endif

         case Upper( SubStr( uVarValue, 1, 5 ) ) == "CLASS"
              do case
                 case cVarType == "Local"
                    __DbgObject( __vmVarLGet( nProcLevel, ::aVars[ nVar ][ 2 ] ), cVarName )

                 case cVarType == "Static"
                    __DbgObject( __vmVarSGet( ::aVars[ nVar ][ 2 ] ), cVarName )

                 otherwise
                    __DbgObject( ::aVars[ nVar ][ 2 ], cVarName )
              endcase

         otherwise
            do case
               case cVarType == "Local"
                  __vmVarLSet( nProcLevel, ::aVars[ nVar ][ 2 ], &uVarValue )

               case cVarType == "Static"
                  __vmVarSSet( ::aVars[ nVar ][ 2 ], &uVarValue )

               otherwise
                  ::aVars[ nVar ][ 2 ] := &uVarValue
                  &( ::aVars[ nVar ][ 1 ] ) := ::aVars[ nVar ][ 2 ]
            endcase
      endcase
   endif

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

   ::lEnd := .f.

   while ! ::lEnd

      nKey := InKey( 0, INKEY_ALL )

      do case
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
                       n := Len( ::aWindows ) + 1
                    endif
                 next
              endif

         case nKey == K_LBUTTONDOWN
              if MRow() == 0
                 if ( nPopup := ::oPullDown:GetItemOrdByCoors( 0, MCol() ) ) != 0
                    SetCursor( SC_NONE )
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
                       n := Len( ::aWindows ) + 1
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
              oWnd := ::aWindows[ ::nCurrentWindow ]
              oWnd:KeyPressed( nKey )

         case nKey == K_F4
              ::ShowAppScreen()

         case nKey == K_F5
              ::Go()


         case nKey == K_F8
              ::Step()

         case nKey == K_F9
              ::ToggleBreakPoint()

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

   for n = 1 to Len( ::aWindows )
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
   InKey( 0 )
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )

return nil

METHOD ShowCallStack() CLASS TDebugger

   local n := 1

   if ::oWndStack == nil
      ::oWndCode:nRight -= 16
      ::oBrwText:Resize(,,, ::oBrwText:nRight - 16)
      ::oWndCode:SetFocus( .t. )
      ::oWndStack := TDbWindow():New( 1, MaxCol() - 15, MaxRow() - 6, MaxCol(),;
                                     "Calls" )
      AAdd( ::aWindows, ::oWndStack )
      ::oBrwStack := TBrowseNew( 2, MaxCol() - 14, MaxRow() - 7, MaxCol() - 1 )
      ::oBrwStack:ColorSpec := ::aColors[ 3 ] + "," + ::aColors[ 4 ] + "," + ::aColors[ 5 ]
      ::oBrwStack:GoTopBlock := { || n := 1 }
      ::oBrwStack:GoBottomBlock := { || n := Len( ::aCallStack ) }
      ::oBrwStack:SkipBlock := { | nSkip, nPos | nPos := n,;
                             n := iif( nSkip > 0, Min( Len( ::aCallStack ), n + nSkip ),;
                             Max( 1, n + nSkip ) ), n - nPos }

      ::oBrwStack:AddColumn( TBColumnNew( "",  { || PadC( ::aCallStack[ n ], 14 ) } ) )
      ::oBrwStack:ForceStable()
      ::oWndStack:bPainted = { || ::oBrwStack:ColorSpec := __DbgColors()[ 2 ] + "," + ;
                                 __DbgColors()[ 3 ] + "," + __DbgColors()[ 4 ],;
                                 ::oBrwStack:RefreshAll(), ::oBrwStack:ForceStable() }
      ::oWndStack:Show( .f. )
   endif

return nil

METHOD LoadVars() CLASS TDebugger // updates monitored variables

   local nCount, n, m, xValue, cName
   local cStaticName, nStaticIndex, nStaticsBase

   ::aVars := {}

   nCount := __mvDbgInfo( HB_MV_PUBLIC )
   for n := nCount to 1 step -1
      xValue := __mvDbgInfo( HB_MV_PUBLIC, n, @cName )
      if cName != "__DBGSTATICS"  // reserved public used by the debugger
         AAdd( ::aVars, { cName, xValue, "Public" } )
      endif
   next

   nCount := __mvDbgInfo( HB_MV_PRIVATE )
   for n := nCount to 1 step -1
      xValue := __mvDbgInfo( HB_MV_PRIVATE, n, @cName )
      AAdd( ::aVars, { cName, xValue, "Private" } )
   next

   if Type( "__DbgStatics" ) == "A"
      for n = 1 to Len( __DbgStatics )
         for m = 1 to Len( __DbgStatics[ n ][ 2 ] )
            cStaticName  = __DbgStatics[ n ][ 2 ][ m ]
            nStaticIndex = __DbgStatics[ n ][ 1 ] + m
            AAdd( ::aVars, { cStaticName, nStaticIndex, "Static" } )
         next
      next
   endif

return nil

METHOD ShowVars( bSort, nType ) CLASS TDebugger

   local n
   local nWidth

   if ::oWndVars == nil

      n := 1

      ::oWndCode:nTop += 7
      ::oBrwText:Resize( ::oBrwText:nTop + 7 )
      ::oBrwText:RefreshAll()
      ::oWndCode:SetFocus( .t. )
      ::oWndVars := TDbWindow():New( 1, 0, 7,;
         MaxCol() - iif( ::oWndStack != nil, ::oWndStack:nWidth(), 0 ),;
         "Monitor" )
      ::oWndVars:Show( .f. )
      AAdd( ::aWindows, ::oWndVars )
      ::oWndVars:bKeyPressed := { | nKey | iif( nKey == K_DOWN .and. ;
      n < Len( ::aVars ), ( ::oBrwVars:Down(), ::oBrwVars:ForceStable() ), nil ),;
      iif( nKey == K_UP .and. n > 1, ( ::oBrwVars:Up(), ::oBrwVars:ForceStable() ),;
      nil ), iif( nKey == K_ENTER, ::EditVar( n ), nil ) }

      ::oWndVars:bLButtonDown = { | nMRow, nMCol | ::WndVarsLButtonDown( nMRow, nMCol ) }
      ::oWndVars:bLDblClick = { | nMRow, nMCol | ::EditVar( n ) }
      ::oWndVars:bPainted = { || ::oBrwVars:ColorSpec := __DbgColors()[ 2 ] + "," + ;
                                 __DbgColors()[ 3 ] + "," + __DbgColors()[ 4 ],;
                                 ::oBrwVars:RefreshAll(), ::oBrwVars:ForceStable() }

      ::oBrwVars := TBrowseNew( 2, 1, 6, MaxCol() - iif( ::oWndStack != nil,;
                               ::oWndStack:nWidth(), 0 ) - 1 )
      ::oBrwVars:ColorSpec := ::aColors[ 2 ] + "," + ::aColors[ 3 ] + "," + ::aColors[ 4 ]
      ::LoadVars()
      ::oBrwVars:GoTopBlock := { || n := 1 }
      ::oBrwVars:GoBottomBlock := { || n := Len( ::aVars ) }
      ::oBrwVars:SkipBlock := { | nSkip, nPos | nPos := n,;
                             n := iif( nSkip > 0, Min( Len( ::aVars ), n + nSkip ),;
                             Max( 1, n + nSkip ) ), n - nPos }

      nWidth := ::oWndVars:nWidth() - 1
      ::oBrwVars:AddColumn( TBColumnNew( "",  { || AllTrim( Str( n - 1 ) ) + ") " + ;
         PadR( GetVarInfo( ::aVars[ n ] ), ::oWndVars:nWidth() - 5 ) } ) )
      ::oBrwVars:ForceStable()
   endif

return nil

static function GetVarInfo( aVar )

   local nProcLevel := 1

   do case
      case aVar[ 3 ] == "Local"
           while ProcName( nProcLevel ) != aVar[ 4 ]
              nProcLevel++
           end
           return aVar[ 1 ] + " <Local, " + ;
                  ValType( __vmVarLGet( nProcLevel, aVar[ 2 ] ) ) + ;
                  ">: " + ValToStr( __vmVarLGet( nProcLevel, aVar[ 2 ] ) )

      case aVar[ 3 ] == "Public" .or. aVar[ 3 ] == "Private"
           return aVar[ 1 ] + " <" + aVar[ 3 ] + ", " + ValType( aVar[ 2 ] ) + ;
                  ">: " + ValToStr( aVar[ 2 ] )

      case aVar[ 3 ] == "Static"
           return aVar[ 1 ] + " <Static, " + ;
                  ValType( __vmVarSGet( aVar[ 2 ] ) ) + ;
                  ">: " + ValToStr( __vmVarSGet( aVar[ 2 ] ) )
   endcase

return ""

static function CompareLine( Self )

return { | a | a[ 1 ] == Self:oBrwText:nRow }  // it was nLine

METHOD ShowCode( cModuleName ) CLASS TDebugger

   local cFunction := SubStr( cModuleName, At( ":", cModuleName ) + 1 )
   local cPrgName  := SubStr( cModuleName, 1, At( ":", cModuleName ) - 1 )

   ASize( ::aCallStack, Len( ::aCallStack ) + 1 )
   AIns( ::aCallStack, 1 )
   if Len( ::aCallStack ) == 1
      ::aCallStack[ 1 ] := ProcName( 3 ) // cFunction
   else
      ::aCallStack[ 1 ] := ProcName( 2 ) // cFunction
   endif

   if ::oWndStack != nil
      ::oBrwStack:RefreshAll()
   endif

   if cPrgName != ::cPrgName
      ::cPrgName := cPrgName
      ::oBrwText := TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                   ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, ::cPrgName, "BG+/B, N/BG, W+/R, W+/BG" )

      ::oWndCode:SetCaption( ::cPrgName )
   endif

return nil

METHOD Open() CLASS TDebugger

   local cFileName := ::InputBox( "Please enter the filename", Space( 30 ) )

return nil

METHOD OSShell() CLASS TDebugger

   local cImage := SaveScreen()
   local cColors := SetColor()
   local cOs    := Upper( OS() )
   local cShell := GetEnv("COMSPEC")
   local bLastHandler := ErrorBlock({ |objErr| BREAK (objErr) })
   local oE

   SET COLOR TO "W/N"
   CLS
   SetCursor( SC_NORMAL )

   begin sequence
      if At("WINDOWS", cOs) != 0 .OR. At("DOS", cOs) != 0 .OR. At("OS/2", cOs) != 0
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

   oWndInput:lShadow := .t.
   oWndInput:Show()

   if bValid == nil
      @ nTop + 1, nLeft + 1 GET uTemp COLOR "," + __DbgColors()[ 5 ]
   else
      @ nTop + 1, nLeft + 1 GET uTemp VALID bValid COLOR "," + __DbgColors()[ 5 ]
   endif

   nOldCursor := SetCursor( SC_NORMAL )
   READ
   SetCursor( nOldCursor )
   oWndInput:Hide()
   Set( _SET_SCOREBOARD, lScoreBoard )

return iif( LastKey() != K_ESC, AllTrim( uTemp ), uValue )


METHOD IsBreakPoint( nLine ) CLASS TDebugger

return AScan( ::aBreakPoints, { | aBreak | aBreak[ 1 ] == nLine } ) != 0


METHOD GotoLine( nLine ) CLASS TDebugger

   ::oBrwText:GotoLine( nLine )

   if ::oBrwVars != nil
      // ::LoadVars()
      ::oBrwVars:RefreshAll()
      ::oBrwVars:ForceStable()
   endif

   if ::oBrwStack != nil .and. ! ::oBrwStack:Stable
      ::oBrwStack:ForceStable()
   endif

return nil

METHOD RestoreAppStatus() CLASS TDebugger

   ::cImage := SaveScreen()
   DispBegin()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
   SetPos( ::nAppRow, ::nAppCol )
   SetColor( ::cAppColors )
   SetCursor( ::nAppCursor )

return nil

METHOD SaveAppStatus() CLASS TDebugger

   ::cAppImage  := SaveScreen()
   ::nAppRow    := Row()
   ::nAppCol    := Col()
   ::cAppColors := SetColor()
   ::nAppCursor := SetCursor()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )
   SetCursor( SC_NONE )
   DispEnd()

return nil

METHOD ToggleBreakPoint() CLASS TDebugger

   local nAt := AScan( ::aBreakPoints, { | aBreak | aBreak[ 1 ] == ;
                       ::oBrwText:nRow } ) // it was nLine

   if nAt == 0
      AAdd( ::aBreakPoints, { ::oBrwText:nRow, ::cPrgName } )     // it was nLine
      ::oBrwText:ToggleBreakPoint(::oBrwText:nRow, .T.)
   else
      ADel( ::aBreakPoints, nAt )
      ASize( ::aBreakPoints, Len( ::aBreakPoints ) - 1 )
      ::oBrwText:ToggleBreakPoint(::oBrwText:nRow, .F.)
   endif

   ::oBrwText:RefreshCurrent()

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

   oBrwSets:ColorSpec := ::ClrModal()
   oBrwSets:GoTopBlock := { || n := 1 }
   oBrwSets:GoBottomBlock := { || n := Len( aSets ) }
   oBrwSets:SkipBlock := { | nSkip, nPos | nPos := n,;
                          n := iif( nSkip > 0, Min( Len( aSets ), n + nSkip ),;
                          Max( 1, n + nSkip ) ), n - nPos }
   oBrwSets:AddColumn( ocol := TBColumnNew( "", { || PadR( aSets[ n ], 12 ) } ) )
   ocol:colorblock :=   { || { iif( n == oBrwSets:Cargo, 2, 1 ), 2 } }
   oBrwSets:AddColumn( oCol := TBColumnNew( "",;
                       { || PadR( ValToStr( Set( n ) ), nWidth - 13 ) } ) )
   oBrwSets:Cargo := 1 // Actual highligthed row
   oBrwSets:colPos:=2
   oBrwSets:Freeze:=1
   ocol:ColorBlock := { || { iif( n == oBrwSets:Cargo, 3, 1 ), 3 } }


   oWndSets:bPainted    := { || oBrwSets:ForceStable() }
   oWndSets:bKeyPressed := { | nKey | SetsKeyPressed( nKey, oBrwSets, Len( aSets ),;
                            oWndSets, "System Settings" ) }

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

   local nSet := oBrwSets:Cargo

   do case
      case nKey == K_UP
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo--
              oBrwSets:RefreshCurrent()
              oBrwSets:Up()
              oBrwSets:ForceStable()
           endif

      case nKey == K_DOWN
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo++
              oBrwSets:RefreshCurrent()
              oBrwSets:Down()
              oBrwSets:ForceStable()
           endif

      case nKey == K_HOME
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo := 1
              oBrwSets:GoTop()
              oBrwSets:ForceStable()
           endif

      case nKey == K_END
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo := nSets
              oBrwSets:GoBottom()
              oBrwSets:ForceStable()
           endif

      case nKey == K_ENTER
           if bEdit != nil
              Eval( bEdit )
           endif

   endcase

   if nSet != oBrwSets:Cargo
      oWnd:SetCaption( cCaption + "[" + AllTrim( Str( oBrwSets:Cargo ) ) + ;
                       ".." + AllTrim( Str( nSets ) ) + "]" )
   endif

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

METHOD SearchLine() CLASS TDebugger

   local cLine

   cLine := ::InputBox( "Line number", "1" )

   if Val( cLine ) > 0
      ::oBrwText:GotoLine ( Val( cLine ) )
   endif

return nil

METHOD CaseSensitive() CLASS TDebugger

   ::lCaseSensitive := !::lCaseSensitive

return nil


function __DbgColors()

return If( ! s_oDebugger:lMonoDisplay, s_oDebugger:aColors,;
           { "W+/N", "W+/N", "W+/N", "N/W", "W/N", "N/W", "W+/N",;
             "N/W", "W+/W", "W/N", "W+/N" } )

function __Dbg()

return s_oDebugger