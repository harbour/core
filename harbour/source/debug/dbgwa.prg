/*
 * Harbour Project source code:
 * The Debugger Array Inspector
 *
 * Copyright 2001 Ignacio Ortiz de Zuñiga <ignacio@fivetech.com>
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

#include "setcurs.ch"
#include "inkey.ch"

function __dbgShowWorkAreas( oDebugger )

   local oDlg, oCol
   local aAlias, aBrw, aStruc, aInfo
   local cColor
   local n1, n2, n3

   aAlias := {}
   aBrw   := Array(3)
   n1      := 1
   n2      := 1
   n3      := 1

   cColor := If( __Dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N",;
                 "N/W, N/BG, R/W, R/BG" )

   do while !Empty( Alias( n1 ) )
      Aadd(aAlias, Alias( n1 ))
      n1++
   enddo

   if len( aAlias ) == 0
      Alert( "No workareas in use")
      return nil
   endif

   /*
   Window creation
   */

   oDlg := TDbWindow():New( 2, 3, 21, 74, "", cColor )

   oDlg:bKeyPressed := { | nKey | DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, @aStruc, @aInfo ) }
   oDlg:bPainted    := { || DlgWorkAreaPaint( oDlg, aBrw ) }

   /*
   Alias browse
   */

   aBrw[1] := TBrowseNew( oDlg:nTop + 1, oDlg:nLeft + 1, oDlg:nBottom - 1, oDlg:nLeft + 11 )

   aBrw[1]:Cargo         := ( n1 := Select() )
   aBrw[1]:ColorSpec     := oDlg:cColor
   aBrw[1]:GoTopBlock    := { || n1 := 1 }
   aBrw[1]:GoBottomBlock := { || n1 := Len( aAlias ) }
   aBrw[1]:SkipBlock     := { | nSkip, nPos | nPos := n1,;
                                 n1 := iif( nSkip > 0, Min( Len( aAlias ), n1 + nSkip ),;
                                          Max( 1, n1 + nSkip ) ), n1 - nPos }

   aBrw[1]:AddColumn( oCol := TBColumnNew( "", { || PadR( aAlias[ n1 ], 11 ) } ) )

   oCol:ColorBlock := { || iif( aAlias[ n1 ] == Alias(), {3, 4}, {1, 2} ) }

   /*
   Info Browse
   */

   aInfo := ( aAlias[n1] )->(DbfInfo())

   aBrw[2] := TBrowseNew( oDlg:nTop + 7, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 50 )

   aBrw[2]:Cargo         := ( n2 := 1 )
   aBrw[2]:ColorSpec     := oDlg:cColor
   aBrw[2]:GoTopBlock    := { || n2 := 1 }
   aBrw[2]:GoBottomBlock := { || n2 := Len( aInfo ) }
   aBrw[2]:SkipBlock     := { | nSkip, nPos | nPos := n2,;
                                 n2 := iif( nSkip > 0, Min( Len( aInfo ), n2 + nSkip ),;
                                          Max( 1, n2 + nSkip ) ), n2 - nPos }

   aBrw[2]:AddColumn( oCol := TBColumnNew( "", { || Padr(aInfo[ n2 ], 38) } ) )

   oCol:ColorBlock := { || iif( aAlias[ n1 ] == Alias() .and. n2 == 1, {3, 4}, {1, 2} ) }

   /*
   Struc browse
   */

   aStruc := ( aAlias[n1] )->(DbStruct())

   aBrw[3] := TBrowseNew( oDlg:nTop + 1, oDlg:nLeft + 52, oDlg:nBottom - 1, oDlg:nLeft + 70 )

   aBrw[3]:Cargo         := 1
   aBrw[3]:ColorSpec     := oDlg:cColor
   aBrw[3]:GoTopBlock    := { || n3 := 1 }
   aBrw[3]:GoBottomBlock := { || n3 := Len( aStruc ) }
   aBrw[3]:SkipBlock     := { | nSkip, nPos | nPos := n3,;
                                 n3 := iif( nSkip > 0, Min( Len( aStruc ), n3 + nSkip ),;
                                          Max( 1, n3 + nSkip ) ), n3 - nPos }

   aBrw[3]:AddColumn( TBColumnNew( "", { || Padr(aStruc[n3, 1], 11) + ;
                                            aStruc[n3, 2] + ;
                                            Str( aStruc[ n3, 3], 4) + ;
                                            Str( aStruc[n3, 4], 3 ) } ) )

   /*
   Show dialog
   */

   oDlg:ShowModal()

return nil

static function DlgWorkAreaPaint( oDlg, aBrw )

   /*
   Display captions
   */

   DispOutAt( oDlg:nTop, oDlg:nLeft + 5 , " Area ", oDlg:cColor )
   DispOutAt( oDlg:nTop, oDlg:nLeft + 28 , " Status ", oDlg:cColor )
   DispOutAt( oDlg:nTop, oDlg:nLeft + 56 , " Structure ", oDlg:cColor )

   /*
   Display separator lines
   */

   @ oDlg:nTop + 1, oDlg:nLeft + 12 TO ;
     oDlg:nBottom - 1, oDlg:nLeft + 12 ;
     COLOR oDlg:cColor

   DispOutAt( oDlg:nTop , oDlg:nLeft + 12 , Chr( 194 ), oDlg:cColor )
   DispOutAt( oDlg:nBottom , oDlg:nLeft + 12 , Chr( 193 ), oDlg:cColor )

   @ oDlg:nTop + 1, oDlg:nLeft + 51 TO ;
     oDlg:nBottom - 1, oDlg:nLeft + 51 ;
     COLOR oDlg:cColor

   DispOutAt( oDlg:nTop , oDlg:nLeft + 51 , Chr( 194 ), oDlg:cColor )
   DispOutAt( oDlg:nBottom , oDlg:nLeft + 51 , Chr( 193 ), oDlg:cColor )

   @ oDlg:nTop + 6, oDlg:nLeft + 13 TO ;
     oDlg:nTop + 6, oDlg:nLeft + 50 ;
     COLOR oDlg:cColor

   DispOutAt( oDlg:nTop + 6 , oDlg:nLeft + 12 , Chr( 195 ), oDlg:cColor )
   DispOutAt( oDlg:nTop + 6 , oDlg:nLeft + 51 , Chr( 180 ), oDlg:cColor )

   /*
   Display labels
   */

   DispOutAt( oDlg:nTop + 1 , oDlg:nLeft + 13 , "Alias:                Record:         ", oDlg:cColor )
   DispOutAt( oDlg:nTop + 2 , oDlg:nLeft + 13 , "   BOF:         Deleted:              ", oDlg:cColor )
   DispOutAt( oDlg:nTop + 3 , oDlg:nLeft + 13 , "   EOF:           Found:              ", oDlg:cColor )
   DispOutAt( oDlg:nTop + 4 , oDlg:nLeft + 13 , "Filter:                               ", oDlg:cColor )
   DispOutAt( oDlg:nTop + 5 , oDlg:nLeft + 13 , "   Key:                               ", oDlg:cColor )

   /*
   Estabilizar browse
   */

   aBrw[1]:ForceStable()
   aBrw[2]:ForceStable()
   aBrw[3]:ForceStable()
   aBrw[2]:Dehilite()
   aBrw[3]:Dehilite()

   UpdateInfo(oDlg, Alias())

return nil

static function DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, aStruc, aInfo )

   static nFocus := 1

   local nAlias

   if nKey == K_TAB .or. nKey == K_SH_TAB
      aBrw[nFocus]:Dehilite()
      nFocus := nFocus + iif( nKey == K_TAB, 1, -1)
      if nFocus < 1
         nFocus := 3
      endif
      if nFocus > 3
         nFocus := 1
      endif
      aBrw[nFocus]:Hilite()
      return nil
   endif

   do case
   case nFocus == 1
      nAlias := aBrw[1]:Cargo
      WorkAreasKeyPressed( nKey, aBrw[1], oDlg, len( aAlias ) )
      if nAlias != aBrw[1]:Cargo
         aBrw[2]:GoTop()
         aBrw[2]:Invalidate()
         aBrw[2]:ForceStable()
         aInfo := ( aAlias[aBrw[1]:Cargo] )->(DbfInfo( aInfo ))
         aBrw[3]:Configure()
         aBrw[2]:Invalidate()
         aBrw[2]:RefreshAll()
         aBrw[2]:ForceStable()
         aBrw[2]:Dehilite()
         aBrw[3]:GoTop()
         aBrw[3]:Invalidate()
         aBrw[3]:ForceStable()
         aStruc := ( aAlias[aBrw[1]:Cargo] )->(DbStruct())
         aBrw[3]:Configure()
         aBrw[3]:Invalidate()
         aBrw[3]:RefreshAll()
         aBrw[3]:ForceStable()
         aBrw[3]:Dehilite()
         UpdateInfo( oDlg, aAlias[aBrw[1]:Cargo] )
      endif
   case nFocus == 2
      WorkAreasKeyPressed( nKey, aBrw[2], oDlg, len( aInfo ) )
   case nFocus == 3
      WorkAreasKeyPressed( nKey, aBrw[3], oDlg, len( aStruc ) )
   end case

return nil

static procedure WorkAreasKeyPressed( nKey, oBrw, oDlg, nTotal )

   do case
      case nKey == K_UP
           if oBrw:Cargo > 1
              oBrw:Cargo--
              oBrw:RefreshCurrent()
              oBrw:Up()
              oBrw:ForceStable()
           endif

      case nKey == K_DOWN
           if oBrw:Cargo < nTotal
              oBrw:Cargo++
              oBrw:RefreshCurrent()
              oBrw:Down()
              oBrw:ForceStable()
           endif

      case nKey == K_HOME
           if oBrw:Cargo > 1
              oBrw:Cargo := 1
              oBrw:GoTop()
              oBrw:ForceStable()
           endif

      case nKey == K_END
           if oBrw:Cargo < nTotal
              oBrw:Cargo := nTotal
              oBrw:GoBottom()
              oBrw:ForceStable()
           endif

   endcase

return

static function DbfInfo( aInfo )

   local nFor
   local xType, xValue, cValue

   aInfo := {}

   Aadd(aInfo, "["+ltrim( str( Select(Alias()) ) ) + "] " + Alias())
   Aadd(aInfo, Space( 4 ) + "Current Driver")
   Aadd(aInfo, Space( 8 ) + RddName())
   Aadd(aInfo, Space( 4 ) + "Workarea Information")
   Aadd(aInfo, Space( 8 ) + "Select Area: " + ltrim( str( Select() ) ) )
   Aadd(aInfo, Space( 8 ) + "Record Size: " + ltrim( str( Recsize() ) ) )
   Aadd(aInfo, Space( 8 ) + "Header Size: " + ltrim( str( Header() ) ) )
   Aadd(aInfo, Space( 8 ) + "Field Count: " + ltrim( str( Fcount() ) ) )
   Aadd(aInfo, Space( 8 ) + "Last Update: " + Dtoc( lUpdate() ) )
   Aadd(aInfo, Space( 8 ) + "Index order: " + ltrim( str( IndexOrd() ) ) )
   Aadd(aInfo, Space( 4 ) + "Current Record")

   for nFor := 1 to Fcount()

      xValue := Fieldget( nFor )
      xType  := Valtype( xValue )

      do case
      case xType $ "CM"
         cValue := xValue
      case xType == "N"
         cValue := ltrim( str( xValue ) )
      case xType == "D"
         cValue := Dtoc( xValue )
      case xType == "L"
         cValue := iif( xValue, ".T.", ".F." )
      case xType == "A"
         cValue := "Array"
      otherwise
         cValue := "Error"
      end case

      Aadd(aInfo, Space( 8 ) + Padr(FieldName( nFor ), 10) + " = " + Padr( cValue , 17 ) )

   next

return aInfo

static function UpdateInfo( oDlg, cAlias )

   local cOldAlias := Alias()

   SELECT (cAlias)

   DispOutAt( oDlg:nTop + 1 , oDlg:nLeft + 20 , Padr( cAlias, 11 ), oDlg:cColor )
   DispOutAt( oDlg:nTop + 1 , oDlg:nLeft + 42 ,;
              Padr( ltrim( str( Recno() ) ) + "/" + ltrim( str( LastRec() ) ), 9 ),;
              oDlg:cColor )

   DispOutAt( oDlg:nTop + 2 , oDlg:nLeft + 21 , iif( Bof(),"Yes" , "No "), oDlg:cColor )
   DispOutAt( oDlg:nTop + 2 , oDlg:nLeft + 38 , iif( Deleted(),"Yes" , "No "), oDlg:cColor )
   DispOutAt( oDlg:nTop + 3 , oDlg:nLeft + 21 , iif( Bof(),"Yes" , "No "), oDlg:cColor )
   DispOutAt( oDlg:nTop + 3 , oDlg:nLeft + 38 , iif( Found(),"Yes" , "No "), oDlg:cColor )
   DispOutAt( oDlg:nTop + 4 , oDlg:nLeft + 21 , Padr( DbFilter(), 29 ), oDlg:cColor )
   DispOutAt( oDlg:nTop + 4 , oDlg:nLeft + 21 , Padr( OrdKey(), 29 ), oDlg:cColor )

   SELECT (cOldAlias)

return nil