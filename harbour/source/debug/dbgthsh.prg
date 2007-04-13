/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Hash Inspector
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
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

 /*
   26/06/2006 - FSG
   Converted dbgtarr.prg to work with hashes.
 */


#include "setcurs.ch"
#include "hbclass.ch"
#include "inkey.ch"
#include "common.ch"

Class TDBGHash

   data   aWindows
   data   TheHash
   data   hashName
   data   nCurWindow
   data   lEditable
   Method new
   method addWindows
   method doget
   method SetsKeyPressed

end class

method new(hHash,pArName,lEditable) Class TDBGHash

   DEFAULT lEditable TO .t.

   ::aWindows   := {}
   ::hashName  := parName
   ::TheHash    := hHash
   ::nCurWindow := 0
   ::lEditable  := lEditable
   ::addWindows( ::TheHash )

Return Self

Method addWindows( hHash, nRow ) Class TDBGHash
   local oBrwSets, nSize := Len( hHash )
   local n := 1
   Local owndsets
   local nWidth, nColWidth
   local oCol, nKeyLen

   if ( nsize < maxrow() - 2 )
      if nRow <> nil
         owndsets := TDbWindow():New( GetTopPos(nRow), 5, getBottomPos(nRow+nsize+1), maxcol()-5, ::hashName+"[1.."+alltrim(str(nsize,6))+"]" ,"N/W" )
      else
         owndsets := TDbWindow():New( 1, 5, 2+nsize, maxcol()-5, ::hashName+"[1.."+alltrim(str(nsize,6))+"]"  ,"N/W")
      endif
   else
      owndsets:=TDbWindow():New( 1, 5, maxrow()-2, maxcol()-5, ::hashName+"[1.."+alltrim(str(nsize,6))+"]"  ,"N/W")
   endif
   ::nCurWindow++
   oWndSets:lFocused:=.t.
   aadd(::aWindows,owndsets)

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1
   oBrwSets:=TbrowseNew(owndsets:nTop+1, owndsets:nLeft+1, owndsets:nBottom-1, owndsets:nRight-1)
   oBrwSets:autolite:=.f.
   oBrwSets:ColorSpec := __Dbg():ClrModal()
   oBrwSets:Cargo :={ 1,{}} // Actual highligthed row
   aadd(oBrwSets:Cargo[2],hHash)

   //oBrwSets:AddColumn( ocol:=     TBColumnNew("", { || ::hashName+"["+alltrim(str(oBrwSets:cargo[ 1 ],6))+"]"} ) )
   oBrwSets:AddColumn( ocol:=     TBColumnNew("", { || ::hashName+"[" + HashKeyString( hHash, oBrwSets:cargo[ 1 ] ) +"]" } ) )

   // calculate max key length
   nKeyLen := 0
   HB_hEval( hHash, {|k,v,p| HB_SYMBOL_UNUSED( k ), HB_SYMBOL_UNUSED( v ), nKeyLen := Max( nKeyLen, len( ::hashName+"["+ HashKeyString( hHash, p ) +"]" ) ) } )
   oCol:width := nKeyLen
   //ocol:width:=len(::arrayName+"["+alltrim(str(len(aarray),6))+"]" )
   oCol:DefColor:={1,2}
   nColWidth = oCol:Width

   //oBrwSets:AddColumn( ocol:=TBColumnNew( "" ,{ || PadR( ValToStr( hHash[ oBrwSets:cargo[ 1 ] ] ), nWidth - nColWidth - 1 ) } ) )
   oBrwSets:AddColumn( ocol:=TBColumnNew( "" ,{ || PadR( ValToStr( HB_HValueAt( hHash, oBrwSets:cargo[ 1 ] ) ), nWidth - nColWidth - 1 ) } ) )

   /* 09/08/2004 - <maurilio.longo@libero.it>
                   Setting a fixed width like it is done in the next line of code wich I've
                   commented exploits a bug of current tbrowse, that is, if every column is
                   narrower than tbrowse but the sum of them is wider tbrowse paints
                   one above the other if code like the one inside RefreshVarsS() is called.
                   (That code is used to have current row fully highlighted and not only
                   current cell). Reproducing this situation on a smaller sample with
                   clipper causes that only column two is visible after first stabilization.

                   I think tbrowse should trim columns up until the point where at leat
                   two are visible in the same moment, I leave this fix to tbrowse for
                   the reader ;)
   oCol:width:=50
   */

   ocol:DefColor:={1,3}

   oBrwSets:GOTOPBLOCK := { || oBrwSets:cargo[ 1 ]:= 1 }
   oBrwSets:GoBottomBlock := { || oBrwSets:cargo[ 1 ]:= Len(oBrwSets:cargo[ 2 ][ 1 ])}
   oBrwSets:SKIPBLOCK := { |nPos| ( nPos:= HashBrowseSkip(nPos, oBrwSets), oBrwSets:cargo[ 1 ]:= ;
                                    oBrwSets:cargo[ 1 ] + nPos,nPos ) }

   ::aWindows[::nCurWindow]:bPainted    := { || (oBrwSets:forcestable(),RefreshVarsS(oBrwSets))}
   ::aWindows[::nCurWindow]:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( hHash ),;
                           ::aWindows[::nCurWindow],::hashName ,Len(hHash),hHash)}

   SetCursor( SC_NONE )
   ::aWindows[::nCurWindow]:ShowModal()

return self

method SetsKeyPressed( nKey, oBrwSets, nSets, oWnd, cName, LenArr, hHash ) Class TDBGHash

   local nSet     := oBrwSets:cargo[1]
   local cOldname := ::hashName
   Local uValue

   HB_SYMBOL_UNUSED( nSets )

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

      Case nKey == K_ENTER
         uValue := HB_HValueAt( hHash, nSet )
         if valtype( uValue ) == "H"
            if Len( uValue ) == 0
               Alert( "Hash is empty" )
            else
               SetPos(ownd:nBottom,ownd:nLeft)
               ::aWindows[::nCurwindow]:lFocused:=.f.

               ::hashName:= ::hashName + "[" + HashKeyString( hHash, nSet ) + "]"
               ::AddWindows( HB_HValueAt( hHash, nSet ), oBrwSets:RowPos+oBrwSets:nTop)
               ::hashName:=coldname

               adel(::aWindows,::nCurWindow)
               asize(::awindows,len(::awindows)-1)
               if ::nCurwindow==0
                  ::nCurwindow:=1
               else
                  ::nCurwindow--
               endif
            endif
         elseif valtype( uValue ) == "B" .or. valtype( uValue ) == "P"
                  Alert("Value cannot be edited")
         else
              if ::lEditable
                 oBrwSets:RefreshCurrent()
                 if ValType( uValue ) == "O"

                    __DbgObject( uValue, cName + ;
                                 "[" + HashKeyString( hHash, nSet ) + "]" )
                 elseif ValType( uValue ) == "A"

                    __DbgArrays( uValue, cName + ;
                                 "[" + HashKeyString( hHash, nSet ) + "]" )
                 else
                    ::doget(oBrwsets, hHash, nSet)
                 endif
                 oBrwSets:RefreshCurrent()
                 oBrwSets:ForceStable()
              else
                 Alert("Value cannot be edited")
              endif

         endif

   endcase

   RefreshVarsS( oBrwSets )

   ::aWindows[::nCurwindow]:SetCaption( cName + "["+AllTrim( Str( oBrwSets:cargo[1] ) ) +".."+ ;
                                        Alltrim(str(LenArr))+ "]")
return self

static function ValToStr( uVal )

   local cType := ValType( uVal )
   local cResult := "U"

   do case
      case uVal == nil
           cResult := "NIL"

      Case cType  =="B"
         cResult:= "{ || ... }"

      case cType == "A"
           cResult := "{ ... }"

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

      case cType == "H"
           cResult := "Hash of " + AllTrim( Str( Len( uVal ) ) ) + " elements"

      case cType == "P"
           cResult := "Pointer"

   endcase

return cResult

METHOD doGet( oBro, pItem, nSet ) Class TDBGHash

    LOCAL nKey
    local getlist := {}
    // save state
    LOCAL lScoreSave := Set( _SET_SCOREBOARD, .f. )
    LOCAL lExitSave  := Set( _SET_EXIT, .t. )
    LOCAL bInsSave   := SetKey( K_INS )
    local cValue     := PadR( ValToStr( HB_HValueAt( pItem, nSet ) ),;
                              oBro:nRight - oBro:nLeft - oBro:GetColumn( 1 ):width )

    // make sure browse is stable
    obro:forcestable()
    // if confirming new record, append blank

    // set insert key to toggle insert mode and cursor
    SetKey( K_INS, { || SetCursor( if( ReadInsert( ! ReadInsert() ),;
            SC_NORMAL, SC_INSERT ) ) } )

    // initial cursor setting
    SetCursor( IF( ReadInsert(), SC_INSERT, SC_NORMAL ) )

    // create a corresponding GET
    @ row(), oBro:nLeft + oBro:GetColumn( 1 ):width + 1 GET cValue ;
       VALID If( Type( cValue ) == "UE", ( Alert( "Expression error" ), .f. ), .t. )

    READ

    if LastKey() == K_ENTER
       HB_HValueAt( pItem, nSet, &cValue )
    endif

    SetCursor( 0 )
    Set( _SET_SCOREBOARD, lScoreSave )
    Set( _SET_EXIT, lExitSave )
    SetKey( K_INS, bInsSave )

    // check exit key from get
    nKey := LastKey()
    IF nKey == K_UP .OR. nKey == K_DOWN .OR. nKey == K_PGUP .OR. nKey == K_PGDN
        KEYBOARD CHR( nKey )
    END

RETURN  nil

function __DbgHashes( hHash, chashName, lEditable )

return TDBGHash():New( hHash, chashName, lEditable )

Static function GetTopPos(nPos)
Local nReturn:=0
nReturn:=if((maxrow()-nPos)<5,Maxrow()-nPos,nPos)
return nReturn

Static function GetBottomPos(nPos)
Local nReturn:=0
nReturn :=if(nPos<maxrow()-2,nPos ,maxrow()-2)
return nReturn

static procedure RefreshVarsS( oBrowse )

   local nLen := oBrowse:ColCount

   if ( nLen == 2 )
      oBrowse:dehilite():colpos:=2
   endif
   oBrowse:dehilite():forcestable()
   if ( nLen == 2 )
      oBrowse:hilite():colpos:=1
   endif
   oBrowse:hilite()
   return

static function HashBrowseSkip( nPos, oBrwSets )

   return iif( oBrwSets:cargo[ 1 ] + nPos < 1, 0 - oBrwSets:cargo[ 1 ] + 1 , ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len(oBrwSets:cargo[ 2 ][ 1 ]), ;
      Len(oBrwSets:cargo[ 2 ][ 1 ]) - oBrwSets:cargo[ 1 ], nPos ) )

static function HashKeyString( hHash, nAt )
  LOCAL cString
  LOCAL xVal  := HB_HKeyAt( hHash, nAt )
  LOCAL cType := ValType( xVal )
  DO CASE
     CASE cType == "C"
          cString := '"' + xVal + '"'
     CASE cType == "D"
          cString := '"' + DToC( xVal ) + '"'
     CASE cType == "N"
          cString := AllTrim( Str( xVal ) )
     OTHERWISE
          cString := AllTrim( cStr( xVal ) )
  ENDCASE

RETURN cString
