/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Array Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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
#include "hbclass.ch"
#include "inkey.ch"
#include "common.ch"
Class TDBGArray

data aWindows
data TheArray
data arrayname
data nCurWindow
Method new
method addWindows
method doget
method SetsKeyPressed
end class

method new(aArray,pArName) Class TDBGArray
::aWindows:={}
::arrayName:=parName
::TheArray:=aArray
::nCurWindow:=0
::addWindows(::TheArray)
Return Self

Method addWindows(aArray,nRow) Class TDBGArray
local oBrwSets,nSize:=Len(AArray)
local n:=1
Local owndsets
   local nWidth
   local oCol
   if (nsize<maxrow()-2)
      if nRow <> nil
         owndsets:=TDbWindow():New( GetTopPos(nRow), 5, getBottomPos(nRow+nsize+1), maxcol()-5, ::arrayName+"[1.."+alltrim(str(nsize,6))+"]" ,"N/W" )
      else
         owndsets:=TDbWindow():New( 1, 5, 2+nsize, maxcol()-5, ::arrayName+"[1.."+alltrim(str(nsize,6))+"]"  ,"N/W")
      endif
   else
      owndsets:=TDbWindow():New( 1, 5, maxrow()-2, maxcol()-5, ::arrayName+"[1.."+alltrim(str(nsize,6))+"]"  ,"N/W")
   endif
   ::nCurWindow++
   oWndSets:lFocused:=.t.
   aadd(::aWindows,owndsets)

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1

   oBrwSets:=TbrowseNew(owndsets:nTop+1, owndsets:nLeft+1, owndsets:nBottom-1, owndsets:nRight-1)
   oBrwSets:autolite:=.f.
   oBrwSets:ColorSpec := __Dbg():ClrModal()
   oBrwSets:Cargo :={ 1,{}} // Actual highligthed row
   oBrwSets:AddColumn( ocol:=     TBColumnNew("", { || ::arrayName+"["+alltrim(str(oBrwSets:cargo[ 1 ],6))+"]"} ) )
   ocol:width:=len(::arrayName+"["+alltrim(str(len(aarray),6))+"]" )
   oCol:DefColor:={1,2}
   oBrwSets:AddColumn( ocol:=TBColumnNew( "" ,{ || PadR( ValToStr( aArray[oBrwSets:cargo[ 1 ] ] ), nWidth - oCol:Width - 1 ) } ) )
   aadd(oBrwSets:Cargo[2],aarray)
   oCol:width:=50
   ocol:DefColor:={1,3}
   oBrwSets:GOTOPBLOCK := { || oBrwSets:cargo[ 1 ]:= 1 } 
   oBrwSets:GoBottomBlock := { || oBrwSets:cargo[ 1 ]:= Len(oBrwSets:cargo[ 2 ][ 1 ])} 
   oBrwSets:SKIPBLOCK := { |nPos| ( nPos:= ArrayBrowseSkip(nPos, oBrwSets), oBrwSets:cargo[ 1 ]:= ;
   oBrwSets:cargo[ 1 ] + nPos,nPos ) } 

   ::aWindows[::nCurWindow]:bPainted    := { || (oBrwSets:forcestable(),RefreshVarsS(oBrwSets))}
   ::aWindows[::nCurWindow]:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( aArray ),;
                           ::aWindows[::nCurWindow],::arrayName ,Len(aArray),aArray)}

   SetCursor( SC_NONE )
   ::aWindows[::nCurWindow]:ShowModal()

return self

method SetsKeyPressed( nKey, oBrwSets, nSets, oWnd ,cName,LenArr,aArray) Class TDBGArray

   local nSet := oBrwSets:cargo[1]
   local cTemp:=str(nSet,4)
   local cOldname:= ::arrayName
   Local nPos

   local nRecsToSkip
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
      Case nKey ==13

               if valtype(aArray[nSet])=="A"
                  SetPos(ownd:nBottom,ownd:nLeft)
                  ::aWindows[::nCurwindow]:lFocused:=.f.
                  ::arrayname:= ::arrayname+"["+alltrim(cTemp)+"]"
                  ::AddWindows(aArray[nSet],oBrwSets:RowPos+oBrwSets:nTop)
                  ::arrayname:=coldname

                  adel(::aWindows,::nCurWindow)
                  asize(::awindows,len(::awindows)-1)
                  if ::nCurwindow==0
                  ::ncurwindow:=1
                  else
                  ::ncurwindow--
                  endif
               elseif valtype(aArray[nSet])=="B"
                  Alert("Value cannot be edited")
               else

              oBrwSets:RefreshCurrent()
              ::doget(oBrwsets,aarray,nSet)
              oBrwSets:RefreshCurrent()
              oBrwSets:ForceStable()

               endif

   endcase
      RefreshVarsS(oBrwSets)

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
   endcase

return cResult

METHOD doGet(oBro,pItem,nSet) Class TDBGArray
    LOCAL column,  nKey
    local getlist:={}
    // save state
    LOCAL lScoreSave := Set( _SET_SCOREBOARD, .f. )
    LOCAL lExitSave  := Set( _SET_EXIT, .t. )
    LOCAL bInsSave   := SetKey( K_INS )

    // make sure browse is stable
    obro:forcestable()
    // if confirming new record, append blank

    // set insert key to toggle insert mode and cursor
    SetKey( K_INS, ;
        { || SetCursor( if(ReadInsert(!ReadInsert()), SC_NORMAL, SC_INSERT)) };
          )

    // initial cursor setting
    SetCursor( IF( ReadInsert(), SC_INSERT, SC_NORMAL ) )

    // get column object from browse
    column := oBro:getColumn( oBro:colPos )

    // create a corresponding GET
    @  row(),col() get pItem[nSet]
    // read it
    ReadModal(getlist )
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

function __DbgArrays(aArray,cArrayName)
return TDBGArray():New(aArray,cArrayName)
Static function GetTopPos(nPos)
Local nReturn:=0
nReturn:=if((maxrow()-nPos)<5,Maxrow()-nPos,nPos)
return nReturn
Static function GetBottomPos(nPos)
Local nReturn:=0
nReturn :=if(nPos<maxrow()-2,nPos ,maxrow()-2)
return nReturn

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
static function ArrayBrowseSkip( nPos, oBrwSets,n )

   return iif( oBrwSets:cargo[ 1 ] + nPos < 1, 0 - oBrwSets:cargo[ 1 ] + 1 , ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len(oBrwSets:cargo[ 2 ][ 1 ]), ;
      Len(oBrwSets:cargo[ 2 ][ 1 ]) - oBrwSets:cargo[ 1 ], nPos ) )
