/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Object Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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


#include "setcurs.ch"
#include "common.ch"
#include "hbclass.ch"
#include "inkey.ch"
#pragma -es0
Class TDBGobject
//export:
data aWindows
data Theobj
data objname
data nCurWindow
data pItems
Data ArrayReference
Data ArrayIndex
Data AllNames
Method new
method addWindows
method doget
method SetsKeyPressed
endclass

method new(aArray,pArName) class tdbgObject
Local nPos
local aTempvars:=   __objGetValueList(aArray)
Local aTempMethods:=  __objGetMethodList(aArray)
::pItems:={}
::AllNames:={}
for nPos :=1 to len(aTempvars)
   aadd(::pItems,{aTempvars[nPos,1],aTempvars[nPos,2]})
   aadd(::AllNames,aTempvars[nPos,1])
next
for nPos :=1 to len(aTempMethods)
   if !empty(aTempMethods[nPos])
      aadd(::pItems,{aTempMethods[nPos],"Method"})
      aadd(::AllNames,aTempMethods[nPos])
   endif
next
::aWindows:={}
::objname:=parName
::TheObj:=aArray

::nCurWindow:=0
::ArrayReference:={}
::ArrayIndex:=1

::addWindows(::pItems)
Return Self

Method addWindows(aArray,nRow) class tdbgObject
local oBrwSets,nSize:=Len(AArray)
//local n:=1
Local owndsets
   local nWidth
   local oCol
   local nMaxElem

   if (nsize<maxrow()-2)
      if nRow <> nil
         owndsets:=TDbWindow():New( nRow, 5, if(nRow+nsize+1<maxrow()-2,nRow+nsize+1,maxrow()-2), maxcol()-5, ::objname +" is of class:" +::TheObj:classname() ,"N/W" )
      else
         owndsets:=TDbWindow():New( 1, 5, 1+nsize, maxcol()-5, ::objname +" is of class:" +::TheObj:classname()  ,"N/W")
      endif
   else
      owndsets:=TDbWindow():New( 1, 5, maxrow()-2, maxcol()-5, ::objname +" is of class:" +::TheObj:classname() ,"N/W")
   endif
                  ::nCurWindow++
   oWndSets:lFocused:=.t.
   aadd(::aWindows,owndsets)

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1

   oBrwSets:=TbrowseNew(owndsets:nTop+1, owndsets:nLeft+1, owndsets:nBottom-1, owndsets:nRight-1)
   ::ArrayReference:=aarray

   oBrwSets:ColorSpec := "N/W, R/W, N/bg"
   oBrwSets:GoTopBlock := { || ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := { || ::arrayindex := Len( ::ArrayReference) }
   oBrwSets:SkipBlock := { | nSkip, nPos | nPos := ::arrayindex,;
                          ::arrayindex := iif( nSkip > 0, Min( ::arrayindex+nSkip, Len(::arrayreference)),;
                          Max( 1, ::arrayindex + nSkip ) ), ::arrayindex - nPos }
   oBrwSets:AddColumn( ocol:=     TBColumnNew("", { || ::ArrayReference[::arrayindex,1]} ))
   nMaxElem = maxelem(::AllNames)
   ocol:width := nMaxElem
   ocol:ColorBlock :=    { || { iif( ::Arrayindex == oBrwSets:Cargo, 2, 1 ), 2 } }
   oBrwSets:Freeze:=1
   oBrwSets:AddColumn( ocol:=TBColumnNew( "" ,{ || If( ::ArrayReference[ ::arrayIndex, 2 ] != "Method", PadR( ValToStr( __ObjSendMsg( ::TheObj, ::ArrayReference[ ::arrayindex ,1] ) ), nWidth  - 12 ), "Method" ) } ) )
   oBrwSets:Cargo := 1 // Actual highligthed row
   ocol:ColorBlock := { || { iif( ::Arrayindex == oBrwSets:Cargo, 3, 1 ), 3 } }
   ocol:width:= MaxCol() - 14 - nMaxElem
   oBrwsets:colpos:=2
   ::aWindows[::nCurWindow]:bPainted    := { || oBrwSets:ForceStable() }
   ::aWindows[::nCurWindow]:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( aArray ),;
                            ::aWindows[::nCurWindow],::objname ,Len(::Arrayreference),::pitems) }
   ::aWindows[::nCurwindow]:cCaption := ::objname +" is of class:" +::TheObj:classname()

   SetCursor( SC_NONE )
   ::aWindows[::nCurWindow]:ShowModal()

return self

method SetsKeyPressed( nKey, oBrwSets, nSets, oWnd ,cName,LenArr,aArray) class tdbgObject

   local nSet := oBrwSets:Cargo
   local cTemp:=str(nSet,4)
   local cOldname:= ::objname
   Local nPos

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

      case nKey == K_PGUP
           oBrwSets:PageUp()
           oBrwSets:Cargo = ::ArrayIndex
           oBrwSets:RefreshCurrent()
           oBrwSets:ForceStable()

      case nKey == K_PGDN
           oBrwSets:PageDown()
           oBrwSets:Cargo = ::ArrayIndex
           oBrwSets:RefreshCurrent()
           oBrwSets:ForceStable()

      Case nKey ==13
            if nSet==oBrwSets:Cargo
               if valtype(aArray[nSet,2])=="A"
                  if len(   aArray[nSet,2])>0
                     TDBGArray():New(aArray[nSet,2],::pitems[nSet,1])
                  endif
               elseif valtype(aArray[nSet,2])=="O"
                  tdbgObject():New(aArray[nSet,2],::pitems[nSet,1])
/*               elseif __objHasMethod(::theObj,aArray[nSet,2])
                  Alert("Value cannot be edited")*/
               elseif valtype(aArray[nSet,2])=="B"
                  Alert("Value cannot be edited")

              else
                 oBrwSets:RefreshCurrent()
                 cTemp:=::doget(oBrwsets,::arrayreference,nSet)
                 oBrwSets:RefreshCurrent()
                 oBrwSets:ForceStable()

               endif

            endif

   endcase

return nil

static function ValToStr( uVal )

   local cType := ValType( uVal )
   local cResult := "U"

   do case
      case uVal == nil
           cResult := "NIL"

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
      Case cType  =="B"
         cResult:= "{ || ... }"

   endcase

return cResult

METHOD doGet(oBro,pItem,nSet) class tdbgObject
    LOCAL column,  nKey
    local getlist:={}
    // save state
    LOCAL lScoreSave := Set( _SET_SCOREBOARD, .f. )
    LOCAL lExitSave  := Set( _SET_EXIT, .t. )
    LOCAL bInsSave   := SetKey( K_INS )
    local cValue

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
    cValue = PadR( ValToStr( pitem[nSet,2] ), column:Width )
    @  row(),col() GET cValue
//    get := Getnew( Row(),col(), column:block,,, oBro:colorSpec )


    // read it
    ReadModal(getlist )
//    eval(column:block,get:Buffer)
    // restore state
    SetCursor( 0 )
    Set( _SET_SCOREBOARD, lScoreSave )
    Set( _SET_EXIT, lExitSave )
    SetKey( K_INS, bInsSave )

    if LastKey() == K_ENTER
       __ObjSendMsg( ::TheObj, "_" + pitem[ nSet, 1 ], &cValue )
    endif

    // check exit key from get
    nKey := LastKey()
    IF nKey == K_UP .OR. nKey == K_DOWN .OR. nKey == K_PGUP .OR. nKey == K_PGDN
        KEYBOARD CHR( nKey )
    END
RETURN nil

static FUNC maxelem( a )

   LOCAL nSize   := LEN( a )
   LOCAL max     := 0
   LOCAL tam     := 0
   LOCAL nCount

   FOR nCount := 1 TO nSize
      tam := LEN( a[ nCount ] )
      max := IF( tam > max, tam, max )
   NEXT

RETURN max

function __DbgObject(aArray,pArName)
return TDBGObject():New(aArray,pArName)