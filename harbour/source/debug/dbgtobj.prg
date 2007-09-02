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
#include "common.ch"
#include "hbclass.ch"
#include "inkey.ch"

//#pragma -es0

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
data lEditable
Method new
method addWindows
method doget
method SetsKeyPressed
endclass

method new(aArray,pArName,lEditable) class tdbgObject
Local aTemp

DEFAULT lEditable TO .t.
::pItems:={}
::AllNames:={}
for each aTemp in __objGetValueList(aArray)
   aadd(::pItems,{aTemp[1],aTemp[2]})
   aadd(::AllNames,aTemp[1])
next
for each aTemp in __objGetMethodList(aArray)
   if !empty(aTemp)
      aadd(::pItems,{aTemp,"Method"})
      aadd(::AllNames,aTemp)
   endif
next
::aWindows:={}
::objname:=parName
::TheObj:=aArray

::nCurWindow:=0
::ArrayReference:={}
::ArrayIndex:=1
::lEditable = lEditable

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
         owndsets:=TDbWindow():New( nRow, 5, if(nRow+nsize+1<maxrow()-2,nRow+nsize+1,maxrow()-2), maxcol()-5, ::objname +" is of class: " +::TheObj:classname() ,"N/W" )
      else
         owndsets:=TDbWindow():New( 1, 5, 2+nsize, maxcol()-5, ::objname +" is of class: " +::TheObj:classname()  ,"N/W")
      endif
   else
      owndsets:=TDbWindow():New( 1, 5, maxrow()-2, maxcol()-5, ::objname +" is of class: " +::TheObj:classname() ,"N/W")
   endif

   ::nCurWindow++
   oWndSets:lFocused:=.t.
   aadd(::aWindows,owndsets)

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1

   oBrwSets:=TbrowseNew(owndsets:nTop+1, owndsets:nLeft+1, owndsets:nBottom-1, owndsets:nRight-1)
   ::ArrayReference:=aarray

   oBrwSets:ColorSpec := __Dbg():ClrModal()
   oBrwSets:GoTopBlock := { || ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := { || ::arrayindex := Len( ::ArrayReference) }
   oBrwSets:SkipBlock := { | nSkip, nPos | nPos := ::arrayindex,;
                          ::arrayindex := iif( nSkip > 0, Min( ::arrayindex+nSkip, Len(::arrayreference)),;
                          Max( 1, ::arrayindex + nSkip ) ), ::arrayindex - nPos }

   nMaxElem := maxelem(::AllNames)
   oBrwSets:AddColumn( ocol := TBColumnNew( "",;
                    { || PadR( ::ArrayReference[ ::arrayindex, 1 ], nMaxElem ) } ) )
   ocol:width := nMaxElem
   ocol:ColorBlock :=    { || { iif( ::Arrayindex == oBrwSets:Cargo, 2, 1 ), 2 } }
   oBrwSets:Freeze:=1

   oBrwSets:AddColumn( ocol:=TBColumnNew( "", { || iif( ValType( ;
   ::ArrayReference[ ::ArrayIndex, 2 ] ) == "C" .and. ;
   ::ArrayReference[ ::ArrayIndex, 2 ] == "Method",;
   "Method", PadR( ValToStr( __ObjSendMsg( ::TheObj, ::ArrayReference[ ::arrayindex ,1] ) ),;
   nWidth  - 12 ) ) } ) )

   oBrwSets:Cargo := 1 // Actual highligthed row
   ocol:ColorBlock := { || { iif( ::Arrayindex == oBrwSets:Cargo, 3, 1 ), 3 } }
   ocol:width:= MaxCol() - 14 - nMaxElem
   oBrwsets:colpos:=2
   ::aWindows[::nCurWindow]:bPainted    := { || oBrwSets:ForceStable() }
   ::aWindows[::nCurWindow]:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( aArray ),;
                            ::aWindows[::nCurWindow],::objname ,Len(::Arrayreference),::pitems) }
   ::aWindows[::nCurwindow]:cCaption := ::objname +" is of class: " +::TheObj:classname()

   SetCursor( SC_NONE )
   ::aWindows[::nCurWindow]:ShowModal()

return self

method SetsKeyPressed( nKey, oBrwSets, nSets, oWnd ,cName,LenArr,aArray) class tdbgObject

   local nSet := oBrwSets:Cargo
   local cTemp:=str(nSet,4)
   local cOldname:= ::objname

   HB_SYMBOL_UNUSED( nSets )
   HB_SYMBOL_UNUSED( oWnd )
   HB_SYMBOL_UNUSED( cName )
   HB_SYMBOL_UNUSED( LenArr )

   Switch nKey
      case K_UP
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo--
              oBrwSets:RefreshCurrent()
              oBrwSets:Up()
              oBrwSets:ForceStable()
           endif
           exit

      case K_DOWN
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo++
              oBrwSets:RefreshCurrent()
              oBrwSets:Down()
              oBrwSets:ForceStable()
           endif
           exit

      case K_HOME
           if oBrwSets:Cargo > 1
              oBrwSets:Cargo := 1
              oBrwSets:GoTop()
              oBrwSets:ForceStable()
           endif
           exit

      case K_END
           if oBrwSets:Cargo < nSets
              oBrwSets:Cargo := nSets
              oBrwSets:GoBottom()
              oBrwSets:ForceStable()
           endif
           exit

      case K_PGUP
           oBrwSets:PageUp()
           oBrwSets:Cargo := ::ArrayIndex
           oBrwSets:RefreshCurrent()
           oBrwSets:ForceStable()
           exit

      case K_PGDN
           oBrwSets:PageDown()
           oBrwSets:Cargo := ::ArrayIndex
           oBrwSets:RefreshCurrent()
           oBrwSets:ForceStable()
           exit

      Case K_ENTER
            if nSet==oBrwSets:Cargo
               if valtype(aArray[nSet,2])=="A"
                  if len(   aArray[nSet,2])>0
                     TDBGArray():New(aArray[nSet,2],::pitems[nSet,1])
                  endif
               elseif valtype(aArray[nSet,2])=="H"
                  if len(   aArray[nSet,2])>0
                     TDBGHash():New(aArray[nSet,2],::pitems[nSet,1])
                  endif
               elseif valtype(aArray[nSet,2])=="O"
                  tdbgObject():New(aArray[nSet,2],::pitems[nSet,1])
               elseif ( ValType( aArray[ nSet, 2 ] ) == "C" .AND. ;
                        aArray[ nSet, 2 ] == "Method" ) .OR. ;
                      ValType( aArray[ nSet, 2 ] ) == "B" .OR. ;
                      ValType( aArray[ nSet, 2 ] ) == "P"
                  Alert("Value cannot be edited")

              else
                 if ::lEditable
                    oBrwSets:RefreshCurrent()
                    cTemp:=::doget(oBrwsets,::arrayreference,nSet)
                    oBrwSets:RefreshCurrent()
                    oBrwSets:ForceStable()
                 else
                    Alert( "Value cannot be edited" )
                 endif

               endif

            endif
            exit

   end

return nil

static function ValToStr( uVal )

   local cType := ValType( uVal )
   local cResult := "U"

   Switch cType
      case "U"
           cResult := "NIL"
           exit

      case "A"
           cResult := "{ ... }"
           exit

      case "H"
           cResult := "Hash of " + AllTrim( Str( Len( uVal ) ) ) + " elements"
           exit

      case "C"
      case "M"
           cResult := '"' + uVal + '"'
           exit

      case "L"
           cResult := iif( uVal, ".T.", ".F." )
           exit

      case "D"
           cResult := DToC( uVal )
           exit

      case "N"
           cResult := AllTrim( Str( uVal ) )
           exit

      case "O"
           cResult := "Class " + uVal:ClassName() + " object"
           exit

      case "B"
           cResult:= "{ || ... }"
           exit

      case "P"
           cResult := "Pointer"
           exit

   end

return cResult

METHOD doGet(oBro,pItem,nSet) class tdbgObject

#ifndef HB_NO_READDBG

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
    cValue := PadR( ValToStr( pitem[nSet,2] ), column:Width )
    @ row(),col() GET cValue ;
        VALID If( Type( cValue ) == "UE", ( Alert( "Expression error" ), .f. ), .t. )


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

#else

    HB_SYMBOL_UNUSED( oBro )
    HB_SYMBOL_UNUSED( pItem )
    HB_SYMBOL_UNUSED( nSet )

#endif

RETURN nil

static FUNC maxelem( a )

   LOCAL max     := 0
   LOCAL tam     := 0
   LOCAL elem

   for each elem in a
      tam := LEN( elem )
      max := IF( tam > max, tam, max )
   NEXT

RETURN max

function __DbgObject(aArray,pArName,lEditable)
return TDBGObject():New(aArray,pArName,lEditable)
