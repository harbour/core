
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RADIOGROUP class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "common.ch"
#include "hbclass.ch"
#include "hbsetup.ch"

#ifdef HB_COMPAT_C53
CLASS HBRadioGroup

   export:

   METHOD AddItem(xItem)
   METHOD DelItem(xItem)
   METHOD Display()
   METHOD GetAccel(xItem)
   METHOD GetItem(Xitem)
   METHOD HitTest(nRow,nCol)
   METHOD InsItem(nPos, oButtom )
   METHOD KillFocus()
   METHOD NextItem()
   METHOD PrevItem()
   MESSAGE Select(xItem) METHOD _Select(xItem)
   MESSAGE SetColor(xItem) METHOD _SetColor(xItem)
   METHOD SetFocus()
   METHOD SetStyle(xItem)
   METHOD New(nTop, nLeft, nBottom, nRight )
   METHOD GetColor(xColor)
   DATA Bottom

   DATA Buffer init  NIL

   DATA CapCol

   DATA CapRow

   DATA Caption init ""

   DATA Cargo  init  Nil

   DATA ColdBox  init  "ÚÄ¿³ÙÄÀ³"

   DATA fBlock  init  NIL

   DATA HasFocus  init  .f.

   DATA HotBox init  "ÉÍ»º¼ÍÈº"

   DATA ItemCount init 0

   DATA Left 

   DATA Message init ""

   DATA Right
   DATA aItems init {}
   DATA lCursor init 0

   DATA TextValue init ""

   DATA Top
   DATA CLASSName init "RADIOGROUP"
   DATA TypeOut init .f.

   DATA Value init 0
   DATA Color
   ACCESS colorspec inline ::GetColor()
   ASSIGN Colorspec(xColor) inline if(xColor!=Nil,::GetColor(xColor),)

ENDCLASS

METHOD New(nTop, nLeft, nBottom, nRight ) CLASS HBRadioGroup

Local cColor
      if ( isdefcolor() )
          ::ColorSpec:= "W/N,W/N,W+/N"
      else
         cColor := SetColor()
         ::ColorSpec:=  __guicolor(cColor, 3) + "," + ;
            __guicolor(cColor, 1) + "," + __guicolor(cColor, 4)
      endif
      ::Bottom:=nBottom
      ::CapCol:= nLeft+2
      ::CapRow:= nTop
      ::Left:=nLeft
      ::right:=nRight
      ::top:=nTop
return Self

METHOD  ADDITEM( xItem ) CLASS HBRadioGroup


   if ( !( ISOBJECT( xItem ) ) )
   elseif ( xItem:classname() == "RADIOBUTTN" )
      AAdd(::aItems, xItem)
      ::ItemCount++
   endif
   return Self

METHOD  SETSTYLE( xStyle ) CLASS HBRadioGroup
   
   local nPos, nLen, aItems := ::aItems
   nLen := ::ItemCount
   for nPos := 1 to nLen
      aItems[ nPos ]:style(xStyle)
   next
   return Self
METHOD  SETFOCus() CLASS HBRadioGroup


   local nPos, nLen, aItems
   if ( !::HasFocus )
      ::lCursor := setcursor(0)
      ::HasFocus := .T.
      aItems := ::aItems
      nLen := ::ItemCount
      dispbegin()
      for nPos := 1 to nLen
         aItems[ nPos ]:setfocus()
      next
      ::display()
      dispend()

      if ( ISBLOCK( ::fBlock ) )
         eval(::fBlock)
      endif
   endif
   return self

METHOD  _SETCOLor( Arg1 ) CLASS HBRadioGroup


   local nPos, nLen, aItems := ::aItems
   nLen := ::ItemCount
   for nPos := 1 to nLen
      aItems[ nPos ]:colorspec :=Arg1
   next
   return Self
METHOD  _SELECT( xValue ) CLASS HBRadioGroup

   local nPos, nLen, cType := ValType(xValue)
   if ( cType == "C" )
      nLen := ::ItemCount
      for nPos := 1 to nLen
         if ( ::aItems[ nPos ]:data == xValue )
            default ::Buffer to ""
            changebutt(self, ::Value, nPos)
            exit
         endif
      next
      if ( nPos > nLen )
         ::Buffer := xValue
      endif
   elseif ( cType != "U" .AND. xValue < 1 )
   elseif ( cType != "U" .AND. xValue <= ::ItemCount )
      default ::Buffer to 0
      changebutt(self, ::Value, xValue)
   endif
   return qself()
METHOD  PREVITem()        CLASS HBRadioGroup


   local xValue, nPos
   if ( !::HasFocus )
   elseif ( ::ItemCount > 0 )
      if ( ( xValue := ::Value ) == 0 )
         nPos := 1
      elseif ( xValue == 1 )
         nPos := ::ItemCount
      else
         nPos := xValue - 1
      endif
      changebutt(self, xValue, nPos)
   endif
   return self
METHOD  NEXTITem()        CLASS HBRadioGroup


   local xValue, nPos
   if ( !::HasFocus )
   elseif ( ::ItemCount > 0 )
      if ( ( xValue := ::Value ) == ::ItemCount )
         nPos := 1
      else
         nPos := xValue + 1
      endif
      changebutt(self, xValue, nPos)
   endif
   return Self
METHOD  KILLFOcus()       CLASS HBRadioGroup

   local nPos, nCount, aItems
   if ( ::HasFocus )
      ::HasFocus := .F.
      if ( ISBLOCK( ::fBlock ) )
         eval(::fBlock)
      endif
      aItems := ::aItems
      nCount := ::ItemCount

      dispbegin()
      for nPos := 1 to nCount
         aItems[ nPos ]:killfocus()
      next
      ::display()
      dispend()

      setcursor(::lCursor)
   endif
   return self
METHOD  INSITEM( nPos, oButtom ) CLASS HBRadioGroup


   if ( !( ISOBJECT( oButtom ) ) )
   elseif ( !( oButtom:classname() == "RADIOBUTTN" ) )
   elseif ( nPos < ::ItemCount )
      asize(::aItems, ++::ItemCount)
      ains(::aItems, nPos)
      ::aItems[ nPos ] := oButtom
   endif
   return ::aItems[ nPos ]
METHOD  HITTEST( nRow, nCol )    CLASS HBRadioGroup

   local nPos, nCount, aItem := ::aItems, nLen, nPosition
   nCount := ::ItemCount
   do case
   case Empty(::Coldbox + ::HotBox)
   case nRow == ::Top
      if ( nCol == ::Left )
         return -1
      elseif ( nCol == ::Right )
         return -3
      elseif ( nCol >= ::Left .AND. nCol <= ::Right )
         return -2
      endif
   case nRow == ::Bottom
      if ( nCol == ::Left )
         return -7
      elseif ( nCol == ::Right )
         return -5
      elseif ( nCol >= ::Left .AND. nCol <= ::Right )
         return -6
      endif
   case nCol == ::Left
      if ( nRow >= ::Top .AND. nRow <= ::Bottom )
         return -8
      else
         return 0
      endif
   case nCol == ::Right
      if ( nRow >= ::Top .AND. nRow <= ::Bottom )
         return -4
      else
         return 0
      endif
   endcase
   nLen := Len(::Caption)
   if ( ( nPosition := At("&", ::Caption) ) == 0 )
   elseif ( nPosition < nLen )
      nLen--
   endif
   do case
   case Empty(::Caption)
   case nRow != ::CapRow
   case nCol < ::CapCol
   case nCol < ::CapCol + nLen
      return -1025
   endcase
   do case
   case nRow < ::Top
   case nRow > ::Bottom
   case nCol < ::Left
   case nCol <= ::Right
      for nPos := 1 to nCount
         if ( aItem[ nPos ]:hittest(nRow, nCol) != 0 )
            return nPos
         endif
      next
      return -2049
   endcase
   return 0
METHOD  GETITEm( xValue )        CLASS HBRadioGroup
   local xReturn := Nil
   if ( xValue < 1 )
   elseif ( xValue <= ::ItemCount )
      xReturn := ::aItems[ xValue ]
   endif
   return xReturn
METHOD  GetAccel( xValue )       CLASS HBRadioGroup

   local nPos, nLen, aItem
   if ( ISNUMBER( xValue ) )
      xValue := Chr(xValue)
   elseif ( !ValType(xValue == "C") )
      return 0
   endif
   aItem := ::aItems
   nLen := Len(aItem)
   xValue := Lower(xValue)
   for nPos := 1 to nLen
      if ( aItem[ nPos ]:isaccel(xValue) )
         return nPos
      endif
   next
   return 0

METHOD  DISPLAY() CLASS HBRadioGroup


   local nPos, nCount, aItem, cColor := SetColor(), nCurRow:= ;
      Row(), nCurCol := Col(), cSelBox, cUnSelBox, cCaption, nPosition
   aItem := ::aItems
   nCount := ::ItemCount

   dispbegin()
   if ( ::HasFocus )
      cSelBox := ::HotBox
      cUnSelBox := ::Coldbox
   else
      cSelBox := ::Coldbox
      cUnSelBox := ::HotBox
   endif
   set color to (__guicolor(::ColorSpec, 1))
   if ( !Empty(cSelBox) )
      @ ::Top, ::Left, ::Bottom, ::Right ;
         box cSelBox
   elseif ( !Empty(cUnSelBox) )
      @ ::Top, ::Left, ::Bottom, ::Right ;
         box cUnSelBox
   endif
   if ( !Empty(cCaption := ::Caption) )
      if ( ( nPosition := At("&", cCaption) ) == 0 )
      elseif ( nPosition == Len(cCaption) )
         nPosition := 0
      else
         cCaption := stuff(cCaption, nPosition, 1, "")
      endif
      set color to (__guicolor(::ColorSpec, 2))
      SetPos(::CapRow, ::CapCol)
      ?? cCaption
      if ( nPosition != 0 )
         set color to (__guicolor(::ColorSpec, 3))
         SetPos(::CapRow, ::CapCol + nPosition - 1)
         ?? SubStr(cCaption, nPosition, 1)
      endif
   endif
   for nPos := 1 to nCount
      aItem[ nPos ]:display()
   next
   dispend()

   set color to (cColor)
   SetPos(nCurRow, nCurCol)
   return self

METHOD  DELITEm( xItem ) CLASS HBRadioGroup

   if ( xItem < 1 )
   elseif ( xItem <= ::ItemCount )
      adel(::aItems[ xItem ])
      asize(::aItems, --::ItemCount)
   endif
   if ( !::HasFocus )
   elseif ( ::ItemCount < ::Value )
      ::Value := ::ItemCount
      ::TextValue := ::aItems[ ::Value ]:data
      if ( ISNUMBER( ::Buffer ) )
         ::Buffer := ::Value
      else
         ::Buffer := ::TextValue
      endif
   endif
   return self

METHOD GetColor(xColor)  CLASS HBRadioGroup
   if ( !( ISNIL( xColor ) ) )
      ::Color := iif( Valtype(xColor)=="C" .and. !Empty(__guicolor(xColor, 3)) .AND. ;
      Empty(__guicolor(xColor, 4)),xColor,)

   endif
   return ::Color

static function  CHANGEBUTT( oItems, xVal, nPos )


   if ( xVal != nPos )

      dispbegin()
      if ( xVal > 0 )
         oItems:aItems[ xVal ]:select(.F.)
         oItems:aItems[ xVal ]:display()
      endif
      if ( nPos > 0 )
         oItems:aItems[ nPos ]:select(.T.)
         oItems:aItems[ nPos ]:display()
      endif
      dispend()

      oItems:Value := nPos
      oItems:TextValue := oItems:aItems[ nPos ]:data
      if ( ISNUMBER( oItems:Buffer ) )
         oItems:Buffer := nPos
      else
         oItems:Buffer := oItems:TextValue
      endif
   endif
   return .T.
// Radio Group Class Constructor Function
function RADIOGROUP( nTop, nLeft, nBottom, nRight )
      if ( ( ISNUMBER( nTop ) ) ) .and. ( ( ISNUMBER( nLeft ) ) ) .and. ( ( ISNUMBER( nBottom ) ) ) .and. ( ( ISNUMBER( nright ) ) )
         Return HBRadioGroup():New(nTop, nLeft, nBottom, nRight )
      endif
   Return Nil


function _RADIOGRP_( nTop, nLeft, nBottom, nRight, xValue, aItems, cCaption, cMessage, ;
   cColor, bFblock )

   local oRadioGroup, nPos, nLen
   oRadioGroup := radiogroup(nTop, nLeft, nBottom, nRight)
   if ( !( ISNIL( oRadioGroup ) ) )
      oRadioGroup:caption:= if(cCaption!=NIL,cCaption,)
      oRadioGroup:colorspec:=if(cColor!=Nil,cColor,)
      oRadioGroup:message:=if(cMessage!=nil,cMessage,)
      oRadioGroup:fblock:=if(bFblock!=nil,bFblock,)
      nLen := Len(aItems)
      for nPos := 1 to nLen
         oRadioGroup:additem(aItems[ nPos ])
      next
      oRadioGroup:select(xValue)
   endif
   return oRadioGroup

#endif
