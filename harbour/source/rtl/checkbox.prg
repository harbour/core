
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHECKBOX class
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

#include "Common.ch"
#include "hbSetup.ch"
#ifdef HB_COMPAT_C53

FUNCTION CHECKBOX(nRow,nCol,cCaption) 
Local cColor:=''
Local oClass
   if ( ( ISNUMBER( nRow ) ) ) .and. ( ( ISNUMBER( nCol ) ) )
      oClass:=TClass():New("CHECKBOX")
      if(!( ISCHARACTER( cCaption ) ) )
         cCaption := ""
     endif
      
   oClass:AddData( "Buffer"    , .f. )
   oClass:AddData( "Caption"   , cCaption )
   oClass:AddData( "CapRow"    , nRow )
   oClass:AddData( "CapCol"    , nCol+3+1 )
   oClass:AddData( "Cargo" )
   oClass:AddData( "Col"       , nCol )
   if ( !isdefcolor() )
      oClass:AddData( "ColorSpec" ,"W/N,W+/N,W/N,W+/N" )
   else
      cColor := SetColor()
      oClass:AddData( "ColorSpec" , __guicolor(cColor, 5) + "," + ;
      __guicolor(cColor, 2) + "," + __guicolor(cColor, 1) + ;
     "," + __guicolor(cColor, 4))
   endif

   oClass:AddData( "FBlock" )
   oClass:AddData( "HasFocus"  , .f. )
   oClass:AddData( "Message"   , "" )
   oClass:AddData( "Row"       , nRow )
   oClass:AddData( "SBlock"    )
   oClass:AddData( "Style"     , "[û ]" )
   oClass:AddData( "lCursor" )
   oClass:AddData( "Typeout"   , .f. )

   oClass:AddMethod( "SetFocus()"  , @SetFocus() )
   oClass:AddMethod( "Select()"    , @_Select() )
   oClass:AddMethod( "KillFocus()" , @KillFocus() )
   oClass:AddMethod( "Display()"   , @DisPlay() )
      oClass:Create()
   else
      return nil
   endif

return oClass:Instance()

STATIC Function SetFocus()
   Local Self := QSelf()
   if ( !::HasFocus .AND. ISBLOCK( ( ::lCursor := setcursor(0), ;
         ::HasFocus := .T., ::display(), ::FBlock ) ) )
      eval(::FBlock)
   endif
   RETURN Self

STATIC Function _Select(lState) 

   Local Self := QSelf()
   local lStatus := ::Buffer
   if ( ISLOGICAL( lState ) )
      ::Buffer := lState
   else
      ::Buffer := !::Buffer
   endif
   if ( lStatus != ::Buffer .AND. ISBLOCK( ( ::display(), ;
         ::SBlock ) ) )
      eval(::SBlock)
   endif
   RETURN Self

STATIC Function KillFocus() 
   Local Self := QSelf()
   if ( ::HasFocus )
      ::HasFocus := .F.
      if ( ISBLOCK( ::FBlock ) )
         eval(::FBlock)
      endif
      qself():display()
      setcursor(::lCursor)
   endif
   RETURN Self

   
STATIC Function Display()

   Local Self := QSelf()
   local cColor := SetColor(), nCurRow:= Row(), nCurCol:= Col(), ;
      cOldStyle := ::Style, cCaption, nPos 

   dispbegin()
   if ( ::HasFocus )
      set color to (__guicolor(::ColorSpec, 2))
   else
      set color to (__guicolor(::ColorSpec, 1))
   endif

      SetPos(::Row, ::Col + 1)
      if ( ::Buffer )
         ?? SubStr(cOldStyle, 2, 1)
      else
         ?? SubStr(cOldStyle, 3, 1)
      endif
      set color to (__guicolor(::ColorSpec, 3))
      SetPos(::Row, ::Col)
      ?? Left(cOldStyle, 1)
      SetPos(::Row, ::Col + 2)
      ?? right(cOldStyle, 1)

   if ( !Empty(cCaption := ::Caption) )
      if ( ( nPos := At("&", cCaption) ) == 0 )
      elseif ( nPos == Len(cCaption) )
         nPos := 0
      else
         cCaption := stuff(cCaption, nPos, 1, "")
      endif
      SetPos(::CapRow, ::CapCol)
      ?? cCaption
      if ( nPos != 0 )
         set color to (__guicolor(::ColorSpec, 4))
         SetPos(::CapRow, ::CapCol + nPos - 1)
         ?? SubStr(cCaption, nPos, 1)
      endif
   endif
   dispend()

   set color to (cColor)
   SetPos(nCurRow, nCurCol)
   RETURN Self



function __GUICOLOR( cPair, nPos )

   local ccolor := cPair, nPosition, nCommaPos
   for nPosition := 2 to nPos
      nCommaPos := At(",", ccolor)
      if ( nCommaPos == 0 )
         ccolor := ""
         exit
      endif
      ccolor := SubStr(ccolor, nCommaPos + 1)
   next
   nCommaPos := At(",", ccolor)
   if ( nCommaPos > 0 )
      ccolor := SubStr(ccolor, 1, nCommaPos - 1)
   endif
   return ccolor
function _CHECKBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7)

   local Local1, Local2, Local3, Local4
   oCheck := checkbox(Row(), Col(), Arg2)
   if ( !( ISNIL( oCheck ) ) )
      oCheck:select(Arg1)
      oCheck:caption :=Arg2
      oCheck:colorspec :=Arg4
      oCheck:message:=Arg3
      if arg7 !=NIL
      oCheck:style:=Arg7
      endif
      oCheck:fblock:=Arg5
      oCheck:sblock:=Arg6

   endif
   return oCheck

function IsDefColor()
   Return (SETCOLOR() != "W/N,N/W,N/N,N/N,N/W")

#endif
