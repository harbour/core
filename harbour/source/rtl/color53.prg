/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Color functions for Getsys and Menu System
 *
 * Copyright 2003 Walter Negro <anegro@overnet.com.ar>
 * www - http://www.xharbour.org
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

Function GETCLRPAIR( cColor, nColor )
   Local nPos

   if ( nPos := getpairpos( cColor, nColor ) ) == 0
      Return ""
   endif

   Return SubStr( cColor, nPos, getpairlen( cColor, nColor ) )

****************************************************************
Function SETCLRPAIR( cColor, nColor, cNewColor )
   Local nPos

   if ( nPos := getpairpos( cColor, nColor ) ) == 0
      Return ""
   endif

   Return stuff( cColor, nPos, getpairlen( cColor, nColor ), cNewColor )

****************************************************************
Function GETPAIRPOS( cColor, nColor )
   Local n, nPos := 1, nSep

   For n := 2 To nColor
      nSep := At( ",", SubStr( cColor, nPos) )
      if nSep == 0
         nPos := 0
         Exit
      endif
      nPos += nSep
   Next

   Return nPos

****************************************************************
Function GETPAIRLEN( cColor, nColor )
   Local nPos := getpairpos( cColor, nColor ), nLen

   if nPos == 0
      Return 0
   endif

   nLen := At( ",", SubStr( cColor, nPos ) )

   if nLen == 0
      nLen := Len( cColor ) - nPos + 1
   Else
      nLen--
   endif

   Return nLen

****************************************************************
Function GETCLRFORE( cColor )
   Local nPos

   if ( nPos := At( "/", cColor ) ) == 0
      Return ""
   endif

   Return SubStr( cColor, 1, nPos - 1 )

****************************************************************
Function GETCLRBACK( cColor )
   Local nPos

   if ( nPos := At( "/", cColor ) ) == 0
      Return ""
   endif

   Return SubStr( cColor, nPos + 1 )

****************************************************************
Function RADGRDEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "W/N", "W+/N")
   Else
      Return applydefau( cColor, 3, 1, 4)
   endif

   Return nil

****************************************************************
Function RADITDEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N")
   Else
      Return applydefau( cColor, 5, 5, 2, 2, 1, 1, 4)
   endif

   Return nil

****************************************************************
Function LISTBDEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N")
   Else
      Return applydefau( cColor, 5, 5, 5, 2, 3, 1, 4)
   endif

   Return nil

****************************************************************
Function COMBODEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N", "W/N")
   Else
      Return applydefau( cColor, 5, 5, 5, 2, 3, 1, 4, 1)
   endif

   Return nil

****************************************************************
Function CHECKDEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "W+/N", "W/N", "W+/N")
   Else
      Return applydefau( cColor, 5, 2, 1, 4)
   endif

   Return nil

****************************************************************
Function BUTTNDEFCO( cColor )

   if isdefcolor()
      Return applydefau( cColor, "W/N", "N/W", "W+/N", "W+/N")
   Else
      Return applydefau( cColor, 5, 2, 1, 4)
   endif

   Return nil

****************************************************************
Function MENUDEFCOL( cColor )

   if isdefcolor()
      Return applydefau( cColor, "N/W", "W/N", "W+/W", "W+/N", "N+/W", "W/N")
   Else
      Return applydefau( cColor, 5, 2, 4, 2, 1, 3)
   endif

   Return nil

****************************************************************
Function APPLYDEFAU( cColor, xClr1, xClr2, xClr3, xClr4, xClr5, xClr6, xClr7, xClr8 )
   Local cSetColor, aSetColor := {}, aNewcolor := {}, nColors, cClrDefa
   Local cClrToSet, cClrFore, cClrBack
   Local cNewClrFore, cNewClrBack, xNewColor, n

   if PCount() == 0
      Return ""
   endif

   if PCount() == 1
      Return cColor
   endif

   cSetColor := setcolor()

   asize( aSetColor, 5)
   aSetColor[1] := getclrpair( cSetColor, 1 )
   aSetColor[2] := getclrpair( cSetColor, 2 )
   aSetColor[3] := getclrpair( cSetColor, 3 )
   aSetColor[4] := getclrpair( cSetColor, 4 )
   aSetColor[5] := getclrpair( cSetColor, 5 )

   asize( aNewColor, 8)
   aNewColor[1] := xClr1
   aNewColor[2] := xClr2
   aNewColor[3] := xClr3
   aNewColor[4] := xClr4
   aNewColor[5] := xClr5
   aNewColor[6] := xClr6
   aNewColor[7] := xClr7
   aNewColor[8] := xClr8

   nColors  := PCount() - 1
   cClrDefa := cColor

   for n = 1 to Len( aNewColor )
       xNewColor = aNewColor[ n ]

      cClrToSet := getclrpair( cClrDefa, n )

      if At( "/", cClrToSet ) == 0

         if ISNUMBER( xNewColor )
            cClrDefa := setclrpair( cClrDefa, n, aSetColor[ xNewColor ] )
         Else
            cClrDefa := setclrpair( cClrDefa, n, xNewColor )
         endif

      else

         cClrFore := getclrfore( cClrToSet )
         cClrBack := getclrback( cClrToSet )

         if ISNUMBER( xNewColor )
            cNewClrFore := getclrfore( aSetColor[ xNewColor ] )
            cNewClrBack := getclrback( aSetColor[ xNewColor ] )
         else
            cNewClrFore := getclrfore( xNewColor )
            cNewClrBack := getclrback( xNewColor )
         endif

         if cClrFore == ""
            cClrFore := cNewClrFore
         endif

         if cClrBack == ""
            cClrBack := cNewClrBack
         endif

         cClrToSet := cClrFore + "/" + cClrBack
         cClrDefa  := setclrpair( cClrDefa, n, cClrToSet )

      endif

   Next
   Return cClrDefa

