/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Conversion Funtions
 *
 * Copyright 1999 Luiz Rafael Culik <Culik@sl.conex.net>
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

FUNCTION IsBin(cString)
   local nX,lFlag:=.t.
   cString:=AllTrim(cString)
   FOR nX:=1 to Len(cString)
      IF !(SubStr(cString,nX,1) $"01")
	 lFlag:=.F.
	 EXIT
      ENDIF
   NEXT nX
   RETURN(lFlag)

FUNCTION IsOctal(cString)
   local nX,lFlag:=.t.
   cString:=AllTrim(cString)
   FOR nX:=1 to Len(cString)
      if !(SubStr(cString,nX,1) $"01234567")
	 lFlag:=.f.
	 EXIT
      ENDIF
   NEXT nX
   RETURN(lFlag)

FUNCTION IsDec(cString)
   local nX,lFlag:=.t.
   cString:=AllTrim(cString)
   FOR nX:=1 to Len(cString)
      if !(SubStr(cString,nX,1) $"0123456789")
	 lFlag:=.f.
	 EXIT
      ENDIF
   NEXT nX
   RETURN(lFlag)

FUNCTION IsHexa(cString)
   local nX,lFlag:=.t.
   cString:=AllTrim(cString)
   FOR nX:=1 to Len(cString)
      if !(SubStr(cString,nX,1) $"0123456789ABCDEF")
	 lFlag:=.f.
	 EXIT
      ENDIF
   NEXT nX
   RETURN(lFlag)

FUNCTION DecToBin(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%2)
      cNewString:=SubStr('01',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/2)
   ENDDO
   RETURN(cNewString)

FUNCTION DecToOctal(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%8)
      cNewString:=SubStr('01234567',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/8)
   ENDDO
   RETURN(cNewString)

FUNCTION DecToHexa(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%16)
      cNewString:=SubStr('0123456789ABCDEF',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/16)
   ENDDO
   RETURN(cNewString)

FUNCTION BinToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'01')-1)*;
	 (2**(nLen-nX))
   NEXT nX
   RETURN(nNumber)

FUNCTION OctalToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'01234567')-1)*;
	 (8**(nLen-nX))
   NEXT nX
   RETURN(nNumber)
                        
FUNCTION HexaToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'0123456789ABCDEF')-1)*;
	 (16**(nLen-nX))
   NEXT nX
   RETURN nNumber


