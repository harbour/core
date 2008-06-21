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


