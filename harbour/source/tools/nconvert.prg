/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Conversion Funtions
 *
 * Copyright 1999 Luiz Rafael Culik
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

/*  $DOC$
 *  $FUNCNAME$
 *   ISBIN()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *  Check if the value is a Binary  Number
 *  $SYNTAX$
 *   ISBIN(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> STRING TO BE CHECKED
 *  $RETURNS$
 *
 *    ^b .T.^b IF THE STRING IS BYNARY
 *    ^b .F.^b^ IF NOT
 *  $DESCRIPTION$
 *	^b check if the passed string is a bynary number or not
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *    ISOCTAL() ISDEC() ISHEXA()
 *  $INCLUDE$
 *
 *  $END$
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

/*  $DOC$
 *  $FUNCNAME$
 *   ISOCTAL()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Check if the value is a Octal  Number
 *  $SYNTAX$
 *   ISOCTAL(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> STRING TO BE CHECKED
 *  $RETURNS$
 *
 *  ^b .T.^b IF THE STRING IS OCTAL
 *  ^b .F.^b^ IF NOT
 *
 *  $DESCRIPTION$
 *	^b check if the passed string is a octal number or not
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *  ISBIN() ISDEC() ISHEXA()
 *  $INCLUDE$
 *
 *  $END$
 */

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

/*  $DOC$
 *  $FUNCNAME$
 *   ISDEC()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Check if the value is a Decimal  Number
 *  $SYNTAX$
 *   ISDEC(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> STRING TO BE CHECKED
 *  $RETURNS$
 *  ^b .T.^b IF THE STRING IS DECIMAL
 *  ^b .F.^b^ IF NOT
 *  $DESCRIPTION$
 *	^b check if the passed string is a decimal number or not
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *  ISOCTAL() ISBIN() ISHEXA()
 *  $INCLUDE$
 *
 *  $END$
 */

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

/*  $DOC$
 *  $FUNCNAME$
 *   ISHEXA()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Check if the value is a Hexal  Number
 *  $SYNTAX$
 *   ISHEXA(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> STRING TO BE CHECKED
 *  $RETURNS$
 *  ^b .T.^b IF THE STRING IS HEXA
 *  ^b .F.^b^ IF NOT
 *  $DESCRIPTION$
 *	  ^b check if the passed string is a hexa number or not
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *  ISOCTAL() ISDEC() ISBIN()
 *  $INCLUDE$
 *
 *  $END$
 */

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

/*  $DOC$
 *  $FUNCNAME$
 *   DECTOBIN()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Decimal Value to Binary
 *  $SYNTAX$
 *   DECTOBIN(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION DecToBin(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%2)
      cNewString:=SubStr('01',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/2)
   ENDDO
   RETURN(cNewString)

/*  $DOC$
 *  $FUNCNAME$
 *   DECTOOCTAL()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Decimal Value to Octal
 *  $SYNTAX$
 *   DECTOOCTAL(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION DecToOctal(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%8)
      cNewString:=SubStr('01234567',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/8)
   ENDDO
   RETURN(cNewString)

/*  $DOC$
 *  $FUNCNAME$
 *   DECTOHEXA()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Decimal Value to Hexa
 *  $SYNTAX$
 *   DECTOHEXA(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION DecToHexa(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%16)
      cNewString:=SubStr('0123456789ABCDEF',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/16)
   ENDDO
   RETURN(cNewString)

/*  $DOC$
 *  $FUNCNAME$
 *   BIntODEC()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Binary Value to Decimal
 *  $SYNTAX$
 *   BIntODEC(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION BinToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'01')-1)*;
	 (2**(nLen-nX))
   NEXT nX
   RETURN(nNumber)

/*  $DOC$
 *  $FUNCNAME$
 *   OCTALTODEC()
 *
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Octal Value to Decimal
 *  $SYNTAX$
 *   OCTALTODEC(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION OctalToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'01234567')-1)*;
	 (8**(nLen-nX))
   NEXT nX
   RETURN(nNumber)


/*  $DOC$
 *  $FUNCNAME$
 *   HEXATODEC()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *   Converts a Hexa Value to Decimal
 *  $SYNTAX$
 *   HEXATODEC(<CN>) -><CNR>
 *
 *  $ARGUMENTS$
 *  <CN> NUMBER TO BE CONVERTED
 *  $RETURNS$
 *  <CNR>  NUMBER CONVERTED
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION HexaToDec(cString)
   local nNumber:=0,nX:=0
   local cNewString:=AllTrim(cString)
   local nLen:=Len(cNewString)
   FOR nX:=1 to nLen
      nNumber+=(At(SubStr(cNewString,nX,1),'0123456789ABCDEF')-1)*;
	 (16**(nLen-nX))
   NEXT nX
   RETURN nNumber

* EOF: NCONVERT.PRG
