/*
 * $Id$
 */

/*
 * STRINGP - Misc. string functions
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 * Part of the Harbour Project www.harbour-project.org
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
 */

#define DATA_SYMBOL 1
#define DATA_VAL    2

/* $Doc$
 * $FuncName$     <xRet> Default( <xArg>, <xDefault> )
 * $Description$  If argument is not set, return default
 * $End$ */
function Default( xArg, xDef )
return if( ValType(xArg) != ValType(xDef), xDef, xArg )


/* $Doc$
 * $FuncName$     <cOut> ToChar( <xTxt>, [cSeparator], [lDebug] )
 * $Description$  Convert to character
 * $Arguments$    <xTxt>       : Item to write
 *                [cSeparator] : Separator for arrays
 *                [lDebug]     : .T. -> Write debug output
 *
 * In DEBUG mode :
 *
 * It will show the xItem according to the following format :
 *
 * <num>                        Numerical
 * dd/mm/yyyy                   Date
 * "<chr>"                      Character
 * {<el1>, <el2>, ...}          Array
 * NIL                          NIL
 * .T. / .F.                    Boolean
 * <ClassName>(<ClassH>):{<DataSymbol1>:<val1>, ... }
 *                              Object
 *
 *
 * $End$ */
function ToChar( xTxt, cSeparator, lDebug )

   local cValTxt
   local cOut
   local n
   local nLen
   local aData

   cSeparator := Default( cSeparator, " " )
   lDebug     := Default( lDebug,     .F. )
   cValTxt    := ValType( xTxt )

   do case
      case cValTxt=="C" .or. cValTxt=="M"       // Character
         cOut := if( lDebug, '"'+xTxt+'"', xTxt )

      case cValTxt=="N"                         // Numeric
         cOut := Alltrim(Str(xTxt))

      case cValTxt=="U"                         // Nothing to write
         cOut := if( lDebug, "NIL", "" )

      case cValTxt=="D"                         // Date
         cOut := TransForm(xTxt, "")

      case cValTxt=="L"                         // Logical
         if lDebug
            cOut := if( xTxt, ".T.", ".F." )
         else
            cOut := if( xTxt, "True", "False" )
         endif

      case cValTxt=="A"                         // Array
         if lDebug
            cOut += "{"
         else
            cOut := ""
         endif
         nLen := Len( xTxt )
         for n := 1 to nLen                     // For each item : Recurse !
            cOut += ToChar( xTxt[n], cSeparator, lDebug )
            if n != nLen
               cOut += cSeparator
            endif
         next n
         if lDebug
            cOut += "}"
         endif

      case cValTxt=="B"                         // Codeblock
         if lDebug
            cOut := "Block"
         else
            cOut := Eval( xTxt )
         endif

      case cValTxt=="O"                         // Object
         if lDebug
            cOut  := xTxt:ClassName() + "(#" + ToChar( xTxt:ClassH() ) + "):{"
            aData := aoGet( xTxt )
            nLen  := Len( aData )
            for n := 1 to nLen                     // For each item : Recurse !
               cOut += aData[n][DATA_SYMBOL] + ":" + ;
                       ToChar( aData[n][DATA_VAL], cSeparator, lDebug )
               if n != nLen
                  cOut += cSeparator
               endif
            next n
            cOut += "}"
         else
            cOut := ToChar( xTxt:Run(), cSeparator, lDebug )
         endif

   endcase

return cOut

//
// <xItem> Debug ( <xItem> )
//
// Non-volatile debugging function showing contents of xItem and returing
// passed argument.
//
function Debug( xItem )

   QOut( ToChar( xItem, ", ", .T. ) )

return xItem



