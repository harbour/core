/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 functions TABEXPAND() and TABPACK()
 *
 * Copyright 2002 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://harbour-project.org
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

#include "ct.ch"

PROCEDURE Main()

   LOCAL cStr, nLen

   ctinit()

   ? "Begin test of TABEXPAND()"
   ?

   // simple tests
   ? "Simple tests:"
   ? [  tabexpand("-"+chr(9)+"!")             == "-       !" ? -> "] + tabexpand( "-" + Chr( 9 ) + "!" )            + ["]
   ? [  tabexpand("----"+chr(9) +"!")         == "----    !" ? -> "] + tabexpand( "----" + Chr( 9 ) + "!" )         + ["]
   ? [  tabexpand("-"+chr(9)+"!",, "+")       == "-+++++++!" ? -> "] + tabexpand( "-" + Chr( 9 ) + "!",, "+" )      + ["]
   ? [  tabexpand("-"+chr(9)+ "!", 4)         == "-   !"     ? -> "] + tabexpand( "-" + Chr( 9 ) + "!", 4 )         + ["]
   ? [  tabexpand("----"+chr(9)+ "!", 8)      == "----    !" ? -> "] + tabexpand( "----" + Chr( 9 ) + "!", 8 )      + ["]
   ? [  tabexpand("----"+chr(9)+ "!", 8, "+") == "----++++!" ? -> "] + tabexpand( "----" + Chr( 9 ) + "!", 8, "+" ) + ["]
   ?

   ? "Tests with newline characters: ^J == LF, ^M == CR"
   cStr := hb_eol()
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   ? [  hb_eol() = "] + cStr + ["]
   cStr := tabexpand( "-" + Chr( 9 ) + "!" + hb_eol() + "----" + Chr( 9 ) + "!", , "+" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   ? [  tabexpand("-"+chr(9)+"!"+hb_eol()+"----"+chr(9)+ "!",, "+")]
   ? [     == "-+++++++!"+hb_eol()+"----++++!"  ? -> "] + cStr + ["]
   cStr := tabexpand( "-" + Chr( 9 ) + "!$$--" + hb_eol() + "--" + Chr( 9 ) + "!", , "+", "$" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   ? [  tabexpand("-"+chr(9)+"!$$--"+hb_eol()+--"+chr(9)+ "!",, "+", "$")]
   nLen := Len( hb_eol() )
   ? [     == "-+++++++!$$--"+hb_eol()+"] + Replicate( "-",4 - nLen ) + [++!"  ? -> "] + cStr + ["]
   ?

   ? "Tests with tab characters:"
   ? [  tabexpand("-"+chr(9)+"-",,"+")      == "-+++++++-" ? -> "] + tabexpand( "-" + Chr( 9 ) + "-",, "+" ) + ["]
   ? [  tabexpand("-"+chr(9)+"-",,"+",,"-")]
   ? [                              == "++++++++^I+++++++" ? -> "] + StrTran( tabexpand( "-" + Chr( 9 ) + "-",, "+",, "-" ), Chr( 9 ), "^I" ) + ["]
   ?

   ? "End test of TABEXPAND()"
   ? "Press any key to continue with tests of TABPACK()..."
   ?
   Inkey( 0 )

   ? "Begin test of TABPACK()"
   ?

   // simple tests
   ? "Simple tests: ^I == tab character"

   ? [  tabpack("AAAAAAA*",, "*")   == "AAAAAAA*"  ? -> "] + StrTran( tabpack("AAAAAAA*",, "*" ), Chr( 9 ), "^I" )  + ["]
   ? [  tabpack("AAAAA***",, "*")   == "AAAAA^I"   ? -> "] + StrTran( tabpack("AAAAA***",, "*" ), Chr( 9 ), "^I" )  + ["]
   ? [  tabpack("AAAAA*****",, "*") == "AAAAA^I**" ? -> "] + StrTran( tabpack("AAAAA*****",, "*" ), Chr( 9 ), "^I" ) + ["]
   ?

   ? "Tests with newline characters:"
   cStr := hb_eol()
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   ? [  hb_eol() = "] + cStr + ["]

   cStr := "ABCD+" + hb_eol() + "++---+++++"
   cStr := tabpack( cStr, 4, "+" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   cStr := StrTran( cStr, Chr( 9 ), "^I" )
   ? [  tabpack("ABCD+" + hb_eol() + "++---+++++", 4, "+")]
   ? [     == "ABCD+"+hb_eol()+"++---"+chr(9)+"++" ? -> "] + cStr + ["]

   ? "End test of TABPACK()"
   ?

// ? "Test with a MEMOEDITed string:"
// ? "  Now, a memoedit() will start. Please type a text, use tab characters"
// ? "  and make sure, you make use of soft and hard returns !"
// ? "  ...press any key to start the memoedit now..."
// ?
// Inkey( 0 )
// CLS
// DispBox( 0, 0, 20, 60 )
// cStr := MemoEdit( , 1, 1, 9, 59,,, 59 )
// CLS
// ? "  Now printing the expanded text using a tab length of 4 and soft CRs"
// cStr1 := tabexpand( cStr, 4, "+",,, .F. )
// cStr1 := StrTran( cStr, Chr( 141 ), hb_eol() )
//
// FOR ni := 1 TO MLCount( cStr1, 59, 4, .T. )
//    ? "  " + Str( ni ) + ": " + MemoLine( cStr1, 59, ni, 4, .T. )
// NEXT
//
// QOut("  Now printing the expanded text using a tab length of 4 but without soft CRs")
// cStr1 := tabexpand( cStr, 4, "+",,, .T. )
//
// FOR ni := 1 TO MLCount( cStr1, 59, 4, .T. )
//    ? "  " + Str( ni ) + ": " + MemoLine( cStr1, 59, ni, 4, .T. )
// NEXT
// Inkey( 0 )

   ctexit()

   RETURN
