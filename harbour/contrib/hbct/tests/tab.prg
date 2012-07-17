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

   QOut( "Begin test of TABEXPAND()" )
   QOut( "" )

   // simple tests
   QOut( "Simple tests:" )
   QOut( [  tabexpand("-"+chr(9)+"!")             == "-       !" ? -> "] + tabexpand( "-" + Chr(9 ) + "!" )             + ["] )
   QOut( [  tabexpand("----"+chr(9) +"!")         == "----    !" ? -> "] + tabexpand( "----" + Chr(9 ) + "!" )         + ["] )
   QOut( [  tabexpand("-"+chr(9)+"!",, "+")       == "-+++++++!" ? -> "] + tabexpand( "-" + Chr(9 ) + "!",, "+" )       + ["] )
   QOut( [  tabexpand("-"+chr(9)+ "!", 4)         == "-   !"     ? -> "] + tabexpand( "-" + Chr(9 ) + "!", 4 )         + ["] )
   QOut( [  tabexpand("----"+chr(9)+ "!", 8)      == "----    !" ? -> "] + tabexpand( "----" + Chr(9 ) + "!", 8 )      + ["] )
   QOut( [  tabexpand("----"+chr(9)+ "!", 8, "+") == "----++++!" ? -> "] + tabexpand( "----" + Chr(9 ) + "!", 8, "+" ) + ["] )
   QOut( "" )

   QOut( "Tests with newline characters: ^J == LF, ^M == CR" )
   cStr := hb_eol()
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   QOut( [  hb_eol() = "] + cStr + ["] )
   cStr := tabexpand( "-" + Chr( 9 ) + "!" + hb_eol() + "----" + Chr( 9 ) + "!", , "+" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   QOut( [  tabexpand("-"+chr(9)+"!"+hb_eol()+"----"+chr(9)+ "!",, "+")] )
   QOut( [     == "-+++++++!"+hb_eol()+"----++++!"  ? -> "] + cStr + ["] )
   cStr := tabexpand( "-" + Chr( 9 ) + "!$$--" + hb_eol() + "--" + Chr( 9 ) + "!", , "+", "$" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   QOut( [  tabexpand("-"+chr(9)+"!$$--"+hb_eol()+--"+chr(9)+ "!",, "+", "$")] )
   nLen := Len( hb_eol() )
   QOut( [     == "-+++++++!$$--"+hb_eol()+"] + Replicate( "-",4 - nLen ) + [++!"  ? -> "] + cStr + ["] )
   QOut( "" )

   QOut( "Tests with tab characters:" )
   QOut( [  tabexpand("-"+chr(9)+"-",,"+")      == "-+++++++-" ? -> "] + tabexpand( "-" + Chr(9 ) + "-",,"+" )     + ["] )
   QOut( [  tabexpand("-"+chr(9)+"-",,"+",,"-")] )
   QOut( [                              == "++++++++^I+++++++" ? -> "] + StrTran( tabexpand("-" + Chr(9 ) + "-",,"+",,"-" ),Chr(9 ),"^I" ) + ["] )
   QOut( "" )

   QOut( "End test of TABEXPAND()" )
   QOut( "Press any key to continue with tests of TABPACK()..." )
   QOut( "" )
   Inkey( 0 )

   QOut( "Begin test of TABPACK()" )
   QOut( "" )

   // simple tests
   QOut( "Simple tests: ^I == tab character" )

   QOut( [  tabpack("AAAAAAA*",, "*")   == "AAAAAAA*"  ? -> "] + StrTran( tabpack("AAAAAAA*",, "*" ),Chr(9 ),"^I" )  + ["] )
   QOut( [  tabpack("AAAAA***",, "*")   == "AAAAA^I"   ? -> "] + StrTran( tabpack("AAAAA***",, "*" ),Chr(9 ),"^I" )  + ["] )
   QOut( [  tabpack("AAAAA*****",, "*") == "AAAAA^I**" ? -> "] + StrTran( tabpack("AAAAA*****",, "*" ),Chr(9 ),"^I" ) + ["] )
   QOut( "" )

   QOut( "Tests with newline characters:" )
   cStr := hb_eol()
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   QOut( [  hb_eol() = "] + cStr + ["] )

   cStr := "ABCD+" + hb_eol() + "++---+++++"
   cStr := tabpack( cStr, 4, "+" )
   cStr := StrTran( cStr, Chr( 10 ), "^J" )
   cStr := StrTran( cStr, Chr( 13 ), "^M" )
   cStr := StrTran( cStr, Chr( 9 ), "^I" )
   QOut( [  tabpack("ABCD+" + hb_eol() + "++---+++++", 4, "+")] )
   QOut( [     == "ABCD+"+hb_eol()+"++---"+chr(9)+"++" ? -> "] + cStr + ["] )

   QOut( "End test of TABPACK()" )
   QOut( "" )

   //  qout("Test with a MEMOEDITed string:")
   //  qout("  Now, a memoedit() will start. Please type a text, use tab characters")
   //  qout("  and make sure, you make use of soft and hard returns !")
   //  qout("  ...press any key to start the memoedit now...")
   //  qout("")
   //  inkey(0)
   //  cls
   //  dispbox(0,0,20,60)
   //  cStr := memoedit(, 1, 1, 9, 59,,,59)
   //  cls
   //  qout("  Now printing the expanded text using a tab length of 4 and soft CRs")
   //  cStr1 := tabexpand(cStr,4,"+",,,.F.)
   //  cStr1 := strtran(cStr, chr(141), hb_eol())
   //
   //  for ni := 1 to mlcount(cStr1, 59, 4, .T.)
   //    qout("  "+str(ni)+": "+memoline(cStr1, 59,ni,4,.T.))
   //  next ni
   //
   //  qout("  Now printing the expanded text using a tab length of 4 but without soft CRs")
   //  cStr1 := tabexpand(cStr,4,"+",,,.T.)
   //
   //  for ni := 1 to mlcount(cStr1, 59, 4, .T.)
   //    qout("  "+str(ni)+": "+memoline(cStr1, 59,ni,4,.T.))
   //  next ni
   //  inkey(0)

   ctexit()

   RETURN
