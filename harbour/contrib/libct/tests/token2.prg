/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 functions 
 *     - TOKENINIT()
 *     - TOKENEXIT()
 *     - TOKENNEXT()
 *     - TOKENNUM()
 *     - TOKENAT()
 *     - SAVETOKEN()
 *     - RESTTOKEN()
 *     - TOKENEND()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "../ct.ch"

procedure main

local cStr1 := "A,BB,CCC,DDDD,EEEEE,FFFFFF"
local cStr2 := "ZZZZZZ,YYYYY,XXXX,WWW,VV,U"
local cStr3 := "0123456789ABCDEFGHIJKLM"
local cStr4 := "08:09:10:11:12"
local cStr5 := "05:00+20:00+35:00+50:00"
local cStr6

local cTE1, cTE2

 ctinit()

 qout ("Begin test of incremental tokenizer function family")
 qout ("")

 // Some simple tests with global token environment
 qout ([  Incremental tokenizing the string "]+cStr1+["])
 qout ([    tokeninit (@cStr1, ",", 1) == .T. ? ----> ] + ltoc(tokeninit (@cStr1, ",", 1)))
 qout ([    tokennum () == 6 ? ---------------------> ] + str(tokennum ()))
 qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 while (!tokenend())
   qout ([      tokennext (@cStr1)  ------------------> "] + tokennext(@cStr1)+["])
   qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 enddo                                           
 qout ()
 qout ([    rewind with tokeninit () == .T. ? ------> ] + ltoc(tokeninit ()))
 qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 while (!tokenend())
   qout ([      tokennext (@cStr1)  ------------------> "] + tokennext(@cStr1)+["])
   qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 enddo                                           
 qout ()
 qout ([    access tokens directly with tokennext])
 qout ([      tokennext (@cStr1,2) == "BB" ? -------> "] + tokennext(@cStr1,2)+["])
 qout ([      tokennext (@cStr1,4) == "DDDD" ? -----> "] + tokennext(@cStr1,4)+["])
 qout ()

 qout ("...Press any key...")
 qout ()
 inkey (0)

 qout ([  Incremental tokenizing the string "]+cStr3+[" with the])
 qout ([  token environment of cStr1 !])
 qout ([    rewind with tokeninit () == .T. ? ------> ] + ltoc(tokeninit ()))
 qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 while (!tokenend())
   qout ([      tokennext (@cStr3)  ------------------> "] + tokennext(@cStr3)+["])
   qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 enddo                                           
 qout ()
 qout ([    rewind with tokeninit () == .T. ? ------> ] + ltoc(tokeninit ()))
 qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 while (!tokenend())
   qout ([      start & end with tokenat(.F./.T.)-----> ] + str(tokenat())+[ ]+str(tokenat(.T.)))
   tokennext(@cStr1) 
   qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 enddo                                           
 qout ()
 qout ([    access tokens directly with tokenat])
 qout ([      tokenat (.F.,2) == 3 ? ---------------> ] + str(tokenat(.F.,2)))
 qout ([      tokenat (.T.,4) == 14 ? --------------> ] + str(tokenat(.T.,4)))
 qout()

 qout ("...Press any key...")
 qout ()
 inkey (0)

 qout ([  Save global token environment with savetoken])
 cTE1 := savetoken()
 qout ([    tokeninit a different string, cStr4 := "]+cStr4+[", with tokeninit()])
 qout ([    tokeninit (@cStr4, ":", 1) == .T. ? ----> ] + ltoc(tokeninit (@cStr4, ":", 1)))
 qout ([    tokennum () == 5 ? ---------------------> ] + str(tokennum ()))
 qout ([    tokennext() == "08" ? ------------------> "]+ tokennext (@cStr4)+["])
 qout ([    Now restore global token environment with resttoken and rewind it])
 resttoken (cTE1)
 tokeninit()
 qout ([    tokennum () == 6 ? ---------------------> ] + str(tokennum ()))
 qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 while (!tokenend())
   qout ([      tokennext (@cStr1)  ------------------> "] + tokennext(@cStr1)+["])
   qout ([      tokenend() ? -------------------------> ] + ltoc (tokenend()))
 enddo                                           
 qout ([  Release global TE with tokenexit () ----> ] + ltoc(tokenexit()))
 qout ()

 qout ("...Press any key...")
 qout ()
 inkey (0)

 qout ([  Now tokenize cStr4 := "]+cStr4+[" and])
 qout ([  cStr5 := "]+cStr5+["])
 qout ([  and store the token environment locally to cTE1 and cTE2:])
 qout ([    tokeninit (@cStr4, ":", 1, @cTE1) == .T. ? -> ] + ltoc(tokeninit (@cStr4, ":", 1, @cTE1)))
 qout ([    tokeninit (@cStr5, "+", 1, @cTE2) == .T. ? -> ] + ltoc(tokeninit (@cStr5, "+", 1, @cTE2)))
 qout ([    tokennum (@cTE1) == 5 ? --------------------> ] + str(tokennum (@cTE1)))
 qout ([    tokennum (@cTE2) == 4 ? --------------------> ] + str(tokennum (@cTE2)))
 qout ([        tokenend (@cTE1) ? ---------------------> ] + ltoc (tokenend (@cTE1)))
 qout ([        tokenend (@cTE2) ? ---------------------> ] + ltoc (tokenend (@cTE2)))
 while (!tokenend (@cTE1) .AND. !tokenend (@cTE2))
   qout ([      next train at ]+tokennext (cStr4,,@cTE1)+":"+tokennext (cStr5,,@cTE2))
   qout ([          compiled with tokennext (cStr4,,@cTE1)+":"+tokennext (cStr5,,@cTE2)])
   qout ([        tokenend (@cTE1) ? ---------------------> ] + ltoc (tokenend (@cTE1)))
   qout ([        tokenend (@cTE2) ? ---------------------> ] + ltoc (tokenend (@cTE2)))
 enddo

 qout ("")
 qout ("End test of incremental tokenizer function family")
 qout ()

 qout ("...Press any key...")
 qout ()
 inkey (0)

 ctexit()

return 



