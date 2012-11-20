/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 functions
 *     - TokenInit()
 *     - TokenExit()
 *     - TokenNext()
 *     - TokenNum()
 *     - TokenAt()
 *     - SaveToken()
 *     - RestToken()
 *     - TokenEnd()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
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

#ifdef __HARBOUR__
#require "hbct"
#endif

PROCEDURE Main()

   LOCAL cStr1 := "A,BB,CCC,DDDD,EEEEE,FFFFFF"

   // LOCAL cStr2 := "ZZZZZZ,YYYYY,XXXX,WWW,VV,U"
   LOCAL cStr3 := "0123456789ABCDEFGHIJKLM"
   LOCAL cStr4 := "08:09:10:11:12"
   LOCAL cStr5 := "05:00+20:00+35:00+50:00"

   LOCAL cTE1, cTE2

   ctinit()

   ? "Begin test of incremental tokenizer function family"
   ?

   // Some simple tests with global token environment
   ? '  Incremental tokenizing the string "' + cStr1 + '"'
   ? '    TokenInit(@cStr1, ",", 1) == .T. ? ----> ' + LToC( TokenInit( @cStr1, ",", 1 ) )
   ? '    TokenNum() == 6 ? ---------------------> ' + Str( TokenNum() )
   ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   WHILE ! TokenEnd()
      ? '      TokenNext(@cStr1)  ------------------> "' + TokenNext( @cStr1 ) + '"'
      ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   ENDDO
   ?
   ? '    rewind with TokenInit() == .T. ? ------> ' + LToC( TokenInit() )
   ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   WHILE ! TokenEnd()
      ? '      TokenNext(@cStr1)  ------------------> "' + TokenNext( @cStr1 ) + '"'
      ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   ENDDO
   ?
   ? '    access tokens directly with tokennext'
   ? '      TokenNext(@cStr1,2) == "BB" ? -------> "' + TokenNext( @cStr1, 2 ) + '"'
   ? '      TokenNext(@cStr1,4) == "DDDD" ? -----> "' + TokenNext( @cStr1, 4 ) + '"'
   ?

   ? "...Press any key..."
   ?
   Inkey( 0 )

   ? '  Incremental tokenizing the string "' + cStr3 + '" with the'
   ? '  token environment of cStr1 !'
   ? '    rewind with TokenInit() == .T. ? ------> ' + LToC( TokenInit() )
   ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   WHILE ! TokenEnd()
      ? '      TokenNext(@cStr3)  ------------------> "' + TokenNext( @cStr3 ) + '"'
      ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   ENDDO
   ?
   ? '    rewind with TokenInit() == .T. ? ------> ' + LToC( TokenInit() )
   ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   WHILE ! TokenEnd()
      ? '      start & end with TokenAt(.F./.T.)-----> ' + Str( TokenAt() ) + ' ' + Str( TokenAt( .T. ) )
      TokenNext( @cStr1 )
      ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   ENDDO
   ?
   ? '    access tokens directly with tokenat'
   ? '      TokenAt( .F., 2 ) == 3 ? ---------------> ' + Str( TokenAt( .F., 2 ) )
   ? '      TokenAt( .T., 4 ) == 14 ? --------------> ' + Str( TokenAt( .T., 4 ) )
   ?

   ? "...Press any key..."
   ?
   Inkey( 0 )

   ? '  Save global token environment with savetoken'
   cTE1 := SaveToken()
   ? '    tokeninit a different string, cStr4 := "' + cStr4 + '", with TokenInit()'
   ? '    TokenInit( @cStr4, ":", 1 ) == .T. ? ----> ' + LToC( TokenInit( @cStr4, ":", 1 ) )
   ? '    TokenNum() == 5 ? ---------------------> ' + Str( TokenNum() )
   ? '    TokenNext() == "08" ? ------------------> "' + TokenNext( @cStr4 ) + '"'
   ? '    Now restore global token environment with resttoken and rewind it'
   RestToken( cTE1 )
   TokenInit()
   ? '    TokenNum() == 6 ? ----------------------> ' + Str( TokenNum() )
   ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   WHILE ! TokenEnd()
      ? '      TokenNext( @cStr1 ) -----------------> "' + TokenNext( @cStr1 ) + '"'
      ? '      TokenEnd() ? -------------------------> ' + LToC( TokenEnd() )
   ENDDO
   ? '  Release global TE with TokenExit() ----> ' + LToC( TokenExit() )
   ?

   ? "...Press any key..."
   ?
   Inkey( 0 )

   ? '  Now tokenize cStr4 := "' + cStr4 + '" and'
   ? '  cStr5 := "' + cStr5 + '"'
   ? '  and store the token environment locally to cTE1 and cTE2:'
   ? '    TokenInit( @cStr4, ":", 1, @cTE1 ) == .T. ? -> ' + LToC( TokenInit( @cStr4, ":", 1, @cTE1 ) )
   ? '    TokenInit( @cStr5, "+", 1, @cTE2 ) == .T. ? -> ' + LToC( TokenInit( @cStr5, "+", 1, @cTE2 ) )
   ? '    TokenNum( @cTE1 ) == 5 ? --------------------> ' + Str( TokenNum( @cTE1 ) )
   ? '    TokenNum( @cTE2 ) == 4 ? --------------------> ' + Str( TokenNum( @cTE2 ) )
   ? '        TokenEnd( @cTE1 ) ? ---------------------> ' + LToC( TokenEnd( @cTE1 ) )
   ? '        TokenEnd( @cTE2 ) ? ---------------------> ' + LToC( TokenEnd( @cTE2 ) )
   WHILE ! TokenEnd( @cTE1 ) .AND. ! TokenEnd( @cTE2 )
      ? '      next train at ' + TokenNext( cStr4,, @cTE1 ) + ":" + TokenNext( cStr5,, @cTE2 )
      ? '          compiled with TokenNext( cStr4,, @cTE1 ) + ":" + TokenNext( cStr5,, @cTE2 )'
      ? '        TokenEnd( @cTE1 ) ? ---------------------> ' + LToC( TokenEnd( @cTE1 ) )
      ? '        TokenEnd( @cTE2 ) ? ---------------------> ' + LToC( TokenEnd( @cTE2 ) )
   ENDDO

   ?
   ? "End test of incremental tokenizer function family"
   ?

   ? "...Press any key..."
   ?
   Inkey( 0 )

   ctexit()

   RETURN
