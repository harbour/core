/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 function CHARNOT
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

#include "ct.ch"

PROCEDURE Main()

   LOCAL ni, cStr

   ctinit()

   ? "Begin test of CHARNOT()"
   ?

   // simple tests
   ? "Simple tests:"

   ? [  charnot(chr(85)+chr(128)+chr(170)+chr(1)) == ]
   ? [  chr(170)+chr(127)+chr(85)+chr(254) ? -->]

   ? [  ]
   cStr :=  charnot( Chr( 85 ) + Chr( 128 ) + Chr( 170 ) + Chr( 1 ) )
   for ni := 1 TO Len( cStr )
      Q? "chr(" + AllTrim( Str(Asc(SubStr(cStr, ni, 1 ) ) ) ) + ")"
      IF ni < Len( cStr )
         Q? "+"
      ENDIF
   next ni
   ?

   ? [  charnot(charnot("This is a test!")) == "This is a test!" ?]
   ? [  --> "] + charnot( charnot("This is a test!" ) ) + ["]
   ?

   ? "End test of CHARNOT()"
   ?

   ctexit()

   RETURN
