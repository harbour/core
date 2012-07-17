/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 function ADDASCII()
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

   LOCAL cStr := "This is a test!"

   ctinit()

   QOut( "Begin test of ADDASCII()" )
   QOut( "" )
   // simple tests
   QOut( "Simple tests:" )
   QOut( [  This should be "1000": ] + addascii( "0000", 1, 1 ) )
   QOut( [  This should be "0001": ] + addascii( "0000", 1 ) )
   QOut( [  This should be "BAAA": ] + addascii( "AAAA", - 255, 1 ) )
   QOut( [  This should be "AAAB": ] + addascii( "AAAA", - 255 ) )

   // csetref() tests
   QOut()
   QOut( "CSETREF tests:" )
   QOut( "  current csetref setting(should be .f.)................: ", csetref() )
   QOut( "  return value of addascii([A],1,1) call(should be 'B'): ", addascii( "A",1,1 ) )
   QOut( "  value of cStr..........................................: ", cStr )
   QOut( "  return value of addascii(cStr,1,1) call...............: ", addascii( cStr,1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of addascii(@cStr,1,1) call..............: ", addascii( @cStr,1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of addascii(@cStr,-1,1) call.............: ", addascii( @cStr, - 1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of csetref(.t.)..........................: ", csetref( .T. ) )
   QOut( "  return value of addascii([A],1,1) call................: ", addascii( "A",1,1 ) )
   QOut( "  return value of addascii(cStr,1,1) call...............: ", addascii( cStr,1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of addascii(@cStr,1,1) call..............: ", addascii( @cStr,1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of addascii(@cStr,-1,1) call.............: ", addascii( @cStr, - 1,1 ) )
   QOut( "  value of cStr is now...................................: ", cStr )
   QOut( "  return value of csetref(.f.)..........................: ", csetref( .F. ) )

   // tests for the new 4th parameter
   QOut()
   QOut( "Carryover tests(new 4th parameter):" )
   QOut( "  return value of addascii([AAAA],1,2,.T.) call('ABAA')....:", addascii( "AAAA",1,2, .T. ) )
   QOut( "  return value of addascii([AAAA],257,2,.T.) call('BBAA')..:", addascii( "AAAA",257,2, .T. ) )
   QOut( "  return value of addascii([AAAA],257,2,.F.) call('ABAA')..:", addascii( "AAAA",257,2, .F. ) )
   QOut( "  return value of addascii([AAAA],258,,.T.) call('AABC')...:", addascii( "AAAA",258,, .T. ) )
   QOut( "  return value of addascii([ABBA],-257,3,.T.) call('AAAA').:", addascii( "ABBA", - 257,3, .T. ) )

   QOut( "End test of ADDASCII()" )
   QOut( "" )

   ctexit()

   RETURN
