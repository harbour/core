/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    hb_Decode() function test
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#require "xhb"

PROCEDURE Main()

   LOCAL aArray

   ? "hb_Decode() FUNCTION TESTS"

   // Single, return empty value
   ? hb_Decode( 10 )
   // A list
   ? hb_Decode( 3, 1, "A", 2, "B", 3, "C" )
   // A list with default
   ? hb_Decode( 4, 1, "A", 2, "B", 3, "C", "X" )
   // Using an array as list of values to check
   ? hb_Decode( 2, { 1, "A", 2, "B", 3, "C" } )
   // Using an array with default as list of values to check
   ? hb_Decode( 2, { 1, "A", 2, "B", 3, "C", "X" } )
   // Using an hash as list
   ? hb_Decode( 2, { 1 => "A", 2 => "B", 3 => "C" } )

   // Returning a codeblock
   ? hb_CStr( hb_Decode( 2, 1, {|| 1 }, 2, {|| 2 }, 3, {|| 3 } ) )

   // Checking an array
   aArray := { 1 }
   ? hb_Decode( aArray, aArray, "A", { 2 }, "B", { 3 }, "C" )

   RETURN
