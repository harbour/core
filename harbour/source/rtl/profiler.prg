/*
 * Harbour Project source code:
 * profiler
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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

#define CRLF HB_OsNewLine()

function Profiler()

   local cText := "          *** Harbour profiler report ***" + CRLF + CRLF + ;
                  "   CLASSES                   CALLS      CONSUMED TIME" + CRLF + ;
                  "=====================================================" + CRLF
   local n := 1, m
   local cClass, aInfo, aMethodInfo

   while ! Empty( cClass := __ClassName( n ) )
      cText += CRLF + "   CLASS " + cClass + CRLF
      aInfo = ASort( __ClassSel( n ) )
      for m = 1 to Len( aInfo )
         if ! Empty( aInfo[ m ] ) // why __ClassSel() returns empty strings ?
            aMethodInfo = __GetMsgPrf( n, aInfo[ m ] )
            cText += "      " + PadR( aInfo[ m ], 20 ) + ;
                     Str( aMethodInfo[ 1 ], 7 ) + Str( aMethodInfo[ 2 ], 17 ) + CRLF
         endif
      next
      n++
   end

   MemoWrit( "profiler.txt", cText )

return nil