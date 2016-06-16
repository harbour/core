/*
 * Test CT3 functions AfterAtNum()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "ct.ch"

PROCEDURE Main()

   LOCAL cStr := "...This...is...a...test!"

   ? "Begin test of AfterAtNum()"
   ?
   ? "  Value of cStr is:", '"' + cStr + '"'
   ?

   ? "  Simple tests:"
   ? '  AfterAtNum( "..", cStr )       should be "test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr ) + '"'
   ? '  AfterAtNum( "..", cStr, 2 )    should be ".is...a...test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr, 2 ) + '"'
   ? '  AfterAtNum( "..", cStr, 2, 2 ) should be ".a...test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr, 2, 2 ) + '"'
   ?

   ? "  Multi-Pass tests"
   ? "  Setting CSetAtMupa() to .T."
   CSetAtMupa( .T. )
   ? '  AfterAtNum( "..", cStr )       should be "test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr ) + '"'
   ? '  AfterAtNum( "..", cStr, 2 )    should be "This...is...a...test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr, 2 ) + '"'
   ? '  AfterAtNum( "..", cStr, 2, 2 ) should be "is...a...test!",'
   ? '                                    and is', '"' + AfterAtNum( "..", cStr, 2, 2 ) + '"'
   ? "  Setting CSetAtMupa() to .F."
   CSetAtMupa( .F. )
   ?

   ? "  SetAtLike tests"
   ? '  Setting SetAtLike( CT_SETATLIKE_WILDCARD, "." )'
   SetAtLike( CT_SETATLIKE_WILDCARD, "." )
   ? '  AfterAtNum( "..", cStr )        should be "",'
   ? '                                     and is', '"' + AfterAtNum( "..", cStr ) + '"'
   ? '  AfterAtNum( "..", cStr, 2, 2 )  should be "s...is...a...test!",'
   ? '                                     and is', '"' + AfterAtNum( "..", cStr, 2, 2 ) + '"'
   ? '  AfterAtNum( "..", cStr, 2, 10 ) should be ".a...test!",'
   ? '                                     and is', '"' + AfterAtNum( "..", cStr, 2, 10 ) + '"'
   ?

   ? "End test of AfterAtNum()"
   ?

   RETURN
