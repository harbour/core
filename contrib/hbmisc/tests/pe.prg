#require "hbmisc"

PROCEDURE Main( cFile )

   LOCAL cText
   LOCAL lEdit := .T.

   IF cFile == NIL
      cText := Example_Text()
   ELSE
      IF hb_FileMatch( cFile, __FILE__ )
         lEdit := .F.
      ENDIF
      cText := MemoRead( cFile )
   ENDIF

   cText := MyMemoEdit( cText, 0, 0, MaxRow(), MaxCol(), lEdit )
   hb_MemoWrit( hb_FNameExtSet( __FILE__, ".out" ), cText )

   RETURN

STATIC FUNCTION MyMemoEdit( cText, nTop, nLeft, nBottom, nRight, lEdit )

   LOCAL oED

   /* NOTE: In current design of editor it doesn't reallocate the memory
      buffer used to hold the text
   */

   oED := EditorNew( nTop, nLeft, nBottom, nRight, 254, , , , Len( cText ) * 2, 168 )
   IF oED != NIL
      EditorSetText( oED, cText )
      EditorEdit( oED, lEdit, .T. )
      cText := EditorGetText( oED )
      EditorKill( oED )
   ELSE
      ? "Editor not created"
   ENDIF

   RETURN cText

STATIC FUNCTION Example_Text()
   RETURN StrTran( Example_Text_Raw(), "~", hb_BChar( 168 ) )

STATIC FUNCTION Example_Text_Raw()
#pragma __cstream | RETURN %s

 ~2THE HARBOUR PROJECT LICENSE~1
~3 ===========================

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version, with one exception:

 The exception is that if you link the Harbour Runtime Library (HRL)
 and/or the Harbour Virtual Machine (HVM) with other files to produce
 an executable, this does not by itself cause the resulting executable
 to be covered by the GNU General Public License. Your use of that
 executable is in no way restricted on account of linking the HRL
 and/or HVM code into it.

 This program is distributed in the hope that it will be useful,
 but ~2WITHOUT ANY WARRANTY~1; without even the implied warranty of
 ~4MERCHANTABILITY~1 or ~5FITNESS FOR A PARTICULAR PURPOSE~1.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 their web site at https://www.gnu.org/).

~5This file have to be separated with CR/LF characters~1
#pragma __endtext
