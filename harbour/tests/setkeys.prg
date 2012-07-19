/*
 * $Id$
 */

/*
 * Harbour Project source code
 * This file contains the Harbour function(s) that maintain the list
 * of set-keys (hot-keys).
 *
 * By: April White <april@users.sourceforge.net>
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

   V 1.0  April White            Initial version
   V 1.1  April White            Add a Help() function to test default F1
*/

#include "inkey.ch"

PROCEDURE Main()

   LOCAL GetList := {}
   LOCAL alpha, bravo, charlie, k, l
   LOCAL F8Active := .T.

   CLS

   @ 2, 2 SAY "Press F10 to popup alert box of current get, not active if empty"
   @ 3, 2 SAY "Press F9 to disable all setkeys, except F9 to restore (uses SetKeySave())"
   @ 4, 2 SAY "Press F8 to test setkey w/ array, SetKeyCheck(), and SetKeyGet()"
   @ 5, 2 SAY "Press F7 to active/deactive F8"

   alpha   := "alpha    "
   bravo   := 123
   charlie := Date()

   @ 10, 10 GET alpha
   @ 11, 10 GET bravo
   @ 12, 10 GET charlie

#ifndef K_F10

#define K_F10 -9
#define K_F9  -8
#define K_F8  -7
#define K_ESC 27

#endif

   SetKey( K_F10, {|| Alert( Transform( GetActive():varGet(), NIL ) ) }, ;
      {|| !Empty( GetActive():VarGet() ) } )  /* :buffer */
   SetKey( K_F9 , {|| k := hb_SetKeySave( NIL ), ;
      SetKey( K_F9, {|| hb_SetKeySave( k ) } ) } )
   SetKey( K_F8 , {|| SubMain() }, {|| F8Active } )
   SetKey( K_F7 , {|| F8Active := ! F8Active } )

   READ
   ? alpha, bravo, charlie

   RETURN

STATIC PROCEDURE SubMain()

   LOCAL n
   LOCAL bF8Action, bF8Active

   bF8Action := hb_SetKeyGet( K_F8, @bF8Active )
   SetKey( K_F8, NIL )

   hb_SetKeyArray( { 49, 50, 52, 53 }, {| x | QOut( Chr( x ) ) } )
   DO WHILE ( n := Inkey( 0 ) ) != K_ESC
      IF hb_SetKeyCheck( n, ProcName(), ProcLine(), ReadVar() )
         QQOut( " hit hot" )
      ELSE
         QOut( Chr( n ) )
         QQOut( " hit cold" )
      ENDIF
   end

   hb_SetKeyArray( { 49, 50, 52, 53 }, NIL )
   SetKey( K_F8, bF8Action, bF8Active )

   RETURN

PROCEDURE Help( cProc, nLine, cVar )

   LOCAL nX := Col(), nY := Row()

   @ 19, 19 SAY "Pcount: " ; ?? PCount()
   @ 20, 10 SAY "Proc  : " ; ?? cProc
   @ 21, 10 SAY "Line  : " ; ?? nLine
   @ 22, 10 SAY "Var   : " ; ?? cVar

   SetPos( nX, nY )

   RETURN
