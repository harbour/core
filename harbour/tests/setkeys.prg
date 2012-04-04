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

Procedure Main()
  local GetList := {}
  local alpha, bravo, charlie, k, l
  local F8Active := .t.

  cls

  @ 2, 2 say "Press F10 to popup alert box of current get, not active if empty"
  @ 3, 2 say "Press F9 to disable all setkeys, except F9 to restore (uses SetKeySave())"
  @ 4, 2 say "Press F8 to test setkey w/ array, SetKeyCheck(), and SetKeyGet()"
  @ 5, 2 say "Press F7 to active/deactive F8"

  alpha   := "alpha    "
  bravo   := 123
  charlie := date()

  @ 10, 10 get alpha
  @ 11, 10 get bravo
  @ 12, 10 get charlie

  #ifndef K_F10

    #define K_F10 -9
    #define K_F9  -8
    #define K_F8  -7
    #define K_ESC 27

  #endif

  setKey( K_F10, {|| Alert( transform( getactive():varGet(), NIL ) ) }, ;
                 {|| !empty( getactive():VarGet() ) } )  /* :buffer */
  setKey( K_F9 , {|| k := hb_SetKeySave( NIL ), ;
                     SetKey( K_F9, {|| hb_SetKeySave( k ) } ) } )
  SetKey( K_F8 , {|| SubMain() }, {|| F8Active } )
  SetKey( K_F7 , {|| F8Active := ! F8Active } )

  read
  ? alpha, bravo, charlie

  return

static Procedure SubMain()
  local n
  local bF8Action, bF8Active

  bF8Action := hb_SetKeyGet( K_F8, @bF8Active )
  SetKey( K_F8, NIL )

  hb_SetKeyArray( { 49, 50, 52, 53 }, {|x| qout( chr( x ) ) } )
  do while ( n := inkey( 0 ) ) != K_ESC
    if hb_SetKeyCheck( n, procname(),procline(), readvar() )
      qqout( " hit hot" )
    else
      qout( chr( n ) )
      qqout( " hit cold" )
    endif
  end

  hb_SetKeyArray( { 49, 50, 52, 53 }, NIL )
  SetKey( K_F8, bF8Action, bF8Active )

  return

Procedure Help( cProc, nLine, cVar )
  local nX := col(), nY := row()

  @ 19, 19 say "Pcount: " ; ?? pcount()
  @ 20, 10 say "Proc  : " ; ?? cProc
  @ 21, 10 say "Line  : " ; ?? nLine
  @ 22, 10 say "Var   : " ; ?? cVar

  SetPos( nX, nY )

  return
