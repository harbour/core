/*
 * $Id$

   Harbour Project source code - http://www.Harbour-Project.org

   By: A White - awhite@user.rose.com

   This file contains the Harbour function(s) that maintain the list
   of set-keys (hot-keys).

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
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.0  A White                 Initial version, submitted to Harbour Project
   V 1.1  A White                 Add a Help() function to test default F1
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
    SetKey( K_F7 , {|| F8Active := .not. F8Active } )

    read
    ? alpha, bravo, charlie


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
