/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ACHOICE() test
 *
 * Copyright 2009 Vladislav Lavrecky <lavr / at / ldz.lv>
 * www - http://harbour-project.org
 *
 */

/*
 * Menu Navigation  - Right and Left arrows keys, ESC to exit
 * After some Right or Left arrows preses
 * in Harbour app all menu items are highlighted
 * an no way to understand which  element is current,
 * with clipper app all is ok.
 */
#include "inkey.ch"
#include "achoice.ch"

function main()

//NIL, empty, numeric, and "not handled" - items
//must be inaccesible and invisible
local aMenu1 := {" --Visky--", "", "not handled"}
local aMenu2 := {" --Vodka--", " --Water--", NIL, "not handled"}
local aMenu3 := {" --Grapa--", 33, "not handled"}

//for AC_NOITEM mode test
local aMenu4 := {"","not handled"}


local lExit := .F.
local nCounter := 1
local nKeyPressed

//set to True for items (de)highlighting
//algoritm in clipper
public lHiLiTest := .F.


setcolor("W+/N, BG+/B, , , W/N")
cls
@ 2,1 SAY " --Visky--   --Vodka--   --Grapa--"
@ 3,14 SAY "--Water--"
do while !lExit

do case
  case nCounter == 1
    achoice(2, 1, 3, 11, aMenu1)
  case nCounter == 2
    achoice(2, 13, 3, 23, aMenu2, .T., "cUF")
  case nCounter == 3
    achoice(2, 25, 3, 35, aMenu3, .T.)
  case nCounter == 4
//User function cUF2() fill screen with exclamation marks
//in clipper it doe's not called in AC_NOITEM mode
    achoice(2, 37, 3, 47, aMenu4, .T., "cUF2")

endcase

nKeyPressed := lastkey()
if nKeyPressed == K_ESC
 lExit := .T.
elseif nKeyPressed == K_RIGHT
 nCounter := iif(nCounter == 4, 1, nCounter+1)
elseif nKeyPressed == K_LEFT
 nCounter := iif(nCounter == 1, 4, nCounter-1)
endif

enddo

return NIL

//Test for current and previous items
//highliting-dehighliting algoritm
function cUF( nMode, nCurElement, nRowPos )


local nRetVal := AC_CONT
local nKey := LASTKEY()

if lHiLiTest

 dispbox( 0, 0, maxrow(), maxcol(), repl("#",9), "GR+/G" )
endif


if nMode == AC_NOITEM
 nRetVal := AC_ABORT
elseif nMode == AC_EXCEPT
 do case
   case nKey == K_RETURN
     nRetVal := AC_SELECT
   otherwise
     nRetVal := AC_ABORT
 endcase
endif

return nRetVal


//test for AC_NOITEM mode
//Clipper in AC_NOITEM mode do not call User Function
function cUF2( nMode, nCurElement, nRowPos )


local nRetVal := AC_CONT
local nKey := LASTKEY()

dispbox( 0, 0, maxrow(), maxcol(), repl("!",9), "GR+/G" )

if nMode == AC_NOITEM
 nRetVal := AC_ABORT
elseif nMode == AC_EXCEPT
 do case
   case nKey == K_RETURN
     nRetVal := AC_SELECT
   otherwise
     nRetVal := AC_ABORT
 endcase
endif

return nRetVal
