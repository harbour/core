/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ACHOICE() test
 *
 * Copyright 2009 Vladislav Lavrecky <lavr / at / ldz.lv>
 * www - http://www.harbour-project.org
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

function main()

local aMenu1 := {"-Visky-"}
local aMenu2 := {"-Vodka-"}
local aMenu3 := {"-Grapa-"}
local lExit := .F.
local nCounter := 1
local nKeyPressed

setcolor("W+/N, BG+/B, , , W/N")
cls
@ 2,1 SAY "-Visky- -Vodka- -Grapa-"
do while !lExit

 do case
    case nCounter == 1
      achoice(2, 1, 2, 7, aMenu1)
    case nCounter == 2
      achoice(2, 9, 2, 15, aMenu2)
    case nCounter == 3
      achoice(2, 17, 2, 23, aMenu3)
 endcase

 nKeyPressed := lastkey()
 if nKeyPressed == K_ESC
   lExit := .T.
 elseif nKeyPressed == K_RIGHT
   nCounter := iif(nCounter == 3, 1, nCounter+1)
 elseif nKeyPressed == K_LEFT
   nCounter := iif(nCounter == 1, 3, nCounter-1)
 endif

enddo

return NIL
