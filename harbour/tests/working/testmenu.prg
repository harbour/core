//
// $Id$
//

// Harbour menuto

#command @ <row>, <col> PROMPT <prompt> [MESSAGE <msg>]                 ;
      => __AtPrompt( <row>, <col>, <prompt> , <msg> )

#command MENU TO <v>                                                    ;
      => <v> := __MenuTo( {|_1| if(PCount() == 0, <v>, <v> := _1)}, #<v> )

#include "box.ch"
********************************************************************************
function main()
********************************************************************************
local nchoice
local amenu := {"Option ~One","Option ~Two","Option T~hree","Option ~Four"}
local i
local ccolor
local scr := savescreen(0,0,maxrow(),maxcol())

set wrap on
set message to 24

scroll()

ccolor := setcolor("w+/n,w+/b,b/n,w+/n,gr+/n")

dispbox( 10, 13, 10 + len( amenu) + 1, 30, B_SINGLE + ' ' )

for i := 1 to len( amenu )
   @10+i,15 prompt amenu[i] message "This Is " + strtran(amenu[i],"~","")
next

menu to nchoice

qout("You Have Chosen Option " + str(nchoice,1) + " !")

inkey(0)

setcolor( ccolor )

restscreen(0,0,maxrow(),maxcol(),scr)

return nil
