/*
 * $Id$
 */
#include "hbgetcmt.ch"

function Main
Local lx :=.f.
local ly :=.f.
Local citem:="Windows NT/2000"
Local aitems[4]
aitems[1]:=RADIOBUTTO( 3,3,"&Windows NT/2000")
aitems[2]:=RADIOBUTTO( 4,3,"W&indows 9x")
aitems[3]:=RADIOBUTTO( 5,3,"&Linux")
aitems[4]:=RADIOBUTTO( 6,3,"&Mac OS")

cls
Setcolor('w/b+,r/b,g+/r,b+/r+,bg/n+,w/bg,rb/bg')
@  2,2,7,40 get citem radiogroup aitems color 'w/b+,r/b,g/b+' MESSAGE "Select Your Os"
@ 8,3 Say "Married"
@ 8,12 Get lx CHECKBOX color 'w/b+,w/b,w+/r,w/g+' MESSAGE "Is You Married?"
@ 9,3 Say "Singer"
@ 9,12 Get ly CHECKBOX color 'w/b+,w/b,w+/r,w/g+' MESSAGE "Are You a  Singer"
read MSG AT maxrow(), 0, maxcol() MSG Color "w/b+"
? "Is the Person Married",if(lx," Yes ", " No ")
? "Is the Person a Singer",if(ly," Yes ", " No ")
? "Your Os is ",cItem
return Nil
