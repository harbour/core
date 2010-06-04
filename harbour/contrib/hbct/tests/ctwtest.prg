/*
 * $Id$
 */

/*
 * Harbour Project source code
 * http://harbour-project.org/
 *
 * Clipper Tool III like window system test program
 * Donated to the public domain on 2006-02-11 by Przemyslaw Czerpak
 */

#define ntrim(n) ltrim(str(n))
#include "inkey.ch"
#include "setcurs.ch"

proc main()
local aWin:=array(9), y, x, i, k, lFlag:=.f., lBoard:=.t.

setblink(.f.)
wboard(5,5,20,75)
wmode(.t.,.t.,.t.,.t.)
wsetshadow(7)
setclearA(10*16+14)
setclearB(35)
dispbox(0,0,maxrow(),maxcol(),repl("#",9),ntocolor(10*16+14))
setpos(0,0)
? "GT driver: "+ HB_GTVERSION()
? HB_GTVERSION(1)
?
? "ESC - quit "
? "0 - select window 0 (base screen) "
? "1-9 select window 1-9 "
? "C - close window "
? "Q - clear screen "
? "P - print text at window 0 "
? "B - board switch "
? "INS - cursor shape "
? "DEL - hide cursor "
? "arrows - window move "

setclearB(61)
for i:=1 to len(aWin)
   y:=i+2
   x:=i*4+10
   setcolor(ntocolor(i*16+15)+",W+/B*")
   wsetshadow(i%8)
   aWin[i]:=wopen(y,x,y+10,x+20)
   wbox()

   @ -1,0 say "TITLE "+ntrim(aWin[i])
   ? ntrim(row())+":"+ntrim(col()),"/",ntrim(maxrow())+":"+ntrim(maxcol()),""
   ? ntrim(wrow())+":"+ntrim(wcol()),"/",ntrim(maxrow(.t.))+":"+ntrim(maxcol(.t.)),""
   ? ntrim(wfrow())+":"+ntrim(wfcol()),"/",;
     ntrim(wflastrow())+":"+ntrim(wflastcol()),""
   ? ntrim(wfrow(.t.))+":"+ntrim(wfcol(.t.)),"/",;
     ntrim(wflastrow(.t.))+":"+ntrim(wflastcol(.t.)),""
   ? "window:",ntrim(aWin[i]),""
   setcursor(int(i%5))

next

dspcord()
while .t.
   k:=inkey(0, INKEY_ALL)
   if k == K_ESC
      exit
   elseif k>=asc("1") .and. k<=asc("9")
      wselect(aWin[k-asc("0")])
   elseif k==asc("0")
      wselect(0)
   elseif k==asc("C") .or. k==asc("c")
      wclose()
   elseif k==asc("Q") .or. k==asc("q")
      clear screen
   elseif k==asc("B") .or. k==asc("b")
      if lBoard
         wboard(0,0,maxrow(.t.)-1,maxcol(.t.))
      else
         wboard(5,5,20,75)
      endif
      lBoard:=!lBoard
   elseif k==asc("P") .or. k==asc("P")
      y:=wfrow()
      x:=wfcol()
      i:=wselect()
      wselect(0)
      @ y,x say "THIS IS WINDOW 0 OUTPUT"
      wselect(i)
   elseif k==K_INS
      lFlag:=!lFlag
      setcursor(iif(lFlag,3,1))
   elseif k==K_DEL
      setcursor(SC_NONE)
   elseif k==K_LEFT
      wmove(wrow(),wcol()-1)
   elseif k==K_RIGHT
      wmove(wrow(),wcol()+1)
   elseif k==K_UP
      wmove(wrow()-1,wcol())
   elseif k==K_DOWN
      wmove(wrow()+1,wcol())
   endif
   dspcord()
enddo
return

static proc dspcord()
local mr:=mrow(), mc:=mcol(), r:=wrow(), c:=wcol(), w:=wselect()
wselect(0)
@ maxrow(), 0 say padr("WPOS("+ltrim(str(r))+","+ltrim(str(c))+")"+;
       iif(MPresent(), "MPOS("+ltrim(str(mr))+","+ltrim(str(mc))+")", ""), maxcol()+1)
wselect(w)
return
