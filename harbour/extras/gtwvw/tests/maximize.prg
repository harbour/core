/*
 * $Id$
 */

/*
   sample on using Maximize Mode in gtwvw.
   budyanto@centrin.net.id
 */

#include "inkey.ch"

* this dimension will be used when user presses RESTORE button
static s_nNormalMaxrow := 24
static s_nNormalMaxcol := 79

STATIC s_lSizeReady := .F.

procedure main
local ch
   * activate WVW_SIZE()
   s_lSizeReady := .T.

   * the biggest possible window
   setcolor("N/W")
   setmode(wvw_maxmaxrow()+1, wvw_maxmaxcol()+1)

   * enable MAXIMIZE button
   wvw_enablemaximize(0, .T.)

   * set the window to MAXIMIZED state
   wvw_maximize(0)

   updatescr()
   do while (ch := inkey(0))!=K_ESC
      * refresh screen, probably in a new dimension
      * (You may alternatively call updatescr() from WVW_SIZE instead)
      updatescr()
   enddo
return

procedure diminfo()
   @ 0, 0 say "Window size: " + alltrim(str(maxrow()+1)) + " x " + alltrim(str(maxcol()+1)) + "   "
return

procedure updatescr()
local i
   CLS
   for i := 0 to maxcol()
      @ 0,i say "T"
   next
   for i := 0 to maxrow()
      @ i,0 say "L"
   next
   for i := 0 to maxcol()
      @ maxrow(),i say "B"
      //@ maxrow()-1,i say right(tran(i,"999"),1)
   next
   for i := 0 to maxrow()
      @ i,maxcol() say "R"
   next
   @ int(maxrow()/2)+0, 2 say padc("Press MAXIMIZE/RESTORE button to change dimension", maxcol()+1-4)
   @ int(maxrow()/2)+1, 2 say padc("Try also changing taskbar size/position", maxcol()+1-4)
   @ int(maxrow()/2)+3, 2 say padc("Press any key to redraw screen", maxcol()+1-4)
   @ int(maxrow()/2)+4, 2 say padc("Press ESC to quit", maxcol()+1-4)
   diminfo()
return

function WVW_SIZE(nWinNum, hWnd, message, wParam, lParam)
* this function is called by gtwvw AFTER the size is changed
* WARNING: screen repainting is not performed completely by gtwvw at this point of call
local cScreen
local lNeedReset := .F., ;
      maxsavedscrrow, maxsavedscrcol
   if !s_lSizeReady
      * program is not ready to handle window resizing
      * (or this function is currently running)
      return NIL
   endif
   if nWinNum!=0
      * only care about Main Window
      return NIL
   endif

   * avoid reentrance
   s_lSizeReady := .F.

   do case
   case wParam == 2 //SIZE_MAXIMIZED
      //alert("MAXIMIZE")
      * reset is required only if we are changing size
      lNeedReset := maxcol() != wvw_maxmaxcol();
                    .or. maxrow() != wvw_maxmaxrow()

      if lNeedReset
         maxsavedscrrow := min(min(s_nNormalMaxrow, wvw_maxmaxrow()),maxrow())
         maxsavedscrcol := min(min(s_nNormalMaxcol, wvw_maxmaxcol()),maxcol())
         cScreen := savescreen(0,0,maxsavedscrrow, maxsavedscrcol)
         if setmode(wvw_maxmaxrow()+1, wvw_maxmaxcol()+1) //adjust maxrow() & maxcol()
            restscreen(0,0,maxsavedscrrow, maxsavedscrcol, cScreen)
         endif
         diminfo()  //updatescr()
      endif
   case wParam == 0 //SIZE_RESTORED
      //alert("RESTORE")
      lNeedReset := maxcol() != s_nNormalMaxcol .or.;
                    maxrow() != s_nNormalMaxrow
      if lNeedReset
         maxsavedscrrow := min(s_nNormalMaxrow, maxrow())
         maxsavedscrcol := min(s_nNormalMaxcol, maxcol())
         cScreen := savescreen(0,0,maxsavedscrrow, maxsavedscrcol)
         if setmode(s_nNormalMaxrow+1,s_nNormalMaxcol+1)
            restscreen(0,0,maxsavedscrrow, maxsavedscrcol, cScreen)
         endif
         diminfo()  //updatescr()
      endif
   otherwise
      * do nothing
   endcase

   * allow next call
   s_lSizeReady := .T.
return NIL
