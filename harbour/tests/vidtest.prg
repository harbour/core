/*
 * $Id$
 */
/*
 * Harbour project video test code
 *
 * Program originally by Brian Dukes <bdukes@yellowthingy.co.uk>
 *
 * Redirect the output of this program to a file.
 *
 * ie: VidTest >results
 *
 */

#include "box.ch"

#ifndef __CLIP__
    #ifdef FlagShip
        #xtranslate hb_secondsCPU([<x>]) => secondsCPU([<x>])
    #else
        #ifndef __HARBOUR__
            #xtranslate hb_secondsCPU([<x>]) => seconds([<x>])
        #endif
        #define EOL chr(13) + chr(10)
    #endif
#endif
#ifndef EOL
    #define EOL chr(10)
#endif
#command ? => outstd(EOL);outerr(EOL)
#command ? <xx,...> => outstd(<xx>, EOL);outerr(<xx>, EOL)

#ifdef FlagShip
    static nDispCount := 0

    #xtranslate dispbegin() => iif((++nDispCount)==1, dispbegin(NIL),)
    #xtranslate dispend()   => iif(nDispCount>0 .and. (--nDispCount)==0, dispend(NIL),)
#endif

function main()
    local aResult := {}

    Initialise()   // Initialise Screen Display

    // Perform Tests
    aadd(aResult, StaticText()   )
    aadd(aResult, WindowBounce() )
    aadd(aResult, ColourBoxes()  )

    // Display Results
    Summary(aResult)
return NIL


// initialise the screen
static function Initialise()
    //SetMode(25,80)
    set colour to "W+/BG"
    dispbox(0,0,MaxRow(), MaxCol(), replicate(chr(176),9), "BG/B")
return NIL


// repeatedly display a string in the same position
// this test determines how well the Screen i/o subsystem is
// caching screen writes.
static function StaticText()
    local cResult
    local r       := MaxRow() / 2
    local str     := Version()
    local c
    local i       := 0
    local nEnd    := 0
    local nStart  := hb_secondsCPU()

    str := "Hello World - From " + Left(str,At(" ",str)-1)
    c   := (MaxCol()-len(str)) / 2

    for i := 1 to 5000
        @ r, c say str
    next i

    nEnd := hb_secondsCPU()

    cResult := "StaticText:  Iterations=5000, Time="+alltrim(str(nEnd-nStart))+ ;
               "secs,  Average FPS = "+alltrim(str(round(5000 / (nEnd-nStart),0)))+" FPS"
return cResult


// Bounce a window around the screen a few thousand times
// timing the duration, and determining the average FPS
static function WindowBounce()
    local cResult := ""
    local nBoxes  := Min(MaxRow(), MaxCol()-7)-6 /* keep the box in bounds */
    local x       := array(NBOXES)
    local y       := array(NBOXES)
    local dx      := array(NBOXES)
    local dy      := array(NBOXES)
    local clr     := array(NBOXES)
    local scr     := array(NBOXES)
    local nFrames := 0
    local nStart  := 0
    local nEnd    := 0
    local i       := 0
    local aCol    := {"N", "B", "G", "BG", "R", "RB", "GR", "W", ;
                      "N*","B*","G*","BG*","R*","RB*","GR*","W*" }

    // initialise boxes
    for i := 1 to nBoxes
        x[i]   := i
        y[i]   := i-1
        dx[i]  := -1
        dy[i]  := 1
        clr[i] := "W+/"+aCol[(i-1)%16+1]
    next i

    nStart := hb_secondsCPU()
    dispbegin()

    do while nFrames < 5000

        for i := 1 to nBoxes
            scr[i] := SaveScreen(x[i], y[i], x[i]+6, y[i]+12)
            @ x[i], y[i], x[i]+6, y[i]+12 box B_SINGLE+" " color clr[i]
        next i

        dispend()
        dispbegin()

        for i := nBoxes to 1 step -1
            // remove boxes from screen
            RestScreen(x[i], y[i], x[i]+6, y[i]+12, scr[i])

            // move
            x[i] += dx[i]
            y[i] += dy[i]
            if x[i] <= 0 .or. x[i]+6 >= MaxRow()
                dx[i] := -dx[i]
            endif
            if y[i] <= 0 .or. y[i]+12 >= MaxCol()
                dy[i] := -dy[i]
            endif
        next i

        ++nFrames
    enddo

    dispend()
    nEnd := hb_secondsCPU()

    cResult := "WindowBounce:Iterations="+alltrim(str(nFrames))+", Time="+alltrim(str(nEnd-nStart))+ ;
               "secs,  Average FPS = "+alltrim(str(round(nFrames / (nEnd-nStart),0)))+" FPS"

return cResult


// Display colour boxes,  repeatedly, this will determine
// how efficiently the screen i/o subsystem is caching the
// dispbegin()'s and dispend()'s
static function ColourBoxes()
    local cResult := ""
    local nFrames := 0
    local nStart  := 0
    local nEnd    := 0
    local i       := 0
    local nDir    := 1
    local nDepth  := 0
    local aCol    := {"N", "B", "G", "BG", "R", "RB", "GR", "W", ;
                      "N*","B*","G*","BG*","R*","RB*","GR*","W*" }

    nStart := hb_secondsCPU()
    // display boxes to screen

    do while nFrames < 5000
        if nDir == 1
            dispbegin()
        else
            dispend()
        endif

        nDepth += nDir

        if nDepth > 4 .or. nDepth < 1
            nDir := -nDir
        endif
        i := nFrames %16 +1
        dispbox(5,10, MaxRow()-5, MaxCol()-10, ;
                      replicate(chr(i+64),9),;
                      "W+/"+aCol[i] )
        ++nFrames
    enddo

    // remove any nested dispbegins()
    do while nDepth > 0
       dispend()
       nDepth--
    enddo

    nEnd := hb_secondsCPU()

    cResult := "ColourBoxes: Iterations="+alltrim(str(nFrames))+", Time="+alltrim(str(nEnd-nStart))+ ;
               "secs,  Average FPS = "+alltrim(str(round(nFrames / (nEnd-nStart),0)))+" FPS"

return cResult


// display results
static function Summary(aResult)
    local i := 0

    clear screen
    ? "Resolution:  " + Ltrim(str( MaxRow()+1 )) + " x " + Ltrim(str( MaxCol()+1 )) + " " + Version()
    for i := 1 to len(aResult)
        ? aResult[i]
    next i
    ?
    ? "press any key to continue"
    inkey(0)

return aResult
