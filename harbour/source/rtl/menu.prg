/*
 * $Id$

   Copyright(C) 1999 by Andi Jahja

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: andij@aonlippo.co.id
*/

#command @ <row>, <col> PROMPT <prompt> [MESSAGE <msg>]                 ;
      => __AtPrompt( <row>, <col>, <prompt> , <msg> )

#command MENU TO <v>                                                    ;
      => <v> := __MenuTo( {|_1| if(PCount() == 0, <v>, <v> := _1)}, #<v> )

#include "inkey.ch"

static aMenu := {}
********************************************************************************
function __atprompt(nrow, ncol, citem, cmessage)
********************************************************************************
local nmarkpos, citemkey

nmarkpos := at("~", citem)
citemkey := if(nmarkpos > 0, lower(substr(citem, nmarkpos + 1, 1)), "")
aadd(amenu, { nrow, ncol, citem, citemkey, cmessage, setcolor() })
sayitem(nrow, ncol, citem, setcolor())

return nil

********************************************************************************
function __menuto(bvarblock, cvarname)
********************************************************************************
local athismenu := amenu
local nselect, ncurrent, nprevious := 0
local chighlight
local nkey
local ncursor := setcursor(0)

amenu := {}

if len(athismenu) == 0
    return 0
endif

nselect := eval(bvarblock)
if valtype(nselect) <> "N"
    nselect := 1
elseif (nselect := int(nselect)) < 1 .or. nselect > len(athismenu)
    nselect := 1
endif

ncurrent := nselect

do while ncurrent > 0

    if ncurrent <> nprevious
        chighlight := athismenu[ncurrent][6]
        chighlight := substr(chighlight, at(",", chighlight) + 1)
        chighlight := left(chighlight, at(",", chighlight))
        sayitem(athismenu[ncurrent][1], athismenu[ncurrent][2], ;
            athismenu[ncurrent][3], chighlight)
        saymessage(athismenu[ncurrent][5])
        nprevious := ncurrent
    endif

    setpos(athismenu[ncurrent][1], athismenu[ncurrent][2])
    nkey := inkey(0)

    do case
    case nkey == K_ENTER .or. nKey == K_PGUP .or. nKey == K_PGDN
        nselect  := ncurrent
        ncurrent := 0
    case nkey == K_ESC
        nselect  := 0
        ncurrent := 0
    case nkey == K_UP .or. nKey == K_LEFT
        if --ncurrent < 1
            ncurrent := if(set(_SET_WRAP), len(athismenu), 1)
        endif
    case nkey == K_DOWN .or. nKey == K_RIGHT
        if ++ncurrent > len(athismenu)
            ncurrent := if(set(_SET_WRAP), 1, len(athismenu))
        endif
    case nkey == K_HOME
        ncurrent := 1
    case nkey == K_END
        ncurrent := len(athismenu)
    case nkey >= 32 .and. nkey <= 127
        if (nselect := ascan(athismenu, { |aitem| aitem[4] == lower(chr(nkey)) })) > 0
            ncurrent := 0
        endif
    endcase

    if ncurrent <> nprevious
        sayitem(athismenu[nprevious][1], athismenu[nprevious][2], ;
            athismenu[nprevious][3], athismenu[nprevious][6])
        if athismenu[nprevious][5] <> nil
            saymessage(space(len(athismenu[nprevious][5])))
        endif
    endif
enddo
setcursor(ncursor)
return nselect

********************************************************************************
static function sayitem(nrow, ncol, ctext, ccolor)
********************************************************************************
local csavecolor, cshorttext, nmarkpos, nmarkidx, ncommapos

ccolor     := trim(ccolor)
csavecolor := setcolor(ccolor)
cshorttext := strtran(ctext, "~")
@ nrow, ncol say cshorttext

if (nmarkpos := at("~", ctext)) > 0

    for nmarkidx := 1 to 4
        ncommapos := at(",", ccolor)
        ccolor    := if(ncommapos > 0, substr(ccolor, ncommapos + 1), "")
    next

    if .not. ccolor == ""
        setcolor(ccolor)
        @ nrow, ncol + nmarkpos - 1 say substr(ctext, nmarkpos + 1, 1)
    endif

endif

setcolor(csavecolor)
return nil

********************************************************************************
static function saymessage(cmessage)
********************************************************************************
local nrow, ncol

if cmessage == nil .or. (nrow := set(_SET_MESSAGE)) == 0
    return nil
endif

ncol := if(set(_SET_MCENTER), int((maxcol() - len(cmessage)) / 2), 0)

@ nrow, ncol say cmessage

return nil
