//
// $Id$
//

// directory test
function main(filespec,attribs)

local adir := {}
local x := 0

adir := asort( directory(filespec,attribs),,, {|x,y|upper(x[1]) < upper(y[1])} )

for x := 1 to len(adir)
   qout(padr(adir[x,1], 20), "|", ;
        padl(adir[x,2], 10), "|", ;
        padr(adir[x,3],  8), "|", ;
        padr(adir[x,4],  8), "|", ;
        adir[x,5])
next x

return nil

