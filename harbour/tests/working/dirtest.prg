//
// $Id$
//

// directory test
function main(filespec,attribs)

local adir := {}
local x := 0
local cOs := OS(), cNewLine

   IF "OS/2" $ cOs .OR. "WIN" $ cOs .OR. "DOS" $ cOs
      cNewLine := CHR( 13 ) + CHR( 10 )
   ELSE
      cNewLine := CHR( 10 )
   END IF

// adir := asort( directory(filespec,attribs),,, {|x,y|upper(x[1]) < upper(y[1])} )
adir := directory(filespec,attribs)

for x := 1 to len(adir)
   outstd(cNewLine)
   outstd(padr(adir[x,1], 20), "|", ;
          padl(adir[x,2], 10), "|", ;
          padr(adir[x,3],  8), "|", ;
          padr(adir[x,4],  8), "|", ;
          adir[x,5])
next x

return nil
