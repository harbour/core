//
// $Id$
//

// Testing Harbour rounding.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#ifdef __HARBOUR__
#define NewLine CHR(10)
#else
#define NewLine CHR(13)+CHR(10)
#endif

function main()
local n, value := -5

   for n := 1 to 100
      OUTSTD(NewLine)
      OUTSTD(value)
      OUTSTD(round(value, 3))
      OUTSTD(round(value, 2))
      OUTSTD(round(value, 1))
      OUTSTD(round(value, 0))
      value += 0.001
   next

return nil
