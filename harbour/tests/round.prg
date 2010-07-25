//
// $Id$
//

// Testing Harbour rounding.
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

function main()
local n, value := -5

   for n := 1 to 100
      OUTSTD(hb_eol())
      OUTSTD(value)
      OUTSTD(round(value, 3))
      OUTSTD(round(value, 2))
      OUTSTD(round(value, 1))
      OUTSTD(round(value, 0))
      value += 0.001
   next

return nil
