//
// $Id$
//

// Testing Harbour rounding.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

function main()
local n, value := -5
local cNewLine := HB_OSNewLine()

   for n := 1 to 100
      OUTSTD(cNewLine)
      OUTSTD(value)
      OUTSTD(round(value, 3))
      OUTSTD(round(value, 2))
      OUTSTD(round(value, 1))
      OUTSTD(round(value, 0))
      value += 0.001
   next

return nil
