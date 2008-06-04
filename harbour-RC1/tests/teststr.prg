//
// $Id$
//

STATIC cNewLine

function main( cParam )

   IF EMPTY( cParam )
      cNewLine := CHR(13)+CHR(10)
   ELSE
      cNewLine := CHR(10)
   END IF

   outstd (cNewLine)
   outstd ("Running with SET FIXED OFF (the default setting): ")
   outstd (cNewLine)
   test()
   __ACCEPT ("Pause before running again with SET FIXED ON: ")
   outstd (cNewLine)
   outstd ("Running with SET FIXED ON: ")
   outstd (cNewLine)
   SET (_SET_FIXED, "ON")
   test()

return nil

procedure test()
local a := 15.1
local b := 10.0002575
local nI, c, d

   outstd( "1: " )
   outstd (10)
   outstd (a)
   outstd (-a)
   outstd (b)
   outstd (-b)
   outstd (cNewLine)
   outstd( "2: " )
   outstd (a + b)
   outstd (a - b)
   outstd (a * b)
   outstd (a / b)
   outstd (cNewLine)
   outstd( "3: " )
   outstd (a % b)
   outstd (a ** b)
   outstd (cNewLine)

   c = a * b
   d = b * a
   outstd (cNewLine)
   outstd( "4: " )
   outstd (str (c))
   outstd (str (d))
   outstd (cNewLine)
   outstd( "5: " )
   outstd (str (c + d))
   outstd (str (c - d))
   outstd (str (c * d))
   outstd (str (c / d))
   outstd (cNewLine)

   outstd (cNewLine)
   outstd( "6: " )
   outstd (a + b + c)
   outstd (c - b - a)
   outstd (b * a * c)
   outstd (b * a * c * d)
   b := 1.000213
   outstd (b * b * b * b * b * b * b)
   outstd (cNewLine)

   FOR nI := 1 to 20
      outstd (cNewLine)
      outstd( LTRIM( STR( 6 + nI ) ) + ": " )
      outstd (10 ** nI + (1.02 * 1.02))
   NEXT nI
   outstd (cNewLine)

   outstd (cNewLine)
   outstd( "27: " )
   outstd (str (a), a)
   outstd (cNewLine)

   outstd( "28: " )
   outstd (str (b), b)
   outstd (cNewLine)

   outstd( "29: " )
   outstd (str (b, 15))
   outstd (cNewLine)

   outstd( "30: " )
   outstd (str (b, 20, 5))
   outstd (cNewLine)

   outstd( "31: " )
   outstd (str (b, 20, 10))
   outstd (cNewLine)

   outstd( "32: " )
   outstd (str (b, 5, 10))
   outstd (cNewLine)

   outstd( "33: " )
   outstd (str (b, 20, -10))
   outstd (cNewLine)

   outstd( "34: " )
   outstd (str (b, -12, 7))
   outstd (cNewLine)

   outstd( "35: " )
   outstd (str (b, 0))
   outstd (cNewLine)

   outstd (cNewLine)
   a := 15.1004
   outstd( "36: " )
   outstd (str (a), a)
   outstd (cNewLine)

return
