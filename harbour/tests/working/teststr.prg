function main()

   outstd (CHR(13)+CHR(10))
   outstd ("Running with SET FIXED OFF (the default setting): ")
   outstd (CHR(13)+CHR(10))
   test()
   __ACCEPT ("Pause before running again with SET FIXED ON: ")
   outstd (CHR(13)+CHR(10))
   SETFIXED ("ON")
   test()

return nil

procedure test()
local a := 15.1
local b := 10.0002575
local nI, c, d

   outstd (10)
   outstd (a)
   outstd (-a)
   outstd (b)
   outstd (-b)
   outstd (CHR(13)+CHR(10))
   outstd (a + b)
   outstd (a - b)
   outstd (a * b)
   outstd (a / b)
   outstd (CHR(13)+CHR(10))
   outstd (a % b)
   outstd (a ** b)
   outstd (CHR(13)+CHR(10))

   c = a * b
   d = b * a
   outstd (CHR(13)+CHR(10))
   outstd (str (c))
   outstd (str (d))
   outstd (CHR(13)+CHR(10))
   outstd (str (c + d))
   outstd (str (c - d))
   outstd (str (c * d))
   outstd (str (c / d))
   outstd (CHR(13)+CHR(10))

   outstd (CHR(13)+CHR(10))
   outstd (a + b + c)
   outstd (c - b - a)
   outstd (b * a * c)
   outstd (b * a * c * d)
   b := 1.000213
   outstd (b * b * b * b * b * b * b)
   outstd (CHR(13)+CHR(10))

   FOR nI := 1 to 20
      outstd (CHR(13)+CHR(10))
      outstd (10 ** nI + (1.02 * 1.02))
   NEXT nI
   outstd (CHR(13)+CHR(10))

   outstd (CHR(13)+CHR(10))
   outstd (str (a))
   outstd (CHR(13)+CHR(10))

   outstd (str (b))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 12))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 12, 5))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 12, 7))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 5, 7))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 12, -7))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, -12, 7))
   outstd (CHR(13)+CHR(10))

   outstd (str (b, 0))
   outstd (CHR(13)+CHR(10))

return
