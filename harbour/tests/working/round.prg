// Testing Harbour rounding.

#ifdef __HARBOUR__
#define NewLine CHR(10)
#else
#define NewLine CHR(13)+CHR(10)
#endif

function main()
local n, value := -5

   for n := 1 to 10000
      OUTSTD(NewLine)
      OUTSTD(value)
      OUTSTD(round(value, 3))
      OUTSTD(round(value, 2))
      OUTSTD(round(value, 1))
      OUTSTD(round(value, 0))
      value += 0.001
   next

return nil
