/*
* $Id$
*/

PROC main()
LOCAL nTime0, nTime, nTimeLast, nMin := 9999, nMax := -9999, nTick := 0, nLoop := 0

  ? "Wait for 10 seconds..."
  nTimeLast := nTime0 := HB_MILLISECONDS()
  DO WHILE ( nTime := HB_MILLISECONDS() ) - nTime0 < 10000
    IF nTimeLast != nTime
       nTick++
       nMin := MIN(nMin, nTime - nTimeLast)
       nMax := MAX(nMax, nTime - nTimeLast)
       nTimeLast := nTime
    ENDIF
    nLoop++
  ENDDO
  nTime := nTimeLast - nTime0
  ? "Ticks per second:", LTRIM(STR(nTick * 1000/ nTime, 12, 3))
  ? "Min/avg/max interval (ms):", LTRIM(STR(nMin, 9, 3)), "/", ;
                                  LTRIM(STR(nTime / nTick, 9, 3)), "/", ;
                                  LTRIM(STR(nMax, 9, 3))
  ? "Loops per tick:", nLoop / nTick
RETURN
