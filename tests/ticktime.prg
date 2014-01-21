PROCEDURE Main()

   LOCAL nTime0, nTime, nTimeLast, nMin := 9999, nMax := - 9999, nTick := 0, nLoop := 0

   ? "Wait for 10 seconds..."
   nTimeLast := nTime0 := hb_MilliSeconds()
   DO WHILE ( nTime := hb_MilliSeconds() ) - nTime0 < 10000
      IF nTimeLast != nTime
         nTick ++
         nMin := Min( nMin, nTime - nTimeLast )
         nMax := Max( nMax, nTime - nTimeLast )
         nTimeLast := nTime
      ENDIF
      nLoop ++
   ENDDO
   nTime := nTimeLast - nTime0
   ? "Ticks per second:", LTrim( Str( nTick * 1000 / nTime, 12, 3 ) )
   ? "Min/avg/max interval (ms):", LTrim( Str( nMin, 9, 3 ) ), "/", ;
      LTrim( Str( nTime / nTick, 9, 3 ) ), "/", ;
      LTrim( Str( nMax, 9, 3 ) )
   ? "Loops per tick:", nLoop / nTick

   RETURN
