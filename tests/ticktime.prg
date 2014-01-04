PROCEDURE Main()

   LOCAL nTime0, nTime, nTimeLast, nMin := 9999, nMax := -9999, nTick := 0, nLoop := 0

   ? "Wait for 10 seconds..."
   nTimeLast := nTime0 := hb_MilliSeconds()
   DO WHILE ( nTime := hb_MilliSeconds() ) - nTime0 < 10000
      IF nTimeLast != nTime
         nTick++
         nMin := Min( nMin, nTime - nTimeLast )
         nMax := Max( nMax, nTime - nTimeLast )
         nTimeLast := nTime
      ENDIF
      nLoop++
   ENDDO
   nTime := nTimeLast - nTime0

   SET DECIMALS TO 3

   ? "Ticks per second:", hb_ntos( nTick * 1000 / nTime )
   ? "Min/avg/max interval (ms):", hb_ntos( nMin ), "/", hb_ntos( nTime / nTick ), "/", hb_ntos( nMax )
   ? "Loops per tick:", hb_ntos( nLoop / nTick )

   RETURN
