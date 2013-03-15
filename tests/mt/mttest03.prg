/*
 * Harbour Project source code:
 *    demonstration/test code for using QUIT commands and ALWAYS statements
 *    execution. Main thread uses QUIT before child one.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#ifdef __XHARBOUR__
   #xtranslate hb_threadStart( <x,...> ) => StartThread( <x> )
   #xtranslate hb_threadJoin( <x,...> ) => JoinThread( <x> )
   #command begin sequence   => try
   #command always           => finally
#endif

proc main()
   local thID, i
   ? Version()
   ? "Main start"
   thID := hb_threadStart( @thFunc(), "A", "B", "C" )
   ? "Thread ID:", thID
   ? "==================="
   ?
   begin sequence
      for i := 1 to 10
         ?? "M"
         hb_idleSleep( 0.050 )
         if i == 5
            ? "Main QUIT"
            quit
         endif
      next
      ? "==================="
   always
      ? "Main ALWAYS section"
      ?
   endsequence
   ? "End of main"
return

proc thFunc( ... )
   local i
   ? "Thread begin"
   ? "Parameters:"
   aeval( hb_aParams(), {| x | qqout( "", x ) } )
   ?
   begin sequence
      for i := 1 to 10
         ?? "t"
         hb_idleSleep( 0.030 )
      next
   always
      ? "Thread ALWAYS section"
      ?
   endsequence
   ? "Thread end"
   ?
return

exit proc p
? "I'm EXIT procedure"
