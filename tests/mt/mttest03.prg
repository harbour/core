/*
 * Demonstration/test code for using QUIT commands and ALWAYS statements
 * execution. Main thread uses QUIT before child one.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#ifdef __XHARBOUR__
   #xtranslate hb_threadStart( <x,...> ) => StartThread( <x> )
   #xtranslate hb_threadJoin( <x,...> ) => JoinThread( <x> )
   #command begin sequence   => try
   #command always           => finally
#endif

procedure Main()

   local thID, i

   ? Version()
   ? "Main start"
   thID := hb_threadStart( @thFunc(), "A", "B", "C" )
   ? "Thread ID:", thID
   ? Replicate( "=", 20 )
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
      ? Replicate( "=", 20 )
   always
      ? "Main ALWAYS section"
      ?
   endsequence
   ? "End of main"

   return

static procedure thFunc( ... )

   local i

   ? "Thread begin"
   ? "Parameters:"
   AEval( hb_AParams(), {| x | QQOut( "", x ) } )
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

exit procedure p()

   ? "I'm EXIT procedure"

   return
