/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for thread return complex values
 *    and detached locals created by thread and used after thread
 *    termination.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#ifdef __XHARBOUR__
   #xtranslate hb_threadStart( <x,...> ) => StartThread( <x> )
   #xtranslate hb_threadJoin( <x,...> ) => JoinThread( <x> )
#endif

static s_var
proc main()
   local xResult
   ? Version()
   ? "join:", hb_threadJoin( hb_threadStart( @thFunc() ), @xResult )
   ? "result:", xResult
   ? "static var type:", valtype( s_var )
   ? eval( s_var )
   ? eval( s_var )
return

func thFunc()
   local i := 12345.678
   s_var := {|| i++ }
return replicate( "Hello World!!! ", 3 )
