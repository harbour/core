/*
 * Demonstration/test code for thread return complex values
 * and detached locals created by thread and used after thread
 * termination.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#ifdef __XHARBOUR__
   #xtranslate hb_threadStart( <x,...> ) => StartThread( <x> )
   #xtranslate hb_threadJoin( <x,...> ) => JoinThread( <x> )
#endif

static s_var

procedure Main()

   local xResult

   ? Version()
   ? "join:", hb_threadJoin( hb_threadStart( @thFunc() ), @xResult )
   ? "result:", xResult
   ? "static var type:", ValType( s_var )
   ? Eval( s_var )
   ? Eval( s_var )

   return

static function thFunc()

   local i := 12345.678

   s_var := {|| i++ }

   return Replicate( "Hello World!!! ", 3 )
