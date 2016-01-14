/*
 * RPC demo module for hbnetio server.
 *    Usage: 'hbnetio -rpc=rpcdemo.hb'
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 *
 */

STATIC FUNCTION HBNETIOSRV_RPCMAIN( sFunc, ... )

   OutStd( "DO", sFunc:name, "WITH", ..., hb_eol() )

   RETURN sFunc:exec( ... )
