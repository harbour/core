/*
 * $Id$
 */

FUNCTION HBNETIOSRV_RPCMAIN( sFunc, ... )

   OutStd( "DO", sFunc:name, "WITH", ..., hb_osNewLine() )

   RETURN sFunc:exec( ... )
