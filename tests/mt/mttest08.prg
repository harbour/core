/*
 * Harbour Project source code:
 *    demonstration/test code for using memvar variables sharing and
 *    copping
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "hbthread.ch"

memvar pub1, pub2
memvar prv1, prv2

proc main()

   ? Version()
   ? "Main start"

   public pub1, pub2
   private prv1, prv2

   ?
   ? "Do not inherit memvars."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Inherit copy of publics."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_BITOR( HB_THREAD_INHERIT_PUBLIC, ;
                                            HB_THREAD_MEMVARS_COPY ), ;
                                  @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Inherit copy of privates."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_BITOR( HB_THREAD_INHERIT_PRIVATE, ;
                                            HB_THREAD_MEMVARS_COPY ), ;
                                  @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Inherit copy of publics and privates."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_BITOR( HB_THREAD_INHERIT_MEMVARS, ;
                                            HB_THREAD_MEMVARS_COPY ), ;
                                  @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Share publics with child threads."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Share privates with child threads."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_THREAD_INHERIT_PRIVATE, @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

   ?
   ? "Share publics and privates with child threads."
   initVars()
   ? "main thread:"
   testAllVars()
   hb_threadJoin( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @thFunc() ) )
   ? "main thread:"
   testAllVars()
   wait

return

static proc initVars()
   pub1 := "main:public1"
   pub2 := "main:public2"
   prv1 := "main:private1"
   prv2 := "main:private2"
return

static proc testAllVars()
   test_var( "PUB1" )
   test_var( "PUB2" )
   test_var( "PRV1" )
   test_var( "PRV2" )
return

static proc test_var( cVarName )
   ? "    " + cVarName + ":", type( cVarName )
   if ! type( cVarName ) == "U"
      ?? " ->", &cVarName
   endif
return

static proc thFunc()
   ? "child thread:"
   testAllVars()
   ? "assign..."
   pub1 := "thread:public1"
   pub2 := "thread:public2"
   prv1 := "thread:private1"
   prv2 := "thread:private2"
   ? "child thread:"
   testAllVars()
return
