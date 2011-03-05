/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    speed test program for string concatenation by += operator
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#define N_LOOP 200000

#ifndef __XHARBOUR__
   #translate secondsCPU( => hb_secondsCPU(
#endif
#ifdef __XPP__
   #translate secondsCPU( => seconds(
#endif
#ifdef __HARBOUR__
#include "hbclass.ch"
#endif

proc main()
   memvar p
   local i, t
   local l, o
   static s, s2[1]
   private p
   p := s2[1] := s := l := ""
   t := secondsCPU()
   for i := 1 to N_LOOP
      l += chr( i )
   next
   t := secondsCPU() - t
   ? "LOCAL  +=", t, "sec."
   t := secondsCPU()
   for i := 1 to N_LOOP
      s += chr( i )
   next
   t := secondsCPU() - t
   ? "STATIC +=", t, "sec."
   t := secondsCPU()
   for i := 1 to N_LOOP
      s2[1] += chr( i )
   next
   t := secondsCPU() - t
   ? "ARRAY[] +=", t, "sec."
   t := secondsCPU()
   for i := 1 to N_LOOP
      p += chr( i )
   next
   t := secondsCPU() - t
   ? "PRIVATE +=", t, "sec."
   p := ""; s := "p"
   t := secondsCPU()
   for i := 1 to N_LOOP
      &s += chr( i )
   next
   t := secondsCPU() - t
   ? "MACRO +=", t, "sec."
   o := mycls():new(); o:v := ""
   t := secondsCPU()
   for i := 1 to N_LOOP
      o:v += chr( i )
   next
   t := secondsCPU() - t
   ? "OBJECT:VAR +=", t, "sec."
   wait
return

class mycls
   exported:
      var v
endclass
