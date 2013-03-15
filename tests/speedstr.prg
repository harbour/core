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
#  translate secondsCPU( => hb_secondsCPU(
#endif
#ifdef __XPP__
#  translate secondsCPU( => seconds(
#endif
#ifdef __HARBOUR__
#  include "hbclass.ch"
#else
#  translate hb_BChar( => Chr(
#endif

PROCEDURE Main()

   MEMVAR p
   LOCAL i, t
   LOCAL l, o
   STATIC s, s2[ 1 ]
   PRIVATE p

   p := s2[ 1 ] := s := l := ""
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      l += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "LOCAL  +=", t, "sec."
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      s += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "STATIC +=", t, "sec."
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      s2[ 1 ] += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "ARRAY[] +=", t, "sec."
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      p += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "PRIVATE +=", t, "sec."
   p := ""; s := "p"
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      &s += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "MACRO +=", t, "sec."
   o := mycls():new(); o:v := ""
   t := secondsCPU()
   FOR i := 1 TO N_LOOP
      o:v += hb_BChar( i )
   NEXT
   t := secondsCPU() - t
   ? "OBJECT:VAR +=", t, "sec."
   WAIT

   RETURN

CLASS mycls

   EXPORTED:
   VAR v

ENDCLASS
