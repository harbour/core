/*
 * Demonstration/test code for non virtual hidden messages
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#xtranslate QQOut( [<x,...>] ) => [OutStd( <x> )]
#xtranslate QOut( [<x,...>] ) => OutStd( hb_eol() )[; OutStd( <x> )]

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := myclass3():new()

   ? OS(), Version(), Date(), Time()
   ?

   o:m1()
   o:m2()
   o:m3()

   RETURN

CREATE CLASS myclass1

   HIDDEN:
   VAR a INIT "(a1)"

   CLASS VAR b INIT "(b1)"

   METHOD x()
   PROTECTED:
   VAR c INIT "(c1)"
   CLASS VAR d INIT "(d1)"
   METHOD y()
   EXPORTED:
   VAR e INIT "(e1)"
   CLASS VAR f INIT "(f1)"
   METHOD z()
   METHOD m1()

ENDCLASS

METHOD m1()

   ? "Method: MYCLASS1:M1()"
   ? "   a =>", ::a, ",  should be: (a1)"
   ? "   b =>", ::b, ",  should be: (b1)"
   ? "   c =>", ::c, ",  should be: (c3)"
   ? "   d =>", ::d, ",  should be: (d3)"
   ? "   e =>", ::e, ",  should be: (e3)"
   ? "   f =>", ::f, ",  should be: (f3)"
   ? "   execute ::x(),  should be: MYCLASS1:X()"
   ::x()
   ? "   execute ::y(),  should be: MYCLASS3:Y()"
   ::y()
   ? "   execute ::z(),  should be: MYCLASS3:Z()"
   ::z()

   RETURN self

METHOD x()

   ? "   Method: MYCLASS1:X()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y()

   ? "   Method: MYCLASS1:Y()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z()

   ? "   Method: MYCLASS1:Z()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

CREATE CLASS myclass2

   HIDDEN:
   VAR a INIT "(a2)"

   CLASS VAR b INIT "(b2)"

   METHOD x()
   PROTECTED:
   VAR c INIT "(c2)"
   CLASS VAR d INIT "(d2)"
   METHOD y()
   EXPORTED:
   VAR e INIT "(e2)"
   CLASS VAR f INIT "(f2)"
   METHOD z()
   METHOD m2()

ENDCLASS

METHOD m2()

   ? "Method: MYCLASS2:M2()"
   ? "   a =>", ::a, ",  should be: (a2)"
   ? "   b =>", ::b, ",  should be: (b2)"
   ? "   c =>", ::c, ",  should be: (c3)"
   ? "   d =>", ::d, ",  should be: (d3)"
   ? "   e =>", ::e, ",  should be: (e3)"
   ? "   f =>", ::f, ",  should be: (f3)"
   ? "   execute ::x(),  should be: MYCLASS2:X()"
   ::x()
   ? "   execute ::y(),  should be: MYCLASS3:Y()"
   ::y()
   ? "   execute ::z(),  should be: MYCLASS3:Z()"
   ::z()

   RETURN self

METHOD x()

   ? "   Method: MYCLASS2:X()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y()

   ? "   Method: MYCLASS2:Y()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z()

   ? "   Method: MYCLASS2:Z()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

CREATE CLASS myclass3 INHERIT myclass1, myclass2

   HIDDEN:
   VAR a INIT "(a3)"

   CLASS VAR b INIT "(b3)"

   METHOD x()
   PROTECTED:
   VAR c INIT "(c3)"
   CLASS VAR d INIT "(d3)"
   METHOD y()
   EXPORTED:
   VAR e INIT "(e3)"
   CLASS VAR f INIT "(f3)"
   METHOD z()
   METHOD m3()

ENDCLASS

METHOD m3()

   ? "Method: MYCLASS3:M3()"
   ? "   a =>", ::a, ",  should be: (a3)"
   ? "   b =>", ::b, ",  should be: (b3)"
   ? "   c =>", ::c, ",  should be: (c3)"
   ? "   d =>", ::d, ",  should be: (d3)"
   ? "   e =>", ::e, ",  should be: (e3)"
   ? "   f =>", ::f, ",  should be: (f3)"
   ? "   execute ::x(),  should be: MYCLASS3:X()"
   ::x()
   ? "   execute ::y(),  should be: MYCLASS3:Y()"
   ::y()
   ? "   execute ::z(),  should be: MYCLASS3:Z()"
   ::z()

   RETURN self

METHOD x()

   ? "   Method: MYCLASS3:X()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y()

   ? "   Method: MYCLASS3:Y()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z()

   ? "   Method: MYCLASS3:Z()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self
