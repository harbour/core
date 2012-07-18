/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for non virtual hidden messages
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#xtranslate QQOUT([<x,...>]) => [OUTSTD(<x>)]
#xtranslate QOUT([<x,...>]) => OUTSTD(hb_eol())[;OUTSTD(<x>)]

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := myclass3():new(), i, cbErr

   ? Date(), Time(), Version(), OS()
   ?

   o:m1()
   o:m2()
   o:m3()

   RETURN

CREATE CLASS myclass1
   hidden:
   var a init "(a1)"

   class var b init "(b1)"

   METHOD x
   protected:
   var c init "(c1)"
   class var d init "(d1)"
   METHOD y
   exported:
   var e init "(e1)"
   class var f init "(f1)"
   METHOD z
   METHOD m1

ENDCLASS

METHOD m1

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

METHOD x

   ? "   Method: MYCLASS1:X()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y

   ? "   Method: MYCLASS1:Y()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z

   ? "   Method: MYCLASS1:Z()"
   ? "      a =>", ::a, ",  should be: (a1)"
   ? "      b =>", ::b, ",  should be: (b1)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

   CREATE CLASS myclass2
   hidden:
   var a init "(a2)"

CLASS var b init "(b2)"

   METHOD x
   protected:
   var c init "(c2)"
   CLASS var d init "(d2)"
   METHOD y
   exported:
   var e init "(e2)"
   CLASS var f init "(f2)"
   METHOD z
   METHOD m2

ENDCLASS

METHOD m2

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

METHOD x

   ? "   Method: MYCLASS2:X()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y

   ? "   Method: MYCLASS2:Y()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z

   ? "   Method: MYCLASS2:Z()"
   ? "      a =>", ::a, ",  should be: (a2)"
   ? "      b =>", ::b, ",  should be: (b2)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

CREATE CLASS myclass3 FROM myclass1, myclass2
   hidden:
   var a init "(a3)"

   class var b init "(b3)"

   METHOD x
   protected:
   var c init "(c3)"
   class var d init "(d3)"
   METHOD y
   exported:
   var e init "(e3)"
   class var f init "(f3)"
   METHOD z
   METHOD m3

ENDCLASS

METHOD m3

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

METHOD x

   ? "   Method: MYCLASS3:X()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD y

   ? "   Method: MYCLASS3:Y()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self

METHOD z

   ? "   Method: MYCLASS3:Z()"
   ? "      a =>", ::a, ",  should be: (a3)"
   ? "      b =>", ::b, ",  should be: (b3)"
   ? "      c =>", ::c, ",  should be: (c3)"
   ? "      d =>", ::d, ",  should be: (d3)"
   ? "      e =>", ::e, ",  should be: (e3)"
   ? "      f =>", ::f, ",  should be: (f3)"

   RETURN self
