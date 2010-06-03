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

#define EOL chr(10)
#xtranslate QQOUT([<x,...>]) => [OUTSTD(<x>)]
#xtranslate QOUT([<x,...>]) => OUTSTD(EOL)[;OUTSTD(<x>)]

#include "hbclass.ch"

proc main()
local o:=myclass3():new(), i, cbErr

? DATE(), TIME(), VERSION(), OS()
?

o:m1()
o:m2()
o:m3()

return

create class myclass1
hidden:
   var a init "(a1)"
   class var b init "(b1)"
   method x
protected:
   var c init "(c1)"
   class var d init "(d1)"
   method y
exported:
   var e init "(e1)"
   class var f init "(f1)"
   method z
   method m1
endclass

method m1
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
return self

method x
? "   Method: MYCLASS1:X()"
? "      a =>", ::a, ",  should be: (a1)"
? "      b =>", ::b, ",  should be: (b1)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method y
? "   Method: MYCLASS1:Y()"
? "      a =>", ::a, ",  should be: (a1)"
? "      b =>", ::b, ",  should be: (b1)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method z
? "   Method: MYCLASS1:Z()"
? "      a =>", ::a, ",  should be: (a1)"
? "      b =>", ::b, ",  should be: (b1)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self


create class myclass2
hidden:
   var a init "(a2)"
   class var b init "(b2)"
   method x
protected:
   var c init "(c2)"
   class var d init "(d2)"
   method y
exported:
   var e init "(e2)"
   class var f init "(f2)"
   method z
   method m2
endclass

method m2
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
return self

method x
? "   Method: MYCLASS2:X()"
? "      a =>", ::a, ",  should be: (a2)"
? "      b =>", ::b, ",  should be: (b2)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method y
? "   Method: MYCLASS2:Y()"
? "      a =>", ::a, ",  should be: (a2)"
? "      b =>", ::b, ",  should be: (b2)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method z
? "   Method: MYCLASS2:Z()"
? "      a =>", ::a, ",  should be: (a2)"
? "      b =>", ::b, ",  should be: (b2)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self


create class myclass3 from myclass1, myclass2
hidden:
   var a init "(a3)"
   class var b init "(b3)"
   method x
protected:
   var c init "(c3)"
   class var d init "(d3)"
   method y
exported:
   var e init "(e3)"
   class var f init "(f3)"
   method z
   method m3
endclass

method m3
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
return self

method x
? "   Method: MYCLASS3:X()"
? "      a =>", ::a, ",  should be: (a3)"
? "      b =>", ::b, ",  should be: (b3)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method y
? "   Method: MYCLASS3:Y()"
? "      a =>", ::a, ",  should be: (a3)"
? "      b =>", ::b, ",  should be: (b3)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self

method z
? "   Method: MYCLASS3:Z()"
? "      a =>", ::a, ",  should be: (a3)"
? "      b =>", ::b, ",  should be: (b3)"
? "      c =>", ::c, ",  should be: (c3)"
? "      d =>", ::d, ",  should be: (d3)"
? "      e =>", ::e, ",  should be: (e3)"
? "      f =>", ::f, ",  should be: (f3)"
return self
