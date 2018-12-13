proc main2()
? "main2", procfile("main2"), procfile(@main2())
? "alert", procfile("alert"), procfile(@alert()), alert()
? "p0", procfile("p0"), procfile(@p0()), (@p0()):exec(), p0(), t("p0",@p0(),p0(),"t0.prg")
? "p1", procfile("p1"), procfile(@p1()), (@p1()):exec(), p1(), t("p1",@p1(),p1(),"t0.prg")
? "p2", procfile("p2"), procfile(@p2()), (@p2()):exec(), p2(), t("p2",@p2(),p2(),"t1.prg")
? "p3", procfile("p3"), procfile(@p3()), (@p3()):exec(), p3(), t("p3",@p3(),p3(),"t0.prg")
? "p4", procfile("p4"), procfile(@p4()), (@p4()):exec(), p4(), t("p4",@p4(),p4(),"t1.prg")
? "p5", procfile("p5"), procfile(@p5()), (@p5()):exec(), p5(), t("p5",@p5(),p5(),"t2.prg")
? "p6", procfile("p6"), procfile(@p6()), (@p6()):exec(), p6(), t("p6",@p6(),p6(),"t0.prg")
? "p7", procfile("p7"), procfile(@p7()), (@p7()):exec(), p7(), t("p7",@p7(),p7(),"t1.prg")
? "==="
main3()
return

func p1(); return "P1:t1.prg"
func p2(); return "P2:t1.prg"
func p3(); return "P3:t1.prg"
func p4(); return "P4:t1.prg"

func alert()
return "[ALERT]"

#pragma begindump
#include "hbapi.h"

HB_FUNC( P7 )
{
   hb_retc( "P7:t1.prg" );
}
#pragma enddump
