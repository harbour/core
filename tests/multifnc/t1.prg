procedure main2()
   ? "main2", ProcFile( "main2" ), ProcFile( @main2() )
   ? "alert", ProcFile( "alert" ), ProcFile( @Alert() ), Alert()
   ? "p0", ProcFile( "p0" ), ProcFile( @p0() ), ( @p0() ):exec(), p0(), t( "p0", @p0(), p0(), "t0.prg" )
   ? "p1", ProcFile( "p1" ), ProcFile( @p1() ), ( @p1() ):exec(), p1(), t( "p1", @p1(), p1(), "t0.prg" )
   ? "p2", ProcFile( "p2" ), ProcFile( @p2() ), ( @p2() ):exec(), p2(), t( "p2", @p2(), p2(), "t1.prg" )
   ? "p3", ProcFile( "p3" ), ProcFile( @p3() ), ( @p3() ):exec(), p3(), t( "p3", @p3(), p3(), "t0.prg" )
   ? "p4", ProcFile( "p4" ), ProcFile( @p4() ), ( @p4() ):exec(), p4(), t( "p4", @p4(), p4(), "t1.prg" )
   ? "p5", ProcFile( "p5" ), ProcFile( @p5() ), ( @p5() ):exec(), p5(), t( "p5", @p5(), p5(), "t2.prg" )
   ? "p6", ProcFile( "p6" ), ProcFile( @p6() ), ( @p6() ):exec(), p6(), t( "p6", @p6(), p6(), "t0.prg" )
   ? "p7", ProcFile( "p7" ), ProcFile( @p7() ), ( @p7() ):exec(), p7(), t( "p7", @p7(), p7(), "t1.prg" )
   ? "==="
   main3()
   return

function p1(); return "P1:t1.prg"
function p2(); return "P2:t1.prg"
function p3(); return "P3:t1.prg"
function p4(); return "P4:t1.prg"

function Alert()
   return "[ALERT]"

#pragma begindump
#include "hbapi.h"

HB_FUNC( P7 )
{
   hb_retc( "P7:t1.prg" );
}
#pragma enddump
