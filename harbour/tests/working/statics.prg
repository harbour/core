// Testing Harbour statics variables management



static z := "First"



function Main()

LOCAL i



   static a := "Hello", b := { 1, 3 }



   QOut( a )

   QOut( b[ 2 ] )



   Two()



   QOut( "Ok!" )



   FOR i:=1 TO 10

     NumStat()

   NEXT



return nil



function Two()



   static a := "Test"



   QOut( a )



return nil



FUNCTION THREE( p )

   QOut( p )

RETURN p



PROCEDURE NumStat()

STATIC n:=0

  QOut( ++n )

  QOut( z )

RETURN