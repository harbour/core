//
// $Id$
//

// Testing Harbour statics variables management

static z := "First"

function Main()
LOCAL i, cb

   static a := "Hello", b := { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }

   QOut( a )
   QOut( b[ 2 ] )

   Two()

   QOut( "Ok!" )

   FOR i:=1 TO 10
     NumStat()
   NEXT
   
   cb :=DetachVar( 10 )
   FOR i:=1 To 10
     QOut( EVAL( cb, b[ i ] ) )
   NEXT

return nil

function Two()

   static a := "Test"

   QOut( a )

return nil

FUNCTION THREE( p )
   QOut( p )
RETURN p

PROCEDURE NumStat(a)
STATIC n:=1
LOCAL cb
//STATIC m:=n      //uncomment it to see an error
//STATIC m:=Time() //uncomment it to see an error

  cb :={|x| z +STR(x)}
  QOut( ++n )
  QOut( EVAL( cb,n ) )
  
RETURN


FUNCTION DetachVar( xLocal )
STATIC xStatic:=100

RETURN( {|x| ++xStatic, x+xStatic+xLocal} )