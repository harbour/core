//
// $Id$
//

//
// Testing Empty() function
//
// Date : 16/05/1999
// Time : 21:00
//
// Writte by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

#include "set.ch"

function Main()

   __SETCENTURY ("ON")                      // SET CENTURY ON
   SET (_SET_DATEFORMAT, "DD/MM/YYYY")       // SET DATE BRITISH
   QOut( "British date format with century on" )
   QOut( "C 'Hallo'      ", empty( "Hallo"                ) )
   QOut( "C ''           ", empty( ""                     ) )
   QOut( "C '  '         ", empty( "  "                   ) )
   QOut( "C ' \0'        ", empty( " "+chr(0)             ) )
   QOut( "C ' \n\t'      ", empty( " "+chr(13)+chr(9)     ) )
   QOut( "C '  A'        ", empty( "  A"                  ) )
   QOut( "C ' x '        ", empty( " x "                  ) )
   QOut( "C ' x\0'       ", empty( " x"+chr(0)            ) )
   QOut( "C ' \nx\t'     ", empty( " "+chr(13)+"x"+chr(9) ) )

   QOut( "N 0            ", empty( 0                      ) )
   QOut( "N -0           ", empty( -0                     ) )
   QOut( "N 0.0          ", empty( 0.0                    ) )
   QOut( "N 70000-70000  ", empty( 70000-70000            ) )
   QOut( "N 1.5*1.5-2.25 ", empty( 1.5*1.5-2.25           ) )

   QOut( "N 10           ", empty( 10                     ) )
   QOut( "N 10.0         ", empty( 10.0                   ) )
   QOut( "N 70000+70000  ", empty( 70000+70000            ) )
   QOut( "N 1.5*1.5*2.25 ", empty( 1.5*1.5*2.25           ) )

   Pause()

   QOut( "D 10/10/1824   ", ctod("10/10/1824"), empty( ctod("10/10/1824")     ) )
   QOut( "D 31/02/1825   ", ctod("31/02/1825"), empty( ctod("31/02/1825")     ) )
   QOut( "D 99/99/9999   ", ctod("99/99/9999"), empty( ctod("99/99/9999")     ) )
   QOut( "D   /  /       ", ctod("  /  /    "), empty( ctod("  /  /    ")     ) )
   QOut( "D              ", ctod(""),           empty( ctod("")               ) )
   QOut( "L .T.          ", empty( .T.                    ) )
   QOut( "L .F.          ", empty( .F.                    ) )
   QOut( "U NIL          ", empty( NIL                    ) )
   QOut( "A {1}          ", empty( {1}                    ) )
   QOut( "A {}           ", empty( {}                     ) )
   QOut( "A {0}          ", empty( {0}                    ) )
   QOut( "B {|x|x+x}     ", empty( {|x|x+x}               ) )

   QOut()
return nil

function Pause()

   QOut()
   __Accept( "Pause:" )
return nil

