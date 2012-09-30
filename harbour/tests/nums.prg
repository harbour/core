/*
 * $Id$
 */

// Testing the different numeric formats Harbour produces

PROCEDURE Main()

   LOCAL a := 0         // it should generate a _ZERO pcode opcode
   LOCAL b := 123       // it should generate a _PUSHINT pcode opcodes
   LOCAL c := 50000     // it should generate a _PUSHLONG pcode opcodes
   LOCAL d := 12000.123 // it should generate a _PUSHDOUBLE pcode opcodes
   LOCAL e := 0xABAB    // Automatic support for hexadecimal numbers
   LOCAL f := .12

   QOut( a )
   QOut( b )
   QOut( c )
   QOut( d )
   QOut( e )
   QOut( f )

   RETURN
