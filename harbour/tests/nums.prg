//
// $Id$
//

// Testing the different numeric formats Harbour produces

function main()

   local a := 0         // it should generate a _ZERO pcode opcode
   local b := 123       // it should generate a _PUSHINT pcode opcodes
   local c := 50000     // it should generate a _PUSHLONG pcode opcodes
   local d := 12000.123 // it should generate a _PUSHDOUBLE pcode opcodes
   local e := 0xABAB    // Automatic support for hexadecimal numbers
   local f := .12

   QOut( a )
   QOut( b )
   QOut( c )
   QOut( d )
   QOut( e )

return nil
