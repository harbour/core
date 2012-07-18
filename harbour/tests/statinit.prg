/*
 * $Id$
 */

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.net)

MEMVAR cMyPubVar

STATIC bBlock1 := {|| Hello() }
STATIC bBlock2 := {|| cMyPubVar }

PROCEDURE Main()

   PUBLIC cMyPubVar := "Printed from a PUBLIC var from a codeblock assigned to a static variable."

   Eval( bBlock1 )
   ? Eval( bBlock2 )

   RETURN

FUNCTION Hello()

   ? "Printed from a codeblock assigned to a static variable."

   RETURN NIL
