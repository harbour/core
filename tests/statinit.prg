/* Donated to the public domain by Viktor Szakats (vszakats.net/harbour) */

MEMVAR p_cMyPubVar

STATIC s_bBlock1 := {|| Hello() }
STATIC s_bBlock2 := {|| p_cMyPubVar }

PROCEDURE Main()

   PUBLIC p_cMyPubVar := "Printed from a PUBLIC var from a codeblock assigned to a static variable."

   Eval( s_bBlock1 )
   ? Eval( s_bBlock2 )

   RETURN

STATIC PROCEDURE Hello()

   ? "Printed from a codeblock assigned to a static variable."

   RETURN
