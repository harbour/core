
PROCEDURE Main()

   LOCAL x

   ? "This lists the cursor modes, along with the expected shape"
   ? "Press a key after each example"
   ?
   FOR x := 0 TO 4
      CursTest( x )
   NEXT
   ? "Note: In Windows Console mode, Special2 can not be emulated (it is 2/3 size)"

   SetCursor( 1 )

   RETURN

PROCEDURE CursTest( nCurs )

   LOCAL aTypes := { "None",    "Underline", "HalfBlock", "FullBlock",   "Upper Half" }
   LOCAL aNames := { "SC_NONE", "SC_NORMAL", "SC_INSERT", "SC_SPECIAL1", "SC_SPECIAL2" }

   SetCursor( nCurs )
   ++nCurs
   ? PadR( aNames[ nCurs ], 11 ), PadR( aTypes[ nCurs ], 11 ), Str( SetCursor(), 3 )
   Inkey( 0 )

   RETURN
