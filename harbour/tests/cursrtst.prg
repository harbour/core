/*
 * $Id$
 */

proc main
Local x
? "This lists the cursor modes, along with the expected shape"
? "Press a key after each example"
?
for x := 0 to 4
  CursTest( x )
next
? "Note: In Windows Console mode, Special2 can not be emulated (it is 2/3 size)"

SetCursor(1)

Proc CursTest( nCurs )
Local aTypes := { "None",    "Underline", "HalfBlock", "FullBlock",   "Upper Half"}
Local aNames := { "SC_NONE", "SC_NORMAL", "SC_INSERT", "SC_SPECIAL1", "SC_SPECIAL2"}

SetCursor( nCurs )
++nCurs
? Padr(aNames[nCurs],11), PadR(aTypes[nCurs],11), Str(SetCursor(),3)
Inkey(0)
