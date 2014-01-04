// Class(y) Class Symbol documentation is located at:
// http://www.clipx.net/ng/classy/ngdebc.php

PROCEDURE Main()

   LOCAL oSym := Symbol():New( "QOut" )

   ? "Now test the :Exec() method"

   oSym:Exec( "This string is being printed by QOut()" )
   oSym:Exec( "which is being invoked by the :Exec()" )
   oSym:Exec( "method in the Symbol class." )

   ?
   ? "symbol name:", oSym:name

   ? "Comparing 'QOut' symbol with 'xOut' symbol"
   ? oSym:IsEqual( Symbol():New( "xOut" ) )

   ? "done!"
   ?

   RETURN
