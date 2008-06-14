/*
 * $Id$
 */

// This example demonstrates operator overloading for
// creating a HTML document.

PROCEDURE Main
   LOCAL oDoc, oNode, oTable, oRow, oCell
   LOCAL nDll, pApi
   CLS

   TRY
      USE ..\Test.dbf
   CATCH
      ? "Error: Database not found TEST.DBF"
      QUIT
   END

   oDoc          := THtmlDocument():new()

   // Operator "+" creates a new node
   oNode         := oDoc:head + "meta"
   oNode:name    := "Generator"
   oNode:content := "THtmlDocument"

   // Operator ":" returns first "h1" from body (creates if not existent)
   oNode         := oDoc:body:h1
   oNode:text    := "My address book"

   // Operator "+" creates a new <p> node
   oNode         := oDoc:body + "p"

   // Operator "+" creates a new <font> node with attribute
   oNode         := oNode   + 'font size="5"'
   oNode:text    := "This is a "

   // Operator "+" creates a new <b> node
   oNode         := oNode   + "b"

   // Operator "+" creates a new <font> node with attribute
   oNode         := oNode   + "font color='blue'"
   oNode:text    := "sample "

   // Operator "-" closes 2nd <font>, result is <b> node
   oNode         := oNode   - "font"

   // Operator "-" closes <b> node, result is 1st <font> node
   oNode         := oNode   - "b"

   oNode:text    := "database!"

   // Operator "-" closes 1st <font> node, result is <p> node
   oNode         := oNode   - "font"

   oNode         := oNode   + "hr"

   // Operator ":" returns first "table" from body (creates if not existent)
   oTable        := oDoc:body:table
   oTable:attr   := 'border="0" width="100%" cellspacing="0" cellpadding="0"'

   oRow          := oTable  + 'tr bgcolor="lightcyan"'
   FOR i:=1 TO FCount()
       oCell     := oRow + "th"
       oCell:text:= FieldName(i)
   NEXT

   FOR i:=1 TO 10
      oRow         := oTable + "tr"
      oRow:bgColor := IIf( Recno() % 2 == 0, "lightgrey", "white" )

      FOR j:=1 TO FCount()
         oCell      := oRow + "td"
         oCell:text := FieldGet(j) 
      NEXT

      SKIP
   NEXT

   oNode := oDoc:body  + "hr"
   oNode := oDoc:body  + "p"

   oNode:text := "10 records from database " + ALias() + ".dbf"

   DbCloseArea()

   IF oDoc:writeFile( "Address.html" )
      ? "File created: Address.html" 
   ELSE
      ? "Error: ", FError()
   ENDIF

   WAIT
   ? HtmlToOem( oDoc:body:getText() )

   nDll := DllLoad( "Shell32.dll" )
   pApi := GetProcAddress( nDll, "ShellExecute" )

   CallDll( pApi, 0, "open", "Address.html", NIL, "", 1 )

   DllUnload( nDll )    
RETURN

