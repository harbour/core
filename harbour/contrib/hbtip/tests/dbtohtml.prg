/*
 * $Id$
 */

/*
 * This example demonstrates operator overloading for
 * creating a HTML document.
 */

#require "hbtip"

PROCEDURE Main

   LOCAL oDoc, oNode, oTable, oRow, oCell
   LOCAL i, j

   CLS

   BEGIN SEQUENCE
      USE ( hb_DirBase() + ".." + hb_ps() + ".." + hb_ps() + ".." + hb_ps() + "tests" + hb_ps() + ;
         "test.dbf" )
   RECOVER
      ? "Error: Database not found test.dbf"
      QUIT
   END SEQUENCE

   oDoc          := THtmlDocument():new()

   /* Operator "+" creates a new node */
   oNode         := oDoc:head + "meta"
   oNode:name    := "Generator"
   oNode:content := "THtmlDocument"

   /* Operator ":" returns first "h1" from body (creates if not existent) */
   oNode         := oDoc:body:h1
   oNode:text    := "My address book"

   /* Operator "+" creates a new <p> node */
   oNode         := oDoc:body + "p"

   /* Operator "+" creates a new <font> node with attribute */
   oNode         := oNode   + 'font size="5"'
   oNode:text    := "This is a "

   /* Operator "+" creates a new <b> node */
   oNode         := oNode   + "b"

   /* Operator "+" creates a new <font> node with attribute */
   oNode         := oNode   + "font color='blue'"
   oNode:text    := "sample "

   /* Operator "-" closes 2nd <font>, result is <b> node */
   oNode         := oNode   - "font"

   /* Operator "-" closes <b> node, result is 1st <font> node */
   oNode         := oNode   - "b"

   oNode:text    := "database!"

   /* Operator "-" closes 1st <font> node, result is <p> node */
   oNode         := oNode   - "font"

   oNode         := oNode   + "hr"
   HB_SYMBOL_UNUSED( oNode )

   /* Operator ":" returns first "table" from body (creates if not existent) */
   oTable        := oDoc:body:table
   oTable:attr   := 'border="0" width="100%" cellspacing="0" cellpadding="0"'

   oRow          := oTable  + 'tr bgcolor="lightcyan"'
   FOR i := 1 TO FCount()
      oCell     := oRow + "th"
      oCell:text := FieldName( i )
      oCell     := oCell - "th"
      HB_SYMBOL_UNUSED( oCell )
   NEXT

   oRow := oRow - "tr"
   HB_SYMBOL_UNUSED( oRow )

   FOR i := 1 TO 10
      oRow         := oTable + "tr"
      oRow:bgColor := iif( RecNo() % 2 == 0, "lightgrey", "white" )

      FOR j := 1 TO FCount()
         oCell      := oRow + "td"
         oCell:text := FieldGet( j )
         oCell      := oCell - "td"
         HB_SYMBOL_UNUSED( oCell )
      NEXT

      oRow := oRow - "tr"
      HB_SYMBOL_UNUSED( oRow )

      SKIP
   NEXT

   oNode := oDoc:body  + "hr"
   HB_SYMBOL_UNUSED( oNode )
   oNode := oDoc:body  + "p"

   oNode:text := "10 records from database " + Alias() + ".dbf"

   dbCloseArea()

   IF oDoc:writeFile( "address.html" )
      ? "File created: address.html"
   ELSE
      ? "Error: ", FError()
   ENDIF

   WAIT
   ? tip_HtmlToStr( oDoc:body:getText() )

   hb_run( "address.html" )

   RETURN
