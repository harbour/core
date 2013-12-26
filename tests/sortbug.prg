PROCEDURE Main()

   dbCreate( "testsort", { { "ITEM", "N", 10, 2 } } )

   USE testsort
   dbAppend(); FIELD->ITEM := 5.00
   dbAppend(); FIELD->ITEM := -5.12
   dbAppend(); FIELD->ITEM := 6.00
   dbAppend(); FIELD->ITEM := 7.00
   dbAppend(); FIELD->ITEM := -5.00

   SORT TO tempsort ON FIELD->ITEM /D

   USE tempsort
   dbEval( {|| QOut( FIELD->ITEM ) } )

   /* TOFIX: To work like in Cl*pper: 7.00, 6.00, 5.00, -5.00, -5.12 */

   dbCloseArea()
   hb_dbDrop( "testsort" )
   hb_dbDrop( "tempsort" )

   RETURN
