#require "rddsql"

REQUEST SQLMIX

PROCEDURE Main()

   CLS

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
#endif

   rddSetDefault( "SQLMIX" )
   dbCreate( "persons", { { "NAME", "C", 20, 0 }, { "FAMILYNAME", "C", 20, 0 }, { "BIRTH", "D", 8, 0 }, { "AMOUNT", "N", 9, 2 } }, , .T., "persons" )

   dbAppend(); AEval( { PadR( "Bil", 20 ),  PadR( "Gatwick", 20 ),  hb_SToD( "19650124" ), 123456.78 }, {| X, Y | FieldPut( Y, X ) } )
   dbAppend(); AEval( { PadR( "Tom", 20 ),  PadR( "Heathrow", 20 ), hb_SToD( "19870512" ),   9086.54 }, {| X, Y | FieldPut( Y, X ) } )
   dbAppend(); AEval( { PadR( "John", 20 ), PadR( "Weber", 20 ),    hb_SToD( "19750306" ),   2975.45 }, {| X, Y | FieldPut( Y, X ) } )
   dbAppend(); AEval( { PadR( "Sim", 20 ),  PadR( "Simsom", 20 ),   hb_SToD( "19930705" ),  32975.37 }, {| X, Y | FieldPut( Y, X ) } )

   dbGoTop()
   Browse()

   INDEX ON FIELD->AMOUNT TO amount
   dbGoTop()
   Browse()
   dbCloseAll()

   RETURN

PROCEDURE RDDSYS() ; RETURN  /* must be a public function */
