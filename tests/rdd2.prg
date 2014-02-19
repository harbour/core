#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#include "directry.ch"
#include "ord.ch"

#define MAX_TEST_RECS   100
#define INDEX_KEY_CHAR  field->CHAR + Str( field->NUM ) + DToS( field->DATE )
#define INDEX_KEY_NUM   field->NUM
#define INDEX_KEY_DATE  field->DATE
#define INDEX_KEY_LOG   field->LOG

#ifdef WITH_ADS
#include "ads.ch"
EXTERNAL _ADS
#endif

EXTERNAL DBFNTX
EXTERNAL DBFCDX

PROCEDURE Main( cRDDType, cAdsMode )

   LOCAL cRDD := "", aStruct, xTemp, bMemoText

   FIELD CHAR, NUM, DATE, LOG

   ErrorBlock( {| e | MyError( e ) } )

   bMemoText := {|| "This is memo #" + hb_ntos( RecNo() ) + "." + hb_eol() + ;
      hb_eol() + ;
      "This is a very long string. " + ;
      "This may seem silly however strings like this are still " + ;
      "used. Not by good programmers though, but I've seen " + ;
      "stuff like this used for Copyright messages and other " + ;
      "long text. What is the point to all of this you'd say. " + ;
      "Well I am coming to the point right now, the constant " + ;
      "string is limited to 256 characters and this string is " + ;
      "a lot bigger. Do you get my drift? If there is somebody " + ;
      "who has read this line upto the very end: Esto es un " + ;
      "sombrero grande ridiculo." + hb_eol() + "/" + hb_eol() + "[;-)" + hb_eol() + "\" }

   DO CASE
   CASE ! HB_ISSTRING( cRDDType )

      NotifyUser( "Usage: rdd2 RDDTYPE [ADSMODE]" + hb_eol() + ;
         hb_eol() + ;
         "RDDTYPE: DBFNTX, DBFCDX, ADSCDX, ADSNTX or ADSADT" + hb_eol() + ;
         hb_eol() + ;
         "ADSMODE: LOCAL or SERVER (only applies to ADSCDX, ADSNTX and ADSADT)" + hb_eol() + ;
         "(If specify SERVER, must be run from a drive suported by ADS server)" )

#ifdef WITH_ADS

   CASE hb_LeftIs( cRDDType := Upper( AllTrim( cRDDType ) ), "ADS" )

      rddRegister( "ADS", 1 )
      rddSetDefault( "ADS" )

      IF ! HB_ISSTRING( cADSMode )
         NotifyUser( "Missing ADS mode" )
      ENDIF

      cADSMode := Upper( AllTrim( cADSMode ) )

      DO CASE
      CASE cADSMode == "LOCAL"  ; AdsSetServerType( ADS_LOCAL_SERVER )
      CASE cADSMode == "SERVER" ; AdsSetServerType( ADS_REMOTE_SERVER )
      OTHERWISE                 ; NotifyUser( "Bad ADS mode" )
      ENDCASE

      DO CASE
      CASE cRDDType == "ADSNTX" ; AdsSetFileType( ADS_NTX ) ; cRDD := "ADSNTX"
      CASE cRDDType == "ADSADT" ; AdsSetFileType( ADS_ADT ) ; cRDD := "ADSADT"
      CASE cRDDType == "ADSCDX" ; AdsSetFileType( ADS_CDX ) ; cRDD := "ADSCDX"
      OTHERWISE                 ; NotifyUser( "Bad ADS flavor" )
      ENDCASE

      AdsLocking( .T. )
      AdsRightsCheck( .T. )

      AdsSetDefault( "" )
      AdsSetSearchPath( "" )

#endif

   CASE cRDDType == "DBFCDX" .OR. ;
        cRDDType == "DBFNTX"

      rddSetDefault( cRDD := cRDDType )

   OTHERWISE

      NotifyUser( "Bad DBF flavor" )

   ENDCASE

   // Delete test_?.* since may be changing RDD flavors (avoid conflicts)
   AEval( Directory( "test_?.*" ), {| a | hb_dbDrop( a[ F_NAME ] ) } )

   IF hb_dbExists( "test_2.dbf" )
      NotifyUser( "Cannot delete test_2.dbf" )
   ENDIF

   // TEST: dbCreate()

   dbCreate( "test_2.dbf", aStruct := { ;
      { "CHAR", "C", 30, 0 }, ;
      { "NUM",  "N", 15, 3 }, ;
      { "DATE", "D",  8, 0 }, ;
      { "LOG",  "L",  1, 0 }, ;
      { "MEMO", "M", 10, 0 } } )

   IF ! hb_dbExists( "test_2.dbf" )
      NotifyUser( "Failed to create test_2.dbf" )
   ENDIF

   // TEST: dbUseArea()/USE

   USE test_2.dbf NEW SHARED ALIAS mytest

   IF ! Alias() == "MYTEST"
      NotifyUser( "Failed to open test_2.dbf" )
   ENDIF

   // TEST: rddName()

   IF ! rddName() == cRDD
      NotifyUser( "Failed to set RDD to " + cRDD )
   ENDIF

   // TEST: dbStruct()

   IF ! CompareArray( aStruct, dbStruct() )
      NotifyUser( "Resulting table structure is not what we asked for" )
   ENDIF

   // TEST: Header()

   IF ! Header() == 194
      NotifyUser( "Header() returned wrong size (" + hb_ntos( Header() ) + " bytes)" )
   ENDIF

   // Add a mix of data to table

   DO WHILE LastRec() < MAX_TEST_RECS

      // TEST: dbAppend()

      dbAppend()

      // TEST: REPLACE

      REPLACE CHAR WITH Chr( Asc( "A" ) + Val( SubStr( hb_ntos( RecNo() ), 2, 1 ) ) ) + ;
         " RECORD " + hb_ntos( RecNo() )

      // TEST: Direct field assigment

      MYTEST->NUM  := ( iif( RecNo() % 2 > 0, - 1, 1 ) * RecNo() ) + ( RecNo() / 1000 )
      MYTEST->DATE := Date() + Int( field->NUM )
      MYTEST->LOG  := ( field->NUM < 0 )
      MYTEST->MEMO := Eval( bMemoText )

   ENDDO

   // TEST: LastRec()

   IF ! LastRec() == MAX_TEST_RECS
      NotifyUser( "dbAppend() and/or LastRec() failed" )
   ENDIF

   // TEST: dbGoBottom()

   dbGoBottom()

   IF ! RecNo() == MAX_TEST_RECS
      NotifyUser( "dbGoBottom() failed" )
   ENDIF

   // TEST: dbGoTop()

   dbGoTop()

   IF ! RecNo() == 1
      NotifyUser( "dbGoTop() failed" )
   ENDIF

   // Now check each and every record for accuracy

   DO WHILE ! Eof()

      // TEST: Field access

      IF ! RTrim( field->CHAR ) == Chr( Asc( "A" ) + Val( SubStr( hb_ntos( RecNo() ), 2, 1 ) ) ) + ;
            " RECORD " + hb_ntos( RecNo() ) .OR. ;
            ! field->NUM == ( iif( RecNo() % 2 > 0, - 1, 1 ) * RecNo() ) + ( RecNo() / 1000 ) .OR. ;
            ! field->DATE == Date() + Int( field->NUM ) .OR. ;
            ! field->LOG == ( field->NUM < 0 ) .OR. ;
            ! field->MEMO == Eval( bMemoText )

         NotifyUser( "Data in table is incorrect" )

      ENDIF

      dbSkip()

   ENDDO

   // TEST: Index creation

   INDEX ON INDEX_KEY_CHAR TO test_c.idx
   INDEX ON INDEX_KEY_NUM  TO test_n.idx ADDITIVE
   INDEX ON INDEX_KEY_DATE TO test_d.idx ADDITIVE
   INDEX ON INDEX_KEY_LOG  TO test_l.idx ADDITIVE

   // TEST: IndexOrd()

   IF ! IndexOrd() == 4
      NotifyUser( "Bad IndexOrd()" )
   ENDIF

   // TEST: DBOI_KEYCOUNT

   ordSetFocus( 1 )
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/1" )
   ENDIF

   ordSetFocus( 2 )
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/2" )
   ENDIF

   ordSetFocus( 3 )
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/3" )
   ENDIF

   ordSetFocus( 4 )
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/4" )
   ENDIF

   // TEST: Character index
   ordSetFocus( 1 )
   dbGoTop()
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_CHAR
      NotifyUser( "Bad DBOI_KEYVAL (CHAR)" )
   ENDIF

   // TEST: Positive index key
   ordSetFocus( 2 )
   LOCATE FOR field->NUM > 0
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_NUM
      NotifyUser( "Bad DBOI_KEYVAL (NUM)" )
   ENDIF

   // TEST: Negative index key
   ordSetFocus( 2 )
   LOCATE FOR field->NUM < 0
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_NUM
      NotifyUser( "Bad DBOI_KEYVAL (NUM)" )
   ENDIF

   // TEST: Date index
   ordSetFocus( 3 )
   dbGoBottom()
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_DATE
      NotifyUser( "Bad DBOI_KEYVAL (DATE)" )
   ENDIF

   // TEST: Logical index
   ordSetFocus( 4 )
   dbGoTop()
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_LOG
      NotifyUser( "Bad DBOI_KEYVAL (LOG/1)" )
   ENDIF
   dbGoBottom()
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_LOG
      NotifyUser( "Bad DBOI_KEYVAL (LOG/2)" )
   ENDIF

   // TEST: EXACT with a locate

   ordSetFocus( 0 )

   SET EXACT ON
   LOCATE FOR field->CHAR = "J RECORD"  /* hb_LeftIs() */
   IF ! Eof()
      NotifyUser( "LOCATE with EXACT ON failed" )
   ENDIF

   SET EXACT OFF
   LOCATE FOR field->CHAR = "J RECORD"  /* hb_LeftIs() */
   IF Eof()
      NotifyUser( "LOCATE with EXACT OFF failed" )
   ENDIF

   // TEST: EXACT with an index (also tests COUNT)

   SET EXACT ON
   ordSetFocus( 0 )
   COUNT FOR RTrim( field->CHAR ) = "A RECORD 1" TO xTemp  // Get proper count  /* hb_LeftIs() */
   INDEX ON field->CHAR TO test_e.idx FOR RTrim( field->CHAR ) = "A RECORD 1" ADDITIVE  /* hb_LeftIs() */
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == xTemp
      NotifyUser( "Bad conditional index count with EXACT ON" )
   ENDIF

   SET EXACT OFF
   ordSetFocus( 0 )
   COUNT FOR RTrim( field->CHAR ) = "A RECORD 1" TO xTemp  // Get proper count  /* hb_LeftIs() */
   INDEX ON field->CHAR TO test_e.idx FOR RTrim( field->CHAR ) = "A RECORD 1" ADDITIVE  /* hb_LeftIs() */
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == xTemp
      NotifyUser( "Bad conditional index count with EXACT OFF" )
   ENDIF

   //
   // PUT MORE RDD TESTS HERE
   //

   // TEST: dbCloseArea()

   MYTEST->( dbCloseArea() )

   IF Select( "MYTEST" ) > 0
      NotifyUser( "Failed to close table" )
   ENDIF

   NotifyUser( "Test PASSED! :-)" )

   RETURN

STATIC PROCEDURE MyError( e )

   LOCAL i := 1 /* Start are "real" error */
   LOCAL cErr

   cErr := "Runtime error" + hb_eol() + ;
      hb_eol() + ;
      "Gencode: " + hb_ntos( e:GenCode ) + hb_eol() + ;
      "Desc: " + e:Description +  + hb_eol() + ;
      "Sub-system: " + hb_ntos( e:SubCode ) + hb_eol() + ;
      hb_eol() + ;
      "Call trace:" + hb_eol() + ;
      hb_eol()

   DO WHILE ! Empty( ProcName( ++i ) )
      cErr += RTrim( ProcName( i ) ) + "(" + hb_ntos( ProcLine( i ) ) + ")" + hb_eol()
   ENDDO

   NotifyUser( cErr )  // Calls quit

   RETURN

STATIC FUNCTION CompareArray( a1, a2 )

   LOCAL i, j

   IF Len( a1 ) != Len( a2 )
      RETURN .F.
   ENDIF

   FOR i := 1 TO Len( a1 )
      FOR j := 1 TO Len( a1[ i ] )
         IF ! a1[ i, j ] == a2[ i, j ]
            RETURN .F.
         ENDIF
      NEXT
   NEXT

   RETURN .T.

STATIC PROCEDURE NotifyUser( cErr )

   ? cErr

   dbCloseAll()

   hb_dbDrop( "test_2" )
   hb_dbDrop( "test_e.idx" )
   hb_dbDrop( "test_c.idx" )
   hb_dbDrop( "test_n.idx" )
   hb_dbDrop( "test_d.idx" )
   hb_dbDrop( "test_l.idx" )

   QUIT

   RETURN
