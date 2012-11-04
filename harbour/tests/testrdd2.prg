/*
 * $Id$
 */

#include "directry.ch"
#include "ord.ch"

#define MAX_TEST_RECS   100
#define INDEX_KEY_CHAR  CHAR + Str( NUM ) + DToS( DATE )
#define INDEX_KEY_NUM   NUM
#define INDEX_KEY_DATE  DATE
#define INDEX_KEY_LOG   LOG

#ifdef WITH_ADS
#include "ads.ch"

EXTERNAL _ADS
#endif

EXTERNAL DBFNTX
EXTERNAL DBFCDX

PROCEDURE Main( cRDDType, cAdsMode )

   LOCAL cRDD := "", aStruct, xTemp, bMemoText

   FIELD CHAR, NUM, DATE, LOG

   bMemoText := {|| "This is memo #" + hb_ntos( RecNo() ) + "." + hb_eol() + ;
      hb_eol() + ;
      "This is a very long string. " + ;
      "This may seem silly however strings like this are still " + ;
      "used. Not by good programmers though, but I've seen " + ;
      "stuff like this used for Copyright messages and other " + ;
      "long text. What is the point to all of this you'd say. " + ;
      "Well I am coming to the point right now, the constant " + ;
      "string is limited to 256 characters and this string is " + ;
      "a lot bigger. Do you get my drift ? If there is somebody " + ;
      "who has read this line upto the very end: Esto es un " + ;
      "sombrero grande ridiculo." + hb_eol() + "/" + hb_eol() + "[;-)" + hb_eol() + "\" }

   DO CASE

   CASE Empty( cRDDType )

      NotifyUser( "Usage: TESTRDD2 RDDTYPE [ADSMODE]" + hb_eol() + ;
         hb_eol() + ;
         "RDDTYPE = DBFNTX, DBFCDX, ADSCDX, ADSNTX or ADSADT" + hb_eol() + ;
         hb_eol() + ;
         "ADSMODE = LOCAL or SERVER (only applies to ADSCDX, ADSNTX and ADSADT)" + hb_eol() + ;
         "(If specify SERVER, must be run from a drive suported by ADS server)", .T. )

#ifdef WITH_ADS

   CASE Left( cRDDType := Upper( AllTrim( cRDDType ) ), 3 ) == "ADS"

      rddRegister( "ADS", 1 )
      rddSetDefault( "ADS" )

      IF Empty( cADSMode )
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

      // TEST: DBAppend()/APPEND BLANK

      APPEND BLANK

      // TEST: REPLACE

      REPLACE CHAR WITH Chr( Asc( "A" ) + Val( SubStr( hb_ntos( RecNo() ), 2, 1 ) ) ) + ;
         " RECORD " + hb_ntos( RecNo() )

      // TEST: Direct field assigment

      MYTEST->NUM  := ( iif( RecNo() % 2 > 0, - 1, 1 ) * RecNo() ) + ( RecNo() / 1000 )
      MYTEST->DATE := Date() + Int( FIELD->NUM )
      MYTEST->LOG  := ( FIELD->NUM < 0 )
      MYTEST->MEMO := Eval( bMemoText )

   ENDDO

   // TEST: LastRec()

   IF ! LastRec() == MAX_TEST_RECS
      NotifyUser( "DbAppend and/or LastRec failed" )
   ENDIF

   // TEST: DbGoBotom()/GO BOTTOM

   GO BOTTOM

   IF ! RecNo() == MAX_TEST_RECS
      NotifyUser( "DbGoBottom failed" )
   ENDIF

   // TEST: DbGoTop()/GO TOP

   GO TOP

   IF ! RecNo() == 1
      NotifyUser( "DbGoTop failed" )
   ENDIF

   // Now check each and every record for accuracy

   DO WHILE ! EOF()

      // TEST: Field access

      IF ! RTrim( FIELD->CHAR ) == Chr( Asc( "A" ) + Val( SubStr( hb_ntos( RecNo() ), 2, 1 ) ) ) + ;
            " RECORD " + hb_ntos( RecNo() ) .OR. ;
            ! FIELD->NUM == ( iif( RecNo() % 2 > 0, - 1, 1 ) * RecNo() ) + ( RecNo() / 1000 ) .OR. ;
            ! FIELD->DATE == Date() + Int( FIELD->NUM ) .OR. ;
            ! FIELD->LOG == ( FIELD->NUM < 0 ) .OR. ;
            ! FIELD->MEMO == Eval( bMemoText )

         NotifyUser( "Data in table is incorrect" )

      ENDIF

      SKIP

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

   SET ORDER TO 1
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/1" )
   ENDIF

   SET ORDER TO 2
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/2" )
   ENDIF

   SET ORDER TO 3
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/3" )
   ENDIF

   SET ORDER TO 4
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == MAX_TEST_RECS
      NotifyUser( "Bad DBOI_KEYCOUNT/4" )
   ENDIF

   // TEST: Character index
   SET ORDER TO 1
   GO TOP
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_CHAR
      NotifyUser( "Bad DBOI_KEYVAL (CHAR)" )
   ENDIF

   // TEST: Positive index key
   SET ORDER TO 2
   LOCATE for FIELD->NUM > 0
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_NUM
      NotifyUser( "Bad DBOI_KEYVAL (NUM)" )
   ENDIF

   // TEST: Negative index key
   SET ORDER TO 2
   LOCATE for FIELD->NUM < 0
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_NUM
      NotifyUser( "Bad DBOI_KEYVAL (NUM)" )
   ENDIF

   // TEST: Date index
   SET ORDER TO 3
   GO BOTTOM
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_DATE
      NotifyUser( "Bad DBOI_KEYVAL (DATE)" )
   ENDIF

   // TEST: Logical index
   SET ORDER TO 4
   GO TOP
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_LOG
      NotifyUser( "Bad DBOI_KEYVAL (LOG/1)" )
   ENDIF
   GO BOTTOM
   IF ! dbOrderInfo( DBOI_KEYVAL ) == INDEX_KEY_LOG
      NotifyUser( "Bad DBOI_KEYVAL (LOG/2)" )
   ENDIF

   // TEST: EXACT with a locate

   SET ORDER TO 0

   SET EXACT ON
   LOCATE for FIELD->CHAR = "J RECORD"
   IF ! EOF()
      NotifyUser( "LOCATE with EXACT ON failed" )
   ENDIF

   SET EXACT OFF
   LOCATE for FIELD->CHAR = "J RECORD"
   IF EOF()
      NotifyUser( "LOCATE with EXACT OFF failed" )
   ENDIF

   // TEST: EXACT with an index (also tests COUNT)

   SET EXACT ON
   SET ORDER TO 0
   COUNT FOR RTrim( FIELD->CHAR ) = "A RECORD 1" TO xTemp  // Get proper count
   INDEX ON CHAR TO test_e.idx FOR RTrim( FIELD->CHAR ) = "A RECORD 1" ADDITIVE
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == xTemp
      NotifyUser( "Bad conditional index count with EXACT ON" )
   ENDIF

   SET EXACT OFF
   SET ORDER TO 0
   COUNT FOR RTrim( FIELD->CHAR ) = "A RECORD 1" TO xTemp  // Get proper count
   INDEX ON CHAR TO test_e.idx FOR RTrim( FIELD->CHAR ) = "A RECORD 1" ADDITIVE
   IF ! dbOrderInfo( DBOI_KEYCOUNT ) == xTemp
      NotifyUser( "Bad conditional index count with EXACT OFF" )
   ENDIF

   //
   //
   // *********************************************
   // P U T   M O R E   R D D   T E S T S   H E R E
   // *********************************************
   //
   //
   //

   // TEST: dbCloseArea()

   MYTEST->( dbCloseArea() )

   IF SELECT( "MYTEST" ) > 0
      NotifyUser( "Failed to close table" )
   ENDIF

   NotifyUser( "Test PASSED! :-)", .T. )

   RETURN

PROCEDURE ErrorSys()

   ErrorBlock( {| e | MyError( e ) } )

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

   IF ! Len( a1 ) == Len( a2 )
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

STATIC PROCEDURE NotifyUser( cErr, lNotError )

   HB_SYMBOL_UNUSED( lNotError )

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
