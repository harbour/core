/* Demo of ADS Connection handling and Data Dictionaries */

#require "rddads"

REQUEST ADS

#if defined( __HBDYNLOAD__RDDADS__ )
#include "rddads.hbx"
#endif

PROCEDURE Main()

   LOCAL n
   LOCAL cErr, cStr
   LOCAL aStru := { { "ID", "A", 1, 0 }, { "Name", "C", 50, 0 }, { "address", "C", 50, 0 }, { "city", "C", 30, 0 }, { "Age", "n", 3, 0 } }
   LOCAL hConnection1

#if defined( __HBDYNLOAD__RDDADS__ )
   LOCAL l := hb_libLoad( hb_libName( "rddads" + hb_libPostfix() ) )

   hb_rddADSRegister()

   HB_SYMBOL_UNUSED( l )
#elif defined( __HBSCRIPT__HBSHELL )
   hb_rddADSRegister()
#endif

   CLS

   rddSetDefault( "ADSADT" )
   AdsSetServerType( 7 )
   SET FILETYPE TO ADT

   ? "Default connection is 0:", AdsConnection()

   FErase( "harbour.add" )
   FErase( "harbour.ai" )
   FErase( "harbour.am" )
   FErase( "table1.adt" )
   FErase( "table1.adi" )
   FErase( "table2.adt" )
   FErase( "table2.adi" )

   // now Create a Data dictionary and the files if not exist
   IF ! hb_FileExists( "harbour.add" )

      AdsDDCreate( "harbour.add", , "Harbour ADS demo for data dictionary" )
      // This also creates an Administrative Handle that is set as the default
      ? "Default connection is now this admin handle:", AdsConnection()
      AdsDisconnect()   // disconnect current default.
      // if you wanted to retain this connection for later, you could use
      // hAdminCon := AdsConnection( 0 )
      // This get/set call would return the current connection, then set it to 0

      ? "Default connection is now this handle (zero):", AdsConnection()

      // now create two free tables with same structure
      dbCreate( "table1", aStru )
      dbCreate( "table2", aStru )
      // now create an index
      USE table1 NEW
      INDEX ON FIELD->id TAG codigo
      USE

      USE table2 NEW
      INDEX ON FIELD->id TAG codigo
      USE
   ENDIF

   // now the magic
   IF AdsConnect60( "harbour.add", 7; /* All types of connection*/
      , "ADSSYS", "", , @hConnection1 )
      // The connection handle to harbour.add is now stored in hConnection1,
      // and this is now the default connection

      ? "Default connection is now this handle:", AdsConnection()
      ? "   Is it a Data Dict connection?  (ADS_DATABASE_CONNECTION=6, "
      ? "      ADS_SYS_ADMIN_CONNECTION=7):", AdsGetHandleType()

      // Add one user
      AdsDDCreateUser( , "Luiz", "papael", "This is user Luiz" )


      IF AdsDDGetUserProperty( "Luiz", ADS_DD_COMMENT, @cStr, hConnection1 )
         ? "User comment:", cStr
      ELSE
         ? "Error retrieving User comment"
      ENDIF


      ? "Add the tables"
      AdsDDAddTable( "table1", "table1.adt", "table1.adi" )
      ?
      IF ! AdsDDAddTable( "Customer Data", "table2.adt", "table2.adi" )
         // notice the "long table name" for file table2.adt.  Later open it with "Customer Data" as the table name
         ? "Error adding table:", AdsGetLastError( @cErr ), cErr
      ENDIF
      ? "Set new admin pword on default  connection:", AdsDDSetDatabaseProperty( ADS_DD_ADMIN_PASSWORD, "newPWord"  )
      ? "Set new admin pword on explicit connection:", AdsDDSetDatabaseProperty( ADS_DD_ADMIN_PASSWORD, "newPWord", hConnection1  )
      ? "Clear admin pword:", AdsDDSetDatabaseProperty( ADS_DD_ADMIN_PASSWORD, ""  )

   ELSE
      ? "Error connecting to harbour.add!"
   ENDIF
   AdsDisconnect( hConnection1 )
   hConnection1 := NIL     // you should always reset a variable holding a handle that is no longer valid

   ? "Default connection is back to 0:", AdsConnection()
   ? "Is a Data Dict connection? (AE_INVALID_HANDLE = 5018):", AdsGetHandleType()

   // now open the tables and put some data

   IF AdsConnect60( "harbour.add", 7; /* All types of connection*/
      , "Luiz", "papael", , @hConnection1 )
      ? "Default connection is now this handle:", AdsConnection()
      ? "Connection type?", AdsGetHandleType()

      FOR n := 1 TO 100
         IF AdsCreateSQLStatement( "Data2", 3 )
            IF ! AdsExecuteSQLDirect( " insert into table1( name, address, city, age ) VALUES( '" + StrZero( n ) + "','" + StrZero( n ) + "','" + StrZero( n ) + "'," + hb_ntos( n ) + ")" )
               ShowAdsError()
            ENDIF
            USE
         ENDIF
      NEXT

      FOR n := 1 TO 100
         IF AdsCreateSQLStatement( "Data1", 3 )
            IF ! AdsExecuteSQLDirect( " insert into " + '"Customer Data"' + "( name, address, city, age ) VALUES( '" + StrZero( n ) + "','" + StrZero( n ) + "','" + StrZero( n ) + "'," + hb_ntos( n ) + ")" )
               ShowAdsError()
            ENDIF
            USE
         ENDIF
      NEXT


      // AdsUseDictionary( .T. )  this function no longer is needed; the system knows if it's using a Data Dictionary connection

      // Open the "long table name" for table2
      dbUseArea( .T., , "Customer Data", "custom", .T., .F. )
      ? "Press a key to browse", Alias()
      Inkey( 0 )
      Browse()
      USE
      USE table1 NEW
      Browse()
      USE
   ENDIF

   AdsDisconnect( hConnection1 )

   RETURN

PROCEDURE ShowAdsError()

   LOCAL cMsg

   AdsGetLastError( @cMsg )

   Alert( cMsg )

   RETURN
