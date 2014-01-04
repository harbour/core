/*
 * Harbour Project source code:
 *    HiPer-SEEK indexing/search test
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "dbinfo.ch"

// REQUEST RMDBFCDX

PROCEDURE Main()

   FIELD FIRST, LAST, STREET, CITY
   LOCAL n, hs

   IF AScan( rddList( 1 ), "RMDBFCDX" ) != 0
      rddSetDefault( "RMDBFCDX" )
   ENDIF

   USE test shared
   hs := hs_Index( "test", "FIRST+LAST+STREET+CITY", 2, 0, , .T., 3 )

   /* Look for all records which have 'SHERMAN' string inside */
   hs_Set( hs, "SHERMAN" )
   DO WHILE ( n := hs_Next( hs ) ) > 0
      dbGoto( n )
      IF hs_Verify( hs ) > 0
         ? RTrim( FIRST + LAST + STREET + CITY )
      ENDIF
   ENDDO
   WAIT

   /* Does RDD support Record Map Filters? */
   IF dbInfo( DBI_RM_SUPPORTED )
      /* if yest then let set filter for all records with 'SHERMAN'
         word and look at them in browser */
      hs_Filter( hs, "SHERMAN" )
      dbGoTop()
      Browse()
   ENDIF
   hs_Close( hs )

   RETURN
