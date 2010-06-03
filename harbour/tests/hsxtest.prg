/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HiPer-SEEK indexing/search test
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "dbinfo.ch"

//REQUEST RMDBFCDX

PROCEDURE Main()
   FIELD FIRST, LAST, STREET, CITY
   LOCAL n, hs

   if ascan( rddList(1), "RMDBFCDX" ) != 0
      rddSetDefault( "RMDBFCDX" )
   endif

   use test shared
   hs := HS_INDEX( "test", "FIRST+LAST+STREET+CITY", 2, 0, , .T., 3 )

   /* Look for all records which have 'SHERMAN' string inside */
   HS_SET( hs, "SHERMAN" )
   while ( n := HS_NEXT( hs ) ) > 0
      dbgoto( n )
      if HS_VERIFY( hs ) > 0
         ? rtrim( FIRST+LAST+STREET+CITY )
      endif
   enddo
   wait

   /* Does RDD support Record Map Filters? */
   if dbinfo( DBI_RM_SUPPORTED )
      /* if yest then let set filter for all records with 'SHERMAN'
         word and look at them in browser */
      HS_FILTER( hs, "SHERMAN" )
      dbgotop()
      browse()
   endif
   HS_CLOSE( hs )
RETURN
