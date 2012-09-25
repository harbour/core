/*
 * $Id$
 */

#include "dbinfo.ch"

PROCEDURE Main()
   FIELD FIRST, LAST, STREET, CITY
   LOCAL n, hs

   dbCreate("_tst", { { "FIRST",       "C", 20,  0 },;
                      { "LAST",        "C", 20,  0 },;
                      { "STREET",      "C", 30,  0 },;
                      { "CITY",        "C", 30,  0 },;
                      { "STATE",       "C",  2,  0 },;
                      { "ZIP",         "C", 10,  0 },;
                      { "HIREDATE",    "D",  8,  0 },;
                      { "MARRIED",     "L",  1,  0 },;
                      { "AGE",         "N",  2,  0 },;
                      { "SALARY",      "N",  6,  0 },;
                      { "NOTES",       "C", 70,  0 } } )
   USE _tst
   HSX_CREATE( "_tst", "FIRST+LAST+STREET+CITY", 2, 0, .T., 3 )
   APPEND FROM test

   /* Look for all records which have 'SHERMAN' string inside */
   hs := HSX_HANDLE( "_tst" )
   HS_SET( hs, "SHERMAN" )
   DO WHILE ( n := HS_NEXT( hs ) ) > 0
      DBGOTO( n )
      IF HS_VERIFY( hs ) > 0
         ? RTRIM( FIRST+LAST+STREET+CITY )
      ENDIF
   ENDDO
   WAIT

   /* Does RDD support Record Map Filters? */
   IF DBINFO( DBI_RM_SUPPORTED )
      /* if yest then let set filter for all records with 'SHERMAN'
         word and look at them in browser */
      HS_FILTER( hs, "SHERMAN" )
      DBGOTOP()
      BROWSE()
   ENDIF
   RETURN
