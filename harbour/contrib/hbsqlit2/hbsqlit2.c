/*
 * $Id$
 */

/*
 *------------------------------------------------------------------------
 *                  HARBOUR INTERFACE for SQLITE
 *------------------------------------------------------------------------
 *
 * Copyright 2003 Alejandro de Gárate <alex_degarate@hotmail.com>
 *
 * License: General Public License (GNU)
 *
 * Developed using:
 *     Harbour 0.42 or upper
 *     Borland C++ BCC 5.5.1
 *
 * History:
 *
 * Ver 0.40 30 December 2003 Fixed an opening error not detected
 *             It seems is a problem with BCC compiler.
 *             If "xxFile" database is not found, an empty file is
 *             created with the same name, given an incorrect signal
 *             to FILE() function. File is empty but exists (Oh man...)
 *             I fix it from harbour code, when have more spare time
 *             I will look in depth.
 *
 * Ver 0.30 28 December 2003 Pick tables, fields and DB structure,
 *                           you can import some dbf (not finish yet)
 *
 * Ver 0.20  5 December 2003 changes in design, A front end is started
 *             Shows database struc, table struct, field type
 *
 * Ver 0.10 26 November 2003 first intempts, open connection, list data
 *             close connection
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

/* NOTE: we need this to prevent base types redefinition */
#define _CLIPDEFS_H

#include "hbapi.h"
#include "extend.api"
#include "item.api"
#include "hbstack.h"
#include "hbapiitm.h"

#include <stdio.h>
#include <stdlib.h>  /* need it for exit() and BCC55 */

#include "sqlite.h"
#include "sqliteInt.h"

/* Public vars */
sqlite * hb_sqlite2_db;   /* public by ale */
char *   hb_sqlite2_szErrMsg = NULL;
int      hb_sqlite2_iLastErrCode = 0;    /* for use in foreign languages (translate msg) */
/* Public temporary buffer till I can find a better approach */
char *   hb_sqlite2_aDataRows[ 1024 ];
char *   hb_sqlite2_aDataCols[  255 ];
int      hb_sqlite2_iDataRows = 0;     /* records */
int      hb_sqlite2_iDataCols = 0;     /* fields */

/* INTERNAL DO NOT TOUCH !!! */
static int hb_sqlite2_callback( void * NotUsed, int argc, char ** argv, char ** azColName )
{
   HB_SYMBOL_UNUSED( NotUsed );
   HB_SYMBOL_UNUSED( argc );
   HB_SYMBOL_UNUSED( argv );
   HB_SYMBOL_UNUSED( azColName );

#if 0
   {
      int i;
      
      /* in arg is the number of data fields for each row */
      printf("argc =>%d\n", argc);
      
      for( i = 0; i < argc; i++)
      {
         printf(">>> %s = %s\n", azColName[ i ], argv[ i ] ? argv[ i ] : "NULL" );
         hb_sqlite2_aDataCols[ i ] = azColName[ i ];  /* en realidad copiar cadena a cadena */
         hb_sqlite2_aDataRows[ i ] = argv[ i ];
      }
      printf("\n");
   }
#endif

   return 0;
}

/* Returns a string explaining last error */
HB_FUNC( SQLITE_ERROR )
{
   if( hb_sqlite2_szErrMsg )
      hb_retc( hb_sqlite2_szErrMsg );
}

/* Open a database file (in SQLite format) and set a public structure */
HB_FUNC( SQLITE_OPEN )
{
   char * szDB = hb_parc( 1 );
   int   iMode = 0; /* hb_parni( 2 ); */

   HB_SYMBOL_UNUSED( iMode );

   hb_sqlite2_db = ( sqlite * ) sqlite_open( szDB, 0, &hb_sqlite2_szErrMsg );

   /* It seems Borland BCC55 do not return a correct pointer to a sqlite
    / struc, thus I can't check if database was successfully open :( */

#if 0
   ( sqlite * )
     printf("\n %lu ", hb_sqlite2_db->flags, );
#endif

   hb_retni( hb_sqlite2_db == 0 ? 1 : 0 ); // error: 1
}

/* Execute a query over the passed table */
HB_FUNC( SQLITE_QUERY )
{
   int rc;
   int i;
   char * szSQLcom = hb_parc( 1 );

   hb_sqlite2_iDataRows = 0;     // reset global records
   hb_sqlite2_iDataCols = 0;     // reset global fields

   /* reset temporary store */
   for( i = 0; i < 255; i++ )
      hb_sqlite2_aDataCols[ i ] = 0L;

   for( i = 0; i < 1024; i++ )
      hb_sqlite2_aDataRows[ i ] = 0L;

   rc = sqlite_exec( hb_sqlite2_db, szSQLcom, hb_sqlite2_callback, 0, &hb_sqlite2_szErrMsg );

   /* Check the operation's result */
   if( rc == SQLITE_OK )
   {
      int iResRows = 0;
      int iResCols = 0;
      int iRec;
      int iField;
      char * pErrmsg;
      char ** pResStr;
      PHB_ITEM paRows;
      PHB_ITEM paCols;

      /* put here a routine to process results */
      sqlite_get_table( hb_sqlite2_db, /* An open database */
                        szSQLcom,      /* SQL to be executed */
                        &pResStr,      /* Result written to a char *[]  that this points to */
                        &iResRows,     /* Number of result rows written here */
                        &iResCols,     /* Number of result columns written here */
                        &pErrmsg       /* Error msg written here */
                      );

      /* global results */
      hb_sqlite2_iDataRows = iResRows;  /* set rows from last operation */
      hb_sqlite2_iDataCols = iResCols;  /* set cols from last operation */

      /* quiero devolver un array bidimensional donde la cantidad de filas
         es rows +1 (ó reccords +1 ) y las columnas los campos
         la primer fila contiene los encabezados de los campos */
#if 0
      for( iRec = 0, i = 0; iRec < iResRows + 1; iRec++ )
      {
         for( iField = 0; iField < iResCols; iField++, i++ )
            printf("%s\t", (pResStr)[ i ] );

         printf("\n");
      }
#endif
      /* dimension rows array */
      paRows = hb_itemArrayNew( iResRows + 1 );
      i = 0;

      for( iRec = 0; iRec < iResRows + 1; iRec++ )
      {
         /* if it's a multidimensional array */
         if( iResCols > 1 )
         {
            paCols = hb_itemArrayNew( iResCols );

            /* for every field */
            for( iField = 0; iField < iResCols; iField++ )
               hb_arraySetC( paCols, iField + 1, pResStr[ i++ ] );

            /* put data onto subarray of records */
            hb_itemArrayPut( paRows, iRec + 1, paCols);
            hb_itemRelease( paCols );
         }
         /* is an unidimensional array */
         else
            hb_arraySetC( paRows, iRec + 1, pResStr[ i++ ] );
      }

      /* free memory allocated */
      sqlite_free_table( pResStr );

      hb_itemReturn( paRows ); /* final, return to harbour & release */
      hb_itemRelease( paRows );
   }
   else
   {
      hb_sqlite2_iLastErrCode = rc;   /* set last error */
      hb_reta( 0 );
   }
}

/* Close currently open database */
HB_FUNC( SQLITE_CLOSE )
{
   sqlite_close( hb_sqlite2_db );
}

/* Returns information about current SQLite package */
HB_FUNC( SQLITE_INFO )
{
   hb_reta( 3 );
   hb_storc( ( char * ) SQLITE_VERSION      , -1, 1 );
   hb_storc( ( char * ) sqlite_libversion() , -1, 2 );
   hb_storc( ( char * ) sqlite_libencoding(), -1, 3 );
}

/* Returns the number of rows resulting from last operation */
HB_FUNC( SQLITE_GETROWS )
{
   hb_retni( hb_sqlite2_iDataRows );
}

/* Returns the number of columns resulting from last operation */
HB_FUNC( SQLITE_GETCOLS )
{
   hb_retni( hb_sqlite2_iDataCols );
}

/* Execute a statment (not a query) over the database */
HB_FUNC( SQLITE_EXECUTE )
{
   int rc, i; /* , iRec, iField; */
   char * szSQLcom = hb_parc( 1 );

   hb_sqlite2_iDataRows = 0;     /* reset global records */
   hb_sqlite2_iDataCols = 0;     /* reset global fields */

   /* reset temporary store */
   for( i = 0; i < 255; i++ )
      hb_sqlite2_aDataCols[ i ] = 0L;

   for( i = 0; i < 1024; i++ )
      hb_sqlite2_aDataRows[ i ] = 0L;

   rc = sqlite_exec( hb_sqlite2_db, szSQLcom, hb_sqlite2_callback, 0, &hb_sqlite2_szErrMsg );

   /* Check the operation's result */
   if( rc != SQLITE_OK )
   {
      hb_sqlite2_iLastErrCode = rc;   /* set last error */
      hb_retni( rc );                 /* return last error also */
   }
}

/* Returns an unidimensional array with FIELD NAMES only */
HB_FUNC( SQLITE_SYSCOLUMNS )
{
   struct Table * pTable = ( struct Table * ) sqliteFindTable( hb_sqlite2_db, ( const char * ) hb_parc( 1 ), 0 );

   if( pTable )
   {
      int iField;
      PHB_ITEM paRows;
      PHB_ITEM paCols;

      /* dimension rows array:
         1 is table name
         2 is field number
         3 to n cols data */ 

      paRows = hb_itemArrayNew( 2 + pTable->nCol );   
                                                      
      /* the Table structure itself */
      hb_arraySetC( paRows, 1, pTable->zName ); /* save name of table */
      hb_arraySetNL( paRows, 2, pTable->nCol ); /* save number of cols/fields */

#if 0
      printf("\n\nName= %s", pTable->zName );
      printf("\n\nCols = %i", pTable->nCol );

      for( i = 0; i < pTable->nCol; i++)
      {
         printf("\n   Name= %s", pTable->aCol[ i ].zName );
         printf("\n   Dflt= %s", pTable->aCol[ i ].zDflt );
         printf("\n   Type= %s", pTable->aCol[ i ].zType );
         printf("\nPrimKey= %i", pTable->aCol[ i ].isPrimKey );
      }
#endif

      for( iField = 0; iField < pTable->nCol; iField++ )
      {
         /* it's a multidimensional array */
         /* four data columns name, default, type, isprimarykey per field */

         paCols = hb_itemArrayNew( 4 );

         hb_arraySetC( paCols, 1, pTable->aCol[ iField ].zName );
         hb_arraySetC( paCols, 2, pTable->aCol[ iField ].zDflt );
         hb_arraySetC( paCols, 3, pTable->aCol[ iField ].zType );
         hb_arraySetL( paCols, 4, pTable->aCol[ iField ].isPrimKey );

         /* put data onto subarray of records */
         hb_itemArrayPut( paRows, 3 + iField, paCols );

         hb_itemRelease( paCols );
      }

      hb_itemReturnRelease( paRows );
   }
   else
      hb_reta( 0 );
}

/* Returns an unidimensional array with field names only */
HB_FUNC( SQLITE_FIELDS )
{
   struct Table * pTable = ( struct Table * ) sqliteFindTable( hb_sqlite2_db, ( const char * ) hb_parc( 1 ), 0 );

   if( pTable )
   {
      int i;

      // the Table structure itself
      hb_reta( pTable->nCol );

      for( i = 0; i < pTable->nCol; i++ )
         hb_storc( pTable->aCol[ i ].zName, -1, 1 + i );
   }
}

/* Returns number of tables inside current open database (not table) */
HB_FUNC( SQLITE_NUMOFTABLES )
{
   hb_retni( hb_sqlite2_db ? hb_sqlite2_db->nTable - 2 : 0 );
}
