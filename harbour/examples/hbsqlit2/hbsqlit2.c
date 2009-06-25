/*
 * $Id$
 */

/*
 *------------------------------------------------------------------------
 *                  HARBOUR INTERFACE for SQLITE
 *------------------------------------------------------------------------
 * Copyright 2003 Alejandro de Garate <alex_degarate@hotmail.com>
 * License: General Public License (GNU)
 *
 * History:
 * Ver 0.40 30 December 2003 Fixed an opening error not detected
 *             It seems is a problem with BCC compiler.
 *             If "xxFile" database is not found, an empty file is
 *             created with the same name, given an incorrect signal
 *             to FILE() function. File is empty but exists (Oh man...)
 *             I fix it from harbour code, when have more spare time
 *             I will look in depth.
 * Ver 0.30 28 December 2003 Pick tables, fields and DB structure,
 *             you can import some dbf (not finish yet)
 * Ver 0.20  5 December 2003 changes in design, A front end is started
 *             Shows database struc, table struct, field type
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

#include "hbapi.h"
#include "hbapiitm.h"

#include "sqlite.h"
#include "sqliteInt.h"

/* Public vars */
sqlite * hb_sqlite2_db = NULL;  /* public by ale */
char *   hb_sqlite2_szErrMsg = NULL;
int      hb_sqlite2_iDataRows = 0; /* records */
int      hb_sqlite2_iDataCols = 0; /* fields */

/* Returns information about current SQLite package */
HB_FUNC( SQLITE_INFO )
{
   hb_reta( 3 );
   hb_storc( ( char * ) SQLITE_VERSION      , -1, 1 );
   hb_storc( ( char * ) sqlite_libversion() , -1, 2 );
   hb_storc( ( char * ) sqlite_libencoding(), -1, 3 );
}

/* Open a database file (in SQLite format) and set a public structure */
HB_FUNC( SQLITE_OPEN )
{
   if( hb_sqlite2_db )
      sqlite_close( hb_sqlite2_db );

   hb_sqlite2_db = ( sqlite * ) sqlite_open( hb_parcx( 1 ), 0, &hb_sqlite2_szErrMsg );

   hb_retni( hb_sqlite2_db == NULL ? 1 : 0 ); /* error: 1 */
}

/* Close currently open database */
HB_FUNC( SQLITE_CLOSE )
{
   if( hb_sqlite2_db )
   {
      sqlite_close( hb_sqlite2_db );
      hb_sqlite2_db = NULL;
   }
}

/* Execute a statment (not a query) over the database */
HB_FUNC( SQLITE_EXECUTE )
{
   if( hb_sqlite2_db )
      hb_retni( sqlite_exec( hb_sqlite2_db, hb_parcx( 1 ), NULL, NULL, &hb_sqlite2_szErrMsg ) );
}

/* Execute a query over the passed table */
HB_FUNC( SQLITE_QUERY )
{
   const char * szSQLcom = hb_parcx( 1 );

   if( hb_sqlite2_db && sqlite_exec( hb_sqlite2_db, szSQLcom, NULL, NULL, &hb_sqlite2_szErrMsg ) == SQLITE_OK )
   {
      int iResRows = 0;
      int iResCols = 0;
      int iRec;
      int iField;
      char * pErrmsg;
      char ** pResStr;
      PHB_ITEM paRows;
      int i;

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

      /* dimension rows array */
      paRows = hb_itemArrayNew( iResRows + 1 );

      for( iRec = 0, i = 0; iRec < iResRows + 1; iRec++ )
      {
         if( iResCols > 1 ) /* if it's a multidimensional array */
         {
            PHB_ITEM paCols = hb_itemArrayNew( iResCols );

            /* for every field */
            for( iField = 0; iField < iResCols; iField++ )
               hb_arraySetC( paCols, iField + 1, pResStr[ i++ ] );

            /* put data onto subarray of records */
            hb_itemArrayPut( paRows, iRec + 1, paCols );
            hb_itemRelease( paCols );
         }
         else /* is an unidimensional array */
            hb_arraySetC( paRows, iRec + 1, pResStr[ i++ ] );
      }

      /* free memory allocated */
      sqlite_free_table( pResStr );

      hb_itemReturnRelease( paRows );
   }
   else
      hb_reta( 0 );
}

/* Returns an unidimensional array with FIELD NAMES only */
HB_FUNC( SQLITE_SYSCOLUMNS )
{
   if( hb_sqlite2_db )
   {
      struct Table * pTable = ( struct Table * ) sqliteFindTable( hb_sqlite2_db, ( const char * ) hb_parcx( 1 ), NULL );

      if( pTable )
      {
         /* dimension rows array:
            1 is table name
            2 is field number
            3 to n cols data */
         PHB_ITEM paRows = hb_itemArrayNew( 2 + pTable->nCol );
         int iField;

         /* the Table structure itself */
         hb_arraySetC( paRows, 1, pTable->zName ); /* save name of table */
         hb_arraySetNL( paRows, 2, pTable->nCol ); /* save number of cols/fields */

         for( iField = 0; iField < pTable->nCol; iField++ )
         {
            /* it's a multidimensional array */
            /* four data columns name, default, type, isprimarykey per field */
            PHB_ITEM paCols = hb_itemArrayNew( 4 );

            hb_arraySetC( paCols, 1, pTable->aCol[ iField ].zName );
            hb_arraySetC( paCols, 2, pTable->aCol[ iField ].zDflt );
            hb_arraySetC( paCols, 3, pTable->aCol[ iField ].zType );
            hb_arraySetL( paCols, 4, pTable->aCol[ iField ].isPrimKey );

            /* put data onto subarray of records */
            hb_itemArrayPut( paRows, 3 + iField, paCols );
            hb_itemRelease( paCols );
         }

         hb_itemReturnRelease( paRows );
         return;
      }
   }

   hb_reta( 0 );
}

/* Returns an unidimensional array with field names only */
HB_FUNC( SQLITE_FIELDS )
{
   if( hb_sqlite2_db )
   {
      struct Table * pTable = ( struct Table * ) sqliteFindTable( hb_sqlite2_db, ( const char * ) hb_parcx( 1 ), NULL );

      if( pTable )
      {
         int i;

         /* the Table structure itself */
         hb_reta( pTable->nCol );

         for( i = 0; i < pTable->nCol; i++ )
            hb_storc( pTable->aCol[ i ].zName, -1, 1 + i );

         return;
      }
   }

   hb_reta( 0 );
}

/* Returns number of tables inside current open database (not table) */
HB_FUNC( SQLITE_NUMOFTABLES )
{
   hb_retni( hb_sqlite2_db ? hb_sqlite2_db->nTable - 2 : 0 );
}

/* Returns a string explaining last error */
HB_FUNC( SQLITE_ERROR )
{
   hb_retc( hb_sqlite2_szErrMsg );
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
