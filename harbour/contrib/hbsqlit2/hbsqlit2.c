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

// ------------------------------------------------------------------------
//     include and C code section
// ------------------------------------------------------------------------

// NOTE: we need this to prevent base types redefinition
#define _CLIPDEFS_H
#if defined(HB_OS_WIN_32_USED)
   #include <windows.h>
#endif

#include "hbapi.h"
#include "extend.api"
#include "item.api"
#include "hbstack.h"
#include "hbapiitm.h"

#include <stdio.h>
#include <stdlib.h>  // need it for exit() and BCC55

#include "sqlite.h"
#include "sqliteInt.h"

// PUBLIC VARS
  sqlite *db ;   // public by ale
  char * szErrMsg = 0;
  int iLastErrCode = 0;    // for use in foreign languages (translate msg)

// public (temporary) buffer till I can find a better approach
char * aDataRows[ 1024];
char * aDataCols[  255];
int iDataRows = 0;     // records
int iDataCols = 0;     // fields


//--------------------------------------------------------------------------
static int callback(void *NotUsed, int argc, char **argv, char **azColName)
//--------------------------------------------------------------------------
// INTERNAL DO NOT TOUCH !!!
//---------------------------------------------------------------------------
{
  int i;

  // in arg is the number of data fields for each row
//  printf("argc =>%d\n", argc);

  HB_SYMBOL_UNUSED( NotUsed );
  HB_SYMBOL_UNUSED( argv );
  HB_SYMBOL_UNUSED( azColName );

  for( i=0; i < argc; i++)
  {
//    printf(">>> %s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
//    aDataCols[i] = azColName[i];  // en realidad copiar cadena a cadena
//    aDataRows[i] = argv[i];
  }
//  printf("\n");
  return 0;
}


//----------------------
 HB_FUNC( SQLITE_ERROR )
//--------------------------------------------------------------------------
// Returns a string explaining last error
//--------------------------------------------------------------------------
{
   if( szErrMsg )
       hb_retc( szErrMsg );
   else
       hb_ret();
}


//----------------------
 HB_FUNC( SQLITE_OPEN )   // sqlite * = sqlite_open(argv[1], 0, &zErrMsg);
//--------------------------------------------------------------------------
// Open a database file (in SQLite format) and set a public structure
//--------------------------------------------------------------------------
{
   char * szDB = hb_parc( 1 );
   int   iMode = 0;   // hb_parni( 2 );

   HB_SYMBOL_UNUSED( iMode );

   // db is a public var (by ale)
   db = (sqlite *) sqlite_open( szDB, 0, &szErrMsg);

   // It seems Borland BCC55 do not return a correct pointer to a sqlite
   // struc, thus I can't check if database was successfully open :(
// (sqlite *)
//   printf("\n %lu ", db->flags, );

   if( db == 0 )
   {
       hb_retni( 1 );    // error!
   }
   else
       hb_retni( 0 );    // successful
}


//-----------------------
 HB_FUNC( SQLITE_QUERY )
//--------------------------------------------------------------------------
// Execute a query over the passed table
//--------------------------------------------------------------------------
{
  int rc, iResRows =0, iResCols =0, i, iRec, iField;
  char * szSQLcom = hb_parc( 1 );
  char * pErrmsg ;
  char ** pResStr ;
  PHB_ITEM paRows;   // PHB_ITEM is the Harbour equivalent of Clipper ITEM
  PHB_ITEM paCols;
  PHB_ITEM ptemp;
  iDataRows = 0;     // reset global records
  iDataCols = 0;     // reset global fields

  // reset temporary store
  for( i=0; i< 255; i++)
       aDataCols [i] = 0L;

  for( i=0; i< 1024; i++)
       aDataRows [i] = 0L;

  rc = sqlite_exec( db, szSQLcom, callback, 0, &szErrMsg);

  // Check the operation's result
  if( rc != SQLITE_OK )
  {
      iLastErrCode = rc;   // set last error
      hb_retc( "" );      // return last error also
      return;
  }

//  else
//  {
     // put here a routine to process results
     sqlite_get_table( db,        /* An open database */
      szSQLcom,          /* SQL to be executed */
      &pResStr,      /* Result written to a char *[]  that this points to */
      &iResRows,          /* Number of result rows written here */
      &iResCols,          /* Number of result columns written here */
      &pErrmsg            /* Error msg written here */
    );

 // global results
 iDataRows = iResRows;  // set rows from last operation
 iDataCols = iResCols;  // set cols from last operation


 // ------------------------------------------------------------------
 // quiero devolver un array bidimensional donde la cantidad de filas
 // es rows +1 (ó reccords +1 ) y las columnas los campos
 // la primer fila contiene los encabezados de los campos
/*
  i=0;
  for( iRec=0; iRec < iResRows+1; iRec++)
  {
       for( iField=0; iField < iResCols; iField++)
       {
            printf("%s\t", ((pResStr)[i]) );
            i++;
       }
       printf("\n");
  }
*/
   // -------- dimension rows array -------------------------------
   paRows = hb_itemArrayNew( iResRows + 1 );
   i = 0;

   for( iRec=0; iRec < iResRows+1; iRec++)
   {

     // ---------- if it's a multidimensional array --------------------
     if( iResCols > 1)
     {
         paCols = hb_itemArrayNew( iResCols );

         // for every field
         for( iField=0; iField < iResCols; iField++)
         {
            // if field is not empty

            if( ((pResStr)[i]) != NULL)
               ptemp = hb_itemPutC( NULL, ((pResStr)[i]) );
            else
               ptemp = hb_itemPutC( NULL, "");

            // put data onto subarray of fields
            hb_itemArrayPut( paCols, iField + 1, ptemp);
            hb_itemRelease( ptemp );
            i++;  // point to next data

         } //endfor internal

         // put data onto subarray of records
         hb_itemArrayPut( paRows, iRec + 1, paCols);
         hb_itemRelease( paCols );
     }
     else
     {
     // ---------- is an unidimensional array -----------------------------

          if( ((pResStr)[i]) != NULL)
             ptemp = hb_itemPutC( NULL, ((pResStr)[i]) );
          else
             ptemp = hb_itemPutC( NULL, "");

          // put data onto subarray of records
          hb_itemArrayPut( paRows, iRec + 1, ptemp);
          hb_itemRelease( ptemp );
          i++;  // point to next data
     }

    }//endfor extern

    // free memory allocated
    sqlite_free_table( pResStr );

//  } // endif check

   hb_itemReturn( paRows );  // final, return to harbour & release
   hb_itemRelease( paRows );
}


//----------------------
 HB_FUNC( SQLITE_CLOSE ) // void sqlite_close(sqlite *db)
//--------------------------------------------------------------------------
// Close currently open database
//--------------------------------------------------------------------------
{
   sqlite_close( db );   // db is public
   hb_ret();
}


//----------------------
 HB_FUNC( SQLITE_INFO )
//--------------------------------------------------------------------------
// Returns information about current SQLite package
//--------------------------------------------------------------------------
{
   hb_reta(3);
   hb_storc( (char *)       SQLITE_VERSION, -1, 1);
   hb_storc( (char *) sqlite_libversion(),  -1, 2);
   hb_storc( (char *) sqlite_libencoding(), -1, 3);
}


//-------------------------
 HB_FUNC( SQLITE_GETROWS )
//--------------------------------------------------------------------------
// Returns the number of rows resulting from last operation
//--------------------------------------------------------------------------
{
  hb_retni( iDataRows );
}


//------------------------
 HB_FUNC( SQLITE_GETCOLS )
//--------------------------------------------------------------------------
// Returns the number of columns resulting from last operation
//--------------------------------------------------------------------------
{
  hb_retni( iDataCols );
}


//------------------------
 HB_FUNC( SQLITE_EXECUTE )
//--------------------------------------------------------------------------
// Execute a statment (not a query) over the database
//--------------------------------------------------------------------------
{
  int rc, iResRows =0, iResCols =0, i; // , iRec, iField;
  char * szSQLcom = hb_parc( 1 );
//char * pErrmsg ;

  HB_SYMBOL_UNUSED( iResCols );
  HB_SYMBOL_UNUSED( iResRows );

  iDataRows = 0;     // reset global records
  iDataCols = 0;     // reset global fields

  // reset temporary store
  for( i=0; i< 255; i++)
       aDataCols [i] = 0L;

  for( i=0; i< 1024; i++)
       aDataRows [i] = 0L;

  rc = sqlite_exec( db, szSQLcom, callback, 0, &szErrMsg);

  // Check the operation's result
  if( rc != SQLITE_OK )
  {
      iLastErrCode = rc;   // set last error
      hb_retni( rc );      // return last error also
  }
}


//---------------------------
 HB_FUNC( SQLITE_SYSCOLUMNS )
//--------------------------------------------------------------------------
// Returns an unidimensional array with FIELD NAMES only
//--------------------------------------------------------------------------
{
  sqlite *pDB = db ;
  struct Table * pTable;
  const char * szName = hb_parc( 1 );
  int iField;
  PHB_ITEM paRows;   // PHB_ITEM is the Harbour equivalent of Clipper ITEM
  PHB_ITEM paCols;
  PHB_ITEM ptemp;

  pTable = (struct Table *) sqliteFindTable( pDB, szName, 0 );

  if( pTable )
  {
      // -------- dimension rows array -------------------------------
      paRows = hb_itemArrayNew( 2 + pTable->nCol );   // 1 is table name
                                                      // 2 is field number
                                                      // 3 to n cols data
      // the Table structure itself
      // save name of table
      if( (pTable->zName) != NULL)
           ptemp = hb_itemPutC( NULL, pTable->zName );
      else
           ptemp = hb_itemPutC( NULL, "");

      hb_itemArrayPut( paRows, 1, ptemp);  // base 1 not zero
      hb_itemRelease( ptemp );

      // save number of cols/fields
      ptemp = hb_itemPutNL( NULL, pTable->nCol );
      hb_itemArrayPut( paRows, 2, ptemp);  // base 1 not zero
      hb_itemRelease( ptemp );

//    printf("\n\nName= %s", pTable->zName );
//    printf("\n\nCols = %i", pTable->nCol );
//      hb_reta( pTable->nCol );
/*
      for( i=0; i < pTable->nCol; i++)
      {
         printf("\n  Name= %s", pTable->aCol[i].zName );
         printf("\n  Dflt= %s", pTable->aCol[i].zDflt );
         printf("\n  Type= %s", pTable->aCol[i].zType );
         printf("\nPrimKey= %i", pTable->aCol[i].isPrimKey );
      }
*/

     for( iField =0; iField < pTable->nCol; iField++ )
     {

     // ---------- it's a multidimensional array --------------------
     // four data columns name, default, type, isprimarykey per field

         paCols = hb_itemArrayNew( 4 );

            // 1) if field name is not empty
         if( (pTable->aCol[iField].zName) != NULL)
              ptemp = hb_itemPutC( NULL, pTable->aCol[iField].zName );
         else
              ptemp = hb_itemPutC( NULL, "");

            // put data onto subarray of fields
         hb_itemArrayPut( paCols, 1, ptemp);  // base 1 not 0
         hb_itemRelease( ptemp );


            // 2) if default value is not empty
         if( (pTable->aCol[iField].zDflt) != NULL)
              ptemp = hb_itemPutC( NULL, pTable->aCol[iField].zDflt );
         else
              ptemp = hb_itemPutC( NULL, "");

            // put data onto subarray of fields
         hb_itemArrayPut( paCols, 2, ptemp);  // base 1 not 0
         hb_itemRelease( ptemp );


            // 3) if field type is not empty
         if( (pTable->aCol[iField].zType) != NULL)
               ptemp = hb_itemPutC( NULL, pTable->aCol[iField].zType );
         else
               ptemp = hb_itemPutC( NULL, "");

            // put data onto subarray of fields
         hb_itemArrayPut( paCols, 3, ptemp);  // base 1 not 0
         hb_itemRelease( ptemp );


         // 4) if primary key is not empty
         ptemp = hb_itemPutL( NULL, pTable->aCol[iField].isPrimKey);

            // put data onto subarray of fields
         hb_itemArrayPut( paCols, 4, ptemp);  // base 1 not 0
         hb_itemRelease( ptemp );

         // put data onto subarray of records
         hb_itemArrayPut( paRows, 3 + iField, paCols);
         hb_itemRelease( paCols );
     } // endfor

   } // endif pTable

   hb_itemReturn( paRows );  // final, return to harbour & release
   hb_itemRelease( paRows );

} // eofunc


//-----------------------
 HB_FUNC( SQLITE_FIELDS )
//--------------------------------------------------------------------------
// Returns an unidimensional array with field names only
//--------------------------------------------------------------------------
{
  sqlite *pDB = db ;
  struct Table * pTable;
  const char * szName = hb_parc( 1 );
  int i;

  pTable = (struct Table *) sqliteFindTable( pDB, szName, 0 );

  if( pTable )
  {
      // the Table structure itself
      hb_reta( pTable->nCol );

      for( i=0; i < pTable->nCol; i++)
      {
         hb_storc( pTable->aCol[i].zName, -1, 1 + i );
      }
  }
  else
     hb_ret();

} // eofunc


//-----------------------------
 HB_FUNC( SQLITE_NUMOFTABLES )
//--------------------------------------------------------------------------
// Returns number of tables inside current open database (not table)
//--------------------------------------------------------------------------
{
  if( db )
      hb_retni( (db->nTable) - 2 ) ;  // two internals
  else
      hb_retni( 0 );
}
