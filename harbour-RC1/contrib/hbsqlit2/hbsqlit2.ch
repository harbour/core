/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * SQLite DBMS defines
 *
 * Copyright 2003 Alejandro de Garate <alex_degarate@hotmail.com>
 * www - http://www.harbour-project.org
 * 
 */

#ifndef HBSQLIT2_CH_
#define HBSQLIT2_CH_

/* Generic defines */

/*
 * Return values for sqlite_exec() and sqlite_step()
 */
#define HB_SQLITE_OK           0   /* Successful result */
#define HB_SQLITE_ERROR        1   /* SQL error or missing database */
#define HB_SQLITE_INTERNAL     2   /* An internal logic error in SQLite */
#define HB_SQLITE_PERM         3   /* Access permission denied */
#define HB_SQLITE_ABORT        4   /* Callback routine requested an abort */
#define HB_SQLITE_BUSY         5   /* The database file is locked */
#define HB_SQLITE_LOCKED       6   /* A table in the database is locked */
#define HB_SQLITE_NOMEM        7   /* A malloc() failed */
#define HB_SQLITE_READONLY     8   /* Attempt to write a readonly database */
#define HB_SQLITE_INTERRUPT    9   /* Operation terminated by sqlite_interrupt() */
#define HB_SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
#define HB_SQLITE_CORRUPT     11   /* The database disk image is malformed */
#define HB_SQLITE_NOTFOUND    12   /* (Internal Only) Table or record not found */
#define HB_SQLITE_FULL        13   /* Insertion failed because database is full */
#define HB_SQLITE_CANTOPEN    14   /* Unable to open the database file */
#define HB_SQLITE_PROTOCOL    15   /* Database lock protocol error */
#define HB_SQLITE_EMPTY       16   /* (Internal Only) Database table is empty */
#define HB_SQLITE_SCHEMA      17   /* The database schema changed */
#define HB_SQLITE_TOOBIG      18   /* Too much data for one row of a table */
#define HB_SQLITE_CONSTRAINT  19   /* Abort due to contraint violation */
#define HB_SQLITE_MISMATCH    20   /* Data type mismatch */
#define HB_SQLITE_MISUSE      21   /* Library used incorrectly */
#define HB_SQLITE_NOLFS       22   /* Uses OS features not supported on host */
#define HB_SQLITE_AUTH        23   /* Authorization denied */
#define HB_SQLITE_FORMAT      24   /* Auxiliary database format error */
#define HB_SQLITE_ROW         100  /* sqlite_step() has another row ready */
#define HB_SQLITE_DONE        101  /* sqlite_step() has finished executing */

/* *       SQLITE_COPY
**       SQLITE_CREATE_INDEX
**       SQLITE_CREATE_TABLE
**       SQLITE_CREATE_TEMP_INDEX
**       SQLITE_CREATE_TEMP_TABLE
**       SQLITE_CREATE_TEMP_TRIGGER
**       SQLITE_CREATE_TEMP_VIEW
**       SQLITE_CREATE_TRIGGER
**       SQLITE_CREATE_VIEW
**       SQLITE_DELETE
**       SQLITE_DROP_INDEX
**       SQLITE_DROP_TABLE
**       SQLITE_DROP_TEMP_INDEX
**       SQLITE_DROP_TEMP_TABLE
**       SQLITE_DROP_TEMP_TRIGGER
**       SQLITE_DROP_TEMP_VIEW
**       SQLITE_DROP_TRIGGER
**       SQLITE_DROP_VIEW
**       SQLITE_INSERT
**       SQLITE_PRAGMA
**       SQLITE_READ
**       SQLITE_SELECT
**       SQLITE_TRANSACTION
**       SQLITE_UPDATE
*/

/* Harbour definitions */
 #define  HB_HB4SQLITE_VER  "0.40"

/* SQLite tag 
 * we use the first part of the tag (defined at btree.c) to not exclude 
 * any version of the database
 * "** This file contains an SQLite 2.1 database **"
 */
#define  XSQLITE_TAG "** This file contains an SQLite " 
#define  XSQLITE_TAG_LEN   LEN(XSQLITE_TAG)

#endif

/* End of hbsqlite.ch */
