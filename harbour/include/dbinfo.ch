/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the RDD API Index OrderInfo and DBInfo support
 *
 * Copyright 2000 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
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
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_DBINFO_CH_
#define HB_DBINFO_CH_

/*
   Constants for SELF_ORDINFO ()
   Be sure these stay in sync with the same ones in hbapirdd.h
*/

#define DBOI_CONDITION            1    /* Get the order's condition     */
#define DBOI_EXPRESSION           2    /* Get the order's expression    */
#define DBOI_POSITION             3    /* Get current key position in scope and filter  */
#define DBOI_RECNO                4    /* Get current key position disregarding filters */
#define DBOI_NAME                 5    /* Get the order's name          */
#define DBOI_NUMBER               6    /* Get the order's list position */
#define DBOI_BAGNAME              7    /* Get the order's Bag name      */
#define DBOI_BAGEXT               8    /* Get the order's Bag Extension */
#define DBOI_INDEXEXT             DBOI_BAGEXT
#define DBOI_INDEXNAME            DBOI_BAGNAME
#define DBOI_ORDERCOUNT           9    /* Get the count of ORDERS in an index file or in total */
#define DBOI_FILEHANDLE           10   /* Get the handle of the index file */
#define DBOI_ISCOND               11   /* Does the order have a FOR condition */
#define DBOI_ISDESC               12   /* Is the order DESCENDing */
#define DBOI_UNIQUE               13   /* Does the order have the unique attribute set? */

/* 53-level constants */
#define DBOI_FULLPATH             20  /* Get the order Bag's Full Path            */
#define DBOI_KEYTYPE              24  /* Get the type of the order's key          */
#define DBOI_KEYSIZE              25  /* Get the size of the order's key          */
#define DBOI_KEYCOUNT             26  /* Get the count of keys in scope and filter*/
#define DBOI_SETCODEBLOCK         27  /* Set codeblock for order key              */
#define DBOI_KEYDEC               28  /* Get # of decimals in order's key         */
#define DBOI_HPLOCKING            29  /* Using High performance index locking?    */
#define DBOI_LOCKOFFSET           35  /* New locking offset                       */

#define DBOI_KEYADD               36  /* Gets/Sets the Key to be added            */
#define DBOI_KEYDELETE            37  /* Gets/Sets the Key to be deleted          */
#define DBOI_KEYVAL               38  /* Get current key's value                  */
#define DBOI_SCOPETOP             39  /* Gets/Sets the scope top                  */
#define DBOI_SCOPEBOTTOM          40  /* Gets/Sets the scope bottom               */
#define DBOI_SCOPETOPCLEAR        41  /* Clear the top scope setting              */
#define DBOI_SCOPEBOTTOMCLEAR     42  /* Clear the bottom scope setting           */

#define DBOI_CUSTOM               45  /* Custom created order                     */
#define DBOI_SKIPUNIQUE           46  /* Flag for skip unique                     */

#define DBOI_KEYSINCLUDED         50  /* # of keys included while indexing        */
/* key numbers and counts */
#define DBOI_KEYGOTO              DBOI_POSITION
#define DBOI_KEYNORAW             51  /* keyno ignoring any filter                */
#define DBOI_KEYCOUNTRAW          52  /* keycount ignoring any filter             */

/* Query Optimization */
#define DBOI_OPTLEVEL             53  /* Optimization level for current query      */

/* Perhaps change these so they don't require an open table */
#define DBOI_STRICTREAD           60  /* Get/set read-thru RDD when indexing       */
#define DBOI_OPTIMIZE             61  /* Get/set use of query optimization         */
#define DBOI_AUTOOPEN             62  /* Get/set auto-open of structural index     */
#define DBOI_AUTOORDER            63  /* Get/set default order of structural index */
#define DBOI_AUTOSHARE            64  /* Get/set automatic share control           */

/* Return values for DBOI_OPTLEVEL */
#define DBOI_OPTIMIZED_NONE       0
#define DBOI_OPTIMIZED_PART       1
#define DBOI_OPTIMIZED_FULL       2

/* constants for SELF_RECINFO() */
#define DBRI_DELETED              1
#define DBRI_LOCKED               2
#define DBRI_RECSIZE              3
#define DBRI_RECNO                4
#define DBRI_UPDATED              5


/* constants for DBINFO() */
#define DBI_ISDBF                 1  /* Logical: does RDD support DBF file format? */
#define DBI_CANPUTREC             2  /* Logical: does RDD support Putting Records? */
#define DBI_GETHEADERSIZE         3  /* Numeric: Get file header size              */
#define DBI_LASTUPDATE            4  /* Date:    Last date RDD file was updated    */
#define DBI_GETDELIMITER          5  /* String:  Get the default delimiter         */
#define DBI_SETDELIMITER          6  /* String:  Set the default delimiter         */
#define DBI_GETRECSIZE            7  /* Numeric: The size of 1 record in the file  */
#define DBI_GETLOCKARRAY          8  /* Array:   Get the array of locked records   */
#define DBI_TABLEEXT              9  /* String:  Get the table's file extension    */
#define DBI_FULLPATH             10  /* String:  Full path name of the file        */

#define DBI_ISFLOCK              20  /* Get the file lock status             */
#define DBI_CHILDCOUNT           22  /* Number of open child relations       */
#define DBI_FILEHANDLE           23  /* Handle of the open data file         */
#define DBI_BOF                  26  /* BOF flag - alternate for bof()    */
#define DBI_EOF                  27  /* EOF flag - alternate for eof()    */
#define DBI_DBFILTER             28  /* Current Filter expression         */
#define DBI_FOUND                29  /* FOUND flag - alternate to found() */
#define DBI_FCOUNT               30  /* Number of fields in a record      */
#define DBI_LOCKCOUNT            31  /* Number of record locks            */
#define DBI_VALIDBUFFER          32  /* Is the current buffer valid?      */
#define DBI_ALIAS                33  /* Alias (name) of workarea          */
#define DBI_GETSCOPE             34  /* LOCATE codeblock                  */
#define DBI_LOCKOFFSET           35  /* Locking offset                    */
#define DBI_SHARED               36  /* Get/Set the shared flag           */
#define DBI_MEMOEXT              37  /* The memo file's file extension    */
#define DBI_MEMOHANDLE           38  /* File handle of memo file          */
#define DBI_MEMOBLOCKSIZE        39  /* Memo File Blocksize               */

#define DBI_DB_VERSION          101  /* Version of Host driver            */
#define DBI_RDD_VERSION         102  /* current RDD version               */

#define DBI_USER               1000  /* Start of user-defined DBI_ values */

#endif /* HB_DBINFO_CH_ */
