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
*/

#define DBOI_CONDITION            1    /* The order's conditional expression     */
#define DBOI_EXPRESSION           2    /* The order's key expression             */
#define DBOI_POSITION             3    /* The current key position in scope and filter  */
#define DBOI_RECNO                4    /* The current key position disregarding filters */
#define DBOI_NAME                 5    /* The name of the order                      */
#define DBOI_NUMBER               6    /* The numeric position in the list of orders */
#define DBOI_BAGNAME              7    /* The name of the file containing this order     */
#define DBOI_BAGEXT               8    /* The extension of the file containing this order */
#define DBOI_INDEXEXT             DBOI_BAGEXT
#define DBOI_INDEXNAME            DBOI_BAGNAME
#define DBOI_ORDERCOUNT           9    /* The count of ORDERS contained in an index file or in total */
#define DBOI_FILEHANDLE           10   /* The OS file handle of the index     */
#define DBOI_ISCOND               11   /* Does the order have a FOR condition? */
#define DBOI_ISDESC               12   /* Is the order DESCENDing? */
#define DBOI_UNIQUE               13   /* Does the order have the UNIQUE attribute? */

/* 53-level constants */
#define DBOI_FULLPATH             20  /* The full path to the index file (Bag) */
#define DBOI_KEYTYPE              24  /* The type of the order's key           */
#define DBOI_KEYSIZE              25  /* The length of the order's key         */
#define DBOI_KEYCOUNT             26  /* The count of keys in scope and filter */
#define DBOI_SETCODEBLOCK         27  /* The codeblock that produces the key   */
#define DBOI_KEYDEC               28  /* The # of decimals in a numeric key    */
#define DBOI_HPLOCKING            29  /* Using High Performance locking for this order?  */
#define DBOI_LOCKOFFSET           35  /* The offset used for logical locking             */

#define DBOI_KEYADD               36  /* Custom Index: Was Key added successfully?       */
#define DBOI_KEYDELETE            37  /* Custom Index: Was Key Deletion successful?      */
#define DBOI_KEYVAL               38  /* The value of the current key      */
#define DBOI_SCOPETOP             39  /* Get or Set the scope top          */
#define DBOI_SCOPEBOTTOM          40  /* Get or Set the scope botto        */
#define DBOI_SCOPETOPCLEAR        41  /* Clear the scope top               */
#define DBOI_SCOPEBOTTOMCLEAR     42  /* Clear the scope bottom            */

#define DBOI_CUSTOM               45  /* Is this a Custom Index?           */
#define DBOI_SKIPUNIQUE           46  /* Was a skip to adjacent unique Key successful?   */

#define DBOI_KEYSINCLUDED         50  /* Number of keys in the index order */
/* key numbers and counts */
#define DBOI_KEYGOTO              DBOI_POSITION
#define DBOI_KEYNORAW             51  /* The key number disregarding filters  */
#define DBOI_KEYCOUNTRAW          52  /* The key count disregarding filter    */

/* Query Optimization */
#define DBOI_OPTLEVEL             53  /* Optimization level for current query */

/* These shouldn't need an open table */
#define DBOI_STRICTREAD           60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
#define DBOI_OPTIMIZE             61  /* Flag for whether to use query optimization             */
#define DBOI_AUTOOPEN             62  /* Flag for automatically opening structural indexes      */
#define DBOI_AUTOORDER            63  /* When a structural index is opened, the order to be set */
#define DBOI_AUTOSHARE            64  /* When a network is detected, open the index shared, otherwise open exclusively   */

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


/* constants for dbInfo()  */
#define DBI_ISDBF                 1  /* Does this RDD support DBFs? */
#define DBI_CANPUTREC             2  /* Can this RDD Put Records?   */
#define DBI_GETHEADERSIZE         3  /* Data file's header size     */
#define DBI_LASTUPDATE            4  /* The last date this file was written to  */
#define DBI_GETDELIMITER          5  /* The delimiter (as a string)         */
#define DBI_SETDELIMITER          6  /* The delimiter (as a string)         */
#define DBI_GETRECSIZE            7  /* The size of 1 record in the file    */
#define DBI_GETLOCKARRAY          8  /* An array of locked records' numbers */
#define DBI_TABLEEXT              9  /* The data file's file extension      */
#define DBI_FULLPATH             10  /* The Full path to the data file      */

#define DBI_ISFLOCK              20  /* Is there a file lock active?        */
#define DBI_CHILDCOUNT           22  /* Number of child relations set       */
#define DBI_FILEHANDLE           23  /* The data file's OS file handle      */
#define DBI_BOF                  26  /* Same as bof()    */
#define DBI_EOF                  27  /* Same as eof()    */
#define DBI_DBFILTER             28  /* Current Filter setting              */
#define DBI_FOUND                29  /* Same as found()  */
#define DBI_FCOUNT               30  /* How many fields in a record?        */
#define DBI_LOCKCOUNT            31  /* Number of record locks              */
#define DBI_VALIDBUFFER          32  /* Is the record buffer valid?         */
#define DBI_ALIAS                33  /* Name (alias) for this workarea      */
#define DBI_GETSCOPE             34  /* The codeblock used in LOCATE        */
#define DBI_LOCKOFFSET           35  /* The offset used for logical locking */
#define DBI_SHARED               36  /* Was the file opened shared?         */
#define DBI_MEMOEXT              37  /* The memo file's file extension      */
#define DBI_MEMOHANDLE           38  /* File handle of the memo file        */
#define DBI_MEMOBLOCKSIZE        39  /* Memo File's block size              */

#define DBI_DB_VERSION          101  /* Version of the Host driver          */
#define DBI_RDD_VERSION         102  /* current RDD's version               */

#define DBI_USER               1000  /* User-defined DBI_ constants */

#endif /* HB_DBINFO_CH_ */
