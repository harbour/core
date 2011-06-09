/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the RDD API Index OrderInfo and DBInfo support
 *
 * Copyright 2000 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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

/* NOTE: This file is also used by C code. */

#ifndef HB_DBINFO_CH_
#define HB_DBINFO_CH_

/*
   Constants for SELF_RDDINFO ()
*/

#define RDDI_ISDBF                1   /* Does this RDD support DBFs? */
#define RDDI_CANPUTREC            2   /* Can this RDD Put Records? */
#define RDDI_DELIMITER            3   /* The field delimiter (as a string) */
#define RDDI_SEPARATOR            4   /* The record separator (as a string) */

#define RDDI_TABLEEXT             5   /* Default data file's file extension */
#define RDDI_MEMOEXT              6   /* Default memo file's file extension */
#define RDDI_ORDBAGEXT            7   /* Default multi tag index's file extension */
#define RDDI_ORDEREXT             8   /* default single tag index's file extension */
#define RDDI_ORDSTRUCTEXT         9   /* default single tag index's file extension */

#define RDDI_LOCAL               10   /* Local file access? */
#define RDDI_REMOTE              11   /* Remote table access? */
#define RDDI_CONNECTION          12   /* Get/Set default connection */
#define RDDI_TABLETYPE           13   /* Type of table file */
#define RDDI_MEMOTYPE            14   /* Type of MEMO file DB_MEMO_*: DBT, SMT, FPT(FP,SIX3,FLEXIII) */
#define RDDI_LARGEFILE           15   /* Is large file size (>=4GB) supported */
#define RDDI_LOCKSCHEME          16   /* Locking scheme used by RDD */
#define RDDI_RECORDMAP           17   /* Does RDD support record map functionality? */
#define RDDI_ENCRYPTION          18   /* Does RDD support encryption */
#define RDDI_TRIGGER             19   /* Get/Set default trigger function */
#define RDDI_AUTOLOCK            20   /* automatic locking on update */

/* index parameters */
#define RDDI_STRUCTORD           21   /* Are structural indexes supported */
#define RDDI_STRICTREAD          22   /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing */
#define RDDI_STRICTSTRUCT        23   /* Flag for strict structural order checking */
#define RDDI_OPTIMIZE            24   /* Flag for whether to use query optimization */
#define RDDI_FORCEOPT            25   /* Flag for forcing linear optimization */
#define RDDI_AUTOOPEN            26   /* Flag for automatically opening structural indexes */
#define RDDI_AUTOORDER           27   /* When a structural index is opened, the order to be set */
#define RDDI_AUTOSHARE           28   /* When a network is detected, open the index shared, otherwise open exclusively */
#define RDDI_MULTITAG            29   /* Does RDD support multi tag in index file */
#define RDDI_SORTRECNO           30   /* Is record number part of key in sorting */
#define RDDI_MULTIKEY            31   /* Does custom orders support repeated keys? */

/* memo parameters */
#define RDDI_MEMOBLOCKSIZE       32   /* Memo File's block size */
#define RDDI_MEMOVERSION         33   /* sub version of memo file */
#define RDDI_MEMOGCTYPE          34   /* type of garbage collector used by GC */
#define RDDI_MEMOREADLOCK        35   /* use read lock in memo file access */
#define RDDI_MEMOREUSE           36   /* reuse free space on write */
#define RDDI_BLOB_SUPPORT        37   /* can support BLOB files directly */

/* misc */
#define RDDI_PENDINGTRIGGER      40   /* set pending trigger for next open operation */
#define RDDI_PENDINGPASSWORD     41   /* set pending password for next open operation */
#define RDDI_PASSWORD            42   /* Get/Set default password */
#define RDDI_LOCKRETRY           43   /* Get/Set record and file lock timeout value */
#define RDDI_DIRTYREAD           44   /* Get/Set index dirty read flag */

/*
   Constants for SELF_ORDINFO ()
*/

#define DBOI_CONDITION            1   /* The order's conditional expression     */
#define DBOI_EXPRESSION           2   /* The order's key expression             */
#define DBOI_POSITION             3   /* The current key position in scope and filter  */
#define DBOI_RECNO                4   /* The current key position disregarding filters */
#define DBOI_NAME                 5   /* The name of the order                      */
#define DBOI_NUMBER               6   /* The numeric position in the list of orders */
#define DBOI_BAGNAME              7   /* The name of the file containing this order     */
#define DBOI_BAGEXT               8   /* The extension of the file containing this order */
#define DBOI_INDEXEXT            DBOI_BAGEXT
#define DBOI_INDEXNAME           DBOI_BAGNAME
#define DBOI_ORDERCOUNT           9   /* The count of ORDERS contained in an index file or in total */
#define DBOI_FILEHANDLE          10   /* The OS file handle of the index     */
#define DBOI_ISCOND              11   /* Does the order have a FOR condition? */
#define DBOI_ISDESC              12   /* Is the order DESCENDing? */
#define DBOI_UNIQUE              13   /* Does the order have the UNIQUE attribute? */

/* 53-level constants */
#define DBOI_FULLPATH            20  /* The full path to the index file (Bag) */
#define DBOI_KEYTYPE             24  /* The type of the order's key           */
#define DBOI_KEYSIZE             25  /* The length of the order's key         */
#define DBOI_KEYCOUNT            26  /* The count of keys in scope and filter */
#define DBOI_SETCODEBLOCK        27  /* The codeblock that produces the key   */
#define DBOI_KEYDEC              28  /* The # of decimals in a numeric key    */
#define DBOI_HPLOCKING           29  /* Using High Performance locking for this order?  */
#define DBOI_LOCKOFFSET          35  /* The offset used for logical locking             */

#define DBOI_KEYADD              36  /* Custom Index: Was Key added successfully?       */
#define DBOI_KEYDELETE           37  /* Custom Index: Was Key Deletion successful?      */
#define DBOI_KEYVAL              38  /* The value of the current key      */
#define DBOI_SCOPETOP            39  /* Get or Set the scope top          */
#define DBOI_SCOPEBOTTOM         40  /* Get or Set the scope botto        */
#define DBOI_SCOPETOPCLEAR       41  /* Clear the scope top               */
#define DBOI_SCOPEBOTTOMCLEAR    42  /* Clear the scope bottom            */
#define DBOI_CUSTOM              45  /* Is this a Custom Index?           */
#define DBOI_SKIPUNIQUE          46  /* Was a skip to adjacent unique Key successful?   */

#define DBOI_KEYSINCLUDED        50  /* Number of keys in the index order */
/* key numbers and counts */
#define DBOI_KEYGOTO             DBOI_POSITION
#define DBOI_KEYGOTORAW          DBOI_KEYNORAW
#define DBOI_KEYNO               DBOI_POSITION
#define DBOI_KEYNORAW            51  /* The key number disregarding filters  */
#define DBOI_KEYCOUNTRAW         52  /* The key count disregarding filter    */

/* Query Optimization */
#define DBOI_OPTLEVEL            53  /* Optimization level for current query */

/* These shouldn't need an open table */
#define DBOI_STRICTREAD          60  /* Flag for avoiding RDD hierarchy and using a bigger buffer when indexing  */
#define DBOI_OPTIMIZE            61  /* Flag for whether to use query optimization             */
#define DBOI_AUTOOPEN            62  /* Flag for automatically opening structural indexes      */
#define DBOI_AUTOORDER           63  /* When a structural index is opened, the order to be set */
#define DBOI_AUTOSHARE           64  /* When a network is detected, open the index shared, otherwise open exclusively   */

/* Harbour extensions */
#define DBOI_SKIPEVAL           100  /* skip while code block doesn't return TRUE */
#define DBOI_SKIPEVALBACK       101  /* skip backward while code block doesn't return TRUE */
#define DBOI_SKIPREGEX          102  /* skip while regular expression on index key doesn't return TRUE */
#define DBOI_SKIPREGEXBACK      103  /* skip backward while regular expression on index key doesn't return TRUE */
#define DBOI_SKIPWILD           104  /* skip while while comparison with given pattern with wildcards doesn't return TRUE */
#define DBOI_SKIPWILDBACK       105  /* skip backward while comparison with given pattern with wildcards doesn't return TRUE */
#define DBOI_SCOPEEVAL          106  /* skip through index evaluating given C function */
#define DBOI_FINDREC            107  /* find given record in a Tag beginning from TOP */
#define DBOI_FINDRECCONT        108  /* find given record in a Tag beginning from current position */
#define DBOI_SCOPESET           109  /* set both scopes */
#define DBOI_SCOPECLEAR         110  /* clear both scopes */

#define DBOI_BAGCOUNT           111  /* number of open order bags */
#define DBOI_BAGNUMBER          112  /* bag position in bag list */
#define DBOI_BAGORDER           113  /* number of first order in a bag */

#define DBOI_ISMULTITAG         114  /* does RDD support multi tag in index file */
#define DBOI_ISSORTRECNO        115  /* is record number part of key in sorting */
#define DBOI_LARGEFILE          116  /* is large file size (>=4GB) supported */
#define DBOI_TEMPLATE           117  /* order with free user keys */
#define DBOI_MULTIKEY           118  /* custom order with multikeys */
#define DBOI_CHGONLY            119  /* update only existing keys */
#define DBOI_PARTIAL            120  /* is index partially updated */
#define DBOI_SHARED             121  /* is index open in shared mode */
#define DBOI_ISREADONLY         122  /* is index open in readonly mode */
#define DBOI_READLOCK           123  /* get/set index read lock */
#define DBOI_WRITELOCK          124  /* get/set index write lock */
#define DBOI_UPDATECOUNTER      125  /* get/set update index counter */

#define DBOI_EVALSTEP           126  /* eval step (EVERY) used in index command */
#define DBOI_ISREINDEX          127  /* Is reindex in process */
#define DBOI_I_BAGNAME          128  /* created index name */
#define DBOI_I_TAGNAME          129  /* created tag name */

#define DBOI_RELKEYPOS          130  /* get/set relative key position (in range 0 - 1) */
#define DBOI_USECURRENT         131  /* get/set "use current index" flag */
#define DBOI_INDEXTYPE          132  /* current index type */
#define DBOI_RESETPOS           133  /* rest logical and raw positions */

/* return values for DBOI_OPTLEVEL */
#define DBOI_OPTIMIZED_NONE       0
#define DBOI_OPTIMIZED_PART       1
#define DBOI_OPTIMIZED_FULL       2

/* return values for DBOI_INDEXTYPE */
#define DBOI_TYPE_UNDEF          -1
#define DBOI_TYPE_NONE            0
#define DBOI_TYPE_NONCOMPACT      1
#define DBOI_TYPE_COMPACT         2
#define DBOI_TYPE_COMPOUND        3

/* constants for DBOI_SCOPEEVAL array parameter */
#define DBRMI_FUNCTION            1
#define DBRMI_PARAM               2
#define DBRMI_LOVAL               3
#define DBRMI_HIVAL               4
#define DBRMI_RESULT              5
#define DBRMI_SIZE                5

/* constants for SELF_RECINFO() */
#define DBRI_DELETED              1
#define DBRI_LOCKED               2
#define DBRI_RECSIZE              3
#define DBRI_RECNO                4
#define DBRI_UPDATED              5
#define DBRI_ENCRYPTED            6
#define DBRI_RAWRECORD            7
#define DBRI_RAWMEMOS             8
#define DBRI_RAWDATA              9

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

/* HARBOUR extension */
#define DBI_LOCKSCHEME          128  /* Locking scheme used by RDD */
#define DBI_ISREADONLY          129  /* Was the file opened readonly? */
#define DBI_ROLLBACK            130  /* Rollback changes made to current record */
#define DBI_PASSWORD            131  /* Workarea password */
#define DBI_ISENCRYPTED         132  /* The database is encrypted */
#define DBI_MEMOTYPE            133  /* Type of MEMO file: DBT, SMT, FPT */
#define DBI_SEPARATOR           134  /* The record separator (as a string) */
#define DBI_MEMOVERSION         135  /* sub version of memo file */
#define DBI_TABLETYPE           136  /* Type of table file */
#define DBI_SCOPEDRELATION      137  /* Is given relation scoped */
#define DBI_TRIGGER             138  /* Get/Set trigger function */
#define DBI_OPENINFO            139  /* DBOPENINFO structure pointer */
#define DBI_ENCRYPT             140  /* Encrypt table */
#define DBI_DECRYPT             141  /* Decrypt table */
#define DBI_MEMOPACK            142  /* Pack memo file */
#define DBI_DIRTYREAD           143  /* Get/Set index dirty read flag */
#define DBI_POSITIONED          144  /* Is cursor positioned to valid record */
#define DBI_ISTEMPORARY         145  /* Is the table a temporary one? */
#define DBI_LOCKTEST            146  /* record / file lock test */

/* RECORD MAP (RM) support */
#define DBI_RM_SUPPORTED        150  /* has WA RDD record map support? */
#define DBI_RM_CREATE           151  /* create new empty work area record map */
#define DBI_RM_REMOVE           152  /* remove active work area record map */
#define DBI_RM_CLEAR            153  /* remove all records from WA record map */
#define DBI_RM_FILL             154  /* add all records to WA record map */
#define DBI_RM_ADD              155  /* add record to work area record map */
#define DBI_RM_DROP             156  /* remove record from work area record map */
#define DBI_RM_TEST             157  /* test if record is set in WA record map */
#define DBI_RM_COUNT            158  /* number of records set in record map */
#define DBI_RM_HANDLE           159  /* get/set record map filter handle */

/* BLOB support - definitions for internal use by blob.ch */
#define DBI_BLOB_DIRECT_EXPORT  201
#define DBI_BLOB_DIRECT_GET     202
#define DBI_BLOB_DIRECT_IMPORT  203
#define DBI_BLOB_DIRECT_PUT     204
#define DBI_BLOB_ROOT_GET       205
#define DBI_BLOB_ROOT_PUT       206
#define DBI_BLOB_ROOT_LOCK      207
#define DBI_BLOB_ROOT_UNLOCK    208

/* CA-Cl*pper documented for public use */
#define DBI_BLOB_DIRECT_LEN     209
#define DBI_BLOB_DIRECT_TYPE    210
#define DBI_BLOB_INTEGRITY      211
#define DBI_BLOB_OFFSET         212
#define DBI_BLOB_RECOVER        213

#define DBI_USER               1000  /* User-defined DBI_ constants */

/* extended dbFieldInfo() actions */
#define DBS_ISNULL              101
#define DBS_BLOB_GET            201  /* This is internal definition */
#define DBS_BLOB_LEN            202
#define DBS_BLOB_OFFSET         203
#define DBS_BLOB_POINTER        204
#define DBS_BLOB_TYPE           205

#define BLOB_EXPORT_APPEND      1
#define BLOB_EXPORT_OVERWRITE   0

#define BLOB_IMPORT_COMPRESS    1
#define BLOB_IMPORT_ENCRYPT     2

#define FILEGET_APPEND          BLOB_EXPORT_APPEND
#define FILEGET_OVERWRITE       BLOB_EXPORT_OVERWRITE

#define FILEPUT_COMPRESS        BLOB_IMPORT_COMPRESS
#define FILEPUT_ENCRYPT         BLOB_IMPORT_ENCRYPT

/* DBF TYPES */
#define DB_DBF_STD              1
#define DB_DBF_VFP              2

/* MEMO TYPES */
#define DB_MEMO_NONE            0
#define DB_MEMO_DBT             1
#define DB_MEMO_FPT             2
#define DB_MEMO_SMT             3

/* MEMO EXTENDED TYPES */
#define DB_MEMOVER_STD          1
#define DB_MEMOVER_SIX          2
#define DB_MEMOVER_FLEX         3
#define DB_MEMOVER_CLIP         4

/* ENCRYPTION TYPE */
#define DB_CRYPT_NONE           0
#define DB_CRYPT_SIX            1

/* LOCK SCHEMES */
#define DB_DBFLOCK_DEFAULT      0
#define DB_DBFLOCK_CLIP         1
#define DB_DBFLOCK_CL53         2
#define DB_DBFLOCK_VFP          3
#define DB_DBFLOCK_CL53EXT      4
#define DB_DBFLOCK_HB64         5

#endif /* HB_DBINFO_CH_ */
