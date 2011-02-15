/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Advantage Database Server RDD
 *
 * Copyright 2000 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/*
   If you want to limit your app to use an ADS version
   earlier than the current one, you must set this constant
      ADS_LIB_VERSION
   to the *latest* version you want to allow/require, as in
      -DADS_LIB_VERSION=500

   As of 6/7/2004, the default supports linking to v6 and v7,
   as there are no v7-specific features yet.
   It does cover v6 data dictionary support, built-in
   Internet Server capabilities, etc.

   So to link to v5, do this:
   1) Link with an ace32.lib created from the version 5
         dll that imports these functions, and

   2) Set this "define" when compiling rddads:
      -DADS_LIB_VERSION=500
*/

/* Supported file types */
#define ADS_NTX                           1
#define ADS_CDX                           2
#define ADS_ADT                           3
#define ADS_VFP                           4

/* Advantage Optimized Filter (AOF) optimization levels */
#define ADS_OPTIMIZED_FULL                1
#define ADS_OPTIMIZED_PART                2
#define ADS_OPTIMIZED_NONE                3

/* Advantage Optimized Filter (AOF) resolution options */
#define ADS_RESOLVE_IMMEDIATE             1
#define ADS_RESOLVE_DYNAMIC               2

/* Advantage Optimized Filter (AOF) customization options */
#define ADS_AOF_ADD_RECORD                1
#define ADS_AOF_REMOVE_RECORD             2
#define ADS_AOF_TOGGLE_RECORD             3

/* For retrieving scope settings
   In the Harbour RDD, use TOPSCOPE and BOTTOMSCOPE as the values are
   different (Top = 0, Bottom = 1)
   #define ADS_TOP                        1
   #define ADS_BOTTOM                     2
*/

/* for calls that can optionally use filters */
#define ADS_RESPECTFILTERS                1
#define ADS_IGNOREFILTERS                 2
#define ADS_RESPECTSCOPES                 3

/* Server type constants for ORing with AdsSetServerType() */
#define ADS_LOCAL_SERVER                  1
#define ADS_REMOTE_SERVER                 2
#define ADS_AIS_SERVER                    4

/* character set types */
#define ADS_ANSI                          1
#define ADS_OEM                           2

/*
 * Constants for AdsMgGetServerType()
 * Note ADS_MGMT_NETWARE_SERVER remains for backwards compatibility only.
 */
#define ADS_MGMT_NETWARE_SERVER           1
#define ADS_MGMT_NETWARE4_OR_OLDER_SERVER 1
#define ADS_MGMT_NT_SERVER                2
#define ADS_MGMT_LOCAL_SERVER             3
#define ADS_MGMT_WIN9X_SERVER             4
#define ADS_MGMT_NETWARE5_OR_NEWER_SERVER 5
#define ADS_MGMT_LINUX_SERVER             6

/* ACE Handle types */
#define ADS_CONNECTION                    1
#define ADS_TABLE                         2
#define ADS_INDEX_ORDER                   3
#define ADS_STATEMENT                     4
#define ADS_CURSOR                        5
#define ADS_DATABASE_CONNECTION           6
#define ADS_SYS_ADMIN_CONNECTION          7
#define ADS_FTS_INDEX_ORDER               8

#define AE_NO_CONNECTION                  5036

#define ADS_DD_TABLE_OBJECT               1
#define ADS_DD_RELATION_OBJECT            2
#define ADS_DD_INDEX_FILE_OBJECT          3
#define ADS_DD_FIELD_OBJECT               4
#define ADS_DD_COLUMN_OBJECT              4
#define ADS_DD_INDEX_OBJECT               5
#define ADS_DD_VIEW_OBJECT                6
#define ADS_DD_VIEW_OR_TABLE_OBJECT       7  /* Used in AdsFindFirst/NextTable */
#define ADS_DD_USER_OBJECT                8
#define ADS_DD_USER_GROUP_OBJECT          9
#define ADS_DD_PROCEDURE_OBJECT           10
#define ADS_DD_DATABASE_OBJECT            11
#define ADS_DD_LINK_OBJECT                12
#define ADS_DD_TABLE_VIEW_OR_LINK_OBJECT  13  /* Used in v6.2 AdsFindFirst/NextTable */

/* Common properties numbers < 100 */
#define ADS_DD_COMMENT                    1
#define ADS_DD_VERSION                    2
#define ADS_DD_USER_DEFINED_PROP          3

/* Database properties between 100 and 199 */
#define ADS_DD_DEFAULT_TABLE_PATH         100
#define ADS_DD_ADMIN_PASSWORD             101
#define ADS_DD_TEMP_TABLE_PATH            102
#define ADS_DD_LOG_IN_REQUIRED            103
#define ADS_DD_VERIFY_ACCESS_RIGHTS       104
#define ADS_DD_ENCRYPT_TABLE_PASSWORD     105
#define ADS_DD_ENCRYPT_NEW_TABLE          106
#define ADS_DD_ENABLE_INTERNET            107
#define ADS_DD_INTERNET_SECURITY_LEVEL    108
#define ADS_DD_MAX_FAILED_ATTEMPTS        109
#define ADS_DD_ALLOW_ADSSYS_NET_ACCESS    110
#define ADS_DD_VERSION_MAJOR              111  /* properties for customer dd version */
#define ADS_DD_VERSION_MINOR              112

/* Table properties between 200 and 299 */
#define ADS_DD_TABLE_VALIDATION_EXPR      200
#define ADS_DD_TABLE_VALIDATION_MSG       201
#define ADS_DD_TABLE_PRIMARY_KEY          202
#define ADS_DD_TABLE_AUTO_CREATE          203
#define ADS_DD_TABLE_TYPE                 204
#define ADS_DD_TABLE_PATH                 205
#define ADS_DD_TABLE_FIELD_COUNT          206
#define ADS_DD_TABLE_RI_GRAPH             207
#define ADS_DD_TABLE_OBJ_ID               208
#define ADS_DD_TABLE_RI_XY                209
#define ADS_DD_TABLE_IS_RI_PARENT         210
#define ADS_DD_TABLE_RELATIVE_PATH        211
#define ADS_DD_TABLE_CHAR_TYPE            212
#define ADS_DD_TABLE_DEFAULT_INDEX        213
#define ADS_DD_TABLE_ENCRYPTION           214
#define ADS_DD_TABLE_MEMO_BLOCK_SIZE      215
#define ADS_DD_TABLE_PERMISSION_LEVEL     216

/* Field properties between 300 - 399 */
#define ADS_DD_FIELD_DEFAULT_VALUE        300
#define ADS_DD_FIELD_CAN_NULL             301
#define ADS_DD_FIELD_MIN_VALUE            302
#define ADS_DD_FIELD_MAX_VALUE            303
#define ADS_DD_FIELD_VALIDATION_MSG       304
#define ADS_DD_FIELD_DEFINITION           305
#define ADS_DD_FIELD_TYPE                 306
#define ADS_DD_FIELD_LENGTH               307
#define ADS_DD_FIELD_DECIMAL              308

/* Index tag properties between 400 - 499 */
#define ADS_DD_INDEX_FILE_NAME            400
#define ADS_DD_INDEX_EXPRESSION           401
#define ADS_DD_INDEX_CONDITION            402
#define ADS_DD_INDEX_OPTIONS              403
#define ADS_DD_INDEX_KEY_LENGTH           404
#define ADS_DD_INDEX_KEY_TYPE             405

/* RI properties between 500-599 */
#define ADS_DD_RI_PARENT_GRAPH            500
#define ADS_DD_RI_PRIMARY_TABLE           501
#define ADS_DD_RI_PRIMARY_INDEX           502
#define ADS_DD_RI_FOREIGN_TABLE           503
#define ADS_DD_RI_FOREIGN_INDEX           504
#define ADS_DD_RI_UPDATERULE              505
#define ADS_DD_RI_DELETERULE              506
#define ADS_DD_RI_NO_PKEY_ERROR           507
#define ADS_DD_RI_CASCADE_ERROR           508

/* User properties between 600-699 */
#define ADS_DD_USER_GROUP_NAME            600

/* View properties between 700-749 */
#define ADS_DD_VIEW_STMT                  700
#define ADS_DD_VIEW_STMT_LEN              701

/* Stored procedure properties 800-899 */
#define ADS_DD_PROC_INPUT                 800
#define ADS_DD_PROC_OUTPUT                801
#define ADS_DD_PROC_DLL_NAME              802
#define ADS_DD_PROC_DLL_FUNCTION_NAME     803
#define ADS_DD_PROC_INVOKE_OPTION         804

/* Index file properties 900-999 */
#define ADS_DD_INDEX_FILE_PATH            900
#define ADS_DD_INDEX_FILE_PAGESIZE        901

/*
 * Object rights properties 1001 - 1099 .  They can be used
 * with either user or user group objects.
 */
#define ADS_DD_TABLES_RIGHTS              1001
#define ADS_DD_VIEWS_RIGHTS               1002
#define ADS_DD_PROCS_RIGHTS               1003
#define ADS_DD_OBJECTS_RIGHTS             1004
#define ADS_DD_FREE_TABLES_RIGHTS         1005

/* User Properties 1101 - 1199 */
#define ADS_DD_USER_PASSWORD              1101
#define ADS_DD_USER_GROUP_MEMBERSHIP      1102
#define ADS_DD_USER_BAD_LOGINS            1103

/* User group Properties 1201 - 1299 */
/* None at this moment. */

/* Link properties 1301 - 1399 */
#define ADS_DD_LINK_PATH                  1300
#define ADS_DD_LINK_OPTIONS               1301
#define ADS_DD_LINK_USERNAME              1302

#define ADS_DD_LEVEL_0                    0
#define ADS_DD_LEVEL_1                    1
#define ADS_DD_LEVEL_2                    2

/* Referential Integrity (RI) update and delete rules */
#define ADS_DD_RI_CASCADE                 1
#define ADS_DD_RI_RESTRICT                2
#define ADS_DD_RI_SETNULL                 3
#define ADS_DD_RI_SETDEFAULT              4

/* Default Field Value Options */
#define ADS_DD_DFV_UNKNOWN                1
#define ADS_DD_DFV_NONE                   2
#define ADS_DD_DFV_VALUES_STORED          3
