/*******************************************************************************
* Source File  : ace.h
* Copyright    : 1996-2001 Extended Systems, Inc.
* Description  : This is the main header file for the Advantage Client
*                Engine.  It contains the type definitions, constants,
*                and prototypes for the APIs
*******************************************************************************/

#ifndef __ACE_INCLUDED__
#define __ACE_INCLUDED__


#if defined( unix )
   #ifndef ADS_LINUX
   #define ADS_LINUX
   #endif
#endif

#if defined( ADS_LINUX )
   /* #include "unixutils.h" */


   #define ADS_PATH_DELIMITER    '/'
   #ifdef ASANLM
      #define delay(x) AdsSleep(x)
   #endif
#endif


#if defined( ADS_LINUX ) && defined( ACE )
   /* This makes the callback functions compile in linux */
   #define WINAPI   /* nothing */
   #define FARPROC  void*
   #define HWND     void*
#endif  // ADS_LINUX && ACE


#if defined(ADS_LINUX) || defined(__GNUC__)
   #pragma pack( 1 )
#else
   #pragma pack( push, 1 )
#endif

/* This forces a warning for single equals in if statements */
#if defined( WIN32 ) && !defined( ADS_LINUX )
   /* 16-bit compiler doesn't seem to like this */
   /* MingWin reports "warning: ignoring pragma: )" */
   #pragma warning( error : 4706 )

   #define ADS_PATH_DELIMITER    '\\'
#endif

/* added to get access() prototype */
#if defined( ADS_LINUX ) && !defined( NATIVE_SQL )
   #include <unistd.h>
#endif

/* data type definitions */
#ifndef RDDUNVRS_H
   typedef unsigned long  UNSIGNED32;
   typedef long           SIGNED32;
   typedef unsigned short UNSIGNED16;
   typedef short          SIGNED16;
   typedef unsigned char  UNSIGNED8;
   typedef char           SIGNED8;
#endif
   typedef unsigned long  ADSHANDLE;
   typedef double         DOUBLE;

#ifdef WIN32
   typedef ULONGLONG   UNSIGNED64;
   typedef LONGLONG    SIGNED64;
#endif

#define VOID   void
#define EXTERN extern
#define STATIC static


#if defined( ASANLM ) || defined( ADS_LINUX ) || defined( ASANT ) || defined( NLM ) || defined( ADS_NT ) || defined( ADS_WIN9X ) || defined( STAND_ALONE_EXE )
   #define ENTRYPOINT
#elif defined( WIN32 ) && !defined( __BORLANDC__ ) && ! defined( __GNUC__ )
   #define ENTRYPOINT _declspec( dllexport ) WINAPI
#else
   #define ENTRYPOINT _export WINAPI
#endif


/* Logical constants */
#define ADS_FALSE                0
#define ADS_TRUE                 1


/* This is for parameters to routines that accept a default setting */
#define ADS_DEFAULT              0

/* character set types */
#define ADS_ANSI                 1
#define ADS_OEM                  2

/* rights checking options */
#define ADS_CHECKRIGHTS          1
#define ADS_IGNORERIGHTS         2

/* options for connecting to Advantage servers - can be ORed together */
#define ADS_INC_USERCOUNT        0x00000001
#define ADS_STORED_PROC_CONN     0x00000002
#define ADS_COMPRESS_ALWAYS      0x00000004
#define ADS_COMPRESS_NEVER       0x00000008
#define ADS_COMPRESS_INTERNET    0x0000000C

/* options for opening tables - can be ORed together */
#define ADS_EXCLUSIVE            0x00000001
#define ADS_READONLY             0x00000002
#define ADS_SHARED               0x00000004
#define ADS_CLIPPER_MEMOS        0x00000008

/* Options for creating indexes - can be ORed together */
#define ADS_ASCENDING            0x00000000
#define ADS_UNIQUE               0x00000001
#define ADS_COMPOUND             0x00000002
#define ADS_CUSTOM               0x00000004
#define ADS_DESCENDING           0x00000008
#define ADS_USER_DEFINED         0x00000010

/* Options for returning string values */
#define ADS_NONE                 0x00000000
#define ADS_LTRIM                0x00000001
#define ADS_RTRIM                0x00000002
#define ADS_TRIM                 0x00000003

/* this is for passing null terminated strings */
#define ADS_NTS    ( ( UNSIGNED16 ) -1 )

/* locking compatibility */
#define ADS_COMPATIBLE_LOCKING   0
#define ADS_PROPRIETARY_LOCKING  1

/* settings for seeks */
#define ADS_SOFTSEEK             0x0001
#define ADS_HARDSEEK             0x0002
#define ADS_SEEKGT               0x0004

/* data types for seeks (and scopes) */
#define ADS_RAWKEY               1   /* no conversion performed on given data */
#define ADS_STRINGKEY            2   /* data given as a string */
#define ADS_DOUBLEKEY            4   /* data is a pointer to 8 byte double */


/* For retrieving scope settings */
#define ADS_TOP                  1
#define ADS_BOTTOM               2

/* for calls that can optionally use filters */
#define ADS_RESPECTFILTERS       0x0001
#define ADS_IGNOREFILTERS        0x0002
#define ADS_RESPECTSCOPES        0x0003
/*
 * This value is only used with GetRecordCount:  It can be ORed in with the
 * ignore filter value to force a read from the table header to get the most
 * current record count.
 */
#define ADS_REFRESHCOUNT         0x0004

/* Server type constants */
#define ADS_LOCAL_SERVER         0x0001
#define ADS_REMOTE_SERVER        0x0002
#define ADS_AIS_SERVER           0x0004

/* ACE Handle types */
#define ADS_CONNECTION            1
#define ADS_TABLE                 2
#define ADS_INDEX_ORDER           3
#define ADS_STATEMENT             4
#define ADS_CURSOR                5
#define ADS_DATABASE_CONNECTION   6
#define ADS_SYS_ADMIN_CONNECTION  7


/* ACE Cursor ReadOnly settings */
#define ADS_CURSOR_READONLY      1
#define ADS_CURSOR_READWRITE     2

/* ACE Cursor Constrain settings */
#define ADS_CONSTRAIN            1
#define ADS_NO_CONSTRAIN         2

/* Select Field Read settings */
#define ADS_READ_ALL_COLUMNS     1
#define ADS_READ_SELECT_COLUMNS  2

/* Disable server query optimization */
#define ADS_NO_OPTIMIZATION      1

/* Data dictionary new contraint property validation options */
#define ADS_NO_VALIDATE           0  /* Do not validate records against the new constraint */
#define ADS_VALIDATE_NO_SAVE      1  /* Delete record not meeting the constraint from the table, no save */
#define ADS_VALIDATE_WRITE_FAIL   2  /* Validate the records against the new constraint and overwrite
                                      * the fail table with records not meeting the constraint. */
#define ADS_VALIDATE_APPEND_FAIL  3  /* Validate the records against the new constraint and append
                                      * the failed records into the fail table */
#define ADS_VALIDATE_RETURN_ERROR 4  /* Validate the records against the new constraint and return
                                      * error if there is any record not meeting the constraint */

/* Possible result values from AdsCompareBookmarks. */
#define ADS_CMP_LESS    -1
#define ADS_CMP_EQUAL    0
#define ADS_CMP_GREATER  1


/* Success return code */
#define AE_SUCCESS                      0

/* Error codes */
#define AE_ALLOCATION_FAILED            5001
#define AE_COMM_MISMATCH                5002
#define AE_DATA_TOO_LONG                5003
#define AE_FILE_NOT_FOUND               5004
#define AE_INSUFFICIENT_BUFFER          5005
#define AE_INVALID_BOOKMARK             5006
#define AE_INVALID_CALLBACK             5007
#define AE_INVALID_CENTURY              5008
#define AE_INVALID_DATEFORMAT           5009
#define AE_INVALID_DECIMALS             5010
#define AE_INVALID_EXPRESSION           5011
#define AE_INVALID_FIELDDEF             5012
#define AE_INVALID_FILTER_OPTION        5013
#define AE_INVALID_INDEX_HANDLE         5014
#define AE_INVALID_INDEX_NAME           5015
#define AE_INVALID_INDEX_ORDER_NAME     5016
#define AE_INVALID_INDEX_TYPE           5017
#define AE_INVALID_HANDLE               5018
#define AE_INVALID_OPTION               5019
#define AE_INVALID_PATH                 5020
#define AE_INVALID_POINTER              5021
#define AE_INVALID_RECORD_NUMBER        5022
#define AE_INVALID_TABLE_HANDLE         5023
#define AE_INVALID_CONNECTION_HANDLE    5024
#define AE_INVALID_TABLETYPE            5025
#define AE_INVALID_WORKAREA             5026
#define AE_INVALID_CHARSETTYPE          5027
#define AE_INVALID_LOCKTYPE             5028
#define AE_INVALID_RIGHTSOPTION         5029
#define AE_INVALID_FIELDNUMBER          5030
#define AE_INVALID_KEY_LENGTH           5031
#define AE_INVALID_FIELDNAME            5032
#define AE_NO_DRIVE_CONNECTION          5033
#define AE_FILE_NOT_ON_SERVER           5034
#define AE_LOCK_FAILED                  5035
#define AE_NO_CONNECTION                5036
#define AE_NO_FILTER                    5037
#define AE_NO_SCOPE                     5038
#define AE_NO_TABLE                     5039
#define AE_NO_WORKAREA                  5040
#define AE_NOT_FOUND                    5041
#define AE_NOT_IMPLEMENTED              5042
#define AE_MAX_THREADS_EXCEEDED         5043
#define AE_START_THREAD_FAIL            5044
#define AE_TOO_MANY_INDEXES             5045
#define AE_TOO_MANY_TAGS                5046
#define AE_TRANS_OUT_OF_SEQUENCE        5047
#define AE_UNKNOWN_ERRCODE              5048
#define AE_UNSUPPORTED_LANGUAGE         5049
#define AE_NAME_TOO_LONG                5050
#define AE_DUPLICATE_ALIAS              5051
#define AE_TABLE_CLOSED_IN_TRANSACTION  5053
#define AE_PERMISSION_DENIED            5054
#define AE_STRING_NOT_FOUND             5055
#define AE_UNKNOWN_CHAR_SET             5056
#define AE_INVALID_OEM_CHAR_FILE        5057
#define AE_INVALID_MEMO_BLOCK_SIZE      5058
#define AE_NO_FILE_FOUND                5059
#define AE_NO_INF_LOCK                  5060
#define AE_INF_FILE_ERROR               5061
#define AE_RECORD_NOT_LOCKED            5062
#define AE_ILLEGAL_COMMAND_DURING_TRANS 5063
#define AE_TABLE_NOT_SHARED             5064
#define AE_INDEX_ALREADY_OPEN           5065
#define AE_INVALID_FIELD_TYPE           5066
#define AE_TABLE_NOT_EXCLUSIVE          5067
#define AE_NO_CURRENT_RECORD            5068
#define AE_PRECISION_LOST               5069
#define AE_INVALID_DATA_TYPE            5070
#define AE_DATA_TRUNCATED               5071
#define AE_TABLE_READONLY               5072
#define AE_INVALID_RECORD_LENGTH        5073
#define AE_NO_ERROR_MESSAGE             5074
#define AE_INDEX_SHARED                 5075
#define AE_INDEX_EXISTS                 5076
#define AE_CYCLIC_RELATION              5077
#define AE_INVALID_RELATION             5078
#define AE_INVALID_DAY                  5079
#define AE_INVALID_MONTH                5080
#define AE_CORRUPT_TABLE                5081
#define AE_INVALID_BINARY_OFFSET        5082
#define AE_BINARY_FILE_ERROR            5083
#define AE_INVALID_DELETED_BYTE_VALUE   5084
#define AE_NO_PENDING_UPDATE            5085
#define AE_PENDING_UPDATE               5086
#define AE_TABLE_NOT_LOCKED             5087
#define AE_CORRUPT_INDEX                5088
#define AE_AUTOOPEN_INDEX               5089
#define AE_SAME_TABLE                   5090
#define AE_INVALID_IMAGE                5091
#define AE_COLLATION_SEQUENCE_MISMATCH  5092
#define AE_INVALID_INDEX_ORDER          5093
#define AE_TABLE_CACHED                 5094
#define AE_INVALID_DATE                 5095
#define AE_ENCRYPTION_NOT_ENABLED       5096
#define AE_INVALID_PASSWORD             5097
#define AE_TABLE_ENCRYPTED              5098
#define AE_SERVER_MISMATCH              5099
#define AE_INVALID_USERNAME             5100
#define AE_INVALID_VALUE                5101
#define AE_INVALID_CONTINUE             5102
#define AE_UNRECOGNIZED_VERSION         5103
#define AE_RECORD_ENCRYPTED             5104
#define AE_UNRECOGNIZED_ENCRYPTION      5105
#define AE_INVALID_SQLSTATEMENT_HANDLE  5106
#define AE_INVALID_SQLCURSOR_HANDLE     5107
#define AE_NOT_PREPARED                 5108
#define AE_CURSOR_NOT_CLOSED            5109
#define AE_INVALID_SQL_PARAM_NUMBER     5110
#define AE_INVALID_SQL_PARAM_NAME       5111
#define AE_INVALID_COLUMN_NUMBER        5112
#define AE_INVALID_COLUMN_NAME          5113
#define AE_INVALID_READONLY_OPTION      5114
#define AE_IS_CURSOR_HANDLE             5115
#define AE_INDEX_EXPR_NOT_FOUND         5116
#define AE_NOT_DML                      5117
#define AE_INVALID_CONSTRAIN_TYPE       5118
#define AE_INVALID_CURSORHANDLE         5119
#define AE_OBSOLETE_FUNCTION            5120
#define AE_TADSDATASET_GENERAL          5121
#define AE_UDF_OVERWROTE_BUFFER         5122
#define AE_INDEX_UDF_NOT_SET            5123
#define AE_CONCURRENT_PROBLEM           5124
#define AE_INVALID_DICTIONARY_HANDLE    5125
#define AE_INVALID_PROPERTY_ID          5126
#define AE_INVALID_PROPERTY             5127
#define AE_DICTIONARY_ALREADY_EXISTS    5128
#define AE_INVALID_FIND_HANDLE          5129
#define AE_DD_REQUEST_NOT_COMPLETED     5130
#define AE_INVALID_OBJECT_ID            5131
#define AE_INVALID_OBJECT_NAME          5132
#define AE_INVALID_PROPERTY_LENGTH      5133
#define AE_INVALID_KEY_OPTIONS          5134
#define AE_CONSTRAINT_VALIDATION_ERROR  5135
#define AE_INVALID_OBJECT_TYPE          5136
#define AE_NO_OBJECT_FOUND              5137
#define AE_PROPERTY_NOT_SET             5138
#define AE_NO_PRIMARY_KEY_EXISTS        5139
#define AE_LOCAL_CONN_DISABLED          5140
#define AE_RI_RESTRICT                  5141
#define AE_RI_CASCADE                   5142
#define AE_RI_FAILED                    5143
#define AE_RI_CORRUPTED                 5144
#define AE_RI_UNDO_FAILED               5145
#define AE_RI_RULE_EXISTS               5146
#define AE_COLUMN_CANNOT_BE_NULL        5147
#define AE_MIN_CONSTRAINT_VIOLATION     5148
#define AE_MAX_CONSTRAINT_VIOLATION     5149
#define AE_RECORD_CONSTRAINT_VIOLATION  5150
#define AE_CANNOT_DELETE_TEMP_INDEX     5151
#define AE_RESTRUCTURE_FAILED           5152
#define AE_INVALID_STATEMENT            5153
#define AE_STORED_PROCEDURE_FAILED      5154
#define AE_INVALID_DICTIONARY_FILE      5155
#define AE_NOT_MEMBER_OF_GROUP          5156
#define AE_ALREADY_MEMBER_OF_GROUP      5157
#define AE_INVALID_OBJECT_RIGHT         5158
#define AE_INVALID_OBJECT_PERMISSION    5158    /* Note that this is same as above. The word
                                                 * permission is more commonly used.
                                                 */
#define AE_CANNOT_OPEN_DATABASE_TABLE   5159
#define AE_INVALID_CONSTRAINT           5160
#define AE_NOT_ADMINISTRATOR            5161
#define AE_NO_TABLE_ENCRYPTION_PASSWORD 5162
#define AE_TABLE_NOT_ENCRYPTED          5163
#define AE_INVALID_ENCRYPTION_VERSION   5164
#define AE_NO_STORED_PROC_EXEC_RIGHTS   5165
#define AE_DD_UNSUPPORTED_DEPLOYMENT    5166
#define AE_INFO_AUTO_CREATION_OCCURRED  5168
#define AE_INFO_COPY_MADE_BY_CLIENT     5169
#define AE_DATABASE_REQUIRE_NEW_SERVER  5170

/* Available OEM Languages (for Clipper compatibility) */
#define ADS_LANG_USA          "USA"
#define ADS_LANG_DANISH       "DANISH"
#define ADS_LANG_DUTCH        "DUTCH"
#define ADS_LANG_FINNISH      "FINNISH"
#define ADS_LANG_FRENCH       "FRENCH"
#define ADS_LANG_GERMAN       "GERMAN"
#define ADS_LANG_GREEK437     "GREEK437"
#define ADS_LANG_GREEK851     "GREEK851"
#define ADS_LANG_ICELD850     "ICELD850"
#define ADS_LANG_ICELD861     "ICELD861"
#define ADS_LANG_ITALIAN      "ITALIAN"
#define ADS_LANG_NORWEGN      "NORWEGN"
#define ADS_LANG_PORTUGUE     "PORTUGUE"
#define ADS_LANG_SPANISH      "SPANISH"
#define ADS_LANG_SWEDISH      "SWEDISH"
#define ADS_LANG_MAZOVIA      "MAZOVIA"
#define ADS_LANG_PC_LATIN     "PC_LATIN"
#define ADS_LANG_ISOLATIN     "ISOLATIN"
#define ADS_LANG_RUSSIAN      "RUSSIAN"
#define ADS_LANG_NTXCZ852     "NTXCZ852"
#define ADS_LANG_NTXCZ895     "NTXCZ895"
#define ADS_LANG_NTXSL852     "NTXSL852"
#define ADS_LANG_NTXSL895     "NTXSL895"
#define ADS_LANG_NTXHU852     "NTXHU852"


/* Supported file types */
#define ADS_DATABASE_TABLE       ADS_DEFAULT
#define ADS_NTX                  1
#define ADS_CDX                  2
#define ADS_ADT                  3

/* for retrieving file names of tables */
#define ADS_BASENAME             1
#define ADS_BASENAMEANDEXT       2
#define ADS_FULLPATHNAME         3
#define ADS_DATADICTIONARY_NAME  4


/* Advantage Optimized Filter (AOF) optimization levels */
#define ADS_OPTIMIZED_FULL       1
#define ADS_OPTIMIZED_PART       2
#define ADS_OPTIMIZED_NONE       3

/* Advantage Optimized Filter (AOF) resolution options */
#define ADS_RESOLVE_IMMEDIATE    1
#define ADS_RESOLVE_DYNAMIC      2

/* Advantage Optimized Filter (AOF) customization options */
#define ADS_AOF_ADD_RECORD       1
#define ADS_AOF_REMOVE_RECORD    2
#define ADS_AOF_TOGGLE_RECORD    3

/* Stored procedure or trigger type */
#define ADS_STORED_PROC          0x00000001


/* some maximum values used by the client */
/* NOTE:  constants meant for string length exclude space for null terminator */
#define ADS_MAX_DATEMASK         12
#define ADS_MAX_ERROR_LEN        600
#define ADS_MAX_INDEX_EXPR_LEN   510   /* this is only accurate for index expressions */
#define ADS_MAX_KEY_LENGTH       256   /* maximum key value length.  This is the max key length
                                        * of NTX index.  CDX and ADI indexes have max key length
                                        * of 240.
                                        */
#define ADS_MAX_FIELD_NAME       128
#define ADS_MAX_DBF_FIELD_NAME   10    /* maximum length of field name in a DBF */
#define ADS_MAX_INDEXES          15    /* physical index files, NOT index orders */
#define ADS_MAX_PATH             260
#define ADS_MAX_TABLE_NAME       255   /* long file name */
#define ADS_MAX_TAG_NAME         128
#define ADS_MAX_TAGS             50    /* maximum for CDX/ADI file */
#define ADS_MAX_OBJECT_NAME      200   /* maximum length of DD object name */


/*
 * Valid range of page sizes for ADI indexes.  The default page size is 512
 * bytes.  Before using another page size, please read the section titled
 * "Index Page Size" in the Advantage Client Engine help file (ace.hlp)
 */
#define ADS_MIN_ADI_PAGESIZE     512
#define ADS_MAX_ADI_PAGESIZE     8192


/* data types */
#define ADS_TYPE_UNKNOWN         0
#define ADS_LOGICAL              1     /* 1 byte logical value */
#define ADS_NUMERIC              2     /* DBF character style numeric */
#define ADS_DATE                 3     /* Date field.  With ADS_NTX and ADS_CDX,
                                        * this is an 8 byte field of the form
                                        * CCYYMMDD.  With ADS_ADT, it is a
                                        * 4 byte Julian date. */
#define ADS_STRING               4     /* Character data */
#define ADS_MEMO                 5     /* Variable length character data */

/* the following are extended data types */
#define ADS_BINARY               6     /* BLOB - any data */
#define ADS_IMAGE                7     /* BLOB - bitmap */
#define ADS_VARCHAR              8     /* variable length character field */
#define ADS_COMPACTDATE          9     /* DBF date represented with 3 bytes */
#define ADS_DOUBLE               10    /* IEEE 8 byte floating point */
#define ADS_INTEGER              11    /* IEEE 4 byte signed long integer */

/* the following are supported with the ADT file only */
#define ADS_SHORTINT             12    /* IEEE 2 byte signed short integer */
#define ADS_TIME                 13    /* 4 byte long integer representing
                                        * milliseconds since midnight */
#define ADS_TIMESTAMP            14    /* 8 bytes.  High order 4 bytes are a
                                        * long integer representing Julian date.
                                        * Low order 4 bytes are a long integer
                                        * representing milliseconds since
                                        * midnight */
#define ADS_AUTOINC              15    /* 4 byte auto-increment value */
#define ADS_RAW                  16    /* Untranslated data */
#define ADS_CURDOUBLE            17    /* IEEE 8 byte floating point currency */


/*
 * supported User Defined Function types to be used with AdsRegisterUDF
 */
#define ADS_INDEX_UDF            1


/*
 * Constant for AdsMgGetConfigInfo
 */
#define ADS_MAX_CFG_PATH         256

/*
 * Constants for AdsMgGetServerType
 * Note ADS_MGMT_NETWARE_SERVER remains for backwards compatibility only
 */
#define ADS_MGMT_NETWARE_SERVER           1
#define ADS_MGMT_NETWARE4_OR_OLDER_SERVER 1
#define ADS_MGMT_NT_SERVER                2
#define ADS_MGMT_LOCAL_SERVER             3
#define ADS_MGMT_WIN9X_SERVER             4
#define ADS_MGMT_NETWARE5_OR_NEWER_SERVER 5
#define ADS_MGMT_LINUX_SERVER             6

/*
 * Constants for AdsMgGetLockOwner
 */
#define ADS_MGMT_NO_LOCK         1
#define ADS_MGMT_RECORD_LOCK     2
#define ADS_MGMT_FILE_LOCK       3

/*
 * Constants for MgGetInstallInfo
 */
#define ADS_REG_OWNER_LEN        36
#define ADS_REVISION_LEN         16
#define ADS_INST_DATE_LEN        16
#define ADS_OEM_CHAR_NAME_LEN    16
#define ADS_ANSI_CHAR_NAME_LEN   16
#define ADS_SERIAL_NUM_LEN       16

/*
 * Constants for MgGetOpenTables
 */
#define ADS_MGMT_MAX_PATH              260
#define ADS_MGMT_PROPRIETARY_LOCKING   1
#define ADS_MGMT_CDX_LOCKING           2
#define ADS_MGMT_NTX_LOCKING           3
#define ADS_MGMT_ADT_LOCKING           4

#define ADS_MAX_USER_NAME        50

#define ADS_MAX_ADDRESS_SIZE     30

/*
 * Management API structures
 */
typedef struct
   {
   double      dPercentCheckSums;   /* % of pkts with checksum failures */
   UNSIGNED32  ulTotalPackets;      /* Total packets received           */
   UNSIGNED32  ulRcvPktOutOfSeq;    /* Receive packets out of sequence  */
   UNSIGNED32  ulNotLoggedIn;       /* Packet owner not logged in       */
   UNSIGNED32  ulRcvReqOutOfSeq;    /* Receive requests out of sequence */
   UNSIGNED32  ulCheckSumFailures;  /* Checksum failures                */
   UNSIGNED32  ulDisconnectedUsers; /* Server initiated disconnects     */
   UNSIGNED32  ulPartialConnects;   /* Removed partial connections      */
   UNSIGNED32  ulInvalidPackets;    /* Rcvd invalid packets (NT only)   */
   UNSIGNED32  ulRecvFromErrors;    /* RecvFrom failed (NT only)        */
   UNSIGNED32  ulSendToErrors;      /* SendTo failed (NT only)          */
   } ADS_MGMT_COMM_STATS;

typedef struct
   {
   UNSIGNED32  ulNumConnections;          /* number connections            */
   UNSIGNED32  ulNumWorkAreas;            /* number work areas             */
   UNSIGNED32  ulNumTables;               /* number tables                 */
   UNSIGNED32  ulNumIndexes;              /* number indexes                */
   UNSIGNED32  ulNumLocks;                /* number locks                  */
   UNSIGNED32  ulUserBufferSize;          /* user buffer                   */
   UNSIGNED32  ulStatDumpInterval;        /* statistics dump interval      */
   UNSIGNED32  ulErrorLogMax;             /* max size of error log         */
   UNSIGNED32  ulNumTPSHeaderElems;       /* number TPS header elems       */
   UNSIGNED32  ulNumTPSVisibilityElems;   /* number TPS vis elems          */
   UNSIGNED32  ulNumTPSMemoTransElems;    /* number TPS memo elems         */
   UNSIGNED16  usNumReceiveECBs;          /* number rcv ECBs (NLM only)    */
   UNSIGNED16  usNumSendECBs;             /* number send ECBs (NLM only)   */
   UNSIGNED16  usNumBurstPackets;         /* number packets per burst      */
   UNSIGNED16  usNumWorkerThreads;        /* number worker threads         */
   UNSIGNED16  usSortBuffSize;            /* index sort buffer size        */
   UNSIGNED8   ucReserved1;               /* reserved                      */
   UNSIGNED8   ucReserved2;               /* reserved                      */
   UNSIGNED8   aucErrorLog[ADS_MAX_CFG_PATH];    /* error log path         */
   UNSIGNED8   aucSemaphore[ADS_MAX_CFG_PATH];   /* semaphore file path    */
   UNSIGNED8   aucTransaction[ADS_MAX_CFG_PATH]; /* TPS log file path      */

   UNSIGNED8   ucReserved3;               /* reserved                      */
   UNSIGNED8   ucReserved4;               /* reserved                      */
   UNSIGNED16  usSendIPPort;              /* NT Service IP send port #     */
   UNSIGNED16  usReceiveIPPort;           /* NT Service IP rcv port #      */
   UNSIGNED8   ucUseIPProtocol;           /* Win9x only. Which protocol to use */
   UNSIGNED8   ucFlushEveryUpdate;        /* Win9x specific                */

   UNSIGNED32  ulGhostTimeout;            /* Diconnection time for partial connections */
   UNSIGNED32  ulFlushFrequency;          /* For local server only         */

   UNSIGNED32  ulKeepAliveTimeOut;     /* When not using semaophore files. In milliseconds */
   UNSIGNED8   ucDisplayNWLoginNames;  /* Display connections using user names. */
   UNSIGNED8   ucUseSemaphoreFiles;    /* Whether or not to use semaphore files */
   UNSIGNED8   ucUseDynamicAOFs;
   UNSIGNED8   ucUseInternet;          /* 0 if an Internet port is not specified. */

   UNSIGNED16  usInternetPort;         /* Internet Port */
   UNSIGNED16  usMaxConnFailures;      /* Maximum Internet connection failures allowed. */
   UNSIGNED32  ulInternetKeepAlive;    /* In Milliseconds */

   } ADS_MGMT_CONFIG_PARAMS;

typedef struct
   {
   UNSIGNED32  ulTotalConfigMem;          /* Total mem taken by cfg params */
   UNSIGNED32  ulConnectionMem;           /* memory taken by connections   */
   UNSIGNED32  ulWorkAreaMem;             /* memory taken by work areas    */
   UNSIGNED32  ulTableMem;                /* memory taken by tables        */
   UNSIGNED32  ulIndexMem;                /* memory taken by indexes       */
   UNSIGNED32  ulLockMem;                 /* memory taken by locks         */
   UNSIGNED32  ulUserBufferMem;           /* memory taken by user buffer   */
   UNSIGNED32  ulTPSHeaderElemMem;        /* memory taken by TPS hdr elems */
   UNSIGNED32  ulTPSVisibilityElemMem;    /* memory taken by TPS vis elems */
   UNSIGNED32  ulTPSMemoTransElemMem;     /* mem taken by TPS memo elems   */
   UNSIGNED32  ulReceiveEcbMem;           /* mem taken by rcv ECBs (NLM)   */
   UNSIGNED32  ulSendEcbMem;              /* mem taken by send ECBs (NLM)  */
   UNSIGNED32  ulWorkerThreadMem;         /* mem taken by worker threads   */
   } ADS_MGMT_CONFIG_MEMORY;

typedef struct
   {
   UNSIGNED32  ulUserOption;                            /* User option purchased*/
   UNSIGNED8   aucRegisteredOwner[ADS_REG_OWNER_LEN];   /* Registered owner     */
   UNSIGNED8   aucVersionStr[ADS_REVISION_LEN];         /* Advantage version    */
   UNSIGNED8   aucInstallDate[ADS_INST_DATE_LEN];       /* Install date string  */
   UNSIGNED8   aucOemCharName[ADS_OEM_CHAR_NAME_LEN];   /* OEM char language    */
   UNSIGNED8   aucAnsiCharName[ADS_ANSI_CHAR_NAME_LEN]; /* ANSI char language   */
   UNSIGNED8   aucEvalExpireDate[ADS_INST_DATE_LEN];    /* Eval expiration date */
   UNSIGNED8   aucSerialNumber[ADS_SERIAL_NUM_LEN];     /* Serial number string */
   } ADS_MGMT_INSTALL_INFO;

typedef struct
   {
   UNSIGNED16  usDays;        /* Number of days server has been up    */
   UNSIGNED16  usHours;       /* Number of hours server has been up   */
   UNSIGNED16  usMinutes;     /* Number of minutes server has been up */
   UNSIGNED16  usSeconds;     /* Number of seconds server has been up */
   } ADS_MGMT_UPTIME_INFO;

typedef struct
   {
   UNSIGNED32  ulInUse;       /* Number of items in use        */
   UNSIGNED32  ulMaxUsed;     /* Max number of items ever used */
   UNSIGNED32  ulRejected;    /* Number of items rejected      */
   } ADS_MGMT_USAGE_INFO;

typedef struct
   {
   UNSIGNED32           ulOperations;     /* Number operations since started */
   UNSIGNED32           ulLoggedErrors;   /* Number logged errors            */
   ADS_MGMT_UPTIME_INFO stUpTime;         /* Length of time ADS has been up  */
   ADS_MGMT_USAGE_INFO  stUsers;          /* Users in use, max, rejected     */
   ADS_MGMT_USAGE_INFO  stConnections;    /* Conns in use, max, rejected     */
   ADS_MGMT_USAGE_INFO  stWorkAreas;      /* WAs in use, max, rejected       */
   ADS_MGMT_USAGE_INFO  stTables;         /* Tables in use, max, rejected    */
   ADS_MGMT_USAGE_INFO  stIndexes;        /* Indexes in use, max, rejected   */
   ADS_MGMT_USAGE_INFO  stLocks;          /* Locks in use, max, rejected     */
   ADS_MGMT_USAGE_INFO  stTpsHeaderElems; /* TPS header elems in use, max    */
   ADS_MGMT_USAGE_INFO  stTpsVisElems;    /* TPS vis elems in use, max       */
   ADS_MGMT_USAGE_INFO  stTpsMemoElems;   /* TPS memo elems in use, max      */
   ADS_MGMT_USAGE_INFO  stWorkerThreads;  /* Worker threads in use, max      */
   } ADS_MGMT_ACTIVITY_INFO;

typedef struct
   {
   UNSIGNED8  aucUserName[ADS_MAX_USER_NAME]; /* Name of connected user    */
   UNSIGNED16 usConnNumber;                   /* NetWare conn # (NLM only) */
   UNSIGNED8  aucAuthUserName[ADS_MAX_USER_NAME];
   UNSIGNED8  aucAddress[ADS_MAX_ADDRESS_SIZE];
   } ADS_MGMT_USER_INFO;

typedef struct
   {
   UNSIGNED8  aucTableName[ADS_MGMT_MAX_PATH]; /* Fully qualified table name */
   UNSIGNED16 usLockType;                      /* Advantage locking mode     */
   } ADS_MGMT_TABLE_INFO;

typedef struct
   {
   UNSIGNED8  aucIndexName[ADS_MGMT_MAX_PATH]; /* Fully qualified table name */
   } ADS_MGMT_INDEX_INFO;

typedef struct
   {
   UNSIGNED32 ulRecordNumber;       /* Record number that is locked */
   } ADS_MGMT_RECORD_INFO;

typedef struct
   {
   UNSIGNED32 ulThreadNumber;                 /* Thread Number               */
   UNSIGNED16 usOpCode;                       /* Operation in progress       */
   UNSIGNED8  aucUserName[ADS_MAX_USER_NAME]; /* Name of user                */
   UNSIGNED16 usConnNumber;                   /* NetWare conn num (NLM only) */
   UNSIGNED16 usReserved1;                    /* Reserved                    */
   } ADS_MGMT_THREAD_ACTIVITY;

/*
 * Data dictionary properties related constants and structure
 */

typedef struct _ADD_FIELD_DESC_
   {
   UNSIGNED16  usFieldType;
   UNSIGNED16  usFieldLength;
   UNSIGNED16  usFieldDecimal;
   } ADD_FIELD_DESC;


#define ADS_DD_PROPERTY_NOT_AVAIL   0xFFFF
#define ADS_DD_MAX_PROPERTY_LEN     0xFFFE
#define ADS_DD_MAX_OBJECT_NAME_LEN  200

#define ADS_DD_TABLE_OBJECT          1
#define ADS_DD_RELATION_OBJECT       2
#define ADS_DD_INDEX_FILE_OBJECT     3
#define ADS_DD_FIELD_OBJECT          4
#define ADS_DD_COLUMN_OBJECT         4
#define ADS_DD_INDEX_OBJECT          5
#define ADS_DD_VIEW_OBJECT           6
#define ADS_DD_VIEW_OR_TABLE_OBJECT  7  /* Used in AdsFindFirst/NextTable */
#define ADS_DD_USER_OBJECT           8
#define ADS_DD_USER_GROUP_OBJECT     9
#define ADS_DD_PROCEDURE_OBJECT     10
#define ADS_DD_DATABASE_OBJECT      11


/* Common properties numbers < 100 */
#define ADS_DD_COMMENT           1
#define ADS_DD_VERSION           2
#define ADS_DD_USER_DEFINED_PROP 3


/* Database properties between 100 and 199 */
#define ADS_DD_DEFAULT_TABLE_PATH      100
#define ADS_DD_ADMIN_PASSWORD          101
#define ADS_DD_TEMP_TABLE_PATH         102
#define ADS_DD_LOG_IN_REQUIRED         103
#define ADS_DD_VERIFY_ACCESS_RIGHTS    104
#define ADS_DD_ENCRYPT_TABLE_PASSWORD  105
#define ADS_DD_ENCRYPT_NEW_TABLE       106
#define ADS_DD_ENABLE_INTERNET         107
#define ADS_DD_INTERNET_SECURITY_LEVEL 108
#define ADS_DD_MAX_FAILED_ATTEMPTS     109
#define ADS_DD_ALLOW_ADSSYS_NET_ACCESS 110


/* Table properties between 200 and 299 */
#define ADS_DD_TABLE_VALIDATION_EXPR   200
#define ADS_DD_TABLE_VALIDATION_MSG    201
#define ADS_DD_TABLE_PRIMARY_KEY       202
#define ADS_DD_TABLE_AUTO_CREATE       203
#define ADS_DD_TABLE_TYPE              204
#define ADS_DD_TABLE_PATH              205
#define ADS_DD_TABLE_FIELD_COUNT       206
#define ADS_DD_TABLE_RI_GRAPH          207
#define ADS_DD_TABLE_OBJ_ID            208
#define ADS_DD_TABLE_RI_XY             209
#define ADS_DD_TABLE_IS_RI_PARENT      210
#define ADS_DD_TABLE_RELATIVE_PATH     211
#define ADS_DD_TABLE_CHAR_TYPE         212
#define ADS_DD_TABLE_DEFAULT_INDEX     213
#define ADS_DD_TABLE_ENCRYPTION        214
#define ADS_DD_TABLE_MEMO_BLOCK_SIZE   215

/* Field properties between 300 - 399 */
#define ADS_DD_FIELD_DEFAULT_VALUE     300
#define ADS_DD_FIELD_CAN_NULL          301
#define ADS_DD_FIELD_MIN_VALUE         302
#define ADS_DD_FIELD_MAX_VALUE         303
#define ADS_DD_FIELD_VALIDATION_MSG    304
#define ADS_DD_FIELD_DEFINITION        305
#define ADS_DD_FIELD_TYPE              306
#define ADS_DD_FIELD_LENGTH            307
#define ADS_DD_FIELD_DECIMAL           308

/* Index tag properties between 400 - 499 */
#define ADS_DD_INDEX_FILE_NAME           400
#define ADS_DD_INDEX_EXPRESSION          401
#define ADS_DD_INDEX_CONDITION           402
#define ADS_DD_INDEX_OPTIONS             403
#define ADS_DD_INDEX_KEY_LENGTH          404
#define ADS_DD_INDEX_KEY_TYPE            405

/* RI properties between 500-599 */
#define ADS_DD_RI_PARENT_GRAPH         500
#define ADS_DD_RI_PRIMARY_TABLE        501
#define ADS_DD_RI_PRIMARY_INDEX        502
#define ADS_DD_RI_FOREIGN_TABLE        503
#define ADS_DD_RI_FOREIGN_INDEX        504
#define ADS_DD_RI_UPDATERULE           505
#define ADS_DD_RI_DELETERULE           506

/* Referential Integrity (RI) update and delete rules */
#define ADS_DD_RI_CASCADE       1
#define ADS_DD_RI_RESTRICT      2
#define ADS_DD_RI_SETNULL       3
#define ADS_DD_RI_SETDEFAULT    4

/* Default Field Value Options */
#define ADS_DD_DFV_UNKNOWN         1
#define ADS_DD_DFV_NONE            2
#define ADS_DD_DFV_VALUES_STORED   3

/* User properties between 600-699 */
#define ADS_DD_USER_GROUP_NAME         600

/* View properties between 700-749 */
#define ADS_DD_VIEW_STMT               700
#define ADS_DD_VIEW_STMT_LEN           701

/* Stored procedure properties 800-899 */
#define ADS_DD_PROC_INPUT              800
#define ADS_DD_PROC_OUTPUT             801
#define ADS_DD_PROC_DLL_NAME           802
#define ADS_DD_PROC_DLL_FUNCTION_NAME  803
#define ADS_DD_PROC_INVOKE_OPTION      804

/* Index file properties 900-999 */
#define ADS_DD_INDEX_FILE_PATH           900
#define ADS_DD_INDEX_FILE_PAGESIZE       901

/*
 * Object rights properties 1001 - 1099 .  They can be used
 * with either user or user group objects.
 */
#define ADS_DD_TABLES_RIGHTS      1001
#define ADS_DD_VIEWS_RIGHTS       1002
#define ADS_DD_PROCS_RIGHTS       1003
#define ADS_DD_OBJECTS_RIGHTS     1004
#define ADS_DD_FREE_TABLES_RIGHTS 1005

/* User Properties 1101 - 1199 */
#define ADS_DD_USER_PASSWORD           1101
#define ADS_DD_USER_GROUP_MEMBERSHIP   1102

#define ADS_DD_LEVEL_0  0
#define ADS_DD_LEVEL_1  1
#define ADS_DD_LEVEL_2  2


/* User group Properties 1201 - 1299 */
/* None at this moment. */
/* Also object rights properties 1001 - 1099 */

/* Supported permissions in the data dictionary */
#define ADS_PERMISSION_READ         1
#define ADS_PERMISSION_UPDATE       2
#define ADS_PERMISSION_INSERT       3
#define ADS_PERMISSION_DELETE       4
#define ADS_PERMISSION_EXECUTE      5
#define ADS_PERMISSION_INHERIT      6



/* stored procedure functions must be of this type */
#ifdef WINAPI
typedef UNSIGNED32 (WINAPI *STORED_PROCEDURE_PTR)
(
   UNSIGNED32  ulConnectionID, // (I) value used to associate a user/connection
                               //     and can be used to track the state
   UNSIGNED8   *pucUserName,   // (I) the user name who invoked this procedure
   UNSIGNED8   *pucPassword,   // (I) the user's password in encrypted form
   UNSIGNED8   *pucProcName,   // (I) the stored procedure name
   UNSIGNED32  ulRecNum,       // (I) reserved for triggers
   UNSIGNED8   *pucTable1,     // (I) table one.  For Stored Proc this table
                               //     contains all input parameters.  For
                               //     triggers, it contains the original field
                               //     values if the trigger is an OnUpdate or
                               //     OnDelete
   UNSIGNED8   *pucTable2      // (I) table two.  For Stored Proc this table
                               //     is empty and the users function will
                               //     optionally add rows to it as output.
                               //     For triggers, it contains the new field
                               //     values if the trigger is an OnUpdate or
                               //     OnInsert
);


typedef UNSIGNED32 (WINAPI *STARTUP_PROCEDURE_PTR)
(
   UNSIGNED32  ulConnectionID, // (I) value used to associate a user/connection
                               //     and can be used to track the state
   UNSIGNED8   *pucUserName,   // (I) the user name who invoked this procedure
   UNSIGNED8   *pucPassword   // (I) the user's password in encrypted form
);


typedef UNSIGNED32 (WINAPI *SHUTDOWN_PROCEDURE_PTR)
(
   UNSIGNED32  ulConnectionID, // (I) value used to associate a user/connection
                               //     and can be used to track the state
   UNSIGNED8   *pucUserName,   // (I) the user name who invoked this procedure
   UNSIGNED8   *pucPassword    // (I) the user's password in encrypted form
);

#endif

/*
 * This macro allows a numeric field value to be passed into functions
 * that expect field names.  If the user prefers to use column number,
 * then calls like this can be made:
 * ulRet = AdsGetDouble( hTable, ADSFIELD( 1 ), &dVal );
 * Where the first column is a numeric value to retrieve.
 */
#define ADSFIELD(x)   ((unsigned char*)(long)( x ))


#ifdef __cplusplus
   extern "C"
   {
#endif


UNSIGNED32 ENTRYPOINT AdsAddCustomKey( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsAppendRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsApplicationExit();

UNSIGNED32 ENTRYPOINT AdsAtBOF( ADSHANDLE    hTable,
                                UNSIGNED16   *pbBof );

UNSIGNED32 ENTRYPOINT AdsAtEOF( ADSHANDLE    hTable,
                                UNSIGNED16   *pbEof );

UNSIGNED32 ENTRYPOINT AdsBeginTransaction( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsBinaryToFile( ADSHANDLE   hTable,
                                       UNSIGNED8   *pucFldName,
                                       UNSIGNED8   *pucFileName );

UNSIGNED32 ENTRYPOINT AdsCacheOpenCursors( UNSIGNED16 usOpen );

UNSIGNED32 ENTRYPOINT AdsCacheOpenTables( UNSIGNED16 usOpen );

UNSIGNED32 ENTRYPOINT AdsCacheRecords( ADSHANDLE hTable,
                                       UNSIGNED16 usNumRecords );

UNSIGNED32 ENTRYPOINT AdsCancelUpdate( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsCheckExistence( ADSHANDLE    hConnect,
                                         UNSIGNED8    *pucFileName,
                                         UNSIGNED16   *pusOnDisk );

UNSIGNED32 ENTRYPOINT AdsClearAllScopes( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsClearDefault( void );

UNSIGNED32 ENTRYPOINT AdsClearFilter( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsClearRelation( ADSHANDLE hTableParent );

UNSIGNED32 ENTRYPOINT AdsClearScope( ADSHANDLE  hIndex,
                                     UNSIGNED16 usScopeOption );

UNSIGNED32 ENTRYPOINT AdsCloneTable( ADSHANDLE  hTable,
                                     ADSHANDLE  *phClone );

UNSIGNED32 ENTRYPOINT AdsCloseAllIndexes( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsCloseAllTables( void );

UNSIGNED32 ENTRYPOINT AdsCloseIndex( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsCloseTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsCommitTransaction( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsConnect( UNSIGNED8  *pucServerName,
                                  ADSHANDLE  *phConnect );

UNSIGNED32 ENTRYPOINT AdsConnect26( UNSIGNED8  *pucServerName,
                                    UNSIGNED16 usServerTypes,
                                    ADSHANDLE  *phConnect );

UNSIGNED32 ENTRYPOINT AdsConnect60( UNSIGNED8  *pucServerPath,
                                    UNSIGNED16 usServerTypes,
                                    UNSIGNED8  *pucUserName,
                                    UNSIGNED8  *pucPassword,
                                    UNSIGNED32 ulOptions,
                                    ADSHANDLE  *phConnect );

UNSIGNED32 ENTRYPOINT AdsContinue( ADSHANDLE    hTable,
                                   UNSIGNED16   *pbFound );

UNSIGNED32 ENTRYPOINT AdsConvertTable( ADSHANDLE   hObj,
                                       UNSIGNED16  usFilterOption,
                                       UNSIGNED8   *pucFile,
                                       UNSIGNED16  usTableType );

UNSIGNED32 ENTRYPOINT AdsCopyTable( ADSHANDLE   hObj,
                                    UNSIGNED16  usFilterOption,
                                    UNSIGNED8   *pucFile );

UNSIGNED32 ENTRYPOINT AdsCopyTableContents( ADSHANDLE    hObj,
                                            ADSHANDLE    hTableTo,
                                            UNSIGNED16   usFilterOption );

UNSIGNED32 ENTRYPOINT AdsCopyTableStructure( ADSHANDLE   hTable,
                                             UNSIGNED8   *pucFile );

UNSIGNED32 ENTRYPOINT AdsCreateIndex( ADSHANDLE    hObj,
                                      UNSIGNED8    *pucFileName,
                                      UNSIGNED8    *pucTag,
                                      UNSIGNED8    *pucExpr,
                                      UNSIGNED8    *pucCondition,
                                      UNSIGNED8    *pucWhile,
                                      UNSIGNED32   ulOptions,
                                      ADSHANDLE    *phIndex );

UNSIGNED32 ENTRYPOINT AdsCreateIndex61( ADSHANDLE    hObj,
                                        UNSIGNED8    *pucFileName,
                                        UNSIGNED8    *pucTag,
                                        UNSIGNED8    *pucExpr,
                                        UNSIGNED8    *pucCondition,
                                        UNSIGNED8    *pucWhile,
                                        UNSIGNED32   ulOptions,
                                        UNSIGNED32   ulPageSize,
                                        ADSHANDLE    *phIndex );

UNSIGNED32 ENTRYPOINT AdsCreateTable( ADSHANDLE    hConnection,
                                      UNSIGNED8    *pucName,
                                      UNSIGNED8    *pucAlias,
                                      UNSIGNED16   usTableType,
                                      UNSIGNED16   usCharType,
                                      UNSIGNED16   usLockType,
                                      UNSIGNED16   usCheckRights,
                                      UNSIGNED16   usMemoSize,
                                      UNSIGNED8    *pucFields,
                                      ADSHANDLE    *phTable );

UNSIGNED32 ENTRYPOINT AdsDDCreate( UNSIGNED8  *pucDictionaryPath,
                                   UNSIGNED16 usEncrypt,
                                   UNSIGNED8  *pucDescription,
                                   ADSHANDLE  *phDictionary );

UNSIGNED32 ENTRYPOINT AdsDDCreateRefIntegrity( ADSHANDLE  hDictionary,
                                               UNSIGNED8  *pucRIName,
                                               UNSIGNED8  *pucFailTable,
                                               UNSIGNED8  *pucParentTableName,
                                               UNSIGNED8  *pucParentTagName,
                                               UNSIGNED8  *pucChildTableName,
                                               UNSIGNED8  *pucChildTagName,
                                               UNSIGNED16 usUpdateRule,
                                               UNSIGNED16 usDeleteRule );

UNSIGNED32 ENTRYPOINT AdsDDRemoveRefIntegrity( ADSHANDLE  hDictionary,
                                               UNSIGNED8  *pucRIName );

UNSIGNED32 ENTRYPOINT AdsDDGetDatabaseProperty( ADSHANDLE  hObject,
                                                UNSIGNED16 usPropertyID,
                                                VOID       *pvProperty,
                                                UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetFieldProperty( ADSHANDLE  hObject,
                                             UNSIGNED8  *pucTableName,
                                             UNSIGNED8  *pucFieldName,
                                             UNSIGNED16 usPropertyID,
                                             VOID       *pvProperty,
                                             UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetIndexFileProperty( ADSHANDLE  hObject,
                                                 UNSIGNED8  *pucTableName,
                                                 UNSIGNED8  *pucIndexFileName,
                                                 UNSIGNED16 usPropertyID,
                                                 VOID       *pvProperty,
                                                 UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetIndexProperty( ADSHANDLE  hObject,
                                             UNSIGNED8  *pucTableName,
                                             UNSIGNED8  *pucIndexName,
                                             UNSIGNED16 usPropertyID,
                                             VOID       *pvProperty,
                                             UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetTableProperty( ADSHANDLE  hObject,
                                             UNSIGNED8  *pucTableName,
                                             UNSIGNED16 usPropertyID,
                                             VOID       *pvProperty,
                                             UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetUserGroupProperty( ADSHANDLE  hObject,
                                                 UNSIGNED8  *pucUserGroupName,
                                                 UNSIGNED16 usPropertyID,
                                                 VOID       *pvProperty,
                                                 UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetUserProperty( ADSHANDLE  hObject,
                                            UNSIGNED8  *pucUserName,
                                            UNSIGNED16 usPropertyID,
                                            VOID       *pvProperty,
                                            UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetViewProperty( ADSHANDLE  hObject,
                                            UNSIGNED8  *pucViewName,
                                            UNSIGNED16 usPropertyID,
                                            VOID       *pvProperty,
                                            UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetProcedureProperty( ADSHANDLE  hObject,
                                                 UNSIGNED8  *pucProcName,
                                                 UNSIGNED16 usPropertyID,
                                                 VOID       *pvProperty,
                                                 UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetRefIntegrityProperty( ADSHANDLE  hObject,
                                                    UNSIGNED8  *pucRIName,
                                                    UNSIGNED16 usPropertyID,
                                                    UNSIGNED8  *pucProperty,
                                                    UNSIGNED16 *pusPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDGetPermissions( ADSHANDLE  hDBConn,
                                           UNSIGNED8  *pucGrantee,
                                           UNSIGNED16 usObjectType,
                                           UNSIGNED8  *pucObjectName,
                                           UNSIGNED8  *pucParentName,
                                           UNSIGNED16 usGetInherited,
                                           UNSIGNED8  *pucPermissions,
                                           UNSIGNED16 *pusBufferLen );

UNSIGNED32 ENTRYPOINT AdsDDGrantPermission( ADSHANDLE  hAdminConn,
                                            UNSIGNED16 usObjectType,
                                            UNSIGNED8  *pucObjectName,
                                            UNSIGNED8  *pucParentName,
                                            UNSIGNED8  *pucGrantee,
                                            UNSIGNED16 usPermission );

UNSIGNED32 ENTRYPOINT AdsDDRevokePermission( ADSHANDLE  hAdminConn,
                                             UNSIGNED16 usObjectType,
                                             UNSIGNED8  *pucObjectName,
                                             UNSIGNED8  *pucParentName,
                                             UNSIGNED8  *pucGrantee,
                                             UNSIGNED16 usPermission );

UNSIGNED32 ENTRYPOINT AdsDDSetDatabaseProperty( ADSHANDLE  hDictionary,
                                        UNSIGNED16 usPropertyID,
                                        VOID       *pvProperty,
                                        UNSIGNED16 usPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDSetFieldProperty( ADSHANDLE  hDictionary,
                                             UNSIGNED8  *pucTableName,
                                             UNSIGNED8  *pucFieldName,
                                             UNSIGNED16 usPropertyID,
                                             VOID       *pvProperty,
                                             UNSIGNED16 usPropertyLen,
                                             UNSIGNED16 usValidateOption,
                                             UNSIGNED8  *pucFailTable );

UNSIGNED32 ENTRYPOINT AdsDDSetTableProperty( ADSHANDLE  hDictionary,
                                             UNSIGNED8  *pucTableName,
                                             UNSIGNED16 usPropertyID,
                                             VOID       *pvProperty,
                                             UNSIGNED16 usPropertyLen,
                                             UNSIGNED16 usValidateOption,
                                             UNSIGNED8  *pucFailTable );

UNSIGNED32 ENTRYPOINT AdsDDSetUserGroupProperty( ADSHANDLE  hDictionary,
                                                 UNSIGNED8  *pucUserGroupName,
                                                 UNSIGNED16 usPropertyID,
                                                 VOID       *pvProperty,
                                                 UNSIGNED16 usPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDSetUserProperty( ADSHANDLE  hDictionary,
                                            UNSIGNED8  *pucUserName,
                                            UNSIGNED16 usPropertyID,
                                            VOID       *pvProperty,
                                            UNSIGNED16 usPropertyLen );

UNSIGNED32 ENTRYPOINT AdsDDSetObjectAccessRights( ADSHANDLE  hDictionary,
                                                  UNSIGNED8  *pucObjectName,
                                                  UNSIGNED8  *pucAccessorName,
                                                  UNSIGNED8  *pucAllowedAccess );

UNSIGNED32 ENTRYPOINT AdsDDAddProcedure( ADSHANDLE  hDictionary,
                                         UNSIGNED8  *pucName,
                                         UNSIGNED8  *pucDLL,
                                         UNSIGNED8  *pucProcName,
                                         UNSIGNED32 ulInvokeOption,
                                         UNSIGNED8  *pucInParams,
                                         UNSIGNED8  *pucOutParams,
                                         UNSIGNED8  *pucComments );


UNSIGNED32 ENTRYPOINT AdsDDAddTable( ADSHANDLE    hDictionary,
                                     UNSIGNED8    *pucTableName,
                                     UNSIGNED8    *pucTablePath,
                                     UNSIGNED16   usTableType,
                                     UNSIGNED16   usCharType,
                                     UNSIGNED8    *pucIndexFiles,
                                     UNSIGNED8    *pucComments );

UNSIGNED32 ENTRYPOINT AdsDDAddView( ADSHANDLE      hDictionary,
                                    UNSIGNED8      *pucName,
                                    UNSIGNED8      *pucComments,
                                    UNSIGNED8      *pucSQL );

UNSIGNED32 ENTRYPOINT AdsDDAddIndexFile( ADSHANDLE    hDictionary,
                                         UNSIGNED8    *pucTableName,
                                         UNSIGNED8    *pucIndexFilePath,
                                         UNSIGNED8    *pucComment );

UNSIGNED32 ENTRYPOINT AdsDDCreateUser( ADSHANDLE    hDictionary,
                                       UNSIGNED8    *pucGroupName,
                                       UNSIGNED8    *pucUserName,
                                       UNSIGNED8    *pucPassword,
                                       UNSIGNED8    *pucDescription );

UNSIGNED32 ENTRYPOINT AdsDDAddUserToGroup( ADSHANDLE    hDictionary,
                                           UNSIGNED8    *pucGroupName,
                                           UNSIGNED8    *pucUserName );

UNSIGNED32 ENTRYPOINT AdsDDRemoveUserFromGroup( ADSHANDLE    hDictionary,
                                                UNSIGNED8    *pucGroupName,
                                                UNSIGNED8    *pucUserName );


UNSIGNED32 ENTRYPOINT AdsDDDeleteUser( ADSHANDLE    hDictionary,
                                       UNSIGNED8    *pucUserName );


UNSIGNED32 ENTRYPOINT AdsDDCreateUserGroup( ADSHANDLE    hDictionary,
                                            UNSIGNED8    *pucGroupName,
                                            UNSIGNED8    *pucDescription );

UNSIGNED32 ENTRYPOINT AdsDDDeleteUserGroup( ADSHANDLE    hDictionary,
                                            UNSIGNED8    *pucGroupName );

UNSIGNED32 ENTRYPOINT AdsDDDeleteIndex( ADSHANDLE    hDictionary,
                                        UNSIGNED8    *pucTableName,
                                        UNSIGNED8    *pucIndexName );

UNSIGNED32 ENTRYPOINT AdsDDRemoveIndexFile( ADSHANDLE    hDictionary,
                                            UNSIGNED8    *pucTableName,
                                            UNSIGNED8    *pucIndexFileName,
                                            UNSIGNED16   usDeleteFile );

UNSIGNED32 ENTRYPOINT AdsDDRemoveProcedure( ADSHANDLE  hDictionary,
                                            UNSIGNED8  *pucName );


UNSIGNED32 ENTRYPOINT AdsDDRemoveTable( ADSHANDLE    hObject,
                                        UNSIGNED8    *pucTableName,
                                        UNSIGNED16   usDeleteFiles );

UNSIGNED32 ENTRYPOINT AdsDDRemoveView( ADSHANDLE   hDictionary,
                                       UNSIGNED8   *pucName );

UNSIGNED32 ENTRYPOINT AdsDDFindFirstObject( ADSHANDLE  hObject,
                                            UNSIGNED16 usFindObjectType,
                                            UNSIGNED8  *pucParentName,
                                            UNSIGNED8  *pucObjectName,
                                            UNSIGNED16 *pusObjectNameLen,
                                            ADSHANDLE  *phFindHandle );

UNSIGNED32 ENTRYPOINT AdsDDFindNextObject( ADSHANDLE  hObject,
                                           ADSHANDLE  hFindHandle,
                                           UNSIGNED8  *pucObjectName,
                                           UNSIGNED16 *pusObjectNameLen );

UNSIGNED32 ENTRYPOINT AdsDDFindClose( ADSHANDLE hObject, ADSHANDLE hFindHandle );

UNSIGNED32 ENTRYPOINT AdsDecryptRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsDecryptTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsDeleteCustomKey( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsDeleteIndex( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsDeleteRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsDisableEncryption( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsDisableLocalConnections( void );

UNSIGNED32 ENTRYPOINT AdsDisconnect( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsEnableEncryption( ADSHANDLE hTable,
                                           UNSIGNED8 *pucPassword );

UNSIGNED32 ENTRYPOINT AdsEncryptRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsEncryptTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsEvalLogicalExpr(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pbResult );

UNSIGNED32 ENTRYPOINT AdsEvalNumericExpr(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              DOUBLE           *pdResult );

UNSIGNED32 ENTRYPOINT AdsEvalStringExpr(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED8        *pucResult,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsEvalTestExpr(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsExtractKey(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucKey,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsFailedTransactionRecovery( UNSIGNED8 *pucServer );

UNSIGNED32 ENTRYPOINT AdsFileToBinary(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       usBinaryType,
                              UNSIGNED8        *pucFileName );

UNSIGNED32 ENTRYPOINT AdsFindConnection(
                              UNSIGNED8        *pucServerName,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsFindConnection25(
                              UNSIGNED8        *pucFullPath,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsFindClose( ADSHANDLE hConnect, SIGNED32 lHandle );

UNSIGNED32 ENTRYPOINT AdsFindFirstTable( ADSHANDLE  hConnect,
                                         UNSIGNED8  *pucFileMask,
                                         UNSIGNED8  *pucFirstFile,
                                         UNSIGNED16 *pusFileLen,
                                         SIGNED32   *plHandle );

UNSIGNED32 ENTRYPOINT AdsFindNextTable( ADSHANDLE hConnect,
                                        SIGNED32 lHandle,
                                        UNSIGNED8 *pucFileName,
                                        UNSIGNED16 *pusFileLen );

UNSIGNED32 ENTRYPOINT AdsGetAllIndexes(
                              ADSHANDLE        hTable,
                              ADSHANDLE        ahIndex[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetAllLocks(
                              ADSHANDLE        hTable,
                              UNSIGNED32       aulLocks[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetAllTables(
                              ADSHANDLE        ahTable[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetBinary(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       ulOffset,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen );

UNSIGNED32 ENTRYPOINT AdsGetBinaryLength(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetBookmark(
                              ADSHANDLE        hTable,
                              ADSHANDLE        *phBookmark );

UNSIGNED32 ENTRYPOINT AdsGetBookmark60(
                              ADSHANDLE        hObj,
                              UNSIGNED8        *pucBookmark,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetBookmarkLength(
                              ADSHANDLE        hObj,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsCompareBookmarks(
                              UNSIGNED8        *pucBookmark1,
                              UNSIGNED8        *pucBookmark2,
                              SIGNED32         *plResult );

UNSIGNED32 ENTRYPOINT AdsGetCollationLang(
                              UNSIGNED8  *pucLang,
                              UNSIGNED16 *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetConnectionType(
                              ADSHANDLE        hConnect,
                              UNSIGNED16       *pusConnectType );

UNSIGNED32 ENTRYPOINT AdsGetDate(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetDateFormat(
                              UNSIGNED8        *pucFormat,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetDecimals( UNSIGNED16 *pusDecimals );

UNSIGNED32 ENTRYPOINT AdsGetDefault(
                              UNSIGNED8        *pucDefault,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetDeleted( UNSIGNED16 *pbUseDeleted );

UNSIGNED32 ENTRYPOINT AdsGetDouble(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              DOUBLE           *pdValue );

UNSIGNED32 ENTRYPOINT AdsGetEpoch( UNSIGNED16 *pusCentury );

UNSIGNED32 ENTRYPOINT AdsGetErrorString(
                              UNSIGNED32       ulErrCode,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED16       *pusBufLen );

UNSIGNED32 ENTRYPOINT AdsGetExact( UNSIGNED16 *pbExact );

UNSIGNED32 ENTRYPOINT AdsGetExact22(
                              ADSHANDLE        hObj,
                              UNSIGNED16       *pbExact );

UNSIGNED32 ENTRYPOINT AdsGetField(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen,
                              UNSIGNED16       usOption );

UNSIGNED32 ENTRYPOINT AdsGetFieldDecimals(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusDecimals );

UNSIGNED32 ENTRYPOINT AdsGetFieldLength(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetFieldName(
                              ADSHANDLE        hTable,
                              UNSIGNED16       usFld,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusBufLen );

UNSIGNED32 ENTRYPOINT AdsGetFieldNum(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetFieldOffset(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulOffset );

UNSIGNED32 ENTRYPOINT AdsGetFieldType(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetFilter(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFilter,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetHandleLong(
                              ADSHANDLE        hObj,
                              UNSIGNED32       *pulVal );

UNSIGNED32 ENTRYPOINT AdsGetHandleType(
                              ADSHANDLE        hObj,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetIndexCondition(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetIndexExpr(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetIndexFilename(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       usOption,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetIndexHandle(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucIndexOrder,
                              ADSHANDLE        *phIndex );

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByOrder(
                              ADSHANDLE        hTable,
                              UNSIGNED16       usOrderNum,
                              ADSHANDLE        *phIndex );


UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByExpr(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED32       ulDescending,
                              ADSHANDLE        *phIndex );


UNSIGNED32 ENTRYPOINT AdsGetIndexName(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetIndexOrderByHandle( ADSHANDLE   hIndex,
                                                UNSIGNED16  *pusIndexOrder );

UNSIGNED32 ENTRYPOINT AdsGetJulian(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              SIGNED32         *plDate );

UNSIGNED32 ENTRYPOINT AdsGetKeyCount(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulCount );

UNSIGNED32 ENTRYPOINT AdsGetKeyNum(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulKey );

UNSIGNED32 ENTRYPOINT AdsGetKeyLength(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *pusKeyLength );

UNSIGNED32 ENTRYPOINT AdsGetKeyType(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *usKeyType );

UNSIGNED32 ENTRYPOINT AdsGetLastError(
                              UNSIGNED32       *pulErrCode,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED16       *pusBufLen );

UNSIGNED32 ENTRYPOINT AdsGetLastTableUpdate(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucDate,
                              UNSIGNED16       *pusDateLen );

UNSIGNED32 ENTRYPOINT AdsGetLogical(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pbValue );

UNSIGNED32 ENTRYPOINT AdsGetLong(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              SIGNED32         *plValue );

UNSIGNED32 ENTRYPOINT AdsGetMemoLength(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetMemoDataType(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetMilliseconds(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              SIGNED32         *plTime );

UNSIGNED32 ENTRYPOINT AdsGetNumFields(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusCount );

UNSIGNED32 ENTRYPOINT AdsGetNumIndexes(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetNumLocks(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetNumOpenTables( UNSIGNED16 *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetRecord(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucRec,
                              UNSIGNED32       *pulLen );

UNSIGNED32 ENTRYPOINT AdsGetRecordCount(
                              ADSHANDLE        hTable,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulCount );

UNSIGNED32 ENTRYPOINT AdsGetRecordNum(
                              ADSHANDLE        hTable,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulRec );

UNSIGNED32 ENTRYPOINT AdsGetRecordLength(
                              ADSHANDLE        hTable,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetRelKeyPos(
                              ADSHANDLE        hIndex,
                              DOUBLE           *pdPos );

UNSIGNED32 ENTRYPOINT AdsGetScope(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       usScopeOption,
                              UNSIGNED8        *pucScope,
                              UNSIGNED16       *pusBufLen );

UNSIGNED32 ENTRYPOINT AdsGetSearchPath(
                              UNSIGNED8        *pucPath,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetServerName(
                              ADSHANDLE        hConnect,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetServerTime(
                              ADSHANDLE        hConnect,
                              UNSIGNED8        *pucDateBuf,
                              UNSIGNED16       *pusDateBufLen,
                              SIGNED32         *plTime,
                              UNSIGNED8        *pucTimeBuf,
                              UNSIGNED16       *pusTimeBufLen );

UNSIGNED32 ENTRYPOINT AdsGetShort(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              SIGNED16         *psValue );

UNSIGNED32 ENTRYPOINT AdsGetString(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen,
                              UNSIGNED16       usOption );

UNSIGNED32 ENTRYPOINT AdsGetTableAlias(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucAlias,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetTableCharType(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusCharType );

UNSIGNED32 ENTRYPOINT AdsGetTableConnection(
                              ADSHANDLE        hTable,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsGetTableFilename(
                              ADSHANDLE        hTable,
                              UNSIGNED16       usOption,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetTableHandle(
                              UNSIGNED8        *pucName,
                              ADSHANDLE        *phTable );

UNSIGNED32 ENTRYPOINT AdsGetTableHandle25(
                              ADSHANDLE   hConnect,
                              UNSIGNED8   *pucName,
                              ADSHANDLE   *phTable );

UNSIGNED32 ENTRYPOINT AdsGetTableLockType(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusLockType );

UNSIGNED32 ENTRYPOINT AdsGetTableMemoSize(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusMemoSize );

UNSIGNED32 ENTRYPOINT AdsGetTableOpenOptions(
                              ADSHANDLE        hTable,
                              UNSIGNED32       *pulOptions );

UNSIGNED32 ENTRYPOINT AdsGetTableRights(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusRights );

UNSIGNED32 ENTRYPOINT AdsGetTableType(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetTime(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetVersion(
                              UNSIGNED32       *pulMajor,
                              UNSIGNED32       *pulMinor,
                              UNSIGNED8        *pucLetter,
                              UNSIGNED8        *pucDesc,
                              UNSIGNED16       *pusDescLen );

UNSIGNED32 ENTRYPOINT AdsGotoBookmark(
                              ADSHANDLE        hTable,
                              ADSHANDLE        hBookmark );

UNSIGNED32 ENTRYPOINT AdsGotoBookmark60(
                              ADSHANDLE        hObj,
                              UNSIGNED8        *pucBookmark );

UNSIGNED32 ENTRYPOINT AdsGotoBottom( ADSHANDLE hObj );

UNSIGNED32 ENTRYPOINT AdsGotoRecord(
                              ADSHANDLE        hTable,
                              UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsGotoTop( ADSHANDLE hObj );

UNSIGNED32 ENTRYPOINT AdsImageToClipboard( ADSHANDLE hTable,
                                           UNSIGNED8 *pucFldName );

UNSIGNED32 ENTRYPOINT AdsInTransaction(
                              ADSHANDLE        hConnect,
                              UNSIGNED16       *pbInTrans );

UNSIGNED32 ENTRYPOINT AdsIsEmpty(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pbEmpty );

UNSIGNED32 ENTRYPOINT AdsIsExprValid(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pbValid );

UNSIGNED32 ENTRYPOINT AdsIsFound(
                              ADSHANDLE        hObj,
                              UNSIGNED16       *pbFound );

UNSIGNED32 ENTRYPOINT AdsIsIndexCompound(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *pbCompound );

UNSIGNED32 ENTRYPOINT AdsIsIndexCustom(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *pbCustom );

UNSIGNED32 ENTRYPOINT AdsIsIndexDescending(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *pbDescending );

UNSIGNED32 ENTRYPOINT AdsIsIndexUnique(
                              ADSHANDLE        hIndex,
                              UNSIGNED16       *pbUnique );

UNSIGNED32 ENTRYPOINT AdsIsRecordDeleted(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pbDeleted );

UNSIGNED32 ENTRYPOINT AdsIsRecordEncrypted(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pbEncrypted );

UNSIGNED32 ENTRYPOINT AdsIsRecordLocked(
                              ADSHANDLE        hTable,
                              UNSIGNED32       ulRec,
                              UNSIGNED16       *pbLocked );

UNSIGNED32 ENTRYPOINT AdsIsRecordVisible(
                              ADSHANDLE        hObj,
                              UNSIGNED16       *pbVisible );

UNSIGNED32 ENTRYPOINT AdsIsServerLoaded(
                              UNSIGNED8        *pucServer,
                              UNSIGNED16       *pbLoaded );

UNSIGNED32 ENTRYPOINT AdsIsTableEncrypted(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pbEncrypted );

UNSIGNED32 ENTRYPOINT AdsIsTableLocked(
                              ADSHANDLE        hTable,
                              UNSIGNED16       *pbLocked );

UNSIGNED32 ENTRYPOINT AdsLocate(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       bForward,
                              UNSIGNED16       *pbFound );

UNSIGNED32 ENTRYPOINT AdsLockRecord(
                              ADSHANDLE        hTable,
                              UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsLockTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsLookupKey( ADSHANDLE  hIndex,
                                    UNSIGNED8 *pucKey,
                                    UNSIGNED16 usKeyLen,
                                    UNSIGNED16 usDataType,
                                    UNSIGNED16 *pbFound );

UNSIGNED32 ENTRYPOINT AdsMgConnect( UNSIGNED8   *pucServerName,
                                    UNSIGNED8   *pucUserName,
                                    UNSIGNED8   *pucPassword,
                                    ADSHANDLE   *phMgmtHandle );

UNSIGNED32 ENTRYPOINT AdsMgDisconnect( ADSHANDLE   hMgmtHandle );


UNSIGNED32 ENTRYPOINT AdsMgGetCommStats( ADSHANDLE           hMgmtHandle,
                                         ADS_MGMT_COMM_STATS *pstCommStats,
                                         UNSIGNED16          *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgResetCommStats( ADSHANDLE hMgmtHandle );

UNSIGNED32 ENTRYPOINT AdsMgDumpInternalTables( ADSHANDLE  hMgmtHandle );

UNSIGNED32 ENTRYPOINT AdsMgGetConfigInfo(
                           ADSHANDLE               hMgmtHandle,
                           ADS_MGMT_CONFIG_PARAMS  *pstConfigValues,
                           UNSIGNED16              *pusConfigValuesStructSize,
                           ADS_MGMT_CONFIG_MEMORY  *pstConfigMemory,
                           UNSIGNED16              *pusConfigMemoryStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetInstallInfo(
                                 ADSHANDLE               hMgmtHandle,
                                 ADS_MGMT_INSTALL_INFO   *pstInstallInfo,
                                 UNSIGNED16              *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetActivityInfo(
                                 ADSHANDLE               hMgmtHandle,
                                 ADS_MGMT_ACTIVITY_INFO  *pstActivityInfo,
                                 UNSIGNED16              *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetUserNames( ADSHANDLE           hMgmtHandle,
                                         UNSIGNED8           *pucFileName,
                                         ADS_MGMT_USER_INFO  astUserInfo[],
                                         UNSIGNED16          *pusArrayLen,
                                         UNSIGNED16          *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetOpenTables(
                                 ADSHANDLE           hMgmtHandle,
                                 UNSIGNED8           *pucUserName,
                                 UNSIGNED16          usConnNumber,
                                 ADS_MGMT_TABLE_INFO astOpenTableInfo[],
                                 UNSIGNED16          *pusArrayLen,
                                 UNSIGNED16          *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetOpenIndexes(
                                 ADSHANDLE           hMgmtHandle,
                                 UNSIGNED8           *pucTableName,
                                 UNSIGNED8           *pucUserName,
                                 UNSIGNED16          usConnNumber,
                                 ADS_MGMT_INDEX_INFO astOpenIndexInfo[],
                                 UNSIGNED16          *pusArrayLen,
                                 UNSIGNED16          *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetLocks( ADSHANDLE            hMgmtHandle,
                                     UNSIGNED8            *pucTableName,
                                     UNSIGNED8            *pucUserName,
                                     UNSIGNED16           usConnNumber,
                                     ADS_MGMT_RECORD_INFO astRecordInfo[],
                                     UNSIGNED16           *pusArrayLen,
                                     UNSIGNED16           *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetServerType( ADSHANDLE   hMgmtHandle,
                                          UNSIGNED16  *pusServerType );

UNSIGNED32 ENTRYPOINT AdsMgKillUser( ADSHANDLE  hMgmtHandle,
                                     UNSIGNED8  *pucUserName,
                                     UNSIGNED16 usConnNumber );

UNSIGNED32 ENTRYPOINT AdsMgGetWorkerThreadActivity(
                        ADSHANDLE                  hMgmtHandle,
                        ADS_MGMT_THREAD_ACTIVITY   astWorkerThreadActivity[],
                        UNSIGNED16                 *pusArrayLen,
                        UNSIGNED16                 *pusStructSize );

UNSIGNED32 ENTRYPOINT AdsMgGetLockOwner( ADSHANDLE          hMgmtHandle,
                                         UNSIGNED8          *pucTableName,
                                         UNSIGNED32         ulRecordNumber,
                                         ADS_MGMT_USER_INFO *pstUserInfo,
                                         UNSIGNED16         *pusStructSize,
                                         UNSIGNED16         *pusLockType );

UNSIGNED32 ENTRYPOINT AdsNullTerminateStrings( UNSIGNED16 bNullTerminate );

UNSIGNED32 ENTRYPOINT AdsOpenIndex(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucName,
                              ADSHANDLE        ahIndex[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsOpenTable(
                              ADSHANDLE        hConnect,
                              UNSIGNED8        *pucName,
                              UNSIGNED8        *pucAlias,
                              UNSIGNED16       usTableType,
                              UNSIGNED16       usCharType,
                              UNSIGNED16       usLockType,
                              UNSIGNED16       usCheckRights,
                              UNSIGNED32       ulOptions,
                              ADSHANDLE        *phTable );

UNSIGNED32 ENTRYPOINT AdsPackTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsRecallRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsRefreshRecord( ADSHANDLE hTable );


#if !( defined( ASANLM ) || defined( ASANT ) || defined( NLM ) || defined( ADS_NT ) || defined( ADS_WIN9X ) || defined( STAND_ALONE_EXE ) || ( defined( ADS_LINUX ) && !defined( ACE ) ) )
   UNSIGNED32 ENTRYPOINT AdsClearProgressCallback( void );

   UNSIGNED32 ENTRYPOINT AdsRegisterProgressCallback(
         UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent ) );

   UNSIGNED32 ENTRYPOINT AdsRegisterCallbackFunction(
         UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent, UNSIGNED32 ulCallbackID ),
         UNSIGNED32 ulCallbackID );

   UNSIGNED32 ENTRYPOINT AdsClearCallbackFunction( void );

#endif

UNSIGNED32 ENTRYPOINT AdsReindex( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsReindex61( ADSHANDLE  hTable,
                                    UNSIGNED32 ulPageSize );

UNSIGNED32 ENTRYPOINT AdsRollbackTransaction( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsSeek(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucKey,
                              UNSIGNED16       usKeyLen,
                              UNSIGNED16       usDataType,
                              UNSIGNED16       usSeekType,
                              UNSIGNED16       *pbFound );

UNSIGNED32 ENTRYPOINT AdsSeekLast(
                                  ADSHANDLE        hIndex,
                                  UNSIGNED8        *pucKey,
                                  UNSIGNED16       usKeyLen,
                                  UNSIGNED16       usDataType,
                                  UNSIGNED16       *pbFound );

UNSIGNED32 ENTRYPOINT AdsSetBinary(
                              ADSHANDLE        hTable,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       usBinaryType,
                              UNSIGNED32       ulTotalLength,
                              UNSIGNED32       ulOffset,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       ulLen );

UNSIGNED32 ENTRYPOINT AdsSetCollationLang( UNSIGNED8 *pucLang );

UNSIGNED32 ENTRYPOINT AdsSetDate(
                              ADSHANDLE        hObj,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucValue,
                              UNSIGNED16       usLen );

UNSIGNED32 ENTRYPOINT AdsSetDateFormat( UNSIGNED8 *pucFormat );

UNSIGNED32 ENTRYPOINT AdsSetDecimals( UNSIGNED16 usDecimals );

UNSIGNED32 ENTRYPOINT AdsSetDefault( UNSIGNED8 *pucDefault );

UNSIGNED32 ENTRYPOINT AdsShowDeleted( UNSIGNED16 bShowDeleted );

UNSIGNED32 ENTRYPOINT AdsSetDouble(
                              ADSHANDLE        hObj,
                              UNSIGNED8        *pucFldName,
                              DOUBLE           dValue );

UNSIGNED32 ENTRYPOINT AdsSetEmpty(
                              ADSHANDLE        hObj,
                              UNSIGNED8        *pucFldName );

UNSIGNED32 ENTRYPOINT AdsSetEpoch( UNSIGNED16 usCentury );

UNSIGNED32 ENTRYPOINT AdsSetExact( UNSIGNED16 bExact );

UNSIGNED32 ENTRYPOINT AdsSetExact22(
                              ADSHANDLE        hObj,
                              UNSIGNED16       bExact );

UNSIGNED32 ENTRYPOINT AdsSetField(
                                 ADSHANDLE        hObj,
                                 UNSIGNED8        *pucFldName,
                                 UNSIGNED8        *pucBuf,
                                 UNSIGNED32       ulLen );

UNSIGNED32 ENTRYPOINT AdsSetFilter(
                                  ADSHANDLE        hTable,
                                  UNSIGNED8        *pucFilter );

UNSIGNED32 ENTRYPOINT AdsSetHandleLong(
                                      ADSHANDLE        hObj,
                                      UNSIGNED32       ulVal );

UNSIGNED32 ENTRYPOINT AdsSetJulian(
                                  ADSHANDLE        hObj,
                                  UNSIGNED8        *pucFldName,
                                  SIGNED32         lDate );

UNSIGNED32 ENTRYPOINT AdsSetLogical(
                                   ADSHANDLE        hObj,
                                   UNSIGNED8        *pucFldName,
                                   UNSIGNED16       bValue );

UNSIGNED32 ENTRYPOINT AdsSetLong(
                                ADSHANDLE        hObj,
                                UNSIGNED8        *pucFldName,
                                SIGNED32         lValue );

UNSIGNED32 ENTRYPOINT AdsSetMilliseconds(
                                        ADSHANDLE        hObj,
                                        UNSIGNED8        *pucFldName,
                                        SIGNED32         lTime );

UNSIGNED32 ENTRYPOINT AdsSetRecord(
                                  ADSHANDLE        hObj,
                                  UNSIGNED8        *pucRec,
                                  UNSIGNED32       ulLen );

UNSIGNED32 ENTRYPOINT AdsSetRelation(
                                    ADSHANDLE        hTableParent,
                                    ADSHANDLE        hIndexChild,
                                    UNSIGNED8        *pucExpr );

UNSIGNED32 ENTRYPOINT AdsSetRelKeyPos(
                                     ADSHANDLE        hIndex,
                                     DOUBLE           dPos );

UNSIGNED32 ENTRYPOINT AdsSetScope(
                                 ADSHANDLE        hIndex,
                                 UNSIGNED16       usScopeOption,
                                 UNSIGNED8        *pucScope,
                                 UNSIGNED16       usScopeLen,
                                 UNSIGNED16       usDataType );

UNSIGNED32 ENTRYPOINT AdsSetScopedRelation(
                                          ADSHANDLE        hTableParent,
                                          ADSHANDLE        hIndexChild,
                                          UNSIGNED8        *pucExpr );

UNSIGNED32 ENTRYPOINT AdsSetSearchPath( UNSIGNED8 *pucPath );

UNSIGNED32 ENTRYPOINT AdsSetServerType( UNSIGNED16 usServerOptions );

UNSIGNED32 ENTRYPOINT AdsSetShort(
                                 ADSHANDLE        hObj,
                                 UNSIGNED8        *pucFldName,
                                 SIGNED16         sValue );

UNSIGNED32 ENTRYPOINT AdsSetString( ADSHANDLE        hObj,
                                    UNSIGNED8        *pucFldName,
                                    UNSIGNED8        *pucBuf,
                                    UNSIGNED32       ulLen );

UNSIGNED32 ENTRYPOINT AdsSetTime( ADSHANDLE        hObj,
                                  UNSIGNED8        *pucFldName,
                                  UNSIGNED8        *pucValue,
                                  UNSIGNED16       usLen );

UNSIGNED32 ENTRYPOINT AdsShowError( UNSIGNED8 *pucTitle );

UNSIGNED32 ENTRYPOINT AdsSkip( ADSHANDLE        hObj,
                               SIGNED32         lRecs );

UNSIGNED32 ENTRYPOINT AdsThreadExit( void );

UNSIGNED32 ENTRYPOINT AdsUnlockRecord( ADSHANDLE        hTable,
                                       UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsUnlockTable( ADSHANDLE hTable );

/* AdsVerifyPassword is obsolete; retained for backward compatibility.
 * Use AdsIsEncryptionEnabled instead.
 */
UNSIGNED32 ENTRYPOINT AdsVerifyPassword( ADSHANDLE       hTable,
                                         UNSIGNED16      *pusEnabled );

UNSIGNED32 ENTRYPOINT AdsIsEncryptionEnabled( ADSHANDLE  hTable,
                                              UNSIGNED16 *pusEnabled );

UNSIGNED32 ENTRYPOINT AdsWriteAllRecords( void );

UNSIGNED32 ENTRYPOINT AdsWriteRecord( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsZapTable( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsSetAOF( ADSHANDLE        hTable,
                                 UNSIGNED8        *pucFilter,
                                 UNSIGNED16       usResolve );

UNSIGNED32 ENTRYPOINT AdsEvalAOF( ADSHANDLE        hTable,
                                  UNSIGNED8        *pucFilter,
                                  UNSIGNED16       *pusOptLevel );

UNSIGNED32 ENTRYPOINT AdsClearAOF( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsRefreshAOF( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsGetAOF( ADSHANDLE        hTable,
                                 UNSIGNED8        *pucFilter,
                                 UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetAOFOptLevel( ADSHANDLE        hTable,
                                         UNSIGNED16       *pusOptLevel,
                                         UNSIGNED8        *pucNonOpt,
                                         UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsIsRecordInAOF( ADSHANDLE  hTable,
                                        UNSIGNED32 ulRecordNum,
                                        UNSIGNED16 *pusIsInAOF );

UNSIGNED32 ENTRYPOINT AdsCustomizeAOF( ADSHANDLE  hTable,
                                       UNSIGNED32 ulNumRecords,
                                       UNSIGNED32 *pulRecords,
                                       UNSIGNED16 usOption );

UNSIGNED32 ENTRYPOINT AdsInitRawKey( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsBuildRawKey( ADSHANDLE        hIndex,
                                      UNSIGNED8        *pucKey,
                                      UNSIGNED16       *pusKeyLen );

UNSIGNED32 ENTRYPOINT AdsCreateSQLStatement( ADSHANDLE hConnect,
                                             ADSHANDLE *phStatement );

UNSIGNED32 ENTRYPOINT AdsPrepareSQL( ADSHANDLE hStatement,
                                     UNSIGNED8 *pucSQL );

UNSIGNED32 ENTRYPOINT AdsExecuteSQL( ADSHANDLE hStatement,
                                     ADSHANDLE *phCursor );

UNSIGNED32 ENTRYPOINT AdsExecuteSQLDirect( ADSHANDLE hStatement,
                                           UNSIGNED8 *pucSQL,
                                           ADSHANDLE *phCursor );

UNSIGNED32 ENTRYPOINT AdsCloseSQLStatement( ADSHANDLE hStatement );

UNSIGNED32 ENTRYPOINT AdsStmtSetTableRights( ADSHANDLE  hStatement,
                                             UNSIGNED16 usCheckRights );

UNSIGNED32 ENTRYPOINT AdsStmtSetTableReadOnly( ADSHANDLE  hStatement,
                                               UNSIGNED16 usReadOnly );

UNSIGNED32 ENTRYPOINT AdsStmtSetTableLockType( ADSHANDLE  hStatement,
                                               UNSIGNED16 usLockType );

UNSIGNED32 ENTRYPOINT AdsStmtSetTableCharType( ADSHANDLE  hStatement,
                                               UNSIGNED16 usCharType );

UNSIGNED32 ENTRYPOINT AdsStmtSetTableType( ADSHANDLE  hStatement,
                                           UNSIGNED16 usTableType );

UNSIGNED32 ENTRYPOINT AdsStmtConstrainUpdates( ADSHANDLE  hStatement,
                                               UNSIGNED16 usConstrain );

UNSIGNED32 ENTRYPOINT AdsStmtEnableEncryption( ADSHANDLE  hStatement,
                                               UNSIGNED8  *pucPassword );

UNSIGNED32 ENTRYPOINT AdsStmtDisableEncryption( ADSHANDLE  hStatement );

UNSIGNED32 ENTRYPOINT AdsStmtSetTablePassword( ADSHANDLE hStatement,
                                               UNSIGNED8 *pucTableName,
                                               UNSIGNED8 *pucPassword );

UNSIGNED32 ENTRYPOINT AdsStmtClearTablePasswords( ADSHANDLE  hStatement );

UNSIGNED32 ENTRYPOINT AdsStmtReadAllColumns( ADSHANDLE  hStatement,
                                             UNSIGNED16 usReadColumns );

UNSIGNED32 ENTRYPOINT AdsClearSQLParams( ADSHANDLE  hStatement );

UNSIGNED32 ENTRYPOINT AdsSetTimeStamp( ADSHANDLE        hObj,
                                       UNSIGNED8        *pucFldName,
                                       UNSIGNED8        *pucBuf,
                                       UNSIGNED32       ulLen );


#if !( defined( ASANLM ) || defined( ASANT ) || defined( NLM ) || defined( ADS_NT ) || defined( ADS_WIN9X ) || defined( STAND_ALONE_EXE ) || ( defined( ADS_LINUX ) && !defined( ACE ) ) )
   UNSIGNED32 ENTRYPOINT AdsClearSQLAbortFunc( void );

   UNSIGNED32 ENTRYPOINT AdsRegisterSQLAbortFunc( UNSIGNED32 (WINAPI *lpfnCallback)(void) );

   UNSIGNED32 ENTRYPOINT AdsRegisterUDF( ADSHANDLE  hObj,
                                         UNSIGNED16 usType,
                                         UNSIGNED32 (WINAPI *lpfnUDF)(void) );
#endif

UNSIGNED32 ENTRYPOINT AdsGetNumParams( ADSHANDLE hStatement, UNSIGNED16 *pusNumParams );

UNSIGNED32 ENTRYPOINT AdsGetLastAutoinc( ADSHANDLE hObj, UNSIGNED32 *pulAutoIncVal );

UNSIGNED32 ENTRYPOINT AdsIsIndexUserDefined( ADSHANDLE hIndex,
                                             UNSIGNED16 *pbUserDefined );

UNSIGNED32 ENTRYPOINT AdsRestructureTable( ADSHANDLE    hObj,
                                           UNSIGNED8    *pucName,
                                           UNSIGNED8    *pucAlias,
                                           UNSIGNED16   usTableType,
                                           UNSIGNED16   usCharType,
                                           UNSIGNED16   usLockType,
                                           UNSIGNED16   usCheckRights,
                                           UNSIGNED8    *pucAddFields,
                                           UNSIGNED8    *pucDeleteFields,
                                           UNSIGNED8    *pucChangeFields );

UNSIGNED32 ENTRYPOINT AdsGetSQLStatementHandle( ADSHANDLE  hCursor,
                                                ADSHANDLE  *phStmt );

UNSIGNED32 ENTRYPOINT AdsGetSQLStatement( ADSHANDLE  hStmt,
                                          UNSIGNED8  *pucSQL,
                                          UNSIGNED16 *pusLen );

UNSIGNED32 ENTRYPOINT AdsFlushFileBuffers( ADSHANDLE hTable );

UNSIGNED32 ENTRYPOINT AdsDDDeployDatabase( UNSIGNED8 *pucDestination,
                                           UNSIGNED8 *pucDestinationPassword,
                                           UNSIGNED8 *pucSource,
                                           UNSIGNED8 *pucSourcePassword,
                                           UNSIGNED16 usServerTypes,
                                           UNSIGNED16 usValidateOption,
                                           UNSIGNED16 usBackupFiles,
                                           UNSIGNED32 ulOptions );

#ifdef __cplusplus
   }  /* extern "C" */
#endif


#if defined(ADS_LINUX) || defined(__GNUC__)
   #pragma pack()
#else
   #pragma pack( pop )
#endif

#endif  /* !__ACE_INCLUDED__ */
