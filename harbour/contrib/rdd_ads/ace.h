/*******************************************************************************
* Source File  : ace.h
* Copyright    : 1996-1999 Extended Systems, Inc.
* Description  : This is the main header file for the Advantage Client
*                Engine.  It contains the type definitions, constants,
*                and prototypes for the APIs
*******************************************************************************/

#ifndef __ACE_INCLUDED__
#define __ACE_INCLUDED__


/* This forces a warning for single equals in if statements */
#ifdef WIN32
   // 16-bit compiler doesn't seem to like this
   #pragma warning( error : 4706 )
#endif

/* data type definitions */
typedef unsigned long  UNSIGNED32;
typedef long           SIGNED32;
typedef unsigned short UNSIGNED16;
typedef short          SIGNED16;
typedef unsigned char  UNSIGNED8;
typedef char           SIGNED8;
typedef unsigned long  ADSHANDLE;
typedef double         DOUBLE;


#define VOID   void
#define EXTERN extern
#define STATIC static

#if defined( __RSXNT__ )
   #define ENTRYPOINT _export WINAPI
#elif defined( __GNUC__ )
   #define ENTRYPOINT __attribute__ (( dllexport ))
#elif defined( WIN32 ) && !defined( ASANT ) && !defined( __BORLANDC__ )
   #define ENTRYPOINT _declspec( dllexport ) WINAPI
#elif defined( ASANLM ) || defined( ASANT )
   #define ENTRYPOINT
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

/* options for opening tables - can be ORed together */
#define ADS_EXCLUSIVE            0x00000001
#define ADS_READONLY             0x00000002
#define ADS_SHARED               0x00000004
#define ADS_CLIPPER_MEMOS        0x00000008

/* Options for creating indexes - can be ORed together */
#define ADS_UNIQUE               0x00000001
#define ADS_COMPOUND             0x00000002
#define ADS_CUSTOM               0x00000004
#define ADS_DESCENDING           0x00000008

#define ADS_ASCENDING            0x00000000


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
#define ADS_CONNECTION           1
#define ADS_TABLE                2
#define ADS_INDEX_ORDER          3
#define ADS_STATEMENT            4
#define ADS_CURSOR               5


/* ACE Cursor ReadOnly settings */
#define ADS_CURSOR_READONLY      1
#define ADS_CURSOR_READWRITE     2

/* ACE Cursor Constrain settings */
#define ADS_CONSTRAIN            1
#define ADS_NO_CONSTRAIN         2

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
#define AE_FIELD_TYPE_MISMATCH          5122

#define AE_MAX_ERROR                    5122


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
#define ADS_NTX                  1
#define ADS_CDX                  2
#define ADS_ADT                  3

/* for retrieving file names of tables */
#define ADS_BASENAME             1
#define ADS_BASENAMEANDEXT       2
#define ADS_FULLPATHNAME         3


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


/* some maximum values used by the client */
/* NOTE:  constants meant for string length exclude space for null terminator */
#define ADS_MAX_DATEMASK         12
#define ADS_MAX_ERROR_LEN        255
#define ADS_MAX_INDEX_EXPR_LEN   510   /* this is only accurate for index expressions */
#define ADS_MAX_KEY_LENGTH       256   /* maximum key value length */
#define ADS_MAX_FIELD_NAME       128
#define ADS_MAX_DBF_FIELD_NAME   10    /* maximum length of field name in a DBF */
#define ADS_MAX_INDEXES          15    /* physical index files, NOT index orders */
#define ADS_MAX_PATH             260
#define ADS_MAX_TABLE_NAME       255   /* long file name */
#define ADS_MAX_TAG_NAME         128
#define ADS_MAX_TAGS             50    /* maximum for CDX/ADI file */


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
 * Constant for AdsMgGetConfigInfo
 */
#define ADS_MAX_CFG_PATH         256

/*
 * Constants for AdsMgGetServerType
 */
#define ADS_MGMT_NETWARE_SERVER  1
#define ADS_MGMT_NT_SERVER       2
#define ADS_MGMT_LOCAL_SERVER    3

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
   UNSIGNED16  usReserved5;               /* reserved                      */
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
 * This macro allows a numeric field value to be passed into functions
 * that expect field names.  If the user prefers to use column number,
 * then calls like this can be made:
 * ulRet = AdsGetDouble( hTbl, ADSFIELD( 1 ), &dVal );
 * Where the first column is a numeric value to retrieve.
 */
#define ADSFIELD(x)   ((unsigned char*)(long)( x ))


#ifdef __cplusplus
   extern "C"
   {
#endif


UNSIGNED32 ENTRYPOINT AdsAddCustomKey( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsAppendRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsApplicationExit( void );

UNSIGNED32 ENTRYPOINT AdsAtBOF( ADSHANDLE    hTbl,
                                UNSIGNED16   *pbBof );

UNSIGNED32 ENTRYPOINT AdsAtEOF( ADSHANDLE    hTbl,
                                UNSIGNED16   *pbEof );

UNSIGNED32 ENTRYPOINT AdsBeginTransaction( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsBinaryToFile( ADSHANDLE   hTbl,
                                       UNSIGNED8   *pucFldName,
                                       UNSIGNED8   *pucFileName );

UNSIGNED32 ENTRYPOINT AdsCacheOpenCursors( UNSIGNED16 usOpen );

UNSIGNED32 ENTRYPOINT AdsCacheOpenTables( UNSIGNED16 usOpen );

UNSIGNED32 ENTRYPOINT AdsCacheRecords( ADSHANDLE hTbl,
                                       UNSIGNED16 usNumRecords );

UNSIGNED32 ENTRYPOINT AdsCancelUpdate( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsCheckExistence( ADSHANDLE    hConnect,
                                         UNSIGNED8    *pucFileName,
                                         UNSIGNED16   *pusOnDisk );

UNSIGNED32 ENTRYPOINT AdsClearAllScopes( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsClearDefault( void );

UNSIGNED32 ENTRYPOINT AdsClearFilter( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsClearRelation( ADSHANDLE hTblParent );

UNSIGNED32 ENTRYPOINT AdsClearScope( ADSHANDLE  hIndex,
                                     UNSIGNED16 usScopeOption );

UNSIGNED32 ENTRYPOINT AdsCloneTable( ADSHANDLE  hTbl,
                                     ADSHANDLE  *phClone );

UNSIGNED32 ENTRYPOINT AdsCloseAllIndexes( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsCloseAllTables( void );

UNSIGNED32 ENTRYPOINT AdsCloseIndex( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsCloseTable( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsCommitTransaction( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsConnect( UNSIGNED8  *pucServerName,
                                  ADSHANDLE  *phConnect );

UNSIGNED32 ENTRYPOINT AdsContinue( ADSHANDLE    hTbl,
                                   UNSIGNED16   *pbFound );

UNSIGNED32 ENTRYPOINT AdsConvertTable( ADSHANDLE   hTbl,
                                       UNSIGNED16  usFilterOption,
                                       UNSIGNED8   *pucFile,
                                       UNSIGNED16  usTableType );

UNSIGNED32 ENTRYPOINT AdsCopyTable( ADSHANDLE   hTbl,
                                    UNSIGNED16  usFilterOption,
                                    UNSIGNED8   *pucFile );

UNSIGNED32 ENTRYPOINT AdsCopyTableContents( ADSHANDLE    hTblFrom,
                                            ADSHANDLE    hTblTo,
                                            UNSIGNED16   usFilterOption );

UNSIGNED32 ENTRYPOINT AdsCopyTableStructure( ADSHANDLE   hTbl,
                                             UNSIGNED8   *pucFile );

UNSIGNED32 ENTRYPOINT AdsCreateIndex( ADSHANDLE    hObj,
                                      UNSIGNED8    *pucFileName,
                                      UNSIGNED8    *pucTag,
                                      UNSIGNED8    *pucExpr,
                                      UNSIGNED8    *pucCondition,
                                      UNSIGNED8    *pucWhile,
                                      UNSIGNED32   ulOptions,
                                      ADSHANDLE    *phIndex );

UNSIGNED32 ENTRYPOINT AdsCreateTable( ADSHANDLE    hConnect,
                                      UNSIGNED8    *pucName,
                                      UNSIGNED8    *pucAlias,
                                      UNSIGNED16   usTableType,
                                      UNSIGNED16   usCharType,
                                      UNSIGNED16   usLockType,
                                      UNSIGNED16   usCheckRights,
                                      UNSIGNED16   usMemoSize,
                                      UNSIGNED8    *pucFields,
                                      ADSHANDLE    *phTbl );

UNSIGNED32 ENTRYPOINT AdsDecryptRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsDecryptTable( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsDeleteCustomKey( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsDeleteIndex( ADSHANDLE hIndex );

UNSIGNED32 ENTRYPOINT AdsDeleteRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsDisableEncryption( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsDisconnect( ADSHANDLE hConnect );

UNSIGNED32 ENTRYPOINT AdsEnableEncryption( ADSHANDLE hTbl,
                                           UNSIGNED8 *pucPassword );

UNSIGNED32 ENTRYPOINT AdsEncryptRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsEncryptTable( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsEvalLogicalExpr(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pbResult );

UNSIGNED32 ENTRYPOINT AdsEvalNumericExpr(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucExpr,
                              DOUBLE           *pdResult );

UNSIGNED32 ENTRYPOINT AdsEvalStringExpr(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED8        *pucResult,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsEvalTestExpr(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsExtractKey(
                              ADSHANDLE        hIndex,
                              UNSIGNED8        *pucKey,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsFailedTransactionRecovery( UNSIGNED8 *pucServer );

UNSIGNED32 ENTRYPOINT AdsFileToBinary(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       usBinaryType,
                              UNSIGNED8        *pucFileName );

UNSIGNED32 ENTRYPOINT AdsFindConnection(
                              UNSIGNED8        *pucServerName,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsFindConnection25(
                              UNSIGNED8        *pucFullPath,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsGetAllIndexes(
                              ADSHANDLE        hTbl,
                              ADSHANDLE        ahIndex[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetAllLocks(
                              ADSHANDLE        hTbl,
                              UNSIGNED32       aulLocks[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetAllTables(
                              ADSHANDLE        ahTbl[],
                              UNSIGNED16       *pusArrayLen );

UNSIGNED32 ENTRYPOINT AdsGetBinary(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       ulOffset,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen );

UNSIGNED32 ENTRYPOINT AdsGetBinaryLength(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetBookmark(
                              ADSHANDLE        hTbl,
                              ADSHANDLE        *phBookmark );

UNSIGNED32 ENTRYPOINT AdsGetCollationLang(
                              UNSIGNED8  *pucLang,
                              UNSIGNED16 *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetConnectionType(
                              ADSHANDLE        hConnect,
                              UNSIGNED16       *pusConnectType );

UNSIGNED32 ENTRYPOINT AdsGetDate(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen,
                              UNSIGNED16       usOption );

UNSIGNED32 ENTRYPOINT AdsGetFieldDecimals(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusDecimals );

UNSIGNED32 ENTRYPOINT AdsGetFieldLength(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetFieldName(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       usFld,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusBufLen );

UNSIGNED32 ENTRYPOINT AdsGetFieldNum(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetFieldOffset(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulOffset );

UNSIGNED32 ENTRYPOINT AdsGetFieldType(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetFilter(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucIndexOrder,
                              ADSHANDLE        *phIndex );

UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByOrder(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       usOrderNum,
                              ADSHANDLE        *phIndex );


UNSIGNED32 ENTRYPOINT AdsGetIndexHandleByExpr(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucDate,
                              UNSIGNED16       *pusDateLen );

UNSIGNED32 ENTRYPOINT AdsGetLogical(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pbValue );

UNSIGNED32 ENTRYPOINT AdsGetLong(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              SIGNED32         *plValue );

UNSIGNED32 ENTRYPOINT AdsGetMemoLength(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED32       *pulLength );

UNSIGNED32 ENTRYPOINT AdsGetMemoDataType(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetMilliseconds(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              SIGNED32         *plTime );

UNSIGNED32 ENTRYPOINT AdsGetNumFields(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusCount );

UNSIGNED32 ENTRYPOINT AdsGetNumIndexes(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetNumLocks(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetNumOpenTables( UNSIGNED16 *pusNum );

UNSIGNED32 ENTRYPOINT AdsGetRecord(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucRec,
                              UNSIGNED32       *pulLen );

UNSIGNED32 ENTRYPOINT AdsGetRecordCount(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulCount );

UNSIGNED32 ENTRYPOINT AdsGetRecordNum(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       usFilterOption,
                              UNSIGNED32       *pulRec );

UNSIGNED32 ENTRYPOINT AdsGetRecordLength(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              SIGNED16         *psValue );

UNSIGNED32 ENTRYPOINT AdsGetString(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED8        *pucBuf,
                              UNSIGNED32       *pulLen,
                              UNSIGNED16       usOption );

UNSIGNED32 ENTRYPOINT AdsGetTableAlias(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucAlias,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetTableCharType(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusCharType );

UNSIGNED32 ENTRYPOINT AdsGetTableConnection(
                              ADSHANDLE        hTbl,
                              ADSHANDLE        *phConnect );

UNSIGNED32 ENTRYPOINT AdsGetTableFilename(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       usOption,
                              UNSIGNED8        *pucName,
                              UNSIGNED16       *pusLen );

UNSIGNED32 ENTRYPOINT AdsGetTableHandle(
                              UNSIGNED8        *pucName,
                              ADSHANDLE        *phTbl );

UNSIGNED32 ENTRYPOINT AdsGetTableHandle25(
                              ADSHANDLE   hConnect,
                              UNSIGNED8   *pucName,
                              ADSHANDLE   *phTbl );

UNSIGNED32 ENTRYPOINT AdsGetTableLockType(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusLockType );

UNSIGNED32 ENTRYPOINT AdsGetTableMemoSize(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusMemoSize );

UNSIGNED32 ENTRYPOINT AdsGetTableOpenOptions(
                              ADSHANDLE        hTbl,
                              UNSIGNED32       *pulOptions );

UNSIGNED32 ENTRYPOINT AdsGetTableRights(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusRights );

UNSIGNED32 ENTRYPOINT AdsGetTableType(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pusType );

UNSIGNED32 ENTRYPOINT AdsGetTime(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              ADSHANDLE        hBookmark );

UNSIGNED32 ENTRYPOINT AdsGotoBottom( ADSHANDLE hObj );

UNSIGNED32 ENTRYPOINT AdsGotoRecord(
                              ADSHANDLE        hTbl,
                              UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsGotoTop( ADSHANDLE hObj );

UNSIGNED32 ENTRYPOINT AdsImageToClipboard( ADSHANDLE hTbl,
                                           UNSIGNED8 *pucFldName );

UNSIGNED32 ENTRYPOINT AdsInTransaction(
                              ADSHANDLE        hConnect,
                              UNSIGNED16       *pbInTrans );

UNSIGNED32 ENTRYPOINT AdsIsEmpty(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucFldName,
                              UNSIGNED16       *pbEmpty );

UNSIGNED32 ENTRYPOINT AdsIsExprValid(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pbDeleted );

UNSIGNED32 ENTRYPOINT AdsIsRecordEncrypted(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pbEncrypted );

UNSIGNED32 ENTRYPOINT AdsIsRecordLocked(
                              ADSHANDLE        hTbl,
                              UNSIGNED32       ulRec,
                              UNSIGNED16       *pbLocked );

UNSIGNED32 ENTRYPOINT AdsIsRecordVisible(
                              ADSHANDLE        hObj,
                              UNSIGNED16       *pbVisible );

UNSIGNED32 ENTRYPOINT AdsIsServerLoaded(
                              UNSIGNED8        *pucServer,
                              UNSIGNED16       *pbLoaded );

UNSIGNED32 ENTRYPOINT AdsIsTableEncrypted(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pbEncrypted );

UNSIGNED32 ENTRYPOINT AdsIsTableLocked(
                              ADSHANDLE        hTbl,
                              UNSIGNED16       *pbLocked );

UNSIGNED32 ENTRYPOINT AdsLocate(
                              ADSHANDLE        hTbl,
                              UNSIGNED8        *pucExpr,
                              UNSIGNED16       bForward,
                              UNSIGNED16       *pbFound );

UNSIGNED32 ENTRYPOINT AdsLockRecord(
                              ADSHANDLE        hTbl,
                              UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsLockTable( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsLookupKey( ADSHANDLE  hIndex,
                                    UNSIGNED8 *pucKey,
                                    UNSIGNED16 usKeyLen,
                                    UNSIGNED16 usDataType,
                                    UNSIGNED16 *pbFound );

/* Begin Management APIs */

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

/* End Management APIs */

UNSIGNED32 ENTRYPOINT AdsNullTerminateStrings( UNSIGNED16 bNullTerminate );

UNSIGNED32 ENTRYPOINT AdsOpenIndex(
                              ADSHANDLE        hTbl,
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
                              ADSHANDLE        *phTbl );

UNSIGNED32 ENTRYPOINT AdsPackTable( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsRecallRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsRefreshRecord( ADSHANDLE hTbl );

#if !( defined( ASANLM ) || defined( ASANT ) )
   UNSIGNED32 ENTRYPOINT AdsClearProgressCallback( void );

   UNSIGNED32 ENTRYPOINT AdsRegisterProgressCallback(
         UNSIGNED32 (WINAPI *lpfnCallback)( UNSIGNED16 usPercent ) );
#endif

UNSIGNED32 ENTRYPOINT AdsReindex( ADSHANDLE hTbl );

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
                              ADSHANDLE        hTbl,
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
                                  ADSHANDLE        hTbl,
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
                                    ADSHANDLE        hTblParent,
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
                                          ADSHANDLE        hTblParent,
                                          ADSHANDLE        hIndexChild,
                                          UNSIGNED8        *pucExpr );

UNSIGNED32 ENTRYPOINT AdsSetSearchPath( UNSIGNED8 *pucPath );

UNSIGNED32 ENTRYPOINT AdsSetServerType( UNSIGNED16 usServerOptions );

UNSIGNED32 ENTRYPOINT AdsSetShort(
                                 ADSHANDLE        hObj,
                                 UNSIGNED8        *pucFldName,
                                 SIGNED16         lValue );

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

UNSIGNED32 ENTRYPOINT AdsUnlockRecord( ADSHANDLE        hTbl,
                                       UNSIGNED32       ulRec );

UNSIGNED32 ENTRYPOINT AdsUnlockTable( ADSHANDLE hTbl );

/* AdsVerifyPassword is obsolete, retained for backward compatibility.
 * Use AdsIsEncryptionEnabled instead.
 */
UNSIGNED32 ENTRYPOINT AdsVerifyPassword( ADSHANDLE       hTbl,
                                         UNSIGNED16      *pusEnabled );

UNSIGNED32 ENTRYPOINT AdsIsEncryptionEnabled( ADSHANDLE  hTbl,
                                             UNSIGNED16  *pusEnabled );

UNSIGNED32 ENTRYPOINT AdsWriteAllRecords( void );

UNSIGNED32 ENTRYPOINT AdsWriteRecord( ADSHANDLE hTbl );

UNSIGNED32 ENTRYPOINT AdsZapTable( ADSHANDLE hTbl );

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

UNSIGNED32 ENTRYPOINT AdsSetTimeStamp( ADSHANDLE        hObj,
                                       UNSIGNED8        *pucFldName,
                                       UNSIGNED8        *pucBuf,
                                       UNSIGNED32       ulLen );

UNSIGNED32 ENTRYPOINT AdsClearSQLParams( ADSHANDLE  hStatement );

#if !( defined( ASANLM ) || defined( ASANT ) )
   UNSIGNED32 ENTRYPOINT AdsClearSQLAbortFunc( void );

   UNSIGNED32 ENTRYPOINT AdsRegisterSQLAbortFunc( UNSIGNED32 (WINAPI *lpfnCallback)() );
#endif


#ifdef __cplusplus
   }  /* extern "C" */
#endif

#endif  /* __ACE_INCLUDED__ */
