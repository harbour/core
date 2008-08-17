/*
 * $Id$
 */

/*********************************************************************
** sqltypes.ch - This file defines the types used in ODBC
*********************************************************************/

#ifndef __SQLTYPES

pragma pack(8)
#define __SQLTYPES

/* if ODBCVER is not defined, assume version 3.51 */
#ifndef ODBCVER
#define ODBCVER 0x0351
#endif  /* ODBCVER */


#define SCHAR          char
#define SQLSCHAR       SCHAR
#define SDWORD         DWORD
#define SWORD          WORD
#define UDWORD         ULONG
#define UWORD          USHORT

#define SLONG          LONG
#define SSHORT         SHORT
#define SDOUBLE        double
#define LDOUBLE        double
#define SFLOAT         float
#define RETCODE        SHORT

/* environment specific definitions */

#ifndef RC_INVOKED

/* API declaration data types */

#define SQLCHAR         UCHAR

//#if (ODBCVER >= 0x0300)
  #define SQLSCHAR       signed char
  #define SQLDATE        unsigned char
  #define SQLDECIMAL     unsigned char
  #define SQLDOUBLE      double
  #define SQLFLOAT       double
//#endif

#define SQLINTEGER       LONG
#define SQLUINTEGER      ULONG

#define SQLROWSETSIZE  SQLUINTEGER

#define SQLLEN SQLINTEGER
#define SQLROWOFFSET    SQLINTEGER
#define SQLROWCOUNT     SQLUINTEGER
#define SQLULEN         SQLUINTEGER
#define SQLTRANSID      DWORD
#define SQLSETPOSIROW   SQLUSMALLINT

//#if (ODBCVER >= 0x0300)
#define SQLNUMERIC      UCHAR
//#endif


#define PTR        LPVOID
#define HENV       LPVOID
#define HDBC       LPVOID
#define HSTMT      LPVOID




#define SQLPOINTER  PTR

//#if (ODBCVER >= 0x0300)
#define SQLREAL         float
//#endif
#define SQLSMALLINT     SHORT
#define SQLUSMALLINT    USHORT
//#if (ODBCVER >= 0x0300)
#define SQLTIME         UCHAR
#define SQLTIMESTAMP    UCHAR
#define SQLVARCHAR      UCHAR
//#endif

/* function return type */
#define SQLRETURN    SQLSMALLINT

/* generic data structures */

//#if (ODBCVER >= 0x0300)
  #define SQLHANDLE    PTR
  #define    SQLHENV         SQLHANDLE
  #define    SQLHDBC         SQLHANDLE
  #define    SQLHSTMT        SQLHANDLE
  #define    SQLHDESC        SQLHANDLE
//#else
//  #define  SQLHENV          void*
//  #define  SQLHDBC          void*
//  #define  SQLHSTMT         void*
//#endif /* ODBCVER >= 0x0300 */

/* SQL portable types for C */


#define SQLHWND    HWND


#ifndef __SQLDATE
#define __SQLDATE

/* transfer types for DATE, TIME, TIMESTAMP */

typedef struct tagDATE_STRUCT;
{;
  SQLSMALLINT    year ;
  SQLUSMALLINT   month;
  SQLUSMALLINT   day  ;
} DATE_STRUCT

//#if (ODBCVER >= 0x0300)
  #define SQL_DATE_STRUCT   DATE_STRUCT
//#endif  /* ODBCVER >= 0x0300 */

typedef struct tagTIME_STRUCT;
{;
        SQLUSMALLINT   hour  ;
        SQLUSMALLINT   minute;
        SQLUSMALLINT   second;
} TIME_STRUCT

//#if (ODBCVER >= 0x0300)
#define SQL_TIME_STRUCT  TIME_STRUCT
//#endif /* ODBCVER >= 0x0300 */

typedef struct tagTIMESTAMP_STRUCT;
{;
        SQLSMALLINT    year;
        SQLUSMALLINT   month;
        SQLUSMALLINT   day;
        SQLUSMALLINT   hour;
        SQLUSMALLINT   minute;
        SQLUSMALLINT   second;
        SQLUINTEGER    fraction;
} TIMESTAMP_STRUCT

//#if (ODBCVER >= 0x0300)
#define SQL_TIMESTAMP_STRUCT   TIMESTAMP_STRUCT
//#endif  /* ODBCVER >= 0x0300 */

/*
 * enumerations for DATETIME_INTERVAL_SUBCODE values for interval data types
 * these values are from SQL-92
 */

//#if (ODBCVER >= 0x0300)

#define         SQL_IS_YEAR                     = 1
#define         SQL_IS_MONTH                    = 2
#define         SQL_IS_DAY                      = 3
#define         SQL_IS_HOUR                     = 4
#define         SQL_IS_MINUTE                   = 5
#define         SQL_IS_SECOND                   = 6
#define         SQL_IS_YEAR_TO_MONTH            = 7
#define         SQL_IS_DAY_TO_HOUR              = 8
#define         SQL_IS_DAY_TO_MINUTE            = 9
#define         SQL_IS_DAY_TO_SECOND            = 10
#define         SQL_IS_HOUR_TO_MINUTE           = 11
#define         SQL_IS_HOUR_TO_SECOND           = 12
#define         SQL_IS_MINUTE_TO_SECOND         = 13

//#endif  /* ODBCVER >= 0x0300 */

//#if (ODBCVER >= 0x0300)
typedef struct tagSQL_YEAR_MONTH ;
{;
                SQLUINTEGER             year;
                SQLUINTEGER             month;
} SQL_YEAR_MONTH_STRUCT

typedef struct tagSQL_DAY_SECOND;
{;
                SQLUINTEGER             day;
                SQLUINTEGER             hour;
                SQLUINTEGER             minute;
                SQLUINTEGER             second;
                SQLUINTEGER             fraction;
} SQL_DAY_SECOND_STRUCT

typedef struct tagSQL_INTERVAL_STRUCT ;
{;
        SQLINTERVAL             interval_type;
        SQLSMALLINT             interval_sign;
        SQL_YEAR_MONTH_STRUCT   intval       ;
} SQL_INTERVAL_STRUCT

//#endif  /* ODBCVER >= 0x0300 */

//#endif        /* __SQLDATE    */

/* the ODBC C types for SQL_C_SBIGINT and SQL_C_UBIGINT */
//#if (ODBCVER >= 0x0300)
//#if (_MSC_VER >= 900)
#define ODBCINT64       __int64
//#endif

/* If using other compilers, define ODBCINT64 to the 
        approriate 64 bit integer type */
//#ifdef ODBCINT64
#define SQLBIGINT  ODBCINT64 
#define SQLUBIGINT unsigned ODBCINT64 
//#endif
//#endif  /* ODBCVER >= 0x0300 */

/* internal representation of numeric data type */
//#if (ODBCVER >= 0x0300)
#define SQL_MAX_NUMERIC_LEN             16
typedef struct tagSQL_NUMERIC_STRUCT;
{;
        SQLCHAR         precision;
        SQLSCHAR        scale;
        SQLCHAR         sign;   /* 1 if positive, 0 if negative */
        SQLCHAR         val[SQL_MAX_NUMERIC_LEN];
} SQL_NUMERIC_STRUCT
//#endif  /* ODBCVER >= 0x0300 */

//#if (ODBCVER >= 0x0350)
#ifdef GUID_DEFINED
#define SQLGUID GUID    
#else
/* size is 16 */
typedef struct  tagSQLGUID;
{;
    DWORD Data1;
    WORD Data2;
    WORD Data3;
    BYTE Data4[ 8 ];
} SQLGUID
#endif  /* GUID_DEFINED */
//#endif  /* ODBCVER >= 0x0350 */

#define BOOKMARK  unsigned long int

#ifdef _WCHAR_T_DEFINED
#define SQLWCHAR wchar_t
#else
#define SQLWCHAR unsigned short
#endif

#define  SQLTCHAR  SQLCHAR
        

#endif     /* RC_INVOKED */


#endif /* #ifndef __SQLTYPES */
