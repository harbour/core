/*
 * Harbour Project source code:
 *    USRRDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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


/* Movement and positioning methods */
#define UR_BOF                                  1
#define UR_EOF                                  2
#define UR_FOUND                                3
#define UR_GOBOTTOM                             4
#define UR_GOTO                                 5
#define UR_GOTOID                               6
#define UR_GOTOP                                7
#define UR_SEEK                                 8
#define UR_SKIP                                 9
#define UR_SKIPFILTER                          10
#define UR_SKIPRAW                             11

/* Data management */
#define UR_ADDFIELD                            12
#define UR_APPEND                              13
#define UR_CREATEFIELDS                        14
#define UR_DELETE                              15
#define UR_DELETED                             16
#define UR_FIELDCOUNT                          17
#define UR_FIELDDISPLAY                        18
#define UR_FIELDINFO                           19
#define UR_FIELDNAME                           20
#define UR_FLUSH                               21
#define UR_GETREC                              22
#define UR_GETVALUE                            23
#define UR_GETVARLEN                           24
#define UR_GOCOLD                              25
#define UR_GOHOT                               26
#define UR_PUTREC                              27
#define UR_PUTVALUE                            28
#define UR_RECALL                              29
#define UR_RECCOUNT                            30
#define UR_RECINFO                             31
#define UR_RECNO                               32
#define UR_RECID                               33
#define UR_SETFIELDEXTENT                      34

/* WorkArea/Database management */
#define UR_ALIAS                               35
#define UR_CLOSE                               36
#define UR_CREATE                              37
#define UR_INFO                                38
#define UR_NEW                                 39
#define UR_OPEN                                40
#define UR_RELEASE                             41
#define UR_STRUCTSIZE                          42
#define UR_SYSNAME                             43
#define UR_DBEVAL                              44
#define UR_PACK                                45
#define UR_PACKREC                             46
#define UR_SORT                                47
#define UR_TRANS                               48
#define UR_TRANSREC                            49
#define UR_ZAP                                 50

/* Relational Methods */
#define UR_CHILDEND                            51
#define UR_CHILDSTART                          52
#define UR_CHILDSYNC                           53
#define UR_SYNCCHILDREN                        54
#define UR_CLEARREL                            55
#define UR_FORCEREL                            56
#define UR_RELAREA                             57
#define UR_RELEVAL                             58
#define UR_RELTEXT                             59
#define UR_SETREL                              60

/* Order Management */
#define UR_ORDLSTADD                           61
#define UR_ORDLSTCLEAR                         62
#define UR_ORDLSTDELETE                        63
#define UR_ORDLSTFOCUS                         64
#define UR_ORDLSTREBUILD                       65
#define UR_ORDSETCOND                          66
#define UR_ORDCREATE                           67
#define UR_ORDDESTROY                          68
#define UR_ORDINFO                             69

/* Filters and Scope Settings */
#define UR_CLEARFILTER                         70
#define UR_CLEARLOCATE                         71
#define UR_CLEARSCOPE                          72
#define UR_COUNTSCOPE                          73
#define UR_FILTERTEXT                          74
#define UR_SCOPEINFO                           75
#define UR_SETFILTER                           76
#define UR_SETLOCATE                           77
#define UR_SETSCOPE                            78
#define UR_SKIPSCOPE                           79
#define UR_LOCATE                              80

/* Miscellaneous */
#define UR_COMPILE                             81
#define UR_ERROR                               82
#define UR_EVALBLOCK                           83

/* Network operations */
#define UR_RAWLOCK                             84
#define UR_LOCK                                85
#define UR_UNLOCK                              86

/* Memofile functions */
#define UR_CLOSEMEMFILE                        87
#define UR_CREATEMEMFILE                       88
#define UR_GETVALUEFILE                        89
#define UR_OPENMEMFILE                         90
#define UR_PUTVALUEFILE                        91

/* Database file header handling */
#define UR_READDBHEADER                        92
#define UR_WRITEDBHEADER                       93

/* non WorkArea functions       */
#define UR_INIT                                94
#define UR_EXIT                                95
#define UR_DROP                                96
#define UR_EXISTS                              97
#define UR_RENAME                              98
#define UR_RDDINFO                             99

/* Special and reserved methods */
#define UR_WHOCARES                           100

#define UR_METHODCOUNT                        100


/* FIELD types */
#ifndef HB_FT_NONE
#define HB_FT_NONE            0
#define HB_FT_STRING          1     /* "C" */
#define HB_FT_LOGICAL         2     /* "L" */
#define HB_FT_DATE            3     /* "D" */
#define HB_FT_LONG            4     /* "N" */
#define HB_FT_FLOAT           5     /* "F" */
#define HB_FT_INTEGER         6     /* "I" */
#define HB_FT_DOUBLE          7     /* "B" */
#define HB_FT_TIME            8     /* "T" */
#define HB_FT_TIMESTAMP       9     /* "@" */
#define HB_FT_MODTIME         10    /* "=" */
#define HB_FT_ROWVER          11    /* "^" */
#define HB_FT_AUTOINC         12    /* "+" */
#define HB_FT_CURRENCY        13    /* "Y" */
#define HB_FT_CURDOUBLE       14    /* "Z" */
#define HB_FT_VARLENGTH       15    /* "Q" */
#define HB_FT_MEMO            16    /* "M" */
#define HB_FT_ANY             17    /* "V" */
#define HB_FT_IMAGE           18    /* "P" */
#define HB_FT_BLOB            19    /* "W" */
#define HB_FT_OLE             20    /* "G" */
#endif

/* Flags for DBTRANSINFO */
#define DBTF_MATCH            0x0001
#define DBTF_PUTREC           0x0002

/* Codes for Locking methods */
#define DBLM_EXCLUSIVE        1
#define DBLM_MULTIPLE         2
#define DBLM_FILE             3

/* Codes for RawLock types */
#define FILE_LOCK             1
#define FILE_UNLOCK           2
#define REC_LOCK              3
#define REC_UNLOCK            4
#define HEADER_LOCK           5
#define HEADER_UNLOCK         6
#define APPEND_LOCK           7
#define APPEND_UNLOCK         8


/* DBOPENINFO */
#define UR_OI_AREA            1
#define UR_OI_NAME            2
#define UR_OI_ALIAS           3
#define UR_OI_SHARED          4
#define UR_OI_READONLY        5
#define UR_OI_CDPID           6
#define UR_OI_CONNECT         7
#define UR_OI_HEADER          8
#define UR_OI_SIZE            8

/* DBFIELDINFO */
#define UR_FI_NAME            1
#define UR_FI_TYPE            2
#define UR_FI_TYPEEXT         3
#define UR_FI_LEN             4
#define UR_FI_DEC             5
#define UR_FI_SIZE            5

/* DBLOCKINFO */
#define UR_LI_RECORD          1
#define UR_LI_METHOD          2
#define UR_LI_RESULT          3
#define UR_LI_SIZE            3

/* DBFILTERINFO */
#define UR_FRI_BEXPR          1
#define UR_FRI_CEXPR          2
#define UR_FRI_ACTIVE         3
#define UR_FRI_OPTIMIZED      4
#define UR_FRI_CARGO          5
#define UR_FRI_SIZE           5

/* DBRELINFO */
#define UR_RI_BEXPR           1
#define UR_RI_CEXPR           2
#define UR_RI_SCOPED          3
#define UR_RI_OPTIMIZED       4
#define UR_RI_PARENT          5
#define UR_RI_CHILD           6
#define UR_RI_NEXT            7
#define UR_RI_SIZE            7

/* DBSCOPEINFO */
#define UR_SI_BFOR            1
#define UR_SI_CFOR            2
#define UR_SI_BWHILE          3
#define UR_SI_CWHILE          4
#define UR_SI_NEXT            5
#define UR_SI_RECORD          6
#define UR_SI_REST            7
#define UR_SI_IGNOREFILTER    8
#define UR_SI_INCLUDEDELETED  9
#define UR_SI_LAST            10
#define UR_SI_IGNOREDUPS      11
#define UR_SI_BACKWARD        12
#define UR_SI_OPTIMIZED       13
#define UR_SI_SIZE            13

/* DBEVALINFO */
#define UR_EI_BLOCK           1
#define UR_EI_CEXPR           2
#define UR_EI_SCOPE           3
#define UR_EI_SIZE            3

/* DBTRANSINFO */
#define UR_TI_SRCAREA         1
#define UR_TI_DSTAREA         2
#define UR_TI_SCOPE           3
#define UR_TI_FLAGS           4
#define UR_TI_ITEMCOUNT       5
#define UR_TI_ITEMS           6
#define UR_TI_SIZE            6
/* DBTRANSITEM */
#define UR_TITEM_SOURCE       1
#define UR_TITEM_DESTIN       2
#define UR_TITEM_SIZE         2

/* DBSORTINFO */
#define UR_SRI_TRANSINFO      1
#define UR_SRI_ITEMS          2
#define UR_SRI_ITEMCOUNT      3
#define UR_SRI_SIZE           3
/* DBSORTITEM */
#define UR_SITEM_FIELD        1
#define UR_SITEM_FLAGS        2
#define UR_SITEM_SIZE         2

/* DBORDERINFO */
#define UR_ORI_BAG            1
#define UR_ORI_TAG            2
#define UR_ORI_BLOCK          3
#define UR_ORI_RESULT         4
#define UR_ORI_NEWVAL         5
#define UR_ORI_ALLTAGS        6
#define UR_ORI_SIZE           6

/* DBORDERCONDINFO */
#define UR_ORC_ACTIVE         1
#define UR_ORC_CFOR           2
#define UR_ORC_CWHILE         3
#define UR_ORC_BFOR           4
#define UR_ORC_BWHILE         5
#define UR_ORC_BEVAL          6
#define UR_ORC_STEP           7
#define UR_ORC_STARTREC       8
#define UR_ORC_NEXT           9
#define UR_ORC_RECORD         10
#define UR_ORC_REST           11
#define UR_ORC_DESCEND        12
#define UR_ORC_SCOPED         13
#define UR_ORC_ALL            14
#define UR_ORC_ADDITIVE       15
#define UR_ORC_USECURRENT     16
#define UR_ORC_CUSTOM         17
#define UR_ORC_NOOPTIMIZE     18
#define UR_ORC_COMPOUND       19
#define UR_ORC_USEFILTER      20
#define UR_ORC_TEMPORARY      21
#define UR_ORC_EXCLUSIVE      22
#define UR_ORC_CARGO          23
#define UR_ORC_SIZE           23

/* DBORDERCREATEINFO */
#define UR_ORCR_CONDINFO      1
#define UR_ORCR_BAGNAME       2
#define UR_ORCR_TAGNAME       3
#define UR_ORCR_ORDER         4
#define UR_ORCR_UNIQUE        5
#define UR_ORCR_BKEY          6
#define UR_ORCR_CKEY          7
#define UR_ORCR_SIZE          7


#define HB_SUCCESS            0
#define HB_FAILURE            1

/* Compatibility #defines. Don't use them with new code and in Harbour sources. */
#define SUCCESS               HB_SUCCESS
#define FAILURE               HB_FAILURE
