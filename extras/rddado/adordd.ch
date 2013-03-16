/*
 * Harbour Project source code:
 * ADORDD - RDD to automatically manage Microsoft ADO
 *
 * Copyright 2007 Fernando Mancera <fmancera@viaopen.com> and
 * Antonio Linares <alinares@fivetechsoft.com>
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

#ifndef _ADORDD_CH
#define _ADORDD_CH

/* Cursor Type */
#define adOpenForwardOnly               0
#define adOpenKeyset                    1
#define adOpenDynamic                   2
#define adOpenStatic                    3

/* Lock Types */
#define adLockReadOnly                  1
#define adLockPessimistic               2
#define adLockOptimistic                3
#define adLockBatchOptimistic           4

/* Field Types */
#define adEmpty                         0
#define adTinyInt                       16
#define adSmallInt                      2
#define adInteger                       3
#define adBigInt                        20
#define adUnsignedTinyInt               17
#define adUnsignedSmallInt              18
#define adUnsignedInt                   19
#define adUnsignedBigInt                21
#define adSingle                        4
#define adDouble                        5
#define adCurrency                      6
#define adDecimal                       14
#define adNumeric                       131
#define adBoolean                       11
#define adError                         10
#define adUserDefined                   132
#define adVariant                       12
#define adIDispatch                     9
#define adIUnknown                      13
#define adGUID                          72
#define adDate                          7
#define adDBDate                        133
#define adDBTime                        134
#define adDBTimeStamp                   135
#define adBSTR                          8
#define adChar                          129
#define adVarChar                       200
#define adLongVarChar                   201
#define adWChar                         130
#define adVarWChar                      202
#define adLongVarWChar                  203
#define adBinary                        128
#define adVarBinary                     204
#define adLongVarBinary                 205
#define adChapter                       136
#define adFileTime                      64
#define adPropVariant                   138
#define adVarNumeric                    139
#define adArray                         /* &H2000 */

#define adRecDeleted                    4

#define adUseNone                       1
#define adUseServer                     2
#define adUseClient                     3
#define adUseClientBatch                3

#define adKeyForeign                    2

/* Constant Group: ObjectStateEnum */
#define adStateClosed                   0
#define adStateOpen                     1
#define adStateConnecting               2
#define adStateExecuting                4
#define adStateFetching                 8

/* Constant Group: SchemaEnum */
#define adSchemaProviderSpecific        -1
#define adSchemaAsserts                 0
#define adSchemaCatalogs                1
#define adSchemaCharacterSets           2
#define adSchemaCollations              3
#define adSchemaColumns                 4
#define adSchemaCheckConstraints        5
#define adSchemaConstraintColumnUsage   6
#define adSchemaConstraintTableUsage    7
#define adSchemaKeyColumnUsage          8
#define adSchemaReferentialContraints   9
#define adSchemaReferentialConstraints  9
#define adSchemaTableConstraints        10
#define adSchemaColumnsDomainUsage      11
#define adSchemaIndexes                 12
#define adSchemaColumnPrivileges        13
#define adSchemaTablePrivileges         14
#define adSchemaUsagePrivileges         15
#define adSchemaProcedures              16
#define adSchemaSchemata                17
#define adSchemaSQLLanguages            18
#define adSchemaStatistics              19
#define adSchemaTables                  20
#define adSchemaTranslations            21
#define adSchemaProviderTypes           22
#define adSchemaViews                   23
#define adSchemaViewColumnUsage         24
#define adSchemaViewTableUsage          25
#define adSchemaProcedureParameters     26
#define adSchemaForeignKeys             27
#define adSchemaPrimaryKeys             28
#define adSchemaProcedureColumns        29
#define adSchemaDBInfoKeywords          30
#define adSchemaDBInfoLiterals          31
#define adSchemaCubes                   32
#define adSchemaDimensions              33
#define adSchemaHierarchies             34
#define adSchemaLevels                  35
#define adSchemaMeasures                36
#define adSchemaProperties              37
#define adSchemaMembers                 38
#define adSchemaTrustees                39
#define adSchemaFunctions               40
#define adSchemaActions                 41
#define adSchemaCommands                42
#define adSchemaSets                    43

/* Constant Group: Supports */
#define adAddNew                        0x1000400 /* Supports the AddNew method to add new records. */
#define adApproxPosition                0x0004000 /* Supports the AbsolutePosition and AbsolutePage properties. */
#define adBookmark                      0x0002000 /* Supports the Bookmark property to gain access to specific records. */
#define adDelete                        0x1000800 /* Supports the Delete method to delete records. */
#define adFind                          0x0080000 /* Supports the Find method to locate a row in a Recordset. */
#define adHoldRecords                   0x0000100 /* Retrieves more records or changes the next position without committing all pending changes. */
#define adIndex                         0x0100000 /* Supports the Index property to name an index. */
#define adMovePrevious                  0x0000200 /* Supports the MoveFirst and MovePrevious methods, and Move or GetRows methods to move the current record position backward without requiring bookmarks. */
#define adNotify                        0x0040000 /* Indicates that the underlying data provider supports notifications (which determines whether Recordset events are supported). */
#define adResync                        0x0020000 /* Supports the Resync method to update the cursor with the data that is visible in the underlying database. */
#define adSeek                          0x0200000 /* Supports the Seek method to locate a row in a Recordset. */
#define adUpdate                        0x1008000 /* Supports the Update method to modify existing data. */
#define adUpdateBatch                   0x0010000

/* Command type */
#define adCmdUnspecified                -1
#define adCmdUnknown                    8
#define adCmdText                       1
#define adCmdTable                      2
#define adCmdStoredProc                 4
#define adCmdFile                       256
#define adCmdTableDirect                512

#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexN)>]] ;
            [ TABLE <cTable> ] ;
            [ <dbEngine: ACCESS, MYSQL, ORACLE, INFORMIX, SQL, FIREBIRD> ];
            [ FROM <cServer> ] ;
            [ QUERY <cQuery> ] ;
            [ USER <cUser> PASSWORD <cPassword> ]=> ;
         [ hb_adoSetTable( <cTable> ) ; ] ;
         [ hb_adoSetEngine( <(dbEngine)> ) ; ] ;
         [ hb_adoSetServer( <cServer> ) ; ] ;
         [ hb_adoSetQuery( <cQuery> ) ; ] ;
         [ hb_adoSetUser( <cUser> ); hb_adoSetPassword( <cPassword> ) ; ] ;
         dbUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, ;
                    iif( <.sh.> .OR. <.ex.>, ! <.ex.>, NIL ), <.ro.> [, <cp>] ) ;
         [; dbSetIndex( <(index1)> )] ;
         [; dbSetIndex( <(indexN)> )]

#command LOCATE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         [ hb_adoSetLocateFor( <(for)> ); ] ;
         __dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#endif
