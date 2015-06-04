/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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


#include "cacherdd.ch"

#include "dbinfo.ch"
#include "rddsys.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "common.ch"

#ifdef __XHARBOUR__
#include "usrrdd.ch"
#else
#include "hbusrrdd.ch"
#endif

#ifndef HB_SYMBOL_UNUSED
#define HB_SYMBOL_UNUSED( x )                     ( ( x ) )
#endif


#ifndef HB_FT_IMAGE
#define HB_FT_IMAGE                               HB_FT_PICTURE
#endif

#define LOCK_RLOCK                                1
#define LOCK_DBRLOCK                              2
#define LOCK_FLOCK                                3

#define DB_FILENAME                               1
#define DB_RECORDS                                2
#define DB_RECINFO                                3
#define DB_OPENNUMBER                             4
#define DB_LOCKED                                 5
#define DB_STRUCT                                 6
#define DB_SIZEOF                                 6

#define RD_DATABASE                               1
#define RD_SIZEOF                                 1

#define RE_DELETED                                1
#define RE_SIZEOF                                 1

#define OI_CONDITION                              1
#define OI_EXPRESSION                             2
#define OI_POSITION                               3
#define OI_RECNO                                  4
#define OI_NAME                                   5
#define OI_NUMBER                                 6
#define OI_BAGNAME                                7
#define OI_BAGEXT                                 8
#define OI_ORDERCOUNT                             9
#define OI_FILEHANDLE                             10
#define OI_ISCOND                                 11
#define OI_ISDESC                                 12
#define OI_UNIQUE                                 13
#define OI_LAST_INDEX_KEY_VALUE                   14
#define OI_LASTRECNO                              15
#define OI_EXPRESSIONBLOCK                        16
#define OI_SCOPETOP                               17
#define OI_SCOPEBOTTOM                            18
#define OI_INDEXKEYSIZE                           19
#define OI_INDEXTYPE                              20
#define OI_WHILEINDEX                             21


ANNOUNCE CACHERDD


INIT PROCEDURE CACHERDD_INIT()
   RddRegister( "CACHERDD", RDT_FULL )
   RETURN


EXIT PROCEDURE CACHERDD_DISCONNECT()
   DBCommitAll()
   DBCloseAll()
   CacheDisconnect()
   RETURN


FUNCTION CACHERDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )
   LOCAL cSuperRDD := NIL                         /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT          ] := ( @CACHE_INIT()          )
   aMyFunc[ UR_NEW           ] := ( @CACHE_NEW()           )
   aMyFunc[ UR_CREATE        ] := ( @CACHE_CREATE()        )
   aMyFunc[ UR_CREATEFIELDS  ] := ( @CACHE_CREATEFIELDS()  )
   aMyFunc[ UR_OPEN          ] := ( @CACHE_OPEN()          )
   aMyFunc[ UR_CLOSE         ] := ( @CACHE_CLOSE()         )
   aMyFunc[ UR_APPEND        ] := ( @CACHE_APPEND()        )
   aMyFunc[ UR_DELETE        ] := ( @CACHE_DELETE()        )
   aMyFunc[ UR_DELETED       ] := ( @CACHE_DELETED()       )

   aMyFunc[ UR_SKIPFILTER    ] := ( @CACHE_SKIPFILTER()    )
   aMyFunc[ UR_SKIPRAW       ] := ( @CACHE_SKIPRAW()       )
   aMyFunc[ UR_GOTO          ] := ( @CACHE_GOTO()          )
   aMyFunc[ UR_GOTOID        ] := ( @CACHE_GOTOID()        )
   aMyFunc[ UR_GOTOP         ] := ( @CACHE_GOTOP()         )
   aMyFunc[ UR_GOBOTTOM      ] := ( @CACHE_GOBOTTOM()      )

// aMyFunc[ UR_RECINFO       ] := ( @CACHE_RECINFO()       )
   aMyFunc[ UR_RECNO         ] := ( @CACHE_RECNO()         )
   aMyFunc[ UR_RECID         ] := ( @CACHE_RECID()         )
   aMyFunc[ UR_RECCOUNT      ] := ( @CACHE_RECCOUNT()      )
   aMyFunc[ UR_GETREC        ] := ( @CACHE_GETREC()        )
   aMyFunc[ UR_GETVALUE      ] := ( @CACHE_GETVALUE()      )
   aMyFunc[ UR_PUTREC        ] := ( @CACHE_PUTREC()        )
   aMyFunc[ UR_PUTVALUE      ] := ( @CACHE_PUTVALUE()      )
   aMyFunc[ UR_ZAP           ] := ( @CACHE_ZAP()           )
   aMyFunc[ UR_GOCOLD        ] := ( @CACHE_GOCOLD()        )
   aMyFunc[ UR_GOHOT         ] := ( @CACHE_GOHOT()         )
   aMyFunc[ UR_FLUSH         ] := ( @CACHE_FLUSH()         )

   aMyFunc[ UR_SKIP          ] := ( @CACHE_SKIP()          )
   aMyFunc[ UR_SEEK          ] := ( @CACHE_SEEK()          )

   aMyFunc[ UR_LOCK          ] := ( @CACHE_LOCK()          )
   aMyFunc[ UR_RAWLOCK       ] := ( @CACHE_RAWLOCK()       )
   aMyFunc[ UR_UNLOCK        ] := ( @CACHE_UNLOCK()        )

   aMyFunc[ UR_ORDLSTFOCUS   ] := ( @CACHE_ORDLISTFOCUS()  )
   aMyFunc[ UR_ORDLSTADD     ] := ( @CACHE_ORDLISTADD()    )
   aMyFunc[ UR_ORDSETCOND    ] := ( @CACHE_ORDSETCOND()    )
   aMyFunc[ UR_ORDCREATE     ] := ( @CACHE_ORDCREATE()     )
   aMyFunc[ UR_ORDINFO       ] := ( @CACHE_ORDINFO()       )
   aMyFunc[ UR_ORDLSTCLEAR   ] := ( @CACHE_ORDLSTCLEAR()   )
   aMyFunc[ UR_ORDLSTREBUILD ] := ( @CACHE_ORDLSTREBUILD() )

   aMyFunc[ UR_INFO          ] := ( @CACHE_INFO()          )

   aMyFunc[ UR_LOCATE        ] := ( @CACHE_LOCATE()        )
   aMyFunc[ UR_SETLOCATE     ] := ( @CACHE_SETLOCATE()     )

   aMyFunc[ UR_SETSCOPE      ] := ( @CACHE_SETSCOPE()      )
   aMyFunc[ UR_SCOPEINFO     ] := ( @CACHE_SCOPEINFO()     )

   aMyFunc[ UR_CLEARFILTER   ] := ( @CACHE_CLEARFILTER()   )
   aMyFunc[ UR_SETFILTER     ] := ( @CACHE_SETFILTER()     )
   aMyFunc[ UR_GETVALUEFILE  ] := ( @CACHE_GETVALUEFILE()  )
   aMyFunc[ UR_PUTVALUEFILE  ] := ( @CACHE_PUTVALUEFILE()  )

#ifdef __RELATIONS__
   aMyFunc[ UR_CHILDEND      ] := ( @CACHE_CHILDEND        )
   aMyFunc[ UR_CHILDSTART    ] := ( @CACHE_CHILDSTART      )
   aMyFunc[ UR_CHILDSYNC     ] := ( @CACHE_CHILDSYNC       )
   aMyFunc[ UR_SYNCCHILDREN  ] := ( @CACHE_SYNCCHILDREN    )
   aMyFunc[ UR_CLEARREL      ] := ( @CACHE_CLEARREL        )
   aMyFunc[ UR_FORCEREL      ] := ( @CACHE_FORCEREL        )
   aMyFunc[ UR_RELAREA       ] := ( @CACHE_RELAREA         )
   aMyFunc[ UR_RELEVAL       ] := ( @CACHE_RELEVAL         )
   aMyFunc[ UR_RELTEXT       ] := ( @CACHE_RELTEXT         )
   aMyFunc[ UR_SETREL        ] := ( @CACHE_SETREL          )
#endif

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, cSuperRDD, aMyFunc )


//----------------------------------------------------------------------//
//                  Structure Initialization Functions
//----------------------------------------------------------------------//

#if 0
STATIC FUNCTION CACHE_RDDDATAINIT()
   RETURN  { ;
             NIL     , ;   //  1 DB_FILENAME
             {}      , ;   //  2 DB_RECORDS
             {}      , ;   //  3 DB_RECINFO
             0       , ;   //  4 DB_OPENNUMBER
             FALSE   , ;   //  5 DB_LOCKED
             NIL       ;   //  6 DB_STRUCT
           }

STATIC FUNCTION CACHE_RECDATAINIT()
   RETURN { ;
            FALSE      ;   //  1 RE_DELETED
          }
#endif


STATIC FUNCTION CACHE_DATABASEINIT()
   RETURN  { ;
             NIL     , ;   //  1 DB_FILENAME
             {}      , ;   //  2 DB_RECORDS
             {}      , ;   //  3 DB_RECINFO
             0       , ;   //  4 DB_OPENNUMBER
             FALSE   , ;   //  5 DB_LOCKED
             NIL       ;   //  6 DB_STRUCT
           }


STATIC FUNCTION CACHE_WADATAINIT()
   RETURN { ;
            NIL      , ;   //  1 WA_DATABASE
            0        , ;   //  2 WA_WORKAREA
            NIL      , ;   //  3 WA_OPENINFO
            0        , ;   //  4 WA_RECNO
            FALSE    , ;   //  5 WA_BOF
            FALSE    , ;   //  6 WA_FORCEBOF  // TO solve an hack IN dbf1.c
            FALSE    , ;   //  7 WA_EOF
            FALSE    , ;   //  8 WA_TOP
            FALSE    , ;   //  9 WA_BOTTOM
            FALSE    , ;   // 10 WA_FOUND
            Hash()   , ;   // 11 WA_LOCKS
            NIL      , ;   // 12 WA_STRUCT
            0        , ;   // 13 WA_ORDER
            {}       , ;   // 14 WA_ORDINFO
            {NIL,NIL}, ;   // 15 WA_SCOPEINFO
            0        , ;   // 16 WA_LENFIELDS
            .F.      , ;   // 17 WA_ISHOT
            0        , ;   // 18 WA_APPENDLOCKREC
            Hash()   , ;   // 19 WA_BUFFER            {}
            ""       , ;   // 20 WA_TABLENAME
            ""       , ;   // 21 WA_CACHENAME
            0        , ;   // 22 WA_PREVREC
            {}       , ;   // 23 WA_PREVBUFFER
            .F.      , ;   // 24 WA_REBUILD
            NIL      , ;   // 25 WA_FILTER
            0        , ;   // 26 WA_LASTRECORD
            -1       , ;   // 27 WA_CONXN
            NIL      , ;   // 28 WA_ORDCOND
            "SQLUSER", ;   // 29 WA_SCHEMA
            CACHE_FIELDS_FETCH_MODE_RECORD, ;   // 30 WA_FETCH
            {}       , ;   // 31 WA_RELATIONS
            .F.      , ;   // 32 WA_ISMEMO
            ""       , ;   // 33 WA_TABLEID
            NIL      , ;   // 34 WA_EMPTYBUFFER
            .T.      , ;   // 35 WA_ISMOVED
            {}       , ;   // 36 WA_FIELDNOSTRING
            NIL,     , ;   // 37 WA_RECBUFFER
            {},      , ;   // 38 WA_FIELDOFFSET
            NIL,     , ;   // 39 WA_FIELDLEN
            .T.        ;   // 40 WA_REPOSITION
          }


STATIC FUNCTION CACHE_ORDERDATAINIT()
   RETURN { ;
            {|| .F. }, ;   //  1 OI_CONDITION
            NIL      , ;   //  2 OI_EXPRESSION
            0        , ;   //  3 OI_POSITION
            0        , ;   //  4 OI_RECNO
            NIL      , ;   //  5 OI_NAME  TAG
            0        , ;   //  6 OI_NUMBER
            NIL      , ;   //  7 OI_BAGNAME
            NIL      , ;   //  8 OI_BAGEXT
            0        , ;   //  9 OI_ORDERCOUNT
            0        , ;   // 10 OI_FILEHANDLE
            .F.      , ;   // 11 OI_ISCOND
            .F.      , ;   // 12 OI_ISDESC
            .F.      , ;   // 13 OI_UNIQUE
            ""       , ;   // 14 OI_LAST_INDEX_KEY_VALUE
            0        , ;   // 15 OI_LASTRECNO
            NIL      , ;   // 16 OI_EXPRESSIONBLOCK
            NIL      , ;   // 17 OI_SCOPETOP
            NIL      , ;   // 18 OI_SCOPEBOTTOM
            0        , ;   // 19 OI_INDEXKEYSIZE
            ""       , ;   // 20 OI_INDEXTYPE
            .F.        ;   // 21 OI_WHILEINDEX
          }

//----------------------------------------------------------------------//
//                    Table create/open/close Functions
//----------------------------------------------------------------------//

STATIC FUNCTION CACHE_INIT( nRDD )
   USRRDD_RDDDATA( nRDD, Hash() )
   RETURN SUCCESS


STATIC FUNCTION CACHE_NEW( pWA )
   USRRDD_AREADATA( pWA, CACHE_WADATAINIT() )
   RETURN SUCCESS


STATIC FUNCTION CACHE_CREATEFIELDS( nWA, aStruct )
   LOCAL aFieldStruct, aField
   LOCAL aWAData  := USRRDD_AREADATA( nWA )

   /* ALWAYS check if we are connected to the database */
   IF ! Cache_Connected()
      RETURN FAILURE
   ENDIF

   // Setting WA number to current WorkArea
   aWAData[ WA_WORKAREA ] := nWA

   // CREATE new file data structure - workarea uses a reference to database
   aWAData[ WA_DATABASE ] := CACHE_DATABASEINIT()

   // Store DBF Structure
   aWAData[ WA_STRUCT   ] := aStruct
   aWAData[ WA_DATABASE ][ DB_STRUCT ] := aStruct

   // Set fields
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct
      aField := Array( UR_FI_SIZE )
      aField[ UR_FI_NAME    ] := Upper( aFieldStruct[ DBS_NAME ] )
      aField[ UR_FI_TYPE    ] := Cache_FieldType( aFieldStruct[ DBS_TYPE ], aFieldStruct )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN     ] := aFieldStruct[ DBS_LEN ]
      aField[ UR_FI_DEC     ] := aFieldStruct[ DBS_DEC ]
#ifdef __XHBSTRUCTURE__
      AAdd( aField, 0 )
      AAdd( aField, 0 )
#endif
      UR_SUPER_ADDFIELD( nWA, aField )
   NEXT

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "CREATEFIELDS", Len( aStruct ) )
#endif
   RETURN SUCCESS


STATIC FUNCTION Cache_FieldType( cTyp )

   SWITCH cTyp
   CASE "C" ; RETURN HB_FT_STRING
   CASE "L" ; RETURN HB_FT_LOGICAL
   CASE "M" ; RETURN HB_FT_MEMO
   CASE "N" ; RETURN HB_FT_LONG                   // iif( aFieldStruct[ DBS_DEC ] > 0, HB_FT_LONG, HB_FT_INTEGER )
   CASE "D" ; RETURN HB_FT_DATE
   CASE "P" ; RETURN HB_FT_IMAGE
   CASE "G" ; RETURN HB_FT_OLE
   END

   RETURN NIL


STATIC FUNCTION Cache_FieldStr( aArray )
   LOCAL nOff

#define SETNEWESTMETHOD

#ifdef SETOLDMETHOD
   AEval( aArray, {|e,i| HB_SYMBOL_UNUSED( e ), aArray[ i ] := LTrim( Str( i,4,0 ) ) + "|^|" } )
#endif

#ifdef SETNEWMETHOD
   AEval( aArray, {|e,i| HB_SYMBOL_UNUSED( e ), aArray[ i ] := Str( i,4,0 ) } )
#endif

#ifdef SETNEWESTMETHOD
   nOff := Len( aArray )
   nOff := iif( nOff < 10, 1, iif( nOff < 100, 2, iif( nOff < 1000, 3, 4 ) ) )
   AEval( aArray, {| e, i | HB_SYMBOL_UNUSED( e ), aArray[ i ] := Str( i, nOff, 0 ) } )
#endif

   RETURN NIL


STATIC FUNCTION CACHE_CREATE( nWA, aOpenInfo )
   LOCAL oError, cFullName, cSchema, cTableID, aNames, cOI, cCacheName, nCacheWA, nResult, lRet
   LOCAL cStr     := ""
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aStruct  := aWAData[ WA_STRUCT ]
   LOCAL aDBFData := aWAData[ WA_DATABASE ]

   /* ALWAYS check if we are connected to the database */
   IF ! Cache_Connected()
      RETURN FAILURE
   ENDIF

   /* Splitting the table name IN diiferent components */
   aNames     := CacheResolveNames( aOpenInfo[ UR_OI_NAME ] )
   cFullName  := aNames[ NME_FULLNAME   ]
   cSchema    := aNames[ NME_SCHEMA     ]
   cCacheName := aNames[ NME_CACHETABLE ]

   /* When there is no ALIAS we will create new one using file name */
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      aOpenInfo[ UR_OI_ALIAS ] := aNames[ NME_ALIAS ]
   ENDIF

   /* Resolve Connection Info */
   IF aOpenInfo[ UR_OI_CONNECT ] == NIL .OR. aOpenInfo[ UR_OI_CONNECT ] == 0
      aOpenInfo[ UR_OI_CONNECT ] := CacheSetConnection()
   ENDIF
   aWAData[ WA_CONXN ] := aOpenInfo[ UR_OI_CONNECT ]

   /* Resolve Open Mode */
#if 0
   nMode := iif( aOpenInfo[ UR_OI_SHARED   ], FO_SHARED , FO_EXCLUSIVE ) + ;
            iif( aOpenInfo[ UR_OI_READONLY ], FO_READ   , FO_READWRITE )
#endif
   /*  Normalize recent changes in rdd layer to distinuish integer vs numeric fields */
   AEval( aStruct, {|e_,i| iif( e_[ 2 ] $ "NI", aStruct[ i,2 ] := "N", NIL ) } )

   aWAData[ WA_SCHEMA    ] := cSchema
   aWAData[ WA_TABLENAME ] := cFullName
   aWAData[ WA_OPENINFO  ] := aOpenInfo
   aWAData[ WA_CACHENAME ] := cCacheName

   /* Setting file attribs */
   aWAData[ WA_DATABASE  ][ DB_FILENAME ] := cFullName

   /* Check IF database is already present IN memory slots */
   cTableID := aWAData[ WA_SCHEMA ] + "_" + aNames[ NME_FULLNAME ] + "_" + NTRIM( aOpenInfo[ UR_OI_CONNECT ] )
   aWAData[ WA_TABLEID ] := cTableID
   IF .T.
      /* I need exclusive mode in creation */
      aDBFData[ DB_LOCKED ] := TRUE

      /*  Actually create the table */
      AEval( aStruct, {|e_| cStr += Upper( e_[ 1 ] ) + " " + e_[ 2 ] + " " +;
                                LTrim( Str( e_[ 3 ] ) ) + " " + LTrim( Str( e_[ 4 ] ) ) + "~" } )

      lRet := CacheCreateTable( aWAData[ WA_CONXN ], ;
                ( aNames[ NME_CACHESQLNAME ] + "~" + aNames[ NME_CACHECLASSNAME ] + "~" ), cStr )
      IF ! lRet
         NetErr( .T. )
         RETURN FAILURE
      ENDIF

      /* I need Exclusive mode IN creation, reversed */
      aDBFData[ DB_LOCKED ] := FALSE
   ELSE
      oError := ErrorNew()
      oError:GenCode     := EG_CREATE
      oError:SubCode     := 1004
      oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + " - Database Already Exists!)"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   /* Open the Table as DBFCDX supports it */
   cOI := cCacheName + " ~ " + ;
           aOpenInfo[ UR_OI_ALIAS    ]             + " ~ " + ;
      iif( aOpenInfo[ UR_OI_SHARED   ], "T", "F" ) + " ~ " + ;
      iif( aOpenInfo[ UR_OI_READONLY ], "T", "F" ) + " ~ "

   IF ( nCacheWA := CachePrepare( aWAData[ WA_CONXN ]         , ;
                                  nWA                         , ;
                                  aNames[ NME_CACHECLASSNAME ], ;
                                  cOI ) ) == 0
      NetErr( .T. )
      RETURN FAILURE
   ENDIF

   /* Put open informations */
   aWAData[ WA_WORKAREA  ] := nCacheWA

   aStruct := CACHE_GETSTRUCT( nWA )
   aWAData[ WA_LENFIELDS ] := Len( aStruct )
   aWAData[ WA_STRUCT    ] := aStruct
   aWAData[ WA_DATABASE  ][ DB_STRUCT ] := aStruct

   aWAData[ WA_BUFFER      ] := AFill( Array( Len( aStruct ) ), NIL )

   aWAData[ WA_PREVBUFFER  ] := AFill( Array( Len( aStruct ) ), NIL )
   aWAData[ WA_EMPTYBUFFER ] := Array( Len( aStruct ) )
   AEval( aWAData[ WA_EMPTYBUFFER ], {|e, i| HB_SYMBOL_UNUSED( e ), aWAData[ WA_EMPTYBUFFER, i ] := RddEmptyField( i, aStruct ) } )

   aWAData[ WA_FIELDNOSTR ] := Array( Len( aStruct ) )
   Cache_FieldStr( aWAData[ WA_FIELDNOSTR ] )

   nResult := UR_SUPER_OPEN( nCacheWA, aOpenInfo )
   NetErr( nResult == FAILURE )

   IF CacheSetUseExclusive() == 1
      IF ! aOpenInfo[ UR_OI_SHARED ]
         aDBFData[ DB_LOCKED ] := TRUE
      ENDIF
   ENDIF

   IF nResult == SUCCESS
      IF AScan( aStruct, {| e_ | e_[ 2 ] $ "MP" } ) > 0
         aWAData[ WA_ISMEMO ] := .T.
      ENDIF
      CACHE_GOTOP( nWA )
      /*
      IF set( _SET_AUTOPEN )
         DbSetIndex( cFullName )
      ENDIF
      */
   ENDIF

   /* increase open number */
   aDBFData[ DB_OPENNUMBER ]++

#ifdef __DEVELOPMENT__
   //__rddDebug( nWA, 'CREATE', -99, ProcName(5),ProcName(6),Cache_SomeInfo(200,'CreateTable') )
#endif
   RETURN nResult


STATIC FUNCTION CACHE_OPEN( nWA, aOpenInfo )
   LOCAL aNames, cFullName, cSchema, cCacheName
   LOCAL cOI, nCacheWA, nResult, aFieldStruct, aField
   LOCAL hRDDData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aStruct  := aWAData[ WA_STRUCT ]

   /* Always check if we are connected to the database */
   IF ! Cache_Connected()
      RETURN FAILURE
   ENDIF

   /* Splitting the table name IN diiferent components */
   aNames     := CacheResolveNames( aOpenInfo[ UR_OI_NAME ] )
   cFullName  := aNames[ NME_FULLNAME   ]
   cSchema    := aNames[ NME_SCHEMA     ]
   cCacheName := aNames[ NME_CACHETABLE ]

   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      aOpenInfo[ UR_OI_ALIAS ] := aNames[ NME_ALIAS ]
   ENDIF

   /* First Check IF same alias is already IN use */
   IF Select( aOpenInfo[ UR_OI_ALIAS ] ) > 0
      NetErr( .T. )
      RETURN FAILURE
   ENDIF

   /* Getting database infos FROM current workarea */
   aWAData[ WA_DATABASE ] := CACHE_DATABASEINIT()

   /* Resolve Connection Info */
   IF aOpenInfo[ UR_OI_CONNECT ] == NIL .OR. aOpenInfo[ UR_OI_CONNECT ] == 0
      aOpenInfo[ UR_OI_CONNECT ] := CacheSetConnection()
   ENDIF
   aWAData[ WA_CONXN ] := aOpenInfo[ UR_OI_CONNECT ]

   /* Resolve Open Mode */
#if 0
   nMode := iif( aOpenInfo[ UR_OI_SHARED   ], FO_SHARED , FO_EXCLUSIVE ) + ;
            iif( aOpenInfo[ UR_OI_READONLY ], FO_READ   , FO_READWRITE )
#endif
   aWAData[ WA_SCHEMA    ] := cSchema
   aWAData[ WA_TABLENAME ] := cFullName
   aWAData[ WA_OPENINFO  ] := aOpenInfo
   aWAData[ WA_CACHENAME ] := cCacheName

   /* Setting file attribs */
   aWAData[ WA_DATABASE  ][ DB_FILENAME ] := cFullName

   /* Adding new database IN RDD memory slots using SCHEMA_Filename_nConnection as key */
   aWAData[ WA_TABLEID ] := aWAData[ WA_SCHEMA ] + "_" + aWAData[ WA_TABLENAME ] + "_" + NTRIM( aOpenInfo[ UR_OI_CONNECT ] )
   HSet( hRDDData, aWAData[ WA_TABLEID ], AClone( aWAData[ WA_DATABASE ] ) )

   cOI := cCacheName + " ~ " + ;
           aOpenInfo[ UR_OI_ALIAS    ]            + " ~ " + ;
      iif( aOpenInfo[ UR_OI_SHARED   ], "T","F" ) + " ~ " + ;
      iif( aOpenInfo[ UR_OI_READONLY ], "T","F" ) + " ~ "

   IF ( nCacheWA := CachePrepare( aWAData[ WA_CONXN ]         , ;
                                  nWA                         , ;
                                  aNames[ NME_CACHECLASSNAME ], ;
                                  cOI ) ) == 0
      NetErr( .T. )
      RETURN FAILURE
   ENDIF

   aWAData[ WA_WORKAREA  ] := nCacheWA

   aStruct := CACHE_GETSTRUCT( nWA )

   aWAData[ WA_LENFIELDS ] := Len( aStruct )
   aWAData[ WA_STRUCT    ] := aStruct
   aWAData[ WA_DATABASE  ][ DB_STRUCT ] := aStruct

   aWAData[ WA_DATABASE  ][ DB_LOCKED  ] := FALSE

   aWAData[ WA_BUFFER      ] := aFill( Array( Len( aStruct ) ), NIL )

   aWAData[ WA_PREVBUFFER  ] := aFill( Array( Len( aStruct ) ), NIL )
   aWAData[ WA_EMPTYBUFFER ] := Array( Len( aStruct ) )
   AEval( aWAData[ WA_EMPTYBUFFER ], {|e, i| HB_SYMBOL_UNUSED( e ), aWAData[ WA_EMPTYBUFFER,i ] := RddEmptyField( i, aStruct ) } )
   aWAData[ WA_FIELDNOSTR ] := Array( Len( aStruct ) )
   Cache_FieldStr( aWAData[ WA_FIELDNOSTR ] )

   /*  Set fields  */
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct
      aField := Array( UR_FI_SIZE )

      aField[ UR_FI_NAME    ] := Upper( aFieldStruct[ DBS_NAME ] )
      aField[ UR_FI_TYPE    ] := Cache_FieldType( aFieldStruct[ DBS_TYPE ], aFieldStruct )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN     ] := aFieldStruct[ DBS_LEN ]
      aField[ UR_FI_DEC     ] := aFieldStruct[ DBS_DEC ]
#ifdef __XHBSTRUCTURE__
      AAdd( aField, 0 )
      AAdd( aField, 0 )
#endif

      UR_SUPER_ADDFIELD( nWA, aField )
   NEXT

   IF CacheSetUseExclusive() == 1
      IF ! aOpenInfo[ UR_OI_SHARED ]
         aWAData[ WA_DATABASE ][ DB_LOCKED ] := TRUE
      ENDIF
   ENDIF

   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )
   NetErr( nResult == FAILURE )

#ifdef __DEVELOPMENT__
   __rddDebug( nWa, "OPEN", -99, aWAData[ WA_WORKAREA ] )
#endif
   IF nResult == SUCCESS
      IF AScan( aStruct, {| e_ | e_[ 2 ] $ "MP" } ) > 0
         aWAData[ WA_ISMEMO ] := .T.
      ENDIF
      IF Set( _SET_AUTOPEN )
         DbSetIndex( cFullName )
         dbSetOrder( 0 )
      ENDIF
      CACHE_GOTOP( nWA )
   ENDIF

   RETURN nResult


STATIC FUNCTION CACHE_CLOSE( nWA )
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL hRDDData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF
#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"CLOSE",NIL )
#endif

   CacheRelease( aWAData[ WA_CONXN ], nWA )

   IF hHasKey( hRDDData, aWAData[ WA_TABLEID ] )
      hDel( hRDDData, aWAData[ WA_TABLEID ] )
   ENDIF

   RETURN UR_SUPER_CLOSE( nWA )


STATIC FUNCTION CACHE_INFO( nWA, nInfo, xResult )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   DO CASE
   CASE nInfo ==  DBI_ISDBF                       /* Does this RDD support DBFs?         */
   CASE nInfo ==  DBI_CANPUTREC                   /* Can this RDD Put Records?           */
   CASE nInfo ==  DBI_GETHEADERSIZE               /* DATA file's header size             */
   CASE nInfo ==  DBI_LASTUPDATE                  /* Last date this file was written TO  */
   CASE nInfo ==  DBI_GETDELIMITER                /* The delimiter (as a string)         */
   CASE nInfo ==  DBI_SETDELIMITER                /* The delimiter (as a string)         */
   CASE nInfo ==  DBI_GETRECSIZE                  /* The size of 1 record IN the file    */
      xResult := 0
      AEval( aWAData[ WA_STRUCT ], {|e_| xResult += e_[ 3 ] } )

   CASE nInfo ==  DBI_GETLOCKARRAY                /* An array of locked records' numbers */
      IF Cache_DbfData( nWA )[ DB_LOCKED ]
         xResult := {}
         RETURN SUCCESS
      ENDIF
      xResult := CacheGetRecLockList( nWA )

   CASE nInfo ==  DBI_TABLEEXT                    /* The DATA file's file extension      */
   CASE nInfo ==  DBI_FULLPATH                    /* The Full path TO the DATA file      */
      xResult := aWAData[ WA_OPENINFO, UR_OI_NAME ]

   CASE nInfo ==  DBI_ISFLOCK                     /* Is there a file lock active?        */
      xResult := Cache_DbfData( nWA )[ DB_LOCKED ]

   CASE nInfo ==  DBI_CHILDCOUNT                  /* Number of child relations set       */
   CASE nInfo ==  DBI_FILEHANDLE                  /* The DATA file's OS file handle      */
   CASE nInfo ==  DBI_BOF                         /* Same as bof()                       */
   CASE nInfo ==  DBI_EOF                         /* Same as eof()                       */
   CASE nInfo ==  DBI_DBFILTER                    /* Current Filter setting              */
   CASE nInfo ==  DBI_FOUND                       /* Same as found()                     */
   CASE nInfo ==  DBI_FCOUNT                      /* How many fields IN a record?        */
   CASE nInfo ==  DBI_LOCKCOUNT                   /* Number of record locks              */
   CASE nInfo ==  DBI_VALIDBUFFER                 /* Is the record buffer valid?         */
   CASE nInfo ==  DBI_ALIAS                       /* Name (alias) FOR this workarea      */
   CASE nInfo ==  DBI_GETSCOPE                    /* The codeblock used IN LOCATE        */
   CASE nInfo ==  DBI_LOCKOFFSET                  /* The offset used FOR logical locking */
   CASE nInfo ==  DBI_SHARED                      /* Was the file opened shared?         */
   CASE nInfo ==  DBI_MEMOEXT                     /* The memo file's file extension      */
   CASE nInfo ==  DBI_MEMOHANDLE                  /* File handle of the memo file        */
   CASE nInfo ==  DBI_MEMOBLOCKSIZE               /* Memo File's block size              */
   ENDCASE

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,'INFO', NIL, nInfo )
#endif
   RETURN SUCCESS


/*
#define DBRI_DELETED              1
#define DBRI_LOCKED               2
#define DBRI_RECSIZE              3
#define DBRI_RECNO                4
#define DBRI_UPDATED              5
#define DBRI_ENCRYPTED            6
#define DBRI_RAWRECORD            7
#define DBRI_RAWMEMOS             8
#define DBRI_RAWDATA              9
*/
/*
STATIC FUNCTION CACHE_RECINFO( nWA, nInfo, nRecNo, xResult )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   DO CASE
   CASE nInfo == DBRI_DELETED
      xResult := .F.
      RETURN SUCCESS

   CASE nInfo == DBRI_LOCKED
      xResult := HHasKey( aWAData[ WA_LOCKS ], nRecNo )
      RETURN SUCCESS
   ENDCASE

   RETURN FAILURE
*/

//----------------------------------------------------------------------//
//                     Record Movement Functions
//----------------------------------------------------------------------//

STATIC FUNCTION CACHE_SETFILTER( nWA, aScopeInfo )

   IF ValType( aScopeInfo ) == "A"
      USRRDD_AREADATA( nWA )[ WA_FILTER ] := aScopeInfo
#ifdef __DEVELOPMENT__
      __rddDebug( nWA,"SETFILTER", NIL, iif( aScopeInfo == NIL, "NOVALUE", aScopeInfo[ 2 ] ) )
#endif
   ENDIF

   RETURN SUCCESS


STATIC FUNCTION CACHE_CLEARFILTER( nWA )

   IF ! ProcName( 2 ) == "CACHE_CLOSE"
      USRRDD_AREADATA( nWA )[ WA_FILTER ] := NIL
#ifdef __DEVELOPMENT__
      __rddDebug( nWA,"CLEARFILTER", NIL )
#endif
   ENDIF

   RETURN SUCCESS


STATIC FUNCTION CACHE_SETSCOPE( /*nWA, aScopeInfo*/ )
   RETURN FAILURE


STATIC FUNCTION CACHE_SCOPEINFO( /*nWA, aScopeInfo*/ )
   RETURN FAILURE


STATIC FUNCTION CACHE_SEEK( nWA, lSoftSeek, xValue, lFindLast )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nRecord, cInfo, oError

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   USRRDD_SETFOUND( nWA, .F. )

   aWAData[ WA_ISMOVED ] := .T.

   IF aWAData[ WA_ORDER ] > 0
      IF aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_INDEXTYPE ] == "C"
         IF Len( xValue ) > aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_INDEXKEYSIZE ]
            xValue := Pad( xValue, aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_INDEXKEYSIZE ] )
         ENDIF
      ENDIF

      nRecord := CacheSeek( aWAData[ WA_CONXN ], ;
                              nWA, ;
                                aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_INDEXTYPE ] + "~" +;
                                iif( lSoftSeek, "T", "F" ) + "~" +;
                                iif( lFindLast, "T", "F" ) + "~" + RddXtoSI( xValue ) )
      IF nRecord == 0
         USRRDD_SETEOF( nWA, .T. )
      ELSE
         IF lSoftSeek
            cInfo := CACHE_SOMEINFO( 60,"SoftSeek" )
            IF cInfo == "FALSE"
               USRRDD_SETFOUND( nWA, .F. )
            ELSE
               USRRDD_SETFOUND( nWA, .T. )
            ENDIF
            USRRDD_SETEOF( nWA, .F. )
         ELSE
            USRRDD_SETEOF( nWA, .F. )
            USRRDD_SETFOUND( nWA, .T. )
         ENDIF
      ENDIF

      aWAData[ WA_RECNO ] := nRecord
   ELSE
      CacheDebug( "CACHE_SEEK", IndexOrd(), alias(), ProcName(2), ProcLine(2), ProcName(3), ProcLine(3)  )
      oError := ErrorNew()

      oError:GenCode     := 2021
      oError:SubCode     := 1
      oError:Description := "Workarea not Indexed!"
      oError:CanDefault  := .T.

      UR_SUPER_ERROR( nWA, oError )

      RETURN FAILURE
   ENDIF

   aWAData[ WA_REPOSITION ] := .F.

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, 'SEEK', NIL, xValue, found(), Cache_SomeInfo(60,"Seek") )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_SKIP( nWA, nRecsToSkip )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL i, nSkips, nDirectn

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

   IF nRecsToSkip == 0
      RETURN SUCCESS
   ENDIF

   USRRDD_SETFOUND( nWA, .F. )
   USRRDD_SETBOF( nWA, .F. )
   USRRDD_SETEOF( nWA, .F. )

   IF aWAData[ WA_FILTER ] == NIL
      CACHE_SKIPRAW( nWA, nRecsToSkip, aWAData )
   ELSE
      nSkips   := abs( nRecsToSkip )
      nDirectn := iif( nRecsToSkip < 0, -1, 1 )

      FOR i := 1 TO nSkips
         IF ! CACHE_SKIPRAW( nWA, nDirectn, aWAData ) == SUCCESS
            EXIT
         ENDIF
         IF ! CACHE_SKIPFILTER( nWA, nDirectn, aWAData ) == SUCCESS
            EXIT
         ENDIF
      NEXT
   ENDIF

   aWAData[ WA_REPOSITION ] := .F.                // must issue it

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"SKIP", NIL, nRecsToSkip )            //, Cache_SomeInfo( 60,'SKIP' ) )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_SKIPFILTER( nWA, nRecords, aWAData )

   IF aWAData[ WA_FILTER ] == NIL
      RETURN SUCCESS
   ENDIF

   DO WHILE .T.
      IF Eval( aWAData[ WA_FILTER,1 ] )
         RETURN SUCCESS
      ENDIF
      IF ! ( CACHE_SKIPRAW( nWA, nRecords, aWAData ) == SUCCESS )
         RETURN FAILURE
      ENDIF
   ENDDO

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "SKIPFILTER", nRecords )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_SKIPRAW( nWA, nRecsToSkip, aWAData )
   LOCAL lRet := SUCCESS
   LOCAL oError, nRecord, xIndexVal

   IF aWAData[ WA_RECNO ] <= 0
      IF nRecsToSkip < 0
         /* Goto bottom irrespective of the scope */
         CACHE_GOBOTTOM( nWA )
      ELSE
         USRRDD_SETEOF( nWA, .T. )
         USRRDD_SETBOTTOM( nWA, .F. )
      ENDIF

      RETURN FAILURE
   ENDIF

   nRecord := aWAData[ WA_RECNO ] := CacheSkip( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], nRecsToSkip, aWAData[ WA_REPOSITION ] )

   //  NOT a RDD behavior. Proprietory to CacheRDD. The error is generated when:
   //
   //  recNo := RecNo()
   //  DbDelete()
   //  DbSeek( some )
   //  DbGoto( recNo )
   //  DbSkip()
   //
   IF nRecord == -1
      oError := ErrorNew()

      oError:GenCode     := 17004
      oError:SubCode     := 17005
      oError:Description := "Skip issued off a deleted record!"
      oError:FileName    := aWAData[ WA_OPENINFO, UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )

      RETURN FAILURE
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

   IF nRecord <= 0
      IF nRecsToSkip < 0
         CACHE_GOTOP( nWA )                       // Double Check It  ......  ..........
         USRRDD_SETBOF( nWA, .T. )
         USRRDD_SETTOP( nWA, .F. )
      ELSE
         USRRDD_SETEOF( nWA, .T. )
         USRRDD_SETBOTTOM( nWA, .F. )
      ENDIF
      lRet := FAILURE
   ELSE
      // IF Scope is Defined
      //
      IF ( aWAData[ WA_ORDER ] > 0 ) ;
                    .AND. ;
         ( ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP    ] == NIL ) ;
                    .OR. ;
           ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] == NIL ) )

         xIndexVal := Eval( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_EXPRESSIONBLOCK ] )

         // Check if it is prior to scope
         //
         IF ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ] == NIL )
            IF ( nRecsToSkip < 0 )
               IF ( xIndexVal < aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ] )
                  // Because no phantom Record on top
                  //
                  CACHE_GOTOP( nWA )
                  USRRDD_SETBOF( nWA, .T. )
                  USRRDD_SETTOP( nWA, .F. )
                  RETURN FAILURE
               ENDIF
            ENDIF
         ENDIF

         IF ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] == NIL )
            IF ( nRecsToSkip > 0 )
               IF ( xIndexVal > aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] )
                  aWAData[ WA_RECNO ] := 0
                  USRRDD_SETEOF( nWA, .T. )
                  USRRDD_SETBOTTOM( nWA, .F. )
                  RETURN FAILURE
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"SKIPRAW", , nRecsToSkip, Cache_SomeInfo( 60,"SKIP" ) )
#endif
   RETURN lRet


STATIC FUNCTION CacheIncLast( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C" ;  RETURN SubStr( xValue, 1, Len( xValue ) - 1 ) + chr( asc( right( xValue, 1 ) ) + 1 )
   CASE cType == "D" ;  RETURN xValue + 1
   CASE cType == "N" ;  RETURN xValue + 0.001
   ENDCASE

   RETURN xValue


STATIC FUNCTION CACHE_GOTOP( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL xIndexVal

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

   USRRDD_SETFOUND( nWA, .F. )

   IF ( aWAData[ WA_ORDER ] > 0 ) .AND. ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ] == NIL )
      // SOFTSEEK
      CACHE_SEEK( nWA, .T., aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ], .F. )

      IF aWAData[ WA_RECNO ] > 0
         // Check IF within scope
         IF ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] == NIL )
            xIndexVal := Eval( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_EXPRESSIONBLOCK ] )

            IF ( xIndexVal > aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] )
               aWAData[ WA_RECNO ] := 0
            ENDIF
         ENDIF
      ENDIF
   ELSE
      aWAData[ WA_RECNO ] := CacheGoTop( aWAData[ WA_CONXN ], nWA )
   ENDIF

   USRRDD_SETBOF( nWA, aWAData[ WA_RECNO ] == 0 )
   USRRDD_SETTOP( nWA, aWAData[ WA_RECNO ] > 0  )
   USRRDD_SETEOF( nWA, aWAData[ WA_RECNO ] == 0 )

   IF ! aWAData[ WA_FILTER ] == NIL
      CACHE_SKIPFILTER( nWA, 1, aWAData )
   ENDIF

   aWAData[ WA_REPOSITION ] := .F.

#ifdef __DEVELOPMENT__
   IF ! ( ProcName( 1 ) == "CACHE_OPEN" )
      __rddDebug( nWA,"GOTOP", NIL )
   ENDIF
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_GOBOTTOM( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL xValue, xIndexVal

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   USRRDD_SETFOUND( nWA, .F. )

   aWAData[ WA_ISMOVED ] := .T.

   IF ( aWAData[ WA_ORDER ] > 0 ) .AND. ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] == NIL )
      xValue := CacheIncLast( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] )

      CACHE_SEEK( nWA, .T., xValue, .F. )         // SoftSeek
      IF aWAData[ WA_RECNO ] > 0
         aWAData[ WA_RECNO ] := CacheSkip( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], -1 )
      ENDIF

      IF aWAData[ WA_RECNO ] == 0
         aWAData[ WA_RECNO ] := CacheGoBottom( aWAData[ WA_CONXN ], nWA )
      ENDIF

      IF aWAData[ WA_RECNO ] > 0
         IF ! ( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ] == NIL )
            xIndexVal := Eval( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_EXPRESSIONBLOCK ] )
            IF ( xIndexVal < aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_SCOPETOP ] )
               aWAData[ WA_RECNO ] := 0
            ENDIF
         ENDIF
      ENDIF
   ELSE
      aWAData[ WA_RECNO ] := CacheGoBottom( aWAData[ WA_CONXN ], nWA )
   ENDIF

   USRRDD_SETBOF( nWA, .F. )
   USRRDD_SETEOF( nWA, aWAData[ WA_RECNO ] <= 0 )
   USRRDD_SETBOTTOM( nWA, aWAData[ WA_RECNO ] > 0 )

   IF ! ( aWAData[ WA_FILTER ] == NIL )
      CACHE_SKIPFILTER( nWA, -1, aWAData )
   ENDIF

   aWAData[ WA_REPOSITION ] := .F.

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "GOBOTTOM", NIL /*, Cache_SomeInfo( 60,'GoBottom' )*/ )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_GOTO( nWA, nRecord )
   RETURN  CACHE_GOTOID( nWA, nRecord )


STATIC FUNCTION CACHE_GOTOID( nWA, nRecord )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL xIndexKey

   //  GOCOLD
   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

   IF nRecord <= 0 .OR. CacheRecCount( aWAData[ WA_CONXN ], nWA ) < nRecord
      aWAData[ WA_RECNO ] := 0

      USRRDD_SETEOF( nWA, .T. )
      USRRDD_SETBOF( nWA, .T. )
      USRRDD_SETBOTTOM( nWA, .F. )
      USRRDD_SETTOP( nWA, .F. )
   ELSE
      aWAData[ WA_RECNO ] := nRecord

      USRRDD_SETBOF( nWA, .F. )
      USRRDD_SETEOF( nWA, .F. )

      IF aWAData[ WA_ORDER ] > 0
         IF aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_WHILEINDEX ]
            xIndexKey := RddXtoS( Eval( aWAData[ WA_ORDINFO, aWAData[ WA_ORDER ], OI_EXPRESSIONBLOCK ] ) )
            CacheSetIndexPos( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], xIndexKey )
         ENDIF
      ELSE
         aWAData[ WA_REPOSITION ] := .T.
      ENDIF
   ENDIF

   USRRDD_SETFOUND( nWA, .F. )

   //  Force Relations
   //  ToDo

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"GOTOID", NIL, Str( nRecord, 8, 0 ), Cache_SomeInfo( 60, "OrdSetFocus" ) )
#endif
   RETURN SUCCESS


/*
#define UR_SI_BFOR                              1    NIL  B
#define UR_SI_CFOR                              2    NIL
#define UR_SI_BWHILE                            3    NIL  B
#define UR_SI_CWHILE                            4    NIL
#define UR_SI_NEXT                              5    NIL  N
#define UR_SI_RECORD                            6    NIL
#define UR_SI_REST                              7         L
#define UR_SI_IGNOREFILTER                      8         L
#define UR_SI_INCLUDEDELETED                    9         L
#define UR_SI_LAST                             10         L
#define UR_SI_IGNOREDUPS                       11         L
#define UR_SI_BACKWARD                         12         L
#define UR_SI_OPTIMIZED                        13         L
*/
STATIC FUNCTION CACHE_SETLOCATE( nWA, aDbScopeInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aWAData[ WA_SCOPEINFO ] := aDbScopeInfo

   USRRDD_SETFOUND( nWA, .F. )

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   /* 26Mar2010 */
   IF aWAData[ WA_SCOPEINFO, UR_SI_NEXT ] == NIL .OR. aWAData[ WA_SCOPEINFO, UR_SI_NEXT ] == 0
      IF aWAData[ WA_SCOPEINFO, UR_SI_REST ] == NIL .OR. !( aWAData[ WA_SCOPEINFO, UR_SI_REST ] )
         CACHE_GOTOP( nWA )
      ENDIF
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"SETLOCATE" )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_LOCATE( nWA, lContinue )
   LOCAL i
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL IsFound := .F.
   LOCAL lWhile  := ValType( aWAData[ WA_SCOPEINFO, UR_SI_BWHILE ] ) == 'B'

   IF ! lContinue
      IF ! ( aWAData[ WA_SCOPEINFO, UR_SI_NEXT ] == NIL )
         FOR i := 1 TO aWAData[ WA_SCOPEINFO, UR_SI_NEXT ]
            IF lWhile
               IF !( Eval( aWAData[ WA_SCOPEINFO, UR_SI_BWHILE ] ) )
                  EXIT
               ENDIF
            ENDIF

            IF ( IsFound := Eval( aWAData[ WA_SCOPEINFO, UR_SI_BFOR ] ) ) .OR. Eof()
               EXIT
            ENDIF

            CACHE_SKIP( nWA, 1 )
         NEXT
      ELSE
         DO WHILE .T.
            IF lWhile
               IF ! Eval( aWAData[ WA_SCOPEINFO, UR_SI_BWHILE ] )
                  EXIT
               ENDIF
            ENDIF

            IF ( IsFound := Eval( aWAData[ WA_SCOPEINFO, UR_SI_BFOR ] ) ) .OR. Eof()
               EXIT
            ENDIF

            CACHE_SKIP( nWA, 1 )
         ENDDO
      ENDIF
   ELSE
      DO WHILE .T.
         CACHE_SKIP( nWA, 1 )

         IF ( IsFound := Eval( aWAData[ WA_SCOPEINFO, UR_SI_BFOR ] ) ) .OR. Eof()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   USRRDD_SETFOUND( nWA, IsFound )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"LOCATE", NIL, IsFound )
#endif
   RETURN SUCCESS

//----------------------------------------------------------------------//
//                        Data in/out functions
//----------------------------------------------------------------------//

STATIC FUNCTION CACHE_APPEND( nWA )//, lReleaseLocks )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nRecord, oError, lNetErr

   USRRDD_SETFOUND( nWA, .F. )

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   IF aWAData[ WA_OPENINFO, UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := EG_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   aWAData[ WA_ISMOVED ] := .T.

   IF ( nRecord := CacheInsert( aWAData[ WA_CONXN ], nWA ) ) > 0
      IF CacheInsertLockMode() == 1
         IF aWAData[ WA_APPENDLOCKREC ] > 0 .AND. HHasKey( aWAData[ WA_LOCKS ], aWAData[ WA_APPENDLOCKREC ] )
            HDel( aWAData[ WA_LOCKS ], aWAData[ WA_APPENDLOCKREC ] )
         ENDIF
         aWAData[ WA_APPENDLOCKREC ] := nRecord
      ENDIF
      HSet( aWAData[ WA_LOCKS ], nRecord, 1 )

      USRRDD_SETBOF( nWA, .F. )
      USRRDD_SETEOF( nWA, .F. )

      aWAData[ WA_RECNO ] := nRecord
      lNetErr := .F.
   ELSE
      lNetErr := .T.
   ENDIF

   NetErr( lNetErr )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "APPEND", NIL, nRecord )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_GETREC( nWA /*,xValue*/ )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( aWAData )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "GETREC", NIL, /*xValue*/ )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_PUTREC( nWA /*,xValue*/ )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( aWAData )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "PUTREC", NIL, /*xValue*/ )
#endif
   RETURN SUCCESS


// Inserts contents of a file into a memo field
//
STATIC FUNCTION CACHE_PUTVALUEFILE( nWA, nField, cFileToLoad, lSuccess )
   LOCAL cBuffer, nHandle, nRead, nSize

   lSuccess := .F.
   IF File( cFileToLoad )
      IF ! ( ( nHandle := FOpen( cFileToLoad, FO_READ ) ) == -1 )
         nSize   := FSeek( nHandle, 0, FS_END )
         FSeek( nHandle, 0, FS_SET )
         cBuffer := Space( nSize )
         nRead   := FRead( nHandle, @cBuffer, nSize )
         FClose( nHandle )

         IF nRead == nSize
            CACHE_PUTVALUE( nWA, nField, cBuffer )
            CACHE_FLUSH( nWA )
            lSuccess := .T.
         ENDIF
      ENDIF
   ENDIF

   NetErr( ! lSuccess )

   RETURN iif( lSuccess, SUCCESS, FAILURE )


// Inserts the contents of a field into a file
//
STATIC FUNCTION CACHE_GETVALUEFILE( nWA, nField, cFileToSave, nMode )
   LOCAL cBuffer, nHandle, lSuccess, nWritten

   HB_SYMBOL_UNUSED( nWA )

   cBuffer  := fieldget( nField )
   lSuccess := .F.

   IF nMode == FILEGET_APPEND .AND. File( cFileToSave )
      nHandle := FOpen( cFileToSave, FO_WRITE )
      IF ! ( nHandle == -1 )
         FSeek( nHandle, 0, FS_END )
         nWritten := FWrite( nHandle, cBuffer, Len( cBuffer ) )
         FClose( nHandle )
         lSuccess := nWritten == Len( cBuffer )
      ENDIF
   ELSE
      nHandle := FCreate( cFileToSave )
      IF ! ( nHandle == -1 )
         nWritten := FWrite( nHandle, cBuffer, Len( cBuffer ) )
         FClose( nHandle )
         lSuccess := nWritten == Len( cBuffer )
      ENDIF
   ENDIF

   NetErr( ! lSuccess )
   RETURN iif( lSuccess, SUCCESS, FAILURE )


// NOT a RDD FUNCTION
//
STATIC FUNCTION CACHE_ISRECLOCKED( nWA, aWAData, nRecNo )
   LOCAL aLocks := CacheGetRecLockList( nWA )

   IF Empty( aLocks ) .OR. AScan( aLocks, {|e| e == nRecNo } ) == 0
      IF ! Cache_DbfData( nWA )[ DB_LOCKED ]
         IF aWAData[ WA_OPENINFO ][ UR_OI_SHARED ]
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF
   RETURN .T.


STATIC FUNCTION CACHE_PUTVALUE( nWA, nField, xValue )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oError, cType, cT

   IF aWAData[ WA_OPENINFO, UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := EG_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   IF ( ! HHasKey( aWAData[ WA_LOCKS ], aWAData[ WA_RECNO ] ) ) .AND. ( ! Cache_DbfData( nWA )[ DB_LOCKED ] ) /* It must ALWAYS has this flag true, confirmed at the time of lock */
      oError := ErrorNew()
      oError:GenCode     := EG_UNLOCKED
      oError:SubCode     := 1022
      oError:Description := HB_LANGERRMSG( EG_UNLOCKED )
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   IF nField > 0 .AND. nField <= aWAData[ WA_LENFIELDS ]
      cType := aWAData[ WA_STRUCT, nField, 2 ]
      cT    := ValType( xValue )

      IF ( cT == "C" .AND. ( cType $ "CMP" ) )
         // OK, memo field is mapped to picture to be binary compatible
         //
      ELSEIF ! ( cT == cType )
         oError := ErrorNew()
         oError:GenCode     := EG_DATATYPE
         oError:SubCode     := 1020
         oError:Description := HB_LANGERRMSG( EG_DATATYPE ) + " [" + aWAData[ WA_STRUCT, nField, 1 ] +"]["+ cType + ":" + cT + "]"
         UR_SUPER_ERROR( nWA, oError )
         RETURN FAILURE
      ENDIF

      aWAData[ WA_ISHOT ] := .T.
      aWAData[ WA_BUFFER,nField ] := xValue

#ifdef __DEVELOPMENT__
      __rddDebug( nWA, "PUTVALUE", NIL, "<F:" + Str( nField, 4, 0 ) + ">", xValue )
#endif
      RETURN SUCCESS
   ENDIF

   RETURN FAILURE


// NOT a RDD METHOD
//
FUNCTION Cache_SetFieldBinary( nWA, cField, cBuffer )
   LOCAL i, cChunk
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nChunk  := 32000
   LOCAL nLen    := Len( cBuffer )
   LOCAL nPass   := ( nLen / nChunk )

   nPass := iif( nPass - Int( nPass ) > 0, nPass + 1, nPass )
   IF nPass < 2
      CacheSetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, cBuffer, nLen, 3 )
   ELSE
      CacheSetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, " ", 1, 0 )
      FOR i := 1 TO nPass
         cChunk := SubStr( cBuffer, ( ( i-1 ) * nChunk ) + 1, nChunk )
         CacheSetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, cChunk, Len( cChunk ), 1 )
      NEXT
      CacheSetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, " ", 1, 2 )
   ENDIF
   RETURN SUCCESS


// NOT a RDD METHOD
//
FUNCTION Cache_GetFieldBinary( nWA, cField )
   LOCAL i, nLen, nPass, ch
   LOCAL cChunk  := ''
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nChunk  := 32000

   nLen := Val( CacheGetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, 0 ) )
   IF nLen > 0
      nPass := ( nLen/nChunk )
      nPass := iif( nPass - Int( nPass ) > 0, nPass+1, nPass )

      FOR i := 1 TO nPass
         ch := CacheGetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, 1 )
         cChunk += ch
      NEXT
      CacheGetFieldBinary( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cField, 2 )
   ENDIF
   RETURN cChunk


STATIC FUNCTION CACHE_FLUSH( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nField, nRecord, oError, cError, cProp, n, cVal, aBuffer, nFields
   LOCAL cInfo   := ""
   LOCAL cFields := ""
   LOCAL nn      := 0
   LOCAL nTimes  := 0

   IF aWAData[ WA_ISHOT ]
      IF aWAData[ WA_RECNO ] > 0
         aBuffer := aWAData[ WA_BUFFER    ]
         nFields := aWAData[ WA_LENFIELDS ]

         IF aWAData[ WA_ISMEMO ]
            FOR nField := 1 TO nFields
               IF ! ( aBuffer[ nField ] == NIL )
                  IF aWAData[ WA_STRUCT, nField, 2 ] $ "MPB"
                     CACHE_SETFIELDBINARY( nWA, aWAData[ WA_STRUCT, nField, 5 ], aBuffer[ nField ] )
                  ELSE
                     cFields += LTrim( Str( nField, 4, 0 ) ) + ";"
                     nn++
                     cInfo += RddXtoS( aBuffer[ nField ] ) + "|~|"
                  ENDIF
               ENDIF
            NEXT
         ELSE
            FOR nField := 1 TO nFields
               IF ! ( aBuffer[ nField ] == NIL )
                  cFields += LTrim( Str( nField, 4, 0 ) ) + ";"
                  nn++
                  cInfo += RddXtoS( aBuffer[ nField ] ) + "|~|"
               ENDIF
            NEXT
         ENDIF

         IF nn > 0
            cInfo   := LTrim( Str( nn,5,0 ) ) + "|^|" + cFields + "|^|" + cInfo + "|^|"

            DO WHILE .T.
               nRecord := CacheSetFields( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], cInfo )
               IF nRecord == aWAData[ WA_RECNO ]
                  EXIT
               ENDIF
               IF ! Empty( cError := CacheGetLastError( nWA, aWAData[ WA_CONXN ] ) )
                  cVal := "" ; cProp := ""
                  IF ( "Error #7203:" $ cError )
                     IF ( n := At( "'", cError ) ) > 0
                        cVal := SubStr( cError, n + 1 )
                        cVal := SubStr( cVal, 1, At( "'", cVal ) - 1 )
                     ENDIF
                     IF ( n := At( "Error #5802:", cError ) ) > 0
                        cError := SubStr( cError, n )
                        IF ( n := At( "'", cError ) ) > 0
                           cError := SubStr( cError, n + 1 )
                           cError := SubStr( cError, At( ":", cError ) + 1 )
                           cProp  := SubStr( cError, 1, At( "'", cError ) - 1 )
                        ENDIF
                     ENDIF
                     oError := ErrorNew()
                     oError:GenCode     := 17011
                     oError:SubCode     := 17013
                     oError:Description := "Data Width Error: " + cProp + " : " + cVal
                     oError:FileName    := aWAData[ WA_OPENINFO, UR_OI_NAME ]
                     UR_SUPER_ERROR( nWA, oError )
                     RETURN FAILURE
                  ENDIF
               ENDIF
               inkey( 1 )
               IF ++nTimes > 4
                  oError := ErrorNew()
                  oError:GenCode     := 17011
                  oError:SubCode     := 17012
                  oError:Description := iif( Empty( cError ), "Failed to replace fields on the server!", "Failed to replace: " + cError )
                  oError:FileName    := aWAData[ WA_OPENINFO, UR_OI_NAME ]
                  UR_SUPER_ERROR( nWA, oError )
                  RETURN FAILURE
               ENDIF
            ENDDO
         ENDIF
      ENDIF

      afill( aWAData[ WA_BUFFER ], NIL )

#ifdef __DEVELOPMENT__
      __rddDebug( nWA, "FLUSH", NIL, Cache_SomeInfo( 60,"SetFields" ) )
#endif

      aWAData[ WA_ISMOVED ] := .T.
      aWAData[ WA_ISHOT   ] := .F.

      RETURN SUCCESS
   ENDIF

   RETURN FAILURE


FUNCTION CacheSetDebugOnError( cErrorDesc )
   LOCAL cError
   THREAD STATIC sError := ''

   cError := sError
   IF ValType( cErrorDesc ) == 'C'
      sError := cErrorDesc
   ENDIF
   RETURN cError


FUNCTION CacheSetLastInfo( aErrorDesc )
   LOCAL cError
   THREAD STATIC sError := {}

   cError := sError
   IF ValType( aErrorDesc ) == 'A'
      sError := aErrorDesc
   ENDIF
   RETURN cError


STATIC FUNCTION CACHE_GETVALUE( nWA, nField, xValue )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL n, xVal, yVal

   IF nField > 0 .AND. nField <= aWAData[ WA_LENFIELDS ]
      IF aWAData[ WA_ISHOT ]
         IF ! ( aWAData[ WA_BUFFER, nField ] == NIL )
            xValue := aWAData[ WA_BUFFER, nField ]
            RETURN SUCCESS
         ENDIF
      ENDIF

      IF ( aWAData[ WA_ISMOVED ] )
         afill( aWAData[ WA_PREVBUFFER ], NIL )
         aWAData[ WA_RECBUFFER ] := NIL
         aWAData[ WA_ISMOVED   ] := .F.
      ENDIF

      IF aWAData[ WA_RECNO ] == 0
         xValue := aWAData[ WA_EMPTYBUFFER,nField ]
      ELSE
         IF ! ( aWAData[ WA_PREVBUFFER,nField ] == NIL )
            xValue := aWAData[ WA_PREVBUFFER, nField ]
         ELSE
            IF aWAData[ WA_RECBUFFER ] == NIL
               xVal := CacheGetRecord( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ] )
               IF ! Empty( xVal )
                  n := At( "|~|", xVal )
                  yVal := SubStr( xVal, 1, n - 1 )
                  aWAData[ WA_FIELDOFFSET ] := hb_aTokens( yVal, ";" )
                  xVal := SubStr( xVal, n + 3 )
                  n := At( "|~|", xVal )
                  yVal := SubStr( xVal, 1, n - 1 )
                  aWAData[ WA_FIELDLEN ] := hb_aTokens( yVal, ";" )
                  aWAData[ WA_RECBUFFER ] := SubStr( xVal, n + 3 )
               ELSE
                  aWAData[ WA_FIELDOFFSET ] := {}
                  aWAData[ WA_RECBUFFER   ] := ""
                  aWAData[ WA_FIELDLEN    ] := ""
               ENDIF
            ENDIF

            IF Empty( aWAData[ WA_FIELDOFFSET ] ) .OR. aWAData[ WA_FIELDOFFSET, nField ] == ""
               aWAData[ WA_PREVBUFFER, nField ] := aWAData[ WA_EMPTYBUFFER, nField ]
            ELSE
               xVal := SubStr( @aWAData[ WA_RECBUFFER ], ;
                             Val( aWAData[ WA_FIELDOFFSET, nField ] ),;
                                 Val( aWAData[ WA_FIELDLEN, nField ] ) )

               aWAData[ WA_PREVBUFFER, nField ] := RddStoX( xVal, nField, aWAData[ WA_STRUCT ] )
            ENDIF
            xValue := aWAData[ WA_PREVBUFFER, nField ]

            //  Check for memo field and pull it only when it is requested and is not fetched previously
            //
            IF aWAData[ WA_ISMEMO ]
               IF aWAData[ WA_STRUCT, nField, 2 ] $ "MP" .AND. Empty( xValue )
                  aWAData[ WA_PREVBUFFER, nField ] := ;
                  Cache_GetFieldBinary( nWA, aWAData[ WA_STRUCT, nField, 5 ] )
                  xValue := aWAData[ WA_PREVBUFFER, nField ]
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      RETURN SUCCESS
   ENDIF
   RETURN FAILURE


STATIC FUNCTION CACHE_GOCOLD( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( aWAData )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "GOCOLD" )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_GOHOT( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( aWAData )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "GOHOT" )
#endif
   RETURN SUCCESS


#if 0
/* DBTRANSINFO
#define UR_TI_SRCAREA                           1
#define UR_TI_DSTAREA                           2
#define UR_TI_SCOPE                             3
#define UR_TI_FLAGS                             4
#define UR_TI_ITEMCOUNT                         5
#define UR_TI_ITEMS                             6

DBTRANSITEM
#define UR_TITEM_SOURCE                         1
#define UR_TITEM_DESTIN                         2
#define UR_TITEM_SIZE                           2
*/
STATIC FUNCTION CACHE_TRANS( nWA, aTransInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL i, a_

   HB_SYMBOL_UNUSED( aWAData )

   a_:= Array( aTransInfo[ UR_TI_ITEMCOUNT ] )
   Select( aTransInfo[ UR_TI_SRCAREA ] )
   DbGotop()
   DO WHILE ! Eof()
      //  Take Care of Scope

      FOR i := 1 TO aTransInfo[ UR_TI_ITEMCOUNT ]
         a_[ i ] := FieldGet( aTransInfo[ UR_TI_ITEMS, i, UR_TITEM_SOURCE ] )
      NEXT
      Select( aTransInfo[ UR_TI_DSTAREA ] )
      APPEND BLANK
      FOR i := 1 TO aTransInfo[ UR_TI_ITEMCOUNT ]
         FieldPut( aTransInfo[ UR_TI_ITEMS, i, UR_TITEM_DESTIN ], a_[ i ] )
      NEXT

      Select( aTransInfo[ UR_TI_SRCAREA ] )
      DbSkip()
   ENDDO
   DbCommit()

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "TRANS", aWAData[ WA_RECNO ] )
#endif
   RETURN SUCCESS
#endif


STATIC FUNCTION CACHE_DELETE( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oError

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   IF !( CACHE_ISRECLOCKED( nWA, aWAData, aWAData[ WA_RECNO ] ) )
      oError := ErrorNew()
      oError:GenCode     := EG_UNLOCKED
      oError:SubCode     := 1022
      oError:Description := HB_LANGERRMSG( EG_UNLOCKED )
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   CacheDelete( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ] )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "DELETE", aWAData[ WA_RECNO ], .T. )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_DELETED( nWA, lDeleted )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nDeleted

   IF aWAData[ WA_RECNO ] > 0
      nDeleted := CacheDeleted( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ] )
      lDeleted := ( nDeleted == 1 )

      IF ( nDeleted == 2 )
         USRRDD_SETEOF( nWA, .T. )
         USRRDD_SETBOTTOM( nWA, .F. )
      ENDIF
   ELSE
      lDeleted := .F.
   ENDIF
   RETURN SUCCESS


STATIC FUNCTION CACHE_ZAP( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL lSuccess, oError

   IF ! aWAData[ WA_OPENINFO, UR_OI_SHARED ] .OR. Cache_DbfData( nWA )[ DB_LOCKED ]
      lSuccess := CacheZap( aWAData[ WA_CONXN ], nWA )
   ELSE
      oError := ErrorNew()
      oError:GenCode     := EG_LOCK
      oError:SubCode     := EG_LOCK
      oError:Description := HB_LANGERRMSG( EG_LOCK ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + " - Database NotLocked!)"
      oError:FileName    := ''
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, 'ZAP', lSuccess )
#endif
   RETURN lSuccess

//----------------------------------------------------------------------//
//                   Record Identification Functions
//----------------------------------------------------------------------//

STATIC FUNCTION CACHE_RECNO( nWA, nRecNo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF aWAData[ WA_RECNO ] == 0
      nRecNo := LastRec() + 1
   ELSE
      nRecNo := aWAData[ WA_RECNO ]
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "RECNO", nRecNo )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_RECID( nWA, nRecNo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF aWAData[ WA_RECNO ] == 0
      nRecNo := CacheRecCount( aWAData[ WA_CONXN ], nWA ) + 1
   ELSE
      nRecNo := aWAData[ WA_RECNO ]
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"RECID", nRecNo )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_RECCOUNT( nWA, nRecords )
   nRecords := CacheRecCount( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"RECCOUNT", NIL, nRecords )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_RAWLOCK( nWA /*, aLock*/ )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL lLocked := CacheRecLock( aWAData[ WA_CONXN ], nWA )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"RAWLOCK", NIL, lLocked )
#endif
   RETURN lLocked


STATIC FUNCTION CACHE_LOCK( nWA, aLock )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! aWAData[ WA_OPENINFO ][ UR_OI_SHARED ]    /* Used Exclusive */
      aLock[ 3 ] := 1
      RETURN SUCCESS
   ENDIF

   IF Cache_DbfData( nWA )[ DB_LOCKED ]           /* FLocked()ed */
      aLock[ 3 ] := 1
      RETURN SUCCESS
   ENDIF

   IF aLock[ 2 ] == LOCK_RLOCK                    /* Record is already locked */
      IF HHasKey( aWAData[ WA_LOCKS ], aWAData[ WA_RECNO ] )
         aLock[ 3 ] := 1
         RETURN SUCCESS
      ENDIF
   ELSEIF aLock[ 2 ] == LOCK_DBRLOCK
      IF HHasKey( aWAData[ WA_LOCKS ], aLock[ 1 ] )
         aLock[ 3 ] := 1
         RETURN SUCCESS
      ENDIF
   ENDIF

   IF aLock[ 2 ] == LOCK_RLOCK
      aLock[ 3 ] := CacheRecLock( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ] )
      IF aLock[ 3 ] == 1
         HSet( aWAData[ WA_LOCKS ], aWAData[ WA_RECNO ], 1 )
      ENDIF

   ELSEIF aLock[ 2 ] == LOCK_DBRLOCK
      aLock[ 3 ] := CacheRecLock( aWAData[ WA_CONXN ], nWA, aLock[ 1 ] )
      IF aLock[ 3 ] == 1
         HSet( aWAData[ WA_LOCKS ], aLock[ 1 ], 1 )
      ENDIF

   ELSEIF aLock[ 2 ] == LOCK_FLOCK
      CACHE_UNLOCK( nWA, 0 )                      /*  Release all record locks */
      aWAData[ WA_LOCKS ] := Hash()
      IF ( aLock[ 3 ] := CacheFileLock( aWAData[ WA_CONXN ], nWA ) )
         Cache_DbfData( nWA )[ DB_LOCKED ] := .T.
      ENDIF
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,'LOCK', NIL, aLock[ 3 ] )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_UNLOCK( nWA,nRecord )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL lSuccess

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   // if used exclusive, nothing to unlock
   //
   IF ! aWAData[ WA_OPENINFO ][ UR_OI_SHARED ]
      RETURN SUCCESS
   ENDIF

   DEFAULT nRecord TO 0

   lSuccess := CacheUnlock( aWAData[ WA_CONXN ], nWA, nRecord ) == 1

   //  Make sure it is not
   IF nRecord == 0
      Cache_DbfData( nWA )[ DB_LOCKED ] := .F.
      aWAData[ WA_LOCKS ] := Hash()

   ELSEIF lSuccess
      // A potential source of error
      IF HHasKey( aWAData[ WA_LOCKS ], nRecord )
         HDel( aWAData[ WA_LOCKS ], nRecord )
      ENDIF
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"UNLOCK", NIL, nRecord, lSuccess )
#endif
   RETURN SUCCESS


//----------------------------------------------------------------------//
//                      Order Management Functions
//----------------------------------------------------------------------//
/*
#define UR_ORI_BAG                              1
#define UR_ORI_TAG                              2
#define UR_ORI_BLOCK                            3
#define UR_ORI_RESULT                           4
#define UR_ORI_NEWVAL                           5
#define UR_ORI_ALLTAGS                          6
*/
STATIC FUNCTION CACHE_ORDINFO( nWA, nInfo, aOrdInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL aOrder  := aWAData[ WA_ORDINFO ]
   LOCAL nIndex

   DO CASE
   CASE nInfo == DBOI_CONDITION                              //  1
      IF aOrdInfo[ 2 ] == NIL
         nIndex := aWAData[ WA_ORDER ]
      ELSE
         nIndex := aOrdInfo[ 2 ]
      ENDIF
      IF ( nIndex  > 0 .AND. nIndex <= Len( aWAData[ WA_ORDINFO ] ) )
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ nIndex, OI_CONDITION ]
      ENDIF

   CASE nInfo == DBOI_EXPRESSION                             //  2
      IF aOrdInfo[ 2 ] == NIL
         nIndex := aWAData[ WA_ORDER ]
      ELSE
         nIndex := aOrdInfo[ 2 ]
      ENDIF
      IF ( nIndex  > 0 .AND. nIndex <= Len( aWAData[ WA_ORDINFO ] ) )
         aOrdInfo[ UR_ORI_RESULT ] := Upper( aOrder[ nIndex, OI_EXPRESSION ] )
      ENDIF

   CASE nInfo == DBOI_POSITION                               //  3
      aOrdInfo[ UR_ORI_RESULT ] := Recno()                   // CacheOrdKeyNo( nWA )

   CASE nInfo == DBOI_RECNO                                  //  4
      aOrdInfo[ UR_ORI_RESULT ] := RecNo()                   // LastRec()

   CASE nInfo == DBOI_NAME                                   //  5
      IF aOrdInfo[ 2 ] == NIL
         nIndex := aWAData[ WA_ORDER ]
      ELSE
         nIndex := aOrdInfo[ 2 ]
      ENDIF
      IF ( nIndex  > 0 .AND. nIndex <= Len( aWAData[ WA_ORDINFO ] ) )
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ nIndex, OI_NAME ]
      ENDIF

   CASE nInfo == DBOI_NUMBER                                 //  6
      aOrdInfo[ UR_ORI_RESULT ] := aWAData[ WA_ORDER ]

   //CASE nInfo == DBOI_BAGNAME                              //  7
   //CASE nInfo == DBOI_BAGEXT                               //  8

   CASE nInfo == DBOI_ORDERCOUNT                             //  9
      aOrdInfo[ UR_ORI_RESULT ] := Len( aWAData[ WA_ORDINFO ] )

   //CASE nInfo == FILEHANDLE                                // 10
   CASE nInfo == DBOI_ISCOND                                 // 11
      IF aOrdInfo[ 2 ] == NIL
         nIndex := aWAData[ WA_ORDER ]
      ELSE
         nIndex := aOrdInfo[ 2 ]
      ENDIF
      IF ( nIndex  > 0 .AND. nIndex <= Len( aWAData[ WA_ORDINFO ] ) )
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ nIndex, OI_ISCOND ]
      ENDIF

   CASE nInfo == DBOI_KEYCOUNT                               // 26
      aOrdInfo[ UR_ORI_RESULT ] := LastRec() // CacheOrdKeyCount( aWAData[ WA_CONXN ], nWA )

   CASE nInfo == DBOI_SCOPETOP                               // 39
      IF aWAData[ WA_ORDER ] > 0
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ aWAData[ WA_ORDER ], OI_SCOPETOP ]
      ENDIF

      IF ! ( aOrdInfo[ UR_ORI_NEWVAL ] == NIL )
         IF aWAData[ WA_ORDER ] == 0
            Cache_Unsupported( nWA, "Workarea not Indexed" )
         ELSE
            aOrder[ aWAData[ WA_ORDER ], OI_SCOPETOP ] := ;
                         iif( ValType( aOrdInfo[ UR_ORI_NEWVAL ] ) == "B", ;
                              Eval( aOrdInfo[ UR_ORI_NEWVAL ] ), aOrdInfo[ UR_ORI_NEWVAL ] )
         ENDIF
      ENDIF

   CASE nInfo == DBOI_SCOPEBOTTOM                            // 40
      IF aWAData[ WA_ORDER ] > 0
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ]
      ENDIF

      IF ! ( aOrdInfo[ UR_ORI_NEWVAL ] == NIL )
         IF aWAData[ WA_ORDER ] == 0
            Cache_Unsupported( nWA, "Workarea not Indexed" )
         ELSE
            aOrder[ aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] := ;
                         iif( ValType( aOrdInfo[ UR_ORI_NEWVAL ] ) == 'B', ;
                              Eval( aOrdInfo[ UR_ORI_NEWVAL ] ), aOrdInfo[ UR_ORI_NEWVAL ] )
         ENDIF
      ENDIF

   CASE nInfo == DBOI_SCOPETOPCLEAR                          // 41
      IF aWAData[ WA_ORDER ] > 0 .AND. Len( aOrder ) >= aWAData[ WA_ORDER ]
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ aWAData[ WA_ORDER ], OI_SCOPETOP ]
         aOrder[ aWAData[ WA_ORDER ], OI_SCOPETOP    ] := NIL
      ENDIF

   CASE nInfo == DBOI_SCOPEBOTTOMCLEAR                       // 42
      IF aWAData[ WA_ORDER ] > 0 .AND. Len( aOrder ) >= aWAData[ WA_ORDER ]
         aOrdInfo[ UR_ORI_RESULT ] := aOrder[ aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ]
         aOrder[ aWAData[ WA_ORDER ], OI_SCOPEBOTTOM ] := NIL
      ENDIF
   ENDCASE

#ifdef __DEVELOPMENT__
   //__rddDebug( nWA, 'ORDINFO', NIL, nInfo )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_ORDLSTCLEAR( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aWAData[ WA_ORDINFO ] := {}
   aWAData[ WA_ORDER   ] := 0

   CacheSetGet( aWAData[ WA_CONXN ], nWA, 142, ' ' )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"ORDLSTCLEAR" )
#endif
   RETURN SUCCESS


STATIC FUNCTION CACHE_ORDLSTREBUILD( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL lRet

   lRet := CacheOrdListRebuild( aWAData[ WA_CONXN ], nWA, '' )

#ifdef __DEVELOPMENT__
   __rddDebug( nWA,"ORDLSTREBUILD",lRet,Cache_SomeInfo(60,"REBUILD") )
#endif
   RETURN lRet


STATIC FUNCTION CACHE_ORDLISTFOCUS( nWA, aOrdInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nIndex, cTag

   IF aWAData[ WA_ISHOT ]
      CACHE_FLUSH( nWA )
   ENDIF

   IF Len( aWAData[ WA_ORDINFO ] ) == 0
      RETURN SUCCESS
   ENDIF

   IF ValType( aOrdInfo[ 2 ] ) == "C"
      cTag := StrTran( Upper( aOrdInfo[ 2 ] ), "_", "" )
      nIndex := AScan( aWAData[ WA_ORDINFO ], {|e_| Upper( e_[ OI_NAME ] ) == cTag } )
   ELSE
      nIndex := aOrdInfo[ 2 ]
   ENDIF

   // Clipper compatibility - out of range order be turned TO 0
   // Instead we MUST raise R/T ERROR
   // 31 Jul 2008
   IF ! ( nIndex == NIL ) .AND. ( nIndex < 0 .OR. nIndex > Len( aWAData[ WA_ORDINFO ] ) )
      nIndex := 0
   ENDIF

   IF ! ( nIndex == NIL ) .AND. nIndex <= Len( aWAData[ WA_ORDINFO ] )
      IF ! ( aWAData[ WA_ORDER ] == nIndex )      /////////////// ????????????  //////////////
         CacheOrdListFocus( aWAData[ WA_CONXN ], nWA, aWAData[ WA_RECNO ], nIndex )

         IF nIndex > 0
            IF aWAData[ WA_ORDINFO, nIndex, OI_WHILEINDEX ]
               CACHE_GOTOP( nWA )
            ENDIF
            // Advantage behavior
            //
            IF ( aWAData[ WA_ORDINFO, nIndex, OI_ISCOND ] )
               CACHE_GOTOP( nWA )
            ENDIF
         ENDIF

         aWAData[ WA_ORDER ] := nIndex

#ifdef __DEVELOPMENT__
         IF ! ( ProcName( 1 ) == "CACHE_ORDLISTADD" )
            __rddDebug( nWA, "ORDLISTFOCUS", NIL, "<" + Str( nIndex, 2, 0 ) + "::" + Str( xRet, 2, 0 ) + ">", Cache_SomeInfo( 60, "OrdSetFocus" ) )
         ENDIF
#endif
      ENDIF

      RETURN SUCCESS
   ENDIF
   RETURN FAILURE


/*
#define UR_ORI_BAG                              1
#define UR_ORI_TAG                              2
#define UR_ORI_BLOCK                            3
#define UR_ORI_RESULT                           4
#define UR_ORI_NEWVAL                           5
#define UR_ORI_ALLTAGS                          6
*/
STATIC FUNCTION CACHE_ORDLISTADD( nWA, aOrdInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL cBagName, cInfo, s, cBagExt, cPath, n, aOrder
   LOCAL a_, x_:={}

   aOrder  := aWAData[ WA_ORDINFO ]

   IF Len( aOrder ) > 0
#ifdef __DEVELOPMENT__
      __rddDebug( nWA,"ORDLISTADD", aWAData[ WA_RECNO ], "........ Index Already Opened!" )
#endif
      RETURN SUCCESS
   ENDIF

   HB_FNAMESPLIT( aOrdInfo[ UR_ORI_BAG ], @cPath, @cBagName, @cBagExt )

   cBagName := StrTran( cBagName, "_", "" )

   CacheOrdListAdd( aWAData[ WA_CONXN ], nWA, Upper( cBagName ) )

   cInfo := CacheSetGet( aWAData[ WA_CONXN ], nWA, CACHE_LASTINFO, " " )

   IF ! Empty( cInfo )
      DO WHILE .T.
         IF ( n := At( "~", cInfo ) ) == 0
            EXIT
         ENDIF
         s := SubStr( cInfo, 1, n - 1 )
         cInfo := SubStr( cInfo, n + 1 )

         a_:={}
         DO WHILE .T.
            IF ( n := At( "^", s ) ) == 0
               EXIT
            ENDIF
            AAdd( a_, SubStr( s, 1, n - 1 ) )
            s := SubStr( s, n + 1 )
         ENDDO

         IF ! Empty( a_ )
            AAdd( x_, a_ )
         ENDIF
      ENDDO

      IF !Empty( x_ )
         FOR n := 1 TO Len( x_ )
            a_:= CACHE_ORDERDATAINIT()

            a_[ OI_CONDITION       ] := x_[ n, 5 ]
            a_[ OI_ISCOND          ] := ! Empty( x_[ n, 5 ] )
            a_[ OI_EXPRESSION      ] := x_[ n, 3 ]
            a_[ OI_NAME            ] := x_[ n, 2 ]
            a_[ OI_NUMBER          ] := n
            a_[ OI_BAGNAME         ] := aOrdInfo[ UR_ORI_BAG ]
            a_[ OI_BAGEXT          ] := cBagExt
            a_[ OI_ORDERCOUNT      ] := Len( x_ )
            a_[ OI_UNIQUE          ] := x_[ n, 4 ] == "T"
            a_[ OI_EXPRESSIONBLOCK ] := &( "{||" + x_[ n, 3 ] + "}" )
            a_[ OI_INDEXTYPE       ] := x_[ n, 6 ]
            a_[ OI_INDEXKEYSIZE    ] := Val( x_[ n, 7 ] )
            a_[ OI_WHILEINDEX      ] := x_[ n, 8 ] == "T"

            AAdd( aWAData[ WA_ORDINFO ], a_ )
         NEXT

#ifdef x__DEVELOPMENT__
         __rddDebug( nWA,'ORDLISTADD', NIL, Len( aOrder ) )
#endif
         SET ORDER TO 1
      ENDIF
   ENDIF
   RETURN SUCCESS


/*
#define UR_ORC_ACTIVE                           1
#define UR_ORC_CFOR                             2
#define UR_ORC_CWHILE                           3
#define UR_ORC_BFOR                             4
#define UR_ORC_BWHILE                           5
#define UR_ORC_BEVAL                            6
#define UR_ORC_STEP                             7
#define UR_ORC_STARTREC                         8
#define UR_ORC_NEXT                             9
#define UR_ORC_RECORD                          10
#define UR_ORC_REST                            11
#define UR_ORC_DESCEND                         12
#define UR_ORC_SCOPED                          13
#define UR_ORC_ALL                             14
#define UR_ORC_ADDITIVE                        15
#define UR_ORC_USECURRENT                      16
#define UR_ORC_CUSTOM                          17
#define UR_ORC_NOOPTIMIZE                      18
#define UR_ORC_COMPOUND                        19
#define UR_ORC_USEFILTER                       20
#define UR_ORC_TEMPORARY                       21
#define UR_ORC_EXCLUSIVE                       22
#define UR_ORC_CARGO                           23
*/
STATIC FUNCTION CACHE_ORDSETCOND( nWA, aOrdCond )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aWAData[ WA_ORDCOND ] := aOrdCond
   RETURN SUCCESS


/*
#define UR_ORCR_CONDINFO                        1
#define UR_ORCR_BAGNAME                         2
#define UR_ORCR_TAGNAME                         3
#define UR_ORCR_ORDER                           4
#define UR_ORCR_UNIQUE                          5
#define UR_ORCR_BKEY                            6
#define UR_ORCR_CKEY                            7
*/
STATIC FUNCTION CACHE_ORDCREATE( nWA, aOrdInfo )
   LOCAL cBagName, cIndexInfo, n, i, cTable, key_, aNames
   LOCAL cSql_1, cCondFor, cFor, cWhile, cCondWhile, cIdxKey
   LOCAL nRec, nRecNo, xIndex, bFor, nOrder, bWhile, lFunc
   LOCAL aWAData       := USRRDD_AREADATA( nWA )
   LOCAL cTable1       := aWAData[ WA_OPENINFO ][ UR_OI_NAME ]
   LOCAL cSchema       := aWAData[ WA_SCHEMA ]
   LOCAL cTag          := StrTran( Upper( aOrdInfo[ UR_ORCR_TAGNAME ] ), "_", "" )
   LOCAL cKey          := aOrdInfo[ UR_ORCR_CKEY ]
   LOCAL cUnique       := iif( aOrdInfo[ UR_ORCR_UNIQUE ], "T", "F" )
   LOCAL aStruct       := aWAData[ WA_STRUCT ]
   LOCAL cCacheField   := ""
   LOCAL cSql          := ""
   LOCAL cCompOnChange := ""
   LOCAL aComp         := {}
   LOCAL aCFor         := {}
   LOCAL lCalculated   := .F.
   LOCAL cField        := NIL
   LOCAL xIndexVal     := &( aOrdInfo[ UR_ORCR_CKEY ] )
   LOCAL cCollate      := ValType( xIndexVal )
   LOCAL nKeySize      := iif( cCollate == "C", Len( xIndexVal ), 0 )
   LOCAL cVVV          := iif( cCollate == "C", '""', iif( cCollate == "N", "-999999999999999", iif( cCollate == "D", "-1", "-1" ) ) )

   THREAD STATIC nTag := 0

   HB_FNAMESPLIT( Upper( cTable1 ), , @cTable )

   aNames   := CacheResolveNames( aOrdInfo[ UR_ORCR_BAGNAME ] )
   cBagName := aNames[ NME_CACHETABLE ]

   IF ! ( aWAData[ WA_ORDCOND ] == NIL ) .AND. ! Empty( aWAData[ WA_ORDCOND ][ UR_ORC_CWHILE ] )
      cWhile     := aWAData[ WA_ORDCOND ][ UR_ORC_CWHILE ]
      cCondWhile := __parseIndexExpression( cWhile, aStruct, @aCFor )
      cTag       := cBagName + cTag
      IF ! Empty( aWAData[ WA_ORDCOND ][ UR_ORC_CFOR ] )
         bFor := COMPILE( aWAData[ WA_ORDCOND ][ UR_ORC_CFOR ] )
      ELSE
         bFor := {|| .T. }
      ENDIF
   ELSE
      cCondWhile := ""
      cWhile     := ""
   ENDIF

   IF ! ( aWAData[ WA_ORDCOND ] == NIL ) .AND. ! Empty( aWAData[ WA_ORDCOND ][ UR_ORC_CFOR ] )
      aCFor    := {}
      cFor     := aWAData[ WA_ORDCOND ][ UR_ORC_CFOR ]
      cCondFor := __parseIndexExpression( cFor, aStruct, @aCFor )
   ELSE
      cCondFor := ""
      cFor     := ""
   ENDIF

   IF Empty( cTag )
      cTag := "T" + NTRIM( ++nTag )
   ENDIF

   IF ( n := At( "->", cKey ) ) > 0
      cKey := SubStr( cKey, n + 2 )
   ENDIF
   cIdxKey := cKey

   cSql_1 := ""
   lFunc  := At( "(", cKey ) > 0
   key_   := __parseExpression( cKey )

   IF Len( key_ ) > 1 .OR. lFunc .OR. ! Empty( cCondFor )
      lCalculated := .T.
      cCacheField := "Idx" + cTag
      cSql_1 := CRLF + "   Set xyz = " + __parseIndexExpression( cKey, aStruct, @aComp )
      IF ! Empty( cCondFor )
         cSql_1 += CRLF + "   Set xyz = $Select( " + cCondFor + ":xyz,1:" + cVVV + " )"
         FOR i := 1 TO Len( aCFor )
            IF AScan( aComp, aCFor[ i ] ) == 0
               AAdd( aComp, aCFor[ i ] )
            ENDIF
         NEXT
      ENDIF
      cSql += cSql_1 + CRLF + "   Set {" + cCacheField + "}=xyz" + CRLF + "   "

      AEval( aComp, {|e| cCompOnChange += StrTran( e, "_", "" ) + "," } )

      cCompOnChange := SubStr( cCompOnChange, 1, Len( cCompOnChange ) - 1 )
   ELSE
      cKey := Upper( AllTrim( key_[ 1, 2 ] ) )
      IF AScan( aStruct, {|e_| Upper( e_[ 1 ] ) == cKey } ) > 0
         cCacheField := cField
      ENDIF
      IF Empty( cCacheField )
         cCacheField := cKey
      ENDIF
      cCacheField := StrTran( cCacheField, "_", "" )
   ENDIF

   cCondFor := cFor

   cIndexInfo := cTable        + "~" +;
                 cBagName      + "~" +;
                 cTag          + "~" +;
                 cIdxKey       + "~" +;
                 cUnique       + "~" +;
                 cCacheField   + "~" +;
                 iif( lCalculated, "T", "F" ) + "~" + ;
                 cSql          + "~" +;
                 cCompOnChange + "~" +;
                 cSchema       + "~" +;
                 cCondFor      + "~" +;
                 cCollate      + "~" +;
                 cWhile        + "~" +;              // Raw as IN PRG
                 cCondWhile    + "~" +;              // Parsed TO qualify FOR ObjectScript
                 NTRIM( nKeySize ) + "~"

   CacheSetGet( aWAData[ WA_CONXN ], nWA, CACHE_CREATEINDEX, cIndexInfo )

   IF ! Empty( cWhile )
      nOrder := IndexOrd()
      nRec   := RecNo()
      bWhile := COMPILE( cWhile )

      DO WHILE .T.
         IF ! Eval( bWhile )
            EXIT
         ENDIF
         IF Eval( bFor )
            nRecNo := RecNo()
            xIndex := &cIdxKey.
            CacheSetGet( aWAData[ WA_CONXN ], nWA, 141, cTag + "~" + cCollate + "~" + NTRIM( nRecNo ) + "~" + RDDXtoS( xIndex ) + "~" )
         ENDIF
         dbskip()
      ENDDO

      aWAData[ WA_ORDINFO ] := {}
      aWAData[ WA_ORDER   ] := 0

      CACHE_ORDLISTADD( nWA, { aOrdInfo[ UR_ORCR_BAGNAME ], , , , , , } )

      DbGoto( nRec )
      DbSetOrder( nOrder )
   ELSE
      aWAData[ WA_ORDINFO ] := {}
      aWAData[ WA_ORDER   ] := 0
      CACHE_ORDLISTADD( nWA, { aOrdInfo[ UR_ORCR_BAGNAME ], , , , , , } )
   ENDIF

#ifdef __DEVELOPMENT__
   __rddDebug( nWA, "ORDCREATE", -99, cTag, Cache_SomeInfo( 60, "OrdCreate" ) )
#endif
   RETURN SUCCESS


STATIC FUNCTION __parseIndexExpression( cExp, aStr, aComp )
   LOCAL i, n, pPP

   FOR i := 1 TO 5
      cExp := __compressExpression( cExp )
   NEXT

   DO WHILE .T.
      IF ( n := At( ".not.", Lower( cExp ) ) ) == 0
         EXIT
      ENDIF
      cExp := SubStr( cExp, 1, n - 1 ) + "!" + SubStr( cExp, n + 5 )
   ENDDO

   //  Because Cache does not support <'> as a character delimiter
   //
   cExp := StrTran( cExp, "'" , '"' )
   cExp := StrTran( cExp, "!" , "~" )

   pPP := __PP_Init()
   __PP_AddRule( pPP, "#xtranslate type      => $$TY       "    )
   __PP_AddRule( pPP, "#xtranslate str     ( => $$STR^Rdd("     )
   __PP_AddRule( pPP, "#xtranslate substr  ( => $$SUBSTR^Rdd("  )
   __PP_AddRule( pPP, "#xtranslate dtos    ( => $$DTOS^Rdd("    )
   __PP_AddRule( pPP, "#xtranslate substr  ( => $$SUBSTR^Rdd("  )
   __PP_AddRule( pPP, "#xtranslate empty   ( => $$EMPTY^Rdd("   )
   __PP_AddRule( pPP, "#xtranslate if      ( => $$IF^Rdd("      )
   __PP_AddRule( pPP, "#xtranslate iif     ( => $$IIF^Rdd("     )
   __PP_AddRule( pPP, "#xtranslate reverse ( => $$REVERSE^Rdd(" )
   __PP_AddRule( pPP, "#xtranslate upper   ( => $$UPPER^Rdd("   )
   __PP_AddRule( pPP, "#xtranslate lower   ( => $$LOWER^Rdd("   )
   __PP_AddRule( pPP, "#xtranslate ctod    ( => $$CTOD^Rdd("    )
   __PP_AddRule( pPP, "#xtranslate seconds ( => $$SECONDS^Rdd(" )
   __PP_AddRule( pPP, "#xtranslate date    ( => $$DATE^Rdd("    )
   __PP_AddRule( pPP, "#xtranslate left    ( => $$LEFT^Rdd("    )
   __PP_AddRule( pPP, "#xtranslate strzero ( => $$STRZERO^Rdd(" )

   cExp :=__PP_Process( pPP, cExp )

   cExp := StrTran( cExp, "==", "=" )
   DO WHILE .T.
      IF ( n := At( ".and.", Lower( cExp ) ) ) == 0
         EXIT
      ENDIF
      cExp := SubStr( cExp, 1, n - 1 ) + "&&" + SubStr( cExp, n + 5 )
   ENDDO
   DO WHILE .T.
      IF ( n := At( ".or.", Lower( cExp ) ) ) == 0
         EXIT
      ENDIF
      cExp := SubStr( cExp, 1, n - 1 ) + "||" + SubStr( cExp, n + 4 )
   ENDDO

   cExp := StrTran( cExp, "$ $TY", "Type" )
   cExp := StrTran( cExp, "$$TY" , "Type" )
   cExp := StrTran( cExp, "~"    , "'"    )
   cExp := StrTran( cExp, "!"    , "'"    )

   IF ! ( aStr == NIL )
      FOR i := 1 TO Len( aStr )
         cExp := __parseByField( cExp, i, aStr, @aComp )
      NEXT
   ENDIF

   cExp := StrTran( cExp, "$ $" , "$$"  )
   cExp := StrTran( cExp, "+ "  , "+"   )
   cExp := StrTran( cExp, "+$$" , "_$$" )

   cExp := StrTran( cExp, "+{"  , "_{"  )
   cExp := StrTran( cExp, "}+"  , "}_"  )
   cExp := StrTran( cExp, ")+(" , ")_(" )

   RETURN cExp


STATIC FUNCTION __parseByField( cExp, nFld, aStr, aComp )
   LOCAL n, lOther, cF, cP, cP1, cType, lMulti, nLen, nNameLen, cFill, cRevs

   cF       := aStr[ nFld, 1 ]
   nNameLen := Len( cF )
   cFill    := Replicate( "^", Len( cF ) )
   cRevs    := Replicate( "~", Len( cF ) )
   cType    := aStr[ nFld,2 ]
   nLen     := aStr[ nFld,3 ]
   lMulti   := ( "+" $ cExp )

   DO WHILE .T.
      IF ( n := At( cF, Upper( cExp ) ) ) == 0
         EXIT
      ENDIF

      lOther := .F.
      cP     := SubStr( cExp, n - 1, 1 )
      cP1    := SubStr( cExp, n + nNameLen, 1 )

      IF ( ( n == 1 ) .OR. ( cP == "" ) .OR. ( cP $ " (,+-><=!/'" ) )
         IF AScan( aComp, cF ) == 0
            AAdd( aComp, cF )
         ENDIF

         DO CASE
         CASE IsAlpha( cP1 ) .OR. cP1 == "_" .OR. IsDigit( cP1 )
            lOther := .T.

         CASE n == 1
            IF lMulti
               IF cType == "C"
                  cExp := "$$PAD^Rdd(" + cFill + "," + NTRIM( nLen ) + ")" + SubStr( cExp, nNameLen + 1 )
               ELSE
                  cExp := cFill + SubStr( cExp, n + nNameLen )
               ENDIF
            ELSE
               cExp := cFill + SubStr( cExp, n + nNameLen )
            ENDIF

         CASE cP == "("                           //  Field - surrounded by ()
            IF SubStr( cExp, n - 5, 5 ) == "^Rdd("
               cExp := SubStr( cExp, 1, n - 1 ) + cFill + SubStr( cExp, n + nNameLen )
            ELSE
               IF lMulti
                  IF cType == "C"
                     cExp := SubStr( cExp, 1, n - 1 ) + "$$PAD^Rdd(" + cFill + "," + NTRIM( nLen ) + ")" + SubStr( cExp, n + nNameLen )
                  ELSE
                     cExp := SubStr( cExp, 1, n - 1 ) + cFill + SubStr( cExp, n + nNameLen )
                  ENDIF
               ELSE
                  cExp := SubStr( cExp, 1, n - 1 ) + cFill + SubStr( cExp, n + nNameLen )
               ENDIF
            ENDIF

         OTHERWISE
            IF lMulti
               IF cType == "C"
                  cExp := SubStr( cExp, 1, n - 1 ) + "$$PAD^Rdd(" + cFill + "," + NTRIM( nLen ) + ")" + SubStr( cExp, n + nNameLen )
               ELSE
                  cExp := SubStr( cExp, 1, n - 1 ) + cFill + SubStr( cExp, n + nNameLen )
               ENDIF
            ELSE
               cExp := SubStr( cExp, 1, n - 1 ) + cFill + SubStr( cExp, n + nNameLen )
            ENDIF
         ENDCASE
      ELSE
         lOther := .T.                            // subpart of another field
      ENDIF

      IF lOther
         cExp := SubStr( cExp, 1, n - 1 ) + cRevs + SubStr( cExp, n + nNameLen )
      ENDIF

      IF NextKey() == 27
         EXIT
      ENDIF
   ENDDO

   cExp := StrTran( cExp, cFill, "{" + cF + "}" )
   cExp := StrTran( cExp, cRevs, cF )
   cExp := StrTran( cExp, "^RDD", "^Rdd" )

   RETURN cExp


STATIC FUNCTION __compressExpression( cExp )

   cExp := StrTran( cExp, "( ", "(" )
   cExp := StrTran( cExp, " (", "(" )

   cExp := StrTran( cExp, ") ", ")" )
   cExp := StrTran( cExp, " )", ")" )

   cExp := StrTran( cExp, ", ", "," )
   cExp := StrTran( cExp, " ,", "," )

   cExp := StrTran( cExp, "+ ", "+" )
   cExp := StrTran( cExp, " +", "+" )

   cExp := StrTran( cExp, "- ", "-" )
   cExp := StrTran( cExp, " -", "-" )

   cExp := StrTran( cExp, "> ", ">" )
   cExp := StrTran( cExp, " >", ">" )

   cExp := StrTran( cExp, "< ", "<" )
   cExp := StrTran( cExp, " <", "<" )

   RETURN cExp


STATIC FUNCTION __parseExpression( cExp )
   LOCAL nBraces, nBrace1, nBrace2, i, j, n, s1, ss, f_, ee_, cChr
   LOCAL fn_:= {}

   ee_:= __parseByPlus( cExp )

   //  ee_ contains all expression components tokenized by PLUS +
   //  strip if first and last character is a brace
   //
   FOR i := 1 TO Len( ee_ )
      IF Left( ee_[ i ], 1 ) == "("
         IF Right( ee_[ i ], 1 ) == ")"
            ee_[ i ] := SubStr( ee_[ i ], 2, Len( ee_[ i ] ) - 2  )
         ENDIF
      ENDIF
   NEXT

   //  Check for how many function calls are there for a variable
   //
   FOR i := 1 TO Len( ee_ )
      cExp    := ee_[ i ]
      nBrace1 := {}
      nBrace2 := {}

      FOR j := 1 TO Len( cExp )
         cChr := SubStr( cExp, j, 1 )
         IF cChr == "("
            AAdd( nBrace1, j )
         ELSEIF cChr == ")"
            AAdd( nBrace2, j )
         ENDIF
      NEXT

      IF ( nBraces := Len( nBrace1 ) ) > 0        // there is function calls more than 0
         IF nBraces > 1
            n := 0
            f_:= {}
            FOR j := Len( nBrace1 ) TO 2 STEP -1
               n++
               s1 := SubStr( cExp, nBrace1[ j - 1 ] + 1, nBrace2[ n ] - nBrace1[ j - 1 ] )

               AAdd( f_, __parseFunc( s1 ) )

               ss   :=  "$" + Space( Len( s1 ) - 1 )
               cExp := StrTran( cExp, s1, ss )
            NEXT
            AAdd( f_, __parseFunc( cExp ) )
            AAdd( fn_, f_ )
         ELSE
            AAdd( fn_, __parseFunc( cExp ) )
         ENDIF
      ELSE
         AAdd( fn_, { "", cExp, NIL, NIL } )
      ENDIF
   NEXT

   RETURN fn_


STATIC FUNCTION __parseFunc( ccKey )
   LOCAL cField, n
   LOCAL p1     := ""
   LOCAL p2     := ""
   LOCAL cFunc  := ""

   IF ( n := At( "(", ccKey ) ) > 0
      cFunc := AllTrim( SubStr( ccKey, 1, n - 1 ) )

      ccKey := AllTrim( SubStr( ccKey, n + 1 ) )
      n := At( ")", ccKey )
      ccKey := LTrim( SubStr( ccKey, 1, n - 1 ) )

      IF ( n := At( ",", ccKey ) ) > 0
         cField := AllTrim( SubStr( ccKey, 1, n - 1 ) )
         ccKey  := LTrim( SubStr( ccKey, n + 1 ) )

         IF ( n := At( ",", ccKey ) ) > 0
            p1  := AllTrim( SubStr( ccKey, 1, n - 1 ) )
            p2  := AllTrim( SubStr( ccKey, n + 1 ) )
         ELSE
            p1  := ccKey
         ENDIF
      ELSE
         cField := ccKey
      ENDIF
   ELSE
      cField := AllTrim( SubStr( ccKey, n + 1 ) )
   ENDIF

   RETURN { cFunc, cField, p1, p2 }


STATIC FUNCTION __parseByPlus( cExp )
   LOCAL nAtPlus
   LOCAL e_:= {}

   DO WHILE .T.
      IF ( nAtPlus := At( "+", cExp ) ) == 0
         EXIT
      ENDIF
      AAdd( e_, AllTrim( SubStr( cExp, 1, nAtPlus - 1 ) ) )
      cExp := AllTrim( SubStr( cExp, nAtPlus + 1 ) )
   ENDDO
   AAdd( e_, AllTrim( cExp ) )

   RETURN e_


STATIC FUNCTION CACHE_GETSTRUCT( nWA )
   LOCAL aStr, n, s, cStr, nCol, cName, cCacheName, cType, cLen, nLen, nDec
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL aS := {}

   cStr := CacheSetGet( aWAData[ WA_CONXN ], nWA, CACHE_STRUCTURE )

   aStr := {}
   DO WHILE .T.
      IF ( n := At( "~", cStr ) ) == 0
         EXIT
      ENDIF
      s    := SubStr( cStr, 1, n - 1 )
      cStr := SubStr( cStr, n + 1 )

      nDec := 0
      nLen := 0

      IF !Empty( s )
         n := At( " ", s )
         nCol := Val( SubStr( s, 1, n - 1 ) )
         s := SubStr( s, n + 1 )

         n := At( " ", s )
         cName := SubStr( s, 1, n - 1 )
         s := SubStr( s, n + 1 )

         n := At( " ", s )
         cCacheName := SubStr( s, 1, n - 1 )
         s := SubStr( s, n + 1 )

         n := At( " ", s )
         cType := SubStr( s, 1, n - 1 )
         s := SubStr( s, n + 1 )

         n := At( " ",s )
         cLen := SubStr( s, 1, n - 1 )

         IF cType == "N" .OR. cType == "I"
            IF ( n := At( ".", cLen ) ) > 0
               nLen := Len( cLen )
               nDec := Len( SubStr( cLen, n + 1 ) )
            ELSE
               nLen := Len( cLen )
               nDec := 0
            ENDIF
         ELSE
            nLen := Val( cLen )
            nDec := 0
         ENDIF
      ENDIF

      AAdd( aStr, { cName, cType, nLen, nDec, cCacheName, nCol } )
   ENDDO

   AEval( aStr, {|e_,i| HB_SYMBOL_UNUSED( e_ ), aStr[ i,6 ]-- } )
   AEval( aStr, {|e_| iif( e_[ 6 ] > 0, AAdd( aS, e_ ), NIL ) } )

   ASort( aS, , , {|e_,f_| e_[ 6 ] < f_[ 6 ] } )
   RETURN aS


STATIC FUNCTION CACHE_DBFDATA( nWA )
   RETURN USRRDD_AREADATA( nWA )[ WA_DATABASE ]


STATIC FUNCTION CACHE_UNSUPPORTED( nWA, cMsg )
   LOCAL oError := ErrorNew()

   oError:GenCode     := EG_CREATE
   oError:SubCode     := 1004
   oError:Description := "Not Supported - " + cMsg
   oError:CanDefault  := .T.

   UR_SUPER_ERROR( nWA, oError )
   RETURN FAILURE


STATIC FUNCTION CACHE_CONNECTED( nWA )
   HB_SYMBOL_UNUSED( nWA )
   RETURN CacheSetConnection() > 0


STATIC FUNCTION RDDEmptyField( nField, aStr )

   SWITCH aStr[ nField, 2 ]
   CASE "C" ; RETURN Space( aStr[ nField, 3 ] )
   CASE "N" ; RETURN Val( Str( 0, aStr[ nField,3 ], aStr[ nField,4 ] ) )
   CASE "I" ; RETURN 0
   CASE "D" ; RETURN SToD( "" )
   CASE "L" ; RETURN .F.
   CASE "M" ; RETURN ""
   END
   RETURN NIL


FUNCTION RddXtoS( xVar )

   SWITCH ValType( xVar )
   CASE "C" ; RETURN Trim( xVar )
   CASE "N" ; RETURN iif( Empty( xVar ), "0", LTrim( Str( xVar ) ) )
   CASE "I" ; RETURN LTrim( Str( xVar ) )
   CASE "D" ; RETURN Trim( dtos( xVar ) )
   CASE "L" ; RETURN iif( xVar, "1", "0" )
   CASE "M" ; RETURN xVar
   END
   RETURN ""


FUNCTION RddXtoSI( xVar )

   SWITCH ValType( xVar )
   CASE "C" ; RETURN xVar
   CASE "N" ; RETURN iif( Empty( xVar ), "0", LTrim( Str( xVar ) ) )
   CASE "I" ; RETURN LTrim( Str( xVar ) )
   CASE "D" ; RETURN Trim( dtos( xVar ) )
   CASE "L" ; RETURN iif( xVar, "1", "0" )
   CASE "M" ; RETURN xVar
   END
   RETURN ""


FUNCTION RddStoX( xVar, nField, aStr )

   SWITCH aStr[ nField, 2 ]
   CASE "C" ; RETURN Pad( xVar,aStr[ nField,3 ] )
   CASE "N" ; RETURN Val( Str( Val( xVar ), aStr[ nField, 3 ], aStr[ nField, 4 ] ) )
   CASE "I" ; RETURN Val( PadL( xVar, aStr[ nField, 3 ] ) )
   CASE "D" ; RETURN SToD( xVar )
   CASE "L" ; RETURN iif( xVar == "1", .T., .F. )
   CASE "M" ; RETURN xVar
   END
   RETURN NIL


STATIC FUNCTION CacheGetRecLockList( nWA )
   LOCAL s, n
   LOCAL aRecLocks := {}

   IF ! Empty( s := CacheDbrLockList( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA ) )
      DO WHILE .T.
         IF ( n := At( " ", s ) ) == 0
            EXIT
         ENDIF
         AAdd( aRecLocks, Val( SubStr( s, 1, n - 1 ) ) )
         s := SubStr( s, n + 1 )
      ENDDO
   ENDIF
   RETURN aRecLocks


// NOT a RDD METHOD
//
FUNCTION CacheGotoPhantom( nWA )
   LOCAL aWAData

   DEFAULT nWA TO Select()

   aWAData := USRRDD_AREADATA( nWA )
   aWAData[ WA_RECNO ] := 0

   RETURN SUCCESS

//--------------------------------------------------------------------//
//                               RELATIONS
//--------------------------------------------------------------------//
#ifdef __RELATIONS__
/*
#define UR_RI_BEXPR                             1
#define UR_RI_CEXPR                             2
#define UR_RI_SCOPED                            3
#define UR_RI_OPTIMIZED                         4
#define UR_RI_PARENT                            5
#define UR_RI_CHILD                             6
#define UR_RI_NEXT                              7
#define UR_RI_SIZE                              7
*/
STATIC FUNCTION CACHE_CHILDEND( nWA, aRelInfo, x )

__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_CHILDSTART( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_CHILDSYNC( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_SYNCCHILDREN( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_CLEARREL( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_FORCEREL( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_RELAREA( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_RELEVAL( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_RELTEXT( nWA, nIndex, cResult )
__rddDebug( nWA )
   RETURN SUCCESS


STATIC FUNCTION CACHE_SETREL( nWA, aRelInfo, x )
__rddDebug( nWA )
   RETURN SUCCESS

#endif   __RELATIONS__
//----------------------------------------------------------------------//
