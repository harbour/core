/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ARRAY RDD
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://harbour-project.org
 * www - http://www.xharbour.org
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


/*
 * This is a Array RDD, or Memory RDD.
 * It works only in memory and actually supports standard dbf commands
 * excepts relations
 *
 * Index and Set Filter Support by Quique <quique@quiquesoft.com> - 2011
 *
 */

#include "rddsys.ch"
#include "hbusrrdd.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "dbinfo.ch"

#include "hbtrace.ch"

#xtranslate Throw( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )

ANNOUNCE ARRAYRDD

#define DATABASE_FILENAME    1
#define DATABASE_RECORDS     2
#define DATABASE_RECINFO     3
#define DATABASE_OPENNUMBER  4
#define DATABASE_LOCKED      5
#define DATABASE_STRUCT      6
#define DATABASE_INDEX       7
#define DATABASE_SIZEOF      7

#define RDDDATA_DATABASE     1
#define RDDDATA_SIZEOF       1

#define WADATA_DATABASE      1
#define WADATA_WORKAREA      2
#define WADATA_OPENINFO      3
#define WADATA_RECNO         4
#define WADATA_BOF           5
#define WADATA_FORCEBOF      6
#define WADATA_EOF           7
#define WADATA_TOP           8
#define WADATA_BOTTOM        9
#define WADATA_FOUND        10
#define WADATA_LOCKS        11
#define WADATA_INDEX        12
#define WADATA_WAORDINFO    13
#define WADATA_ORDRECNO     14
#define WADATA_FILTERINFO   15
#define WADATA_SIZEOF       15

#define RECDATA_DELETED      1
#define RECDATA_LOCKED       2
#define RECDATA_SIZEOF       2

#define INDEX_TAG            1
#define INDEX_ORCR           2
#define INDEX_RECORDS        3
#define INDEX_SIZEOF         3

#define INDEXKEY_KEY         1
#define INDEXKEY_RECORD      2
#define INDEXKEY_SIZEOF      2

#define WAOI_SCOPE_0         1
#define WAOI_SCOPE_1         2
#define WAOI_SIZEOF          2

STATIC s_nRddID := -1

/*
 * non work area methods receive RDD ID as first parameter
 * Methods INIT and EXIT does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */

STATIC FUNCTION AR_INIT( nRDD )

   /* Init DBF Hash */

   USRRDD_RDDDATA( nRDD, { => } )

   RETURN HB_SUCCESS

#if 0

STATIC FUNCTION AR_RDDDATAINIT()

   RETURN { ;
      NIL     ; /* RDDDATA_DATABASE */
   }

#endif

STATIC FUNCTION AR_DATABASEINIT()

   RETURN { ;
      NIL, ;  /* DATABASE_FILENAME */
      {}, ;   /* DATABASE_RECORDS */
      {}, ;   /* DATABASE_RECINFO */
      0, ;    /* DATABASE_OPENNUMBER */
      .F., ;  /* DATABASE_LOCKED */
      NIL, ;  /* DATABASE_STRUCT - aStruct */
      {} ;    /* DATABASE_INDEX */
   }

STATIC FUNCTION AR_WADATAINIT()

   RETURN { ;
      NIL, ; /* WADATA_DATABASE */
      0, ;   /* WADATA_WORKAREA */
      NIL, ; /* WADATA_OPENINFO */
      0, ;   /* WADATA_RECNO */
      .F., ; /* WADATA_BOF */
      .F., ; /* WADATA_FORCEBOF - to solve an hack in dbf1.c */
      .F., ; /* WADATA_EOF */
      .F., ; /* WADATA_TOP */
      .F., ; /* WADATA_BOTTOM */
      .F., ; /* WADATA_FOUND */
      {}, ;  /* WADATA_LOCKS */
      0, ;   /* WADATA_INDEX */
      {}, ;  /* WADATA_WAORDINFO */
      0, ;   /* WADATA_ORDRECNO */
      NIL;   /* WADATA_FILTERINFO */
   }

STATIC FUNCTION AR_RECDATAINIT()

   RETURN { ;
      .F., ; /* RECDATA_DELETED */
      0;     /* RECDATA_LOCKED - which work area lock? */
   }

STATIC FUNCTION AR_INDEXINIT()

   RETURN { ;
      NIL, ; /* INDEX_TAG */
      NIL, ; /* INDEX_ORCR */
      {}   ; /* INDEX_RECORDS */
   }

STATIC FUNCTION AR_INDEXKEYINIT()

   RETURN { ;
      NIL, ; /* INDEXKEY_KEY */
      NIL ;  /* INDEXKEY_RECORD */
   }

STATIC FUNCTION AR_WAOIINIT()

   RETURN { ;
      NIL, ; /* WAOI_SCOPE_0 */
      NIL ;  /* WAOI_SCOPE_1 */
   }

/*
 * methods: NEW and RELEASE receive pointer to work area structure
 * not work area number. It's necessary because the can be executed
 * before work area is allocated
 * these methods does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */

STATIC FUNCTION AR_NEW( pWA )

   /*
    * Set in our private AREA item the array with slot number and
    * BOF/EOF flags. There is no BOF support in HB_F* function so
    * we have to emulate it and there is no phantom record so we
    * cannot return EOF flag directly.
    */

   USRRDD_AREADATA( pWA, AR_WADATAINIT() )

   RETURN HB_SUCCESS

/* Creating fields for new DBF - dbCreate() in current workarea */

STATIC FUNCTION AR_CREATEFIELDS( nWA, aStruct )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL nResult   := HB_SUCCESS
   LOCAL aFieldStruct, aField

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aStruct = %2$s", nWA, hb_ValToExp( aStruct ) ) )

   /* Setting WA number to current WorkArea */
   aWAData[ WADATA_WORKAREA ] := nWA

   /* Create new file data structure - workarea uses a reference to database */
   aWAData[ WADATA_DATABASE ] := AR_DATABASEINIT()

   /* Store DBF Structure */
   aWAData[ WADATA_DATABASE ][ DATABASE_STRUCT ] := aStruct

   /* Set fields */
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct
      aFieldStruct[ DBS_NAME ] := Upper( aFieldStruct[ DBS_NAME ] )
      aFieldStruct[ DBS_TYPE ] := Upper( aFieldStruct[ DBS_TYPE ] )

      aField := Array( UR_FI_SIZE )
      aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
      aField[ UR_FI_TYPE ]    := hb_Decode( aFieldStruct[ DBS_TYPE ], "C", HB_FT_STRING, "L", HB_FT_LOGICAL, "M", HB_FT_MEMO, "D", HB_FT_DATE, "N", iif( aFieldStruct[ DBS_DEC ] > 0, HB_FT_DOUBLE, HB_FT_INTEGER ) )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
      aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
      UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   RETURN nResult

/* Create database from current WA fields definition */

STATIC FUNCTION AR_CREATE( nWA, aOpenInfo )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL hRDDData  := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL cName
   LOCAL cFullName, aDBFData, nResult /*, aFieldStruct, aField, aStruct */

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aOpenInfo = %2$s", nWA, hb_ValToExp( aOpenInfo ) ) )

   /* getting database infos from current workarea */
   aDBFData  := aWAData[ WADATA_DATABASE ]

   /* setting in uppercase chars to avoid differences */
   cFullName := Upper( aOpenInfo[ UR_OI_NAME ] )

   /* When there is no ALIAS we will create new one using file name */
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      hb_FNameSplit( cFullName,, @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   ENDIF

   /* Check if database is already present in memory slots */
   /*
      07/11/2008 FSG - dbCreate() doesn't check if a dbf file exists. So I will not check it.
      If you need to check if a table exists use hb_FileArrayRdd() function that works in
      similar way of File(), i.e.:
      IF hb_FileArrayRdd( cFullName )
         dbCreate( cFullName, aStructure, "ARRAYRDD" )
         ....
   */

   /* Setting file attribs */
   aDBFData[ DATABASE_FILENAME ] := cFullName
   aDBFData[ DATABASE_LOCKED   ] := .T.      /* I need Exclusive mode in creation */

   /* Adding new database in RDD memory slots using filename as key */
   hb_HSet( hRDDData, cFullName, aDBFData )

   /* Set WorkArea Info */
   aWAData[ WADATA_WORKAREA ] := nWA
   aWAData[ WADATA_OPENINFO ] := aOpenInfo /* Put open informations */

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   IF nResult == HB_SUCCESS
      /* Add a new open number */
      aDBFData[ DATABASE_OPENNUMBER ]++
      /* default values for Records == 0 */
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_OPEN( nWA, aOpenInfo )

   LOCAL cFullName, cName, hRDDData, aWAData, aDBFData
   LOCAL aStruct, oError, aFieldStruct, aField, nResult

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aOpenInfo = %2$s", nWA, hb_ValToExp( aOpenInfo ) ) )

   cFullName := Upper( aOpenInfo[ UR_OI_NAME ] )

   IF Right( cFullName, 1 ) == "."
      cFullName := Left( cFullName, Len( cFullName ) - 1 )
   ENDIF

   /* When there is no ALIAS we will create new one using file name */
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      hb_FNameSplit( cFullName,, @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   ENDIF

   hRDDData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )

   IF hb_HHasKey( hRDDData, cFullName )
      aDBFData := hRDDData[ cFullName ]
      aStruct  := aDBFData[ DATABASE_STRUCT ]

   ELSE
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := hb_langErrMsg( EG_OPEN ) + ", memory file not found"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      NetErr( .T. )
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   /* Set WorkArea Infos */
   aWAData  := USRRDD_AREADATA( nWA )
   aWAData[ WADATA_DATABASE ] := aDBFData  /* Put a reference to database */
   aWAData[ WADATA_WORKAREA ] := nWA
   aWAData[ WADATA_OPENINFO ] := aOpenInfo /* Put open informations       */

   /* Set fields */
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct
      aField := Array( UR_FI_SIZE )
      aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
      aField[ UR_FI_TYPE ]    := hb_Decode( aFieldStruct[ DBS_TYPE ], "C", HB_FT_STRING, "L", HB_FT_LOGICAL, "M", HB_FT_MEMO, "D", HB_FT_DATE, "N", iif( aFieldStruct[ DBS_DEC ] > 0, HB_FT_DOUBLE, HB_FT_INTEGER ) )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
      aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
      UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   /* Add a new open number */
   aDBFData[ DATABASE_OPENNUMBER ]++

   /* File already opened in exclusive mode */
   /* I have to do this check here because, in case of error, AR_CLOSE() is called however */
   IF aDBFData[ DATABASE_LOCKED ]
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := hb_langErrMsg( EG_OPEN ) + "(" + ;
         hb_langErrMsg( EG_LOCK ) + " - already opened in exclusive mode)"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      NetErr( .T. )
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   /* Open file in exclusive mode */
   IF ! aOpenInfo[ UR_OI_SHARED ]
      IF aDBFData[ DATABASE_OPENNUMBER ] == 1
         aDBFData[ DATABASE_LOCKED     ] := .T.

      ELSE
         oError := ErrorNew()
         oError:GenCode     := EG_OPEN
         oError:SubCode     := 1000
         oError:Description := hb_langErrMsg( EG_OPEN ) + "(" + ;
            hb_langErrMsg( EG_LOCK ) + " - already opened in shared mode)"
         oError:FileName    := aOpenInfo[ UR_OI_NAME ]
         oError:CanDefault  := .T.
         UR_SUPER_ERROR( nWA, oError )
         NetErr( .T. )
         RETURN HB_FAILURE

      ENDIF
   ENDIF

   IF nResult == HB_SUCCESS
      NetErr( .F. )
      AR_GOTOP( nWA )
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_CLOSE( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF HB_ISARRAY( aDBFData )
      /* decrease open number */
      aDBFData[ DATABASE_OPENNUMBER ]--

      /* unlock file */
      aDBFData[ DATABASE_LOCKED     ] := .F.  /* Exclusive mode */
   ENDIF

   RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION AR_GETVALUE( nWA, nField, xValue )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aStruct   := aDBFData[ DATABASE_STRUCT  ]
   LOCAL nRecNo    := aWAData[ WADATA_RECNO ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nField = %2$d, xValue = %3$s", nWA, nField, hb_ValToExp( xValue ) ) )

   IF nField > 0 .AND. nField <= Len( aStruct )
      IF aWAData[ WADATA_EOF ]
         /* We are at EOF position, return empty value */
         xValue := EmptyValue( aStruct[ nField ][ DBS_TYPE ], aStruct[ nField ][ DBS_LEN ], aStruct[ nField ][ DBS_DEC ] )
      ELSE
         xValue := aRecords[ nRecNo ][ nField ]
      ENDIF
      RETURN HB_SUCCESS
   ENDIF

   RETURN HB_FAILURE

STATIC FUNCTION AR_PUTVALUE( nWA, nField, xValue )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aStruct   := aDBFData[ DATABASE_STRUCT  ]
   LOCAL nRecNo    := aWAData[ WADATA_RECNO ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL aKeys[ Len( aIndexes ) ]
   LOCAL xVal

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nField = %2$d, xValue = %3$s", nWA, nField, hb_ValToExp( xValue ) ) )

   IF nField > 0 .AND. nField <= Len( aStruct ) .AND. ;
      iif( HB_ISSTRING( xValue ) .AND. aStruct[ nField ][ DBS_TYPE ] == "M", .T., ValType( xValue ) == aStruct[ nField ][ DBS_TYPE ] )

      xVal := PutValue( xValue, aStruct[ nField ][ DBS_TYPE ], aStruct[ nField ][ DBS_LEN ], aStruct[ nField ][ DBS_DEC ] )

      AEval( aIndexes, {| aInd, n | aKeys[ n ] := Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ) } )

      IF ! aWAData[ WADATA_EOF ]
         aRecords[ nRecNo ][ nField ] := xVal
      ENDIF

      AEval( aIndexes, {| aInd, n | ModifyIndex( n, Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ), aInd, aWAData, aKeys[ n ] ) } )

      RETURN HB_SUCCESS

   ENDIF

   RETURN HB_FAILURE

STATIC FUNCTION AR_GOTO( nWA, nRecord )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL nRecCount := Len( aRecords )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecord = %2$d, nRecCount = %3$d", nWA, nRecord, nRecCount ) )

   IF nRecord >= 1 .AND. nRecord <= nRecCount
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .F.
      aWAData[ WADATA_RECNO ] := nRecord

   ELSEIF nRecCount == 0
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1

   ELSEIF nRecord < 0
      aWAData[ WADATA_BOF ]   := .T.
      aWAData[ WADATA_EOF ]   := .F.
      aWAData[ WADATA_RECNO ] := 1

   ELSEIF nRecord == 0 .OR. nRecord > nRecCount
      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .T.
      aWAData[ WADATA_RECNO ] := nRecCount + 1

   ENDIF

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "aWAData[ WADATA_BOF ] = %1$s, aWAData[ WADATA_EOF ] = %2$s, aWAData[ WADATA_RECNO ] = %3$d", ;
      hb_ValToExp( aWAData[ WADATA_BOF ] ), hb_ValToExp( aWAData[ WADATA_EOF ] ), aWAData[ WADATA_RECNO ] ) )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_GOTOID( nWA, nRecord )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecord = %2$d", nWA, nRecord ) )

   RETURN AR_GOTO( nWA, nRecord )

STATIC FUNCTION AR_GOTOP( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL nRecCount := Len( aRecords )
   LOCAL nIndex    := aWAData[ WADATA_INDEX ]
   LOCAL nResult   := HB_SUCCESS

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF nRecCount == 0
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1
   ELSE
      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .F.
      IF nIndex == 0
         aWAData[ WADATA_RECNO ] := 1
      ELSEIF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] == NIL
         IF Empty( aIndexes[ nIndex, INDEX_RECORDS ] )
            aWAData[ WADATA_ORDRECNO ] := 0
            nResult := AR_GOTO( nWA, 0 )
         ELSE
            aWAData[ WADATA_ORDRECNO ] := 1
            nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, 1, INDEXKEY_RECORD ] )
         ENDIF
      ELSE
         aWAData[ WADATA_ORDRECNO ] := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .F. )
         nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_RECORD ] )
      ENDIF

      IF Set( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
         RETURN AR_SKIPFILTER( nWA, 1 )
      ENDIF
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_GOBOTTOM( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL nIndex    := aWAData[ WADATA_INDEX ]
   LOCAL nResult   := HB_SUCCESS

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF Len( aRecords ) == 0
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1
   ELSE
      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .F.
      IF nIndex == 0
         aWAData[ WADATA_RECNO ] := Len( aRecords )
      ELSEIF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] == NIL
         IF Empty( aIndexes[ nIndex, INDEX_RECORDS ] )
            aWAData[ WADATA_ORDRECNO ] := 0
            nResult := AR_GOTO( nWA, 0 )
         ELSE
            aWAData[ WADATA_ORDRECNO ] := Len( ATail( aIndexes[ nIndex, INDEX_RECORDS ] ) )
            nResult := AR_GOTO( nWA, ATail( aIndexes[ nIndex, INDEX_RECORDS ] )[ INDEXKEY_RECORD ] )
         ENDIF
      ELSE
         aWAData[ WADATA_ORDRECNO ] := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .T. )
         nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_RECORD ] )
      ENDIF

      IF Set( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
         RETURN AR_SKIPFILTER( nWA, -1 )
      ENDIF

   ENDIF

   RETURN nResult

STATIC FUNCTION AR_SETFILTER( nWa, aDbFilterInfo )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aDbFilterInfo = %2$s", nWA, hb_ValToExp( aDbFilterInfo ) ) )

   USRRDD_AREADATA( nWA )[ WADATA_FILTERINFO ] := aDbFilterInfo

   RETURN SUCCESS

STATIC FUNCTION AR_CLEARFILTER( nWA )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   USRRDD_AREADATA( nWA )[ WADATA_FILTERINFO ] := NIL

   RETURN SUCCESS

STATIC FUNCTION AR_SKIPFILTER( nWA, nRecords )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL lBof, nToSkip

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecords = %2$d", nWA, nRecords ) )

   nToSkip := iif( nRecords > 0, 1, iif( nRecords < 0, -1, 0 ) )

   IF nToSkip != 0
      DO WHILE ! aWAData[ WADATA_BOF ] .AND. ! aWAData[ WADATA_EOF ]
         IF ( Set( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ] ) .OR. ;
               ( aWAData[ WADATA_FILTERINFO ] != NIL .AND. ! Eval( aWAData[ WADATA_FILTERINFO, UR_FRI_BEXPR ] ) )
            IF !( AR_SKIPRAW( nWA, nToSkip ) == HB_SUCCESS )
               RETURN HB_FAILURE
            ENDIF
            IF nToSkip < 0 .AND. aWAData[ WADATA_BOF ]
               lBof := .T.
               aWAData[ WADATA_BOF ] := .F.
               nToSkip := 1
            ELSEIF nToSkip > 0 .AND. aWAData[ WADATA_EOF ]
               EXIT
            ENDIF
            LOOP
         ENDIF

         /* FILTERS */
         EXIT
      ENDDO

      IF lBof != NIL
         aWAData[ WADATA_BOF ] := .T.
      ENDIF

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_SKIPRAW( nWA, nRecords )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL nIndex   := aWAData[ WADATA_INDEX ]
   LOCAL aIndexes := aWAData[ WADATA_DATABASE, DATABASE_INDEX ]
   LOCAL lBof, lEof
   LOCAL nResult, nRec, nEnd, lScope0, lScope1
   LOCAL nIni := 0

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecords = %2$d", nWA, nRecords ) )

   IF nRecords == 0
      lBof := aWAData[ WADATA_BOF ]
      lEof := aWAData[ WADATA_EOF ]

      nResult := AR_GOTO( nWA, aWAData[ WADATA_RECNO ] )

      aWAData[ WADATA_BOF ] := lBof
      aWAData[ WADATA_EOF ] := lEof

   ELSEIF nIndex > 0
      nRec    := ordKeyNo()
      lScope0 := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] != NIL
      lScope1 := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] != NIL
      nEnd := ordKeyCount()
      IF nRec == 0
         nRec := nEnd + 1
      ENDIF
      IF lScope0
         nIni := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .F. )
         nIni--
      ENDIF
      IF nIni == -1 .OR. Empty( aIndexes[ nIndex, INDEX_RECORDS ] )
         nResult := AR_GOTO( nWA, 0 )
         aWAData[ WADATA_ORDRECNO ] := 0
      ELSEIF nRecords < 0 .AND. -nRecords >= nRec
         nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, nIni + 1, INDEXKEY_RECORD ] )
         aWAData[ WADATA_ORDRECNO ] := 1
         aWAData[ WADATA_BOF ]      := .T.
      ELSEIF nRecords > 0 .AND. nRec + nRecords > nEnd
         nResult := AR_GOTO( nWA, 0 )
         aWAData[ WADATA_ORDRECNO ] := 0
      ELSE
         nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, nRec + nRecords + nIni, INDEXKEY_RECORD ] )
         aWAData[ WADATA_ORDRECNO ] := nRec + nRecords + nIni
         IF aIndexes[ nIndex, INDEX_ORCR, UR_ORCR_CONDINFO, UR_ORC_DESCEND ]
            IF nRecords < 0
               IF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] != NIL .AND. aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] < aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_KEY ]
                  nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, 1, INDEXKEY_RECORD ] )
                  aWAData[ WADATA_ORDRECNO ] := 1
                  aWAData[ WADATA_BOF ]      := .T.
               ENDIF
            ELSEIF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] != NIL .AND. aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] > aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_KEY ]
               nResult := AR_GOTO( nWA, 0 )
               aWAData[ WADATA_ORDRECNO ] := 0
            ENDIF
         ELSEIF lScope0 .AND. ! aIndexes[ nIndex, INDEX_RECORDS, nRec + nRecords + nIni, INDEXKEY_KEY ] >= aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] .OR. lScope1 .AND. ! aIndexes[ nIndex, INDEX_RECORDS, nRec + nRecords + nIni, INDEXKEY_KEY ] <= aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ]
            IF nRecords < 0
               IF aIndexes[ nIndex, INDEX_RECORDS, nIni + 1, INDEXKEY_KEY ]
               ENDIF
               aWAData[ WADATA_ORDRECNO ] := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .F. )
               aWAData[ WADATA_BOF ]      := aWAData[ WADATA_EOF ]
            ELSE
               nResult := AR_GOTO( nWA, 0 )
               aWAData[ WADATA_ORDRECNO ] := 0
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nRecords < 0 .AND. -nRecords >= aWAData[ WADATA_RECNO ]
      nResult := AR_GOTO( nWA, 1 )
      aWAData[ WADATA_BOF ]   := .T.
   ELSE
      nResult := AR_GOTO( nWA, aWAData[ WADATA_RECNO ] + nRecords )
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_BOF( nWA, lBof )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, lBof = %2$s", nWA, hb_ValToExp( lBof ) ) )

   /* This is a hack to protect from dbf1.c skipraw hack */
   IF aWAData[ WADATA_FORCEBOF ] .AND. lBof
      aWAData[ WADATA_BOF ] := lBof
      aWAData[ WADATA_FORCEBOF ] := .F.
   ELSE
      lBof := aWAData[ WADATA_BOF ]
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_EOF( nWA, lEof )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, lEof = %2$s", nWA, hb_ValToExp( lEof ) ) )

   lEof := aWAData[ WADATA_EOF ]

   RETURN HB_SUCCESS

STATIC FUNCTION AR_DELETE( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL aKeys[ Len( aIndexes ) ]
   LOCAL oError

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF aOpenInfo[ UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025                /* EDBF_READONLY */
      oError:Description := hb_langErrMsg( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF

   IF ! aWAData[ WADATA_EOF ]
      IF aOpenInfo[ UR_OI_SHARED ] .AND. AScan( aWAData[ WADATA_LOCKS ], aWAData[ WADATA_RECNO ] ) == 0

         oError := ErrorNew()
         oError:GenCode     := EG_UNLOCKED
         oError:SubCode     := 1022            /* EDBF_UNLOCKED */
         oError:Description := hb_langErrMsg( EG_UNLOCKED )
         oError:FileName    := aOpenInfo[ UR_OI_NAME ]
         UR_SUPER_ERROR( nWA, oError )
         RETURN HB_FAILURE

      ENDIF

      IF Len( aRecInfo ) > 0 .AND. aWAData[ WADATA_RECNO ] <= Len( aRecInfo )
         AEval( aIndexes, {| aInd, n | aKeys[ n ] := Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ) } )

         aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ] := .T.

         AEval( aIndexes, {| aInd, n | ModifyIndex( n, Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ), aInd, aWAData, aKeys[ n ] ) } )

      ENDIF

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_DELETED( nWA, lDeleted )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, lDeleted = %2$s", nWA, hb_ValToExp( lDeleted ) ) )

   IF Len( aRecInfo ) > 0 .AND. aWAData[ WADATA_RECNO ] <= Len( aRecInfo )
      lDeleted := aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
   ELSE
      lDeleted := .F.
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_RECALL( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL aKeys[ Len( aIndexes ) ]
   LOCAL oError

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF aOpenInfo[ UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 /* EDBF_READONLY */
      oError:Description := hb_langErrMsg( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF

   IF ! aWAData[ WADATA_EOF ]
      IF aOpenInfo[ UR_OI_SHARED ] .AND. AScan( aWAData[ WADATA_LOCKS ], aWAData[ WADATA_RECNO ] ) == 0
         oError := ErrorNew()
         oError:GenCode     := EG_UNLOCKED
         oError:SubCode     := 1022 /* EDBF_UNLOCKED */
         oError:Description := hb_langErrMsg( EG_UNLOCKED )
         oError:FileName    := aOpenInfo[ UR_OI_NAME ]
         UR_SUPER_ERROR( nWA, oError )
         RETURN HB_FAILURE
      ENDIF

      IF Len( aRecInfo ) > 0 .AND. aWAData[ WADATA_RECNO ] <= Len( aRecInfo )
         AEval( aIndexes, {| aInd, n | aKeys[ n ] := Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ) } )
         aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ] := .F.
         AEval( aIndexes, {| aInd, n | ModifyIndex( n, Eval( aInd[ INDEX_ORCR, UR_ORCR_BKEY ] ), aInd, aWAData, aKeys[ n ] ) } )
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_APPEND( nWA, nRecords )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aStruct   := aDBFData[ DATABASE_STRUCT  ]
   LOCAL aIndexes  := aDBFData[ DATABASE_INDEX ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL oError, aRecord, aRecDataInit

   HB_SYMBOL_UNUSED( nRecords )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecords = %2$s", nWA, hb_ValToExp( nRecords ) ) )

   IF aOpenInfo[ UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 /* EDBF_READONLY */
      oError:Description := hb_langErrMsg( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
#if 0
      oError:OsCode      := FError()
#endif
      oError:CanDefault  := .T.
      oError:CanRetry    := .T.
      NetErr( .T. )
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF
   aRecord := BlankRecord( aStruct )
   AAdd( aRecords, aRecord )

   aRecDataInit := AR_RECDATAINIT()
   AAdd( aRecInfo, aRecDataInit )

   NetErr( .F. )
   AR_GOTO( nWa, Len( aRecords ) )
   AEval( aIndexes, {| aIndex, n | ModifyIndex( n, Eval( aIndex[ INDEX_ORCR, UR_ORCR_BKEY ] ), aIndex, aWAData ) } )

   /* SHARED ACCESS */
   IF aWAData[ WADATA_OPENINFO, UR_OI_SHARED ]
      aRecDataInit[ RECDATA_LOCKED ] := nWA
      AAdd( aWAData[ WADATA_LOCKS ], aWAData[ WADATA_RECNO ] )
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_LOCK( nWA, aLock )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL nRec     := iif( aLock[ UR_LI_RECORD ] == NIL, aWAData[ WADATA_RECNO ], aLock[ UR_LI_RECORD ] )
   LOCAL aRecInfo

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aLock = %2$s", nWA, hb_ValToExp( aLock ) ) )

   IF aWAData[ WADATA_EOF ]
      aLock[ UR_LI_RESULT ] := .T.

   ELSE
      aRecInfo := aWAData[ WADATA_DATABASE, DATABASE_RECINFO, nRec ]
      IF aWAData[ WADATA_OPENINFO, UR_OI_SHARED ]
         IF aRecInfo[ RECDATA_LOCKED ] == nWA
            aLock[ UR_LI_RESULT ] := .T.
         ELSEIF aRecInfo[ RECDATA_LOCKED ] != 0
            aLock[ UR_LI_RESULT ] := .F.
         ELSE
            aRecInfo[ RECDATA_LOCKED ] := nWA
            AAdd( aWAData[ WADATA_LOCKS ], nRec )
            aLock[ UR_LI_RESULT ] := .T.
         ENDIF
      ELSE
         aLock[ UR_LI_RESULT ] := .T.
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_UNLOCK( nWA, nRec )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aRecords := aWAData[ WADATA_LOCKS ]
   LOCAL aRecInfo := aWAData[ WADATA_DATABASE, DATABASE_RECINFO ]
   LOCAL nPos

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRec = %2$d", nWA, nRec ) )

   IF ! Empty( aRecords )
      IF nRec == NIL            /* Unlock All */
         FOR EACH nRec IN aRecords
            aRecInfo[ nRec, RECDATA_LOCKED ] := 0
         NEXT
         ASize( aRecords, 0 )
      ELSE
         nPos := AScan( aRecords, nRec )
         IF nPos > 0
            aRecInfo[ nRec, RECDATA_LOCKED ] := 0
            hb_ADel( aRecords, nPos, .T. )
         ENDIF
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_RECID( nWA, nRecNo )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL nRecCount := Len( aRecords )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecNo = %2$s", nWA, hb_ValToExp( nRecNo ) ) )

   IF aWAData[ WADATA_EOF ]
      nRecNo := nRecCount + 1
   ELSE
      nRecNo := aWAData[ WADATA_RECNO ]
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_RECCOUNT( nWA, nRecords )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nRecords = %2$s", nWA, hb_ValToExp( nRecords ) ) )

   nRecords := Len( aRecords )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nRecords = %1$d", nRecords ) )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_PACK( nWA )

   LOCAL oError, nRec, aIndex
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aDBFData := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo := aDBFData[ DATABASE_RECINFO ]
   LOCAL aIndexes := aDBFData[ DATABASE_INDEX ]
   LOCAL nDel     := 0

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF ! aDBFData[ DATABASE_LOCKED ]
      oError := ErrorNew()
      oError:GenCode     := EG_UNLOCKED
      oError:SubCode     := 1022 /* EDBF_UNLOCKED */
      oError:Description := hb_langErrMsg( EG_UNLOCKED )
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE

   ENDIF

   AEval( aIndexes, {| aIndex, n | ModifyIndex( n, Eval( aIndex[ INDEX_ORCR, UR_ORCR_BKEY ] ), aIndex, aWAData ) } )
   FOR EACH aIndex IN aIndexes
      FOR nRec := Len( aIndex[ INDEX_RECORDS ] ) TO 1 STEP -1
         IF aRecInfo[ aIndex[ INDEX_RECORDS, INDEXKEY_RECORD ], RECDATA_DELETED ]
            ADel( aIndex[ INDEX_RECORDS ], nRec )
            nDel++
         ENDIF
      NEXT
      IF nDel > 0
         ASize( aIndex[ INDEX_RECORDS ], Len( aIndex[ INDEX_RECORDS ] ) - nDel )
         nDel := 0
      ENDIF
   NEXT

   FOR nRec := Len( aRecInfo ) TO 1 STEP -1
      IF aRecInfo[ nRec, RECDATA_DELETED ]
         ADel( aRecInfo, nRec, .T. )
         ADel( aRecords, nRec, .T. )
      ENDIF
   NEXT
   AR_GOTOP( nWA )

   RETURN SUCCESS

STATIC FUNCTION AR_ZAP( nWA )

   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL oError

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d", nWA ) )

   IF aOpenInfo[ UR_OI_READONLY ]
      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 /* EDBF_READONLY */
      oError:Description := hb_langErrMsg( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF

   IF aOpenInfo[ UR_OI_SHARED ]
      oError := ErrorNew()
      oError:GenCode     := EG_SHARED
      oError:SubCode     := 1023 /* EDBF_SHARED */
      oError:Description := hb_langErrMsg( EG_SHARED )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF

   /* empty records */
   aDBFData[ DATABASE_RECORDS ] := {}
   aDBFData[ DATABASE_RECINFO ] := {}

   /* move to 0 recno */
   AR_GOTO( nWA, 0 )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_FOUND( nWa, lFound )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, lFound = %2$s", nWa, hb_ValToExp( lFound ) ) )

   lFound := USRRDD_AREADATA( nWA )[ WADATA_FOUND ]

   RETURN HB_SUCCESS

STATIC FUNCTION AR_SEEK( nWa, lSoftSeek, xSeek, lLast )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aIndexes := aWAData[ WADATA_DATABASE, DATABASE_INDEX ]
   LOCAL nIndex   := aWAData[ WADATA_INDEX ]
   LOCAL nResult  /*:= HB_SUCCESS */

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, lSoftSeek = %2$s, xSeek = %3$s, lLast = %4$s", nWa, hb_ValToExp( lSoftSeek ), hb_ValToExp( xSeek ), hb_ValToExp( lLast ) ) )

   aWAData[ WADATA_ORDRECNO ] := Seek( xSeek, lSoftSeek, lLast, aIndexes[ nIndex ] )
   IF aWAData[ WADATA_ORDRECNO ] == 0 .OR. aWAData[ WADATA_ORDRECNO ] > Len( aIndexes[ nIndex, INDEX_RECORDS ] )
      aWAData[ WADATA_FOUND ] := .F.
      nResult := AR_GOTO( nWA, 0 )
   ELSE
      aWAData[ WADATA_FOUND ] := aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_KEY ] = xSeek
      nResult := AR_GOTO( nWA, aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_RECORD ] )
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_INFO( nWA, nMsg, xValue )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aDBFData := aWAData[ WADATA_DATABASE ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nMsg = %2$s, xValue = %3$s", nWA, hb_ValToExp( nMsg ), hb_ValToExp( xValue ) ) )

   SWITCH nMsg
   CASE DBI_TABLEEXT
      xValue := ""
      EXIT
   CASE DBI_SHARED
      xValue := aDBFData[ DATABASE_LOCKED ]
      EXIT
   OTHERWISE
      RETURN UR_SUPER_INFO( nWA, nMsg, @xValue )
   ENDSWITCH

   RETURN HB_SUCCESS

STATIC FUNCTION AR_ORDLSTADD( nWA, aOrderInfo )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aDBFData := aWAData[ WADATA_DATABASE ]
   LOCAL aIndexes := aDBFData[ DATABASE_INDEX ]

   HB_SYMBOL_UNUSED( aOrderInfo )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aOrderInfo = %2$s", nWA, hb_ValToExp( aOrderInfo ) ) )

   IF Empty( aIndexes )
      aWAData[ WADATA_INDEX ] := 0

   ELSE
      aWAData[ WADATA_INDEX ] := 1
      IF Empty( aWAData[ WADATA_WAORDINFO ] )
         AEval( aWAData[ WADATA_WAORDINFO ] := Array( Len( aIndexes ) ), {| x, y | HB_SYMBOL_UNUSED( x ), aWAData[ WADATA_WAORDINFO, y ] := AR_WAOIINIT() } )
      ENDIF

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_ORDLSTFOCUS( nWA, aOrderInfo )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aDBFData := aWAData[ WADATA_DATABASE ]
   LOCAL aIndexes := aDBFData[ DATABASE_INDEX ]
   LOCAL xIndex   := aOrderInfo[ UR_ORI_TAG ]

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aOrderInfo = %2$s", nWA, hb_ValToExp( aOrderInfo ) ) )

   aOrderInfo[ UR_ORI_RESULT ] := iif( aWAData[ WADATA_INDEX ] > 0, aIndexes[ aWAData[ WADATA_INDEX ], INDEX_TAG ], "" )

   SWITCH ValType( xIndex )
   CASE "N"
      aWAData[ WADATA_INDEX ] := iif( xIndex >= 1 .AND. xIndex <= Len( aIndexes ), Int( xIndex ), 0 )
      EXIT
   CASE "C"
      xIndex := Upper( xIndex )
      aWAData[ WADATA_INDEX ] := AScan( aIndexes, {| x | x[ INDEX_TAG ] == xIndex } )
      EXIT
   ENDSWITCH

   RETURN HB_SUCCESS

STATIC FUNCTION AR_ORDCREATE( nWA, aOrderCreate )

   LOCAL aWAData, aDBFData, aOCInfo, nNext
   LOCAL aIndexes, nContNext, nContStep
   LOCAL bWhile, nRec, bNext, bEval, bEvalOCI, nStep, nIndex, cIndex, aIndex

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, aOrderCreate = %2$s", nWA, hb_ValToExp( aOrderCreate ) ) )

   aWAData   := USRRDD_AREADATA( nWA )
   aDBFData  := aWAData[ WADATA_DATABASE ]

   IF HB_ISARRAY( aOrderCreate[ UR_ORCR_CONDINFO ] )
      aOCInfo   := aOrderCreate[ UR_ORCR_CONDINFO ]
   ELSE
      aOCInfo   := aOrderCreate[ UR_ORCR_CONDINFO ] := { ;
         .F., ; /* #define UR_ORC_ACTIVE         1   */
         "", ;  /* #define UR_ORC_CFOR           2   */
         "", ;  /* #define UR_ORC_CWHILE         3   */
         NIL, ; /* #define UR_ORC_BFOR           4   */
         NIL, ; /* #define UR_ORC_BWHILE         5   */
         NIL, ; /* #define UR_ORC_BEVAL          6   */
         0, ;   /* #define UR_ORC_STEP           7   */
         0, ;   /* #define UR_ORC_STARTREC       8   */
         0, ;   /* #define UR_ORC_NEXT           9   */
         0, ;   /* #define UR_ORC_RECORD         10  */
         .F., ; /* #define UR_ORC_REST           11  */
         .F., ; /* #define UR_ORC_DESCEND        12  */
         .F., ; /* #define UR_ORC_SCOPED         13  */
         .T., ; /* #define UR_ORC_ALL            14  */
         .F., ; /* #define UR_ORC_ADDITIVE       15  */
         .F., ; /* #define UR_ORC_USECURRENT     16  */
         .F., ; /* #define UR_ORC_CUSTOM         17  */
         .F., ; /* #define UR_ORC_NOOPTIMIZE     18  */
         .F., ; /* #define UR_ORC_COMPOUND       19  */
         .F., ; /* #define UR_ORC_USEFILTER      20  */
         .F., ; /* #define UR_ORC_TEMPORARY      21  */
         .F., ; /* #define UR_ORC_EXCLUSIVE      22  */
         NIL  ; /* #define UR_ORC_CARGO          23  */
      }
   ENDIF

   nNext     := aOCInfo[ UR_ORC_NEXT ]
   aIndexes  := aDBFData[ DATABASE_INDEX ]
   nContNext := 1
   nContStep := 0

   IF Empty( aOrderCreate[ UR_ORCR_TAGNAME ] )
      aOrderCreate[ UR_ORCR_TAGNAME ] := aOrderCreate[ UR_ORCR_BAGNAME ]
   ENDIF
   cIndex := aOrderCreate[ UR_ORCR_TAGNAME ] := Upper( aOrderCreate[ UR_ORCR_TAGNAME ] )
   aIndex := AR_INDEXINIT()
   aIndex[ INDEX_TAG   ]   := cIndex
   aIndex[ INDEX_ORCR  ]   := aOrderCreate
   nIndex := AScan( aIndexes, {| x | x[ INDEX_TAG ] == cIndex } )
   IF nIndex > 0
      ADel( aIndexes, nIndex )
      aIndexes[ Len( aIndexes ) ] := aIndex
   ELSE
      AAdd( aIndexes, aIndex )
   ENDIF

   IF aOCInfo[ UR_ORC_BWHILE ] == NIL .AND. nNext == 0
      nRec := 1
      AR_GOTO( nWA, nRec )
   ELSE
      nRec := aOCInfo[ UR_ORC_STARTREC ]
      AR_GOTO( nWA, nRec )
   ENDIF
   IF aOCInfo[ UR_ORC_BWHILE ] == NIL
      bWhile := {|| .T. }
   ELSE
      bWhile := aOCInfo[ UR_ORC_BWHILE ]
   ENDIF
   IF nNext == 0
      bNext := {|| .T. }
   ELSE
      bNext := {|| nContNext++ <= nNext }
   ENDIF
   IF aOCInfo[ UR_ORC_BEVAL ] == NIL
      HB_TRACE( HB_TR_DEBUG, "bEval = {|| .T. }" )
      bEval := {|| .T. }
   ELSEIF aOCInfo[ UR_ORC_STEP ] == NIL
      bEval := aOCInfo[ UR_ORC_BEVAL ]
      HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "bEval = %1$s", hb_ValToExp( bEval ) ) )
   ELSE
      bEvalOCI := aOCInfo[ UR_ORC_BEVAL ]
      nStep    := aOCInfo[ UR_ORC_STEP ]
      bEval    := {|| iif( ++nContStep == nStep, ( nContStep := 0, Eval( bEvalOCI ) ), .T. ) }
      HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "bEvalOCI = %1$s, nStep = %2$d, bEval = %3$s", hb_ValToExp( bEvalOCI ), nStep, hb_ValToExp( bEval ) ) )
   ENDIF

   AAdd( aWAData[ WADATA_WAORDINFO ], AR_WAOIINIT() )
   aWAData[ WADATA_INDEX ] := Len( aIndexes )

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "aWAData[ WADATA_EOF ] = %1$s", hb_ValToExp( aWAData[ WADATA_EOF ] ) ) )

   DO WHILE ! aWAData[ WADATA_EOF ] .AND. Eval( bEval ) .AND. Eval( bNext ) .AND. Eval( bWhile )
      HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "aWAData[ WADATA_INDEX ] = %1$s, Eval( aIndex[ INDEX_ORCR, UR_ORCR_BKEY ] ) = %2$s, aIndex = %3$s, aWAData = %4$s", ;
         hb_ValToExp( aWAData[ WADATA_INDEX ] ), hb_ValToExp( Eval( aIndex[ INDEX_ORCR, UR_ORCR_BKEY ] ) ), ;
         hb_ValToExp( hb_ValToExp( aIndex ) ), hb_ValToExp( aWAData ) ) )
      ModifyIndex( aWAData[ WADATA_INDEX ], Eval( aIndex[ INDEX_ORCR, UR_ORCR_BKEY ] ), aIndex, aWAData )
      AR_GOTO( nWA, ++nRec )
   ENDDO

   RETURN AR_GOTOP( nWA )

STATIC FUNCTION AR_ORDINFO( nWA, nMsg, aOrderInfo )

   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL aIndexes := aWAData[ WADATA_DATABASE, DATABASE_INDEX ]
   LOCAL nIndex, nPos

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nWA = %1$d, nMsg = %2$s, aOrderInfo = %3$s", nWA, hb_ValToExp( nMsg ), hb_ValToExp( aOrderInfo ) ) )

   IF Empty( aOrderInfo[ UR_ORI_TAG ] )
      aOrderInfo[ UR_ORI_TAG ] := aOrderInfo[ UR_ORI_BAG ]
   ENDIF

   SWITCH ValType( aOrderInfo[ UR_ORI_TAG ] )
   CASE "C"
      nIndex := Upper( aOrderInfo[ UR_ORI_TAG ] )
      nIndex := AScan( aIndexes, {| x | x[ INDEX_TAG ] == nIndex } )
      EXIT
   CASE "N"
      nIndex := aOrderInfo[ UR_ORI_TAG ]
      EXIT
   OTHERWISE
      nIndex  := aWAData[ WADATA_INDEX ]
   ENDSWITCH

   SWITCH nMsg
   CASE DBOI_EXPRESSION /* 2 */
      IF nIndex < 1 .OR. Empty( aIndexes ) .OR. nIndex > Len( aIndexes[ nIndex ] )
         aOrderInfo[ UR_ORI_RESULT ] := ""
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := aIndexes[ nIndex, INDEX_ORCR, UR_ORCR_CKEY ]
      ENDIF
      EXIT
   CASE DBOI_POSITION   /* 3 */
      IF nIndex < 1 .OR. Empty( aIndexes ) .OR. nIndex > Len( aIndexes[ nIndex ] ) .OR. Empty( aIndexes[ nIndex, INDEX_RECORDS ] ) .OR. aWAData[ WADATA_ORDRECNO ] == 0
         aOrderInfo[ UR_ORI_RESULT ] := 0
      ELSE
         IF aIndexes[ nIndex, INDEX_RECORDS, aWAData[ WADATA_ORDRECNO ], INDEXKEY_RECORD ] != aWAData[ WADATA_RECNO ]
            aWAData[ WADATA_ORDRECNO ] := Seek( Eval( aIndexes[ nIndex, INDEX_ORCR, UR_ORCR_BKEY ] ), .F., .F., aIndexes[ nIndex ], aWAData[ WADATA_RECNO ] )
         ENDIF
         IF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] == NIL
            aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_ORDRECNO ]
         ELSE
            nPos := Seek( aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ], .T., .F., aIndexes[ nIndex ] )
            IF nPos > 0 .AND. ! aIndexes[ nIndex, INDEX_RECORDS, nPos, INDEXKEY_KEY ] = aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ]
               IF nPos > 1 .AND. aIndexes[ nIndex, INDEX_RECORDS, nPos - 1, INDEXKEY_KEY ] >= aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ]
                  nPos--
               ELSE
                  aOrderInfo[ UR_ORI_RESULT ] := 0
                  EXIT
               ENDIF
            ENDIF
            aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_ORDRECNO ] - nPos + 1
         ENDIF
      ENDIF
      EXIT
   CASE DBOI_BAGNAME /* 7 */
      aOrderInfo[ UR_ORI_RESULT ] := ""
      EXIT
   CASE DBOI_KEYCOUNT   /* 26 */
      IF nIndex > 0 .AND. ! Empty( aWAData[ WADATA_DATABASE, DATABASE_RECORDS ] )
         IF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] == NIL
            nPos := 0
         ELSE
            nPos := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .F. )
            IF nPos == 0
               aOrderInfo[ UR_ORI_RESULT ] := 0
               EXIT
            ENDIF
         ENDIF
         IF aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] == NIL
            IF nPos > 0
               nPos := Len( aIndexes[ nIndex, INDEX_RECORDS ] ) - nPos + 1
            ENDIF
         ELSE
            nMsg := SeekScope( aIndexes[ nIndex ], aWAData[ WADATA_WAORDINFO, nIndex ], .T. )
            IF nMsg > 0
               IF nPos == 0
                  nPos := nMsg
               ELSE
                  nPos := nMsg - nPos + 1
               ENDIF
            ENDIF
         ENDIF
         IF nPos > 0
            aOrderInfo[ UR_ORI_RESULT ] := nPos
         ELSE
            aOrderInfo[ UR_ORI_RESULT ] := Len( aIndexes[ nIndex, INDEX_RECORDS ] )
         ENDIF
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := 0
      ENDIF
      EXIT
   CASE DBOI_SCOPETOP   /* 39 */
      aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ]
      IF aOrderInfo[ UR_ORI_ALLTAGS ] != NIL
         aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] := aOrderInfo[ UR_ORI_NEWVAL ]
      ENDIF
      EXIT
   CASE DBOI_SCOPEBOTTOM   /* 40 */
      aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ]
      IF aOrderInfo[ UR_ORI_ALLTAGS ] != NIL
         aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] := aOrderInfo[ UR_ORI_NEWVAL ]
      ENDIF
      EXIT
   CASE DBOI_SCOPETOPCLEAR /* 41 */
      aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ]
      aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_0 ] := NIL
      EXIT
   CASE DBOI_SCOPEBOTTOMCLEAR /* 42 */
      aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ]
      aWAData[ WADATA_WAORDINFO, nIndex, WAOI_SCOPE_1 ] := NIL
      EXIT
   OTHERWISE
      RETURN HB_FAILURE

   ENDSWITCH

   RETURN HB_SUCCESS

STATIC FUNCTION AR_DUMMY()

   RETURN HB_SUCCESS

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */

FUNCTION ARRAYRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, pSuperRddID )

   LOCAL cSuperRDD := NIL     /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   s_nRddID := nRddID

   aMyFunc[ UR_INIT         ] := ( @AR_INIT()         )
   aMyFunc[ UR_NEW          ] := ( @AR_NEW()          )
   aMyFunc[ UR_FLUSH        ] := ( @AR_DUMMY()        )
   aMyFunc[ UR_CREATE       ] := ( @AR_CREATE()       )
   aMyFunc[ UR_CREATEFIELDS ] := ( @AR_CREATEFIELDS() )
   aMyFunc[ UR_OPEN         ] := ( @AR_OPEN()         )
   aMyFunc[ UR_CLOSE        ] := ( @AR_CLOSE()        )
   aMyFunc[ UR_BOF          ] := ( @AR_BOF()          )
   aMyFunc[ UR_EOF          ] := ( @AR_EOF()          )
   aMyFunc[ UR_APPEND       ] := ( @AR_APPEND()       )
   aMyFunc[ UR_DELETE       ] := ( @AR_DELETE()       )
   aMyFunc[ UR_DELETED      ] := ( @AR_DELETED()      )
   aMyFunc[ UR_RECALL       ] := ( @AR_RECALL()       )
   aMyFunc[ UR_SETFILTER    ] := ( @AR_SETFILTER()    )
   aMyFunc[ UR_CLEARFILTER  ] := ( @AR_CLEARFILTER()  )
   aMyFunc[ UR_SKIPFILTER   ] := ( @AR_SKIPFILTER()   )
   aMyFunc[ UR_SKIPRAW      ] := ( @AR_SKIPRAW()      )
   aMyFunc[ UR_GOTO         ] := ( @AR_GOTO()         )
   aMyFunc[ UR_GOTOID       ] := ( @AR_GOTOID()       )
   aMyFunc[ UR_GOTOP        ] := ( @AR_GOTOP()        )
   aMyFunc[ UR_GOBOTTOM     ] := ( @AR_GOBOTTOM()     )
   aMyFunc[ UR_RECID        ] := ( @AR_RECID()        )
   aMyFunc[ UR_LOCK         ] := ( @AR_LOCK()         )
   aMyFunc[ UR_UNLOCK       ] := ( @AR_UNLOCK()       )
   aMyFunc[ UR_RECCOUNT     ] := ( @AR_RECCOUNT()     )
   aMyFunc[ UR_GETVALUE     ] := ( @AR_GETVALUE()     )
   aMyFunc[ UR_PUTVALUE     ] := ( @AR_PUTVALUE()     )
   aMyFunc[ UR_PACK         ] := ( @AR_PACK()         )
   aMyFunc[ UR_ZAP          ] := ( @AR_ZAP()          )
   aMyFunc[ UR_FOUND        ] := ( @AR_FOUND()        )
   aMyFunc[ UR_SEEK         ] := ( @AR_SEEK()         )
   aMyFunc[ UR_INFO         ] := ( @AR_INFO()         )
   aMyFunc[ UR_ORDLSTADD    ] := ( @AR_ORDLSTADD()    )
   aMyFunc[ UR_ORDLSTFOCUS  ] := ( @AR_ORDLSTFOCUS()  )
   aMyFunc[ UR_ORDCREATE    ] := ( @AR_ORDCREATE()    )
   aMyFunc[ UR_ORDINFO      ] := ( @AR_ORDINFO()      )

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
      cSuperRDD, aMyFunc, pSuperRddID )

INIT PROCEDURE ARRAYRDD_INIT()

   rddRegister( "ARRAYRDD", RDT_FULL )

   RETURN

/* -------------------------------------------------- */
/*           UTILITY FUNCTIONS                        */
/* -------------------------------------------------- */

/*
  hb_EraseArrayRdd() function is equivalent of FErase() function, but works here in memory
*/

FUNCTION hb_EraseArrayRdd( cFullName )

   LOCAL nReturn := HB_FAILURE
   LOCAL aDBFData, oError
   LOCAL hRDDData

   IF s_nRddID >= 0
      hRDDData := USRRDD_RDDDATA( s_nRddID )

      IF hRDDData != NIL
         IF HB_ISSTRING( cFullName )
            cFullName := Upper( cFullName )
            /* First search if memory dbf exists */
            IF hb_HHasKey( hRDDData, cFullName )

               /* Get ARRAY DATA */
               aDBFData := hRDDData[ cFullName ]

               /* Check if there are current opened workarea */
               IF aDBFData[ DATABASE_OPENNUMBER ] > 0
                  oError := ErrorNew()

                  oError:GenCode     := EG_UNSUPPORTED
                  oError:SubCode     := 1000  /* EDBF_UNSUPPORTED */
                  oError:Description := hb_langErrMsg( EG_UNSUPPORTED ) + " (" + ;
                     "database in use)"
                  oError:FileName    := cFullName
                  oError:CanDefault  := .T.
                  Throw( oError )

                  nReturn := HB_FAILURE

               ELSE
                  /* Delete database from slot */
                  hb_HDel( hRDDData, cFullName )
                  nReturn := HB_SUCCESS

               ENDIF
            ENDIF
         ENDIF

      ELSE
         oError := ErrorNew()

         oError:GenCode     := EG_UNSUPPORTED
         oError:SubCode     := 1000  /* EDBF_UNSUPPORTED */
         oError:Description := hb_langErrMsg( EG_UNSUPPORTED ) + " (" + ;
            "ARRAYRDD not inizialized)"
         oError:FileName    := cFullName
         oError:CanDefault  := .T.
         /* UR_SUPER_ERROR( 0, oError ) */
         Throw( oError )

         nReturn := HB_FAILURE

      ENDIF

   ELSE
      oError := ErrorNew()

      oError:GenCode     := EG_UNSUPPORTED
      oError:SubCode     := 1000  /* EDBF_UNSUPPORTED */
      oError:Description := hb_langErrMsg( EG_UNSUPPORTED ) + " (" + ;
         "ARRAYRDD not in use)"
      oError:FileName    := cFullName
      oError:CanDefault  := .T.
      Throw( oError )

      nReturn := HB_FAILURE

   ENDIF

   RETURN nReturn

/*
  hb_FileArrayRdd( cFullName ) --> lExist
  This function is equivalent of File() function, but works here in memory
*/

FUNCTION hb_FileArrayRdd( cFullName )

   LOCAL nReturn := HB_FAILURE
   LOCAL oError
   LOCAL hRDDData

   IF s_nRddID >= 0
      hRDDData := USRRDD_RDDDATA( s_nRddID )

      IF hRDDData != NIL
         IF HB_ISSTRING( cFullName )
            cFullName := Upper( cFullName )
            /* First search if memory dbf exists */
            IF hb_HHasKey( hRDDData, cFullName )
               nReturn := HB_SUCCESS

            ENDIF
         ENDIF

      ELSE
         oError := ErrorNew()

         oError:GenCode     := EG_UNSUPPORTED
         oError:SubCode     := 1000  /* EDBF_UNSUPPORTED */
         oError:Description := hb_langErrMsg( EG_UNSUPPORTED ) + " (" + ;
            "ARRAYRDD not inizialized)"
         oError:FileName    := cFullName
         oError:CanDefault  := .T.
         Throw( oError )

         nReturn := HB_FAILURE

      ENDIF

   ELSE
      oError := ErrorNew()

      oError:GenCode     := EG_UNSUPPORTED
      oError:SubCode     := 1000  /* EDBF_UNSUPPORTED */
      oError:Description := hb_langErrMsg( EG_UNSUPPORTED ) + " (" + ;
         "ARRAYRDD not in use)"
      oError:FileName    := cFullName
      oError:CanDefault  := .T.
      Throw( oError )

      nReturn := HB_FAILURE

   ENDIF

   RETURN nReturn == HB_SUCCESS

FUNCTION hb_SetArrayRdd( aArray )

   LOCAL aRecInfo
   LOCAL nWA      := Select()
   LOCAL aDBFData := USRRDD_AREADATA( nWA )[ WADATA_DATABASE ]

   aDBFData[ DATABASE_RECORDS ] := aArray
   aDBFData[ DATABASE_RECINFO ] := Array( Len( aArray ) )
   FOR EACH aRecInfo IN aDBFData[ DATABASE_RECINFO ]
      aRecInfo := AR_RECDATAINIT()
   NEXT
   AR_GOTOP( nWA )

   RETURN NIL

STATIC FUNCTION BlankRecord( aStruct )

   LOCAL nLenStruct := Len( aStruct )
   LOCAL aRecord    := Array( nLenStruct )
   LOCAL nField

   FOR nField := 1 TO nLenStruct
      aRecord[ nField ] := EmptyValue( aStruct[ nField ][ DBS_TYPE ], aStruct[ nField ][ DBS_LEN ], aStruct[ nField ][ DBS_DEC ] )
   NEXT

   RETURN aRecord

STATIC FUNCTION PutValue( xValue, cType, nLen, nDec )

   LOCAL xVal

   DO CASE
   CASE cType == "C"
      xVal := PadR( xValue, nLen )
   CASE cType == "M"
      xVal := xValue  /* No limit for a memo field */
   CASE cType == "N"
      xVal := Val( Str( xValue, nLen, nDec ) )
   OTHERWISE
      xVal := xValue
   ENDCASE

   RETURN xVal

STATIC FUNCTION EmptyValue( cType, nLen, nDec )

   LOCAL xVal

   hb_default( @nLen, 0 )
   hb_default( @nDec, 0 )

   DO CASE
   CASE cType == "C" .OR. cType == "M"
      xVal := Space( nLen )
   CASE cType == "D"
      xVal := hb_SToD()
   CASE cType == "L"
      xVal := .F.
   CASE cType == "N"
      xVal := Val( Str( 0, nLen, nDec ) )
   ENDCASE

   RETURN xVal

/**
 * Function .......: hb_Decode( <var>, [ <case1,ret1 [,...,caseN,retN] ] [, <def> ]> ) ---> <xRet>
 * Author .........: Francesco Saverio Giudice
 * Date of creation: 1991/01/25
 * Last revision ..: 2006/01/24 1.13 - rewritten for xHarbour and renamed in hb_Decode()
 *
 *                   Decode a value from a list.
 */

STATIC FUNCTION hb_Decode( ... )

   LOCAL aParams, nParams, xDefault
   LOCAL xVal, cKey, xRet
   LOCAL aValues, aResults, n, i, nPos, nLen

   aParams  := hb_AParams()
   nParams  := PCount()
   xDefault := NIL

   DO CASE
   CASE nParams > 1     /* More parameters, real CASE */
      xVal := aParams[ 1 ]

      ADel( aParams, 1, .T. ) /* Resize params */
      nParams := Len( aParams )

      /* if I have a odd number of members, last is DEFAULT */
      IF ( nParams % 2 ) != 0
         xDefault := ATail( aParams )
         /* Resize again deleting last */
         ADel( aParams, nParams, .T. )
         nParams := Len( aParams )
      ENDIF

      /* Ok because I have no other value than default, I will check if it is a complex value */
      /* like an array or an hash, so I can get it to decode values */
      IF xDefault != NIL .AND. ;
         ( HB_ISARRAY( xDefault ) .OR. HB_ISHASH( xDefault ) )

         /* If it is an array I will restart this function creating a linear call */
         IF HB_ISARRAY( xDefault ) .AND. Len( xDefault ) > 0
            /* I can have a linear array like { 1, "A", 2, "B", 3, "C" }
             * or an array of array couples like { { 1, "A" }, { 2, "B" }, { 3, "C" } }
             * first element tell me what type is */

            /* couples of values */
            IF HB_ISARRAY( xDefault[ 1 ] )
               /* If i have an array as default, this contains couples of key / value */
               /* so I have to convert in a linear array */

               nLen := Len( xDefault )

               /* Check if array has a default value, this will be last value and has a value */
               /* different from an array */
               IF ! HB_ISARRAY( ValType( xDefault[ nLen ] ) )
                  aParams := Array( ( nLen - 1 ) * 2 )

                  n := 1
                  FOR i := 1 TO nLen - 1
                     aParams[ n++ ] := xDefault[ i ][ 1 ]
                     aParams[ n++ ] := xDefault[ i ][ 2 ]
                  NEXT

                  AAdd( aParams, xDefault[ nLen ] )

               ELSE
                  /* I haven't a DEFAULT */
                  aParams := Array( Len( xDefault ) * 2 )

                  n := 1
                  FOR i := 1 TO Len( xDefault )
                     aParams[ n++ ] := xDefault[ i ][ 1 ]
                     aParams[ n++ ] := xDefault[ i ][ 2 ]
                  NEXT

               ENDIF
            ELSE
               /* I have a linear array */
               aParams := xDefault

            ENDIF

         ELSEIF HB_ISHASH( xDefault ) /* If it is an hash, translate it in an array */

            aParams := Array( Len( xDefault ) * 2 )

            i := 1
            FOR EACH cKey IN xDefault:Keys
               aParams[ i++ ] := cKey
               aParams[ i++ ] := xDefault[ cKey ]
            NEXT

         ENDIF

         /* Then add Decoding value at beginning */
         AIns( aParams, 1, xVal, .T. )

         /* And run decode() again */
         xRet := hb_ExecFromArray( @hb_Decode(), aParams )

      ELSE
         /* Ok let's go ahead with real FUNCTION */

         /* Combine in 2 lists having elements as { value } and { decode } */
         aValues  := Array( nParams / 2 )
         aResults := Array( nParams / 2 )

         i := 1
         FOR n := 1 TO nParams - 1 STEP 2
            aValues[ i ]  := aParams[ n ]
            aResults[ i ] := aParams[ n + 1 ]
            i++
         NEXT

         /* Check if value exists (valtype of values MUST be same of xVal,
          * otherwise I will get a runtime error)
          * TODO: Have I to check also between different valtypes, jumping different ? */
         nPos := AScan( aValues, {| e | e == xVal } )

         IF nPos == 0   /* Not Found, returning DEFAULT */
            xRet := xDefault   /* it could be also NIL because not present */

         ELSE
            xRet := aResults[ nPos ]

         ENDIF
      ENDIF

   CASE nParams == 0    /* No parameters */
      xRet := NIL

   CASE nParams == 1    /* Only value to decode as parameter, return an empty value of itself */
      xRet := DecEmptyValue( aParams[ 1 ] )

   ENDCASE

   RETURN xRet

STATIC FUNCTION DecEmptyValue( xVal )

   LOCAL xRet
   LOCAL cType := ValType( xVal )

   SWITCH cType
   CASE "C"  /* Char */
   CASE "M"  /* Memo */
      xRet := ""
      EXIT
   CASE "D"  /* Date */
      xRet := hb_SToD()
      EXIT
   CASE "L"  /* Logical */
      xRet := .F.
      EXIT
   CASE "N"  /* Number */
      xRet := 0
      EXIT
   CASE "B"  /* code block */
      xRet := {|| NIL }
      EXIT
   CASE "A"  /* array */
      xRet := {}
      EXIT
   CASE "H"  /* hash */
      xRet := { => }
      EXIT
   CASE "U"  /* undefined */
      xRet := NIL
      EXIT
   CASE "O"  /* Object */
      xRet := NIL   /* Or better another value ? */
      EXIT
   OTHERWISE
      /* Create a runtime error for new datatypes */
      xRet := ""
      IF xRet == 0 /* BANG! */
      ENDIF

   ENDSWITCH

   RETURN xRet

STATIC FUNCTION ModifyIndex( nIndex, xValue, aIndex, aWAData, xValorAnt )

   LOCAL nPos, aOCInfo, lFor, lDel

   HB_TRACE( HB_TR_DEBUG, hb_StrFormat( "nIndex = %1$d, xValue = %2$s, aIndex = %3$s, aWAData = %4$s, xValorAnt = %5$s", ;
      nIndex, hb_ValToExp( xValue ), hb_ValToExp( aIndex ), hb_ValToExp( aWAData ), hb_ValToExp( xValorAnt ) ) )

   aOCInfo := aIndex[ INDEX_ORCR, UR_ORCR_CONDINFO ]
   lFor    := ( aOCInfo[ UR_ORC_BFOR ] == NIL .OR. Eval( aOCInfo[ UR_ORC_BFOR ] ) )
   lDel    := .F.

   IF xValorAnt != NIL .AND. ( ! lFor .OR. ! xValue == xValorAnt )
      ADel( aIndex[ INDEX_RECORDS ], Seek( xValorAnt, .F., .F., aIndex, aWAData[ WADATA_RECNO ] ) )
      lDel := .T.
   ENDIF

   IF lFor .AND. ! xValue == xValorAnt
      nPos := Seek( xValue, .T., .T., aIndex )
      IF xValorAnt == NIL
         AAdd( aIndex[ INDEX_RECORDS ], NIL )
      ENDIF
      IF nPos > 0
         IF aIndex[ INDEX_RECORDS, nPos ] != NIL .AND. aIndex[ INDEX_RECORDS, nPos, INDEXKEY_KEY ] <= xValue
            nPos++
         ENDIF
      ELSE
         nPos := Len( aIndex[ INDEX_RECORDS ] )
      ENDIF
      AIns( aIndex[ INDEX_RECORDS ], nPos )
      aIndex[ INDEX_RECORDS, nPos ] := AR_INDEXKEYINIT()
      aIndex[ INDEX_RECORDS, nPos, INDEXKEY_KEY    ] := xValue
      aIndex[ INDEX_RECORDS, nPos, INDEXKEY_RECORD ] := aWAData[ WADATA_RECNO ]
      IF nIndex == aWAData[ WADATA_INDEX ]
         aWAData[ WADATA_ORDRECNO ] := nPos
      ENDIF

   ELSEIF lDel
      ASize( aIndex[ INDEX_RECORDS ], Len( aIndex[ INDEX_RECORDS ] ) - 1 )
      IF nIndex == aWAData[ WADATA_INDEX ]
         aWAData[ WADATA_ORDRECNO ] := 0
      ENDIF

   ENDIF

   RETURN NIL

STATIC FUNCTION Seek( xSeek, lSoft, lLast, aIndexInfo, nRec )

   LOCAL nPos, bFirst, bBefore, bAfter, bAjust
   LOCAL aIndex := aIndexInfo[ INDEX_RECORDS ]
   LOCAL nIni   := 1
   LOCAL nEnd   := Len( aIndex )

   SWITCH nEnd
   CASE 0   /* empty archive */
      nPos := 0
      EXIT
   CASE 1   /* Archive with 1 record */
      IF aIndex[ 1 ] == NIL .OR. iif( lSoft, iif( aIndexInfo[ INDEX_ORCR, UR_ORCR_CONDINFO, UR_ORC_DESCEND ], aIndex[ 1, INDEXKEY_KEY ] <= xSeek, aIndex[ 1, INDEXKEY_KEY ] >= xSeek ), aIndex[ 1, INDEXKEY_KEY ] == xSeek ) /* TOFIX: == comparison? */
         nPos := 1
      ELSE
         nPos := 0
      ENDIF
      EXIT
   OTHERWISE   /* Archive with 2 or more records */
      IF aIndexInfo[ INDEX_ORCR, UR_ORCR_CONDINFO, UR_ORC_DESCEND ]
         bFirst  := {|| aIndex[ 2, INDEXKEY_KEY ] >= xSeek }
         bBefore := {|| xSeek > aIndex[ nPos, INDEXKEY_KEY ]  }
         bAfter  := {|| xSeek < aIndex[ nPos, INDEXKEY_KEY ] }
         bAjust  := {|| ! aIndex[ nPos, INDEXKEY_KEY ] <= xSeek }
      ELSE
         bFirst  := {|| aIndex[ 2, INDEXKEY_KEY ] <= xSeek }
         bBefore := {|| ! aIndex[ nPos, INDEXKEY_KEY ] <= xSeek }
         bAfter  := {|| xSeek > aIndex[ nPos, INDEXKEY_KEY ] }
         bAjust  := {|| ! aIndex[ nPos, INDEXKEY_KEY ] >= xSeek }
      ENDIF

      IF aIndex[ 2 ] != NIL .AND. Eval( bFirst )
         DO WHILE nIni <= nEnd
            nPos := Int( ( nIni + nEnd ) / 2 )
            IF aIndex[ nPos ] == NIL .OR. Eval( bBefore )
               nEnd := nPos - 1
            ELSEIF Eval( bAfter )
               nIni := nPos + 1
            ELSE
               IF lLast
                  IF nPos < nEnd .AND. aIndex[ nPos + 1 ] != NIL .AND. aIndex[ nPos + 1, INDEXKEY_KEY ] == xSeek /* TOFIX: == comparison? */
                     nIni := nPos + 1
                  ELSE
                     EXIT
                  ENDIF
               ELSE
                  nEnd := nPos - 1
               ENDIF
               IF nRec != NIL .AND. nRec == aIndex[ nPos, INDEXKEY_RECORD ]
                  EXIT
               ENDIF
            ENDIF
         ENDDO
         IF aIndex[ nPos ] != NIL .AND. Eval( bAjust )
            nPos++
         ENDIF
      ELSE
         nPos := 1
      ENDIF
      IF nRec != NIL
         IF nIni <= nEnd .AND. ! Empty( aIndex ) .AND. aIndex[ nPos ] != NIL .AND. nRec != aIndex[ nPos, INDEXKEY_RECORD ]
            nEnd := Len( aIndex )
            FOR nPos := nIni TO nEnd
               IF aIndex[ nPos ] == NIL .OR. !( xSeek == aIndex[ nPos, INDEXKEY_KEY ] ) /* TOFIX: == comparison? */
                  nPos := 0
                  EXIT
               ELSEIF aIndex[ nPos, INDEXKEY_RECORD ] == nRec
                  EXIT
               ENDIF
            NEXT
            IF nPos > nEnd
               nPos := 0
            ENDIF
         ENDIF
      ELSEIF ! lSoft
         IF nPos > Len( aIndex ) .OR. !( aIndex[ nPos, INDEXKEY_KEY ] == xSeek ) /* TOFIX: == comparison? */
            nPos := 0
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN nPos

STATIC FUNCTION SeekScope( aIndex, aOrdInfo, lBottom )

   LOCAL nPos := Seek( aOrdInfo[ WAOI_SCOPE_0 ], .T., lBottom, aIndex )

   IF nPos > 0 .AND. ! aIndex[ INDEX_RECORDS, nPos, INDEXKEY_KEY ] == aOrdInfo[ WAOI_SCOPE_1 ] /* TOFIX: == comparison? */
      IF nPos > 1 .AND. aIndex[ INDEX_RECORDS, nPos - 1, INDEXKEY_KEY ] >= aOrdInfo[ WAOI_SCOPE_0 ]
         nPos--
      ELSE
         nPos := 0
      ENDIF
   ENDIF

   RETURN nPos
