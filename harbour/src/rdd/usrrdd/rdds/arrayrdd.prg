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
 * This is a Array RDD, or Memory RDD.
 * It works only in memory and actually supports standard dbf commands
 * excepts indexes, orders, relations
 */

#include "rddsys.ch"
#include "hbusrrdd.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "common.ch"

#xtranslate THROW( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )

//#define ACTIVATE_DEBUG // add xhb.lib to link

ANNOUNCE ARRAYRDD

#define DATABASE_FILENAME    1
#define DATABASE_RECORDS     2
#define DATABASE_RECINFO     3
#define DATABASE_OPENNUMBER  4
#define DATABASE_LOCKED      5
#define DATABASE_STRUCT      6
#define DATABASE_SIZEOF      6

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
#define WADATA_SIZEOF       11

#define RECDATA_DELETED      1
#define RECDATA_SIZEOF       1

static s_nRddID := -1

/*
 * non work area methods receive RDD ID as first parameter
 * Methods INIT and EXIT does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */
STATIC FUNCTION AR_INIT( nRDD )

   /* Init DBF Hash */
   USRRDD_RDDDATA( nRDD, hb_Hash() )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_RDDDATAINIT()
   RETURN { ;
            NIL     ; // RDDDATA_DATABASE
          }

STATIC FUNCTION AR_DATABASEINIT()
   RETURN { ;
             NIL   ,; // DATABASE_FILENAME
             {}    ,; // DATABASE_RECORDS
             {}    ,; // DATABASE_RECINFO
             0     ,; // DATABASE_OPENNUMBER
             .F.   ,; // DATABASE_LOCKED
             NIL    ; // DATABASE_STRUCT - aStruct
          }

STATIC FUNCTION AR_WADATAINIT()
   RETURN { ;
            NIL    ,; // WADATA_DATABASE
            0      ,; // WADATA_WORKAREA
            NIL    ,; // WADATA_OPENINFO
            0      ,; // WADATA_RECNO
            .F.    ,; // WADATA_BOF
            .F.    ,; // WADATA_FORCEBOF  // to solve an hack in dbf1.c
            .F.    ,; // WADATA_EOF
            .F.    ,; // WADATA_TOP
            .F.    ,; // WADATA_BOTTOM
            .F.    ,; // WADATA_FOUND
            {}      ; // WADATA_LOCKS
          }

STATIC FUNCTION AR_RECDATAINIT()
   RETURN { ;
            .F.     ; // RECDATA_DELETED
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

// Creating fields for new DBF - dbCreate() in current workarea
STATIC FUNCTION AR_CREATEFIELDS( nWA, aStruct )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL nResult   := HB_SUCCESS
   LOCAL aFieldStruct, aField

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_CREATEFIELDS(): nWA = %i, aStruct = %s\n\r", nWA, hb_ValToExp( aStruct ) )
#endif

   // Setting WA number to current WorkArea
   aWAData[ WADATA_WORKAREA ] := nWA

   // Create new file data structure - workarea uses a reference to database
   aWAData[ WADATA_DATABASE ] := AR_DATABASEINIT()

   // Store DBF Structure
   aWAData[ WADATA_DATABASE ][ DATABASE_STRUCT ] := aStruct

   // Set fields
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct

       aField := ARRAY( UR_FI_SIZE )
       aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
       aField[ UR_FI_TYPE ]    := HB_Decode( aFieldStruct[ DBS_TYPE ], "C", HB_FT_STRING, "L", HB_FT_LOGICAL, "M", HB_FT_MEMO, "D", HB_FT_DATE, "N", IIF( aFieldStruct[ DBS_DEC ] > 0, HB_FT_DOUBLE, HB_FT_INTEGER ) )
       aField[ UR_FI_TYPEEXT ] := 0
       aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
       aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
       UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   RETURN nResult

// Create database from current WA fields definition
STATIC FUNCTION AR_CREATE( nWA, aOpenInfo )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL hRDDData  := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL cName
   LOCAL cFullName, aDBFData, nResult//, aFieldStruct, aField, aStruct

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_CREATE(): nWA = %i, aOpenInfo = %s\n\r", nWA, hb_ValToExp( aOpenInfo ) )
#endif

   /* getting database infos from current workarea */
   aDBFData  := aWAData[ WADATA_DATABASE ]

   /* setting in uppercase chars to avoid differences */
   cFullName := Upper( aOpenInfo[ UR_OI_NAME ] )

   /* When there is no ALIAS we will create new one using file name */
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      HB_FNAMESPLIT( cFullName, , @cName )
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
   //IF !( cFullName $ hRDDData:Keys )

      /* Setting file attribs */
      aDBFData[ DATABASE_FILENAME ] := cFullName
      aDBFData[ DATABASE_LOCKED   ] := .T.      /* I need Exclusive mode in creation */

      /* Adding new database in RDD memory slots using filename as key */
      hb_hSet( hRDDData, cFullName, aDBFData )

   /* TODO: to clean this part
   ELSE

      // ERROR: database already exists

      oError := ErrorNew()

      oError:GenCode     := EG_CREATE
      oError:SubCode     := 1004  // EDBF_CREATE_DBF
      oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + " - database already exists)"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )

      RETURN HB_FAILURE

   ENDIF
   */

   // Set WorkArea Info
   aWAData[ WADATA_WORKAREA ] := nWA
   aWAData[ WADATA_OPENINFO ] := aOpenInfo // Put open informations

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   IF nResult == HB_SUCCESS
      /* Add a new open number */
      aDBFData[ DATABASE_OPENNUMBER ]++
      // default values for Records == 0
      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_OPEN( nWA, aOpenInfo )
   LOCAL cFullName, cName, hRDDData, aWAData, aDBFData
   LOCAL aStruct, oError, aFieldStruct, aField, nResult

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_OPEN(): nWA = %i, aOpenInfo = %s\n\r", nWA, hb_ValToExp( aOpenInfo ) )
#endif

   cFullName := Upper( aOpenInfo[ UR_OI_NAME ] )

   /* When there is no ALIAS we will create new one using file name */
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      HB_FNAMESPLIT( cFullName, , @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   ENDIF

   //nMode := IIF( aOpenInfo[ UR_OI_SHARED ], FO_SHARED , FO_EXCLUSIVE ) + ;
   //         IIF( aOpenInfo[ UR_OI_READONLY ], FO_READ, FO_READWRITE )

   hRDDData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )

   IF HB_HHasKey( hRDDData, cFullName )

      aDBFData := hRDDData[ cFullName ]
      aStruct  := aDBFData[ DATABASE_STRUCT ]

   ELSE

      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := HB_LANGERRMSG( EG_OPEN ) + ", memory file not found"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   // Set WorkArea Infos
   aWAData  := USRRDD_AREADATA( nWA )
   aWAData[ WADATA_DATABASE ] := aDBFData  // Put a reference to database
   aWAData[ WADATA_WORKAREA ] := nWA
   aWAData[ WADATA_OPENINFO ] := aOpenInfo // Put open informations

   // Set fields
   UR_SUPER_SETFIELDEXTENT( nWA, Len( aStruct ) )

   FOR EACH aFieldStruct IN aStruct

       aField := ARRAY( UR_FI_SIZE )
       aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
       aField[ UR_FI_TYPE ]    := HB_Decode( aFieldStruct[ DBS_TYPE ], "C", HB_FT_STRING, "L", HB_FT_LOGICAL, "M", HB_FT_MEMO, "D", HB_FT_DATE, "N", IIF( aFieldStruct[ DBS_DEC ] > 0, HB_FT_DOUBLE, HB_FT_INTEGER ) )
       aField[ UR_FI_TYPEEXT ] := 0
       aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
       aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
       UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   /* Add a new open number */
   aDBFData[ DATABASE_OPENNUMBER ]++

   // File already opened in exclusive mode
   // I have to do this check here because, in case of error, AR_CLOSE() is called however
   IF aDBFData[ DATABASE_LOCKED ]

      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := HB_LANGERRMSG( EG_OPEN ) + "(" + ;
                            HB_LANGERRMSG( EG_LOCK ) + " - already opened in exclusive mode)"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   // Open file in exclusive mode
   IF !aOpenInfo[ UR_OI_SHARED ]
      IF aDBFData[ DATABASE_OPENNUMBER ] == 1
         aDBFData[ DATABASE_LOCKED     ] := .T.
      ELSE
         oError := ErrorNew()
         oError:GenCode     := EG_OPEN
         oError:SubCode     := 1000
         oError:Description := HB_LANGERRMSG( EG_OPEN ) + "(" + ;
                               HB_LANGERRMSG( EG_LOCK ) + " - already opened in shared mode)"
         oError:FileName    := aOpenInfo[ UR_OI_NAME ]
         oError:CanDefault  := .T.
         UR_SUPER_ERROR( nWA, oError )
         RETURN HB_FAILURE
      ENDIF
   ENDIF

   IF nResult == HB_SUCCESS
      AR_GOTOP( nWA )
   ENDIF

   RETURN nResult

STATIC FUNCTION AR_CLOSE( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_CLOSE(): nWA = %i\n\r", nWA )
#endif

   IF HB_ISARRAY( aDBFData )
      // decrease open number
      aDBFData[ DATABASE_OPENNUMBER ]--

      // unlock file
      aDBFData[ DATABASE_LOCKED     ] := .F.  // Exclusive mode
   ENDIF

   RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION AR_GETVALUE( nWA, nField, xValue )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aStruct   := aDBFData[ DATABASE_STRUCT  ]
   LOCAL nRecNo    := aWAData[ WADATA_RECNO ]

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_GETVALUE(): nWA = %i, nField = %i, xValue = %s\n\r", nWA, nField, xValue )
#endif

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
   LOCAL xVal

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_PUTVALUE(): nWA = %i, nField = %i, xValue = %s\n\r", nWA, nField, xValue )
#endif

   IF nField > 0 .AND. nField <= Len( aStruct ) .AND. ;
      IIF( ValType( xValue ) == "C" .AND. aStruct[ nField ][ DBS_TYPE ] == "M", .T., ValType( xValue ) == aStruct[ nField ][ DBS_TYPE ] )

      xVal := PutValue( xValue, aStruct[ nField ][ DBS_TYPE ], aStruct[ nField ][ DBS_LEN ], aStruct[ nField ][ DBS_DEC ] )

      //IF aWAData:APPENDACTIVE .OR. aWAData[ WADATA_EOF ]
      //   aWAData:PHANTOM[ nField ]   := xVal
      IF !aWAData[ WADATA_EOF ]
         aRecords[ nRecNo ][ nField ] := xVal
      ENDIF

      RETURN HB_SUCCESS

   ENDIF

   RETURN HB_FAILURE

STATIC FUNCTION AR_GOTO( nWA, nRecord )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL nRecCount := Len( aRecords )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_GOTO(): nWA = %i, nRecord = %i\n\r", nWA, nRecord )
#endif

   //if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
   //   return HB_FAILURE;
   //
   //if( pArea->lpdbPendingRel )
   //{
   //   if( pArea->lpdbPendingRel->isScoped )
   //      SELF_FORCEREL( ( AREAP ) pArea );
   //   else /* Reset parent rel struct */
   //      pArea->lpdbPendingRel = NULL;
   //}
   ///* Update record count */
   //if( ulRecNo > pArea->ulRecCount && pArea->fShared )
   //   pArea->ulRecCount = hb_dbfCalcRecCount( pArea );


   IF nRecord >= 1 .AND. nRecord <= nRecCount

      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .F.
      aWAData[ WADATA_RECNO ] := nRecord

      //pArea->fBof = pArea->fEof = pArea->fValidBuffer = .F.;
      //pArea->fPositioned = .T.;

   ELSEIF nRecCount == 0

      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1

   ELSEIF nRecord <= 0

      aWAData[ WADATA_BOF ]   := .T.
      aWAData[ WADATA_EOF ]   := .F.
      aWAData[ WADATA_RECNO ] := 1


   ELSEIF nRecord > nRecCount

      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .T.
      aWAData[ WADATA_RECNO ] := nRecCount + 1

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_GOTOID( nWA, nRecord )
#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_GOTOID(): nWA = %i, nRecord = %i\n\r", nWA, nRecord )
#endif
   RETURN AR_GOTO( nWA, nRecord )

STATIC FUNCTION AR_GOTOP( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL nRecCount := Len( aRecords )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_GOTOP(): nWA = %i\n\r", nWA )
#endif

   IF nRecCount == 0

      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1

   ELSE

      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .F.
      aWAData[ WADATA_RECNO ] := 1

      IF Set( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
         RETURN AR_SKIPFILTER( nWA, 1 )
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_GOBOTTOM( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_GOBOTTOM(): nWA = %i\n\r", nWA )
#endif

   IF Len( aRecords ) == 0

      aWAData[ WADATA_EOF ]   := aWAData[ WADATA_BOF ] := .T.
      aWAData[ WADATA_RECNO ] := 1

   ELSE

      aWAData[ WADATA_BOF ]   := .F.
      aWAData[ WADATA_EOF ]   := .F.
      aWAData[ WADATA_RECNO ] := Len( aRecords )

      IF Set( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
         RETURN AR_SKIPFILTER( nWA, -1 )
      ENDIF

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_SKIPFILTER( nWA, nRecords )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL lBof, nToSkip

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_SKIPFILTER(): nWA = %i, nRecords = %i\n\r", nWA, nRecords )
#endif

   nToSkip := IIF( nRecords > 0, 1, IIF( nRecords < 0, -1, 0 ) )

   IF nToSkip != 0
      DO WHILE !aWAData[ WADATA_BOF ] .AND. !aWAData[ WADATA_EOF ]
         IF SET( _SET_DELETED ) .AND. aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
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

         // FILTERS

         EXIT
      ENDDO

      IF lBof != NIL
         aWAData[ WADATA_BOF ] := .T.
      ENDIF

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_SKIPRAW( nWA, nRecords )
   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL lBof, lEof
   LOCAL nResult

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_SKIPRAW(): nWA = %i, nRecords = %i\n\r", nWA, nRecords )
#endif

   //if( pArea->lpdbPendingRel )
   //   SELF_FORCEREL( ( AREAP ) pArea );

   //IF nRecCount > 0

      IF nRecords == 0

         lBof := aWAData[ WADATA_BOF ]
         lEof := aWAData[ WADATA_EOF ]

         nResult := AR_GOTO( nWA, aWAData[ WADATA_RECNO ] )

         aWAData[ WADATA_BOF ] := lBof
         aWAData[ WADATA_EOF ] := lEof

      ELSEIF nRecords < 0 .AND. -nRecords >= aWAData[ WADATA_RECNO ]

            nResult := AR_GOTO( nWA, 1 )
            aWAData[ WADATA_BOF ]   := .T.
            // Hack for dbf1.c hack GOTOP
            // aWAData[ WADATA_FORCEBOF ] := .T.

      ELSE

         nResult := AR_GOTO( nWA, aWAData[ WADATA_RECNO ] + nRecords )

      ENDIF

   RETURN nResult // HB_SUCCESS

STATIC FUNCTION AR_BOF( nWA, lBof )
   LOCAL aWAData    := USRRDD_AREADATA( nWA )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_BOF(): nWA = %i, lBof = %s\n\r", nWA, lBof )
#endif

   // This is a hack to protect from dbf1.c skipraw hack
   IF aWAData[ WADATA_FORCEBOF ] .AND. lBof
      aWAData[ WADATA_BOF ] := lBof
      aWAData[ WADATA_FORCEBOF ] := .F.
   ELSE
      lBof := aWAData[ WADATA_BOF ]
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_EOF( nWA, lEof )
   LOCAL aWAData    := USRRDD_AREADATA( nWA )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_EOF(): nWA = %i, lEof = %s\n\r", nWA, lEof )
#endif

   lEof := aWAData[ WADATA_EOF ]

   RETURN HB_SUCCESS

STATIC FUNCTION AR_DELETE( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL oError

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_DELETE(): nWA = %i\n\r", nWA )
#endif

   IF aOpenInfo[ UR_OI_READONLY ]

      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 // EDBF_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   IF ! aWAData[ WADATA_EOF ]
      IF aOpenInfo[ UR_OI_SHARED ] .AND. !( aScan( aWAData[ WADATA_LOCKS ], aWAData[ WADATA_RECNO ] ) > 0  )

         oError := ErrorNew()
         oError:GenCode     := EG_UNLOCKED
         oError:SubCode     := 1022 // EDBF_UNLOCKED
         oError:Description := HB_LANGERRMSG( EG_UNLOCKED )
         oError:FileName    := aOpenInfo[ UR_OI_NAME ]
         UR_SUPER_ERROR( nWA, oError )
         RETURN HB_FAILURE

      ENDIF

      IF Len( aRecInfo ) > 0 .AND. aWAData[ WADATA_RECNO ] <= Len( aRecInfo )
         aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ] := .T.
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_DELETED( nWA, lDeleted )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_DELETED(): nWA = %i, lDeleted = %s\n\r", nWA, lDeleted )
#endif

   // lDeleted := .F.
   IF Len( aRecInfo ) > 0 .AND. aWAData[ WADATA_RECNO ] <= Len( aRecInfo )
      lDeleted := aRecInfo[ aWAData[ WADATA_RECNO ] ][ RECDATA_DELETED ]
   ELSE
      lDeleted := .F.
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION AR_APPEND( nWA, nRecords )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL aRecInfo  := aDBFData[ DATABASE_RECINFO ]
   LOCAL aStruct   := aDBFData[ DATABASE_STRUCT  ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL oError, aRecord

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_APPEND(): nWA = %i, nRecords = %s\n\r", nWA, nRecords )
#endif

   HB_SYMBOL_UNUSED( nRecords )

   IF aOpenInfo[ UR_OI_READONLY ]

      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 // EDBF_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      //oError:OsCode      := fError()
      oError:CanDefault  := .T.
      oError:CanRetry    := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   aRecord := BlankRecord( aStruct )
   AAdd( aRecords, aRecord )
   AAdd( aRecInfo, AR_RECDATAINIT() )
   AR_GOBOTTOM( nWA )

   /* TODO: SHARED ACCESS */

   RETURN HB_SUCCESS

STATIC FUNCTION AR_LOCK( nWA, aLock )
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aLock )
/*
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL nReg     := iif( aLock[ UR_LI_RECORD ] == NIL, aWAData[ WADATA_RECNO ], aLock[ UR_LI_RECORD ] )
   LOCAL aRecInfo

   IF aWAData[ WADATA_EOF ]
      aLock[ UR_LI_RESULT ] := .t.
   ELSE
      aRecInfo := aWAData[ WADATA_DATABASE, DATABASE_RECINFO, nReg ]
      IF aWAData[ WADATA_OPENINFO, UR_OI_SHARED ]
         IF aRecInfo[ RECDATA_LOCKED ] == nWA
            aLock[ UR_LI_RESULT ] := .T.
         ELSEIF aRecInfo[ RECDATA_LOCKED ] != 0
            aLock[ UR_LI_RESULT ] := .F.
         ELSE
            aRecInfo[ RECDATA_LOCKED ] := nWA
            aLock[ UR_LI_RESULT ] := .T.
         ENDIF
      ELSE
         aLock[ UR_LI_RESULT ] := .T.
      ENDIF
   ENDIF
*/
   RETURN SUCCESS

STATIC FUNCTION AR_RECID( nWA, nRecNo )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aRecords  := aDBFData[ DATABASE_RECORDS ]
   LOCAL nRecCount := Len( aRecords )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_RECID(): nWA = %i, nRecNo = %s\n\r", nWA, nRecNo )
#endif

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

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_RECCOUNT(): nWA = %i, nRecords = %s\n\r", nWA, nRecords )
#endif

   nRecords := Len( aRecords )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_ZAP( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL aDBFData  := aWAData[ WADATA_DATABASE ]
   LOCAL aOpenInfo := aWAData[ WADATA_OPENINFO ]
   LOCAL oError

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_ZAP(): nWA = %i\n\r", nWA )
#endif

   IF aOpenInfo[ UR_OI_READONLY ]

      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 // EDBF_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   IF aOpenInfo[ UR_OI_SHARED ]

      oError := ErrorNew()
      oError:GenCode     := EG_SHARED
      oError:SubCode     := 1023 // EDBF_SHARED
      oError:Description := HB_LANGERRMSG( EG_SHARED )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   // empty records
   aDBFData[ DATABASE_RECORDS ] := {}
   aDBFData[ DATABASE_RECINFO ] := {}

   // move to 0 recno
   AR_GOTO( nWA, 0 )

   RETURN HB_SUCCESS

STATIC FUNCTION AR_ORDINFO( nWA, xMsg, xValue )
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( xMsg )
   HB_SYMBOL_UNUSED( xValue )

#ifdef ACTIVATE_DEBUG
   hb_ToOutDebug( "AR_ORDINFO(): nWA = %i, xMsg = %s, xValue = %s\n\r", nWA, xMsg, xValue )
#endif

   /*
   LOCAL hRDDData  := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL aOpenInfo := hRDDData[ nWA ]:OPENINFO
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL oError

   Tracelog( "nWA, xMsg, xValue", nWA, xMsg, xValue )

   IF aOpenInfo[ UR_OI_READONLY ]

      oError := ErrorNew()
      oError:GenCode     := EG_READONLY
      oError:SubCode     := 1025 // EDBF_READONLY
      oError:Description := HB_LANGERRMSG( EG_READONLY )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   IF aOpenInfo[ UR_OI_SHARED ]

      oError := ErrorNew()
      oError:GenCode     := EG_SHARED
      oError:SubCode     := 1023 // EDBF_SHARED
      oError:Description := HB_LANGERRMSG( EG_SHARED )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE

   ENDIF

   aWAData[ ARRAY_RECORDS ] := {}
   aWAData[ ARRAY_RECINFO ] := {}
   AR_GOTO( nWA, 0 )
   */

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
   aMyFunc[ UR_SKIPFILTER   ] := ( @AR_SKIPFILTER()   )
   aMyFunc[ UR_SKIPRAW      ] := ( @AR_SKIPRAW()      )
   aMyFunc[ UR_GOTO         ] := ( @AR_GOTO()         )
   aMyFunc[ UR_GOTOID       ] := ( @AR_GOTOID()       )
   aMyFunc[ UR_GOTOP        ] := ( @AR_GOTOP()        )
   aMyFunc[ UR_GOBOTTOM     ] := ( @AR_GOBOTTOM()     )
   aMyFunc[ UR_RECID        ] := ( @AR_RECID()        )
   aMyFunc[ UR_LOCK         ] := ( @AR_LOCK()         )
   aMyFunc[ UR_RECCOUNT     ] := ( @AR_RECCOUNT()     )
   aMyFunc[ UR_GETVALUE     ] := ( @AR_GETVALUE()     )
   aMyFunc[ UR_PUTVALUE     ] := ( @AR_PUTVALUE()     )
   aMyFunc[ UR_ZAP          ] := ( @AR_ZAP()          )
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

         IF ISCHARACTER( cFullName )
            cFullName := Upper( cFullName )
            // First search if memory dbf exists
            IF HB_HHasKey( hRDDData, cFullName )

               // Get ARRAY data
               aDBFData := hRDDData[ cFullName ]

               // Check if there are current opened workarea
               IF aDBFData[ DATABASE_OPENNUMBER ] > 0

                  oError := ErrorNew()

                  oError:GenCode     := EG_UNSUPPORTED
                  oError:SubCode     := 1000  // EDBF_UNSUPPORTED
                  oError:Description := HB_LANGERRMSG( EG_UNSUPPORTED ) + " (" + ;
                                        "database in use)"
                  oError:FileName    := cFullName
                  oError:CanDefault  := .T.
                  //UR_SUPER_ERROR( 0, oError )
                  Throw( oError )

                  nReturn := HB_FAILURE

               ELSE

                  // Delete database from slot
                  hb_HDel( hRDDData, cFullName )
                  nReturn := HB_SUCCESS

               ENDIF

            ENDIF
         ENDIF

      ELSE

         oError := ErrorNew()

         oError:GenCode     := EG_UNSUPPORTED
         oError:SubCode     := 1000  // EDBF_UNSUPPORTED
         oError:Description := HB_LANGERRMSG( EG_UNSUPPORTED ) + " (" + ;
                               "ARRAYRDD not inizialized)"
         oError:FileName    := cFullName
         oError:CanDefault  := .T.
         //UR_SUPER_ERROR( 0, oError )
         Throw( oError )

         nReturn := HB_FAILURE

      ENDIF
   ELSE

      oError := ErrorNew()

      oError:GenCode     := EG_UNSUPPORTED
      oError:SubCode     := 1000  // EDBF_UNSUPPORTED
      oError:Description := HB_LANGERRMSG( EG_UNSUPPORTED ) + " (" + ;
                            "ARRAYRDD not in use)"
      oError:FileName    := cFullName
      oError:CanDefault  := .T.
      //UR_SUPER_ERROR( 0, oError )
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

         IF ISCHARACTER( cFullName )
            cFullName := Upper( cFullName )
            // First search if memory dbf exists
            IF HB_HHasKey( hRDDData, cFullName )

               nReturn := HB_SUCCESS

            ENDIF
         ENDIF

      ELSE

         oError := ErrorNew()

         oError:GenCode     := EG_UNSUPPORTED
         oError:SubCode     := 1000  // EDBF_UNSUPPORTED
         oError:Description := HB_LANGERRMSG( EG_UNSUPPORTED ) + " (" + ;
                               "ARRAYRDD not inizialized)"
         oError:FileName    := cFullName
         oError:CanDefault  := .T.
         //UR_SUPER_ERROR( 0, oError )
         Throw( oError )

         nReturn := HB_FAILURE

      ENDIF

   ELSE

      oError := ErrorNew()

      oError:GenCode     := EG_UNSUPPORTED
      oError:SubCode     := 1000  // EDBF_UNSUPPORTED
      oError:Description := HB_LANGERRMSG( EG_UNSUPPORTED ) + " (" + ;
                            "ARRAYRDD not in use)"
      oError:FileName    := cFullName
      oError:CanDefault  := .T.
      //UR_SUPER_ERROR( 0, oError )
      Throw( oError )

      nReturn := HB_FAILURE

   ENDIF
   RETURN ( nReturn == HB_SUCCESS )

FUNCTION hb_setArrayRdd( aArray )
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
        xVal := xValue  // No limit for a memo field
   CASE cType == "N"
        xVal := Val( Str( xValue, nLen, nDec ) )
   OTHERWISE
        xVal := xValue
   ENDCASE

   RETURN xVal


STATIC FUNCTION EmptyValue( cType, nLen, nDec )
   LOCAL xVal

   DEFAULT nLen TO 0
   DEFAULT nDec TO 0

   DO CASE
   CASE cType == "C" .OR. cType == "M"
        xVal := Space( nLen )
   CASE cType == "D"
        xVal := CToD( "" )
   CASE cType == "L"
        xVal := .F.
   CASE cType == "N"
        xVal := Val( Str( 0, nLen, nDec ) )
   ENDCASE
RETURN xVal

/******************
* Function .......: hb_Decode( <var>, [ <case1,ret1 [,...,caseN,retN] ] [, <def> ]> ) ---> <xRet>
* Author .........: Francesco Saverio Giudice
* Date of creation: 25/01/1991
* Last revision ..: 24/01/2006 1.13 - rewritten for xHarbour and renamed in hb_Decode()
*
*                   Decode a value from a list.
*******************/
STATIC FUNCTION HB_Decode(...)

   LOCAL aParams, nParams, xDefault
   LOCAL xVal, cKey, xRet
   LOCAL aValues, aResults, n, i, nPos, nLen

   aParams  := hb_aParams()
   nParams  := PCount()
   xDefault := NIL

   DO CASE

   CASE nParams > 1     // More parameters, real case

        xVal := aParams[ 1 ]

        aDel( aParams, 1, .T. ) // Resize params
        nParams := Len( aParams )

        // if I have a odd number of members, last is default
        IF ( nParams % 2 <> 0 )
           xDefault := aTail( aParams )
           // Resize again deleting last
           aDel( aParams, nParams, .T. )
           nParams := Len( aParams )
        ENDIF

        // Ok because I have no other value than default, I will check if it is a complex value
        // like an array or an hash, so I can get it to decode values
        IF xDefault <> NIL .AND. ;
           ( ISARRAY( xDefault ) .OR. ;
             ValType( xDefault ) == "H" )

           // If it is an array I will restart this function creating a linear call
           IF ISARRAY( xDefault ) .AND. Len( xDefault ) > 0

              // I can have a linear array like { 1, "A", 2, "B", 3, "C" }
              // or an array of array couples like { { 1, "A" }, { 2, "B" }, { 3, "C" } }
              // first element tell me what type is

              // couples of values
              IF ISARRAY( xDefault[ 1 ] )

                 //// If i have an array as default, this contains couples of key / value
                 //// so I have to convert in a linear array

                 nLen := Len( xDefault )

                 // Check if array has a default value, this will be last value and has a value
                 // different from an array
                 IF ! ISARRAY( ValType( xDefault[ nLen ] ) )

                    aParams := Array( ( nLen - 1 ) * 2 )

                    n := 1
                    FOR i := 1 TO nLen - 1
                        aParams[ n++ ] := xDefault[ i ][ 1 ]
                        aParams[ n++ ] := xDefault[ i ][ 2 ]
                    NEXT

                    aAdd( aParams, xDefault[ nLen ] )

                 ELSE

                    // I haven't a default

                    aParams := Array( Len( xDefault ) * 2 )

                    n := 1
                    FOR i := 1 TO Len( xDefault )
                        aParams[ n++ ] := xDefault[ i ][ 1 ]
                        aParams[ n++ ] := xDefault[ i ][ 2 ]
                    NEXT

                 ENDIF
              ELSE
                 // I have a linear array

                 aParams := xDefault
              ENDIF


           // If it is an hash, translate it in an array
           ELSEIF ValType( xDefault ) == "H"

              aParams := Array( Len( xDefault ) * 2 )

              i := 1
              FOR EACH cKey IN xDefault:Keys
                  aParams[ i++ ] := cKey
                  aParams[ i++ ] := xDefault[ cKey ]
              NEXT

           ENDIF

           // Then add Decoding value at beginning
           aIns( aParams, 1, xVal, .T. )

           // And run decode() again
           xRet := hb_ExecFromArray( @hb_Decode(), aParams )

        ELSE

           // Ok let's go ahead with real function

           // Combine in 2 lists having elements as { value } and { decode }
           aValues  := Array( nParams / 2 )
           aResults := Array( nParams / 2 )

           i := 1
           FOR n := 1 TO nParams - 1 STEP 2
               aValues[ i ]  := aParams[ n ]
               aResults[ i ] := aParams[ n + 1 ]
               i++
           NEXT

           // Check if value exists (valtype of values MUST be same of xVal,
           // otherwise I will get a runtime error)
           // TODO: Have I to check also between different valtypes, jumping different ?
           nPos := AScan( aValues, {|e| e == xVal } )

           IF nPos == 0 // Not Found, returning default

              xRet := xDefault   // it could be also nil because not present

           ELSE

              xRet := aResults[ nPos ]

           ENDIF

        ENDIF

   CASE nParams == 0    // No parameters
        xRet := NIL

   CASE nParams == 1    // Only value to decode as parameter, return an empty value of itself
        xRet := DecEmptyValue( aParams[ 1 ] )

   ENDCASE

   RETURN xRet

STATIC FUNCTION DecEmptyValue( xVal )
   LOCAL xRet
   LOCAL cType := ValType( xVal )

   SWITCH cType
   CASE "C"  // Char
   CASE "M"  // Memo
        xRet := ""
        EXIT
   CASE "D"  // Date
        xRet := hb_STOD()
        EXIT
   CASE "L"  // Logical
        xRet := .F.
        EXIT
   CASE "N"  // Number
        xRet := 0
        EXIT
   CASE "B"  // code block
        xRet := {|| NIL }
        EXIT
   CASE "A"  // array
        xRet := {}
        EXIT
   CASE "H"  // hash
        xRet := {=>}
        EXIT
   CASE "U"  // undefined
        xRet := NIL
        EXIT
   CASE "O"  // Object
        xRet := NIL   // Or better another value ?
        EXIT
   OTHERWISE
        // Create a runtime error for new datatypes
        xRet := ""
        IF xRet == 0 // BANG!
        ENDIF
   ENDSWITCH

   RETURN xRet
