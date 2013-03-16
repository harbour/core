/*
 * Harbour Project source code:
 *    HSCDX
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
 * A simple RDD which adds automatically update HSX indexes to DBFCDX
 * To create new HSX index for current work area use: HSX_CREATE()
 * To open already existing one use HSX_OPEN(),
 * To close use: HSX_CLOSE()
 * To retieve an handle use: HSX_HANDLE()
 */

#include "rddsys.ch"
#include "hbusrrdd.ch"
#include "fileio.ch"
#include "dbinfo.ch"

ANNOUNCE HSCDX

/*
 * methods: NEW and RELEASE receive pointer to work area structure
 * not work area number. It's necessary because the can be executed
 * before work area is allocated
 * these methods does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */

STATIC FUNCTION _HSX_NEW( pWA )

   LOCAL aWData := { .F., {}, {} }

   /*
    * Set in our private AREA item the array where we will kepp HSX indexes
    * and HOT buffer flag
    */

   USRRDD_AREADATA( pWA, aWData )

   RETURN HB_SUCCESS

STATIC FUNCTION _HSX_CLOSE( nWA )

   LOCAL aWData := USRRDD_AREADATA( nWA ), nHSX

   /* close all HSX indexes */

   FOR EACH nHSX IN aWData[ 2 ]
      hs_Close( nHSX )
   NEXT

   /* clean the HSX index array */
   ASize( aWData[ 2 ], 0 )
   ASize( aWData[ 3 ], 0 )

   /* call SUPER CLOSE method to close parent RDD */

   RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION _HSX_GOCOLD( nWA )

   LOCAL nResult, aWData, nHSX, nRecNo, nKeyNo

   nResult := UR_SUPER_GOCOLD( nWA )
   IF nResult == HB_SUCCESS
      aWData := USRRDD_AREADATA( nWA )
      IF aWData[ 1 ]
         IF ! Empty( aWData[ 2 ] )
            nRecNo := RecNo()
            /* update HSX indexes */
            FOR EACH nHSX IN aWData[ 2 ]
               nKeyNo := hs_KeyCount( nHSX )
               DO WHILE nKeyNo >= 0 .AND. nKeyNo < nRecNo
                  nKeyNo := hs_Add( nHSX, "" )
               ENDDO
               IF nKeyNo >= nRecNo
                  hs_Replace( nHSX,, nRecNo )
               ENDIF
            NEXT
         ENDIF
         aWData[ 1 ] := .F.
      ENDIF
   ENDIF

   RETURN nResult

STATIC FUNCTION _HSX_GOHOT( nWA )

   LOCAL nResult, aWData

   nResult := UR_SUPER_GOHOT( nWA )
   IF nResult == HB_SUCCESS
      aWData := USRRDD_AREADATA( nWA )
      aWData[ 1 ] := .T.
   ENDIF

   RETURN nResult

STATIC FUNCTION _HSX_APPEND( nWA, lUnlockAll )

   LOCAL nResult, aWData

   nResult := UR_SUPER_APPEND( nWA, lUnlockAll )
   IF nResult == HB_SUCCESS
      aWData := USRRDD_AREADATA( nWA )
      aWData[ 1 ] := .T.
   ENDIF

   RETURN nResult

/*
 * Three public functions for CREATE, OPEN and CLOSE HSX indexes bound
 * with current work are and automatically updated.
 */

FUNCTION HSX_CREATE( cFile, cExpr, nKeySize, nBufSize, lCase, nFiltSet )

   LOCAL aWData, nHsx := -1, nOpenMode

   IF ! Used()
   ELSEIF ! rddName() == "HSCDX"
   ELSE
      aWData := USRRDD_AREADATA( Select() )
      nOpenMode := iif( dbInfo( DBI_SHARED ), 1, 0 ) + ;
         iif( dbInfo( DBI_ISREADONLY ), 2, 0 )
      nHsx := hs_Index( cFile, cExpr, nKeySize, nOpenMode, ;
         nBufSize, lCase, nFiltSet )
      IF nHsx >= 0
         AAdd( aWData[ 2 ], nHsx )
         AAdd( aWData[ 3 ], cFile )
      ENDIF
   ENDIF

   RETURN nHsx

FUNCTION HSX_OPEN( cFile, nBufSize )

   LOCAL aWData, nHsx, nOpenMode

   IF ! Used()
   ELSEIF ! rddName() == "HSCDX"
   ELSE
      aWData := USRRDD_AREADATA( Select() )
      nOpenMode := iif( dbInfo( DBI_SHARED ), 1, 0 ) + ;
         iif( dbInfo( DBI_ISREADONLY ), 2, 0 )
      nHsx := hs_Open( cFile, nBufSize, nOpenMode )
      IF nHsx >= 0
         AAdd( aWData[ 2 ], nHsx )
         AAdd( aWData[ 3 ], cFile )
      ENDIF
   ENDIF

   RETURN NIL

FUNCTION HSX_CLOSE( xHSX )

   LOCAL aWData, nSlot

   IF Used() .AND. rddName() == "HSCDX"
      aWData := USRRDD_AREADATA( Select() )
      IF HB_ISNUMERIC( xHSX )
         nSlot := AScan( aWData[ 2 ], xHSX )
      ELSEIF HB_ISSTRING( xHSX )
         nSlot := AScan( aWData[ 3 ], {| _1 | _1 == xHSX } )
      ELSE
         nSlot := 0
      ENDIF
      IF nSlot != 0
         hb_ADel( aWData[ 2 ], nSlot, .T. )
         hb_ADel( aWData[ 3 ], nSlot, .T. )
      ENDIF
   ENDIF

   RETURN NIL

FUNCTION HSX_HANDLE( cFile )

   LOCAL aWData, nSlot

   IF Used() .AND. rddName() == "HSCDX"
      aWData := USRRDD_AREADATA( Select() )
      nSlot := AScan( aWData[ 3 ], {| _1 | _1 == cFile } )
      IF nSlot != 0
         RETURN aWData[ 2, nSlot ]
      ENDIF
   ENDIF

   RETURN -1

FUNCTION HSX_FILE( nHsx )

   LOCAL aWData, nSlot

   IF Used() .AND. rddName() == "HSCDX"
      aWData := USRRDD_AREADATA( Select() )
      nSlot := AScan( aWData[ 3 ], nHsx )
      IF nSlot != 0
         RETURN aWData[ 3, nSlot ]
      ENDIF
   ENDIF

   RETURN ""

FUNCTION HSX_GET( nSlot )

   LOCAL aWData

   IF Used() .AND. rddName() == "HSCDX"
      aWData := USRRDD_AREADATA( Select() )
      IF nSlot > 0 .AND. nSlot <= Len( aWData[ 2 ] )
         RETURN aWData[ 2, nSlot ]
      ENDIF
   ENDIF

   RETURN -1

/* Force linking DBFCDX from which our RDD inherits */

REQUEST DBFCDX

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */

FUNCTION HSCDX_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, pSuperRddID )

   LOCAL cSuperRDD := "DBFCDX" /* We are inheriting from DBFCDX */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_NEW    ] := ( @_HSX_NEW()    )
   aMyFunc[ UR_CLOSE  ] := ( @_HSX_CLOSE()  )
   aMyFunc[ UR_GOCOLD ] := ( @_HSX_GOCOLD() )
   aMyFunc[ UR_GOHOT  ] := ( @_HSX_GOHOT()  )
   aMyFunc[ UR_APPEND ] := ( @_HSX_APPEND() )

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
      cSuperRDD, aMyFunc, pSuperRddID )

/*
 * Register our HSCDX at program startup
 */

INIT PROCEDURE HSCDX_INIT()

   rddRegister( "HSCDX", RDT_FULL )

   RETURN
