/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * APPEND FROM & COPY TO RDD commands
 *
 * Copyright 2000 Harrier <harrier@topconnect.cl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

// as we are in C, the code is upside down,
//  find __SBAPP & __DBCOPY at the bottom

// create a new AREANODE and open it's Area
// I assume the file exists, else extra code must be added to create it
static LPAREANODE GetTheOtherArea( char *szDriver, char * szFileName )
{
  LPAREANODE pAreaNode;
  LPRDDNODE  pRDDNode;
  USHORT     uiRddID;
  DBOPENINFO pInfo;

  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
    return NULL;
  }

/* get the new area node for this AREA */
  pAreaNode =  hb_rddNewAreaNode( pRDDNode, uiRddID );

/* Fill pInfo structure */
  memset( &pInfo, 0, sizeof(DBOPENINFO) );
  pInfo.uiArea = uiRddID;
  pInfo.abName = ( BYTE * )  hb_xgrab( _POSIX_PATH_MAX + 1 );
  strcpy( ( char * ) pInfo.abName, szFileName );
  pInfo.atomAlias = ( BYTE * ) "TMPAREA";
  pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
  pInfo.fReadonly = FALSE;
  ( ( AREAP ) pAreaNode->pArea )->uiArea = uiRddID;

  if( SELF_OPEN( ( AREAP ) pAreaNode->pArea, &pInfo ) == FAILURE )
  {
    hb_errRT_DBCMD( EG_OPEN, 0, NULL, "DBAPP" ); // Could not open it
    SELF_RELEASE( ( AREAP ) pAreaNode->pArea );
    hb_xfree( pAreaNode );
    return NULL;
  }
  return pAreaNode;
}

// check if the field is on the Fields Array
static BOOL IsFieldIn( char * fieldName, PHB_ITEM pFields )
{
  USHORT i, uiFields = ( USHORT ) hb_arrayLen( pFields );
  for ( i=0; i<uiFields; i++ )
  {
    PHB_ITEM pField = pFields->item.asArray.value->pItems + i;
    if ( strcmp( fieldName, (char *)pField->item.asString.value ) == 0 )
      return TRUE;
  }
  return FALSE;
}

// move the Field Data between areas by name
static void rddMoveFields( AREAP pAreaFrom, AREAP pAreaTo, PHB_ITEM pFields )
{
  USHORT   i;
  PHB_ITEM fieldValue;

  fieldValue = hb_itemNew( NULL );
  for ( i=0 ; i<pAreaFrom->uiFieldCount; i++ )
  {
    // list or field in the list?
    if ( !pFields || IsFieldIn( (( PHB_DYNS )(pAreaFrom->lpFields + i)->sym )->pSymbol->szName,  pFields ))
    {
      SELF_GETVALUE( pAreaFrom, i+1, fieldValue );
      SELF_PUTVALUE( pAreaTo, i+1, fieldValue );
    }
  }
  hb_itemRelease( fieldValue );
}

// move the records filtering if apropiate
static ERRCODE rddMoveRecords( char *cAreaFrom, char *cAreaTo, PHB_ITEM pFields,
                               PHB_ITEM pFor, PHB_ITEM pWhile, LONG lNext,
                               ULONG lRec, BOOL bRest, char *cDriver )
{
  char     * szFileName, * szDriver;
  LONG       toGo=lNext;
  BOOL       bFor, bWhile;
  BOOL       keepGoing=TRUE;
  AREAP      pAreaFrom;
  AREAP      pAreaTo;
  DBEVALINFO pEvalInfo;
  LPAREANODE pAreaRelease=NULL;
  LPAREANODE s_pCurrAreaSaved=s_pCurrArea;

  HB_SYMBOL_UNUSED( bRest );

  if ( !s_pCurrArea )  // We need a current Area to APPEND TO or FROM
  {
     hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
     return EG_NOTABLE;
  }

  // get the RDD Driver to use for the "other" Area
  if( cDriver )
     szDriver = cDriver;
  else
     szDriver = s_szDefDriver;

  if( !cAreaFrom && ! cAreaTo )         // File is needed
  {
     hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
     return EG_ARG;
  }

  if ( cAreaTo ) // it's a COPY TO
  {
    pAreaRelease = GetTheOtherArea( szDriver, cAreaTo );
    pAreaTo = (AREAP) pAreaRelease->pArea;
  }
  else
    pAreaTo = (AREAP) s_pCurrArea->pArea;


  if ( cAreaFrom ) // it's an APPEND FROM
  {                // make it current
    pAreaRelease = s_pCurrArea = GetTheOtherArea( szDriver, cAreaFrom );
    pAreaFrom =  (AREAP) pAreaRelease->pArea;
  }
  else
    pAreaFrom = (AREAP) s_pCurrArea->pArea;

  // or one or the other but necer none
  if ( !pAreaRelease )  // We need another Area to APPEND TO
  {
     hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
     return EG_NOTABLE;
  }

  if ( lRec > 0 )                 // only one record
    SELF_GOTO( pAreaFrom, lRec ); // go there

  // move those records assuming we are positioned on one.
  while( keepGoing )
  {
    keepGoing = FALSE;
    if( !pAreaFrom->fEof ) // until eof or an evaluation failed
    {
       if( pWhile )
          bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
       else
          bWhile = TRUE;

       if( pFor )
          bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
       else
          bFor = TRUE;

      if( bWhile && bFor && (!lNext || toGo > 0 )) // candidate?
      {
        PHB_ITEM lpFields = pFields ? pFields->item.asArray.value->ulLen ? pFields : NULL : NULL;
        SELF_APPEND( ( AREAP ) pAreaTo, FALSE );   // put a new one on TO Area
        rddMoveFields( pAreaFrom, pAreaTo, lpFields ); // move the data
        if ( lRec == 0 ) // only one record?
          keepGoing = TRUE;
        else
          continue;
      }
      toGo--;                     // one less to go
      SELF_SKIP( pAreaFrom, 1L ); // get the next one
    }
  }

  s_pCurrArea = s_pCurrAreaSaved;  // set current WorkArea to initial state

  // Close the File
  SELF_CLOSE( ( AREAP ) pAreaRelease->pArea );
  SELF_RELEASE( ( AREAP ) pAreaRelease->pArea );
  hb_xfree( pAreaRelease );

/* should the following be released?
  if ( pFields )
    hb_itemRelease( pFields );
  if ( pFor )
    hb_itemRelease( pFor );
  if ( pWhile )
    hb_itemRelease( pWhile );
*/

  return SUCCESS;
}

HB_FUNC( __DBAPP )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  hb_parc( 1 ),                /* File From */
                     NULL,                        /* TO current area */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parc( 8 ) : NULL ); /* RDD */
  }
}

HB_FUNC( __DBCOPY )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  NULL,                        /* fro CURRENT Area */
                     hb_parc( 1 ),                /* To File */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parc( 8 ) : NULL ); /* RDD */
  }
}


