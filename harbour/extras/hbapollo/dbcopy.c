/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"

static void __sx_Copy( PBYTE cTargetAlias, PBYTE cSourceAlias, PHB_ITEM pArray,
                       int iFieldCount );

HB_FUNC( __SX_DBCOPY ) /* (file,afields,bfor,bwhile,nnext,nrec,lrest,rdd) */
{
   LONG     nNextRecords,
            nRecNo;
   HB_BOOL     lRest,
            bGoTop;
   LONG     nCurrentRecNo,
            ulCount,
            ulRecNo;
   PHB_ITEM pArray        = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pbFor         = hb_param( 3, HB_IT_BLOCK );
   PHB_ITEM pbCondition   = hb_param( 4, HB_IT_BLOCK );
   PHB_ITEM pTemp;
   int      iRDEType;                     /* RDD to use for new DBF file */
   PBYTE    cNewDBFFile;                  /* New DBF File name */
   PBYTE    cSourceAlias;                 /* New Alias for new DBF opened */
   PBYTE    cNewAlias;                    /* New Alias for new DBF opened */
   int      iFieldCount;
   HB_ISIZ  uilenpArray,
            ui;
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   char *   cFieldToCopy;

   /* Check WorkArea */
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOALIAS, NULL, "__SX_DBCOPY" );

   if( ! HB_ISNIL( 9 ) )
      iWorkArea = _sx_select( hb_param( 9, HB_IT_ANY ) );

   /* Alias of currently selected area */
   cSourceAlias  = ( PBYTE ) sx_Alias( 0 );

   /* Current Record Position */
   nCurrentRecNo = sx_RecNo();

   /* New file name passed ? */
   if( ! HB_ISCHAR( 1 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );
   else
   {
      cNewDBFFile = ( PBYTE ) hb_parc( 1 );

      /* Reject if empty string is passed */
      if( strlen( ( char * ) cNewDBFFile ) == 0 )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );

      cNewAlias = ( PBYTE ) "__TEMP__";

      /*    cNewAlias = (PBYTE) _sx_AutoAlias( hb_parc( 1 ) ); */
      if( ! HB_ISNIL( 3 ) && ! HB_ISBLOCK( 3 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );
      }

      if( ! HB_ISNIL( 4 ) && ! HB_ISBLOCK( 4 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );
      }

      /* Next Records */
      if( HB_ISNUM( 5 ) )
         nNextRecords = hb_parnl( 5 );
      else
         nNextRecords = -1;

      /* Record Number to process */
      if( HB_ISNUM( 6 ) )
      {
         nRecNo = hb_parnl( 6 );

         /* nRecNo should be <= LastRec() */
         if( nRecNo > sx_RecCount() || nRecNo < 1 )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );
            return;
         }

         /* nRecNo vs Current Position vs Next Clause */
         if( nNextRecords > 0 && nRecNo <= nCurrentRecNo )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBCOPY" );
            return;
         }
      }
      else
         nRecNo = -1;

      /* Process all records ? */
      if( HB_ISLOG( 7 ) )
         lRest = hb_parl( 7 );
      else
         lRest = HB_TRUE;

      if( nNextRecords == -1 && nRecNo == -1 )
      {
         lRest   = HB_TRUE;
         bGoTop  = HB_TRUE;
      }
      else
         bGoTop = HB_FALSE;

      /* RDE Type Passed ? */
      if( HB_ISCHAR( 8 ) )
      {
         char * cRDDChosen = ( char * ) hb_parc( 8 );
         iRDEType = _sx_CheckRDD( cRDDChosen );
      }
      else
         iRDEType = i_sxApi_RDD_Default;

      /* Now Processing Data ..... */
      /* All Records Until Eof() With For And Condition */
      sx_Select( sx_WorkArea( cSourceAlias ) );
      iFieldCount   = sx_FieldCount();

      /* Checking Array of Fields */
      uilenpArray   = hb_arrayLen( pArray );
      if( uilenpArray == 0 )               /* Not Field Not Specified assumed ALL */
      {
         PHB_ITEM pFieldName = hb_itemNew( NULL );
         hb_arraySize( pArray, iFieldCount );
         for( ui = 0; ui < iFieldCount; ui++ )
         {
            cFieldToCopy  = ( char * ) sx_FieldName( ( WORD ) ( ui + 1 ) );
            pTemp         = hb_itemPutC( NULL, cFieldToCopy );
            hb_arraySet( pArray, ui + 1, pTemp );
            hb_itemRelease( pTemp );
         }

         if( pFieldName )
            hb_itemRelease( pFieldName );

         /* Create DBF Based on Source Structure */
      }

      /* Create Custom DBF based on FIELDS clause */
      if( ! _sx_CopyStructure( cNewDBFFile, pArray ) )
         return;
      if( ! sx_Use( cNewDBFFile, cNewAlias, EXCLUSIVE, ( WORD ) iRDEType ) )
         return;

      /* Back to Source Alias */
      sx_Select( sx_WorkArea( cSourceAlias ) );

      if( bGoTop && lRest && HB_ISBLOCK( 3 ) && HB_ISBLOCK( 4 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && HB_ISNIL( 3 ) && HB_ISBLOCK( 4 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && HB_ISBLOCK( 3 ) && HB_ISNIL( 4 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && HB_ISNIL( 3 ) && HB_ISNIL( 4 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
            sx_Skip( 1 );
         }
      }
      /* nNextRecords Clause */
      /* lRest is assumed HB_FALSE */
      else if( ! bGoTop && ( nNextRecords > 0 ) )
      {
         ulCount = 0;
         if( HB_ISBLOCK( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( ulCount <= nNextRecords &&
                   _sx_Eval( pbCondition ) &&
                   ! sx_Eof() )
            {
               if( _sx_Eval( pbFor ) )
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( ulCount <= nNextRecords &&
                   _sx_Eval( pbCondition ) &&
                   ! sx_Eof() )
            {
               __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISBLOCK( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ulCount <= nNextRecords && ! sx_Eof() )
            {
               if( _sx_Eval( pbFor ) )
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ulCount < nNextRecords && ! sx_Eof() )
            {
               __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
               sx_Skip( 1 );
               ulCount++;
            }
         }
      }
      /* nNextRecords Clause with nRecNo Clause */
      /* lRest is assumed HB_FALSE */
      else if( ! bGoTop && ( nNextRecords > 0 ) && ( nRecNo > 0 ) )
      {
         ulCount = 0;
         if( HB_ISBLOCK( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( ulCount <= nNextRecords &&
                   _sx_Eval( pbCondition ) &&
                   ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( ulCount <= nNextRecords &&
                   _sx_Eval( pbCondition ) &&
                   ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISBLOCK( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ulCount <= nNextRecords && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ulCount <= nNextRecords && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
      }
      /* nRecNo Clause with no nNextRecords Clause */
      /* lRest is assumed HB_FALSE */
      else if( ! bGoTop && ( nRecNo > 0 ) )
      {
         if( HB_ISBLOCK( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISBLOCK( 4 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( HB_ISBLOCK( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( HB_ISNIL( 3 ) && HB_ISNIL( 4 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_Copy( cNewAlias, cSourceAlias, pArray, iFieldCount );
                  break;
               }

               sx_Skip( 1 );
            }
         }
      }

      /* Free allocated memory */
      sx_Select( sx_WorkArea( cNewAlias ) ); /* Select New Area */
      sx_Commit();
      sx_Close();                            /* Close It */
      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
}

static void __sx_Copy( PBYTE cTarget, PBYTE cSource, PHB_ITEM pArray,
                       int iFieldCount )
{
   char cRecord[ 256 ];
   HB_ISIZ uiLen = hb_arrayLen( pArray );

   if( uiLen == 0 )   /* Assumed all fields are copied */
   {
      /* Currently in Source Area */
      sx_GetRecord( ( PBYTE ) cRecord );                 /* Get Record from source */
      sx_Select( sx_WorkArea( cTarget ) );               /* Select New Area */
      sx_AppendBlank();                                  /* Append Blank Record */
      sx_PutRecord( ( PBYTE ) cRecord );                 /* Put the new record */
      sx_Select( sx_WorkArea( cSource ) );               /* Select Source Area */
   }
   else
   {
      int      ui,
               uiNew;
      char *   cFieldName,
      * cFieldToCopy,
      * cFieldType;
      PBYTE    cMemo;
      PVOID    vVariant;
      HB_BOOL     lTrue;
      double   vDouble;

      sx_Select( sx_WorkArea( cTarget ) );               /* Select New Area */
      sx_AppendBlank();                                  /* Append Blank Record */

      for( ui = 1; ui <= iFieldCount; ui++ )             /* Field Names of Source */
      {
         cFieldName = ( char * ) sx_FieldName( ( WORD ) ui );
         for( uiNew = 1; uiNew <= uiLen; uiNew++ )        /* Check The ARray Passed */
         {
            cFieldToCopy = ( char * ) hb_arrayGetC( pArray, uiNew );
            if( cFieldToCopy )
            {
               if( strcmp( cFieldToCopy, cFieldName ) == 0 )
               {
                  sx_Select( sx_WorkArea( cSource ) );       /* Select Source Area */
                  cFieldType = ( char * ) sx_FieldType( ( PBYTE ) cFieldName );
                  switch( *cFieldType )
                  {
                     case 'N':
                        vDouble = sx_GetDouble( ( PBYTE ) cFieldName );
                        sx_Select( sx_WorkArea( cTarget ) ); /* Select New Area */
                        sx_Replace( ( PBYTE ) cFieldToCopy, R_DOUBLE, ( PVOID ) &vDouble );
                        break;

                     case 'C':
                        vVariant = ( PVOID ) sx_GetString( ( PBYTE ) cFieldName );
                        sx_Select( sx_WorkArea( cTarget ) ); /* Select New Area */
                        sx_Replace( ( PBYTE ) cFieldToCopy, R_CHAR, vVariant );
                        break;

                     case 'M':
                        cMemo = ( PBYTE ) sx_GetMemo( ( PBYTE ) cFieldName, 0 );
                        sx_Select( sx_WorkArea( cTarget ) ); /* Select New Area */
                        sx_Replace( ( PBYTE ) cFieldToCopy, R_MEMO, ( PVOID ) cMemo );
                        if( lstrlen( ( char * ) cMemo ) ) /* TOFIX: lstrlen() usage */
                           sx_MemDealloc( cMemo );
                        break;

                     case 'D':
                        vVariant = ( PVOID ) sx_GetDateString( ( PBYTE ) cFieldName );
                        sx_Select( sx_WorkArea( cTarget ) ); /* Select New Area */
                        sx_Replace( ( PBYTE ) cFieldToCopy, R_DATESTR, vVariant );
                        break;

                     case 'L':
                        lTrue = ( HB_BOOL ) sx_GetLogical( ( PBYTE ) cFieldName );
                        sx_Select( sx_WorkArea( cTarget ) ); /* Select New Area */
                        sx_Replace( ( PBYTE ) cFieldToCopy, R_LOGICAL, ( PVOID ) &lTrue );
                        break;
                  }

                  hb_xfree( cFieldToCopy );
               }
               else
                  hb_xfree( cFieldToCopy );
            }
         }  /* end for( uiNew = 0; uiNew < uiLen ; uiNew ++ ) */

         sx_Select( sx_WorkArea( cSource ) ); /* Select Source Area */
      }
   }

   sx_Select( sx_WorkArea( cSource ) );       /* Select Source Area */
}
