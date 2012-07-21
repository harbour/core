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

HB_FUNC( SX_DBEVAL )   /* (bBlock,bFor,bCOndition,nNextRecords,nRecords,lRest) */
{
   /* Param 7 added as work area selected 2003.05.08 */
   LONG     nNextRecords,
            nRecNo;
   HB_BOOL     lRest,
            bGoTop;
   LONG     nCurrentRecNo,
            ulCount,
            ulRecNo;
   WORD     wPreviousArea = SX_DUMMY_NUMBER;
   PHB_ITEM pbBlock       = hb_param( 1, HB_IT_BLOCK );
   PHB_ITEM pbFor         = hb_param( 2, HB_IT_BLOCK );
   PHB_ITEM pbCondition   = hb_param( 3, HB_IT_BLOCK );

   if( ! _sx_Used() )
   {
      hb_itemRelease( pbBlock );
      hb_itemRelease( pbFor );
      hb_itemRelease( pbCondition );
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_DBEVAL" );
   }

   if( ! HB_ISNIL( 7 ) )
      wPreviousArea = _sx_select( hb_param( 7, HB_IT_ANY ) );

   /* Current Record Position */
   nCurrentRecNo = sx_RecNo();

   /* CoceBlock passed ? */
   if( ! HB_ISBLOCK( 1 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "SX_DBEVAL" );

   if( ! HB_ISNIL( 2 ) && ! HB_ISBLOCK( 2 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "SX_DBEVAL" );

   if( ! HB_ISNIL( 3 ) && ! HB_ISBLOCK( 3 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "SX_DBEVAL" );

   /* Next Records */
   if( HB_ISNUM( 4 ) )
      nNextRecords = hb_parnl( 4 );
   else
      nNextRecords = -1;

   /* Record Number to process */
   if( HB_ISNUM( 5 ) )
   {
      nRecNo = hb_parnl( 5 );

      /* nRecNo should be <= LastRec() */
      if( nRecNo > sx_RecCount() || nRecNo < 1 )
         return;

      /* nRecNo vs Current Position vs Next Clause */
      if( nNextRecords > 0 && nRecNo <= nCurrentRecNo )
         return;
   }
   else
      nRecNo = -1;

   /* Process all records ? */
   if( HB_ISLOG( 6 ) )
      lRest = hb_parl( 6 );
   else
      lRest = HB_TRUE;

   if( nNextRecords == -1 && nRecNo == -1 )
   {
      lRest   = HB_TRUE;
      bGoTop  = HB_TRUE;
   }
   else
      bGoTop = HB_FALSE;

   /* Now Processing Data ..... */
   /* All Records Until Eof() With For And Condition */
   if( bGoTop && lRest && HB_ISBLOCK( 2 ) && HB_ISBLOCK( 3 ) )
   {
      sx_GoTop();
      while( _sx_Eval( pbCondition ) && ! sx_Eof() )
      {
         if( _sx_Eval( pbFor ) )
            _sx_Eval( pbBlock );

         sx_Skip( 1 );
      }
   }
   else if( bGoTop && lRest && HB_ISNIL( 2 ) && HB_ISBLOCK( 3 ) )
   {
      sx_GoTop();
      while( _sx_Eval( pbCondition ) && ! sx_Eof() )
      {
         _sx_Eval( pbBlock );
         sx_Skip( 1 );
      }
   }
   else if( bGoTop && lRest && HB_ISBLOCK( 2 ) && HB_ISNIL( 3 ) )
   {
      sx_GoTop();
      while( ! sx_Eof() )
      {
         if( _sx_Eval( pbFor ) )
            _sx_Eval( pbBlock );

         sx_Skip( 1 );
      }
   }
   else if( bGoTop && lRest && HB_ISNIL( 2 ) && HB_ISNIL( 3 ) )
   {
      sx_GoTop();
      while( ! sx_Eof() )
      {
         _sx_Eval( pbBlock );
         sx_Skip( 1 );
      }
   }
   /* nNextRecords Clause */
   /* lRest is assumed HB_FALSE */
   else if( ! bGoTop && ( nNextRecords > 0 ) )
   {
      ulCount = 0;
      if( HB_ISBLOCK( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( ulCount <= nNextRecords && _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               _sx_Eval( pbBlock );

            sx_Skip( 1 );

            ulCount++;
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( ulCount <= nNextRecords && _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            _sx_Eval( pbBlock );
            sx_Skip( 1 );
            ulCount++;
         }
      }
      else if( HB_ISBLOCK( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ulCount <= nNextRecords && ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               _sx_Eval( pbBlock );
            sx_Skip( 1 );
            ulCount++;
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ulCount < nNextRecords && ! sx_Eof() )
         {
            _sx_Eval( pbBlock );
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
      if( HB_ISBLOCK( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( ulCount <= nNextRecords && _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
            ulCount++;
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( ulCount <= nNextRecords && _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
            ulCount++;
         }
      }
      else if( HB_ISBLOCK( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ulCount <= nNextRecords && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
            ulCount++;
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ulCount <= nNextRecords && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo )
            {
               _sx_Eval( pbBlock );
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
      if( HB_ISBLOCK( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo && _sx_Eval( pbFor ) )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISBLOCK( 3 ) )
      {
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
         }
      }
      else if( HB_ISBLOCK( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
         }
      }
      else if( HB_ISNIL( 2 ) && HB_ISNIL( 3 ) )
      {
         while( ! sx_Eof() )
         {
            ulRecNo = sx_RecNo();
            if( nRecNo == ulRecNo )
            {
               _sx_Eval( pbBlock );
               break;
            }

            sx_Skip( 1 );
         }
      }
   }

   /* Back to Previous Work Area */
   if( ! ( wPreviousArea == SX_DUMMY_NUMBER ) )
      sx_Select( wPreviousArea );
}
