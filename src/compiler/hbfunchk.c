/*
 * Harbour Project source code:
 * Compile time RTL argument checking
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
 */

#include "hbcomp.h"

/* NOTE: iMinParam = -1, means no lower limit
 *       iMaxParam = -1, means no upper limit
 */

typedef struct
{
   const char * cFuncName;                /* function name              */
   int          iMinParam;                /* min no of parms it needs   */
   int          iMaxParam;                /* max no of parms need       */
} HB_FUNCINFO, * PHB_FUNCINFO;


/* NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
 */
static const HB_FUNCINFO s_stdFunc[] =
{
   { "AADD"      , 2,  2 },
   { "ABS"       , 1,  1 },
   { "ASC"       , 1,  1 },
   { "AT"        , 2,  2 },
   { "BOF"       , 0,  0 },
   { "BREAK"     , 0,  1 },
   { "CDOW"      , 1,  1 },
   { "CHR"       , 1,  1 },
   { "CMONTH"    , 1,  1 },
   { "COL"       , 0,  0 },
   { "CTOD"      , 1,  1 },
   { "DATE"      , 0,  0 },
   { "DAY"       , 1,  1 },
   { "DELETED"   , 0,  0 },
   { "DEVPOS"    , 2,  2 },
   { "DOW"       , 1,  1 },
   { "DTOC"      , 1,  1 },
   { "DTOS"      , 1,  1 },
   { "EMPTY"     , 1,  1 },
   { "EOF"       , 0,  0 },
   { "EVAL"      , 1, -1 },
   { "EXP"       , 1,  1 },
   { "FCOUNT"    , 0,  0 },
   { "FIELDNAME" , 1,  1 },
   { "FILE"      , 1,  1 },
   { "FLOCK"     , 0,  0 },
   { "FOUND"     , 0,  0 },
   { "INKEY"     , 0,  2 },
   { "INT"       , 1,  1 },
   { "LASTREC"   , 0,  0 },
   { "LEFT"      , 2,  2 },
   { "LEN"       , 1,  1 },
   { "LOCK"      , 0,  0 },
   { "LOG"       , 1,  1 },
   { "LOWER"     , 1,  1 },
   { "LTRIM"     , 1,  1 },
   { "MAX"       , 2,  2 },
   { "MIN"       , 2,  2 },
   { "MONTH"     , 1,  1 },
   { "PCOL"      , 0,  0 },
   { "PCOUNT"    , 0,  0 },
   { "PROW"      , 0,  0 },
   { "QSELF"     , 0,  0 },
   { "RECCOUNT"  , 0,  0 },
   { "RECNO"     , 0,  0 },
   { "REPLICATE" , 2,  2 },
   { "RLOCK"     , 0,  0 },
   { "ROUND"     , 2,  2 },
   { "ROW"       , 0,  0 },
   { "RTRIM"     , 1,  1 },
   { "SECONDS"   , 0,  0 },
   { "SELECT"    , 0,  1 },
   { "SETPOS"    , 2,  2 },
   { "SETPOSBS"  , 0,  0 },
   { "SPACE"     , 1,  1 },
   { "SQRT"      , 1,  1 },
   { "STR"       , 1,  3 },
   { "SUBSTR"    , 2,  3 },
   { "TIME"      , 0,  0 },
   { "TRANSFORM" , 2,  2 },
   { "TRIM"      , 1,  1 },
   { "TYPE"      , 1,  1 },
   { "UPPER"     , 1,  1 },
   { "VAL"       , 1,  1 },
   { "VALTYPE"   , 1,  1 },
   { "WORD"      , 1,  1 },
   { "YEAR"      , 1,  1 }
};

#define HB_STD_FUNCOUNT  ( sizeof( s_stdFunc ) / sizeof( HB_FUNCINFO ) )

HB_BOOL hb_compFunCallCheck( HB_COMP_DECL, const char * szFuncCall, int iArgs )
{
   unsigned int uiFirst = 0, uiLast = HB_STD_FUNCOUNT - 1, uiMiddle;
   int iLen = ( int ) strlen( szFuncCall ), iCmp;

   /* Respect 4 or more letters shortcuts
    * SECO() is not allowed because of Clipper function SECONDS()
    * however SECO32() is a valid name.
    */
   if( iLen < 4 )
      iLen = 4;
   do
   {
      uiMiddle = ( uiFirst + uiLast ) >> 1;
      iCmp = strncmp( szFuncCall, s_stdFunc[ uiMiddle ].cFuncName, iLen );
      if( iCmp <= 0 )
         uiLast = uiMiddle;
      else
         uiFirst = uiMiddle + 1;
   }
   while( uiFirst < uiLast );

   if( uiFirst != uiMiddle )
      iCmp = strncmp( szFuncCall, s_stdFunc[ uiFirst ].cFuncName, iLen );

   if( iCmp == 0 )
   {
      const HB_FUNCINFO * pFunc = &s_stdFunc[ uiFirst ];

      if( ( pFunc->iMinParam != -1 && iArgs < pFunc->iMinParam ) ||
          ( pFunc->iMaxParam != -1 && iArgs > pFunc->iMaxParam ) )
      {
         char szMsg[ 64 ];

         if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_HARBOUR ) )
         {
            if( pFunc->iMinParam == pFunc->iMaxParam )
               hb_snprintf( szMsg, sizeof( szMsg ), "\nPassed: %i, expected: %i", iArgs, pFunc->iMinParam );
            else if( pFunc->iMaxParam == -1 )
               hb_snprintf( szMsg, sizeof( szMsg ), "\nPassed: %i, expected at least: %i", iArgs, pFunc->iMinParam );
            else if( pFunc->iMinParam == -1 )
               hb_snprintf( szMsg, sizeof( szMsg ), "\nPassed: %i, expected less than: %i", iArgs, pFunc->iMaxParam );
            else
               hb_snprintf( szMsg, sizeof( szMsg ), "\nPassed: %i, expected from: %i to: %i", iArgs, pFunc->iMinParam, pFunc->iMaxParam );
         }
         else
            szMsg[ 0 ] = '\0';

         hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CHECKING_ARGS, szFuncCall, szMsg );

         return HB_FALSE;
      }
   }

   return HB_TRUE;
}
