/*
 * Harbour Project source code:
 * sql_sprintf() function
 *
 * Copyright 2008 Xavi <jarabal/at/gmail.com>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"

static void STAItm( PHB_ITEM pItmPar )
{
   HB_UINT      i, ulItmPar = ( HB_UINT ) hb_itemGetCLen( pItmPar );
   const char * cItmPar = hb_itemGetCPtr( pItmPar ), * c;
   char *       cRes;

   for( i = 3, c = cItmPar; *c; c++ )
   {
      if( *c == '\'' )
         i++;                 /* Count ' Tokens */
   }
   cRes = ( char * ) hb_xgrab( ulItmPar + i * sizeof( char ) );
   i    = 0; c = cItmPar; cRes[ i++ ] = '\'';
   while( *c )
   {
      if( *c == '\'' )
         cRes[ i++ ] = '\'';
      cRes[ i++ ] = *c++;
   }
   cRes[ i++ ] = '\''; /* cRes[i] = '\0'; */
   hb_itemPutCLPtr( pItmPar, cRes, i );
}

static HB_UINT SCItm( char * cBuffer, HB_UINT ulMaxBuf, char * cParFrm, int iCOut, int IsIndW,
                      int iIndWidth, int IsIndP, int iIndPrec,
                      PHB_ITEM pItmPar )
{
   HB_UINT s;

   if( IsIndW && IsIndP )
   {
      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetPtr( pItmPar ) );
            break;
         case 's': case 'S':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetCPtr( pItmPar ) );
            break;
         case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetND( pItmPar ) );
            break;
         /* case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X': */
         default:
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec,
                             HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar ) );
      }
   }
   else if( IsIndW || IsIndP )
   {
      int iInd = ( IsIndW ? iIndWidth : iIndPrec );

      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetPtr( pItmPar ) );
            break;
         case 's': case 'S':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetCPtr( pItmPar ) );
            break;
         case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetND( pItmPar ) );
            break;
         /* case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X': */
         default:
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, iInd,
                             HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar ) );
      }
   }
   else
   {
      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetPtr( pItmPar ) );
            break;
         case 's': case 'S':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetCPtr( pItmPar ) );
            break;
         case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetND( pItmPar ) );
            break;
         /* case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X': */
         default:
            s = hb_snprintf( cBuffer, ulMaxBuf, cParFrm,
                             HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar ) );
      }
   }
   return s;
}

/**
 * ANSI C sprintf() for ANSI SQL with DATE, DATETIME, LOGICAL, NIL, NUMERIC
 * ------------------------------------------------------------------------
 *                      cRes := sql_sprintf( cFrm, ... )
 *
 * Full compatible ANSI C99 formats with C,S converters wchar_t (UNICODE)
 * Integer & Floating point converters with Width and Precision for NUMERIC & STRING
 * a,A converters Hexadecimal floating point format. Thanks Rafa.
 *  ? sql_sprintf( "Phi = %A", ( 1 + 5 ^ 0.5 ) / 2 ) // Phi = 0X1,9E3779B97F4A8P+0
 * %m$,*m$ Index & Indirect arguments C99. Thanks Viktor.
 *  ? sql_sprintf( "Phi = %2$0*3$.*1$f", 4, ( 1 + 5 ^ 0.5 ) / 2, 7 ) // Phi = 01.6180
 *
 * s converter for format Harbour data types.
 *    NUMERIC with FIXED DECIMALS = n | n.d   STRING = String's ANSI\C
 *    DATE = HB_SET_DATEFORMAT      DATETIME = HB_SET_DATEFORMAT hh:mm:ss
 *     New Internal Modifier {}. Thanks Mindaugas.
 *     Date and Time Format separate by first space {YYYY-MM-DD hh:mm:ss.fff pp}
 *     {YYYY-MM-DD} = Only Date | { hh:mm:ss.fff pp} = Only Time
 *        ? sql_sprintf( "%s", Date() )                   // 2008-06-16
 *        ? sql_sprintf( "%s", hb_DateTime() )            // 2008-06-16 04:11:21.531
 *        ? sql_sprintf( "%{YYYYMMDD}s", hb_DateTime() )  // 20080616
 *        ? sql_sprintf( "%{ hh:mm pp}s", hb_DateTime() ) // 04:11 AM
 *    LOGICAL = TRUE | FALSE        %d converter for LOGICAL = 1 | 0
 *     Accepts Internal Modifier TRUE and FALSE Format separate by first comma
 *     {T .T.,F .F.} = TRUE & FALSE | {ON} = Only TRUE | {,OFF} = Only FALSE
 *        ? sql_sprintf( "%{VERDADERO,FALSO}s", .F. ) // FALSO
 *        ? sql_sprintf( "%{ONLY IF TRUE}s", .T. ) // ONLY IF TRUE
 *
 * New t,T converter for format ANSI SQL types.
 *    NUMERIC with FIXED DECIMALS = n | n.d   STRING = 'String''s ANSI\\SQL'
 *     Print DEFAULT if 0 length STRING for T converter
 *    DATE = 'YYYY-MM-DD' DATETIME = 'YYYY-MM-DD HH:MM:SS'
 *     Accepts Internal Modifier like s converter {YYYY-MM-DD hh:mm:ss.fff pp}
 *     Print DEFAULT if the DATE, DATETIME is EMPTY for T converter or print
 *           DK_EMPTYDATE, DK_EMPTYDATETIME for t converter
 *    LOGICAL = TRUE | FALSE        Accepts Internal Modifier like s {ON,OFF}
 *
 * Print DEFAULT if the parameter is NIL for T converter.
 * Print NULL if the parameter is HB_IT_NULL or NIL for the rest of converters.
 * Processing %% and n converter Position.
 */

#define DK_INCRES         1024
#define DK_INCBUF         512
#define DK_BLKBUF         HB_MAX_DOUBLE_LENGTH  /* Expense of DK_INCBUF */
#define DK_EMPTYDATE      "'0001-01-01 BC'"
#define DK_EMPTYDATETIME  "'0001-01-01 00:00:00 BC'"

HB_FUNC( SQL_SPRINTF )
{
   HB_UINT      ulItmFrm;
   const char * cItmFrm;
   char *       cRes;
   int          argc    = hb_pcount() - 1;
   PHB_ITEM     pItmFrm = hb_param( 1, HB_IT_STRING );

   if( ! pItmFrm || ( cItmFrm = hb_itemGetCPtr( pItmFrm ) ) == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
   else if( ( ulItmFrm = ( HB_UINT ) hb_itemGetCLen( pItmFrm ) ) == 0 )
   {
      hb_retc_null();
   }
   else if( ! argc )
   {
      cRes = ( char * ) hb_xgrab( ulItmFrm + sizeof( char ) );
      memcpy( cRes, cItmFrm, ulItmFrm + sizeof( char ) );
      hb_retclen_buffer( cRes, ulItmFrm );
   }
   else
   {
      static const char s_szToken[] = "stTcdiouxXaAeEfgGpnSC";

      PHB_ITEM     pItmPar, pItmCpy;
      char *       cIntMod, * cBuffer, * cParFrm;
      const char * c;
      int          p, arg, iCOut, IsType, IsIndW, IsIndP, iIndWidth, iIndPrec, iErrorPar = 0;
      HB_UINT      s, f, i, ulWidth, ulParPos = 0, ulResPos = 0, ulMaxBuf = DK_INCBUF, ulMaxRes =
         DK_INCRES;

      cIntMod = NULL;
      cRes    = ( char * ) hb_xgrab( ulMaxRes );
      cBuffer = ( char * ) hb_xgrab( ulMaxBuf );
      cParFrm = ( char * ) hb_xgrab( ulItmFrm + sizeof( char ) );

      for( p = 0; p < argc; /* Not p++ by support index & indirect arguments */ )
      {

         c = cItmFrm + ulParPos;
         s = f = i = ulWidth = arg = iCOut = IsType = IsIndW = iIndWidth = IsIndP = iIndPrec = 0;
         do    /* Get Par Format */
         {
            cParFrm[ i++ ] = *c;
            if( f && *c == '%' )
            {
               f = ulWidth = IsIndW = IsIndP = 0;
            }
            else if( f && ! ulWidth && *c >= '0' && *c <= '9' )
            {
               ulWidth = atol( c );
            }
            else if( f && *c == '.' )
            {
               if( f++ == 2 )
                  iErrorPar = 1;
            }
            else if( f && *c == '*' )
            {
               if( f == 2 )
               {
                  if( IsIndP )
                  {
                     f         = 3;
                     iErrorPar = 1;
                  }
                  else
                     IsIndP = 1;
               }
               else if( ! IsIndW )
                  IsIndW = 1;
               else
               {
                  f         = 3;
                  iErrorPar = 1;
               }
            }
            else if( f && *c == '$' )
            {
               if( ulWidth && IsIndP && ! iIndPrec )
               {
                  iIndPrec = ulWidth;
                  iCOut    = '*';
               }
               else if( ulWidth && IsIndW && ! iIndWidth )
               {
                  iIndWidth = ulWidth;
                  ulWidth   = 0;
                  iCOut     = '*';
               }
               else if( ulWidth && ! arg )
               {
                  arg     = ulWidth;
                  ulWidth = 0;
                  iCOut   = '%';
               }
               else
               {
                  f         = 3;
                  iErrorPar = 1;
               }
               while( i && cParFrm[ --i ] != iCOut )
               {
               }
               ++i;
               iCOut = 0;
            }
            else if( f && *c == '{' )
            {
               if( s )
               {
                  f         = 3;
                  iErrorPar = 1;
               }
               else     /* Remove Internal Modifier */
               {
                  if( cIntMod == NULL )
                     cIntMod = ( char * ) hb_xgrab( ulItmFrm + sizeof( char ) );

                  while( *c++ && *c != '}' )
                     cIntMod[ s++ ] = *c;
                  --i;
                  cIntMod[ s ] = '\0';
                  if( *( c - 1 ) == '\0' )
                  {
                     f         = 3;
                     iErrorPar = 1;
                  }
               }
            }
            else if( f && strchr( s_szToken, *c ) )
            {
               f     = 3;
               iCOut = *c;
            }
            else if( *c == '%' )
               f = 1;
            c++;
         }
         while( f < 3 && *c );
         cParFrm[ f = i ] = '\0';
         if( iErrorPar )
            break;

         if( iCOut == 't' || iCOut == 'T' )
         {
            if( cParFrm[ f - 2 ] == '%' )
            {
               IsType = ( iCOut == 'T' ? 2 : 1 );
               iCOut  = cParFrm[ f - 1 ] = 's';
            }
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( IsIndW )   /* Get Par Indirectly Width Item */
         {
            pItmPar = hb_param( ( iIndWidth ? iIndWidth + 1 :  p++ + 2 ), HB_IT_INTEGER );
            if( pItmPar )
            {
               if( ( iIndWidth = hb_itemGetNI( pItmPar ) ) < 0 )
                  ulWidth = -iIndWidth;
               else
                  ulWidth = iIndWidth;
            }
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( IsIndP )   /* Get Par Indirectly Precision Item */
         {
            pItmPar = hb_param( ( iIndPrec ? iIndPrec + 1 : p++ + 2 ), HB_IT_INTEGER );
            if( pItmPar )
            {
               iIndPrec = hb_itemGetNI( pItmPar );
            }
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( ! arg && *c && p == argc - 1 )  /* No more Par Items */
         {
            do
            {
               cParFrm[ i++ ] = *c;
            }
            while( *c++ );
            i--;
         }  /* i == strlen( cParFrm ) */

         pItmPar = hb_param( ( arg ? arg + 1 : p++ + 2 ), HB_IT_ANY );   /* Get Par Item */
         if( ! pItmPar )
         {
            iErrorPar = 1;
            break;
         }

         if( ! iCOut || iCOut == 'n' )      /* Par Text Out */
         {
            for( f = i, i = 0; i < f; i++ ) /* Change %% with % */
            {
               if( cParFrm[ i ] == '%' && cParFrm[ i + 1 ] == '%' )
               {
                  memcpy( cParFrm + i, cParFrm + i + 1, f - i );
                  f--;
               }
            }  /* i == strlen( cParFrm ) */
            if( iCOut )
            {
               for( f = 0; f < i; f++ )   /* Erase %n */
               {
                  if( cParFrm[ f ] == '%' && cParFrm[ f + 1 ] == 'n' )
                  {
                     memcpy( cParFrm + f, cParFrm + f + 2, i - f - 1 );
                     break;
                  }
               }  /* f == Index % of n */
               if( f < i )
               {
                  i -= 2;  /* i == strlen(cParFrm) */
                  hb_itemPutNS( pItmPar, ulResPos + f );
               }
               else
               {
                  iErrorPar = 1;
                  break;
               }
            }
            if( ( f = i + sizeof( char ) ) > ulMaxBuf )
            {
               ulMaxBuf += f + DK_INCBUF;
               cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
            }
            hb_strncpy( cBuffer, cParFrm, i );
            s = i;
         }
         else     /* Par Item sprintf() Out */
         {
#        ifdef HB_IT_NULL
            if( ( HB_IS_NIL( pItmPar ) || HB_IS_NULL( pItmPar ) ) )
            {
#        else
            if( HB_IS_NIL( pItmPar ) )
            {
#        endif
               ulWidth = f;
               IsIndW  = IsIndP = 0;
               while( cParFrm[ --f ] != '%' )
               {
               }
               iCOut = cParFrm[ f + 1 ] = 's';  /* Change format with %s */
               memcpy( cParFrm + f + 2, cParFrm + ulWidth, i - ulWidth + 1 );
               i -= ulWidth - f - 2;            /* i == strlen(cParFrm) */
               if( ( f = i + 8 ) > ulMaxBuf )   /* size of "DEFAULT" == 8 */
               {
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
               }
               pItmCpy = hb_itemNew( NULL );
#           ifdef HB_IT_NULL
               if( IsType == 2 && ! HB_IS_NULL( pItmPar ) )
                  hb_itemPutCL( pItmCpy, "DEFAULT", 7 );
#           else  /* Print DEFAULT if NIL for T converter if not NULL, print NULL for the rest of converters */
               if( IsType == 2 )
                  hb_itemPutCL( pItmCpy, "DEFAULT", 7 );
#           endif
               else
                  hb_itemPutCL( pItmCpy, "NULL", 4 );
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                          pItmCpy );
               hb_itemRelease( pItmCpy );

            }
            else if( HB_IS_STRING( pItmPar ) && ( iCOut == 's' || iCOut == 'S' ) )
            {
               if( IsType )
               {
                  hb_itemCopy( pItmCpy = hb_itemNew( NULL ), pItmPar );
                  pItmPar = pItmCpy;
                  if( IsType == 2 && hb_itemGetCLen( pItmPar ) == 0 )   /* 0 length string print DEFAULT for T converter */
                     hb_itemPutCL( pItmPar, "DEFAULT", 7 );
                  else
                     STAItm( pItmPar );
               }
               f = ( HB_UINT ) hb_itemGetCLen( pItmPar );
               if( ( f = i + HB_MAX( ulWidth, f ) ) > ulMaxBuf )
               {
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                          pItmPar );
               if( IsType )
                  hb_itemRelease( pItmPar );

            }
            else if( HB_IS_DATETIME( pItmPar ) && iCOut == 's' )
            {
               long lDate, lTime;
               char cDTBuf[ 9 ], cDTFrm[ 27 ];

               if( s )  /* Internal Modifier */
               {
                  for( f = 0; cIntMod[ f ] && cIntMod[ f ] != ' '; f++ )
                  {
                  }
                  if( f != s )
                     cIntMod[ f++ ] = '\0';           /* Date & Time */
               }

               if( HB_IS_TIMESTAMP( pItmPar ) )
               {
                  hb_itemGetTDT( pItmPar, &lDate, &lTime );
                  hb_timeStampFormat(  cDTFrm,
                                       ( s ? cIntMod : ( IsType ? "YYYY-MM-DD" :
                                                         hb_setGetDateFormat() ) ),
                                       ( s ? cIntMod +
                                         f : ( IsType ? "HH:MM:SS" : hb_setGetTimeFormat() ) ),
                                       lDate, lTime );
                  if( s )
                  {
                     if( ! cIntMod[ 0 ] )
                        memcpy( cDTFrm, cDTFrm + 1, 26 );  /* LTrim 1 space if only Time */
                     else if( cDTFrm[ s ] == ' ' )
                        cDTFrm[ s ] = '\0';                /* RTrim 1 space if only Date */
                  }
               }
               else
                  hb_dateFormat( hb_itemGetDS( pItmPar, cDTBuf ), cDTFrm,
                                 ( s ? cIntMod : ( IsType ? "YYYY-MM-DD" : hb_setGetDateFormat() ) ) );

               /* 27 + 2 if %t and change format time */
               if( ( f = i + HB_MAX( ulWidth, 29 ) ) > ulMaxBuf )
               {
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
               }
               pItmCpy = hb_itemNew( NULL );
               hb_itemPutC( pItmCpy, cDTFrm );
               if( IsType )
               {
                  /* Empty DATE, DATETIME print DEFAULT for T converter or DK_EMPTYDATE, DK_EMPTYDATETIME for t converter */
                  if( *cDTFrm == ' ' )
                     hb_itemPutC( pItmCpy, ( HB_IS_TIMESTAMP( pItmPar ) ?
                                             ( IsType == 2 ? "DEFAULT" : DK_EMPTYDATETIME ) :
                                             ( IsType == 2 ? "DEFAULT" : DK_EMPTYDATE ) ) );
                  else
                     STAItm( pItmCpy );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                          pItmCpy );
               hb_itemRelease( pItmCpy );

            }
            else if( HB_IS_LOGICAL( pItmPar ) )
            {

               if( s )  /* Internal Modifier */
               {
                  for( f = 0; cIntMod[ f ] && cIntMod[ f ] != ','; f++ )
                  {
                  }
                  if( f != s )
                     cIntMod[ f++ ] = '\0';           /* TRUE & FALSE */
               }
               if( iCOut == 's' )
               {
                  hb_itemCopy( pItmCpy = hb_itemNew( NULL ), pItmPar );
                  pItmPar = pItmCpy;
                  hb_itemPutC( pItmPar,
                               ( hb_itemGetL( pItmPar ) ? ( s ? cIntMod : "TRUE" ) : ( s ? cIntMod
                                                                                       + f :
                                                                                       "FALSE" ) ) );
               }
               if( ( f = i +
                         ( iCOut ==
                           's' ? HB_MAX( ulWidth, ( s ? s : 6 ) ) : HB_MAX( ulWidth,
                                                                            DK_BLKBUF ) ) ) >
                   ulMaxBuf )
               {
                  ulMaxBuf += f + DK_INCBUF;   /* size of "FALSE" == 6 */
                  cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                          pItmPar );
               if( iCOut == 's' )
                  hb_itemRelease( pItmPar );

            }
            else if( iCOut == 's' )
            {
               char *       cStr = hb_itemStr( pItmPar, NULL, NULL );
               const char * cTrimStr;

               if( cStr )
               {
                  HB_SIZE nLen = strlen( cStr );
                  cTrimStr = hb_strLTrim( cStr, &nLen );
                  f        = ( HB_UINT ) nLen;
                  if( ( f = i + HB_MAX( ulWidth, f ) ) > ulMaxBuf )
                  {
                     ulMaxBuf += f + DK_INCBUF;
                     cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
                  }
                  pItmCpy = hb_itemNew( NULL );
                  hb_itemPutCL( pItmCpy, cTrimStr, f );
                  s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                             pItmCpy );
                  hb_itemRelease( pItmCpy );
                  hb_xfree( cStr );
               }
               else
               {
                  iErrorPar = p + 2;
                  break;
               }

            }
            else if( HB_IS_NUMERIC( pItmPar ) || HB_IS_POINTER( pItmPar ) )
            {
               if( ( f = i + HB_MAX( ulWidth, DK_BLKBUF ) ) > ulMaxBuf )
               {
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer   = ( char * ) hb_xrealloc( cBuffer, ulMaxBuf );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec,
                          pItmPar );

            }
            else
            {
               iErrorPar = p + 2;
               break;
            }
         }

         if( ( f = s + ulResPos + sizeof( char ) ) > ulMaxRes )
         {
            ulMaxRes += f + DK_INCRES;
            cRes      = ( char * ) hb_xrealloc( cRes, ulMaxRes );
         }

         hb_strncpy( cRes + ulResPos, cBuffer, s );
         ulResPos += s;

         if( ( ulParPos = ( HB_UINT ) ( c - cItmFrm ) ) >= ulItmFrm )
            break;   /* No more Par Format */
      }

      if( cIntMod )
         hb_xfree( cIntMod );

      hb_xfree( cParFrm );
      hb_xfree( cBuffer );

      if( iErrorPar )
      {
         hb_xfree( cRes );

         if( iErrorPar > 1 )
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( iErrorPar ) );
         else
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      }
      else
         hb_retclen_buffer( cRes, ulResPos );
   }
}
