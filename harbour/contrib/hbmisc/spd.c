/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SQL_SPRINTF() function
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"

static void STAItm( PHB_ITEM pItmPar )
{
   ULONG i, ulItmPar = hb_itemGetCLen( pItmPar );
   char *cRes, *c, *cItmPar = hb_itemGetCPtr( pItmPar );

   for( i = 3, c = cItmPar; *c; c++ ){
      if( *c == '\'' ) i++;   /* Count ' Tokens */
   }
   cRes = (char *)hb_xgrab( ulItmPar + i * sizeof(char) );
   i = 0; c = cItmPar; cRes[i++] = '\'';
   while( *c ){
      if( *c == '\'' ) cRes[i++] = '\'';
      cRes[i++] = *c++;
   }
   cRes[i++] = '\''; /* cRes[i] = '\0'; */
   hb_itemPutCLPtr( pItmPar, cRes, i );
}

static ULONG SCItm( char *cBuffer, ULONG ulMaxBuf, char *cParFrm, int iCOut, int IsIndW, int iIndWidth, int IsIndP, int iIndPrec, PHB_ITEM pItmPar )
{
   ULONG s = 0;

   if( IsIndW && IsIndP ){
      switch( iCOut ){
      case 'p':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetPtr( pItmPar ) );
         break;
      case 's': case 'S':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetCPtr( pItmPar ) );
         break;
      case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, hb_itemGetND( pItmPar ) );
         break;
      case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iIndWidth, iIndPrec, (HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar )) );
      }
   }else if( IsIndW || IsIndP ){
      int iInd = (IsIndW ? iIndWidth : iIndPrec);

      switch( iCOut ){
      case 'p':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetPtr( pItmPar ) );
         break;
      case 's': case 'S':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetCPtr( pItmPar ) );
         break;
      case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, hb_itemGetND( pItmPar ) );
         break;
      case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, iInd, (HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar )) );
      }
   }else{
      switch( iCOut ){
      case 'p':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetPtr( pItmPar ) );
         break;
      case 's': case 'S':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetCPtr( pItmPar ) );
         break;
      case 'e': case 'E': case 'f': case 'g': case 'G': case 'a': case 'A':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, hb_itemGetND( pItmPar ) );
         break;
      case 'c': case 'C': case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
         s = snprintf( cBuffer, ulMaxBuf, cParFrm, (HB_IS_LONG( pItmPar ) ? hb_itemGetNL( pItmPar ) : hb_itemGetNI( pItmPar )) );
      }
   }
   return s;
}

/*******************************************************************************
* ANSI C sprintf() for ANSI SQL with DATE, DATETIME, LOGICAL, NIL, NUMERIC
* ------------------------------------------------------------------------
* cRes := SQL_SPRINTF( cFrm, ... )
* cFrm : %s for DATE = YYYY-MM-DD, DATETIME = YYYY-MM-DD HH:MM:SS
*        %t for DATE = 'YYYY-MM-DD', DATETIME = 'YYYY-MM-DD HH:MM:SS'
*        %t for STRING = 'String''s ANSI SQL'
*        %t & %s for LOGICAL = TRUE | FALSE, %d for LOGICAL = 1 | 0
*        %t & %s is also for NUMERIC with FIXED DECIMALS
*        NIL or HB_IT_NULL = NULL
*
* NOTE .-
* Remove C ESC sequences and converts them to Clipper chars in cRes.
*    OutStd( SQL_SPRINTF( 'Hello\nworld!' ) ) => like printf( "Hello\nworld!" );
* Accepts conversion inside if variable is passed by reference.
*    Local xDate := Date(); SQL_SPRINTF('%s', @xDate) => xDate == '2008-05-19'
* Support index & indirect arguments C99.
*    cRes := SQL_SPRINTF( "Phi = %2$0*3$.*1$f \n", 10, (1 + 5**0.5) / 2, 13 )
*******************************************************************************/

#define DK_INCRES 1024
#define DK_INCBUF 512
#define DK_BLKBUF HB_MAX_DOUBLE_LENGTH   /* Expense of DK_INCBUF */
#define DK_FRMTIM "HH:MM:SS"
	
#if defined( __XHARBOUR__ ) && ! defined( HB_ERR_FUNCNAME )
   #define HB_ERR_FUNCNAME "SQL_SPRINTF"
#endif

HB_FUNC( SQL_SPRINTF )
{
   ULONG ulItmFrm;
   char *cRes, *cItmFrm;
   int argc = hb_pcount() - 1;
   PHB_ITEM pItmFrm = hb_param( 1, HB_IT_STRING );

   if( !pItmFrm || (cItmFrm = hb_itemGetCPtr( pItmFrm )) == NULL ){
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }else if( !(ulItmFrm = hb_itemGetCLen( pItmFrm )) ){
      hb_retc( NULL );
   }else if( !argc ){
      cRes = (char *)hb_xgrab( ulItmFrm + sizeof(char) );
      memcpy( cRes, cItmFrm, ulItmFrm + sizeof(char) );
      cRes = hb_strRemEscSeq( cRes, &ulItmFrm );
      hb_retclen_buffer( cRes, ulItmFrm );
   }else{
      PHB_ITEM pItmPar;
      char *cBuffer, *cParFrm, *c;
      int p, arg, iCOut, IsType, IsIndW, IsIndP, iIndWidth, iIndPrec, iErrorPar = 0;
      ULONG s, f, i, ulWidth, ulParPos = 0, ulResPos = 0, ulMaxBuf = DK_INCBUF, ulMaxRes = DK_INCRES;
      static char cToken[] = "stcdiouxXaAeEfgGpnSC";

      cRes = (char *)hb_xgrab( ulMaxRes );
      cBuffer = (char *)hb_xgrab( ulMaxBuf );
      cParFrm = (char *)hb_xgrab( ulItmFrm + sizeof(char) );

      for( p = 0; p < argc; /* Not p++ by support index & indirect arguments */ ){

         c = cItmFrm + ulParPos;
         f = i = ulWidth = arg = iCOut = IsType = IsIndW = iIndWidth = IsIndP = iIndPrec = 0;
         do{   /* Get Par Format */
            cParFrm[i++] = *c;
            if( f && *c == '%' ){
               f = ulWidth = IsIndW = IsIndP = 0;
            }else if( f && !ulWidth && *c >= '0' && *c <= '9' ){
               ulWidth = atol( c );
            }else if( f && *c == '.' ){
               if( f++ == 2 ) iErrorPar = 1;
            }else if( f && *c == '*' ){
               if( f == 2 ){
                  if( IsIndP ){
                     f = 3; iErrorPar = 1;
                  }else{
                     IsIndP = 1;
                  }
               }else if( !IsIndW ){
                  IsIndW = 1;
               }else{
                  f = 3; iErrorPar = 1;
               }
            }else if( f && *c == '$' ){
               if( ulWidth && IsIndP && !iIndPrec ){
                  iIndPrec = ulWidth;
                  iCOut = '*';
               }else if( ulWidth && IsIndW && !iIndWidth ){
                  iIndWidth = ulWidth;
                  ulWidth = 0; iCOut = '*';
               }else if( ulWidth && !arg ){
                  arg = ulWidth;
                  ulWidth = 0; iCOut = '%';
               }else{
                  f = 3; iErrorPar = 1;
               }
               while( i && cParFrm[--i] != iCOut );
               ++i; iCOut = 0;
            }else if( f && strchr(cToken, *c) ){
               f = 3; iCOut = *c;
            }else if( *c == '%' ){
               f = 1;
            }
            c++;
         }while( f < 3 && *c ); cParFrm[f = i] = '\0';
         if( iErrorPar ) break;

         if( iCOut == 't' ){
            if( cParFrm[f - 2] == '%' ){
               IsType = 1; iCOut = cParFrm[f - 1] = 's';
            }else{
               iErrorPar = 1; break;
            }
         }

         if( IsIndW  ){ /* Get Par Indirectly Width Item */
            pItmPar = hb_param( (iIndWidth ? iIndWidth + 1 :  p++ + 2), HB_IT_INTEGER );
            if( pItmPar ){
               if( (iIndWidth = hb_itemGetNI( pItmPar )) < 0 ){
                  ulWidth = -iIndWidth;
               }else{
                  ulWidth = iIndWidth;
               }
            }else{
               iErrorPar = 1; break;
            }
         }

         if( IsIndP ){  /* Get Par Indirectly Precision Item */
            pItmPar = hb_param( (iIndPrec ? iIndPrec + 1 :  p++ + 2), HB_IT_INTEGER );
            if( pItmPar ){
               iIndPrec = hb_itemGetNI( pItmPar );
            }else{
               iErrorPar = 1; break;
            }
         }

         if( !arg && *c && p == argc - 1 ){ /* No more Par Items */
            do{ cParFrm[i++] = *c; }while( *c++ ); i--;
         }  /* i == strlen(cParFrm) */

         pItmPar = hb_param( (arg ? arg + 1 :  p++ + 2), HB_IT_ANY );   /* Get Par Item */
         if( !pItmPar ){
            iErrorPar = 1; break;
         }

         if( !iCOut || iCOut == 'n' ){ /* Par Text Out */

            for( f = i, i = 0; i < f; i++ ){ /* Change %% with % */
               if( cParFrm[i] == '%' && cParFrm[i + 1] == '%' ){
                  memcpy( cParFrm + i, cParFrm + i + 1, f - i );
                  f--;
               }
            }  /* i == strlen(cParFrm) */
            if( iCOut ){
               for( f = 0; f < i; f++ ){  /* Erase %n */
                  if( cParFrm[f] == '%' && cParFrm[f + 1] == 'n' ){
                     memcpy( cParFrm + f, cParFrm + f + 2, i - f - 1 );
                     break;
                  }
               }  /* f == Index % of n */
               if( f < i ){
                  i -= 2;  /* i == strlen(cParFrm) */
                  hb_itemPutNL( pItmPar, ulResPos + f );
               }else{
                  iErrorPar = 1; break;
               }
            }
            if( (f = i + sizeof(char)) > ulMaxBuf ){
               ulMaxBuf += f + DK_INCBUF;
               cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
            }
            strcpy( cBuffer, cParFrm ); s = i;

         }else{   /* Par Item sprintf() Out */

#        ifdef HB_IT_NULL
            if( (HB_IS_NIL( pItmPar ) || HB_IS_NULL( pItmPar )) ){
#        else
            if( HB_IS_NIL( pItmPar ) ){
#        endif
               ulWidth = f; IsIndW = IsIndP = 0;
               while( cParFrm[--f] != '%' );
               iCOut = cParFrm[f + 1] = 's'; /* Change format with %s */
               memcpy( cParFrm + f + 2, cParFrm + ulWidth, i - ulWidth + 1 );
               i -= ulWidth - f - 2;   /* i == strlen(cParFrm) */
               if( (f = i + 5) > ulMaxBuf ){
                  ulMaxBuf += f + DK_INCBUF;   /* size of "NULL" == 5 */
                  cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
               }
               hb_itemPutCL( pItmPar, "NULL", 4 );
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );

            }else if( HB_IS_STRING( pItmPar ) && (iCOut == 's' || iCOut == 'S') ){
               if( IsType ) STAItm( pItmPar );
               f = hb_itemGetCLen( pItmPar );
               if( (f = i + HB_MAX(ulWidth, f)) > ulMaxBuf ){
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );

            }else if( HB_IS_DATE( pItmPar ) && iCOut == 's' ){
               char cDTBuf[ 19 ], cDTFrm[ 28 ]; /* 26 + 2 if %t and change format time */

#           ifdef __XHARBOUR__
               if( HB_IS_DATETIME( pItmPar ) ){
                  hb_datetimeFormat( hb_itemGetDTS( pItmPar, cDTBuf ), cDTFrm, "YYYY-MM-DD", DK_FRMTIM );
               }else
#           endif
                  hb_dateFormat( hb_itemGetDS( pItmPar, cDTBuf ), cDTFrm, "YYYY-MM-DD" );

               if( (f = i + HB_MAX(ulWidth, 28)) > ulMaxBuf ){
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
               }
               hb_itemPutC( pItmPar, cDTFrm );
               if( IsType ) STAItm( pItmPar );
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );

            }else if( HB_IS_LOGICAL( pItmPar ) ){
               if( (f = i + (iCOut == 's' ? HB_MAX(ulWidth, 6) : HB_MAX(ulWidth, DK_BLKBUF))) > ulMaxBuf ){
                  ulMaxBuf += f + DK_INCBUF;   /* size of "FALSE" == 6 */
                  cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
               }
               if( iCOut == 's' ){
                  hb_itemPutC( pItmPar, (hb_itemGetL( pItmPar ) ? "TRUE" : "FALSE") );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );

            }else if( iCOut == 's' ){
               char *cTrimStr, *cStr = hb_itemStr( pItmPar, NULL, NULL );

               if( cStr ){
                  f = strlen( cStr ); cTrimStr = hb_strLTrim( cStr, &f );
                  if( (f = i + HB_MAX(ulWidth, f)) > ulMaxBuf ){
                     ulMaxBuf += f + DK_INCBUF;
                     cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
                  }
                  hb_itemPutCL( pItmPar, cTrimStr, f );
                  s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );
                  hb_xfree( cStr );
               }else{
                  iErrorPar = p + 2; break;
               }

            }else if( HB_IS_NUMERIC( pItmPar ) || HB_IS_POINTER( pItmPar ) ){
               if( (f = i + HB_MAX(ulWidth, DK_BLKBUF)) > ulMaxBuf ){
                  ulMaxBuf += f + DK_INCBUF;
                  cBuffer = (char *)hb_xrealloc( cBuffer, ulMaxBuf );
               }
               s = SCItm( cBuffer, ulMaxBuf, cParFrm, iCOut, IsIndW, iIndWidth, IsIndP, iIndPrec, pItmPar );

            }else{
               iErrorPar = p + 2; break;
            }
         }

         if( (f = s + ulResPos + sizeof(char)) > ulMaxRes ){
            ulMaxRes += f + DK_INCRES;
            cRes = (char *)hb_xrealloc( cRes, ulMaxRes );
         }
         strcpy( cRes + ulResPos, cBuffer ); ulResPos += s;

         if( (ulParPos = c - cItmFrm) >= ulItmFrm ){
            break;   /* No more Par Format */
         }
      }
      hb_xfree( cParFrm ); hb_xfree( cBuffer );
      if( iErrorPar ){
         hb_xfree( cRes );
         if( iErrorPar > 1 ){
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( iErrorPar ) );
         }else{
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
         }
      }else{
         cRes = hb_strRemEscSeq( cRes, &ulResPos );
         hb_retclen_buffer( cRes, ulResPos );
      }
   }
}
