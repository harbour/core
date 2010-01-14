/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
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
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbstack.h"

HB_FUNC( TIPENCODERBASE64_ENCODE )
{
   const char *cData = hb_parc( 1 );
   char *cRet;
   int nLen = hb_parclen( 1 );
   int nPos = 0, nPosRet = 0;
   int nPosBlock = 0, nLineCount = 0;
   ULONG nFinalLen;
   unsigned char cElem, cElem1;
   HB_BOOL bExcept;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }

   /* read the status of bHttpExcept */
   if( hb_pcount() > 1 )
   {
      /* this makes this function static!!!! */
      bExcept = hb_parl( 2 );
   }
   else
   {
      hb_objSendMsg( hb_stackSelfItem(), "BHTTPEXCEPT", 0 );
      bExcept = hb_parl( -1 );
   }
   /* we know exactly the renturned length. */
   nFinalLen = (ULONG) ((nLen / 3 + 2) * 4);
   /* add line termination padding, CRLF each 76 output bytes */
   nFinalLen += (nFinalLen / 72 +1) * 2;
   cRet = ( char * ) hb_xgrab( nFinalLen );

   while( nPos < nLen )
   {
      cElem = (unsigned char) cData[ nPos ];
      /* NOT using trailing 0 here as some buggy 3dparty func
         will create strings without trailing 0. */

      nPosBlock++;

      switch( nPosBlock )
      {
         case 1:
            cElem = cElem >> 2;
            break;
         case 2:
            cElem1 = nPos < nLen -1 ? (unsigned char) cData[ nPos + 1] : 0;
            cElem = ((cElem & 0x3) << 4) | (cElem1 >> 4);
            nPos++;
            break;
         case 3:
            cElem1 = nPos < nLen -1 ? (unsigned char) cData[ nPos + 1] : 0;
            cElem = ((cElem & 0xF) << 2) | (cElem1 >> 6);
            nPos++;
            break;
         case 4:
            cElem = cElem & 0x3f;
            nPos++;
            nPosBlock = 0;
            break;
      }

      if( cElem < 26 )
      {
         cRet[nPosRet++] = cElem + 'A';
      }
      else if( cElem < 52 )
      {
         cRet[nPosRet++] = ( cElem - 26 ) + 'a';
      }
      else if( cElem < 62 )
      {
         cRet[nPosRet++] = ( cElem - 52 ) + '0';
      }
      else if( cElem == 62 )
      {
         cRet[nPosRet++] = '+';
      }
      else
      {
         cRet[nPosRet++] = '/';
      }

      if( ! bExcept )
      {
         nLineCount ++ ;
         /* RFC says to add a CRLF each 76 chars, but is pretty unclear about
            the fact of this 76 chars counting CRLF or not. Common
            practice is to limit line size to 72 chars */
         if( nLineCount == 72 )
         {
            cRet[nPosRet++] = '\r';
            cRet[nPosRet++] = '\n';
            nLineCount = 0;
         }
      }
   }

   switch( nPos % 3 )
   {
      case 1:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
      case 2:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
   }

   /* RFC is not explicit, but CRLF SHOULD be added at bottom
      during encoding phase */
   if( ! bExcept )
   {
      cRet[nPosRet++] = '\r';
      cRet[nPosRet++] = '\n';
   }

   /* this function also adds a zero */
   hb_retclen_buffer( cRet, nPosRet );
}

HB_FUNC( TIPENCODERBASE64_DECODE )
{
   const char *cData = hb_parc( 1 );
   unsigned char *cRet;
   int nLen = hb_parclen( 1 );
   int nPos = 0, nPosRet = 0, nPosBlock = 0;
   unsigned char cElem;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }


   /* we know exactly the renturned length. */
   cRet = ( unsigned char * ) hb_xgrab( (nLen / 4 + 1) * 3 );

   while( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if( cElem >= 'A' && cElem <= 'Z' )
      {
         cElem -= 'A';
      }
      else if( cElem >= 'a' && cElem <= 'z' )
      {
         cElem = cElem - 'a' + 26;
      }
      else if( cElem >= '0' && cElem <= '9' )
      {
         cElem = cElem - '0' + 52;
      }
      else if( cElem == '+' )
      {
         cElem = 62;
      }
      else if( cElem == '/' )
      {
         cElem = 63;
      }
      /* end of stream? */
      else if( cElem == '=' )
      {
         break;
      }
      /* RFC 2045 specifies characters not in base64 must be ignored */
      else
      {
         nPos++;
         continue;
      }

      switch( nPosBlock )
      {
         case 0:
            cRet[nPosRet]  = cElem << 2;
            nPosBlock++;
            break;
         case 1:
            /* higer bits are zeros */
            cRet[nPosRet] |= cElem >> 4;
            nPosRet++;
            cRet[nPosRet]  = cElem << 4;
            nPosBlock++;
            break;
         case 2:
            /* higer bits are zeros */
            cRet[nPosRet] |= cElem >> 2;
            nPosRet++;
            cRet[nPosRet]  = cElem << 6;
            nPosBlock++;
            break;
         case 3:
            cRet[nPosRet] |= cElem;
            nPosRet++;
            nPosBlock = 0;
            break;
      }

      nPos++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = ( unsigned char * ) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclen_buffer( ( char * ) cRet, nPosRet );
}

HB_FUNC( TIPENCODERQP_ENCODE )
{
   const char *cData = hb_parc( 1 );
   int nLen = hb_parclen( 1 );
   char *cRet;
   unsigned char cElem;
   int nVal, iLineLen = 0;
   int nPosRet = 0, nPos = 0;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }

   /* Preallocating maximum possible length */
   cRet = ( char * ) hb_xgrab( nLen * 3 + ( nLen/72 ) *3 + 3 );
   /* last +3 is trailing \r\n\0 */
   while( nPos < nLen )
   {
      cElem = (unsigned char) cData[ nPos ];

      /* We chose not to encode spaces and tab here.
         cElem is signed and ranges from -126 to +127.
         negative values are automatically encoded */
      if( (cElem >=33 && cElem <= 60) || cElem >= 62 ||
         cElem == 9 || cElem == 32 )
      {
         cRet[nPosRet++] = (char) cElem;
         iLineLen++;
      }
      else
      {
         cRet[nPosRet++] = '=';
         nVal = cElem >> 4;
         cRet[nPosRet++] = (char) (nVal < 10 ? '0' + nVal : 'A' + nVal - 10);
         nVal = cElem & 0x0f;
         cRet[nPosRet++] = (char) (nVal < 10 ? '0' + nVal : 'A' + nVal - 10);
         iLineLen+=3;
      }

      nPos++;

      if( iLineLen >= 72 )
      {
         cRet[nPosRet++] = '=';
         cRet[nPosRet++] = '\r';
         cRet[nPosRet++] = '\n';
         iLineLen = 0;
      }
   }

   /* Securing last line trailing space, if needed */
   cElem = (unsigned char) cRet[nPosRet - 1];
   if( cElem == 9 || cElem == 32 )
   {
      cRet[nPosRet++] = '=';
      cRet[nPosRet++] = '\r';
      cRet[nPosRet++] = '\n';
   }
   /* Adding canonical new line for RFC2045 blocks */

   /* this function also adds a zero */
   cRet = ( char * ) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclen_buffer( cRet, nPosRet );
}

HB_FUNC( TIPENCODERQP_DECODE )
{
   const char *cData = hb_parc( 1 );
   int nLen = hb_parclen( 1 );
   char *cRet;
   int nPos = 0, nPosRet = 0, nVal;
   unsigned char cElem, cCipher;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }


   /* allocate maximum possible lenght. */
   cRet = ( char * ) hb_xgrab( nLen + 1 );

   while( nPos < nLen )
   {
      cElem = (unsigned char) cData[ nPos ];

      if( cElem == '=' )
      {
         if( nPos < nLen - 2 )
         {
            cCipher = (unsigned char) cData[ ++nPos ];
            /* soft line break */
            if( cCipher == '\r' )
            {
               nPos += 2;
               continue;
            }
            else {

               nVal = cCipher >= 'A' && cCipher <= 'F' ? cCipher - 'A' + 10 :
                     cCipher - '0';
               nVal *= 16;

               cCipher = (unsigned char) cData[ ++nPos ];
               nVal += cCipher >= 'A' && cCipher <= 'F' ? cCipher - 'A' + 10 :
                     cCipher - '0';

               cRet[ nPosRet++ ] = (char) nVal;
            }
         }
         /* else the encoding is malformed */
         else
         {
            if(nPosRet > 0 )
            {
               break;
            }
         }
      }
      else
      {
         cRet[ nPosRet++ ] = (char) cElem;
      }

      nPos ++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = ( char * ) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclen_buffer( cRet, nPosRet );
}

HB_FUNC( TIPENCODERURL_ENCODE )
{
   const char *cData = hb_parc( 1 );
   int nLen = hb_parclen( 1 );
   HB_BOOL bComplete = hb_parl( 2 );
   char *cRet;
   int nPos = 0, nPosRet = 0, nVal;
   char cElem;

   if( hb_pcount() < 2 )
      bComplete = HB_TRUE;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }

   /* Giving maximum final length possible */
   cRet = ( char * ) hb_xgrab( nLen * 3 + 1 );

   while( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if( cElem == ' ' )
      {
         cRet[ nPosRet ] = '+';
      }
      else if(
         (cElem >= 'A' && cElem <= 'Z') ||
         (cElem >= 'a' && cElem <= 'z') ||
         (cElem >= '0' && cElem <= '9') ||
         cElem == '.' || cElem == ',' || cElem == '&' ||
         cElem == '/' || cElem == ';' || cElem =='_' )
      {
         cRet[ nPosRet ] = cElem;
      }
      else if( ! bComplete && ( cElem == ':' || cElem == '?' || cElem == '=' ) )
      {
         cRet[ nPosRet ] = cElem;
      }
      else /* encode! */
      {
         cRet[ nPosRet++ ] = '%';
         nVal = ( ( unsigned char ) cElem ) >> 4;
         cRet[ nPosRet++ ] = nVal < 10 ? '0' + ( char ) nVal : 'A' + ( char ) nVal - 10;
         nVal = ( ( unsigned char ) cElem ) & 0x0F;
         cRet[ nPosRet ] = nVal < 10 ? '0' + ( char ) nVal : 'A' + ( char ) nVal - 10;
      }

      nPosRet++;
      nPos++;
   }

   hb_retclen_buffer( cRet, nPosRet );
}

HB_FUNC( TIPENCODERURL_DECODE )
{
   const char *cData = hb_parc( 1 );
   int nLen = hb_parclen( 1 );
   char *cRet;
   int nPos = 0, nPosRet = 0;
   char cElem;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }


   /* maximum possible lenght */
   cRet = ( char * ) hb_xgrab( nLen );

   while( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if( cElem == '+' )
      {
         cRet[ nPosRet ] = ' ';
      }
      else if( cElem == '%' )
      {
         if( nPos < nLen - 2 )
         {
            cElem = cData[ ++nPos ];
            cRet[ nPosRet ] = cElem < 'A' ? cElem - '0' : cElem - 'A' + 10;
            cRet[ nPosRet ] *= 16;

            cElem = cData[ ++nPos ];
            cRet[ nPosRet ] |= cElem < 'A' ? cElem - '0' : cElem - 'A' + 10;
         }
         else
         {
            if( nPosRet > 0 )
            {
               break;
            }
         }
      }
      else
      {
         cRet[ nPosRet ] = cElem;
      }

      nPos++;
      nPosRet++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = ( char * ) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclen_buffer( cRet, nPosRet );
}
