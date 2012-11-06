/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    CRC checksum functions
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.h"
#include "hbchksum.h"

/* X^32+X^26+X^23+X^22+X^16+X^12+X^11+X^10+X^8+X^7+X^5+X^4+X^2+X^1+X^0 */
/* 0x104C11DB7 => 0xEDB88320 */
static const HB_U32 crc32_tab[ 256 ] = {
   0x00000000L, 0x77073096L, 0xEE0E612CL, 0x990951BAL, 0x076DC419L,
   0x706AF48FL, 0xE963A535L, 0x9E6495A3L, 0x0EDB8832L, 0x79DCB8A4L,
   0xE0D5E91EL, 0x97D2D988L, 0x09B64C2BL, 0x7EB17CBDL, 0xE7B82D07L,
   0x90BF1D91L, 0x1DB71064L, 0x6AB020F2L, 0xF3B97148L, 0x84BE41DEL,
   0x1ADAD47DL, 0x6DDDE4EBL, 0xF4D4B551L, 0x83D385C7L, 0x136C9856L,
   0x646BA8C0L, 0xFD62F97AL, 0x8A65C9ECL, 0x14015C4FL, 0x63066CD9L,
   0xFA0F3D63L, 0x8D080DF5L, 0x3B6E20C8L, 0x4C69105EL, 0xD56041E4L,
   0xA2677172L, 0x3C03E4D1L, 0x4B04D447L, 0xD20D85FDL, 0xA50AB56BL,
   0x35B5A8FAL, 0x42B2986CL, 0xDBBBC9D6L, 0xACBCF940L, 0x32D86CE3L,
   0x45DF5C75L, 0xDCD60DCFL, 0xABD13D59L, 0x26D930ACL, 0x51DE003AL,
   0xC8D75180L, 0xBFD06116L, 0x21B4F4B5L, 0x56B3C423L, 0xCFBA9599L,
   0xB8BDA50FL, 0x2802B89EL, 0x5F058808L, 0xC60CD9B2L, 0xB10BE924L,
   0x2F6F7C87L, 0x58684C11L, 0xC1611DABL, 0xB6662D3DL, 0x76DC4190L,
   0x01DB7106L, 0x98D220BCL, 0xEFD5102AL, 0x71B18589L, 0x06B6B51FL,
   0x9FBFE4A5L, 0xE8B8D433L, 0x7807C9A2L, 0x0F00F934L, 0x9609A88EL,
   0xE10E9818L, 0x7F6A0DBBL, 0x086D3D2DL, 0x91646C97L, 0xE6635C01L,
   0x6B6B51F4L, 0x1C6C6162L, 0x856530D8L, 0xF262004EL, 0x6C0695EDL,
   0x1B01A57BL, 0x8208F4C1L, 0xF50FC457L, 0x65B0D9C6L, 0x12B7E950L,
   0x8BBEB8EAL, 0xFCB9887CL, 0x62DD1DDFL, 0x15DA2D49L, 0x8CD37CF3L,
   0xFBD44C65L, 0x4DB26158L, 0x3AB551CEL, 0xA3BC0074L, 0xD4BB30E2L,
   0x4ADFA541L, 0x3DD895D7L, 0xA4D1C46DL, 0xD3D6F4FBL, 0x4369E96AL,
   0x346ED9FCL, 0xAD678846L, 0xDA60B8D0L, 0x44042D73L, 0x33031DE5L,
   0xAA0A4C5FL, 0xDD0D7CC9L, 0x5005713CL, 0x270241AAL, 0xBE0B1010L,
   0xC90C2086L, 0x5768B525L, 0x206F85B3L, 0xB966D409L, 0xCE61E49FL,
   0x5EDEF90EL, 0x29D9C998L, 0xB0D09822L, 0xC7D7A8B4L, 0x59B33D17L,
   0x2EB40D81L, 0xB7BD5C3BL, 0xC0BA6CADL, 0xEDB88320L, 0x9ABFB3B6L,
   0x03B6E20CL, 0x74B1D29AL, 0xEAD54739L, 0x9DD277AFL, 0x04DB2615L,
   0x73DC1683L, 0xE3630B12L, 0x94643B84L, 0x0D6D6A3EL, 0x7A6A5AA8L,
   0xE40ECF0BL, 0x9309FF9DL, 0x0A00AE27L, 0x7D079EB1L, 0xF00F9344L,
   0x8708A3D2L, 0x1E01F268L, 0x6906C2FEL, 0xF762575DL, 0x806567CBL,
   0x196C3671L, 0x6E6B06E7L, 0xFED41B76L, 0x89D32BE0L, 0x10DA7A5AL,
   0x67DD4ACCL, 0xF9B9DF6FL, 0x8EBEEFF9L, 0x17B7BE43L, 0x60B08ED5L,
   0xD6D6A3E8L, 0xA1D1937EL, 0x38D8C2C4L, 0x4FDFF252L, 0xD1BB67F1L,
   0xA6BC5767L, 0x3FB506DDL, 0x48B2364BL, 0xD80D2BDAL, 0xAF0A1B4CL,
   0x36034AF6L, 0x41047A60L, 0xDF60EFC3L, 0xA867DF55L, 0x316E8EEFL,
   0x4669BE79L, 0xCB61B38CL, 0xBC66831AL, 0x256FD2A0L, 0x5268E236L,
   0xCC0C7795L, 0xBB0B4703L, 0x220216B9L, 0x5505262FL, 0xC5BA3BBEL,
   0xB2BD0B28L, 0x2BB45A92L, 0x5CB36A04L, 0xC2D7FFA7L, 0xB5D0CF31L,
   0x2CD99E8BL, 0x5BDEAE1DL, 0x9B64C2B0L, 0xEC63F226L, 0x756AA39CL,
   0x026D930AL, 0x9C0906A9L, 0xEB0E363FL, 0x72076785L, 0x05005713L,
   0x95BF4A82L, 0xE2B87A14L, 0x7BB12BAEL, 0x0CB61B38L, 0x92D28E9BL,
   0xE5D5BE0DL, 0x7CDCEFB7L, 0x0BDBDF21L, 0x86D3D2D4L, 0xF1D4E242L,
   0x68DDB3F8L, 0x1FDA836EL, 0x81BE16CDL, 0xF6B9265BL, 0x6FB077E1L,
   0x18B74777L, 0x88085AE6L, 0xFF0F6A70L, 0x66063BCAL, 0x11010B5CL,
   0x8F659EFFL, 0xF862AE69L, 0x616BFFD3L, 0x166CCF45L, 0xA00AE278L,
   0xD70DD2EEL, 0x4E048354L, 0x3903B3C2L, 0xA7672661L, 0xD06016F7L,
   0x4969474DL, 0x3E6E77DBL, 0xAED16A4AL, 0xD9D65ADCL, 0x40DF0B66L,
   0x37D83BF0L, 0xA9BCAE53L, 0xDEBB9EC5L, 0x47B2CF7FL, 0x30B5FFE9L,
   0xBDBDF21CL, 0xCABAC28AL, 0x53B39330L, 0x24B4A3A6L, 0xBAD03605L,
   0xCDD70693L, 0x54DE5729L, 0x23D967BFL, 0xB3667A2EL, 0xC4614AB8L,
   0x5D681B02L, 0x2A6F2B94L, 0xB40BBE37L, 0xC30C8EA1L, 0x5A05DF1BL,
   0x2D02EF8DL
};

/* X^16+X^15+X^2+X^0 */
/* 0x18005 => 0xA001 */
static const HB_U16 crc16_tab[] =
{
   0x0000, 0xC0C1, 0xC181, 0x0140, 0xC301, 0x03C0, 0x0280, 0xC241,
   0xC601, 0x06C0, 0x0780, 0xC741, 0x0500, 0xC5C1, 0xC481, 0x0440,
   0xCC01, 0x0CC0, 0x0D80, 0xCD41, 0x0F00, 0xCFC1, 0xCE81, 0x0E40,
   0x0A00, 0xCAC1, 0xCB81, 0x0B40, 0xC901, 0x09C0, 0x0880, 0xC841,
   0xD801, 0x18C0, 0x1980, 0xD941, 0x1B00, 0xDBC1, 0xDA81, 0x1A40,
   0x1E00, 0xDEC1, 0xDF81, 0x1F40, 0xDD01, 0x1DC0, 0x1C80, 0xDC41,
   0x1400, 0xD4C1, 0xD581, 0x1540, 0xD701, 0x17C0, 0x1680, 0xD641,
   0xD201, 0x12C0, 0x1380, 0xD341, 0x1100, 0xD1C1, 0xD081, 0x1040,
   0xF001, 0x30C0, 0x3180, 0xF141, 0x3300, 0xF3C1, 0xF281, 0x3240,
   0x3600, 0xF6C1, 0xF781, 0x3740, 0xF501, 0x35C0, 0x3480, 0xF441,
   0x3C00, 0xFCC1, 0xFD81, 0x3D40, 0xFF01, 0x3FC0, 0x3E80, 0xFE41,
   0xFA01, 0x3AC0, 0x3B80, 0xFB41, 0x3900, 0xF9C1, 0xF881, 0x3840,
   0x2800, 0xE8C1, 0xE981, 0x2940, 0xEB01, 0x2BC0, 0x2A80, 0xEA41,
   0xEE01, 0x2EC0, 0x2F80, 0xEF41, 0x2D00, 0xEDC1, 0xEC81, 0x2C40,
   0xE401, 0x24C0, 0x2580, 0xE541, 0x2700, 0xE7C1, 0xE681, 0x2640,
   0x2200, 0xE2C1, 0xE381, 0x2340, 0xE101, 0x21C0, 0x2080, 0xE041,
   0xA001, 0x60C0, 0x6180, 0xA141, 0x6300, 0xA3C1, 0xA281, 0x6240,
   0x6600, 0xA6C1, 0xA781, 0x6740, 0xA501, 0x65C0, 0x6480, 0xA441,
   0x6C00, 0xACC1, 0xAD81, 0x6D40, 0xAF01, 0x6FC0, 0x6E80, 0xAE41,
   0xAA01, 0x6AC0, 0x6B80, 0xAB41, 0x6900, 0xA9C1, 0xA881, 0x6840,
   0x7800, 0xB8C1, 0xB981, 0x7940, 0xBB01, 0x7BC0, 0x7A80, 0xBA41,
   0xBE01, 0x7EC0, 0x7F80, 0xBF41, 0x7D00, 0xBDC1, 0xBC81, 0x7C40,
   0xB401, 0x74C0, 0x7580, 0xB541, 0x7700, 0xB7C1, 0xB681, 0x7640,
   0x7200, 0xB2C1, 0xB381, 0x7340, 0xB101, 0x71C0, 0x7080, 0xB041,
   0x5000, 0x90C1, 0x9181, 0x5140, 0x9301, 0x53C0, 0x5280, 0x9241,
   0x9601, 0x56C0, 0x5780, 0x9741, 0x5500, 0x95C1, 0x9481, 0x5440,
   0x9C01, 0x5CC0, 0x5D80, 0x9D41, 0x5F00, 0x9FC1, 0x9E81, 0x5E40,
   0x5A00, 0x9AC1, 0x9B81, 0x5B40, 0x9901, 0x59C0, 0x5880, 0x9841,
   0x8801, 0x48C0, 0x4980, 0x8941, 0x4B00, 0x8BC1, 0x8A81, 0x4A40,
   0x4E00, 0x8EC1, 0x8F81, 0x4F40, 0x8D01, 0x4DC0, 0x4C80, 0x8C41,
   0x4400, 0x84C1, 0x8581, 0x4540, 0x8701, 0x47C0, 0x4680, 0x8641,
   0x8201, 0x42C0, 0x4380, 0x8341, 0x4100, 0x81C1, 0x8081, 0x4040
};


HB_U32 hb_crc32( HB_U32 crc, const void * buf, HB_SIZE len )
{
   crc ^= 0xffffffffL;
   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      do
      {
         crc = crc32_tab[ ( crc ^ *ucbuf++ ) & 0xFF ] ^ ( crc >> 8 );
      }
      while( --len );
   }
   return crc ^ 0xffffffffL;
}

HB_U16 hb_crc16( HB_U16 crc, const void * buf, HB_SIZE len )
{
   crc ^= 0xffff;
   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      do
      {
         crc = crc16_tab[ ( crc ^ *ucbuf++ ) & 0xFF ] ^ ( crc >> 8 );
      }
      while( --len );
   }
   return crc ^ 0xffff;
}

HB_MAXUINT hb_crc( HB_MAXUINT crc, const void * buf, HB_SIZE len, HB_MAXUINT poly )
{
   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      HB_MAXUINT mask = 1, revp = 0;

      while( poly > 1 )
      {
         mask <<= 1;
         revp <<= 1;
         if( poly & 1 )
            revp |= 1;
         poly >>= 1;
      }
      crc ^= --mask;
      do
      {
         int i = 8;
         crc ^= *ucbuf++;
         do
         {
            crc = crc & 1 ? revp ^ ( crc >> 1 ) : crc >> 1;
         }
         while( --i );
      }
      while( --len );
      crc ^= mask;
   }
   return crc;
}

HB_MAXUINT hb_crcct( HB_MAXUINT crc, const void * buf, HB_SIZE len, HB_MAXUINT poly )
{
   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      HB_MAXUINT mask, revp = poly;
      int bits = 0;

      while( revp >>= 1 )
         ++bits;
      mask = ( HB_MAXINT ) 1 << ( bits - 1 );
      bits -= 8;
      if( bits < 0 )
      {
         mask <<= -bits;
         poly <<= -bits;
         crc <<= -bits;
         do
         {
            int i = 8;
            crc ^= ( HB_MAXUINT ) ( *ucbuf++ );
            do
            {
               crc = crc & mask ? poly ^ ( crc << 1 ) : crc << 1;
            }
            while( --i );
         }
         while( --len );
         crc &= ( mask << 1 ) - 1;
         crc >>= -bits;
      }
      else
      {
         do
         {
            int i = 8;
            crc ^= ( HB_MAXUINT ) ( *ucbuf++ ) << bits;
            do
            {
               crc = crc & mask ? poly ^ ( crc << 1 ) : crc << 1;
            }
            while( --i );
         }
         while( --len );
         crc &= ( mask << 1 ) - 1;
      }
   }
   return crc;
}

HB_FUNC( HB_CRC32 )
{
   const char * szString = hb_parc( 1 );

   if( szString )
      hb_retnint( hb_crc32( ( HB_U32 ) hb_parnl( 2 ), szString, hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CRC16 )
{
   const char * szString = hb_parc( 1 );

   if( szString )
      hb_retnint( hb_crc16( ( HB_U16 ) hb_parnl( 2 ), szString, hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CRC )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_MAXUINT ulPolynomial = ( HB_MAXUINT ) hb_parnint( 3 );
      if( ulPolynomial == 0 )
         ulPolynomial = 0x11021;
      hb_retnint( hb_crc( ( HB_MAXUINT ) hb_parnint( 2 ), szString, hb_parclen( 1 ), ulPolynomial ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CRCCT )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_MAXUINT ulPolynomial = ( HB_MAXUINT ) hb_parnint( 3 );
      if( ulPolynomial == 0 )
         ulPolynomial = 0x11021;
      hb_retnint( hb_crcct( ( HB_MAXUINT ) hb_parnint( 2 ), szString, hb_parclen( 1 ), ulPolynomial ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
