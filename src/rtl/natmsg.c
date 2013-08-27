/*
 * Harbour Project source code:
 * NATION undocumented functions
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    __natSortVer()
 *    __natMsgVer()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapicdp.h"

/* NOTE: Ad-hoc names mostly taken from various Clipper source files.
         These should be named properly if exported outside this file.
         [vszakats] */

#define _DIR_HEADER      1              /* "Database Files    # Records    Last Update     Size" */
#define _LF_SAMPLES      2              /* "Do you want more samples?" */
#define _RF_PAGENO       3              /* "Page No." */
#define _RF_SUBTOTAL     4              /* "** Subtotal **" */
#define _RF_SUBSUBTOTAL  5              /* "* Subsubtotal *" */
#define _RF_TOTAL        6              /* "*** Total ***" */
#define _GET_INSERT_ON   7              /* "Ins" */
#define _GET_INSERT_OFF  8              /* "   " */
#define _GET_INVD_DATE   9              /* "Invalid Date" */
#define _GET_RANGE_FROM  10             /* "Range: " */
#define _GET_RANGE_TO    11             /* " - " */
#define _LF_YN           12             /* "Y/N" */ /* NOTE: This must be in uppercase. [vszakats] */
#define _INVALID_EXPR    13             /* "INVALID EXPRESSION" */

static const char * hb_nationGetMsg( int iMsg )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_nationGetMsg(%u)", iMsg ) );

   return ( iMsg >= 1 && iMsg <= 13 ) ? hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + iMsg - 1 ) : "";
}

HB_FUNC( __NATISAFFIRM )
{
   HB_SIZE nLen = hb_parclen( 1 );
   HB_BOOL fIS = HB_FALSE;

   if( nLen > 0 )
   {
      const char * szYesNo = hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 );
      HB_SIZE nStr = 0;

      while( szYesNo[ nStr ] && szYesNo[ nStr ] != '/' )
         ++nStr;

      if( nStr && nLen >= nStr )
      {
         PHB_CODEPAGE cdp = hb_vmCDP();
         if( cdp )
            fIS = hb_cdpicmp( hb_parc( 1 ), nLen, szYesNo, nStr, cdp, HB_FALSE ) == 0;
         else
            fIS = hb_strnicmp( hb_parc( 1 ), szYesNo, nStr ) == 0;
      }
   }
   hb_retl( fIS );
}

HB_FUNC( __NATISNEGATIVE )
{
   HB_SIZE nLen = hb_parclen( 1 );
   HB_BOOL fIS = HB_FALSE;

   if( nLen > 0 )
   {
      const char * szYesNo = hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 );
      HB_SIZE nStr;

      while( *szYesNo )
         if( *szYesNo++ == '/' )
            break;
      nStr = strlen( szYesNo );

      if( nStr && nLen >= nStr )
      {
         PHB_CODEPAGE cdp = hb_vmCDP();
         if( cdp )
            fIS = hb_cdpicmp( hb_parc( 1 ), nLen, szYesNo, nStr, cdp, HB_FALSE ) == 0;
         else
            fIS = hb_strnicmp( hb_parc( 1 ), szYesNo, nStr ) == 0;
      }
   }
   hb_retl( fIS );
}

HB_FUNC( __NATMSG )
{
   if( hb_pcount() == 0 )
      /* TODO: Replace this with Language API call. */
      hb_retc_const( "Invalid argument" );
   else if( HB_ISNUM( 1 ) )
      hb_retc_const( hb_nationGetMsg( hb_parni( 1 ) ) );
   else
      hb_retc_null();
}

HB_FUNC( __NATMSGVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATMSGS v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATMSGS v1.3i x19 06/Mar/95" */

   hb_retc_const( "NATMSGS (Harbour)" );
}
