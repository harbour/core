/*
 * Harbour Project source code:
 *    code used to register new CP definition
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapicdp.h"

HB_CODEPAGE_ANNOUNCE( HB_CP_ID )

#if defined( HB_PRAGMA_STARTUP )
HB_CALL_ON_STARTUP_BEGIN( _hb_codepage_Init_ )
#else
HB_CALL_ON_STARTUP_BEGIN( HB_MACRONAME_JOIN( _hb_codepage_Init_, HB_CP_ID ) )
#endif

#if defined( HB_CP_RAW )
   #if defined( HB_CP_CUSTOM )
      #define HB_CP_TP_CUSTOM    HB_CDP_TYPE_CUSTOM
   #else
      #if defined( HB_CP_GET_FUNC ) && \
          defined( HB_CP_PUT_FUNC ) && \
          defined( HB_CP_LEN_FUNC )
         #define HB_CP_TP_CUSTOM    HB_CDP_TYPE_CUSTOM
      #else
         #define HB_CP_TP_CUSTOM    0
         #define HB_CP_GET_FUNC     NULL
         #define HB_CP_PUT_FUNC     NULL
         #define HB_CP_LEN_FUNC     NULL
      #endif
      #if ! defined( HB_CP_UPPER_FUNC ) && \
          ! defined( HB_CP_LOWER_FUNC ) && \
          ! defined( HB_CP_FLAG_FUNC )
         #define HB_CP_UPPER_FUNC   NULL
         #define HB_CP_LOWER_FUNC   NULL
         #define HB_CP_FLAGS_FUNC   NULL
      #endif
      #ifndef HB_CP_CMP_FUNC
         #define HB_CP_CMP_FUNC     NULL
      #endif
      #ifndef HB_CP_CMPI_FUNC
         #define HB_CP_CMPI_FUNC    NULL
      #endif
   #endif

   #if defined( HB_CP_CHARIDX )
      #define HB_CP_TP_CHARIDX   HB_CDP_TYPE_CHARIDX
   #else
      #define HB_CP_TP_CHARIDX   0
   #endif

   #if defined( HB_CP_CHARUNI )
      #define HB_CP_TP_CHARUNI   HB_CDP_TYPE_CHARUNI
   #else
      #define HB_CP_TP_CHARUNI   0
   #endif

   #if defined( HB_CP_UTF8 )
      #define HB_CP_TP_UTF8      HB_CDP_TYPE_UTF8
   #else
      #define HB_CP_TP_UTF8      0
   #endif

   static HB_CODEPAGE s_codePage =
   {
      HB_MACRO2STRING( HB_CP_ID ),
      HB_CP_INFO,
      HB_CP_UNITB,
      s_flags,
      s_upper,
      s_lower,
      s_sort,
      NULL,
      HB_CDP_ACSORT_NONE,
      ( HB_CP_TP_CUSTOM | HB_CP_TP_CHARIDX | HB_CP_TP_CHARUNI | HB_CP_TP_UTF8 ),
      HB_CP_GET_FUNC,
      HB_CP_PUT_FUNC,
      HB_CP_LEN_FUNC,
      HB_CP_UPPER_FUNC,
      HB_CP_LOWER_FUNC,
      HB_CP_FLAGS_FUNC,
      HB_CP_CMP_FUNC,
      HB_CP_CMPI_FUNC,
      0,
      0,
      NULL,
      NULL,
      NULL,
   };
   #if defined( HB_CP_INIT )
      HB_CP_INIT( &s_codePage );
   #endif
   hb_cdpRegisterRaw( &s_codePage );
#else
   #ifndef HB_CP_CSSORT
      #define HB_CP_CSSORT    HB_CDP_CSSORT_UPLO
   #endif
   #ifdef HB_CP_UTF8
      #define HB_CP_UTF8_STR  HB_TRUE
   #else
      #define HB_CP_UTF8_STR  HB_FALSE
   #endif
   hb_cdpRegisterNew( HB_MACRO2STRING( HB_CP_ID ), HB_CP_INFO, HB_CP_UNITB,
                      HB_CP_UPPER, HB_CP_LOWER, HB_CP_ACSORT, HB_CP_CSSORT,
                      HB_CP_UTF8_STR );
#endif /* HB_CP_RAW */

#if defined( HB_PRAGMA_STARTUP )
HB_CALL_ON_STARTUP_END( _hb_codepage_Init_ )
#else
HB_CALL_ON_STARTUP_END( HB_MACRONAME_JOIN( _hb_codepage_Init_, HB_CP_ID ) )
#endif

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_codepage_Init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    \
         HB_DATASEG_FUNC( HB_MACRONAME_JOIN( _hb_codepage_Init_, HB_CP_ID ) )
   #include "hbiniseg.h"
#endif
