/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#ifndef HB_ZEBRA_H_
#define HB_ZEBRA_H_

#include "hbapi.h"
#include "hbzebra.ch"

typedef struct
{
   HB_BYTE * pBuffer;
   HB_SIZE   nLen;
   HB_SIZE   nAlloc;
   void *    pCargo;
} HB_BITBUFFER, * PHB_BITBUFFER;

typedef struct
{
   int             iType;
   int             iError;
   int             iCol;
   char *          szCode;
   PHB_BITBUFFER   pBits;
} HB_ZEBRA, * PHB_ZEBRA;

HB_EXTERN_BEGIN

extern HB_EXPORT PHB_BITBUFFER hb_bitbuffer_create( void );
extern HB_EXPORT void          hb_bitbuffer_destroy( PHB_BITBUFFER pBitBuffer );
extern HB_EXPORT void          hb_bitbuffer_set( PHB_BITBUFFER pBitBuffer, HB_SIZE nPos, HB_BOOL fValue );
extern HB_EXPORT void          hb_bitbuffer_cat_int( PHB_BITBUFFER pBitBuffer, int iValue, int iLen );
extern HB_EXPORT HB_SIZE       hb_bitbuffer_len( PHB_BITBUFFER pBitBuffer );
extern HB_EXPORT HB_BOOL       hb_bitbuffer_get( PHB_BITBUFFER pBitBuffer, HB_SIZE nPos );

extern HB_EXPORT PHB_ZEBRA     hb_zebra_create( void );
extern HB_EXPORT void          hb_zebra_destroy( PHB_ZEBRA pZebra );

extern HB_EXPORT PHB_ZEBRA     hb_zebraItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM      hb_zebraItemPut( PHB_ITEM pItem, PHB_ZEBRA pZebra );
extern HB_EXPORT void          hb_zebraItemClear( PHB_ITEM pItem );
extern HB_EXPORT PHB_ZEBRA     hb_zebra_param( int iParam );
extern HB_EXPORT void          hb_zebra_ret( PHB_ZEBRA pZebra );

HB_EXTERN_END

#endif /* HB_ZEBRA_H_ */
