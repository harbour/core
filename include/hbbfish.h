/*
 * Harbour Project source code:
 *    This code implements BlowFish algorithm designed by Bruce Schneier.
 *    The description of BlowFish algorithm can be found at:
 *       https://www.schneier.com/paper-blowfish-fse.html
 *    This code uses for initial s-boxes and p-array values PI hex digits
 *    taken from tables public at:
 *       https://www.schneier.com/blowfish.html
 *    which can be downloaded from:
 *       https://www.schneier.com/code/constants.txt
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

#ifndef HB_BLOWFISH_H_
#define HB_BLOWFISH_H_

#include "hbdefs.h"

HB_EXTERN_BEGIN

#define SUBKEYS_COUNT   18
#define SBOX_ENTRIES    256

typedef struct
{
   HB_U32   P[ SUBKEYS_COUNT ];
   HB_U32   S1[ SBOX_ENTRIES ];
   HB_U32   S2[ SBOX_ENTRIES ];
   HB_U32   S3[ SBOX_ENTRIES ];
   HB_U32   S4[ SBOX_ENTRIES ];
}
HB_BLOWFISH;

extern HB_EXPORT void hb_blowfishInit( HB_BLOWFISH * bf, const void * keydata, int keylen );
extern HB_EXPORT void hb_blowfishEncrypt( const HB_BLOWFISH * bf, HB_U32 * xl, HB_U32 * xr );
extern HB_EXPORT void hb_blowfishDecrypt( const HB_BLOWFISH * bf, HB_U32 * xl, HB_U32 * xr );

HB_EXTERN_END

#endif /* HB_BLOWFISH_H_ */
