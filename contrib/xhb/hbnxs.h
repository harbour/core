/*
 * xHarbour Project source code:
 * Cryptography for xharbour
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR NXS BELOW.
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

/***************************************************************
 * NXS aglorithm is FREE SOFTWARE. It can be reused for any
 * purpose, provided that this copiright notice is still present
 * in the software.
 *
 * This program is distributed WITHOUT ANY WARRANTY that it can
 * fit any particular need.
 *
 * NXS author is Giancarlo Niccolai <giancarlo@niccolai.ws>
 *
 * Adler 32 CRC is copyrighted by Martin Adler
 **************************************************************/

#ifndef HBNXS_H
#define HBNXS_H

#include "hbdefs.h"

#define NXS_MAX_KEYLEN  256

void nxs_crypt(
   const unsigned char * source, HB_SIZE srclen,
   const unsigned char * key, HB_SIZE keylen,
   unsigned char * cipher );

void nxs_decrypt(
   const unsigned char * cipher, HB_SIZE cypherlen,
   const unsigned char * key, HB_SIZE keylen,
   unsigned char * result );

void nxs_scramble(
   const unsigned char * source, HB_SIZE srclen,
   const unsigned char * key, HB_SIZE keylen,
   unsigned char * cipher );

void nxs_partial_scramble(
   const unsigned char * source, unsigned char * cipher,
   HB_ISIZ * scramble,
   HB_SIZE len, HB_SIZE keylen );

void nxs_partial_unscramble(
   unsigned char * cipher,
   HB_ISIZ * scramble,
   HB_SIZE len, HB_SIZE keylen );

void nxs_unscramble(
   unsigned char * cipher, HB_SIZE cypherlen,
   const unsigned char * key, HB_SIZE keylen );

void nxs_xorcode(
   unsigned char * cipher, HB_SIZE cipherlen,
   const unsigned char * key, HB_SIZE keylen );

void nxs_xordecode(
   unsigned char * cipher, HB_SIZE cipherlen,
   const unsigned char * key, HB_SIZE keylen );

void nxs_xorcyclic(
   unsigned char * cipher, HB_SIZE cipherlen,
   const unsigned char * key, HB_SIZE keylen );

HB_U32 nxs_cyclic_sequence( HB_U32 input );

void nxs_make_scramble(
   HB_ISIZ * scramble,
   const unsigned char * key,
   HB_SIZE keylen );

#endif
