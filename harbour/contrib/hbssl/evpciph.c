/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (EVP CIPHER) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbapierr.h"
#include "hbapiitm.h"

#include "hbssl.ch"

#include <openssl/evp.h>

HB_FUNC( OPENSSL_ADD_ALL_CIPHERS )
{
   OpenSSL_add_all_ciphers();
}

static HB_GARBAGE_FUNC( EVP_CIPHER_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      hb_xfree( * ph );

      /* set pointer to NULL just in case */
      * ph = NULL;
   }
}

static void * hb_EVP_CIPHER_CTX_is( int iParam )
{
   return hb_parptrGC( EVP_CIPHER_CTX_release, iParam );
}

static EVP_CIPHER_CTX * hb_EVP_CIPHER_CTX_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( EVP_CIPHER_CTX_release, iParam );

   return ph ? ( EVP_CIPHER_CTX * ) * ph : NULL;
}

int hb_EVP_CIPHER_is( int iParam )
{
   return HB_ISCHAR( iParam ) || HB_ISNUM( iParam );
}

const EVP_CIPHER * hb_EVP_CIPHER_par( int iParam )
{
   const EVP_CIPHER * method;

   if( HB_ISCHAR( iParam ) )
      return EVP_get_cipherbyname( hb_parc( iParam ) );

   switch( hb_parni( iParam ) )
   {
   case HB_EVP_CIPHER_ENC_NULL            : method = EVP_enc_null();            break;
#ifndef OPENSSL_NO_DES
   case HB_EVP_CIPHER_DES_ECB             : method = EVP_des_ecb();             break;
   case HB_EVP_CIPHER_DES_EDE             : method = EVP_des_ede();             break;
   case HB_EVP_CIPHER_DES_EDE3            : method = EVP_des_ede3();            break;
   case HB_EVP_CIPHER_DES_EDE_ECB         : method = EVP_des_ede_ecb();         break;
   case HB_EVP_CIPHER_DES_EDE3_ECB        : method = EVP_des_ede3_ecb();        break;
   case HB_EVP_CIPHER_DES_CFB64           : method = EVP_des_cfb64();           break;
   case HB_EVP_CIPHER_DES_CFB             : method = EVP_des_cfb();             break;
   case HB_EVP_CIPHER_DES_CFB1            : method = EVP_des_cfb1();            break;
   case HB_EVP_CIPHER_DES_CFB8            : method = EVP_des_cfb8();            break;
   case HB_EVP_CIPHER_DES_EDE_CFB64       : method = EVP_des_ede_cfb64();       break;
   case HB_EVP_CIPHER_DES_EDE_CFB         : method = EVP_des_ede_cfb();         break;
   case HB_EVP_CIPHER_DES_EDE3_CFB64      : method = EVP_des_ede3_cfb64();      break;
   case HB_EVP_CIPHER_DES_EDE3_CFB        : method = EVP_des_ede3_cfb();        break;
   case HB_EVP_CIPHER_DES_EDE3_CFB1       : method = EVP_des_ede3_cfb1();       break;
   case HB_EVP_CIPHER_DES_EDE3_CFB8       : method = EVP_des_ede3_cfb8();       break;
   case HB_EVP_CIPHER_DES_OFB             : method = EVP_des_ofb();             break;
   case HB_EVP_CIPHER_DES_EDE_OFB         : method = EVP_des_ede_ofb();         break;
   case HB_EVP_CIPHER_DES_EDE3_OFB        : method = EVP_des_ede3_ofb();        break;
   case HB_EVP_CIPHER_DES_CBC             : method = EVP_des_cbc();             break;
   case HB_EVP_CIPHER_DES_EDE_CBC         : method = EVP_des_ede_cbc();         break;
   case HB_EVP_CIPHER_DES_EDE3_CBC        : method = EVP_des_ede3_cbc();        break;
   case HB_EVP_CIPHER_DESX_CBC            : method = EVP_desx_cbc();            break;
#endif
#ifndef OPENSSL_NO_RC4
   case HB_EVP_CIPHER_RC4                 : method = EVP_rc4();                 break;
   case HB_EVP_CIPHER_RC4_40              : method = EVP_rc4_40();              break;
#endif
#ifndef OPENSSL_NO_IDEA
   case HB_EVP_CIPHER_IDEA_ECB            : method = EVP_idea_ecb();            break;
   case HB_EVP_CIPHER_IDEA_CFB64          : method = EVP_idea_cfb64();          break;
   case HB_EVP_CIPHER_IDEA_CFB            : method = EVP_idea_cfb();            break;
   case HB_EVP_CIPHER_IDEA_OFB            : method = EVP_idea_ofb();            break;
   case HB_EVP_CIPHER_IDEA_CBC            : method = EVP_idea_cbc();            break;
#endif
#ifndef OPENSSL_NO_RC2
   case HB_EVP_CIPHER_RC2_ECB             : method = EVP_rc2_ecb();             break;
   case HB_EVP_CIPHER_RC2_CBC             : method = EVP_rc2_cbc();             break;
   case HB_EVP_CIPHER_RC2_40_CBC          : method = EVP_rc2_40_cbc();          break;
   case HB_EVP_CIPHER_RC2_64_CBC          : method = EVP_rc2_64_cbc();          break;
   case HB_EVP_CIPHER_RC2_CFB64           : method = EVP_rc2_cfb64();           break;
   case HB_EVP_CIPHER_RC2_CFB             : method = EVP_rc2_cfb();             break;
   case HB_EVP_CIPHER_RC2_OFB             : method = EVP_rc2_ofb();             break;
#endif
#ifndef OPENSSL_NO_BF
   case HB_EVP_CIPHER_BF_ECB              : method = EVP_bf_ecb();              break;
   case HB_EVP_CIPHER_BF_CBC              : method = EVP_bf_cbc();              break;
   case HB_EVP_CIPHER_BF_CFB64            : method = EVP_bf_cfb64();            break;
   case HB_EVP_CIPHER_BF_CFB              : method = EVP_bf_cfb();              break;
   case HB_EVP_CIPHER_BF_OFB              : method = EVP_bf_ofb();              break;
#endif
#ifndef OPENSSL_NO_CAST
   case HB_EVP_CIPHER_CAST5_ECB           : method = EVP_cast5_ecb();           break;
   case HB_EVP_CIPHER_CAST5_CBC           : method = EVP_cast5_cbc();           break;
   case HB_EVP_CIPHER_CAST5_CFB64         : method = EVP_cast5_cfb64();         break;
   case HB_EVP_CIPHER_CAST5_CFB           : method = EVP_cast5_cfb();           break;
   case HB_EVP_CIPHER_CAST5_OFB           : method = EVP_cast5_ofb();           break;
#endif
#ifndef OPENSSL_NO_RC5
   case HB_EVP_CIPHER_RC5_32_12_16_CBC    : method = EVP_rc5_32_12_16_cbc();    break;
   case HB_EVP_CIPHER_RC5_32_12_16_ECB    : method = EVP_rc5_32_12_16_ecb();    break;
   case HB_EVP_CIPHER_RC5_32_12_16_CFB64  : method = EVP_rc5_32_12_16_cfb64();  break;
   case HB_EVP_CIPHER_RC5_32_12_16_CFB    : method = EVP_rc5_32_12_16_cfb();    break;
   case HB_EVP_CIPHER_RC5_32_12_16_OFB    : method = EVP_rc5_32_12_16_ofb();    break;
#endif
#ifndef OPENSSL_NO_AES
   case HB_EVP_CIPHER_AES_128_ECB         : method = EVP_aes_128_ecb();         break;
   case HB_EVP_CIPHER_AES_128_CBC         : method = EVP_aes_128_cbc();         break;
   case HB_EVP_CIPHER_AES_128_CFB1        : method = EVP_aes_128_cfb1();        break;
   case HB_EVP_CIPHER_AES_128_CFB8        : method = EVP_aes_128_cfb8();        break;
   case HB_EVP_CIPHER_AES_128_CFB128      : method = EVP_aes_128_cfb128();      break;
   case HB_EVP_CIPHER_AES_128_CFB         : method = EVP_aes_128_cfb();         break;
   case HB_EVP_CIPHER_AES_128_OFB         : method = EVP_aes_128_ofb();         break;
   case HB_EVP_CIPHER_AES_192_ECB         : method = EVP_aes_192_ecb();         break;
   case HB_EVP_CIPHER_AES_192_CBC         : method = EVP_aes_192_cbc();         break;
   case HB_EVP_CIPHER_AES_192_CFB1        : method = EVP_aes_192_cfb1();        break;
   case HB_EVP_CIPHER_AES_192_CFB8        : method = EVP_aes_192_cfb8();        break;
   case HB_EVP_CIPHER_AES_192_CFB128      : method = EVP_aes_192_cfb128();      break;
   case HB_EVP_CIPHER_AES_192_CFB         : method = EVP_aes_192_cfb();         break;
   case HB_EVP_CIPHER_AES_192_OFB         : method = EVP_aes_192_ofb();         break;
   case HB_EVP_CIPHER_AES_256_ECB         : method = EVP_aes_256_ecb();         break;
   case HB_EVP_CIPHER_AES_256_CBC         : method = EVP_aes_256_cbc();         break;
   case HB_EVP_CIPHER_AES_256_CFB1        : method = EVP_aes_256_cfb1();        break;
   case HB_EVP_CIPHER_AES_256_CFB8        : method = EVP_aes_256_cfb8();        break;
   case HB_EVP_CIPHER_AES_256_CFB128      : method = EVP_aes_256_cfb128();      break;
   case HB_EVP_CIPHER_AES_256_CFB         : method = EVP_aes_256_cfb();         break;
   case HB_EVP_CIPHER_AES_256_OFB         : method = EVP_aes_256_ofb();         break;
#endif
#ifndef OPENSSL_NO_CAMELLIA
   case HB_EVP_CIPHER_CAMELLIA_128_ECB    : method = EVP_camellia_128_ecb();    break;
   case HB_EVP_CIPHER_CAMELLIA_128_CBC    : method = EVP_camellia_128_cbc();    break;
   case HB_EVP_CIPHER_CAMELLIA_128_CFB1   : method = EVP_camellia_128_cfb1();   break;
   case HB_EVP_CIPHER_CAMELLIA_128_CFB8   : method = EVP_camellia_128_cfb8();   break;
   case HB_EVP_CIPHER_CAMELLIA_128_CFB128 : method = EVP_camellia_128_cfb128(); break;
   case HB_EVP_CIPHER_CAMELLIA_128_CFB    : method = EVP_camellia_128_cfb();    break;
   case HB_EVP_CIPHER_CAMELLIA_128_OFB    : method = EVP_camellia_128_ofb();    break;
   case HB_EVP_CIPHER_CAMELLIA_192_ECB    : method = EVP_camellia_192_ecb();    break;
   case HB_EVP_CIPHER_CAMELLIA_192_CBC    : method = EVP_camellia_192_cbc();    break;
   case HB_EVP_CIPHER_CAMELLIA_192_CFB1   : method = EVP_camellia_192_cfb1();   break;
   case HB_EVP_CIPHER_CAMELLIA_192_CFB8   : method = EVP_camellia_192_cfb8();   break;
   case HB_EVP_CIPHER_CAMELLIA_192_CFB128 : method = EVP_camellia_192_cfb128(); break;
   case HB_EVP_CIPHER_CAMELLIA_192_CFB    : method = EVP_camellia_192_cfb();    break;
   case HB_EVP_CIPHER_CAMELLIA_192_OFB    : method = EVP_camellia_192_ofb();    break;
   case HB_EVP_CIPHER_CAMELLIA_256_ECB    : method = EVP_camellia_256_ecb();    break;
   case HB_EVP_CIPHER_CAMELLIA_256_CBC    : method = EVP_camellia_256_cbc();    break;
   case HB_EVP_CIPHER_CAMELLIA_256_CFB1   : method = EVP_camellia_256_cfb1();   break;
   case HB_EVP_CIPHER_CAMELLIA_256_CFB8   : method = EVP_camellia_256_cfb8();   break;
   case HB_EVP_CIPHER_CAMELLIA_256_CFB128 : method = EVP_camellia_256_cfb128(); break;
   case HB_EVP_CIPHER_CAMELLIA_256_CFB    : method = EVP_camellia_256_cfb();    break;
   case HB_EVP_CIPHER_CAMELLIA_256_OFB    : method = EVP_camellia_256_ofb();    break;
#endif
#ifndef OPENSSL_NO_SEED
   case HB_EVP_CIPHER_SEED_ECB            : method = EVP_seed_ecb();            break;
   case HB_EVP_CIPHER_SEED_CBC            : method = EVP_seed_cbc();            break;
   case HB_EVP_CIPHER_SEED_CFB128         : method = EVP_seed_cfb128();         break;
   case HB_EVP_CIPHER_SEED_CFB            : method = EVP_seed_cfb();            break;
   case HB_EVP_CIPHER_SEED_OFB            : method = EVP_seed_ofb();            break;
#endif
   default                                : method = NULL;
   }

   return method;
}

static int hb_EVP_CIPHER_ptr_to_id( const EVP_CIPHER * method )
{
   int n;

   if(      method == EVP_enc_null()            ) n = HB_EVP_CIPHER_ENC_NULL;
#ifndef OPENSSL_NO_DES
   else if( method == EVP_des_ecb()             ) n = HB_EVP_CIPHER_DES_ECB;
   else if( method == EVP_des_ede()             ) n = HB_EVP_CIPHER_DES_EDE;
   else if( method == EVP_des_ede3()            ) n = HB_EVP_CIPHER_DES_EDE3;
   else if( method == EVP_des_ede_ecb()         ) n = HB_EVP_CIPHER_DES_EDE_ECB;
   else if( method == EVP_des_ede3_ecb()        ) n = HB_EVP_CIPHER_DES_EDE3_ECB;
   else if( method == EVP_des_cfb64()           ) n = HB_EVP_CIPHER_DES_CFB64;
   else if( method == EVP_des_cfb()             ) n = HB_EVP_CIPHER_DES_CFB;
   else if( method == EVP_des_cfb1()            ) n = HB_EVP_CIPHER_DES_CFB1;
   else if( method == EVP_des_cfb8()            ) n = HB_EVP_CIPHER_DES_CFB8;
   else if( method == EVP_des_ede_cfb64()       ) n = HB_EVP_CIPHER_DES_EDE_CFB64;
   else if( method == EVP_des_ede_cfb()         ) n = HB_EVP_CIPHER_DES_EDE_CFB;
   else if( method == EVP_des_ede3_cfb64()      ) n = HB_EVP_CIPHER_DES_EDE3_CFB64;
   else if( method == EVP_des_ede3_cfb()        ) n = HB_EVP_CIPHER_DES_EDE3_CFB;
   else if( method == EVP_des_ede3_cfb1()       ) n = HB_EVP_CIPHER_DES_EDE3_CFB1;
   else if( method == EVP_des_ede3_cfb8()       ) n = HB_EVP_CIPHER_DES_EDE3_CFB8;
   else if( method == EVP_des_ofb()             ) n = HB_EVP_CIPHER_DES_OFB;
   else if( method == EVP_des_ede_ofb()         ) n = HB_EVP_CIPHER_DES_EDE_OFB;
   else if( method == EVP_des_ede3_ofb()        ) n = HB_EVP_CIPHER_DES_EDE3_OFB;
   else if( method == EVP_des_cbc()             ) n = HB_EVP_CIPHER_DES_CBC;
   else if( method == EVP_des_ede_cbc()         ) n = HB_EVP_CIPHER_DES_EDE_CBC;
   else if( method == EVP_des_ede3_cbc()        ) n = HB_EVP_CIPHER_DES_EDE3_CBC;
   else if( method == EVP_desx_cbc()            ) n = HB_EVP_CIPHER_DESX_CBC;
#endif
#ifndef OPENSSL_NO_RC4
   else if( method == EVP_rc4()                 ) n = HB_EVP_CIPHER_RC4;
   else if( method == EVP_rc4_40()              ) n = HB_EVP_CIPHER_RC4_40;
#endif
#ifndef OPENSSL_NO_IDEA
   else if( method == EVP_idea_ecb()            ) n = HB_EVP_CIPHER_IDEA_ECB;
   else if( method == EVP_idea_cfb64()          ) n = HB_EVP_CIPHER_IDEA_CFB64;
   else if( method == EVP_idea_cfb()            ) n = HB_EVP_CIPHER_IDEA_CFB;
   else if( method == EVP_idea_ofb()            ) n = HB_EVP_CIPHER_IDEA_OFB;
   else if( method == EVP_idea_cbc()            ) n = HB_EVP_CIPHER_IDEA_CBC;
#endif
#ifndef OPENSSL_NO_RC2
   else if( method == EVP_rc2_ecb()             ) n = HB_EVP_CIPHER_RC2_ECB;
   else if( method == EVP_rc2_cbc()             ) n = HB_EVP_CIPHER_RC2_CBC;
   else if( method == EVP_rc2_40_cbc()          ) n = HB_EVP_CIPHER_RC2_40_CBC;
   else if( method == EVP_rc2_64_cbc()          ) n = HB_EVP_CIPHER_RC2_64_CBC;
   else if( method == EVP_rc2_cfb64()           ) n = HB_EVP_CIPHER_RC2_CFB64;
   else if( method == EVP_rc2_cfb()             ) n = HB_EVP_CIPHER_RC2_CFB;
   else if( method == EVP_rc2_ofb()             ) n = HB_EVP_CIPHER_RC2_OFB;
#endif
#ifndef OPENSSL_NO_BF
   else if( method == EVP_bf_ecb()              ) n = HB_EVP_CIPHER_BF_ECB;
   else if( method == EVP_bf_cbc()              ) n = HB_EVP_CIPHER_BF_CBC;
   else if( method == EVP_bf_cfb64()            ) n = HB_EVP_CIPHER_BF_CFB64;
   else if( method == EVP_bf_cfb()              ) n = HB_EVP_CIPHER_BF_CFB;
   else if( method == EVP_bf_ofb()              ) n = HB_EVP_CIPHER_BF_OFB;
#endif
#ifndef OPENSSL_NO_CAST
   else if( method == EVP_cast5_ecb()           ) n = HB_EVP_CIPHER_CAST5_ECB;
   else if( method == EVP_cast5_cbc()           ) n = HB_EVP_CIPHER_CAST5_CBC;
   else if( method == EVP_cast5_cfb64()         ) n = HB_EVP_CIPHER_CAST5_CFB64;
   else if( method == EVP_cast5_cfb()           ) n = HB_EVP_CIPHER_CAST5_CFB;
   else if( method == EVP_cast5_ofb()           ) n = HB_EVP_CIPHER_CAST5_OFB;
#endif
#ifndef OPENSSL_NO_RC5
   else if( method == EVP_rc5_32_12_16_cbc()    ) n = HB_EVP_CIPHER_RC5_32_12_16_CBC;
   else if( method == EVP_rc5_32_12_16_ecb()    ) n = HB_EVP_CIPHER_RC5_32_12_16_ECB;
   else if( method == EVP_rc5_32_12_16_cfb64()  ) n = HB_EVP_CIPHER_RC5_32_12_16_CFB64;
   else if( method == EVP_rc5_32_12_16_cfb()    ) n = HB_EVP_CIPHER_RC5_32_12_16_CFB;
   else if( method == EVP_rc5_32_12_16_ofb()    ) n = HB_EVP_CIPHER_RC5_32_12_16_OFB;
#endif
#ifndef OPENSSL_NO_AES
   else if( method == EVP_aes_128_ecb()         ) n = HB_EVP_CIPHER_AES_128_ECB;
   else if( method == EVP_aes_128_cbc()         ) n = HB_EVP_CIPHER_AES_128_CBC;
   else if( method == EVP_aes_128_cfb1()        ) n = HB_EVP_CIPHER_AES_128_CFB1;
   else if( method == EVP_aes_128_cfb8()        ) n = HB_EVP_CIPHER_AES_128_CFB8;
   else if( method == EVP_aes_128_cfb128()      ) n = HB_EVP_CIPHER_AES_128_CFB128;
   else if( method == EVP_aes_128_cfb()         ) n = HB_EVP_CIPHER_AES_128_CFB;
   else if( method == EVP_aes_128_ofb()         ) n = HB_EVP_CIPHER_AES_128_OFB;
   else if( method == EVP_aes_192_ecb()         ) n = HB_EVP_CIPHER_AES_192_ECB;
   else if( method == EVP_aes_192_cbc()         ) n = HB_EVP_CIPHER_AES_192_CBC;
   else if( method == EVP_aes_192_cfb1()        ) n = HB_EVP_CIPHER_AES_192_CFB1;
   else if( method == EVP_aes_192_cfb8()        ) n = HB_EVP_CIPHER_AES_192_CFB8;
   else if( method == EVP_aes_192_cfb128()      ) n = HB_EVP_CIPHER_AES_192_CFB128;
   else if( method == EVP_aes_192_cfb()         ) n = HB_EVP_CIPHER_AES_192_CFB;
   else if( method == EVP_aes_192_ofb()         ) n = HB_EVP_CIPHER_AES_192_OFB;
   else if( method == EVP_aes_256_ecb()         ) n = HB_EVP_CIPHER_AES_256_ECB;
   else if( method == EVP_aes_256_cbc()         ) n = HB_EVP_CIPHER_AES_256_CBC;
   else if( method == EVP_aes_256_cfb1()        ) n = HB_EVP_CIPHER_AES_256_CFB1;
   else if( method == EVP_aes_256_cfb8()        ) n = HB_EVP_CIPHER_AES_256_CFB8;
   else if( method == EVP_aes_256_cfb128()      ) n = HB_EVP_CIPHER_AES_256_CFB128;
   else if( method == EVP_aes_256_cfb()         ) n = HB_EVP_CIPHER_AES_256_CFB;
   else if( method == EVP_aes_256_ofb()         ) n = HB_EVP_CIPHER_AES_256_OFB;
#endif
#ifndef OPENSSL_NO_CAMELLIA
   else if( method == EVP_camellia_128_ecb()    ) n = HB_EVP_CIPHER_CAMELLIA_128_ECB;
   else if( method == EVP_camellia_128_cbc()    ) n = HB_EVP_CIPHER_CAMELLIA_128_CBC;
   else if( method == EVP_camellia_128_cfb1()   ) n = HB_EVP_CIPHER_CAMELLIA_128_CFB1;
   else if( method == EVP_camellia_128_cfb8()   ) n = HB_EVP_CIPHER_CAMELLIA_128_CFB8;
   else if( method == EVP_camellia_128_cfb128() ) n = HB_EVP_CIPHER_CAMELLIA_128_CFB128;
   else if( method == EVP_camellia_128_cfb()    ) n = HB_EVP_CIPHER_CAMELLIA_128_CFB;
   else if( method == EVP_camellia_128_ofb()    ) n = HB_EVP_CIPHER_CAMELLIA_128_OFB;
   else if( method == EVP_camellia_192_ecb()    ) n = HB_EVP_CIPHER_CAMELLIA_192_ECB;
   else if( method == EVP_camellia_192_cbc()    ) n = HB_EVP_CIPHER_CAMELLIA_192_CBC;
   else if( method == EVP_camellia_192_cfb1()   ) n = HB_EVP_CIPHER_CAMELLIA_192_CFB1;
   else if( method == EVP_camellia_192_cfb8()   ) n = HB_EVP_CIPHER_CAMELLIA_192_CFB8;
   else if( method == EVP_camellia_192_cfb128() ) n = HB_EVP_CIPHER_CAMELLIA_192_CFB128;
   else if( method == EVP_camellia_192_cfb()    ) n = HB_EVP_CIPHER_CAMELLIA_192_CFB;
   else if( method == EVP_camellia_192_ofb()    ) n = HB_EVP_CIPHER_CAMELLIA_192_OFB;
   else if( method == EVP_camellia_256_ecb()    ) n = HB_EVP_CIPHER_CAMELLIA_256_ECB;
   else if( method == EVP_camellia_256_cbc()    ) n = HB_EVP_CIPHER_CAMELLIA_256_CBC;
   else if( method == EVP_camellia_256_cfb1()   ) n = HB_EVP_CIPHER_CAMELLIA_256_CFB1;
   else if( method == EVP_camellia_256_cfb8()   ) n = HB_EVP_CIPHER_CAMELLIA_256_CFB8;
   else if( method == EVP_camellia_256_cfb128() ) n = HB_EVP_CIPHER_CAMELLIA_256_CFB128;
   else if( method == EVP_camellia_256_cfb()    ) n = HB_EVP_CIPHER_CAMELLIA_256_CFB;
   else if( method == EVP_camellia_256_ofb()    ) n = HB_EVP_CIPHER_CAMELLIA_256_OFB;
#endif
#ifndef OPENSSL_NO_SEED
   else if( method == EVP_seed_ecb()            ) n = HB_EVP_CIPHER_SEED_ECB;
   else if( method == EVP_seed_cbc()            ) n = HB_EVP_CIPHER_SEED_CBC;
   else if( method == EVP_seed_cfb128()         ) n = HB_EVP_CIPHER_SEED_CFB128;
   else if( method == EVP_seed_cfb()            ) n = HB_EVP_CIPHER_SEED_CFB;
   else if( method == EVP_seed_ofb()            ) n = HB_EVP_CIPHER_SEED_OFB;
#endif
   else                                           n = HB_EVP_CIPHER_UNSUPPORTED;

   return n;
}

HB_FUNC( EVP_GET_CIPHERBYNAME )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_EVP_CIPHER_ptr_to_id( EVP_get_cipherbyname( hb_parc( 1 ) ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_GET_CIPHERBYNID )
{
   if( HB_ISNUM( 1 ) )
      hb_retni( hb_EVP_CIPHER_ptr_to_id( EVP_get_cipherbynid( hb_parni( 1 ) ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_NID )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_nid( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_BLOCK_SIZE )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_block_size( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_KEY_LENGTH )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_key_length( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_KEY_IV_LENGTH )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_iv_length( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_KEY_FLAGS )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retnint( method ? EVP_CIPHER_flags( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_KEY_MODE )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_mode( method ) : 0 );
}

HB_FUNC( EVP_CIPHER_TYPE )
{
   const EVP_CIPHER * method = hb_EVP_CIPHER_par( 1 );

   hb_retni( method ? EVP_CIPHER_type( method ) : 0 );
}

HB_FUNC( HB_EVP_CIPHER_CTX_CREATE )
{
   void ** ph = ( void ** ) hb_gcAlloc( sizeof( EVP_CIPHER_CTX * ), EVP_CIPHER_CTX_release );

   EVP_CIPHER_CTX * ctx = ( EVP_CIPHER_CTX * ) hb_xgrab( sizeof( EVP_CIPHER_CTX ) );

   * ph = ( void * ) ctx;

   hb_retptrGC( ph );
}

HB_FUNC( EVP_CIPHER_CTX_INIT )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         EVP_CIPHER_CTX_init( ctx );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_CLEANUP )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CIPHER_CTX_cleanup( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_SET_PADDING )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CIPHER_CTX_set_padding( ctx, hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_KEY_LENGTH )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CIPHER_CTX_key_length( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_SET_KEY_LENGTH )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CIPHER_CTX_set_key_length( ctx, hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_CTRL )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         /* NOTE: 4th param doesn't have a 'const' qualifier. This is a setter
                  function, so even if we do a copy, what sort of allocation
                  routines to use? Probably an omission from OpenSSLs part. [vszakats] */
         hb_retni( EVP_CIPHER_CTX_ctrl( ctx, hb_parni( 2 ), hb_parni( 3 ), ( void * ) hb_parc( 4 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHER_CTX_CIPHER )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( hb_EVP_CIPHER_ptr_to_id( EVP_CIPHER_CTX_cipher( ctx ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_ENCRYPTINIT )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_EncryptInit( ctx,
                                    hb_EVP_CIPHER_par( 2 ),
                                    ( const unsigned char * ) hb_parc( 3 ),
                                    ( const unsigned char * ) hb_parc( 4 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_ENCRYPTINIT_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_EncryptInit_ex( ctx,
                                       hb_EVP_CIPHER_par( 2 ),
                                       ( ENGINE * ) hb_parptr( 3 ),
                                       ( const unsigned char * ) hb_parc( 4 ),
                                       ( const unsigned char * ) hb_parc( 5 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_ENCRYPTUPDATE )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( hb_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1 );
         int size = 0;

         hb_retni( EVP_EncryptUpdate( ctx,
                                      buffer,
                                      &size,
                                      ( const unsigned char * ) hb_parcx( 3 ),
                                      ( size_t ) hb_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_ENCRYPTFINAL )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_EncryptFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_ENCRYPTFINAL_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_EncryptFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DECRYPTINIT )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_DecryptInit( ctx,
                                    hb_EVP_CIPHER_par( 2 ),
                                    ( const unsigned char * ) hb_parc( 3 ),
                                    ( const unsigned char * ) hb_parc( 4 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DECRYPTINIT_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_DecryptInit_ex( ctx,
                                       hb_EVP_CIPHER_par( 2 ),
                                       ( ENGINE * ) hb_parptr( 3 ),
                                       ( const unsigned char * ) hb_parc( 4 ),
                                       ( const unsigned char * ) hb_parc( 5 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DECRYPTUPDATE )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( hb_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_DecryptUpdate( ctx,
                                      buffer,
                                      &size,
                                      ( const unsigned char * ) hb_parcx( 3 ),
                                      ( size_t ) hb_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DECRYPTFINAL )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_DecryptFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DECRYPTFINAL_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_DecryptFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHERINIT )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CipherInit( ctx,
                                    hb_EVP_CIPHER_par( 2 ),
                                    ( const unsigned char * ) hb_parc( 3 ),
                                    ( const unsigned char * ) hb_parc( 4 ),
                                    hb_parni( 5 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHERINIT_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) && hb_EVP_CIPHER_is( 2 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_CipherInit_ex( ctx,
                                      hb_EVP_CIPHER_par( 2 ),
                                      ( ENGINE * ) hb_parptr( 3 ),
                                      ( const unsigned char * ) hb_parc( 4 ),
                                      ( const unsigned char * ) hb_parc( 5 ),
                                      hb_parni( 6 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHERUPDATE )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( hb_parclen( 3 ) + EVP_CIPHER_CTX_block_size( ctx ) - 1 );
         int size = 0;

         hb_retni( EVP_CipherUpdate( ctx,
                                     buffer,
                                     &size,
                                     ( const unsigned char * ) hb_parcx( 3 ),
                                     ( size_t ) hb_parclen( 3 ) ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHERFINAL )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_CipherFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_CIPHERFINAL_EX )
{
   if( hb_EVP_CIPHER_CTX_is( 1 ) )
   {
      EVP_CIPHER_CTX * ctx = hb_EVP_CIPHER_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_CIPHER_CTX_block_size( ctx ) );
         int size = 0;

         hb_retni( EVP_CipherFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
               hb_xfree( buffer );
         }
         else
            hb_storc( NULL, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if 0

#define EVP_CIPHER_CTX_get_app_data(e) ((e)->app_data)
#define EVP_CIPHER_CTX_set_app_data(e,d) ((e)->app_data=(char *)(d))

int EVP_CIPHER_param_to_asn1(EVP_CIPHER_CTX *c, ASN1_TYPE *type);
int EVP_CIPHER_asn1_to_param(EVP_CIPHER_CTX *c, ASN1_TYPE *type);

int EVP_OpenInit(EVP_CIPHER_CTX *ctx,EVP_CIPHER *type,unsigned char *ek, int ekl,unsigned char *iv,EVP_PKEY *priv);
int EVP_OpenUpdate(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl, unsigned char *in, int inl);
int EVP_OpenFinal(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl);

int EVP_SealInit(EVP_CIPHER_CTX *ctx, const EVP_CIPHER *type, unsigned char **ek, int *ekl, unsigned char *iv, EVP_PKEY **pubk, int npubk);
int EVP_SealUpdate(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl, unsigned char *in, int inl);
int EVP_SealFinal(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl);

#endif
