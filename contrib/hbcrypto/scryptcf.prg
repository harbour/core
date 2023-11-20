/*
 * hb_scrypt_setup()
 *
 * Harbour adaptation 2015 Viktor Szakats
 * based on original C code by Colin Percival in scryptenc.c
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* Converts from high-level scrypt parameters to low-level ones.
   Some calls (e.g. scryptenc_setup()) use high-level ones,
   hb_scrypt()/crypto_scrypt() expect the low-level ones. */

PROCEDURE hb_scrypt_setup( nOpsLimit, nMemLimit, /* @ */ N, /* @ */ r, /* @ */ p )

   LOCAL maxN
   LOCAL maxrp

   r := 8

   IF HB_ISNUMERIC( nOpsLimit ) .AND. ;
      HB_ISNUMERIC( nMemLimit ) .AND. ;
      nMemLimit >= 0

      IF nOpsLimit < 32768
         nOpsLimit := 32768
      ENDIF

      IF nOpsLimit < ( nMemLimit / 32 )
         p := 1
         maxN := nOpsLimit / ( r * 4 )
         FOR N := 1 TO 63
            IF hb_bitShift( 1, N ) > ( maxN / 2 )
               EXIT
            ENDIF
         NEXT
      ELSE
         maxN := nMemLimit / ( r * 128 )
         FOR N := 1 TO 63
            IF hb_bitShift( 1, N ) > ( maxN / 2 )
               EXIT
            ENDIF
         NEXT
         maxrp := ( nOpsLimit / 4 ) / hb_bitShift( 1, N )
         IF maxrp > 0x3FFFFFFF
            maxrp := 0X3FFFFFFF
         ENDIF
         p := hb_bitAnd( maxrp / r, 0xFFFFFFFF )
      ENDIF

      N := hb_bitShift( 1, N )
   ELSE
      p := 1
      N := 1024
   ENDIF

   RETURN
