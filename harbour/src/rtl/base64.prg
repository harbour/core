/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_BASE64DECODE() function
 *
 * Based on VB code by: 1999-2004 Antonin Foller, http://www.motobit.com, http://motobit.cz
 * Converted to Clipper and optimized by Viktor Szakats (harbour.01 syenar.hu)
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

FUNCTION HB_BASE64DECODE( cString )
   LOCAL cResult

   LOCAL nLen
   LOCAL nGroupPos
   LOCAL nGroup
   LOCAL nCharPos
   LOCAL nDataLen
   LOCAL nData

   /* remove white spaces, If any */
   cString := StrTran( cString, Chr( 10 ) )
   cString := StrTran( cString, Chr( 13 ) )
   cString := StrTran( cString, Chr( 9 ) )
   cString := StrTran( cString, " " )

   /* The source must consists from groups with Len of 4 chars */
   IF ( nLen := Len( cString ) ) % 4 != 0
      RETURN "" /* Bad Base64 string */
   ENDIF

#if 0
   IF nLen > Int( MAXSTRINGLENGTH / 1.34 ) /* Base64 is 1/3rd larger than source text. */
      RETURN "" /* Not enough memory to decode */
   ENDIF
#endif

   cResult := ""

   /* Now decode each group: */
   FOR nGroupPos := 1 TO nLen STEP 4

      /* Each data group encodes up To 3 actual bytes */
      nDataLen := 3
      nGroup := 0

      FOR nCharPos := 0 TO 3

         /* Convert each character into 6 bits of data, And add it To
            an integer For temporary storage. If a character is a '=', there
            is one fewer data byte. (There can only be a maximum of 2 '=' In
            the whole string.) */

         nData := At( SubStr( cString, nGroupPos + nCharPos, 1 ), "=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ) - 2

         DO CASE
         CASE nData >= 0
            /* Do nothing (for speed) */
         CASE nData == -1
            nData := 0
            nDataLen--
         CASE nData == -2
            RETURN "" /* Bad character In Base64 string */
         ENDCASE

         nGroup := 64 * nGroup + nData
      NEXT

      /* Convert the 24 bits to 3 characters
         and add nDataLen characters To out string */
      cResult += Left( Chr( nGroup / 65536 ) +;          /* bitwise AND 255, which is done by Chr() automatically */
                       Chr( nGroup /   256 ) +;          /* bitwise AND 255, which is done by Chr() automatically */
                       Chr( nGroup         ), nDataLen ) /* bitwise AND 255, which is done by Chr() automatically */
   NEXT

   RETURN cResult
