/*
 * High-level VF FILE API based functions.
 *
 * Copyright 2009-2017 Viktor Szakats (vsz.me/hb)
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

FUNCTION hb_vfDirBuild( cDir )

   LOCAL cDirTemp
   LOCAL cDirItem
   LOCAL tmp

   IF ! HB_ISSTRING( cDir )
      RETURN .F.
   ENDIF

   cDir := hb_PathNormalize( cDir )

   IF ! hb_vfDirExists( cDir )

      cDir := hb_DirSepAdd( cDir )

      IF ! hb_osDriveSeparator() == "" .AND. ;
         ( tmp := At( hb_osDriveSeparator(), cDir ) ) > 0
         cDirTemp := Left( cDir, tmp )
         cDir := SubStr( cDir, tmp + 1 )
#ifdef __PLATFORM__WINDOWS
      ELSEIF hb_LeftEq( cDir, "\\" ) /* UNC Path, network share */
         cDirTemp := Left( cDir, hb_At( "\", cDir, 3 ) )
         cDir := SubStr( cDir, Len( cDirTemp ) + 1 )
#endif
      ELSEIF hb_LeftEq( cDir, hb_ps() )
         cDirTemp := Left( cDir, 1 )
         cDir := SubStr( cDir, 2 )
      ELSE
         cDirTemp := ""
      ENDIF

      FOR EACH cDirItem IN hb_ATokens( cDir, hb_ps() )
         IF ! Right( cDirTemp, 1 ) == hb_ps() .AND. ! cDirTemp == ""
            cDirTemp += hb_ps()
         ENDIF
         IF ! cDirItem == ""  /* Skip root path, if any */
            cDirTemp += cDirItem
            IF hb_vfExists( cDirTemp )
               RETURN .F.
            ELSEIF ! hb_vfDirExists( cDirTemp )
               IF hb_vfDirMake( cDirTemp ) != 0
                  RETURN .F.
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN .T.

FUNCTION hb_vfDirUnbuild( cDir )

   LOCAL tmp

   IF ! HB_ISSTRING( cDir )
      RETURN .F.
   ENDIF

   IF hb_vfDirExists( cDir )

      cDir := hb_DirSepDel( cDir )

      DO WHILE ! cDir == ""
         IF hb_vfDirExists( cDir ) .AND. ;
            hb_vfDirRemove( cDir ) != 0
            RETURN .F.
         ENDIF
         IF ( tmp := RAt( hb_ps(), cDir ) ) == 0  /* FIXME: use hb_URAt() function */
            EXIT
         ENDIF
         cDir := Left( cDir, tmp - 1 )
         IF ! hb_osDriveSeparator() == "" .AND. ;
            Right( cDir, Len( hb_osDriveSeparator() ) ) == hb_osDriveSeparator()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN .T.

FUNCTION hb_vfNameExists( cName )
   RETURN ;
      hb_vfExists( cName ) .OR. ;
      hb_vfDirExists( cName )
