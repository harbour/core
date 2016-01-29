/*
 * Linux specific functions
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
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

#include "fileio.ch"

/* https://wiki.freedesktop.org/www/Specifications/trash-spec/ */
FUNCTION linux_MoveToTrash( cFileName )

   THREAD STATIC t_cTrashDirInfo
   THREAD STATIC t_cTrashDirFiles
   THREAD STATIC t_lInit := .F.

   LOCAL aPath
   LOCAL cTrashDir
   LOCAL cInfo
   LOCAL cFile
   LOCAL tmp
   LOCAL n
   LOCAL tDate

   IF ! HB_ISSTRING( cFileName )
      RETURN -1
   ENDIF

   IF ! t_lInit

      aPath := {}

      IF ! HB_ISNULL( tmp := GetEnv( "XDG_DATA_HOME" ) )
         AAdd( aPath, hb_DirSepAdd( tmp ) + "Trash" )
      ENDIF

      AAdd( aPath, hb_DirSepAdd( GetEnv( "HOME" ) ) + hb_DirSepToOS( ".local/share/Trash" ) )
      AAdd( aPath, hb_DirSepAdd( GetEnv( "HOME" ) ) + ".trash" )

      FOR EACH tmp IN aPath
         IF hb_vfDirExists( tmp )
            cTrashDir := tmp
            EXIT
         ENDIF
      NEXT

      IF cTrashDir == NIL
         RETURN -2
      ENDIF

      t_cTrashDirInfo  := hb_DirSepAdd( cTrashDir ) + "info"
      t_cTrashDirFiles := hb_DirSepAdd( cTrashDir ) + "files"

      IF ! hb_vfDirExists( t_cTrashDirInfo ) .AND. ;
         ! hb_vfDirExists( t_cTrashDirFiles )
         /* TODO: create dirs if missing */
         RETURN -3
      ENDIF

      t_lInit := .T.
   ENDIF

   cFileName := hb_PathJoin( hb_DirBase(), cFileName )

   IF ! hb_vfExists( cFileName )
      RETURN -4
   ENDIF

   n := 0
   DO WHILE .T.

      tmp := hb_FNameName( cFileName ) + iif( n == 0, "", "." + hb_ntos( n ) ) + hb_FNameExt( cFileName )

      cInfo := hb_DirSepAdd( t_cTrashDirInfo ) + tmp + ".trashinfo"
      cFile := hb_DirSepAdd( t_cTrashDirFiles ) + tmp

      IF ! hb_vfExists( cInfo ) .AND. ;
         ! hb_vfExists( cFile )
         EXIT
      ENDIF

      n++
   ENDDO

   IF hb_vfRename( cFileName, cFile ) == F_ERROR
      RETURN -5
   ENDIF

   IF hb_MemoWrit( cInfo, hb_StrToUTF8( ;
         e"[Trash Info]\n" + ;
         e"Path=" + cFileName + e"\n" + ;
         e"DeletionDate=" + ;
            hb_DToC( tDate := hb_DateTime(), "yyyy-mm-dd" ) + ;
            hb_TToC( tDate, "", "Thh:mm:ss.fffZ" ) + e"\n" ) )
      RETURN 0
   ENDIF

   RETURN -6
