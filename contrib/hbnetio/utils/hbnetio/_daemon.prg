/*
 * Harbour NETIO server daemon support
 *
 * Copyright 2015 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their website at https://www.gnu.org/).
 *
 */

PROCEDURE DaemonMain( ... )

   LOCAL nID, tmp

   IF Lower( hb_defaultValue( hb_PValue( 1 ), "" ) ) == "-d"

      IF ! Empty( tmp := CmdLineSuffix( "-gid:" ) ) .AND. ;
         ( ( nID := Val( tmp ) ) != 0 .OR. ( nID := posix_getgrnam( tmp ) ) != 0 )
         posix_setgid( nID )
      ENDIF
      IF ! Empty( tmp := CmdLineSuffix( "-uid:" ) ) .AND. ;
         ( ( nID := Val( tmp ) ) != 0 .OR. ( nID := posix_getpwnam( tmp ) ) != 0 )
         posix_setuid( nID )
      ENDIF
      IF ! Empty( tmp := CmdLineSuffix( "-egid:" ) ) .AND. ;
         ( ( nID := Val( tmp ) ) != 0 .OR. ( nID := posix_getgrnam( tmp ) ) != 0 )
         posix_setegid( nID )
      ENDIF
      IF ! Empty( tmp := CmdLineSuffix( "-euid:" ) ) .AND. ;
         ( ( nID := Val( tmp ) ) != 0 .OR. ( nID := posix_getpwnam( tmp ) ) != 0 )
         posix_seteuid( nID )
      ENDIF

      IF unix_daemon( 0, 0 ) == -1
         OutStd( hb_StrFormat( "Daemon failed to launch with errno=%1$d", posix_errno() ) + hb_eol() )
         ErrorLevel( 1 )
      ELSE
         netiosrv_Main( .F., ... )  /* Non-interactive */
      ENDIF
   ELSE
      netiosrv_Main( .T., ... )  /* Interactive */
   ENDIF

   RETURN

STATIC FUNCTION CmdLineSuffix( cName )

   LOCAL tmp

   FOR EACH tmp IN hb_ACmdLine()
      IF hb_LeftEqI( tmp, cName )
         RETURN SubStr( tmp, Len( cName ) + 1 )
      ENDIF
   NEXT

   RETURN ""
