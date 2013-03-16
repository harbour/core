/* test file locking */

#include "fileio.ch"
#include "inkey.ch"

#define FLX_EXCLUSIVE 0x0000   /* Exclusive lock */
#define FLX_SHARED    0x0100   /* Shared lock */
#define FLX_NO_WAIT   0x0000   /* Immediate return */
#define FLX_WAIT      0x0200   /* Wait for lock until success */

#define hb_keyCode( x ) Asc( x )

STATIC s_lLocked
STATIC s_lExclusive
STATIC s_lBlocking

PROCEDURE Main()

   LOCAL hLockFile
   LOCAL lSuccess
   LOCAL nExclusivity
   LOCAL nBlocking
   LOCAL nKeyHit
   LOCAL nLockType

   CLS

   IF ! hb_FileExists( "emphasis.6lo" )
      hb_MemoWrit( "emphasis.6lo", "" )
   ENDIF

   ? "Opening lock file"
   IF ( hLockFile := FOpen( "emphasis.6lo", FO_READWRITE ) ) == F_ERROR
      ? "ERROR: Cannot open Lock File"
      RETURN
   ENDIF
   ? "Lock file opened - handle is", hb_ntos( hLockFile )
   ?
   s_lLocked      := .F.
   s_lExclusive   := .T.
   s_lBlocking    := .F.
   nExclusivity := FLX_EXCLUSIVE
   nBlocking    := FLX_NO_WAIT
   ShowStatus()
   ? "[+] to get a lock, [-] to release it, [Esc] to exit, [E] for exclusive, [S] for shared, [B] for blocking, [N] for non-blocking"
   DO WHILE .T.
      nKeyHit := Inkey( 0 )
      SWITCH nKeyHit
      CASE hb_keyCode( "+" )
         IF s_lLocked
            ? "Already locked"
         ELSE
            nLockType := nExclusivity + nBlocking
            ? "Requesting Lock"
            lSuccess := hb_FLock( hLockFile, 0, 1, nLockType  )
            IF lSuccess
               ? "Lock has been obtained"
               s_lLocked := .T.
            ELSE
               ? "Lock Request Failed - Error Code:", FError()
            ENDIF
         ENDIF
         EXIT
      CASE hb_keyCode( "-" )
         IF ! s_lLocked
            ? "Lock not currently held"
         ELSE
            lSuccess := hb_FUnlock( hLockFile, 0, 1 )
            IF lSuccess
               ? "Lock has been released"
               s_lLocked := .F.
            ELSE
               ? "Unlock Request Failed - Error Code:", FError()
            ENDIF
         ENDIF
         EXIT
      CASE hb_keyCode( "E" )
      CASE hb_keyCode( "e" )
         IF s_lLocked
            ? "Release Lock before changing lock type"
         ELSE
            s_lExclusive := .T.
            nExclusivity := FLX_EXCLUSIVE
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "S" )
      CASE hb_keyCode( "s" )
         IF s_lLocked
            ? "Release Lock before changing lock type"
         ELSE
            s_lExclusive := .F.
            nExclusivity := FLX_SHARED
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "B" )
      CASE hb_keyCode( "b" )
         IF s_lLocked
            ? "Release Lock before changing function mode"
         ELSE
            s_lBlocking := .T.
            nExclusivity := FLX_WAIT
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "N" )
      CASE hb_keyCode( "n" )
         IF s_lLocked
            ? "Release Lock before changing function mode"
         ELSE
            s_lBlocking := .F.
            nExclusivity := FLX_NO_WAIT
            ShowStatus()
         ENDIF
         EXIT
      CASE K_ESC
         ?
         FClose( hLockFile )
         ? "Exiting"
         RETURN
      OTHERWISE
         ? "Key not supported", nKeyHit
      ENDSWITCH
   ENDDO

   RETURN

PROCEDURE ShowStatus()

   ? "Lock: " + iif( s_lLocked, "Held", "Released" ) +;
     "    Type: " + iif( s_lExclusive, "Exclusive", "Shared" ) +;
     "  Request is: " + iif( s_lBlocking, "Blocking", "Non-Blocking" )

   RETURN
