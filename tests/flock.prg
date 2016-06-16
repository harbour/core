/* file locking test */

#include "fileio.ch"
#include "inkey.ch"

#define hb_keyCode( x )  Asc( x )

STATIC s_lLocked      := .F.
STATIC s_nExclusivity := HB_FLX_EXCLUSIVE
STATIC s_nBlocking    := HB_FLX_NO_WAIT

PROCEDURE Main()

   LOCAL hLockFile
   LOCAL cLockFile := hb_FNameExtSet( __FILE__, ".lck" )

   CLS

   IF ! hb_vfExists( cLockFile )
      hb_MemoWrit( cLockFile, "" )
   ENDIF

   ? "Opening lock file"
   IF ( hLockFile := hb_vfOpen( cLockFile, FO_READWRITE ) ) == NIL
      ? "ERROR: Cannot open Lock File"
      RETURN
   ENDIF
   ? "Lock file opened"
   ?
   ShowStatus()
   ? "[+] to get a lock, [-] to release it, [Esc] to exit, [E] for exclusive, [S] for shared, [B] for blocking, [N] for non-blocking"
   DO WHILE .T.
      SWITCH hb_keyStd( Inkey( 0 ) )
      CASE hb_keyCode( "+" )
         IF s_lLocked
            ? "Already locked"
         ELSE
            ? "Requesting Lock"
            IF hb_vfLock( hLockFile, 0, 1, hb_bitOr( s_nExclusivity, s_nBlocking ) )
               ? "Lock has been obtained"
               s_lLocked := .T.
            ELSE
               ? "Lock Request Failed - Error Code:", FError()
            ENDIF
         ENDIF
         EXIT
      CASE hb_keyCode( "-" )
         IF s_lLocked
            IF hb_vfUnlock( hLockFile, 0, 1 )
               ? "Lock has been released"
               s_lLocked := .F.
            ELSE
               ? "Unlock Request Failed - Error Code:", FError()
            ENDIF
         ELSE
            ? "Lock not currently held"
         ENDIF
         EXIT
      CASE hb_keyCode( "E" )
      CASE hb_keyCode( "e" )
         IF s_lLocked
            ? "Release Lock before changing lock type"
         ELSE
            s_nExclusivity := HB_FLX_EXCLUSIVE
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "S" )
      CASE hb_keyCode( "s" )
         IF s_lLocked
            ? "Release Lock before changing lock type"
         ELSE
            s_nExclusivity := HB_FLX_SHARED
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "B" )
      CASE hb_keyCode( "b" )
         IF s_lLocked
            ? "Release Lock before changing function mode"
         ELSE
            s_nBlocking := HB_FLX_WAIT
            ShowStatus()
         ENDIF
         EXIT
      CASE hb_keyCode( "N" )
      CASE hb_keyCode( "n" )
         IF s_lLocked
            ? "Release Lock before changing function mode"
         ELSE
            s_nBlocking := HB_FLX_NO_WAIT
            ShowStatus()
         ENDIF
         EXIT
      CASE K_ESC
         ?
         hb_vfClose( hLockFile )
         ? "Exiting"
         RETURN
      OTHERWISE
         ? "Key not supported", hb_keyLast()
      ENDSWITCH
   ENDDO

   RETURN

STATIC PROCEDURE ShowStatus()

   ? ;
      "Lock:", iif( s_lLocked, "Held", "Released" ), " ", ;
      "Type:", iif( s_nExclusivity == HB_FLX_EXCLUSIVE, "Exclusive", "Shared" ), " ", ;
      "Request is:", iif( s_nBlocking == HB_FLX_NO_WAIT, "Non-Blocking", "Blocking" )

   RETURN
