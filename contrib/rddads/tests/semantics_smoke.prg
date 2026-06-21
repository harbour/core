/*
 * contrib/rddads semantics smoke test
 *
 * Exercises Clipper-compatible workarea flags (Limbo, soft-seek miss)
 * against a local Advantage connection.  Mirrors the opening blocks of
 * tests/rddtest/rddtst.prg without pulling the full harness.
 *
 * Exit code 0 = all PASS, 1 = at least one FAIL (for scripted CI).
 */

#require "rddads"

REQUEST ADS
REQUEST ADSCDX

#if defined( __HBDYNLOAD__RDDADS__ )
#include "rddads.hbx"
#endif

#define SMOKE_BASE  "rddsem"

STATIC s_nFail := 0

PROCEDURE Main()

   LOCAL cDir := hb_FNameMerge( hb_FTempDir(), SMOKE_BASE )

#if defined( __HBDYNLOAD__RDDADS__ )
   LOCAL lLib := hb_libLoad( hb_libName( "rddads" + hb_libPostfix() ) )
   hb_rddADSRegister()
   HB_SYMBOL_UNUSED( lLib )
#elif defined( __HBSCRIPT__HBSHELL )
   hb_rddADSRegister()
#endif

   hb_DirCreate( cDir )
   hb_FChangeDir( cDir )
   FErase( SMOKE_BASE + ".dbf" )
   FErase( SMOKE_BASE + ".cdx" )

   RddSetDefault( "ADSCDX" )
   Set( _SET_DELETED, .F. )
   Set( _SET_SOFTSEEK, .T. )

   ? "rddads semantics smoke — dir:", hb_FCurDir()
   ?

   TestEmptyLimbo()
   TestSoftSeekMiss()

   ?
   IF s_nFail == 0
      ? "RESULT: OK (all PASS)"
   ELSE
      ? "RESULT: FAIL count =", s_nFail
   ENDIF

   hb_FChangeDir( hb_FTempDir() )
   hb_DirRemove( cDir )

   ErrorLevel( iif( s_nFail == 0, 0, 1 ) )

   RETURN

STATIC PROCEDURE TestEmptyLimbo()

   /* Same sequence as rddtst.prg on an empty shared table (no index). */
   LOCAL aStru := { { "FNUM", "N", 10, 0 }, { "FSTR", "C", 4, 0 } }

   DbCreate( SMOKE_BASE, aStru, "DBFCDX" )
   USE ( SMOKE_BASE ) SHARED NEW

   AssertState( "empty GOTOP",           1, .T., .T., NIL )
   AssertState( "empty GOBOTTOM",        1, .T., .T., NIL )
   AssertState( "empty SKIP(0)",         1, .T., .T., NIL )
   AssertState( "empty GOTO(0)",         1, .T., .T., NIL )
   AssertState( "empty SKIP(+1)",        1, .F., .T., NIL )
   AssertState( "empty SKIP(-1)",        1, .T., .F., NIL )
   AssertState( "empty SKIP(0) after",   1, .T., .F., NIL )

   DbCloseArea()

   RETURN

STATIC PROCEDURE TestSoftSeekMiss()

   LOCAL n
   LOCAL aStru := { { "FNUM", "N", 10, 0 }, { "FSTR", "C", 4, 0 } }

   FErase( SMOKE_BASE + ".dbf" )
   FErase( SMOKE_BASE + ".cdx" )

   DbCreate( SMOKE_BASE, aStru, "DBFCDX" )
   USE ( SMOKE_BASE ) SHARED NEW

   FOR n := 1 TO 15
      DbAppend()
      Replace FNUM WITH Int( ( n + 2 ) / 3 )
      Replace FSTR WITH Chr( FNUM + 48 )
   NEXT
   DbCommit()

   INDEX ON FSTR TAG tg_c TO ( SMOKE_BASE )
   OrdSetFocus( "tg_c" )

   /* Soft seek past every key — must land at EOF, not on last record. */
   AssertSeek( "soft seek miss 6", "6", .T., .F., 16, .F., .T. )
   AssertSeek( "soft seek hit 5",  "5", .T., .T.,  13, .F., .F. )

   /* Empty key soft seek with data — Clipper FOUND .T. on first key. */
   AssertSeek( "soft seek empty",  "",  .T., .T.,   1, .F., .F. )

   INDEX ON FSTR TAG tg_d TO ( SMOKE_BASE ) DESCENDING
   OrdSetFocus( "tg_d" )

   /* Descending: soft seek past keys stays at EOF (recno LastRec()+1). */
   AssertSeek( "desc soft miss",   "6", .T., .F.,  16, .F., .T. )

   DbCloseArea()

   RETURN

STATIC PROCEDURE AssertState( cTag, nRec, lBof, lEof, lFound )

   LOCAL lOk := .T.

   IF RecNo() != nRec
      lOk := .F.
   ENDIF
   IF Bof() != lBof
      lOk := .F.
   ENDIF
   IF Eof() != lEof
      lOk := .F.
   ENDIF
   IF lFound != NIL .AND. Found() != lFound
      lOk := .F.
   ENDIF

   Report( cTag, lOk, { RecNo(), Bof(), Eof(), iif( lFound == NIL, "n/a", Found() ) } )

   RETURN

STATIC PROCEDURE AssertSeek( cTag, xKey, lSoft, lFound, nRec, lBof, lEof )

   LOCAL lOk := DbSeek( xKey, lSoft, iif( lSoft, .T., .F. ) )

   IF lOk != lFound
      Report( cTag + " FOUND", .F., { lOk, lFound } )
      RETURN
   ENDIF

   AssertState( cTag, nRec, lBof, lEof, lFound )

   RETURN

STATIC PROCEDURE Report( cTag, lOk, xDetail )

   IF lOk
      ? "PASS:", cTag
   ELSE
      ? "FAIL:", cTag, "got", xDetail
      ++s_nFail
   ENDIF

   RETURN