/*
 * Linux OEM charset smoke — reproduces the scenario from issue #354.
 *
 * Opens a DBF/CDX table in OEM mode, stores a national-character key,
 * rebuilds the index, and verifies DbSeek round-trip without ACE
 * converting through the wrong ANSI/OEM tables.
 *
 * On non-Unix hosts prints SKIP and exits 0 (bug is Linux-specific).
 */

#require "rddads"

REQUEST ADS
REQUEST ADSCDX

#if defined( __HBDYNLOAD__RDDADS__ )
#include "rddads.hbx"
#endif

#define SMOKE_BASE  "rddoem"

STATIC s_nFail := 0

PROCEDURE Main()

#if defined( __HBDYNLOAD__RDDADS__ )
   LOCAL lLib := hb_libLoad( hb_libName( "rddads" + hb_libPostfix() ) )
   hb_rddADSRegister()
   HB_SYMBOL_UNUSED( lLib )
#elif defined( __HBSCRIPT__HBSHELL )
   hb_rddADSRegister()
#endif

#if ! defined( HB_OS_UNIX )
   ? "SKIP: oem_linux_smoke — requires HB_OS_UNIX (see issue #354)"
   ErrorLevel( 0 )
   RETURN
#endif

   RunOemRoundTrip()

   IF s_nFail == 0
      ? "RESULT: OK"
      ErrorLevel( 0 )
   ELSE
      ? "RESULT: FAIL count =", s_nFail
      ErrorLevel( 1 )
   ENDIF

   RETURN

STATIC PROCEDURE RunOemRoundTrip()

   LOCAL cDir := hb_FNameMerge( hb_FTempDir(), SMOKE_BASE )
   LOCAL cKeyOem
   LOCAL cRead
   LOCAL aStru := { { "NAME", "C", 20, 0 } }

   cKeyOem := OemSampleName()

   hb_DirCreate( cDir )
   hb_FChangeDir( cDir )
   FErase( SMOKE_BASE + ".dbf" )
   FErase( SMOKE_BASE + ".cdx" )

   RddSetDefault( "ADSCDX" )

   AdsSetCharType( 2, .T. )

   DbCreate( SMOKE_BASE, aStru, "DBFCDX" )
   USE ( SMOKE_BASE ) EXCLUSIVE NEW

   DbAppend()
   Replace NAME WITH cKeyOem
   DbCommit()

   INDEX ON NAME TAG tg_name TO ( SMOKE_BASE )
   OrdSetFocus( "tg_name" )

   AssertEqual( "field after append", cKeyOem, FieldGet( "NAME" ) )

   IF ! DbSeek( cKeyOem, .F., .F. )
      Report( "seek on OEM key", .F., { Found(), RecNo() } )
   ELSE
      cRead := FieldGet( "NAME" )
      AssertEqual( "field after seek", cKeyOem, cRead )
      Report( "seek FOUND", .T., NIL )
   ENDIF

   DbCloseArea()
   hb_FChangeDir( hb_FTempDir() )
   hb_DirRemove( cDir )

   RETURN

STATIC FUNCTION OemSampleName()

   LOCAL c := ""

   /* CP852 OEM spelling similar to reporter sample (issue #354). */
   c += Chr( 0x9D )  /* Ł */
   c += Chr( 0xA5 )  /* Ą */
   c += "CZNIK"
   c += Chr( 0xE0 )  /* Ó */

   RETURN PadR( c, 20 )

STATIC PROCEDURE AssertEqual( cTag, cExp, cGot )

   LOCAL lOk := ( cExp == cGot )

   IF ! lOk
      Report( cTag, .F., { "exp len", Len( cExp ), "got len", Len( cGot ) } )
   ELSE
      Report( cTag, .T., NIL )
   ENDIF

   RETURN

STATIC PROCEDURE Report( cTag, lOk, xDetail )

   IF lOk
      ? "PASS:", cTag
   ELSE
      ? "FAIL:", cTag, xDetail
      ++s_nFail
   ENDIF

   RETURN