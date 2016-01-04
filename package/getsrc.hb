#!/usr/bin/env hbmk2

/* Copyright 2016 Viktor Szakats (vszakats.net/harbour) */

#include "hbver.ch"

PROCEDURE Main()

   LOCAL cURL := StrTran( hb_Version( HB_VERSION_URL_SOURCE ), "commit", "archive" ) + ".zip"
   LOCAL cDirBase := hb_FNameDir( hbshell_ScriptName() )
   LOCAL cOldDir := hb_cwd( cDirBase )
   LOCAL cFileName

   IF hb_vfDirExists( hb_DirSepToOS( "../src/rtl" ) ) .AND. .F.
      OutStd( "! Error: This installation has the sources downloaded already." + hb_eol() )
      Inkey( 0 )
      ErrorLevel( 1 )
   ELSE
      cFileName := "src.zip"

      OutStd( hb_StrFormat( "! Downloading Harbour source revision %1$s to '%2$s'...", ;
         hb_Version( HB_VERSION_ID ), cFileName ) + hb_eol() )

      hb_processRun( "curl" + ;
         " -fsS" + ;
         " -o " + cFileName + ;
         " -L --proto-redir =https" + ;
         " " + cURL )
   ENDIF

   hb_cwd( cOldDir )

   RETURN
