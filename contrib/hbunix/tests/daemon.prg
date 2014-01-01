/*
 * Harbour Project source code:
 *
 * Copyright 2011 Tamas TEVESZ <ice@extreme.hu>
 * www - http://harbour-project.org
 *
 */

#require "hbunix"

REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main()

   LOCAL cLogFile, cLogText

   cLogFile := hb_ps() + CurDir() + hb_ps() + hb_FNameExtSet( __FILE__, ".txt" )

   ? hb_StrFormat( "Parent(%1$d) launching child... ", posix_getpid() )

   IF unix_daemon( 0, 0 ) == -1
      ? hb_StrFormat( "failed with errno=%1$d", posix_errno() )
      ErrorLevel( 1 )
   ELSE
      IF hb_FileExists( cLogFile )
         FErase( cLogFile )
      ENDIF

      ? "***" + hb_eol() + "* If you see this, something is b0rked" + hb_eol() + "***"

      cLogText := hb_StrFormat( "Hello, this is the daemon child(%1$d) writing.", posix_getpid() ) + hb_eol()
      cLogText += hb_StrFormat( "I am currenty residing in %1$s and ", hb_ps() + CurDir() ) + hb_eol()
      cLogText += hb_StrFormat( "am writing this message to %1$s", cLogFile ) + hb_eol()
      cLogText += "Good bye now." + hb_eol()

      hb_MemoWrit( cLogFile, cLogText )
   ENDIF

   RETURN
