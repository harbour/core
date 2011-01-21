/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Tamas TEVESZ <ice@extreme.hu>
 * www - http://harbour-project.org
 *
 */

REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main()

   LOCAL cLogFile, cLogText

   cLogFile := hb_ps() + CurDir() + hb_ps() + "testdmn.txt"

   OutStd( hb_strFormat( "Parent(%d) launching child... ", posix_getpid() ) + hb_eol() )

   IF ! hb_posix_daemon( .F., .F. )
      OutStd( "failed." + hb_eol() )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF File( cLogFile )
      FErase( cLogFile )
   ENDIF

   OutStd( "***" + hb_eol() + "* If you see this, something is b0rked" + hb_eol() + "***" + hb_eol() )

   cLogText := hb_strFormat( "Hello, this is the daemon child(%d) writing.", posix_getpid() ) + hb_eol()
   cLogText += hb_strFormat( "I am currenty residing in %s and ", hb_ps() + CurDir() ) + hb_eol()
   cLogText += hb_strFormat( "am writing this message to %s", cLogFile ) + hb_eol()
   cLogText += "Good bye now." + hb_eol()

   hb_MemoWrit( cLogFile, cLogText )

   RETURN
