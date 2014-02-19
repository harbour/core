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

   LOCAL cLogFile := hb_cwd() + hb_FNameExtSet( __FILE__, ".txt" )

   ? hb_StrFormat( "Parent(%1$d) launching child...", posix_getpid() )

   IF unix_daemon( 0, 0 ) == -1
      ? hb_StrFormat( "failed with errno=%1$d", posix_errno() )
      ErrorLevel( 1 )
   ELSE
      IF hb_FileExists( cLogFile )
         FErase( cLogFile )
      ENDIF

      ? e"***\n* If you see this, something is b0rked\n***"

      hb_MemoWrit( cLogFile, hb_StrFormat( ;
         e"Hello, this is the daemon child(%1$d) writing.\n" + ;
         e"I am currenty residing in %2$s and\n" + ;
         e"am writing this message to %3$s\n" + ;
         e"Good bye now.", ;
         posix_getpid(), hb_cwd(), cLogFile ) + hb_eol() )
   ENDIF

   RETURN
