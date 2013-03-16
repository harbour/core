
#include "hbgtinfo.ch"
#include "inkey.ch"

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL )
   #if defined( __PLATFORM__WINDOWS )
      hbshell_gtSelect( "GTWVT" )
   #elif defined( __PLATFORM__UNIX )
      hbshell_gtSelect( "GTXWC" )
   #endif
#endif

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      hb_gtInfo( HB_GTI_FONTNAME, "Lucida Console" )
   ELSE
      ? "launch me under a graphical GT"
      QUIT
   ENDIF

   ? "GT" + hb_gtVersion() + " test"

   ?  hb_gtInfo( HB_GTI_ISFULLSCREEN, .T. ), "we should be on full screen"
   WAIT
   ?  hb_gtInfo( HB_GTI_ISFULLSCREEN, .F. ), "we should be windowed"
   ?  hb_gtInfo( HB_GTI_ALTENTER, .T. ), "<Alt+Enter> is now enabled, try it"
   DO WHILE Inkey( 0 ) != K_ESC
      ? Row()
   ENDDO

   RETURN
