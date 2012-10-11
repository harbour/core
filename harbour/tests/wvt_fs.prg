/*
 * $Id$
 */

#include "hbgtinfo.ch"
#include "inkey.ch"

PROCEDURE Main()
   LOCAL cGt

#if defined( __PLATFORM__WINDOWS ) .AND. defined( __HBSCRIPT__HBSHELL )
   hbshell_gtSelect( "GTWVT" )
#endif

   cGt := HB_GtVersion( 0 )
   IF cGt == "WVT"
      HB_GtInfo( HB_GTI_FONTNAME, "Lucida Console" )
   ELSE
      ? "launch me under GTWVT"
      QUIT
   ENDIF

   ? "GTWVT test"

   ?  HB_GtInfo( HB_GTI_ISFULLSCREEN, .T. ), "we should be on full screen"
   WAIT
   ?  HB_GtInfo( HB_GTI_ISFULLSCREEN, .F. ), "we should be windowed"
   ?  HB_GtInfo( HB_GTI_ALTENTER, .T. ), "Alt+Enter is now enabled, try it"
   DO WHILE Inkey( 0 ) != K_ESC
      ? Row()
   ENDDO

   RETURN
