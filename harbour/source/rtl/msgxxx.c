/*
 * $Id$
 */

#include "hbdefs.h"
#include "hbsetup.h"

#if   defined(HARBOUR_LANGUAGE_EU)
#include "natmsg/msgeu.c"
#elif defined(HARBOUR_LANGUAGE_NL)
#include "natmsg/msgnl.c"
#elif defined(HARBOUR_LANGUAGE_EO)
#include "natmsg/msgeo.c"
#elif defined(HARBOUR_LANGUAGE_GL)
#include "natmsg/msggl.c"
#elif defined(HARBOUR_LANGUAGE_DE)
#include "natmsg/msgde.c"
#elif defined(HARBOUR_LANGUAGE_ID)
#include "natmsg/msgid.c"
#elif defined(HARBOUR_LANGUAGE_HU852)
#include "natmsg/msghu852.c"
#elif defined(HARBOUR_LANGUAGE_HUCWI)
#include "natmsg/msghucwi.c"
#elif defined(HARBOUR_LANGUAGE_HUWIN)
#include "natmsg/msghuwin.c"
#elif defined(HARBOUR_LANGUAGE_KO)
#include "natmsg/msgko.c"
#elif defined(HARBOUR_LANGUAGE_PT)
#include "natmsg/msgpt.c"
#elif defined(HARBOUR_LANGUAGE_RUWIN)
#include "natmsg/msgruwin.c"
#elif defined(HARBOUR_LANGUAGE_RU886)
#include "natmsg/msgru886.c"
#elif defined(HARBOUR_LANGUAGE_ES)
#include "natmsg/msges.c"
#elif defined(HARBOUR_LANGUAGE_CA)
#include "natmsg/msgca.c"
#elif defined(HARBOUR_LANGUAGE_IT)
#include "natmsg/msgit.c"
#elif defined(HARBOUR_LANGUAGE_FR)
#include "natmsg/msgfr.c"
#elif defined(HARBOUR_LANGUAGE_CS852)
#include "natmsg/msgcs852.c"
#elif defined(HARBOUR_LANGUAGE_CSKAM)
#include "natmsg/msgcskam.c"
#elif defined(HARBOUR_LANGUAGE_SR852)
#include "natmsg/msgsr852.c"
#elif defined(HARBOUR_LANGUAGE_PL852)
#include "natmsg/msgpl852.c"
#elif defined(HARBOUR_LANGUAGE_PLMAZ)
#include "natmsg/msgplmaz.c"
#else
#ifdef __TURBOC__
   #include "source\rtl\natmsg\msgen.c"
#else
   #include "natmsg/msgen.c"
#endif
#endif

char *hb_errorNatDescription( ULONG ulGenError )
{
   if( ulGenError < sizeof(hb_errorsGeneric) / sizeof(hb_errorsGeneric[ 0 ]) )
      return hb_errorsGeneric[ ulGenError ];
   else
      return hb_errorsGeneric[ 0 ];
}
