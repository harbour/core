/*
 * $Id$
 */

#include "hbdefs.h"
#include "hbsetup.h"

#if   defined(HARBOUR_LANGUAGE_EU)
#include "source/rtl/natmsg/msgeu.c"
#elif defined(HARBOUR_LANGUAGE_NL)
#include "source/rtl/natmsg/msgnl.c"
#elif defined(HARBOUR_LANGUAGE_EO)
#include "source/rtl/natmsg/msgeo.c"
#elif defined(HARBOUR_LANGUAGE_GL)
#include "source/rtl/natmsg/msggl.c"
#elif defined(HARBOUR_LANGUAGE_DE)
#include "source/rtl/natmsg/msgde.c"
#elif defined(HARBOUR_LANGUAGE_ID)
#include "source/rtl/natmsg/msgid.c"
#elif defined(HARBOUR_LANGUAGE_HU852)
#include "source/rtl/natmsg/msghu852.c"
#elif defined(HARBOUR_LANGUAGE_HUCWI)
#include "source/rtl/natmsg/msghucwi.c"
#elif defined(HARBOUR_LANGUAGE_HUWIN)
#include "source/rtl/natmsg/msghuwin.c"
#elif defined(HARBOUR_LANGUAGE_KO)
#include "source/rtl/natmsg/msgko.c"
#elif defined(HARBOUR_LANGUAGE_PT)
#include "source/rtl/natmsg/msgpt.c"
#elif defined(HARBOUR_LANGUAGE_RUWIN)
#include "source/rtl/natmsg/msgruwin.c"
#elif defined(HARBOUR_LANGUAGE_RU886)
#include "source/rtl/natmsg/msgru886.c"
#elif defined(HARBOUR_LANGUAGE_ES)
#include "source/rtl/natmsg/msges.c"
#elif defined(HARBOUR_LANGUAGE_CA)
#include "source/rtl/natmsg/msgca.c"
#elif defined(HARBOUR_LANGUAGE_IT)
#include "source/rtl/natmsg/msgit.c"
#elif defined(HARBOUR_LANGUAGE_FR)
#include "source/rtl/natmsg/msgfr.c"
#elif defined(HARBOUR_LANGUAGE_CS852)
#include "source/rtl/natmsg/msgcs852.c"
#elif defined(HARBOUR_LANGUAGE_CSKAM)
#include "source/rtl/natmsg/msgcskam.c"
#elif defined(HARBOUR_LANGUAGE_SR852)
#include "source/rtl/natmsg/msgsr852.c"
#elif defined(HARBOUR_LANGUAGE_PL852)
#include "source/rtl/natmsg/msgpl852.c"
#elif defined(HARBOUR_LANGUAGE_PLMAZ)
#include "source/rtl/natmsg/msgplmaz.c"
#else
#include "source/rtl/natmsg/msgen.c"
#endif

char *hb_errorNatDescription( ULONG ulGenError )
{
   if( ulGenError < sizeof(hb_errorsGeneric) / sizeof(hb_errorsGeneric[ 0 ]) )
      return hb_errorsGeneric[ ulGenError ];
   else
      return hb_errorsGeneric[ 0 ];
}
