/*
 * $Id$
 */
#include "hbdefs.h"
#include "hbsetup.h"

#if defined(HARBOUR_LANGUAGE_BAS)
#include "natmsg/msgbas.c"
#elif defined(HARBOUR_LANGUAGE_DUT)
#include "natmsg/msgdut.c"
#elif defined(HARBOUR_LANGUAGE_EO)
#include "natmsg/msgeo.c"
#elif defined(HARBOUR_LANGUAGE_GAL)
#include "natmsg/msggal.c"
#elif defined(HARBOUR_LANGUAGE_GER)
#include "natmsg/msgger.c"
#elif defined(HARBOUR_LANGUAGE_INA)
#include "natmsg/msgia.c"
#elif defined(HARBOUR_LANGUAGE_HU)
#include "natmsg/msghu.c"
#elif defined(HARBOUR_LANGUAGE_KOR)
#include "natmsg/msgkor.c"
#elif defined(HARBOUR_LANGUAGE_POR)
#include "natmsg/msgpor.c"
#elif defined(HARBOUR_LANGUAGE_R1251)
#include "natmsg/msgr1251.c"
#elif defined(HARBOUR_LANGUAGE_RU886)
#include "natmsg/msgru886.c"
#elif defined(HARBOUR_LANGUAGE_SPA)
#include "natmsg/msgspa.c"
#elif defined(HARBOUR_LANGUAGE_CAT)
#include "natmsg/msgcat.c"
#elif defined(HARBOUR_LANGUAGE_ITA)
#include "natmsg/msgita.c"
#elif defined(HARBOUR_LANGUAGE_FRE)
#include "natmsg/msgfre.c"
#elif defined(HARBOUR_LANGUAGE_CZ852)
#include "natmsg/msgcz852.c"
#elif defined(HARBOUR_LANGUAGE_CZKAM)
#include "natmsg/msgczkam.c"
#elif defined(HARBOUR_LANGUAGE_YU852)
#include "natmsg/msgyu852.c"
#elif defined(HARBOUR_LANGUAGE_PL852)
#include "natmsg/msgpl852.c"
#elif defined(HARBOUR_LANGUAGE_PLMAZ)
#include "natmsg/msgplmaz.c"
#else
#include "natmsg/msguk.c"
#endif
