/*
 * $Id$
 */

#if !defined(HB_LANGUAGE)
#define HB_LANGUAGE UK
#endif

#if (HB_LANGUAGE == DUT)
#include "natmsg/msgdut.c"
#elif (HB_LANGUAGE == GAL)
#include "natmsg/msggal.c"
#elif (HB_LANGUAGE == GER)
#include "natmsg/msgger.c"
#elif (HB_LANGUAGE == HU)
#include "natmsg/msghu.c"
#elif (HB_LANGUAGE == POR)
#include "natmsg/msgpor.c"
#elif (HB_LANGUAGE == R1251)
#include "natmsg/msgr1251.c"
#elif (HB_LANGUAGE == RU886)
#include "natmsg/msgru886.c"
#elif (HB_LANGUAGE == SPA)
#include "natmsg/msgspa.c"
#elif (HB_LANGUAGE == UK)
#include "natmsg/msguk.c"
#elif (HB_LANGUAGE == CAT)
#include "natmsg/msgcat.c"
#elif (HB_LANGUAGE == ITA)
#include "natmsg/msgita.c"
#elif (HB_LANGUAGE == FRE)
#include "natmsg/msgfre.c"
#endif
