/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Localization module includer for GNU compilers
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

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
#elif defined(HARBOUR_LANGUAGE_HE862)
   #include "natmsg/msghe862.c"
#elif defined(HARBOUR_LANGUAGE_HEWIN)
   #include "natmsg/msghewin.c"
#elif defined(HARBOUR_LANGUAGE_ZHGB)
   #include "natmsg/msgzhgb.c"
#elif defined(HARBOUR_LANGUAGE_ZHBIG)
   #include "natmsg/msgzhbig.c"
#else
   #include "natmsg/msgen.c"
#endif
