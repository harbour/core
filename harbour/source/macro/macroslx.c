/*
 * $Id$
 */


#define HB_MACRO_SUPPORT

#define yylex hb_complex

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "hbmacro.h"
#include "hbcomp.h"

#include "macroy.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "hbdefs.h"

/* NOTE: 02/08/2000 - maurilio.longo@libero.it, under OS/2 GCC I need to use relative paths in include command */
/* this is relative to position of simplex.c in harbour source tree */
#define SLX_RULES "../macro/macro.slx"

/* this is relative to position of macroslx.c in harbour source tree */
#include "../compiler/simplex.c"
