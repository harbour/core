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

#define MAX_STREAM                               2048 /* Max length of in-line LITERAL */
#define MAX_STREAM_STARTER                          2
#define MAX_STREAM_TERMINATOR                       2
#define MAX_STREAM_EXCLUSIONS                       2

#define TOKEN_SIZE             HB_SYMBOL_NAME_LEN + 1

/* NOTE: 02/08/2000 - maurilio.longo@libero.it, under OS/2 GCC I need to use relative paths in include command */
/* this is relative to position of simplex.c in harbour source tree */
#define SLX_RULES "../macro/macro.slx"

/* this is relative to position of macroslx.c in harbour source tree */
#include "../compiler/simplex.c"
