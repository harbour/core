/*
 * $Id$
 */

#include "hbcomp.h"
#include "harboury.h"
#include "hbsetup.h"
#include "hberrors.h"
#include "hbdefs.h"

#define MAX_STREAM                               2048 /* Max length of in-line LITERAL */
#define MAX_STREAM_STARTER                          7 /* "QOUT([" */
#define MAX_STREAM_TERMINATOR                       4 /* "])\n"   */
#define MAX_STREAM_EXCLUSIONS                       2

#define TOKEN_SIZE             HB_SYMBOL_NAME_LEN + 1

#define SLX_RULES "harbour.slx"

#include "simplex.c"
