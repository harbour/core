/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MACRO Compiler SimpLex rules main (host) file.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
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
