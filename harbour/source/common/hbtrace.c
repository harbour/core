/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Tracing functions.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hbtrace.h"

char * hb_tr_file_ = "";
int    hb_tr_line_ = 0;
int    hb_tr_level_ = 0;

static int hb_tr_state_ = 1;
static FILE* hb_tr_fp_ = 0;
static char* slevel[HB_TR_LAST] =
{
  "HB_TR_ALWAYS",
  "HB_TR_FATAL",
  "HB_TR_ERROR",
  "HB_TR_WARNING",
  "HB_TR_INFO",
  "HB_TR_DEBUG"
};


void hb_traceon( void )
{
  hb_tr_state_ = 1;
}

void hb_traceoff( void )
{
  hb_tr_state_ = 0;
}

int hb_tracelevel( int new_level )
{
  int old_level = hb_tr_level_;

  if (new_level >= HB_TR_ALWAYS &&
      new_level <  HB_TR_LAST) {
    hb_tr_level_ = new_level;
  }

  return old_level;
}

int hb_tr_level(void)
{
  static int level = -1;
  int i;
  char* env;
  char* out;

  if (level != -1) {
    return level;
  }

  hb_tr_fp_ = stderr;
  out = getenv("HB_TR_OUTPUT");
  if (out != 0 && out[0] != '\0') {
    hb_tr_fp_ = fopen(out, "w");
    if (hb_tr_fp_ == NULL) {
      hb_tr_fp_ = stderr;
    }
  }

  level = HB_TR_DEFAULT;
  env = getenv("HB_TR_LEVEL");
  if (env != 0 && env[0] != '\0') {
    for (i = 0; i < HB_TR_LAST; ++i) {
      if (strcmp(env, slevel[i]) == 0) {
        level = i;
        break;
      }
    }
  }

  return level;
}

void hb_tr_trace( char * fmt, ... )
{
  int i;
  va_list ap;

  /*
   * If tracing is disabled, do nothing.
   */
  if( ! hb_tr_state_ ) {
    return;
  }

  /*
   * Clean up the file, so that instead of showing
   *
   *   ../../../foo/bar/baz.c
   *
   * we just show
   *
   *   foo/bar/baz.c
   */
  for (i = 0; hb_tr_file_[i] != '\0'; ++i) {
    if (hb_tr_file_[i] != '.' &&
        hb_tr_file_[i] != '/' &&
        hb_tr_file_[i] != '\\')
      break;
  }

  /*
   * Print file and line.
   */
  fprintf(hb_tr_fp_, "%s:%d: %s ",
          hb_tr_file_ + i, hb_tr_line_, slevel[hb_tr_level_]);

  /*
   * Print the name and arguments for the function.
   */
  va_start(ap, fmt);
  vfprintf(hb_tr_fp_, fmt, ap);
  va_end(ap);

  /*
   * Print a new-line.
   */
  fprintf(hb_tr_fp_, "\n");

  /*
   * Reset file and line.
   */
  hb_tr_file_ = "";
  hb_tr_line_ = -1;
  hb_tr_level_ = -1;
}
