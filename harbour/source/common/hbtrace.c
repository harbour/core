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

#if defined(HB_DO_TRACE)

#include <stdio.h>
#include <stdarg.h>
#include "hbtrace.h"

char * hb_tr_file_ = "";
int    hb_tr_line_ = 0;

void hb_tr_trace( char * fmt, ... )
{
  char file[256];
  int i, j;
  va_list ap;

  /*
   * Clean up the file, so that instead of showing
   *
   *   ../../../foo/bar/baz.c
   *
   * we just show
   *
   *   baz.c
   */
  for (i = 0; hb_tr_file_[i] != '\0'; ++i) {
    if (hb_tr_file_[i] != '.' &&
        hb_tr_file_[i] != '/' &&
        hb_tr_file_[i] != '\\')
      break;
  }
  for (j = 0; hb_tr_file_[i] != '\0'; ++i, ++j) {
    file[j] = hb_tr_file_[i];
  }
  file[j] = '\0';

  /*
   * Print file and line.
   */
  fprintf(stderr, "%s:%d: ",
	  file, hb_tr_line_);

  /*
   * Print the name and arguments for the function.
   */
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  /*
   * Print a new-line.
   */
  fprintf(stderr, "\n");

  /*
   * Reset file and line.
   */
  hb_tr_file_ = "";
  hb_tr_line_ = 0;
}

#endif /* #if defined(HB_DO_TRACE) */

