/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for trace macros and functions.
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

#ifndef HB_TRACE_H_
#define HB_TRACE_H_

#if defined(HB_DO_TRACE)

extern char* hb_tr_file_;
extern int   hb_tr_line_;

void hb_tr_trace(char* fmt, ...);

#define HB_TRACE(x) \
do { \
  hb_tr_file_ = __FILE__; \
  hb_tr_line_ = __LINE__; \
  hb_tr_trace x ; \
} while (0)

#else

#define HB_TRACE(x)

#endif  /* #if defined(HB_DO_TRACE) */

#endif /* HB_TRACE_H_ */
