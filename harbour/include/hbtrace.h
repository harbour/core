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

/*
 * Tracing levels.
 */
#define HB_TR_ALWAYS     0
#define HB_TR_FATAL      1
#define HB_TR_ERROR      2
#define HB_TR_WARNING    3
#define HB_TR_INFO       4
#define HB_TR_DEBUG      5
#define HB_TR_LAST       6

/*
 * Default tracing level.
 */
#define HB_TR_DEFAULT   HB_TR_WARNING

/*
 * If we compiled without specifying a -DHB_TR_LEVEL, use the value
 * for HB_TR_DEFAULT.
 */
#ifndef HB_TR_LEVEL
#define HB_TR_LEVEL     HB_TR_DEFAULT
#endif


/*
 * This is black magic...
 * What we do here is to generate calls to ECHO_CREATE only for those
 * levels that are less or equal to the COMPILATION time HB_TR_LEVEL.
 */

#define ECHO_CREATE(l, x)     do \
                              { \
                                if (hb_tr_level() >= l) { \
                                  hb_tr_file_ = __FILE__; \
                                  hb_tr_line_ = __LINE__; \
                                  hb_tr_level_ = l; \
                                  hb_tr_trace x ; \
                                } \
                              } while (0)

#if HB_TR_LEVEL >= HB_TR_DEBUG
#define _ECHO_TRACE_HB_TR_DEBUG(x)    ECHO_CREATE(HB_TR_DEBUG, x)
#else
#define _ECHO_TRACE_HB_TR_DEBUG(x)    do {} while (0)
#endif

#if HB_TR_LEVEL >= HB_TR_INFO
#define _ECHO_TRACE_HB_TR_INFO(x)     ECHO_CREATE(HB_TR_INFO, x)
#else
#define _ECHO_TRACE_HB_TR_INFO(x)     do {} while (0)
#endif

#if HB_TR_LEVEL >= HB_TR_WARNING
#define _ECHO_TRACE_HB_TR_WARNING(x)  ECHO_CREATE(HB_TR_WARNING, x)
#else
#define _ECHO_TRACE_HB_TR_WARNING(x)  do {} while (0)
#endif

#if HB_TR_LEVEL >= HB_TR_ERROR
#define _ECHO_TRACE_HB_TR_ERROR(x)    ECHO_CREATE(HB_TR_ERROR, x)
#else
#define _ECHO_TRACE_HB_TR_ERROR(x)    do {} while (0)
#endif

#if HB_TR_LEVEL >= HB_TR_FATAL
#define _ECHO_TRACE_HB_TR_FATAL(x)    ECHO_CREATE(HB_TR_FATAL, x)
#else
#define _ECHO_TRACE_HB_TR_FATAL(x)    do {} while (0)
#endif

#if 1  /* always! */
#define _ECHO_TRACE_HB_TR_ALWAYS(x)   ECHO_CREATE(HB_TR_ALWAYS, x)
#else
#define _ECHO_TRACE_HB_TR_ALWAYS(x)   do {} while (0)
#endif

     
#define  HB_TRACE(l, x)               _ECHO_TRACE_##l(x)



extern char * hb_tr_file_;
extern int    hb_tr_line_;
extern int    hb_tr_level_;

extern void   hb_tr_trace( char* fmt, ... );
extern int    hb_tr_level(void);

#endif /* HB_TRACE_H_ */
