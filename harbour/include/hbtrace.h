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

#if defined(__cplusplus)
//extern "C" {
#endif

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
 * What we do here is to generate calls to HB_ECHO_CREATE only for those
 * levels that are less or equal to the COMPILATION time HB_TR_LEVEL.
 */

#define HB_ECHO_CREATE( l, x )  do \
                                { \
                                   if( hb_tr_level() >= l ) \
                                   { \
                                      hb_tr_file_ = __FILE__; \
                                      hb_tr_line_ = __LINE__; \
                                      hb_tr_level_ = l; \
                                      hb_tr_trace x ; \
                                   } \
                                } while( 0 )

#if HB_TR_LEVEL >= HB_TR_DEBUG
#define HB_ECHO_TRACE_HB_TR_DEBUG(x)    HB_ECHO_CREATE(HB_TR_DEBUG, x)
#else
#define HB_ECHO_TRACE_HB_TR_DEBUG(x)
#endif

#if HB_TR_LEVEL >= HB_TR_INFO
#define HB_ECHO_TRACE_HB_TR_INFO(x)     HB_ECHO_CREATE(HB_TR_INFO, x)
#else
#define HB_ECHO_TRACE_HB_TR_INFO(x)
#endif

#if HB_TR_LEVEL >= HB_TR_WARNING
#define HB_ECHO_TRACE_HB_TR_WARNING(x)  HB_ECHO_CREATE(HB_TR_WARNING, x)
#else
#define HB_ECHO_TRACE_HB_TR_WARNING(x)
#endif

#if HB_TR_LEVEL >= HB_TR_ERROR
#define HB_ECHO_TRACE_HB_TR_ERROR(x)    HB_ECHO_CREATE(HB_TR_ERROR, x)
#else
#define HB_ECHO_TRACE_HB_TR_ERROR(x)
#endif

#if HB_TR_LEVEL >= HB_TR_FATAL
#define HB_ECHO_TRACE_HB_TR_FATAL(x)    HB_ECHO_CREATE(HB_TR_FATAL, x)
#else
#define HB_ECHO_TRACE_HB_TR_FATAL(x)
#endif

#if 1  /* always! */
#define HB_ECHO_TRACE_HB_TR_ALWAYS(x)   HB_ECHO_CREATE(HB_TR_ALWAYS, x)
#else
#define HB_ECHO_TRACE_HB_TR_ALWAYS(x)
#endif


#define HB_TRACE(l, x)                HB_ECHO_TRACE_##l(x)

/* NOTE: This will print tracing info without changing current
 * filename/linenum information - this is usefull if we want to
 * trace the source of unreleased memory blocks
 */
#define HB_ECHO_STEALTH( l, x ) do \
                                { \
                                   if( hb_tr_level() >= l ) \
                                   { \
                                      hb_tr_level_ = l; \
                                      hb_tr_trace x ; \
                                   } \
                                } while( 0 )

#if HB_TR_LEVEL >= HB_TR_DEBUG
#define HB_ECHO_STEALTH_HB_TR_DEBUG(x)    HB_ECHO_STEALTH(HB_TR_DEBUG, x)
#else
#define HB_ECHO_STEALTH_HB_TR_DEBUG(x)
#endif

#if HB_TR_LEVEL >= HB_TR_INFO
#define HB_ECHO_STEALTH_HB_TR_INFO(x)     HB_ECHO_STEALTH(HB_TR_INFO, x)
#else
#define HB_ECHO_STEALTH_HB_TR_INFO(x)
#endif

#if HB_TR_LEVEL >= HB_TR_WARNING
#define HB_ECHO_STEALTH_HB_TR_WARNING(x)  HB_ECHO_STEALTH(HB_TR_WARNING, x)
#else
#define HB_ECHO_STEALTH_HB_TR_WARNING(x)
#endif

#if HB_TR_LEVEL >= HB_TR_ERROR
#define HB_ECHO_STEALTH_HB_TR_ERROR(x)    HB_ECHO_STEALTH(HB_TR_ERROR, x)
#else
#define HB_ECHO_STEALTH_HB_TR_ERROR(x)
#endif

#if HB_TR_LEVEL >= HB_TR_FATAL
#define HB_ECHO_STEALTH_HB_TR_FATAL(x)    HB_ECHO_STEALTH(HB_TR_FATAL, x)
#else
#define HB_ECHO_STEALTH_HB_TR_FATAL(x)
#endif

#if 1  /* always! */
#define HB_ECHO_STEALTH_HB_TR_ALWAYS(x)   HB_ECHO_STEALTH(HB_TR_ALWAYS, x)
#else
#define HB_ECHO_STEALTH_HB_TR_ALWAYS(x)
#endif

/* NOTE: This will print tracing info without changing current
 * filename/linenum information
 */
#define HB_TRACE_STEALTH(l, x)            HB_ECHO_STEALTH_##l(x)

extern char * hb_tr_file_;
extern int    hb_tr_line_;
extern int    hb_tr_level_;

extern int    hb_tracestate( int new_state );
extern int    hb_tracelevel( int new_level );

extern int    hb_tr_level( void );
extern void   hb_tr_trace( char * fmt, ... );

#if defined(__cplusplus)
//}
#endif

#endif /* HB_TRACE_H_ */
