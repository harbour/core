/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for trace macros and functions.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_TRACE_H_
#define HB_TRACE_H_

#include "hbsetup.h"

HB_EXTERN_BEGIN

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

#define HB_TR_FM         10

/*
 * Default tracing level.
 */
#define HB_TR_DEFAULT   HB_TR_WARNING

/*
 * If we compiled without specifying a -DHB_TR_LEVEL, use the value
 * for HB_TR_DEFAULT.
 */

#ifdef HB_TR_LEVEL_ALWAYS
#define HB_TR_LEVEL     HB_TR_ALWAYS
#endif
#ifdef HB_TR_LEVEL_FATAL
#define HB_TR_LEVEL     HB_TR_FATAL
#endif
#ifdef HB_TR_LEVEL_ERROR
#define HB_TR_LEVEL     HB_TR_ERROR
#endif
#ifdef HB_TR_LEVEL_WARNING
#define HB_TR_LEVEL     HB_TR_WARNING
#endif
#ifdef HB_TR_LEVEL_INFO
#define HB_TR_LEVEL     HB_TR_INFO
#endif
#ifdef HB_TR_LEVEL_DEBUG
#define HB_TR_LEVEL     HB_TR_DEBUG
#endif

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
                                      hb_traceset( l, __FILE__, __LINE__, NULL ); \
                                      hb_tr_trace x ; \
                                   } \
                                } while( 0 )

#if HB_TR_LEVEL >= HB_TR_DEBUG
#define HB_ECHO_TRACE_HB_TR_DEBUG(x)    HB_ECHO_CREATE(HB_TR_DEBUG, x)
#else
#define HB_ECHO_TRACE_HB_TR_DEBUG(x)    do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_INFO
#define HB_ECHO_TRACE_HB_TR_INFO(x)     HB_ECHO_CREATE(HB_TR_INFO, x)
#else
#define HB_ECHO_TRACE_HB_TR_INFO(x)     do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_WARNING
#define HB_ECHO_TRACE_HB_TR_WARNING(x)  HB_ECHO_CREATE(HB_TR_WARNING, x)
#else
#define HB_ECHO_TRACE_HB_TR_WARNING(x)  do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_ERROR
#define HB_ECHO_TRACE_HB_TR_ERROR(x)    HB_ECHO_CREATE(HB_TR_ERROR, x)
#else
#define HB_ECHO_TRACE_HB_TR_ERROR(x)    do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_FATAL
#define HB_ECHO_TRACE_HB_TR_FATAL(x)    HB_ECHO_CREATE(HB_TR_FATAL, x)
#else
#define HB_ECHO_TRACE_HB_TR_FATAL(x)    do {} while( 0 )
#endif

#if 1  /* always! */
#define HB_ECHO_TRACE_HB_TR_ALWAYS(x)   HB_ECHO_CREATE(HB_TR_ALWAYS, x)
#else
#define HB_ECHO_TRACE_HB_TR_ALWAYS(x)   do {} while( 0 )
#endif


#define HB_TRACE(l, x)                HB_ECHO_TRACE_##l(x)

/* NOTE: This will print tracing info without changing current
 * filename/linenum information - this is usefull if we want to
 * trace the source of unreleased memory blocks
 */
#define HB_ECHO_STEALTH( l, x ) do \
                                { \
                                   if( hb_tr_level() >= l ) \
                                      hb_tr_stealth x ; \
                                } while( 0 )

#if HB_TR_LEVEL >= HB_TR_DEBUG
#define HB_ECHO_STEALTH_HB_TR_DEBUG(x)    HB_ECHO_STEALTH(HB_TR_DEBUG, x)
#else
#define HB_ECHO_STEALTH_HB_TR_DEBUG(x)    do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_INFO
#define HB_ECHO_STEALTH_HB_TR_INFO(x)     HB_ECHO_STEALTH(HB_TR_INFO, x)
#else
#define HB_ECHO_STEALTH_HB_TR_INFO(x)     do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_WARNING
#define HB_ECHO_STEALTH_HB_TR_WARNING(x)  HB_ECHO_STEALTH(HB_TR_WARNING, x)
#else
#define HB_ECHO_STEALTH_HB_TR_WARNING(x)  do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_ERROR
#define HB_ECHO_STEALTH_HB_TR_ERROR(x)    HB_ECHO_STEALTH(HB_TR_ERROR, x)
#else
#define HB_ECHO_STEALTH_HB_TR_ERROR(x)    do {} while( 0 )
#endif

#if HB_TR_LEVEL >= HB_TR_FATAL
#define HB_ECHO_STEALTH_HB_TR_FATAL(x)    HB_ECHO_STEALTH(HB_TR_FATAL, x)
#else
#define HB_ECHO_STEALTH_HB_TR_FATAL(x)    do {} while( 0 )
#endif

#if 1  /* always! */
#define HB_ECHO_STEALTH_HB_TR_ALWAYS(x)   HB_ECHO_STEALTH(HB_TR_ALWAYS, x)
#else
#define HB_ECHO_STEALTH_HB_TR_ALWAYS(x)   do {} while( 0 )
#endif

/* NOTE: This will print tracing info without changing current
 * filename/linenum information
 */
#define HB_TRACE_STEALTH(l, x)            HB_ECHO_STEALTH_##l(x)

typedef struct
{
   const char * file;
   const char * proc;
   int          line;
   int          level;
}
HB_TRACEINFO, * PHB_TRACEINFO;

extern HB_EXPORT int           hb_tracestate( int new_state );
extern HB_EXPORT int           hb_tracelevel( int new_level );
extern HB_EXPORT int           hb_traceflush( int new_flush );
extern HB_EXPORT int           hb_tracesysout( int new_sysout );
extern HB_EXPORT const char *  hb_tracemode( const char * szNewMode );
extern HB_EXPORT HB_BOOL       hb_tracefile( const char * szFile );

extern HB_EXPORT void          hb_tracelog( int level, const char * file, int line, const char * proc, const char * fmt, ... ) HB_PRINTF_FORMAT( 5, 6 );

extern HB_EXPORT void          hb_traceset( int level, const char * file, int line, const char * proc );
extern HB_EXPORT PHB_TRACEINFO hb_traceinfo( void );

extern HB_EXPORT int           hb_tr_level( void );
extern HB_EXPORT void          hb_tr_trace( const char * fmt, ... ) HB_PRINTF_FORMAT( 1, 2 );
extern HB_EXPORT void          hb_tr_stealth( const char * fmt, ... ) HB_PRINTF_FORMAT( 1, 2 );

HB_EXTERN_END

#endif /* HB_TRACE_H_ */
