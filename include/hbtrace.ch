/*
 * Harbour Project source code:
 * Header file with trace constants and PP rules
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_TRACE_CH_
#define HB_TRACE_CH_

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

#ifdef _HB_TR_NOALWAYS_
#undef HB_TR_ALWAYS
#define HB_TR_ALWAYS     HB_TR_LAST
#endif

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

#xtranslate HB_TRACE_STEALTH( <l>, <x,...> ) => HB_TRACE( <l>, <x> )
#xtranslate HB_TRACE( <l>, ( <x,...> ) )     => HB_TRACE( <l>, <x> )
#xtranslate HB_TRACE( <l>, <x,...> )     => ;
            iif( HB_TR_LEVEL >= <l>, hb_traceLogAt( <l>, <x> ), )

#endif /* HB_TRACE_CH_ */
