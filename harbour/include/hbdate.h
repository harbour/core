/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Date API
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.  If not, write to
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

#ifndef HB_DATE_H_
#define HB_DATE_H_

#include "hbdefs.h"

HB_EXTERN_BEGIN

extern HB_EXPORT double hb_secondsCPU( int n );

extern HB_EXPORT double hb_dateSeconds( void );
/* return UTC julian timestamp in milliseconds */
extern HB_EXPORT HB_MAXUINT hb_dateMilliSeconds( void );

/* functions to operate on julian date values */

extern HB_EXPORT void   hb_dateTimeStr( char * pszTime );
extern HB_EXPORT void   hb_dateToday( int * piYear, int * piMonth, int * piDay );

extern HB_EXPORT long   hb_dateEncode( int iYear, int iMonth, int iDay );
extern HB_EXPORT void   hb_dateDecode( long julian, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT void   hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay );
extern HB_EXPORT void   hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT char * hb_dateDecStr( char * szDate, long lJulian );
extern HB_EXPORT long   hb_dateEncStr( const char * szDate );
extern HB_EXPORT int    hb_dateDOW( int iYear, int iMonth, int iDay );
extern HB_EXPORT int    hb_dateJulianDOW( long lJulian );

/* RTL functions */
extern HB_EXPORT const char * hb_dateCMonth( int iMonth );
extern HB_EXPORT const char * hb_dateCDOW( int iDay );
extern HB_EXPORT char *       hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern HB_EXPORT long         hb_dateUnformat( const char * szDate, const char * szDateFormat );

/* functions to operate on time values */

extern HB_EXPORT long   hb_timeEncode( int iHour, int iMinutes, int iSeconds, int iMSec );
extern HB_EXPORT void   hb_timeDecode( long lMilliSec, int * piHour, int * piMinutes,
                                       int * piSeconds, int * piMSec );
extern HB_EXPORT char * hb_timeStr( char * szTime, long lMilliSec );
extern HB_EXPORT HB_BOOL hb_timeStrGet( const char * szTime,
                                        int * piHour, int * piMinutes,
                                        int * piSeconds, int * piMSec );

extern HB_EXPORT void   hb_timeStrRawGet( const char * szTime,
                                          int * piHour, int * piMinutes,
                                          int * piSeconds, int * piMSec );

/* functions to operate on date and time values */

extern HB_EXPORT void hb_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes,
                                            int * piSeconds, int * piMSec );
extern HB_EXPORT void   hb_timeStampGet( long * plJulian, long * plMilliSec );

extern HB_EXPORT long   hb_timeUTCOffset( void ); /* in seconds */

extern HB_EXPORT char * hb_timeStampStrRawPut( char * szDateTime, long lJulian, long lMilliSec );
extern HB_EXPORT void   hb_timeStampStrRawGet( const char * szDateTime, long * plJulian, long * plMilliSec );

extern HB_EXPORT char * hb_timeStampStr( char * szDateTime, long lJulian, long lMilliSec );
extern HB_EXPORT HB_BOOL hb_timeStampStrGet( const char * szDateTime,
                                            int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes, int * piSeconds,
                                            int * piMSec );
extern HB_EXPORT HB_BOOL hb_timeStampStrGetDT( const char * szDateTime,
                                              long * plJulian, long * plMilliSec );

extern HB_EXPORT double hb_timeStampPackDT( long lJulian, long lMilliSec );
extern HB_EXPORT void   hb_timeStampUnpackDT( double dTimeStamp,
                                              long * plJulian, long * plMilliSec );
extern HB_EXPORT double hb_timeStampPack( int iYear, int iMonth, int iDay,
                                          int iHour, int iMinutes, int iSeconds, int iMSec );
extern HB_EXPORT void   hb_timeStampUnpack( double dTimeStamp,
                                            int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes, int * piSeconds,
                                            int * piMSec );
extern HB_EXPORT double hb_timeStampPackD( int iYear, int iMonth, int iDay,
                                           int iHour, int iMinutes, double dSeconds );
extern HB_EXPORT void   hb_timeStampUnpackD( double dTimeStamp,
                                             int * piYear, int * piMonth, int * piDay,
                                             int * piHour, int * piMinutes, double * pdSeconds );

/* RTL functions */
extern HB_EXPORT char * hb_timeFormat( char * szBuffer, const char * szTimeFormat, long lMilliSec );
extern HB_EXPORT char * hb_timeStampFormat( char * szBuffer,
                                            const char * szDateFormat, const char * szTimeFormat,
                                            long lJulian, long lMilliSec );

extern HB_EXPORT long   hb_timeUnformat( const char * szTime, const char * szTimeFormat );
extern HB_EXPORT void   hb_timeStampUnformat( const char * szDateTime,
                                              const char * szDateFormat, const char * szTimeFormat,
                                              long * plJulian, long * plMilliSec );

HB_EXTERN_END

#define HB_MINUTES_PER_DAY    ( 24 * 60 )
#define HB_SECONDS_PER_DAY    ( HB_MINUTES_PER_DAY * 60 )
#define HB_MILLISECS_PER_DAY  ( HB_SECONDS_PER_DAY * 1000 )

#define HB_TIMEDIFF_DEC       6     /* default number of decimal places in numeric timestamp diff values */

#if ( defined( _POSIX_C_SOURCE ) || defined( _XOPEN_SOURCE ) || \
      defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
      defined( HB_OS_SUNOS ) || defined( HB_OS_BEOS ) || \
      defined( HB_OS_ANDROID ) ) && \
   ! defined( HB_OS_DARWIN_5 ) && !defined( HB_HAS_LOCALTIME_R )
#  define HB_HAS_LOCALTIME_R
#endif

#endif /* HB_DATE_H_ */
