/*
 * $Id$
 */

#ifndef HB_DATES_H_
#define HB_DATES_H_

/* In msgxxx.c modules */
extern char *   hb_monthsname [];
extern char *   hb_daysname [];

extern double   hb_secondsToday( void );
extern char *   hb_cmonth( int iMonth );
extern char *   hb_cdow( int iDay );
extern long     hb_dow( long lDay, long lMonth, long lYear );
extern char *   hb_dtoc( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern long     hb_dateEncode( long lDay, long lMonth, long lYear );
extern void     hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear );
extern void     hb_dateStrPut( char * szDate, long lDay, long lMonth, long lYear );
extern void     hb_dateStrGet( const char * szDate, long * plDay, long * plMonth, long * plYear );
extern char *   hb_dateDecStr( char * szDate, long lJulian );
extern long     hb_dateEncStr( char * szDate );

#endif /* HB_DATES_H_ */
