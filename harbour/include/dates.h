/*
 * $Id$
 */

#ifndef HB_DATES_H_
#define HB_DATES_H_

/* In msgxxx.c modules */
extern char *   hb_monthsname [];
extern char *   hb_daysname [];

extern long     hb_dow( long d, long m, long y );
extern char *   hb_dtoc( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern long     hb_dateEncode( long lDay, long lMonth, long lYear );
extern void     hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear );
extern void     hb_dateStrPut( char * szDate, long lDay, long lMonth, long lYear );
extern void     hb_dateStrGet( const char * szDate, long * plDay, long * plMonth, long * plYear );

#endif /* HB_DATES_H_ */
