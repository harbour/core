/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Date API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
