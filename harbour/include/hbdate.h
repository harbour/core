/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Date API
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#ifndef HB_DATE_H_
#define HB_DATE_H_

#include "hbsetup.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

extern double   hb_dateSeconds( void );
extern char *   hb_dateCMonth( int iMonth );
extern char *   hb_dateCDOW( int iDay );
extern long     hb_dateDOW( long lYear, long lMonth, long lDay );
extern char *   hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern long     hb_dateEncode( long lYear, long lMonth, long lDay );
extern void     hb_dateDecode( long julian, long * plYear, long * plMonth, long * plDay );
extern void     hb_dateStrPut( char * szDate, long lYear, long lMonth, long lDay );
extern void     hb_dateStrGet( const char * szDate, long * plYear, long * plMonth, long * plDay );
extern char *   hb_dateDecStr( char * szDate, long lJulian );
extern long     hb_dateEncStr( char * szDate );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_DATE_H_ */
