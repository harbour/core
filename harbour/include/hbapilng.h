/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Language API
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

#ifndef HB_APILNG_H_
#define HB_APILNG_H_

#include "hbdefs.h"
#include "hbvmpub.h"
#include "hbinit.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* Macro to publish a specific language module, for both C and Harbour level */
#define HB_LANG_ANNOUNCE( id )          HB_FUNC( HB_LANG_##id ) {}

/* Base values for the unified language item table */
#define HB_LANG_ITEM_BASE_ID            0
#define HB_LANG_ITEM_BASE_MONTH         6
#define HB_LANG_ITEM_BASE_DAY           18
#define HB_LANG_ITEM_BASE_NATMSG        25
#define HB_LANG_ITEM_BASE_ERRDESC       38
#define HB_LANG_ITEM_BASE_ERRINTR       89
#define HB_LANG_ITEM_BASE_TEXT          95
#define HB_LANG_ITEM_MAX_               98

typedef struct _HB_LANG
{
   void * pItemList[ HB_LANG_ITEM_MAX_ ];
} HB_LANG, * PHB_LANG, * HB_LANG_PTR;

/* Supported language list management */

extern BOOL     hb_langRegister         ( PHB_LANG lang );
extern BOOL     hb_langDeRegister       ( char * pszID );
extern PHB_LANG hb_langFind             ( char * pszID );

/* Default language selection and data query */

extern PHB_LANG hb_langSelect           ( PHB_LANG lang );
extern void *   hb_langDGetItem         ( int iIndex );

/* Compatibility interface */

extern char *   hb_langDGetErrorDesc    ( ULONG ulIndex );
extern char *   hb_langDGetErrorIntr    ( ULONG ulIndex );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APILNG_H_ */

