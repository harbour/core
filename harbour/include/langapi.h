/*
 * $Id$
 */

/*
   Harbour Project source code

   Language API Header

   Copyright (C) 1999  Victor Szel <info@szelvesz.hu>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

#ifndef HB_LANGAPI_H_
#define HB_LANGAPI_H_

#define HB_LANG_TEXT_DATEFMT    0
#define HB_LANG_TEXT_YESCHAR    1
#define HB_LANG_TEXT_NOCHAR     2
#define HB_LANG_TEXT_MAX_	3

#define HB_LANG_ED_MAX_		51
#define HB_LANG_EI_MAX_		6

/* ; */

typedef struct
{
   BYTE nWeight;
   BYTE nFlags;
} HB_LANGCHAR;

typedef struct
{
   char * szName;
   char * szID;
   char * szCodepage;
   char * szTextList [ HB_LANG_TEXT_MAX_ ];
   char * szMonthNameList [ 12 ];
   char * szDayNameList [ 7 ];
   char * szErrorDescList [ HB_LANG_ED_MAX_ ];
   char * szErrorIntrList [ HB_LANG_EI_MAX_ ];
   HB_LANGCHAR * langcharList [ 256 ];
} HB_LANG, * PHB_LANG, * HB_LANG_PTR;

typedef struct _HB_LANGNODE
{
   PHB_LANG pLang;
   struct _HB_LANGNODE * pNext;
} HB_LANGNODE, * PHB_LANGNODE;

/* TODO: check if it have to be visible outside of langapi.c
 * It it is required then there is a conflict:
 * it is declared here as 'extern' and in langapi.c it is declared as
 * 'static' - Watcom compiler reports error for this conflict
 */
/* extern PHB_LANG langDef; */
extern PHB_LANGNODE langList;

/* Supported language list management */

extern void     hb_langListAdd          ( PHB_LANG lang );
extern PHB_LANG hb_langListFind         ( char * szName );
extern void     hb_langListRelease      ( void );

/* Default language selection and data query */

extern void     hb_langDSet 	  	( PHB_LANG lang );
extern PHB_LANG hb_langDGet 	  	( void );

extern char *   hb_langDGetName  	( void );
extern char *   hb_langDGetID    	( void );
extern char *   hb_langDGetText		( ULONG ulIndex );
extern char *   hb_langDGetDayName	( ULONG ulIndex );
extern char *   hb_langDGetMonthName	( ULONG ulIndex );
extern char *   hb_langDGetErrorDesc	( ULONG ulIndex );
extern char *   hb_langDGetErrorIntr	( ULONG ulIndex );

/* Single language */

extern PHB_LANG hb_langNew              ( void );
extern void     hb_langDelete           ( PHB_LANG lang );

#endif /* HB_LANGAPI_H_ */
