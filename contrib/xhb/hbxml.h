/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * HBXML - XML DOM oriented routines
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *    See also MXML library related copyright below
 *
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

/*
 * MXML (Mini XML) Library related copyright notice.
 * (referring to Harbour/xHarbour version).
 *
 * This source file contains a modified version of MXML (Mini XML)
 * library, developed by Giancarlo Niccolai. MXML is released under
 * LGPL license; this modified version (called HBXML) is released under
 * GPL with HARBOUR exception. HBXML license does not extends into
 * MXML; HBXML and any modification to HBXML is to be considered as
 * a part of Harbour or xHarbour projects, as it is modified to
 * be specifically working in the context of the compiler's RTL.
 *
 * Original MXML lib can be obtained requesting it at
 * Giancarlo Niccolai <giancarlo@niccolai.org>
 */

#ifndef HB_XML_H
#define HB_XML_H

/* Standard definitions */
#ifdef HB_OS_MAC
   #define MXML_LINE_TERMINATOR       '\r'
   #define MXML_SOFT_LINE_TERMINATOR  '\n'
#else
/*Notice, this works for unix AND windows */
   #define MXML_LINE_TERMINATOR       '\n'
   #define MXML_SOFT_LINE_TERMINATOR  '\r'
#endif

#define MXML_EOF                      -256

#define MXML_ALLOC_BLOCK              128
#define MXML_MAX_DEPTH                64

/* Styles */
#define MXML_STYLE_INDENT             0x0001
#define MXML_STYLE_TAB                0x0002
#define MXML_STYLE_THREESPACES        0x0004
#define MXML_STYLE_NOESCAPE           0x0008

/* Status vaules */

typedef enum
{
   MXML_STATUS_ERROR = 0,
   MXML_STATUS_OK    = 1,
   MXML_STATUS_MORE,
   MXML_STATUS_DONE,
   MXML_STATUS_UNDEFINED,
   MXML_STATUS_MALFORMED
} MXML_STATUS;

/* Error codes */
typedef enum
{
   MXML_ERROR_NONE = 0,
   MXML_ERROR_IO   = 1,
   MXML_ERROR_NOMEM,

   MXML_ERROR_OUTCHAR,
   MXML_ERROR_INVNODE,
   MXML_ERROR_INVATT,
   MXML_ERROR_MALFATT,
   MXML_ERROR_INVCHAR,
   MXML_ERROR_NAMETOOLONG,
   MXML_ERROR_ATTRIBTOOLONG,
   MXML_ERROR_VALATTOOLONG,
   MXML_ERROR_UNCLOSED,
   MXML_ERROR_UNCLOSEDENTITY,
   MXML_ERROR_WRONGENTITY
} MXML_ERROR_CODE;

/* Node types */

typedef enum
{
   MXML_TYPE_TAG = 0,
   MXML_TYPE_COMMENT,
   MXML_TYPE_PI,
   MXML_TYPE_DIRECTIVE,
   MXML_TYPE_DATA,
   MXML_TYPE_CDATA,     /* Used for <![CDATA[ nodes */
   MXML_TYPE_DOCUMENT   /* used for document level root node */
} MXML_NODE_TYPE;

/* Refil function */
struct tag_mxml_refil;
struct tag_mxml_output;

typedef void ( *MXML_REFIL_FUNC )( struct tag_mxml_refil * ref );
typedef void ( *MXML_OUTPUT_FUNC )( struct tag_mxml_output * out, const char * data, HB_ISIZ len );

/*************************************************
   Structures holding the XML data
 **************************************************/


/* Refiller */

typedef struct tag_mxml_refil
{
   /* status variables */
   MXML_STATUS     status;
   MXML_ERROR_CODE error;

   /* buffer for reading data */
   unsigned char * buffer;
   HB_ISIZ         bufsize; /* size of the whole buffer */
   HB_ISIZ         buflen;  /* valid characters in the current buffer */
   HB_ISIZ         bufpos;  /* current position */

   /* lenght of the stream for implementing progress indicators */
   HB_ISIZ streampos;
   HB_ISIZ streamlen;

   /* callback funcs */
   MXML_REFIL_FUNC refil_func;

   /* ungetc implementation */
   int sparechar;

   /* data available for callback functions */
   union
   {
      HB_FHANDLE hFile;
      void *     vPtr;
   } u;

} MXML_REFIL;


typedef struct tag_mxml_output
{
   /* status variables */
   MXML_STATUS     status;
   MXML_ERROR_CODE error;

   /* output operation */
   MXML_OUTPUT_FUNC output_func;

   /* data to implement progress indicators */
   int node_count;
   int node_done;

   /* data available for callback functions */
   union
   {
      HB_FHANDLE hFile;
      void *     vPtr;
   } u;

} MXML_OUTPUT;

/* tag mxml self growing string */
typedef struct
{
   char *  buffer;
   HB_ISIZ allocated;
   HB_ISIZ length;
} MXML_SGS;

typedef struct
{
   PHB_ITEM pName;
   PHB_ITEM pValue;
} HBXML_ATTRIBUTE, * PHBXML_ATTRIBUTE;


/* Allocator and deletor functions are meant to be redeclared by includers */
#ifndef MXML_ALLOCATOR
   #define MXML_ALLOCATOR    hb_xgrab
#endif

#ifndef MXML_DELETOR
   #define MXML_DELETOR      hb_xfree
#endif

#ifndef MXML_REALLOCATOR
   #define MXML_REALLOCATOR  hb_xrealloc
#endif

#endif
