/*
 * xHarbour Project source code:
 * HBXML - XML DOM oriented routines
 * Define wrappers for xHarbour PRG.
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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


#ifndef HB_XML_CH
#define HB_XML_CH

/* Styles */
#define HBXML_STYLE_NOINDENT      0  /* no indent nodes and insert new line after each node */
#define HBXML_STYLE_INDENT        1  /* indent nodes with 1 space and insert new line after each node  (default) */
#define HBXML_STYLE_TAB           2  /* indent nodes with tab spaces and insert new line after each node */
#define HBXML_STYLE_THREESPACES   4  /* indent nodes with 3 spaces and insert new line after each node */
#define HBXML_STYLE_NOESCAPE      8
#define HBXML_STYLE_NONEWLINE     16 /* no indent and no insert newline */

/* Status values */
#define HBXML_STATUS_ERROR        0
#define HBXML_STATUS_OK           1
#define HBXML_STATUS_MORE         2
#define HBXML_STATUS_DONE         3
#define HBXML_STATUS_UNDEFINED    4
#define HBXML_STATUS_MALFORMED    5

/* Error codes */
#define HBXML_ERROR_NONE          0
#define HBXML_ERROR_IO            1
#define HBXML_ERROR_NOMEM         2
#define HBXML_ERROR_OUTCHAR       3
#define HBXML_ERROR_INVNODE       4
#define HBXML_ERROR_INVATT        5
#define HBXML_ERROR_MALFATT       6
#define HBXML_ERROR_INVCHAR       7
#define HBXML_ERROR_NAMETOOLONG   8
#define HBXML_ERROR_ATTRIBTOOLONG 9
#define HBXML_ERROR_VALATTOOLONG  10
#define HBXML_ERROR_UNCLOSED      11
#define HBXML_ERROR_UNCLOSEDENTITY   12
#define HBXML_ERROR_WRONGENTITY      13

/* Node types */
#define HBXML_TYPE_TAG            0
#define HBXML_TYPE_COMMENT        1
#define HBXML_TYPE_PI             2
#define HBXML_TYPE_DIRECTIVE      3
#define HBXML_TYPE_DATA           4
#define HBXML_TYPE_CDATA          5
#define HBXML_TYPE_DOCUMENT       6

#endif
