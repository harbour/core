/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    MINIXML functions wrapper
 *
 * Copyright 2010 Petr Chornyj <myorg63@mail.ru>
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

#ifndef HBMXML_CH_
#define HBMXML_CH_

#define MXML_TAB                8 /* Tabs every N columns */

#define MXML_NO_CALLBACK        0 /* Don't use a type callback */
#define MXML_INTEGER_CALLBACK   1 /* Treat all data as integers */
#define MXML_OPAQUE_CALLBACK    2 /* Treat all data as opaque */
#define MXML_REAL_CALLBACK      3 /* Treat all data as real numbers */
#define MXML_TEXT_CALLBACK      4 /* Treat all data as text */
#define MXML_IGNORE_CALLBACK    5 /* Ignore all non-element content */

#define MXML_NO_PARENT          0

#define MXML_IGNORE            -1 

#define MXML_ELEMENT            0
#define MXML_INTEGER            1
#define MXML_OPAQUE             2
#define MXML_REAL               3
#define MXML_TEXT               4
#define MXML_CUSTOM             5

#define MXML_DESCEND            1 /* Descend when finding/walking */
#define MXML_NO_DESCEND         0 /* Don't descend when finding/walking */
#define MXML_DESCEND_FIRST     -1 /* Descend for first find */

#define MXML_WS_BEFORE_OPEN     0 /* Callback for before open tag */
#define MXML_WS_AFTER_OPEN      1 /* Callback for after open tag */
#define MXML_WS_BEFORE_CLOSE    2 /* Callback for before close tag */
#define MXML_WS_AFTER_CLOSE     3 /* Callback for after close tag */

#define MXML_ADD_BEFORE         0   /* Add node before specified node */
#define MXML_ADD_AFTER          1   /* Add node after specified node */
#define MXML_ADD_TO_PARENT      NIL /* Add node relative to parent */

#endif /* HBMXML_CH_ */
