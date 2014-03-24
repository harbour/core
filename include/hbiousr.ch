/*
 * Harbour Project source code:
 *    IOUSRD - module to create new FILE IO redirectors at prg level
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _HBIOUSR_CH
#define _HBIOUSR_CH

/* method numbers */
#define IOUSR_ACCEPT                 1
#define IOUSR_EXISTS                 2
#define IOUSR_DELETE                 3
#define IOUSR_RENAME                 4
#define IOUSR_COPY                   5

#define IOUSR_DIREXISTS              6
#define IOUSR_DIRMAKE                7
#define IOUSR_DIRREMOVE              8
#define IOUSR_DIRSPACE               9
#define IOUSR_DIRECTORY             10

#define IOUSR_TIMEGET               11
#define IOUSR_TIMESET               12
#define IOUSR_ATTRGET               13
#define IOUSR_ATTRSET               14

#define IOUSR_LINK                  15
#define IOUSR_LINKSYM               16
#define IOUSR_LINKREAD              17

#define IOUSR_OPEN                  18
#define IOUSR_CLOSE                 19
#define IOUSR_LOCK                  20
#define IOUSR_LOCKTEST              21
#define IOUSR_READ                  22
#define IOUSR_WRITE                 23
#define IOUSR_READAT                24
#define IOUSR_WRITEAT               25
#define IOUSR_TRUNCAT               26
#define IOUSR_SEEK                  27
#define IOUSR_SIZE                  28
#define IOUSR_EOF                   29
#define IOUSR_FLUSH                 30
#define IOUSR_COMMIT                31
#define IOUSR_CONFIGURE             32
#define IOUSR_HANDLE                33

#define IOUSR_METHODCOUNT           33

#endif /* _HBIOUSR_CH */
