/*
 * xHarbour Project source code:
 * DBF error codes
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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

#ifndef HB_DBFERR_H_
#define HB_DBFERR_H_

HB_EXTERN_BEGIN

/* DBF errors */
#define EDBF_UNSUPPORTED                           1000
#define EDBF_OPEN_DBF                              1001
#define EDBF_OPEN_MEMO                             1002
#define EDBF_OPEN_INDEX                            1003
#define EDBF_CREATE_DBF                            1004
#define EDBF_CREATE_MEMO                           1005
#define EDBF_CREATE_INDEX                          1006
#define EDBF_CREATE                   EDBF_CREATE_INDEX
#define EDBF_READ                                  1010
#define EDBF_WRITE                                 1011
#define EDBF_CORRUPT                               1012
#define EDBF_DATATYPE                              1020
#define EDBF_DATAWIDTH                             1021
#define EDBF_UNLOCKED                              1022
#define EDBF_SHARED                                1023
#define EDBF_APPENDLOCK                            1024
#define EDBF_READONLY                              1025
#define EDBF_LIMITEXCEEDED                         1027
#define EDBF_LOCKTIMEOUT                           1035
#define EDBF_LOCK                                  1038
/* ORDER errors */
#define EDBF_INVALIDKEY                            1026
#define EDBF_NOTINDEXED                            1201
#define EDBF_INVALIDORDER                          1050
#define EDBF_SCOPETYPE                             1051
#define EDBF_NOTCUSTOM                             1052
#define EDBF_INVALIDFOR                            1053
#define EDBF_KEYLENGTH                             1054
#define EDBF_SIGNATURE                             1055

#define EDBF_MEMOTYPE                              1056
#define EDBF_MEMOTOOLONG                           1057

#define EDBF_CREATE_TEMP                           1060
#define EDBF_WRITE_TEMP                            1061
#define EDBF_READ_TEMP                             1062

HB_EXTERN_END

#endif /* HB_DBFERR_H_ */
