/*
 * Harbour Project source code:
 *   CT3 Harbour header file
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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


#ifndef _CT_CH
#define _CT_CH 1

/* subsystem name */
#define CT_SUBSYSTEM "CT"

/* CSetArgErr() argument error behaviour */
#include "error.ch"
#define CT_ARGERR_WHOCARES      ES_WHOCARES
#define CT_ARGERR_WARNING       ES_WARNING
#define CT_ARGERR_ERROR         ES_ERROR
#define CT_ARGERR_CATASTROPHIC  ES_CATASTROPHIC
#define CT_ARGERR_IGNORE        -1

/* SETMATHERR() stati and modes for math error correction */
#define CT_MATHERR_STATUS_NOTFOUND   -1 /* math handler is not installed */
#define CT_MATHERR_STATUS_INACTIVE   0  /* math handler is installed but inactive */
#define CT_MATHERR_STATUS_ACTIVE     1  /* math handler is installed and active */

#define CT_MATHERR_MODE_NONE         0  /* no correction at all, program will exit */
#define CT_MATHERR_MODE_DEFAULT      1  /* default return value will be used, no error msgs ! */
#define CT_MATHERR_MODE_USER         2  /* error will be thrown to user who is responsible for error correction */
#define CT_MATHERR_MODE_USERDEFAULT  3  /* error will be thrown, but if user fails, default correction will be used */

/* SetAtLike() modes */
#define CT_SETATLIKE_EXACT     0
#define CT_SETATLIKE_WILDCARD  1

#endif
