/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for MATHDEFERRMODE function
 *
 * Copyright 2002 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#ifndef HB_MATH_CH_
#define HB_MATH_CH_

/* map the C math lib error definitions to harbour constants */
#define HB_MATH_ERR_UNKNOWN              -1
#define HB_MATH_ERR_NONE                  0
#define HB_MATH_ERR_DOMAIN                1
#define HB_MATH_ERR_SING                  2
#define HB_MATH_ERR_OVERFLOW              3
#define HB_MATH_ERR_UNDERFLOW             4
#define HB_MATH_ERR_TLOSS                 5
#define HB_MATH_ERR_PLOSS                 6

/* working mode for hb_matherr, the basic Harbour math error handler */
#define HB_MATH_ERRMODE_DEFAULT           0   /* no common error handling, save error data only;
                                                 Harbour function using math routines must handle error */
#define HB_MATH_ERRMODE_CDEFAULT          1   /* handle error by using the C RTL correction values */
#define HB_MATH_ERRMODE_USER              2   /* throw Harbour error, user MUST correct math error within Harbour error
                                                 handling */
#define HB_MATH_ERRMODE_USERDEFAULT       3   /* dito, but if user does not correct math error, default
                                                 error handling, i.e. by individual function applies */
#define HB_MATH_ERRMODE_USERCDEFAULT      4   /* as ERRMODE_USER, but if user does not correct math error, C RTL
                                                 correction values are used */

/* array element indices in aInfo parameter passed to math errorblock */
#define HB_MATHERRORBLOCK_RETVAL  1
#define HB_MATHERRORBLOCK_HANDLED 2

#endif /* HB_MATH_CH */
