/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for version information
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
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

#ifndef HB_VER_H_
#define HB_VER_H_

#if defined(SIMPLEX)
   #define HB_VER_LEX "SimpLex"
#else
   #define HB_VER_LEX "Flex"
#endif
#define HB_VER_MAJOR    0       /* Major version number */
#define HB_VER_MINOR    38      /* Minor version number */
#define HB_VER_REVISION 1       /* Revision number */

/* TOFIX: Ideally these should be generated dynamically, until 
          then, they should be updated by the builder. [vszakats] */

#define HB_VER_LENTRY   "2002-01-16 11:10 UTC-0500 David G. Holm <dholm@jsd-llc.com>"
#define HB_VER_CHLCVS   "ChangeLog,v 1.3763 2002/01/16 16:10:57 dholm"
#define HB_VER_C_USR    ""
#define HB_VER_L_USR    ""
#define HB_VER_PRG_USR  ""

/* #include "hb_ver_.h" */

#endif /* HB_VER_H_ */
