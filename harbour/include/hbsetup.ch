/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for runtime configuration, common for Harbour and C level.
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: This file is also used by C code. */

#ifndef HB_SETUP_CH_
#define HB_SETUP_CH_

/* NOTE: You can select here, which features you want to include of the
         different Clipper implementations. */

#define HB_EXTENSION              /* Enable Harbour extensions */

#define HB_C52_UNDOC              /* Enable CA-Cl*pper 5.2e undocumented features */
/* #define HB_C52_STRICT */       /* Enable CA-Cl*pper 5.2e strict compatibility */

#define HB_COMPAT_C53             /* Enable CA-Cl*pper 5.3x extensions */
#define HB_COMPAT_XPP             /* Enable Alaska Xbase++ extensions */
/* #define HB_COMPAT_VO */        /* Enable CA-VO extensions */
/* #define HB_COMPAT_FLAGSHIP */  /* Enable Flagship extensions */
/* #define HB_COMPAT_FOXPRO */    /* Enable FoxPro extensions */
/* #define HB_COMPAT_DBASE */     /* Enable dBase extensions */

/* NOTE: HB_SHORTNAMES must be defined manually if the symbol name length is
         set to 10 explicitly and not through the HB_C52_STRICT option
         [vszakats] */

/* Turn on short names support for the class engine */
#ifdef HB_C52_STRICT
   #define HB_SHORTNAMES
#endif

/*#define HB_CLS_ENFORCERO */     /* Activate the RO checking on OO DATA    */
                                  /* when not called from a constructor     */

#endif /* HB_SETUP_CH_ */

