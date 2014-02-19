/*
 * Harbour Project source code:
 *    common file with all HVM functions for compilers which can improve
 *    speed automatically inlining functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this #define HAVE TO be placed before all #include directives
 */
#define _HB_HASH_INTERNAL_
#define HB_MACRO_SUPPORT

#define HB_NO_FLATTEN

#define _HB_STACK_LOCAL_MACROS_
#define HB_STACK_PRELOAD

#define INCL_BASE
#define INCL_DOSMISC
#define INCL_DOSERRORS
#define INCL_DOSDATETIME
#define INCL_DOSSEMAPHORES
#define INCL_DOSPROCESS
#define INCL_DOSMEMMGR
#define INCL_NOPMAPI

/* For Linux and mremap() function */
#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

/* warning the order of included files is important
 * due to macros used to overload some functions
 */

#include "hbvmopt.h"
#include "hbfloat.h"

#include "hbstack.h"

#if defined( _HB_STACK_MACROS_ ) && defined( _HB_STACK_LOCAL_MACROS_ )
#  if defined( HB_MT_VM )
#     if defined( HB_USE_TLS )
#        if defined( __BORLANDC__ )
            static PHB_STACK HB_TLS_ATTR hb_stack_ptr = NULL;
#        else
            static HB_TLS_ATTR PHB_STACK hb_stack_ptr = NULL;
#        endif
#     elif ! defined( hb_stack_ptr_get )
         static HB_TLS_KEY hb_stack_key;
#     endif
#  else
      static HB_STACK hb_stack;
#  endif
#endif


#include "hvm.c"
#include "itemapi.c"
#include "hashes.c"
#include "arrays.c"
#include "classes.c"
#include "codebloc.c"
#include "dynsym.c"
#include "macro.c"
#include "set.c"
#include "memvars.c"
#include "thread.c"
#include "garbage.c"
#include "fm.c"
#include "strapi.c"
#include "extend.c"
#include "estack.c"
