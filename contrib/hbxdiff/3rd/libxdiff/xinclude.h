/*
 *  LibXDiff by Davide Libenzi ( File Differential Library )
 *  Copyright (C) 2003  Davide Libenzi
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Davide Libenzi <davidel@xmailserver.org>
 *
 */

#if !defined(XINCLUDE_H)
#define XINCLUDE_H

#if defined(HAVE_HBCONFIG_H)
#include "_hbconf.h"
#endif /* #if defined(HAVE_HBCONFIG_H) */

#if defined(HAVE_WINCONFIG_H)
#include "winconf.h"
#endif /* #if defined(HAVE_CONFIG_H) */

#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif /* #if defined(HAVE_CONFIG_H) */

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif /* #if defined(HAVE_STDIO_H) */

#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif /* #if defined(HAVE_STDLIB_H) */

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif /* #if defined(HAVE_UNISTD_H) */

#if defined(HAVE_STRING_H)
#include <string.h>
#endif /* #if defined(HAVE_STRING_H) */

#if defined(HAVE_LIMITS_H)
#include <limits.h>
#endif /* #if defined(HAVE_LIMITS_H) */



#include "xmacros.h"
#include "xmissing.h"
#include "xdiff.h"
#include "xtypes.h"
#include "xutils.h"
#include "xadler32.h"
#include "xprepare.h"
#include "xdiffi.h"
#include "xemit.h"
#include "xbdiff.h"



#endif /* #if !defined(XINCLUDE_H) */

