/*
 *  LibXDiff by Davide Libenzi ( File Differential Library )
 *  Copyright (C) 2003	Davide Libenzi
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

#include "xinclude.h"



static memallocator_t xmalt = {NULL, NULL, NULL};



int xdl_set_allocator(memallocator_t const *malt) {
	xmalt = *malt;
	return 0;
}


void *xdl_malloc(unsigned int size) {
	return xmalt.malloc ? xmalt.malloc(xmalt.priv, size): NULL;
}


void xdl_free(void *ptr) {
	if (xmalt.free)
		xmalt.free(xmalt.priv, ptr);
}


void *xdl_realloc(void *ptr, unsigned int size) {
	return xmalt.realloc ? xmalt.realloc(xmalt.priv, ptr, size): NULL;
}

