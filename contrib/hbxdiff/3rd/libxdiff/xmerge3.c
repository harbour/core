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


#define XDL_MERGE3_BLKSIZE (1024 * 8)
#define XDL_MERGE3_CTXLEN 3



int xdl_merge3(mmfile_t *mmfo, mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb,
	       xdemitcb_t *rjecb) {
	xpparam_t xpp;
	xdemitconf_t xecfg;
	xdemitcb_t xecb;
	mmfile_t mmfp;

	if (xdl_init_mmfile(&mmfp, XDL_MERGE3_BLKSIZE, XDL_MMF_ATOMIC) < 0) {

		return -1;
	}

	xpp.flags = 0;

	xecfg.ctxlen = XDL_MERGE3_CTXLEN;

	xecb.priv = &mmfp;
	xecb.outf = xdl_mmfile_outf;

	if (xdl_diff(mmfo, mmf2, &xpp, &xecfg, &xecb) < 0) {

		xdl_free_mmfile(&mmfp);
		return -1;
	}

	if (xdl_patch(mmf1, &mmfp, XDL_PATCH_NORMAL, ecb, rjecb) < 0) {

		xdl_free_mmfile(&mmfp);
		return -1;
	}

	xdl_free_mmfile(&mmfp);

	return 0;
}

