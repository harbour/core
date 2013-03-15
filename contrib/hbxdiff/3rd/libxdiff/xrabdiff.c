/*
 *  xrabdiff by Davide Libenzi (Rabin's polynomial fingerprint based delta generator)
 *  Copyright (C) 2006  Davide Libenzi
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Davide Libenzi <davidel@xmailserver.org>
 *
 *
 *  Hints, ideas and code for the implementation came from:
 *
 *  Rabin's original paper: http://www.xmailserver.org/rabin.pdf
 *  Chan & Lu's paper:      http://www.xmailserver.org/rabin_impl.pdf
 *  Broder's paper:         http://www.xmailserver.org/rabin_apps.pdf
 *  LBFS source code:       http://www.fs.net/sfswww/lbfs/
 *  Geert Bosch's post:     http://marc.theaimsgroup.com/?l=git&m=114565424620771&w=2
 *
 */

#include "xinclude.h"


#if !defined(XRABPLY_TYPE32) && !defined(XRABPLY_TYPE64)
#define XRABPLY_TYPE64 long long
#define XV64(v) ((xply_word) v ## ULL)
#endif

#include "xrabply.c"



#define XRAB_SLIDE(v, c) do {					\
		if (++wpos == XRAB_WNDSIZE) wpos = 0;		\
		v ^= U[wbuf[wpos]];				\
		wbuf[wpos] = (c);				\
		v = ((v << 8) | (c)) ^ T[v >> XRAB_SHIFT];	\
	} while (0)


#define XRAB_MINCPYSIZE 12
#define XRAB_WBITS (sizeof(xply_word) * 8)



typedef struct s_xrabctx {
	long idxsize;
	long *idx;
	unsigned char const *data;
	long size;
} xrabctx_t;

typedef struct s_xrabcpyi {
	long src;
	long tgt;
	long len;
} xrabcpyi_t;

typedef struct s_xrabcpyi_arena {
	long cnt, size;
	xrabcpyi_t *acpy;
} xrabcpyi_arena_t;



static void xrab_init_cpyarena(xrabcpyi_arena_t *aca) {
	aca->cnt = aca->size = 0;
	aca->acpy = NULL;
}


static void xrab_free_cpyarena(xrabcpyi_arena_t *aca) {
	xdl_free(aca->acpy);
}


static int xrab_add_cpy(xrabcpyi_arena_t *aca, xrabcpyi_t const *rcpy) {
	long size;
	xrabcpyi_t *acpy;

	if (aca->cnt >= aca->size) {
		size = 2 * aca->size + 1024;
		if ((acpy = (xrabcpyi_t *)
		     xdl_realloc(aca->acpy, size * sizeof(xrabcpyi_t))) == NULL)
			return -1;
		aca->acpy = acpy;
		aca->size = size;
	}
	aca->acpy[aca->cnt++] = *rcpy;

	return 0;
}


static long xrab_cmnseq(unsigned char const *data, long start, long size) {
	unsigned char ch = data[start];
	unsigned char const *ptr, *top;

	for (ptr = data + start + 1, top = data + size; ptr < top && ch == *ptr; ptr++);

	return (long) (ptr - (data + start + 1));
}


static int xrab_build_ctx(unsigned char const *data, long size, xrabctx_t *ctx) {
	long i, isize, idxsize, seq, wpos = 0;
	xply_word fp = 0, mask;
	unsigned char ch;
	unsigned char const *ptr, *eot;
	long *idx;
	unsigned char wbuf[XRAB_WNDSIZE];
	long maxoffs[256];
	long maxseq[256];
	xply_word maxfp[256];

	memset(wbuf, 0, sizeof(wbuf));
	memset(maxseq, 0, sizeof(maxseq));
	isize = 2 * (size / XRAB_WNDSIZE);
	for (idxsize = 1; idxsize < isize; idxsize <<= 1);
	mask = (xply_word) (idxsize - 1);
	if ((idx = (long *) xdl_malloc(idxsize * sizeof(long))) == NULL)
		return -1;
	memset(idx, 0, idxsize * sizeof(long));
	for (i = 0; i + XRAB_WNDSIZE < size; i += XRAB_WNDSIZE) {
		/*
		 * Generate a brand new hash for the current window. Here we could
		 * try to perform pseudo-loop unroll by 4 blocks if necessary, and
		 * if we force XRAB_WNDSIZE to be a multiple of 4, we could reduce
		 * the branch occurence inside XRAB_SLIDE by a factor of 4.
		 */
		for (ptr = data + i, eot = ptr + XRAB_WNDSIZE; ptr < eot; ptr++)
			XRAB_SLIDE(fp, *ptr);

		/*
		 * Try to scan for single value scans, and store them in the
		 * array according to the longest one. Before we do a fast check
		 * to avoid calling xrab_cmnseq() when not necessary.
		 */
		if ((ch = data[i]) == data[i + XRAB_WNDSIZE - 1] &&
		    (seq = xrab_cmnseq(data, i, size)) > XRAB_WNDSIZE &&
		    seq > maxseq[ch]) {
			maxseq[ch] = seq;
			maxfp[ch] = fp;
			maxoffs[ch] = i + XRAB_WNDSIZE;
			seq = (seq / XRAB_WNDSIZE) * XRAB_WNDSIZE;
			i += seq - XRAB_WNDSIZE;
		} else
			idx[fp & mask] = i + XRAB_WNDSIZE;
	}

	/*
	 * Restore back the logest sequences by overwriting target hash buckets.
	 */
	for (i = 0; i < 256; i++)
		if (maxseq[i])
			idx[maxfp[i] & mask] = maxoffs[i];
	ctx->idxsize = idxsize;
	ctx->idx = idx;
	ctx->data = data;
	ctx->size = size;

	return 0;
}


static void xrab_free_ctx(xrabctx_t *ctx) {

	xdl_free(ctx->idx);
}


static int xrab_diff(unsigned char const *data, long size, xrabctx_t *ctx,
		     xrabcpyi_arena_t *aca) {
	long i, offs, ssize, src, tgt, esrc, etgt, wpos = 0;
	xply_word fp = 0, mask;
	long const *idx;
	unsigned char const *sdata;
	xrabcpyi_t rcpy;
	unsigned char wbuf[XRAB_WNDSIZE];

	xrab_init_cpyarena(aca);
	memset(wbuf, 0, sizeof(wbuf));
	for (i = 0; i < XRAB_WNDSIZE - 1 && i < size; i++)
		XRAB_SLIDE(fp, data[i]);
	idx = ctx->idx;
	sdata = ctx->data;
	ssize = ctx->size;
	mask = (xply_word) (ctx->idxsize - 1);
	while (i < size) {
		unsigned char ch = data[i++];

		XRAB_SLIDE(fp, ch);
		offs = idx[fp & mask];

		/*
		 * Fast check here to probabilistically reduce false positives
		 * that would trigger the slow path below.
		 */
		if (offs == 0 || ch != sdata[offs - 1])
			continue;

		/*
		 * Stretch the match both sides as far as possible.
		 */
		src = offs - 1;
		tgt = i - 1;
		for (; tgt > 0 && src > 0 && data[tgt - 1] == sdata[src - 1];
		     tgt--, src--);
		esrc = offs;
		etgt = i;
		for (; etgt < size && esrc < ssize && data[etgt] == sdata[esrc];
		     etgt++, esrc++);

		/*
		 * Avoid considering copies smaller than the XRAB_MINCPYSIZE
		 * threshold.
		 */
		if (etgt - tgt >= XRAB_MINCPYSIZE) {
			rcpy.src = src;
			rcpy.tgt = tgt;
			rcpy.len = etgt - tgt;
			if (xrab_add_cpy(aca, &rcpy) < 0) {
				xrab_free_cpyarena(aca);
				return -1;
			}

			/*
			 * Fill up the new window and exit with 'i' properly set on exit.
			 */
			for (i = etgt - XRAB_WNDSIZE; i < etgt; i++)
				XRAB_SLIDE(fp, data[i]);
		}
	}

	return 0;
}


static int xrab_tune_cpyarena(unsigned char const *data, long size, xrabctx_t *ctx,
			      xrabcpyi_arena_t *aca) {
	long i, cpos;
	xrabcpyi_t *rcpy;

	for (cpos = size, i = aca->cnt - 1; i >= 0; i--) {
		rcpy = aca->acpy + i;
		if (rcpy->tgt >= cpos)
			rcpy->len = 0;
		else if (rcpy->tgt + rcpy->len > cpos) {
			if ((rcpy->len = cpos - rcpy->tgt) >= XRAB_MINCPYSIZE)
				cpos = rcpy->tgt;
			else
				rcpy->len = 0;
		} else
			cpos = rcpy->tgt;
	}

	return 0;
}


int xdl_rabdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, xdemitcb_t *ecb) {
	long i, cpos, size;
	unsigned long fp;
	xrabcpyi_t *rcpy;
	xrabctx_t ctx;
	xrabcpyi_arena_t aca;
	mmbuffer_t mb[2];
	unsigned char cpybuf[32];

	fp = xdl_mmb_adler32(mmb1);
	if (xrab_build_ctx((unsigned char const *) mmb1->ptr, mmb1->size,
			   &ctx) < 0)
		return -1;
	if (xrab_diff((unsigned char const *) mmb2->ptr, mmb2->size, &ctx,
		      &aca) < 0) {
		xrab_free_ctx(&ctx);
		return -1;
	}
	xrab_tune_cpyarena((unsigned char const *) mmb2->ptr, mmb2->size, &ctx,
			   &aca);
	xrab_free_ctx(&ctx);

	/*
	 * Prepare and emit the binary patch file header. It will be used
	 * to verify that that file being patched matches in size and fingerprint
	 * the one that generated the patch.
	 */
	size = mmb1->size;
	XDL_LE32_PUT(cpybuf, fp);
	XDL_LE32_PUT(cpybuf + 4, size);

	mb[0].ptr = (char *) cpybuf;
	mb[0].size = 4 + 4;
	if (ecb->outf(ecb->priv, mb, 1) < 0) {
		xrab_free_cpyarena(&aca);
		return -1;
	}
	for (cpos = 0, i = 0; i < aca.cnt; i++) {
		rcpy = aca.acpy + i;
		if (rcpy->len == 0)
			continue;
		if (cpos < rcpy->tgt) {
			size = rcpy->tgt - cpos;
			if (size > 255) {
				cpybuf[0] = XDL_BDOP_INSB;
				XDL_LE32_PUT(cpybuf + 1, size);
				mb[0].ptr = (char *) cpybuf;
				mb[0].size = XDL_INSBOP_SIZE;
			} else {
				cpybuf[0] = XDL_BDOP_INS;
				cpybuf[1] = (unsigned char) size;
				mb[0].ptr = (char *) cpybuf;
				mb[0].size = 2;
			}
			mb[1].ptr = mmb2->ptr + cpos;
			mb[1].size = size;
			if (ecb->outf(ecb->priv, mb, 2) < 0) {
				xrab_free_cpyarena(&aca);
				return -1;
			}
			cpos = rcpy->tgt;
		}
		cpybuf[0] = XDL_BDOP_CPY;
		XDL_LE32_PUT(cpybuf + 1, rcpy->src);
		XDL_LE32_PUT(cpybuf + 5, rcpy->len);
		mb[0].ptr = (char *) cpybuf;
		mb[0].size = XDL_COPYOP_SIZE;
		if (ecb->outf(ecb->priv, mb, 1) < 0) {
			xrab_free_cpyarena(&aca);
			return -1;
		}
		cpos += rcpy->len;
	}
	xrab_free_cpyarena(&aca);
	if (cpos < mmb2->size) {
		size = mmb2->size - cpos;
		if (size > 255) {
			cpybuf[0] = XDL_BDOP_INSB;
			XDL_LE32_PUT(cpybuf + 1, size);
			mb[0].ptr = (char *) cpybuf;
			mb[0].size = XDL_INSBOP_SIZE;
		} else {
			cpybuf[0] = XDL_BDOP_INS;
			cpybuf[1] = (unsigned char) size;
			mb[0].ptr = (char *) cpybuf;
			mb[0].size = 2;
		}
		mb[1].ptr = mmb2->ptr + cpos;
		mb[1].size = size;
		if (ecb->outf(ecb->priv, mb, 2) < 0)
			return -1;
	}

	return 0;
}


int xdl_rabdiff(mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb) {
	mmbuffer_t mmb1, mmb2;

	if (!xdl_mmfile_iscompact(mmf1) || !xdl_mmfile_iscompact(mmf2))
		return -1;
	if ((mmb1.ptr = (char *) xdl_mmfile_first(mmf1, &mmb1.size)) == NULL)
		mmb1.size = 0;
	if ((mmb2.ptr = (char *) xdl_mmfile_first(mmf2, &mmb2.size)) == NULL)
		mmb2.size = 0;

	return xdl_rabdiff_mb(&mmb1, &mmb2, ecb);
}

