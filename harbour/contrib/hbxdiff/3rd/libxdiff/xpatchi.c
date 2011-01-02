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



#define XDL_MAX_FUZZ 3
#define XDL_MIN_SYNCLINES 4



typedef struct s_recinfo {
	char const *ptr;
	long size;
} recinfo_t;

typedef struct s_recfile {
	mmfile_t *mf;
	long nrec;
	recinfo_t *recs;
} recfile_t;

typedef struct s_hunkinfo {
	long s1, s2;
	long c1, c2;
	long cmn, radd, rdel, pctx, sctx;
} hunkinfo_t;

typedef struct s_patchstats {
	long adds, dels;
} patchstats_t;

typedef struct s_patch {
	recfile_t rf;
	hunkinfo_t hi;
	long hkrec;
	long hklen;
	long flags;
	patchstats_t ps;
	int fuzzies;
} patch_t;




static int xdl_load_hunk_info(char const *line, long size, hunkinfo_t *hki);
static int xdl_init_recfile(mmfile_t *mf, int ispatch, recfile_t *rf);
static void xdl_free_recfile(recfile_t *rf);
static char const *xdl_recfile_get(recfile_t *rf, long irec, long *size);
static int xdl_init_patch(mmfile_t *mf, long flags, patch_t *pch);
static void xdl_free_patch(patch_t *pch);
static int xdl_load_hunk(patch_t *pch, long hkrec);
static int xdl_first_hunk(patch_t *pch);
static int xdl_next_hunk(patch_t *pch);
static int xdl_line_match(patch_t *pch, const char *s, long ns, char const *m, long nm);
static int xdl_hunk_match(recfile_t *rf, long irec, patch_t *pch, int mode, int fuzz);
static int xdl_find_hunk(recfile_t *rf, long ibase, patch_t *pch, int mode,
			 int fuzz, long *hkpos, int *exact);
static int xdl_emit_rfile_line(recfile_t *rf, long line, xdemitcb_t *ecb);
static int xdl_flush_section(recfile_t *rf, long start, long top, xdemitcb_t *ecb);
static int xdl_apply_hunk(recfile_t *rf, long hkpos, patch_t *pch, int mode,
			  long *ibase, xdemitcb_t *ecb);
static int xdl_reject_hunk(recfile_t *rf, patch_t *pch, int mode,
			   xdemitcb_t *rjecb);
static int xdl_process_hunk(recfile_t *rff, patch_t *pch, long *ibase, int mode,
			    xdemitcb_t *ecb, xdemitcb_t *rjecb);




static int xdl_load_hunk_info(char const *line, long size, hunkinfo_t *hki) {
	char const *next;

	/*
	 * The diff header format should be:
	 *
	 *   @@ -OP,OC +NP,NC @@
	 *
	 * Unfortunately some software avoid to emit OP or/and NP in case
	 * of not existing old or new file (it should be mitted as zero).
	 * We need to handle both syntaxes.
	 */
	if (memcmp(line, "@@ -", 4))
		return -1;
	line += 4;
	size -= 4;

	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->s1 = xdl_atol(line, &next);
	size -= next - line;
	line = next;
	if (!size)
		return -1;
	if (*line == ',') {
		size--, line++;
		if (!size || !XDL_ISDIGIT(*line))
			return -1;
		hki->c1 = xdl_atol(line, &next);
		size -= next - line;
		line = next;
		if (!size || *line != ' ')
			return -1;
		size--, line++;
	} else if (*line == ' ') {
		size--, line++;
		hki->c1 = hki->s1;
		hki->s1 = 0;
	} else
		return -1;

	if (!size || *line != '+')
		return -1;
	size--, line++;
	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->s2 = xdl_atol(line, &next);
	size -= next - line;
	line = next;
	if (!size)
		return -1;
	if (*line == ',') {
		size--, line++;
		if (!size || !XDL_ISDIGIT(*line))
			return -1;
		hki->c2 = xdl_atol(line, &next);
		size -= next - line;
		line = next;
		if (!size || *line != ' ')
			return -1;
		size--, line++;
	} else if (*line == ' ') {
		size--, line++;
		hki->c2 = hki->s2;
		hki->s2 = 0;
	} else
		return -1;
	if (size < 2 || memcmp(line, "@@", 2) != 0)
		return -1;

	/*
	 * We start from zero, so decrement by one unless it's the special position
	 * '0' inside the unified diff (new or deleted file).
	 */
	if (hki->s1 > 0 && hki->c1 > 0)
		hki->s1--;
	if (hki->s2 > 0 && hki->c2 > 0)
		hki->s2--;

	return 0;
}


static int xdl_init_recfile(mmfile_t *mf, int ispatch, recfile_t *rf) {
	long narec, nrec, bsize;
	recinfo_t *recs, *rrecs;
	char const *blk, *cur, *top, *eol;

	narec = xdl_guess_lines(mf);
	if (!(recs = (recinfo_t *) xdl_malloc(narec * sizeof(recinfo_t)))) {

		return -1;
	}
	nrec = 0;
	if ((cur = blk = xdl_mmfile_first(mf, &bsize)) != NULL) {
		for (top = blk + bsize;;) {
			if (cur >= top) {
				if (!(cur = blk = xdl_mmfile_next(mf, &bsize)))
					break;
				top = blk + bsize;
			}
			if (nrec >= narec) {
				narec *= 2;
				if (!(rrecs = (recinfo_t *)
				      xdl_realloc(recs, narec * sizeof(recinfo_t)))) {

					xdl_free(recs);
					return -1;
				}
				recs = rrecs;
			}
			recs[nrec].ptr = cur;
			if (!(eol = memchr(cur, '\n', top - cur)))
				eol = top - 1;
			recs[nrec].size = (long) (eol - cur) + 1;
			if (ispatch && *cur == '\\' && nrec > 0 && recs[nrec - 1].size > 0 &&
			    recs[nrec - 1].ptr[recs[nrec - 1].size - 1] == '\n')
				recs[nrec - 1].size--;
			else
				nrec++;
			cur = eol + 1;
		}
	}
	rf->mf = mf;
	rf->nrec = nrec;
	rf->recs = recs;

	return 0;
}


static void xdl_free_recfile(recfile_t *rf) {

	xdl_free(rf->recs);
}


static char const *xdl_recfile_get(recfile_t *rf, long irec, long *size) {

	if (irec < 0 || irec >= rf->nrec)
		return NULL;
	*size = rf->recs[irec].size;

	return rf->recs[irec].ptr;
}


static int xdl_init_patch(mmfile_t *mf, long flags, patch_t *pch) {

	if (xdl_init_recfile(mf, 1, &pch->rf) < 0) {

		return -1;
	}
	pch->hkrec = 0;
	pch->hklen = 0;
	pch->flags = flags;
	pch->ps.adds = pch->ps.dels = 0;
	pch->fuzzies = 0;

	return 0;
}


static void xdl_free_patch(patch_t *pch) {

	xdl_free_recfile(&pch->rf);
}


static int xdl_load_hunk(patch_t *pch, long hkrec) {
	long size, i, nb;
	char const *line;

	for (;; hkrec++) {
		pch->hkrec = hkrec;
		if (!(line = xdl_recfile_get(&pch->rf, pch->hkrec, &size)))
			return 0;
		if (*line == '@')
			break;
	}
	if (xdl_load_hunk_info(line, size, &pch->hi) < 0) {

		return -1;
	}
	pch->hi.cmn = pch->hi.radd = pch->hi.rdel = pch->hi.pctx = pch->hi.sctx = 0;
	for (i = pch->hkrec + 1, nb = 0;
	     (line = xdl_recfile_get(&pch->rf, i, &size)) != NULL; i++) {
		if (*line == '@' || *line == '\n')
			break;
		if (*line == ' ') {
			nb++;
			pch->hi.cmn++;
		} else if (*line == '+') {
			if (pch->hi.radd + pch->hi.rdel == 0)
				pch->hi.pctx = nb;
			nb = 0;
			pch->hi.radd++;
		} else if (*line == '-') {
			if (pch->hi.radd + pch->hi.rdel == 0)
				pch->hi.pctx = nb;
			nb = 0;
			pch->hi.rdel++;
		} else {

			return -1;
		}
	}
	pch->hi.sctx = nb;
	if (pch->hi.cmn + pch->hi.radd != pch->hi.c2 ||
	    pch->hi.cmn + pch->hi.rdel != pch->hi.c1) {

		return -1;
	}
	pch->hklen = i - pch->hkrec - 1;

	return 1;
}


static int xdl_first_hunk(patch_t *pch) {

	return xdl_load_hunk(pch, 0);
}


static int xdl_next_hunk(patch_t *pch) {

	return xdl_load_hunk(pch, pch->hkrec + pch->hklen + 1);
}


static int xdl_line_match(patch_t *pch, const char *s, long ns, char const *m, long nm) {

	for (; ns > 0 && (s[ns - 1] == '\r' || s[ns - 1] == '\n'); ns--);
	for (; nm > 0 && (m[nm - 1] == '\r' || m[nm - 1] == '\n'); nm--);
	if (pch->flags & XDL_PATCH_IGNOREBSPACE) {
		for (; ns > 0 && (*s == ' ' || *s == '\t'); ns--, s++);
		for (; ns > 0 && (s[ns - 1] == ' ' || s[ns - 1] == '\t'); ns--);
		for (; nm > 0 && (*m == ' ' || *m == '\t'); nm--, m++);
		for (; nm > 0 && (m[nm - 1] == ' ' || m[nm - 1] == '\t'); nm--);
	}

	return ns == nm && memcmp(s, m, ns) == 0;
}


static int xdl_hunk_match(recfile_t *rf, long irec, patch_t *pch, int mode, int fuzz) {
	long i, j, z, fsize, psize, ptop, pfuzz, sfuzz, misses;
	char const *fline, *pline;

	/*
	 * Limit fuzz to not be greater than the prefix and suffix context.
	 */
	pfuzz = fuzz < pch->hi.pctx ? fuzz: pch->hi.pctx;
	sfuzz = fuzz < pch->hi.sctx ? fuzz: pch->hi.sctx;

	/*
	 * First loop through the prefix fuzz area. In this loop we simply
	 * note mismatching lines. We allow missing lines here, that is,
	 * some prefix context lines are missing.
	 */
	for (z = pfuzz, misses = 0, i = irec, j = pch->hkrec + 1,
	     ptop = pch->hkrec + 1 + pch->hklen - sfuzz;
	     z > 0 && i < rf->nrec && j < ptop; i++, j++, z--) {
		if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)))
			return 0;
		if (!(fline = xdl_recfile_get(rf, i, &fsize)) ||
		    !xdl_line_match(pch, fline, fsize, pline + 1, psize - 1))
			misses++;
	}
	if (misses > fuzz)
		return 0;

	/*
	 * Strict match loop.
	 */
	for (; i < rf->nrec && j < ptop; i++, j++) {
		for (; j < ptop; j++) {
			if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)))
				return 0;
			if (*pline == ' ' || *pline == mode)
				break;
		}
		if (j == ptop)
			break;
		if (!(fline = xdl_recfile_get(rf, i, &fsize)) ||
		    !xdl_line_match(pch, fline, fsize, pline + 1, psize - 1))
			return 0;
	}
	for (; j < ptop; j++)
		if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)) ||
		    *pline == ' ' || *pline == mode)
			return 0;

	/*
	 * Finally loop through the suffix fuzz area. In this loop we simply
	 * note mismatching lines. We allow missing lines here, that is,
	 * some suffix context lines are missing.
	 */
	for (z = sfuzz; z > 0 && i < rf->nrec; i++, j++, z--) {
		if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)))
			return 0;
		if (!(fline = xdl_recfile_get(rf, i, &fsize)) ||
		    !xdl_line_match(pch, fline, fsize, pline + 1, psize - 1))
			misses++;
	}

	return misses <= fuzz;
}


static int xdl_find_hunk(recfile_t *rf, long ibase, patch_t *pch, int mode,
			 int fuzz, long *hkpos, int *exact) {
	long hpos, hlen, i, j;
	long pos[2];

	hpos = mode == '-' ? pch->hi.s1: pch->hi.s2;
	hlen = mode == '-' ? pch->hi.cmn + pch->hi.rdel: pch->hi.cmn + pch->hi.radd;
	if (xdl_hunk_match(rf, hpos, pch, mode, fuzz)) {
		*hkpos = hpos;
		*exact = 1;
		return 1;
	}
	for (i = 1;; i++) {
		/*
		 * We allow a negative starting hunk position, up to the
		 * number of prefix context lines.
		 */
		j = 0;
		if (hpos - i >= ibase - pch->hi.pctx)
			pos[j++] = hpos - i;
		if (hpos + i + hlen <= rf->nrec)
			pos[j++] = hpos + i;
		if (!j)
			break;
		for (j--; j >= 0; j--)
			if (xdl_hunk_match(rf, pos[j], pch, mode, fuzz)) {
				*hkpos = pos[j];
				*exact = 0;
				return 1;
			}
	}

	return 0;
}


static int xdl_emit_rfile_line(recfile_t *rf, long line, xdemitcb_t *ecb) {
	mmbuffer_t mb;

	if (!(mb.ptr = (char *) xdl_recfile_get(rf, line, &mb.size)) ||
	    ecb->outf(ecb->priv, &mb, 1) < 0) {

		return -1;
	}

	return 0;
}


static int xdl_flush_section(recfile_t *rf, long start, long top, xdemitcb_t *ecb) {
	long i;

	for (i = start; i <= top; i++) {
		if (xdl_emit_rfile_line(rf, i, ecb) < 0) {

			return -1;
		}
	}

	return 0;
}


static int xdl_apply_hunk(recfile_t *rf, long hkpos, patch_t *pch, int mode,
			  long *ibase, xdemitcb_t *ecb) {
	long j, psize, ptop;
	char const *pline;
	mmbuffer_t mb;

	/*
	 * The hunk starting position (hkpos) can be negative, up to the number
	 * of prefix context lines. Since this function only emit the core of
	 * the hunk (the remaining lines are flushed by xdl_flush_section() calls)
	 * we need to normalize it by adding the number of prefix context lines.
	 * The normalized value of the starting position is then greater/equal
	 * to zero.
	 */
	hkpos += pch->hi.pctx;
	if (xdl_flush_section(rf, *ibase, hkpos - 1, ecb) < 0) {

		return -1;
	}
	*ibase = hkpos;
	for (j = pch->hkrec + 1 + pch->hi.pctx,
	     ptop = pch->hkrec + 1 + pch->hklen - pch->hi.sctx; j < ptop; j++) {
		if (!(pline = xdl_recfile_get(&pch->rf, j, &psize))) {

			return -1;
		}
		if (*pline == ' ') {
			if (xdl_emit_rfile_line(rf, *ibase, ecb) < 0) {

				return -1;
			}
			(*ibase)++;
		} else if (*pline != mode) {
			mb.ptr = (char *) pline + 1;
			mb.size = psize - 1;
			if (ecb->outf(ecb->priv, &mb, 1) < 0) {

				return -1;
			}
			pch->ps.adds++;
		} else {
			(*ibase)++;
			pch->ps.dels++;
		}
	}

	return 0;
}


static int xdl_reject_hunk(recfile_t *rf, patch_t *pch, int mode,
			   xdemitcb_t *rjecb) {
	long i, size, s1, s2, c1, c2;
	char const *line, *pre;
	mmbuffer_t mb;

	if (mode == '-') {
		s1 = pch->hi.s1;
		s2 = pch->hi.s2;
		c1 = pch->hi.c1;
		c2 = pch->hi.c2;
	} else {
		s1 = pch->hi.s2;
		s2 = pch->hi.s1;
		c1 = pch->hi.c2;
		c2 = pch->hi.c1;
	}
	s1 += pch->ps.adds - pch->ps.dels;
	if (xdl_emit_hunk_hdr(s1 + 1, c1, s2 + 1, c2, rjecb) < 0) {

		return -1;
	}
	for (i = pch->hkrec + 1;
	     (line = xdl_recfile_get(&pch->rf, i, &size)) != NULL; i++) {
		if (*line == '@' || *line == '\n')
			break;
		if (mode == '-' || *line == ' ') {
			mb.ptr = (char *) line;
			mb.size = size;
			if (rjecb->outf(rjecb->priv, &mb, 1) < 0) {

				return -1;
			}
		} else {
			pre = *line == '+' ? "-": "+";
			if (xdl_emit_diffrec(line + 1, size - 1, pre, strlen(pre),
					     rjecb) < 0) {

				return -1;
			}
		}
	}

	return 0;
}


static int xdl_process_hunk(recfile_t *rff, patch_t *pch, long *ibase, int mode,
			    xdemitcb_t *ecb, xdemitcb_t *rjecb) {
	int fuzz, exact, hlen, maxfuzz;
	long hkpos;

	hlen = mode == '-' ? pch->hi.cmn + pch->hi.rdel: pch->hi.cmn + pch->hi.radd;
	maxfuzz = XDL_MAX_FUZZ;
	if (hlen - maxfuzz < XDL_MIN_SYNCLINES)
		maxfuzz = hlen - XDL_MIN_SYNCLINES;
	if (maxfuzz < 0)
		maxfuzz = 0;
	for (fuzz = 0; fuzz <= maxfuzz; fuzz++) {
		if (xdl_find_hunk(rff, *ibase, pch, mode, fuzz,
				  &hkpos, &exact)) {
			if (xdl_apply_hunk(rff, hkpos, pch, mode,
					   ibase, ecb) < 0) {

				return -1;
			}
			if (!exact || fuzz)
				pch->fuzzies++;

			return 0;
		}
	}
	if (xdl_reject_hunk(rff, pch, mode, rjecb) < 0) {

		return -1;
	}

	return 0;
}


int xdl_patch(mmfile_t *mf, mmfile_t *mfp, int mode, xdemitcb_t *ecb,
	      xdemitcb_t *rjecb) {
	int hkres, exact;
	long hkpos, ibase;
	recfile_t rff;
	patch_t pch;

	if (xdl_init_recfile(mf, 0, &rff) < 0) {

		return -1;
	}
	if (xdl_init_patch(mfp, mode & ~XDL_PATCH_MODEMASK, &pch) < 0) {

		xdl_free_recfile(&rff);
		return -1;
	}
	mode &= XDL_PATCH_MODEMASK;
	ibase = 0;
	if ((hkres = xdl_first_hunk(&pch)) > 0) {
		do {
			if (xdl_process_hunk(&rff, &pch, &ibase, mode,
					     ecb, rjecb) < 0) {
				xdl_free_patch(&pch);
				xdl_free_recfile(&rff);
				return -1;
			}
		} while ((hkres = xdl_next_hunk(&pch)) > 0);
	}
	if (hkres < 0) {

		xdl_free_patch(&pch);
		xdl_free_recfile(&rff);
		return -1;
	}
	if (xdl_flush_section(&rff, ibase, rff.nrec - 1, ecb) < 0) {

		xdl_free_patch(&pch);
		xdl_free_recfile(&rff);
		return -1;
	}
	xdl_free_patch(&pch);
	xdl_free_recfile(&rff);

	return pch.fuzzies;
}

