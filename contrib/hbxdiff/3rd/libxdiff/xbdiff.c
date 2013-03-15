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


typedef struct s_bdrecord {
	struct s_bdrecord *next;
	unsigned long fp;
	char const *ptr;
} bdrecord_t;

typedef struct s_bdfile {
	char const *data, *top;
	chastore_t cha;
	unsigned int fphbits;
	bdrecord_t **fphash;
} bdfile_t;



static int xdl_prepare_bdfile(mmbuffer_t *mmb, long fpbsize, bdfile_t *bdf) {
	unsigned int fphbits;
	long i, size, hsize;
	char const *base, *data, *top;
	bdrecord_t *brec;
	bdrecord_t **fphash;

	fphbits = xdl_hashbits((unsigned int) (mmb->size / fpbsize) + 1);
	hsize = 1 << fphbits;
	if (!(fphash = (bdrecord_t **) xdl_malloc(hsize * sizeof(bdrecord_t *)))) {

		return -1;
	}
	for (i = 0; i < hsize; i++)
		fphash[i] = NULL;

	if (xdl_cha_init(&bdf->cha, sizeof(bdrecord_t), hsize / 4 + 1) < 0) {

		xdl_free(fphash);
		return -1;
	}

	if (!(size = mmb->size)) {
		bdf->data = bdf->top = NULL;
	} else {
		bdf->data = data = base = mmb->ptr;
		bdf->top = top = mmb->ptr + mmb->size;

		if ((data += (size / fpbsize) * fpbsize) == top)
			data -= fpbsize;

		for (; data >= base; data -= fpbsize) {
			if (!(brec = (bdrecord_t *) xdl_cha_alloc(&bdf->cha))) {

				xdl_cha_free(&bdf->cha);
				xdl_free(fphash);
				return -1;
			}

			brec->fp = xdl_adler32(0, (unsigned char const *) data,
					       XDL_MIN(fpbsize, (long) (top - data)));
			brec->ptr = data;

			i = (long) XDL_HASHLONG(brec->fp, fphbits);
			brec->next = fphash[i];
			fphash[i] = brec;
		}
	}

	bdf->fphbits = fphbits;
	bdf->fphash = fphash;

	return 0;
}


static void xdl_free_bdfile(bdfile_t *bdf) {

	xdl_free(bdf->fphash);
	xdl_cha_free(&bdf->cha);
}


unsigned long xdl_mmb_adler32(mmbuffer_t *mmb) {

	return mmb->size ? xdl_adler32(0, (unsigned char const *) mmb->ptr, mmb->size): 0;
}


unsigned long xdl_mmf_adler32(mmfile_t *mmf) {
	unsigned long fp = 0;
	long size;
	char const *blk;

	if ((blk = (char const *) xdl_mmfile_first(mmf, &size)) != NULL) {
		do {
			fp = xdl_adler32(fp, (unsigned char const *) blk, size);
		} while ((blk = (char const *) xdl_mmfile_next(mmf, &size)) != NULL);
	}
	return fp;
}


int xdl_bdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, bdiffparam_t const *bdp, xdemitcb_t *ecb) {
	long i, rsize, size, bsize, csize, msize, moff;
	unsigned long fp;
	char const *blk, *base, *data, *top, *ptr1, *ptr2;
	bdrecord_t *brec;
	bdfile_t bdf;
	mmbuffer_t mb[2];
	unsigned char cpybuf[32];

	if ((bsize = bdp->bsize) < XDL_MIN_BLKSIZE)
		bsize = XDL_MIN_BLKSIZE;
	if (xdl_prepare_bdfile(mmb1, bsize, &bdf) < 0) {

		return -1;
	}

	/*
	 * Prepare and emit the binary patch file header. It will be used
	 * to verify that that file being patched matches in size and fingerprint
	 * the one that generated the patch.
	 */
	fp = xdl_mmb_adler32(mmb1);
	size = mmb1->size;
	XDL_LE32_PUT(cpybuf, fp);
	XDL_LE32_PUT(cpybuf + 4, size);

	mb[0].ptr = (char *) cpybuf;
	mb[0].size = 4 + 4;

	if (ecb->outf(ecb->priv, mb, 1) < 0) {

		xdl_free_bdfile(&bdf);
		return -1;
	}

	if ((blk = (char const *) mmb2->ptr) != NULL) {
		size = mmb2->size;
		for (base = data = blk, top = data + size; data < top;) {
			rsize = XDL_MIN(bsize, (long) (top - data));
			fp = xdl_adler32(0, (unsigned char const *) data, rsize);

			i = (long) XDL_HASHLONG(fp, bdf.fphbits);
			for (msize = 0, brec = bdf.fphash[i]; brec; brec = brec->next)
				if (brec->fp == fp) {
					csize = XDL_MIN((long) (top - data), (long) (bdf.top - brec->ptr));
					for (ptr1 = brec->ptr, ptr2 = data; csize && *ptr1 == *ptr2;
					     csize--, ptr1++, ptr2++);

					if ((csize = (long) (ptr1 - brec->ptr)) > msize) {
						moff = (long) (brec->ptr - bdf.data);
						msize = csize;
					}
				}

			if (msize < XDL_COPYOP_SIZE) {
				data++;
			} else {
				if (data > base) {
					i = (long) (data - base);
					if (i > 255) {
						cpybuf[0] = XDL_BDOP_INSB;
						XDL_LE32_PUT(cpybuf + 1, i);

						mb[0].ptr = (char *) cpybuf;
						mb[0].size = XDL_INSBOP_SIZE;
					} else {
						cpybuf[0] = XDL_BDOP_INS;
						cpybuf[1] = (unsigned char) i;

						mb[0].ptr = (char *) cpybuf;
						mb[0].size = 2;
					}
					mb[1].ptr = (char *) base;
					mb[1].size = i;

					if (ecb->outf(ecb->priv, mb, 2) < 0) {

						xdl_free_bdfile(&bdf);
						return -1;
					}
				}

				data += msize;

				cpybuf[0] = XDL_BDOP_CPY;
				XDL_LE32_PUT(cpybuf + 1, moff);
				XDL_LE32_PUT(cpybuf + 5, msize);

				mb[0].ptr = (char *) cpybuf;
				mb[0].size = XDL_COPYOP_SIZE;

				if (ecb->outf(ecb->priv, mb, 1) < 0) {

					xdl_free_bdfile(&bdf);
					return -1;
				}
				base = data;
			}
		}
		if (data > base) {
			i = (long) (data - base);
			if (i > 255) {
				cpybuf[0] = XDL_BDOP_INSB;
				XDL_LE32_PUT(cpybuf + 1, i);

				mb[0].ptr = (char *) cpybuf;
				mb[0].size = XDL_INSBOP_SIZE;
			} else {
				cpybuf[0] = XDL_BDOP_INS;
				cpybuf[1] = (unsigned char) i;

				mb[0].ptr = (char *) cpybuf;
				mb[0].size = 2;
			}
			mb[1].ptr = (char *) base;
			mb[1].size = i;

			if (ecb->outf(ecb->priv, mb, 2) < 0) {

				xdl_free_bdfile(&bdf);
				return -1;
			}
		}
	}

	xdl_free_bdfile(&bdf);

	return 0;
}


int xdl_bdiff(mmfile_t *mmf1, mmfile_t *mmf2, bdiffparam_t const *bdp, xdemitcb_t *ecb) {
	mmbuffer_t mmb1, mmb2;

	if (!xdl_mmfile_iscompact(mmf1) || !xdl_mmfile_iscompact(mmf2)) {

		return -1;
	}

	if ((mmb1.ptr = (char *) xdl_mmfile_first(mmf1, &mmb1.size)) == NULL)
		mmb1.size = 0;
	if ((mmb2.ptr = (char *) xdl_mmfile_first(mmf2, &mmb2.size)) == NULL)
		mmb2.size = 0;

	return xdl_bdiff_mb(&mmb1, &mmb2, bdp, ecb);
}


long xdl_bdiff_tgsize(mmfile_t *mmfp) {
	long tgsize = 0, size, off, csize;
	char const *blk;
	unsigned char const *data, *top;

	if ((blk = (char const *) xdl_mmfile_first(mmfp, &size)) == NULL ||
	    size < XDL_BPATCH_HDR_SIZE) {

		return -1;
	}
	blk += XDL_BPATCH_HDR_SIZE;
	size -= XDL_BPATCH_HDR_SIZE;

	do {
		for (data = (unsigned char const *) blk, top = data + size;
		     data < top;) {
			if (*data == XDL_BDOP_INS) {
				data++;
				csize = (long) *data++;
				tgsize += csize;
				data += csize;
			} else if (*data == XDL_BDOP_INSB) {
				data++;
				XDL_LE32_GET(data, csize);
				data += 4;
				tgsize += csize;
				data += csize;
			} else if (*data == XDL_BDOP_CPY) {
				data++;
				XDL_LE32_GET(data, off);
				data += 4;
				XDL_LE32_GET(data, csize);
				data += 4;
				tgsize += csize;
			} else {

				return -1;
			}
		}
	} while ((blk = (char const *) xdl_mmfile_next(mmfp, &size)) != NULL);

	return tgsize;
}

