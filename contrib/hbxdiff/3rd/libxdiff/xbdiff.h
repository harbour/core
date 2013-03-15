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

#if !defined(XBDIFF_H)
#define XBDIFF_H


#define XDL_BPATCH_HDR_SIZE (4 + 4)
#define XDL_MIN_BLKSIZE 16
#define XDL_INSBOP_SIZE (1 + 4)
#define XDL_COPYOP_SIZE (1 + 4 + 4)



unsigned long xdl_mmb_adler32(mmbuffer_t *mmb);
unsigned long xdl_mmf_adler32(mmfile_t *mmf);



#endif /* #if !defined(XBDIFF_H) */

