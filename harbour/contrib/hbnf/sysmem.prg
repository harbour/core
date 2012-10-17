/*
 * $Id$
 */

/*
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   17 Aug 1991 15:46:10   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.3   15 Aug 1991 23:04:40   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:04   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:41:50   GLENN
 * Documentation mod and check for ft_int86() compatibility
 *
 *    Rev 1.0   01 Apr 1991 01:02:20   GLENN
 * Nanforum Toolkit
 *
 */

#include "hbmemory.ch"

FUNCTION FT_SYSMEM()

   RETURN Memory( HB_MEM_FM )
