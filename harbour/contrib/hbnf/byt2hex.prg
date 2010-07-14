/*
 * $Id$
 */

/*
 * File......: byt2hex.prg
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:10   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:48   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYT2HEX(cByte)

  local cHexTable := "0123456789ABCDEF"
  local xHexString

  if valtype(cByte) != "C"
     xHexString := NIL
  else
     xHexString := substr(cHexTable, int(asc(cByte) / 16) + 1, 1) ;
                 + substr(cHexTable, int(asc(cByte) % 16) + 1, 1) ;
                 + "h"
  endif

RETURN xHexString
