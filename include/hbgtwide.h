/*
 * hbgtwide.h - Graphic Terminal Wide Character Width Support
 *
 * Copyright (C) 2001 Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/>
 *
 * This software is placed in the public domain.
 *
 * Original source: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 *
 * Adapted for Harbour by Dongming Wang <wangdongming / at / gmail.com>
 *
 * ANSI C89 compatible version for Harbour
 *
 * Unicode TR11 (East Asian Width) implementation
 */

#ifndef HBGTWIDE_H_
#define HBGTWIDE_H_

#include "hbdefs.h"

HB_EXTERN_BEGIN

/*
 * Returns the width in screen columns of a Unicode code point.
 *
 * Parameters:
 *   ucs: Unicode code point (HB_WCHAR32)
 *
 * Returns:
 *   0: Control characters, non-printing characters, combining characters
 *   1: Most characters (Latin, Cyrillic, Greek, Arabic, etc.)
 *   2: East Asian full-width characters (Chinese, Japanese, Korean)
 *
 * Notes:
 *   - This function implements Unicode TR11 (East Asian Width)
 *   - Ambiguous characters are treated as narrow (width 1)
 *   - Private use area characters are treated as narrow (width 1)
 *   - Unassigned characters are treated as narrow (width 1)
 */
HB_EXTERN int hb_wcwidth( HB_WCHAR32 ucs );

/*
 * Returns the width in screen columns of a null-terminated Unicode string.
 *
 * Parameters:
 *   pwcs: Pointer to wide character string (HB_WCHAR*)
 *
 * Returns:
 *   Total width of the string in screen columns
 *   -1 if the string contains a non-printable character
 *
 * Notes:
 *   - This function processes the entire string until null terminator
 *   - Returns -1 if any character has width 0 (non-printable)
 */
HB_EXTERN int hb_wcswidth( const HB_WCHAR *pwcs );

/*
 * Returns the width in screen columns of a substring of a Unicode string.
 *
 * Parameters:
 *   pwcs: Pointer to wide character string (HB_WCHAR*)
 *   n: Maximum number of characters to process
 *
 * Returns:
 *   Total width of the substring in screen columns
 *   -1 if the string contains a non-printable character
 *
 * Notes:
 *   - This function processes at most n characters
 *   - Returns -1 if any character has width 0 (non-printable)
 */
HB_EXTERN int hb_wcswidth_cjk( const HB_WCHAR *pwcs, HB_SIZE n );

HB_EXTERN_END

#endif /* HBGTWIDE_H_ */
