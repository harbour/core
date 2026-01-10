/*
 * mk_wcwidth.h
 *
 * Copyright (C) 2001 Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/>
 *
 * This software is placed in the public domain.
 *
 * Historical reference: https://www.postgresql.org/message-id/attachment/8417/pg_mb_utf8.c
 *
 * Original source: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 *
 * Adapted for Harbour by Dongming Wang <wangdongming / at / gmail.com>
 *
 * ANSI C89 compatible version for Harbour
 */

#ifndef MK_WCWIDTH_H
#define MK_WCWIDTH_H

/* Include Harbour type definitions */
#include "hbdefs.h"

/* Type aliases: preserve original API */
#define wchar_t  HB_WCHAR
#define size_t   HB_SIZE

/* Combining character width */
#define COMBINING_WIDTH 0

/* Zero width character */
#define ZERO_WIDTH 0

/* Full width character */
#define FULL_WIDTH 2

/* Half width character */
#define HALF_WIDTH 1

/* Wide character width */
#define WIDE_WIDTH 2

/* Narrow character width */
#define NARROW_WIDTH 1

/* Neutral character width */
#define NEUTRAL_WIDTH 1

/* Ambiguous character width */
#define AMBIGUOUS_WIDTH 1

/*
 * Function: mk_wcwidth
 * --------------------
 * Returns the width in screen columns of a Unicode code point.
 *
 * Parameters:
 *   ucs: Unicode code point (wchar_t)
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
int mk_wcwidth( wchar_t ucs );

/*
 * Function: mk_wcswidth
 * ---------------------
 * Returns the width in screen columns of a null-terminated Unicode string.
 *
 * Parameters:
 *   pwcs: Pointer to wide character string
 *
 * Returns:
 *   Total width of the string in screen columns
 *   -1 if the string contains a non-printable character
 *
 * Notes:
 *   - This function processes the entire string until null terminator
 *   - Returns -1 if any character has width 0 (non-printable)
 */
int mk_wcswidth( const wchar_t *pwcs );

/*
 * Function: mk_wcswidth_cjk
 * -------------------------
 * Returns the width in screen columns of a substring of a Unicode string.
 *
 * Parameters:
 *   pwcs: Pointer to wide character string
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
int mk_wcswidth_cjk( const wchar_t *pwcs, size_t n );

#endif /* MK_WCWIDTH_H */
