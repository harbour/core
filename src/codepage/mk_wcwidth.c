/*
 * mk_wcwidth.c
 *
 * Copyright (C) 2020 Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/>>
 *
 * This software is placed in the public domain.
 *
 * This file is part of the mk_wcwidth() Unicode width calculation function.
 *
 * Original source: https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 *
 * Adapted for Harbour by Dongming Wang <wangdongming / at / gmail.com>
 * 
 * This is a simplified implementation of Unicode TR11 (East Asian Width).
 * It covers the most common Unicode character ranges.
 *
 */

#include "mk_wcwidth.h"
#include <stddef.h>

struct interval
{
   unsigned int first;
   unsigned int last;
};

/* Binary search in range table */
static int bisearch( wchar_t ucs, const struct interval *table, int max )
{
   int min = 0;
   int mid;

   if( ucs < table[ 0 ].first || ucs > table[ max ].last )
      return 0;

   while( max >= min )
   {
      mid = ( min + max ) / 2;

      if( ucs > table[ mid ].last )
         min = mid + 1;
      else if( ucs < table[ mid ].first )
         max = mid - 1;
      else
         return 1;
   }

   return 0;
}

/* Combining characters (width 0) */
static const struct interval combining[] =
{
   { 0x0300, 0x034E }, { 0x0360, 0x0362 }, { 0x0483, 0x0486 },
   { 0x0488, 0x0489 }, { 0x0591, 0x05B9 }, { 0x05BB, 0x05BD },
   { 0x05BF, 0x05BF }, { 0x05C1, 0x05C2 }, { 0x05C4, 0x05C5 },
   { 0x05C7, 0x05C7 }, { 0x0610, 0x0615 }, { 0x064B, 0x065E },
   { 0x0670, 0x0670 }, { 0x06D6, 0x06DC }, { 0x06DE, 0x06E4 },
   { 0x06E7, 0x06E8 }, { 0x06EA, 0x06ED }, { 0x070F, 0x070F },
   { 0x0711, 0x0711 }, { 0x0730, 0x074A }, { 0x07A6, 0x07B0 },
   { 0x07EB, 0x07F3 }, { 0x0901, 0x0902 }, { 0x093C, 0x093C },
   { 0x0941, 0x0948 }, { 0x094D, 0x094D }, { 0x0951, 0x0954 },
   { 0x0962, 0x0963 }, { 0x0981, 0x0981 }, { 0x09BC, 0x09BC },
   { 0x09C1, 0x09C4 }, { 0x09CD, 0x09CD }, { 0x09E2, 0x09E3 },
   { 0x0A01, 0x0A02 }, { 0x0A3C, 0x0A3C }, { 0x0A41, 0x0A42 },
   { 0x0A47, 0x0A48 }, { 0x0A4B, 0x0A4D }, { 0x0A70, 0x0A71 },
   { 0x0A81, 0x0A82 }, { 0x0ABC, 0x0ABC }, { 0x0AC1, 0x0AC5 },
   { 0x0AC7, 0x0AC8 }, { 0x0ACD, 0x0ACD }, { 0x0AE2, 0x0AE3 },
   { 0x0B01, 0x0B01 }, { 0x0B3C, 0x0B3C }, { 0x0B3F, 0x0B3F },
   { 0x0B41, 0x0B43 }, { 0x0B4D, 0x0B4D }, { 0x0B56, 0x0B56 },
   { 0x0B82, 0x0B82 }, { 0x0BC0, 0x0BC0 }, { 0x0BCD, 0x0BCD },
   { 0x0C3E, 0x0C40 }, { 0x0C46, 0x0C48 }, { 0x0C4A, 0x0C4D },
   { 0x0C55, 0x0C56 }, { 0x0CBC, 0x0CBC }, { 0x0CBF, 0x0CBF },
   { 0x0CC6, 0x0CC6 }, { 0x0CCC, 0x0CCD }, { 0x0CE2, 0x0CE3 },
   { 0x0D41, 0x0D43 }, { 0x0D4D, 0x0D4D }, { 0x0DCA, 0x0DCA },
   { 0x0DD2, 0x0DD4 }, { 0x0DD6, 0x0DD6 }, { 0x0E31, 0x0E31 },
   { 0x0E34, 0x0E3A }, { 0x0E47, 0x0E4E }, { 0x0EB1, 0x0EB1 },
   { 0x0EB4, 0x0EB9 }, { 0x0EBB, 0x0EBC }, { 0x0EC8, 0x0ECD },
   { 0x0F18, 0x0F19 }, { 0x0F35, 0x0F35 }, { 0x0F37, 0x0F37 },
   { 0x0F39, 0x0F39 }, { 0x0F71, 0x0F7E }, { 0x0F80, 0x0F84 },
   { 0x0F86, 0x0F87 }, { 0x0F90, 0x0F97 }, { 0x0F99, 0x0FBC },
   { 0x0FC6, 0x0FC6 }, { 0x102D, 0x1030 }, { 0x1032, 0x1032 },
   { 0x1036, 0x1037 }, { 0x1039, 0x103A }, { 0x1058, 0x1059 },
   { 0x1160, 0x11FF }, { 0x135F, 0x135F }, { 0x1712, 0x1714 },
   { 0x1732, 0x1734 }, { 0x1752, 0x1753 }, { 0x1772, 0x1773 },
   { 0x17B4, 0x17B5 }, { 0x17B7, 0x17BD }, { 0x17C6, 0x17C6 },
   { 0x17C9, 0x17D3 }, { 0x17DD, 0x17DD }, { 0x180B, 0x180D },
   { 0x18A9, 0x18A9 }, { 0x1920, 0x1922 }, { 0x1927, 0x1928 },
   { 0x1932, 0x1932 }, { 0x1939, 0x193B }, { 0x1A17, 0x1A18 },
   { 0x1B00, 0x1B03 }, { 0x1B34, 0x1B34 }, { 0x1B36, 0x1B3A },
   { 0x1B3C, 0x1B3C }, { 0x1B42, 0x1B42 }, { 0x1B6B, 0x1B73 },
   { 0x1DC0, 0x1DC3 }, { 0x200B, 0x200F }, { 0x202A, 0x202E },
   { 0x2060, 0x2063 }, { 0x20D0, 0x20EA }, { 0x302A, 0x302F },
   { 0x3099, 0x309A }, { 0xA806, 0xA806 }, { 0xA80B, 0xA80B },
   { 0xA825, 0xA826 }, { 0xFB1E, 0xFB1E }, { 0xFE00, 0xFE0F },
   { 0xFE20, 0xFE23 }, { 0xFEFF, 0xFEFF }, { 0xFFF9, 0xFFFB },
   { 0x10A01, 0x10A03 }, { 0x10A05, 0x10A06 }, { 0x10A0C, 0x10A0F },
   { 0x10A38, 0x10A3A }, { 0x10A3F, 0x10A3F }, { 0x1D167, 0x1D169 },
   { 0x1D173, 0x1D17A }, { 0x1D17B, 0x1D182 }, { 0x1D185, 0x1D18B },
   { 0x1D1AA, 0x1D1AD }, { 0xE0100, 0xE01EF }
};

/* Wide characters (width 2) - CJK full-width characters */
static const struct interval wide[] =
{
   { 0x1100, 0x1159 }, { 0x2E80, 0xA4CF }, { 0xAC00, 0xD7A3 },
   { 0xF900, 0xFAFF }, { 0xFE10, 0xFE19 }, { 0xFE30, 0xFE6F },
   { 0xFF00, 0xFF60 }, { 0xFFE0, 0xFFE6 }, { 0x20000, 0x2FFFD },
   { 0x30000, 0x3FFFD }
};

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
 */
int mk_wcwidth( wchar_t ucs )
{
   /* Test for 8-bit control characters */
   if( ucs == 0 )
      return 0;

   if( ucs < 32 || ( ucs >= 0x7F && ucs < 0xA0 ) )
      return 0;

   /* Binary search in table of non-spacing characters */
   if( bisearch( ucs, combining, sizeof( combining ) / sizeof( struct interval ) - 1 ) )
      return 0;

   /* If we arrive here, ucs is not a combining or C0/C1 control character */

   /* Binary search in table of wide characters */
   if( bisearch( ucs, wide, sizeof( wide ) / sizeof( struct interval ) - 1 ) )
      return 2;

   return 1;
}

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
 */
int mk_wcswidth( const wchar_t *pwcs )
{
   int width = 0;

   while( *pwcs != L'\0' )
   {
      int w = mk_wcwidth( *pwcs );

      if( w < 0 )
         return -1;

      width += w;
      pwcs++;
   }

   return width;
}

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
 */
int mk_wcswidth_cjk( const wchar_t *pwcs, size_t n )
{
   int width = 0;

   while( n-- > 0 && *pwcs != L'\0' )
   {
      int w = mk_wcwidth( *pwcs );

      if( w < 0 )
         return -1;

      width += w;
      pwcs++;
   }

   return width;
}
