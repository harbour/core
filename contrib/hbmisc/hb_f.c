/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

/* Redirecting a very old and limited copy-paste adaptation
   of NanFor lib's file handling functions into their originals.
   I haven't verified any minor differences that developed
   througout the years, it's possible there are some.
   Two marginal functions are not supported, see below.
   [vszakats] */

#include "hbapi.h"

HB_FUNC_TRANSLATE( HB_FUSE, FT_FUSE )
HB_FUNC_TRANSLATE( HB_FRECNO, FT_FRECNO )
HB_FUNC_TRANSLATE( HB_FSKIP, FT_FSKIP )
HB_FUNC_TRANSLATE( HB_FREADLN, FT_FREADLN )
HB_FUNC_TRANSLATE( HB_FATEOF, FT_FEOF )
#if defined( HB_LEGACY_LEVEL4 )
HB_FUNC_TRANSLATE( HB_FEOF, FT_FEOF )
#endif
HB_FUNC_TRANSLATE( HB_FGOTO, FT_FGOTO )
HB_FUNC_TRANSLATE( HB_FGOBOTTOM, FT_FGOBOTTOM )
HB_FUNC_TRANSLATE( HB_FGOTOP, FT_FGOTOP )
HB_FUNC_TRANSLATE( HB_FLASTREC, FT_FLASTREC )
HB_FUNC_TRANSLATE( HB_FSELECT, FT_FSELECT )

HB_FUNC( HB_FINFO )  /* used for debugging - now a dummy function */
{
   hb_reta( 6 );
   hb_storvni( 0, -1, 1 );
   hb_storvni( 0, -1, 2 );
   hb_storvni( 0, -1, 3 );
   hb_storvnint( 0, -1, 4 );
   hb_storvnint( 0, -1, 5 );
   hb_storvl( HB_FALSE, -1, 6 );
}

/* Warning: This is a rogue function! It is a first shot at adding the logic
   to read .csv records that respect EOL embedded within quotes.
   It is very common, especially with Microsoft products, for
   comma-separated files to allow a field (usually an address field)
   to have hard returns within it. These records appear corrupted to any
   reader that presumes all hard returns are record separators.

   This function is useful right now to loop through a CSV file
   DO WHILE ! ft_FEof(), but it does NOT recognize the same record count
   and positioning that the other functions in this file use.
   It does its own skip and read, so an entire file can be read
   sequentially with just this function.
   [BH] */

HB_FUNC( HB_FREADANDSKIP )  /* TODO: Rewrite with portable EOL support */
{
   hb_retc_null();
}
