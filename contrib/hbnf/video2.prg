/* This is an original work from 2012 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

FUNCTION ft_CLS( nTop, nLeft, nBottom, nRight, nColor )
   RETURN hb_Scroll( nTop, nLeft, nBottom, nRight,,, hb_defaultValue( nColor, 0 ) )

FUNCTION ft_VidStr( nRow, nCol, cString, nColor )
   RETURN hb_DispOutAt( nRow, nCol, cString, hb_defaultValue( nColor, 0 ) )

FUNCTION ft_WrtChr( nRow, nCol, cChar, nColor )
   RETURN hb_DispOutAt( nRow, nCol, Left( cChar, 1 ), hb_defaultValue( nColor, 0 ) )
