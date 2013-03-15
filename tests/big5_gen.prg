/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    code to generate C source with conversion tables between BIG5 and UCS16
 *    using data defined by Unicode, Inc. in BIG5.TXT
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */


/* ***************************************************************************

these characters are mapped to U+FFFD REPLACEMENT CHARACTER in BIG5.TXT:
   0xA15A      SPACING UNDERSCORE             duplicates A1C4
   0xA1C3      SPACING HEAVY OVERSCORE        not in Unicode
   0xA1C5      SPACING HEAVY UNDERSCORE       not in Unicode
   0xA1FE      LT DIAG UP RIGHT TO LOW LEFT   duplicates A2AC
   0xA240      LT DIAG UP LEFT TO LOW RIGHT   duplicates A2AD
   0xA2CC      HANGZHOU NUMERAL TEN           conflicts with A451 mapping
   0xA2CE      HANGZHOU NUMERAL THIRTY        conflicts with A4CA mapping

duplicated character has the following mapping in BIG5.TXT:
   0xA1C4	0xFF3F	# FULLWIDTH LOW LINE
   0xA2AC	0x2571	# BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
   0xA2AD	0x2572	# BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
   0xA451	0x5341	# <CJK>
   0xA4CA	0x5345	# <CJK>


# iconv using the following mapping for them:
0xA15A	0x2574	# BOX DRAWINGS LIGHT LEFT
0xA1C3	0xFFE3	# FULLWIDTH MACRON
0xA1C5	0x02CD	# MODIFIER LETTER LOW MACRON
0xA1FE	0xFF0F	# FULLWIDTH SOLIDUS
0xA240	0xFF3C	# FULLWIDTH REVERSE SOLIDUS
0xA2CC	0x5341	# <CJK>
0xA2CE	0x5345	# <CJK>

# this seems to be closer mapping:
0xA15A	0xFF3F	# FULLWIDTH LOW LINE
0xA1C3	0xFFE3	# FULLWIDTH MACRON
0xA1C5	0x02CD	# MODIFIER LETTER LOW MACRON
0xA1FE	0x2571	# BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
0xA240	0x2572	# BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
0xA2CC	0x5341	# <CJK>
0xA2CE	0x5345	# <CJK>

*************************************************************************** */


//#define DO_START_OPT

proc main()
   local cLine, aVal, aVal2, aValU, aValU2, hVal, aInd, ;
         n, nn, nBG5, nU16, nMin, nMax, nUMin, nUMax, cResult, nBit

   aVal  := afill( array( 0x10000 ), 0 )
   aValU := afill( array( 0x10000 ), 0 )
   nMin  := nUMin := 0xFFFF
   nMax  := nUMax := 0x0000

   for each cLine in hb_aTokens( hb_memoRead( "BIG5.TXT" ), hb_eol() )
      if cLine = "0x" .and. ( n := hb_at( "0x", cLine, 7 ) ) != 0
         nBG5 := hb_hexToNum( substr( cLine, 3, 4 ) )
         nU16 := hb_hexToNum( substr( cLine, n + 2, 4 ) )
         if nBG5 == 0 .or. nU16 == 0
            ? "unrecognized line:", cLine:__enumIndex()
            return
         elseif nBG5 > 0xFFFF .or. nU16 > 0xFFFF
            ? "wrong character range at line:", cLine:__enumIndex()
            return
         endif
         aVal[ nBG5 ] := nU16
         if nBG5 > nMax
            nMax := nBG5
         endif
         if nBG5 < nMin
            nMin := nBG5
         endif
         if nU16 > nUMax
            nUMax := nU16
         endif
         if nU16 < nUMin
            nUMin := nU16
         endif
      endif
   next

   for n := 1 to len( aVal )
      if aVal[ n ] != 0
         aValU[ aVal[ n ] ] := n
      endif
   next

#if 0
   cResult += "static const HB_USHORT s_big5uni[ " + hb_ntos( nMax - nMin + 1 ) + " ] =" + hb_eol()
   cResult += "{" + hb_eol()
   l := 0
   for n := nMin to nMax
      if ++l > 8
         l := 1
         cResult += ","
         cResult += hb_eol()
         cResult += "   "
      elseif n == nMin
         cResult += "   "
      else
         cResult += ", "
      endif
      cResult += "0x"
      cResult += hb_numToHex( aVal[ n ], 4 )
   next
   cResult += hb_eol()
   cResult += "};" + hb_eol()
#endif

   ? "BIG5->UCS16 tables."
   ? "raw size:", hb_ntos( ( nMax - nMin + 1 ) * 2 )
#ifndef DO_START_OPT
   nMin := min( nMin, 41280 ) // optimal
#endif
   n := min_size( aVal, @nMin, nMax, @nBit )
   ? "minimal size:", hb_ntos( n ), ;
     "for", hb_ntos( hb_bitshift( 1, nBit ) ), "byte blocks, (from: "+hb_ntos( nMin ) + ")"
   calc_size( aVal, nMin, nMax, nBit, @hVal, @aInd, @nn )
   aVal2 := hash_to_array( hVal )


   cResult := ;
      "/*" + hb_eol() + ;
      " * Harbour Project source code:" + hb_eol() + ;
      " *    BIG5 <-> UCS16 conversion tables" + hb_eol() + ;
      " *    code generated automatically by tests/big5_gen.prg" + hb_eol() + ;
      " *" + hb_eol() + ;
      " * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>" + hb_eol() + ;
      " * www - http://harbour-project.org" + hb_eol() + ;
      " *" + hb_eol() + ;
      " */" + hb_eol() + ;
       hb_eol()

   cResult += '#include "hbapi.h"' + hb_eol()
   cResult += hb_eol()
   cResult += "#define HB_BIG5_FIRST   0x" + hb_numToHex( nMin, 4 ) + hb_eol()
   cResult += "#define HB_BIG5_LAST    0x" + hb_numToHex( nMax, 4 ) + hb_eol()
   cResult += "#define HB_BIG5_BITS    " + hb_ntos( nBit ) + hb_eol()
   cResult += hb_eol()

   cResult += array_to_code( aInd, "s_big5index", nn )
   cResult += hb_eol()
   cResult += array_to_code( aVal2, "s_big5_ucs16", 2 )
   cResult += hb_eol()
   cResult += index_func( "s_big5_to_ucs16", "s_big5index", "s_big5_ucs16", ;
                          "HB_BIG5_FIRST", "HB_BIG5_LAST", "HB_BIG5_BITS" )

   check_conv( aVal, aInd, aVal2, nMin, nMax, nBit )

   ?
   ? "UCS16->BIG5 tables."
   ? "raw size:", hb_ntos( ( nUMax - nUMin + 1 ) * 2 )
#ifndef DO_START_OPT
   nUMin := min( nUMin, 160 ) // optimal
#endif
   n := min_size( aValU, @nUMin, nUMax, @nBit )
   ? "minimal size:", hb_ntos( n ), ;
     "for", hb_ntos( hb_bitshift( 1, nBit ) ), "byte blocks, (from: "+hb_ntos( nUMin ) + ")"
   calc_size( aValU, nUMin, nUMax, nBit, @hVal, @aInd, @nn )
   aValU2 := hash_to_array( hVal )

   cResult += hb_eol()
   cResult += "#define HB_U16_FIRST    0x" + hb_numToHex( nUMin, 4 ) + hb_eol()
   cResult += "#define HB_U16_LAST     0x" + hb_numToHex( nUMax, 4 ) + hb_eol()
   cResult += "#define HB_U16_BITS     " + hb_ntos( nBit ) + hb_eol()
   cResult += hb_eol()
   cResult += array_to_code( aInd, "s_ucs16index", nn )
   cResult += hb_eol()
   cResult += array_to_code( aValU2, "s_ucs16_big5", 2 )
   cResult += hb_eol()
   cResult += index_func( "s_ucs16_to_big5", "s_ucs16index", "s_ucs16_big5", ;
                          "HB_U16_FIRST", "HB_U16_LAST", "HB_U16_BITS" )

   check_conv( aValU, aInd, aValU2, nUMin, nUMax, nBit )

   hb_memowrit( "big5.c", cResult )
return

static function array_to_code( aVal, cName, nn )
   local cResult, l, n

   cResult := "static const " + ;
              iif( nn == 1, "HB_BYTE", "HB_USHORT" ) + " " + ;
              cName + "[ " + hb_ntos( len( aVal ) ) + " ] =" + hb_eol()
   cResult += "{" + hb_eol()
   l := 0
   for n := 1 to len( aVal )
      if ++l > iif( nn == 1, 12, 8 )
         l := 1
         cResult += ","
         cResult += hb_eol()
         cResult += "   "
      elseif n == 1
         cResult += "   "
      else
         cResult += ", "
      endif
      cResult += "0x"
      cResult += hb_numToHex( aVal[ n ], nn * 2 )
   next
   cResult += hb_eol()
   cResult += "};" + hb_eol()
return cResult;

static function hash_to_array( hVal )
   local aVal := {}, cLine, n
   for each cLine in hVal
      for n := 1 to len( cLine ) step( 2 )
         aadd( aVal, bin2w( substr( cLine, n, 2 ) ) )
      next
   next
return aVal

function min_size( aVal, nMin, nMax, nBit )
   local n, nM, nS, nSize, nMinX
   nSize := 0xFFFFFF
   nMinX := nMin
#ifdef DO_START_OPT
   for nM := hb_bitAnd( nMin, 0xFF00 ) to nMin
#else
   for nM := nMin to nMin
#endif
      for n := 1 to 16
         nS := calc_size( aVal, nM, nMax, n )
         if nS < nSize
            nSize := nS
            nBit := n
            nMinX := nM
         endif
      next
   next
   nMin := nMinX
return nSize

function calc_size( aVal, nMin, nMax, nBit, hVal, aInd, nn )
   local nLine, n, cLine, c

   nLine := int( 2 ^ nBit )

   cLine := ""
   hVal := {=>}
   aInd := {}
   hb_hKeepOrder( hVal, .t. )
   for n := nMin to nMax
      cLine += i2bin( iif( n == 0, 0, aVal[ n ] ) )
      if len( cLine ) == nLine * 2
         hVal[ cLine ] := cLine
         aadd( aInd, hb_hpos( hVal, cLine ) - 1 )
         cLine := ""
      endif
   next
   if ! cLine == ""
      for each c in hVal
         if c = cLine
            cLine := c
            exit
         endif
      next
      hVal[ cLine ] := cLine
      aadd( aInd, hb_hpos( hVal, cLine ) - 1 )
   endif
   nn := iif( len( aInd ) > 256, 2, 1 )
   n := len( aInd ) * nn
   for each c in hVal
      n += len( c )
   next

return n

static function index_func( cName, cNameInd, cNameConv, cMin, cMax, cBit )
   local cResult

   cResult := "static HB_USHORT " + cName + "( int n )" + hb_eol() + ;
              "{" + hb_eol() + ;
              "   n -= " + cMin + ";" + hb_eol() + ;
              "   if( n >= 0 && n <= ( " + cMax + " - " + cMin + " ) )" + hb_eol() + ;
              "   {" + hb_eol() + ;
              "      return " + cNameConv + "[ ( " + cNameInd + ;
                     "[ n >> " + cBit + " ] << " + cBit + " ) +" + hb_eol() + ;
              space( len( cNameConv ) + 15 ) + ;
                     "( n & ( ( 1 << " + cBit + " ) - 1 ) ) ];" + hb_eol() + ;
              "   }" + hb_eol() + ;
              "   return 0;" + hb_eol() + ;
              "}" + hb_eol()

return cResult

static function conv_get( n, aInd, aVal2, nMin, nMax, nBit )
   local nDiv
   if n >= nMin .and. n <= nMax
      nDiv := 2 ^ nBit
      n -= nMin
      return aVal2[ aInd[ n / nDiv + 1 ] * nDiv + n % nDiv + 1 ]
   endif
return 0

static function check_conv( aVal, aInd, aVal2, nMin, nMax, nBit )
   local n, nVal
   for n := 1 to len( aVal )
      nVal := conv_get( n, aInd, aVal2, nMin, nMax, nBit )
      if aVal[ n ] != nVal
         ? "Wrong decoding:", n, aVal[ n ], nVal, len( aVal ), nMax, hb_eol()
         break
      endif
   next
return nil
