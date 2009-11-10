/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Simple program to generate information for Harbour CP module definition.
 *    Compile it with Clipper and link with given national sorting module
 *    (usually ntx*.obj) and then execute to generate letters strings for
 *    given national sorting module. Then use this string to define Harbour
 *    CP module in src/codepage/ directory.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */


proc main( cdp, info, unicode )
   local cUp, cLo, cOrd, cOrd2, c, i, a, lWarn, lBin, lEqual

   set alternate to cpinfo.txt additive
   set alternate on


#ifdef __HARBOUR__
   /* for test */
   set( _SET_CODEPAGE, iif( empty( cdp ), "PLMAZ", upper( cdp ) ) )
   lEqual := .t.
#else
   lEqual := .f.
#endif

   if !empty( cdp )
      memowrit( "cp" + lower( cdp ) + ".c", genCP( cdp, info, unicode ) )
   endif

   a := array( 256 )
   for i := 1 to len( a )
      a[ i ] := i - 1
   next
   asort( a,,, { |x,y| chr( x ) + chr( 0 ) < chr( y ) + chr( 0 ) } )

   ? date(), time(), os(), version()
#ifdef __HARBOUR__
   ? "Character encoding: " + Set( _SET_CODEPAGE )
#else
   ? "Character encoding: " + _natSortVersion()
#endif
   ? repl( "=", 50 )
   lBin := .t.
   for i := 1 to len( a ) - 1
      if a[ i ] > a[ i + 1 ]
         lBin := .f.
         exit
      endif
   next
   if lBin
      ? "simple byte sorting !!!"
   endif
   lBin := lWarn := .f.
   cUp := cLo := cOrd := ""
   for i := 1 to len( a )
      if i < len(a) .and. a[i] > a[ i + 1 ] .and. !isalpha( chr( a[ i ] ) )
         ? "non alpha character " + charval( chr( a[ i ] ) ) + ;
           " sorted in non ASCII order !!!"
         lWarn := .t.
      endif
      c := chr( a[ i ] )
      if i < len(a)
         if c + chr( 0 ) > chr( a[ i + 1 ] ) + chr( 0 )
            ? "character " + charis( c ) + " is wrongly sorted"
            lBin := lWarn := .t.
         elseif !lEqual .and. c + chr( 0 ) = chr( a[ i + 1 ] ) + chr( 0 )
            ? "character " + charis( c ) + " and " + chr( a[ i + 1 ] ) + ;
              " have the same weight"
            lWarn := .t.
         endif
      endif
      cOrd += c
      if isdigit( c )
         if asc( c ) < asc( "0" ) .or. asc( c ) > asc( "9" )
            ? "character " + charis( c ) + " defined as digit"
            lBin := lWarn := .t.
         endif
      elseif asc( c ) >= asc( "0" ) .and. asc( c ) <= asc( "9" )
         ? "character " + charis( c ) + " is not defined as digit"
         lBin := lWarn := .t.
      endif
      if isalpha( c )
         if isupper( c )
            cUp += c
            if islower( c )
               ? "character " + charis( c ) + " defined as upper and lower"
               lWarn := .t.
            endif
            if lower( c ) == c
               ? "character " + charis( c ) + ;
                 " is the same as upper and lower"
               lWarn := .t.
            elseif !islower( lower( c ) )
               ? "character " + charis( c ) + ;
                 " has lower character " + charis( lower( c ) ) + ;
                 " not marked as lower"
               lBin := lWarn := .t.
            endif
         elseif islower( c )
            cLo += c
            if isupper( c )
               ? "character " + charis( c ) + " defined as upper and lower"
               lWarn := .t.
            endif
            if upper( c ) == c
               ? "character " + charis( c ) + ;
                 " is the same as upper and lower"
               lWarn := .t.
            elseif !isupper( upper( c ) )
               ? "character " + charis( c ) + ;
                 " has upper character " + charis( upper( c ) ) + ;
                 " not marked as upper"
               lBin := lWarn := .t.
            endif
         else
            ? "character " + charis( c ) + " not defined as upper or lower"
            lBin := lWarn := .t.
         endif
      else
         if islower( c ) .or. isupper( c )
            ? "wrongly defined character " + ;
              charval( c ) + ":" + charinfo( c )
            lBin := lWarn := .t.
         endif
         if ! c == lower( c )
            ? "non alpha character " + charis( c ) + " has corresponding " + ;
              "lower character " + charis( lower( c ) )
            lBin := lWarn := .t.
         endif
         if ! c == upper( c )
            ? "non alpha character " + charis( c ) + " has corresponding " + ;
              "upper character " + charis( upper( c ) )
            lBin := lWarn := .t.
         endif
      endif
   next
   for i := 1 to len( cUp ) - 1
      c := substr( cUp, i, 1 )
      if c + chr( 0 ) > substr( cUp, i + 1, 1 ) + chr( 0 )
         ? "letter " + charis( c ) + " is wrongly sorted"
         lWarn := .t.
      endif
   next
   for i := 1 to len( cLo ) - 1
      c := substr( cLo, i, 1 )
      if c + chr( 0 ) > substr( cLo, i + 1, 1 ) + chr( 0 )
         ? "letter " + charis( c ) + " is wrongly sorted"
         lWarn := .t.
      endif
   next
   cOrd2 := ""
   for i := 0 to 255
      if i == asc( cUp )
         cOrd2 += cUp
      elseif i == asc( cLo )
         cOrd2 += cLo
      endif
      c := chr( i )
      if ! c $ cUp + cLo
         cOrd2 += chr( i )
      endif
   next
   if ! len( cUp ) == len( cLo )
      ? "number of upper and lower characters is different"
      lWarn := .t.
   endif
   if ! cOrd == cOrd2
      ? "letters are not sorted continuously"
      lWarn := .t.
   endif
   if lWarn
      ? "Warning: irregular CP which needs special definition in Harbour"
      if lBin
         ? "         using binary tables generated by this program"
      endif
   endif
   ? '      upper: "' + cUp + '"'
   ? '      lower: "' + cLo + '"'
   if pad_letters( @cUp, @cLo )
      ? 'HB_CP_UPPER: "' + cUp + '"'
      ? 'HB_CP_LOWER: "' + cLo + '"'
   endif
   ? repl( "=", 50 )
   ?
return

static function pad_letters( cUp, cLo )
   local lRet, cUp2, cLo2, cU, cL, i, j

   cUp2 := cLo2 := ""

   i := j := 1
   while i <= len( cUp ) .or. j <= len( cLo )
      cU := substr( cUp, i, 1 )
      cL := substr( cLo, j, 1 )
      if upper( cL ) == cU .and. lower( cU ) == cL
         ++i
         ++j
      elseif cL == "" .or. ( ! cU == "" .and. !islower( lower( cU ) ) )
         cL := " "
         ++i
      elseif cU == "" .or. !isupper( upper( cL ) )
         cU := " "
         ++j
      elseif upper( cL ) $ substr( cUp, i + 1 )
         cL := lower( cU )
         ++i
      else
         cU := upper( cL )
         ++j
      endif
      cUp2 += cU
      cLo2 += cL
   enddo

   lRet := !( cUp == cUp2 .and. cLo == cLo2 )
   cUp := cUp2
   cLo := cLo2
return lRet

static function charval( c )
return "'" + c + "' (" + ltrim( str( asc( c ) ) ) + ")"

static function charis( c )
return "'" + c + "' (" + ltrim( str( asc( c ) ) ) + ":" + ;
       iif( isalpha( c ), "A", " " ) + ;
       iif( isupper( c ), "U", " " ) + ;
       iif( islower( c ), "L", " " ) + ;
       iif( isdigit( c ), "D", " " ) + ")"

static function charinfo( c )
   local cInfo
   cInfo :=   "ISALPHA->" + iif( isalpha( c ), "Y", "N" )
   cInfo += ", ISUPPER->" + iif( isupper( c ), "Y", "N" )
   cInfo += ", ISLOWER->" + iif( islower( c ), "Y", "N" )
   cInfo += ", ISDIGIT->" + iif( isdigit( c ), "Y", "N" )
   cInfo += ", UPPER->'" + upper( c ) + "'"
   cInfo += ", LOWER->'" + lower( c ) + "'"
return cInfo


#ifdef __HARBOUR__
   #include "hbextcdp.ch"
   #define EOL    hb_osNewLine()
#else
   #define EOL    chr( 13 ) + chr( 10 )
#endif

#define HB_CDP_DIGIT    1
#define HB_CDP_ALPHA    2
#define HB_CDP_LOWER    4
#define HB_CDP_UPPER    8

static function genCP( id, info, unicode )
   local flags[ 256 ], upper[ 256 ], lower[ 256 ], sort[ 256 ], tmp[ 256 ]
   local i, c

   id := upper( id )
   if empty( info )
      info := _natSortVer()
   endif
   if empty( unicode )
      unicode := "437"
   else
      unicode := upper( unicode )
   endif

   for i := 1 to 256
      c := chr( i - 1 )
      flags[ i ] := 0
      if isdigit( c )
         flags[ i ] += HB_CDP_DIGIT
      endif
      if isalpha( c )
         flags[ i ] += HB_CDP_ALPHA
      endif
      if isupper( c )
         flags[ i ] += HB_CDP_UPPER
      endif
      if islower( c )
         flags[ i ] += HB_CDP_LOWER
      endif
      upper[ i ] := asc( upper( c ) )
      lower[ i ] := asc( lower( c ) )
      tmp[ i ] := i - 1
   next
   asort( tmp,,, { |x,y| chr( x ) + chr( 0 ) < chr( y ) + chr( 0 ) } )
   for i := 1 to 256
      sort[ tmp[ i ] + 1 ] := i - 1
   next

   return genCPfile( id, info, unicode, flags, upper, lower, sort )

static function genCPfile( id, info, unicode, flags, upper, lower, sort )
   local cDef := ;
      '/*' + EOL + ' * $Id$' + EOL + ' */' + EOL + EOL + ;
      '/*' + EOL + ;
      ' * Harbour Project source code:' + EOL + ;
      ' * National Collation Support Module ( $1 )' + EOL + ;
      ' *' + EOL + ;
      ' * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>' + EOL + ;
      ' * www - http://www.harbour-project.org' + EOL + ;
      ' *' + EOL + ;
      ' * This file is generated automatically by cpinfo.prg' + EOL + ;
      ' */' + EOL + ;
      '#include "hbapicdp.h"' + EOL + EOL + ;
      '#define HB_CP_RAW       &s_codePage' + EOL + EOL + ;
      'static const unsigned char s_flags[ 256 ] = { $f };' + EOL + ;
      'static const unsigned char s_upper[ 256 ] = { $u };' + EOL + ;
      'static const unsigned char s_lower[ 256 ] = { $l };' + EOL + ;
      'static const unsigned char s_sort [ 256 ] = { $s };' + EOL + ;
      EOL + ;
      'static HB_CODEPAGE s_codePage =' + EOL + ;
      '{' + EOL + ;
      '   "$1",'        + EOL + ;
      '   "$2",'        + EOL + ;
      '   HB_UNITB_$3,' + EOL + ;
      '   s_flags,'     + EOL + ;
      '   s_upper,'     + EOL + ;
      '   s_lower,'     + EOL + ;
      '   s_sort,'      + EOL + ;
      '   NULL,'        + EOL + ;
      '   0,' + EOL + '   0,' + EOL + '   0,' + EOL + ;
      '   NULL,' + EOL + '   NULL,' + EOL + '   NULL,' + EOL + ;
      '};' + EOL + EOL + ;
      '/* include CP registration code */' + EOL + ;
      '#include "hbcdpreg.h"' + EOL

   cDef := strtran( cDef, "$f", a2def( flags ) )
   cDef := strtran( cDef, "$u", a2def( upper ) )
   cDef := strtran( cDef, "$l", a2def( lower ) )
   cDef := strtran( cDef, "$s", a2def( sort ) )
   cDef := strtran( cDef, "$1", id )
   cDef := strtran( cDef, "$2", info )
   cDef := strtran( cDef, "$3", unicode )
return cDef

func a2def( a )
   local i, cData := ""
   for i := 1 to len( a )
      cData += iif( i == 1, "", "," ) + ltrim( str( a[ i ] ) )
   next
return cData
