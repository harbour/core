/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Simple program to generate information for Harbour CP module definition.
 *    Compile it with Clipper and link with given national sorting module
 *    (usually ntx*.obj) and then execute to generate letters strings for
 *    given national sorting module. Then use this string to define Harbour
 *    CP module in source/codepage/ directory.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */


proc main()
   local cLo, cUp, c, cl, cu, i, a, lWarn

   set alternate to cpinfo.txt additive
   set alternate on

   a := array( 256 )
   for i := 1 to len( a )
      a[ i ] := i - 1
   next
   asort( a,,, { |x,y| chr( x ) + chr( 0 ) < chr( y ) + chr( 0 ) } )

   ? date(), time(), os(), version()
   ? "Character encoding:"
   ? "==================="
   lWarn := .t.
   for i := 1 to len( a ) - 1
      if a[ i ] > a[ i + 1 ]
         lWarn := .f.
         exit
      endif
   next
   if lWarn
      ? "simple byte sorting !!!"
      lWarn := .f.
   endif
   cLo := cUp := ""
   for i := 1 to len( a )
      if i < len(a) .and. a[i] > a[ i + 1 ] .and. !isalpha( chr( a[ i ] ) )
         ? "non alpha character " + charval( chr( a[ i ] ) ) + ;
           " sorted in non ASCII order !!!"
         lWarn := .t.
      endif
      c := chr( a[ i ] )
      if isalpha( c )
         cl := lower( c )
         cu := upper( c )
         cLo += cl
         cUp += cu
         if cl == cu
            ? "upper " + charval( cu ) + " and lower " + charval( cl ) + ;
              " equal"
            lWarn := .t.
         elseif !isalpha( cl )
            ? "wrongly defined character " + ;
              charval( c ) + ":" + charinfo( c ) + ;
              ", lower " + charval( cl ) + ":" + charinfo( cl )
            lWarn := .t.
         elseif !isalpha( cu )
            ? "wrongly defined character " + ;
              charval( c ) + ":" + charinfo( c ) + ;
              ", upper " + charval( cu ) + ":" + charinfo( cu )
            lWarn := .t.
         endif
      elseif islower( c ) .or. isupper( c )
         ? "wrongly defined character " + ;
           charval( c ) + ":" + charinfo( c )
         lWarn := .t.
      endif
   next
   if lWarn
      ? "Warning: irregular CP which needs special definition in Harbour"
   endif
   ? 'upper: "' + cUp + '"'
   ? 'lower: "' + cLo + '"'
   ? "==================="
   ?
return

static function charval( c )
return "'" + c + "' (" + ltrim( str( asc( c ) ) ) + ")"

static function charinfo( c )
   local cInfo
   cInfo :=   "ISALPHA->" + iif( isalpha( c ), "Y", "N" )
   cInfo += ", ISUPPER->" + iif( isupper( c ), "Y", "N" )
   cInfo += ", ISLOWER->" + iif( islower( c ), "Y", "N" )
return cInfo
