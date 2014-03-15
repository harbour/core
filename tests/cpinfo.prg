/*
 * Harbour Project source code:
 *    Simple program to generate information for Harbour CP module definition.
 *    Compile it with Clipper and link with given national sorting module
 *    (usually ntx*.obj) and then execute to generate letters strings for
 *    given national sorting module. Then use this string to define Harbour
 *    CP module in src/codepage/ directory.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "fileio.ch"

procedure main( cdp, info, unicode )

   local cUp, cLo, cUp2, cLo2, cOrd, cOrd2, cOrdMix, cMix, c, i, a
   local lWarn, lBin, lSort, lEqual, lMixed, lIsUp, lIsLo

   set alternate to cpinfo.txt additive
   set alternate on

#ifdef __HARBOUR__
   /* for test */
   Set( _SET_CODEPAGE, iif( Empty( cdp ), "PLMAZ", Upper( cdp ) ) )
   hb_SetTermCP( Set( _SET_CODEPAGE ), Set( _SET_CODEPAGE ) )
   lEqual := .f.
#else
   lEqual := .f.
#endif

   a := Array( 256 )
   for i := 1 to Len( a )
      a[ i ] := i - 1
   next
   ASort( a,,, {| x, y | Chr( x ) + Chr( 0 ) < Chr( y ) + Chr( 0 ) } )

   ? Date(), Time(), OS(), Version()
#ifdef __HARBOUR__
   ? "Character encoding:", Set( _SET_CODEPAGE )
#else
   ? "Character encoding:", _natSortVersion()
#endif
   ? Replicate( "=", 50 )
   lSort := .f.
   for i := 1 to Len( a ) - 1
      if a[ i ] > a[ i + 1 ]
         lSort := .t.
         exit
      endif
   next
   if ! lSort
      ? "simple byte sorting !!!"
   endif
   lBin := lWarn := lMixed := .f.
   cUp := cLo := cOrd := ""
   for i := 1 to Len( a )
      if i < Len(a) .and. a[i] > a[ i + 1 ] .and. ! IsAlpha( Chr( a[ i ] ) )
         ? "non alpha character", charval( Chr( a[ i ] ) ), ;
           "sorted in non ASCII order !!!"
         lBin := lWarn := .t.
      endif
      c := Chr( a[ i ] )
      if i < Len(a)
         if c + Chr( 0 ) > Chr( a[ i + 1 ] ) + Chr( 0 )
            ? "character", charis( c ), "is wrongly sorted"
            lBin := lWarn := .t.
         elseif ! lEqual .and. c + Chr( 0 ) = Chr( a[ i + 1 ] ) + Chr( 0 )  /* hb_LeftEq() */
            ? "character", charis( c ), "and", Chr( a[ i + 1 ] ), ;
              "have the same weight"
            lWarn := .t.
         endif
      endif
      cOrd += c
      if IsDigit( c )
         if Asc( c ) < Asc( "0" ) .or. Asc( c ) > Asc( "9" )
            ? "character", charis( c ), "defined as digit"
            lBin := lWarn := .t.
         endif
      elseif Asc( c ) >= Asc( "0" ) .and. Asc( c ) <= Asc( "9" )
         ? "character", charis( c ), "is not defined as digit"
         lBin := lWarn := .t.
      endif
      if IsAlpha( c )
         if IsUpper( c )
            cUp += c
            if IsLower( c )
               ? "character", charis( c ), "defined as upper and lower"
               lWarn := .t.
            endif
            if Lower( c ) == c
               ? "character", charis( c ), ;
                 "is the same as upper and lower"
               lWarn := .t.
            elseif ! IsLower( Lower( c ) )
               ? "character", charis( c ), ;
                 "has lower character", charis( Lower( c ) ), ;
                 "not marked as lower"
               lBin := lWarn := .t.
            endif
         elseif IsLower( c )
            cLo += c
            if IsUpper( c )
               ? "character", charis( c ), "defined as upper and lower"
               lWarn := .t.
            endif
            if Upper( c ) == c
               ? "character", charis( c ), "is the same as upper and lower"
               lWarn := .t.
            elseif ! IsUpper( Upper( c ) )
               ? "character", charis( c ), ;
                 "has upper character", charis( Upper( c ) ), ;
                 "not marked as upper"
               lBin := lWarn := .t.
            endif
         else
            ? "character", charis( c ), "not defined as upper or lower"
            lBin := lWarn := .t.
         endif
      else
         if IsLower( c ) .or. IsUpper( c )
            ? "wrongly defined character", ;
              charval( c ) + ":" + charinfo( c )
            lBin := lWarn := .t.
         endif
         if ! c == Lower( c )
            ? "non alpha character", charis( c ), ;
              "has corresponding lower character", charis( Lower( c ) )
            lBin := lWarn := .t.
         endif
         if ! c == Upper( c )
            ? "non alpha character", charis( c ), ;
              "has corresponding upper character", charis( Upper( c ) )
            lBin := lWarn := .t.
         endif
      endif
   next
   for i := 1 to Len( cUp ) - 1
      c := SubStr( cUp, i, 1 )
      if c + Chr( 0 ) > SubStr( cUp, i + 1, 1 ) + Chr( 0 )
         ? "letter", charis( c ), "is wrongly sorted"
         lBin := lWarn := .t.
      endif
   next
   for i := 1 to Len( cLo ) - 1
      c := SubStr( cLo, i, 1 )
      if c + Chr( 0 ) > SubStr( cLo, i + 1, 1 ) + Chr( 0 )
         ? "letter", charis( c ), "is wrongly sorted"
         lBin := lWarn := .t.
      endif
   next
   cMix := ""
   if ! Len( cUp ) == Len( cLo )
      ? "number of upper and lower characters is different"
      lWarn := .t.
   else
      for i := 1 to Len( cUp )
         cMix += SubStr( cUp, i, 1 )
         cMix += SubStr( cLo, i, 1 )
      next
   endif
   cOrd2 := cOrdMix := ""
   for i := 0 to 255
      if i == Asc( cUp ) .or. i == Asc( cLo )
         if i == Asc( cUp )
            cOrd2 += cUp
         else
            cOrd2 += cLo
         endif
         cOrdMix += cMix
         cMix := ""
      endif
      c := Chr( i )
      if ! c $ cUp .and. ! c $ cLo
         cOrd2 += c
         cOrdMix += c
      endif
   next
   if ! cOrd == cOrd2
      if cUp = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" .and. ;  /* hb_LeftEq() */
         cLo = "abcdefghijklmnopqrstuvwxyz"          /* hb_LeftEq() */
         cUp2 := SubStr( cUp, 27 )
         cLo2 := SubStr( cLo, 27 )
         cOrd2 := ""
         lIsUp := lIsLo := .f.
         for i := 0 to 255
            c := Chr( i )
            if c $ cUp2
               if ! lIsUp
                  cOrd2 += cUp2
                  lIsUp := .t.
                  if lIsLo
                     cOrd2 += cLo2
                  endif
               endif
            elseif c $ cLo2
               if ! lIsLo
                  if lIsUp
                     cOrd2 += cLo2
                  endif
                  lIsLo := .t.
               endif
            else
               cOrd2 += Chr( i )
            endif
         next
         if cOrd == cOrd2
            cUp := cUp2
            cLo := cLo2
         else
            cOrd2 := ""
            for i := 0 to 255
               if i == Asc( cUp2 )
                  cOrd2 += cUp2
               elseif i == Asc( cLo2 )
                  cOrd2 += cLo2
               endif
               c := Chr( i )
               if ! c $ cUp2 .and. ! c $ cLo2
                  cOrd2 += Chr( i )
               endif
            next
            if cOrd == cOrd2
               cUp := cUp2
               cLo := cLo2
               if lSort
                  ? "letters are not sorted continuously" + ;
                     " (padded to 1-st non latin character)"
                  lBin := lWarn := .t.
               endif
            elseif cUp2 >= Chr( 127 ) .and. cLo2 >= Chr( 127 )
               cUp := cUp2
               cLo := cLo2
            endif
         endif
      elseif cOrd == cOrdMix
         ? "letters case are mixed"
         lMixed := .t.
      endif
      if ! cOrd == cOrd2 .and. lSort .and. ! lMixed
         ? "letters are not sorted continuously"
         lBin := lWarn := .t.
      endif
   endif
   if lWarn
      if lBin
         ? "Warning: irregular CP which needs special definition in Harbour"
         ? "         using binary tables generated by this program"
      else
         ? "Warning: irregular CP which needs verification for human"
         ? "         readable definition in Harbour"
      endif
   endif
   ? "      upper:", '"' + cUp + '"'
   ? "      lower:", '"' + cLo + '"'
   if pad_letters( @cUp, @cLo, @lBin )
      ? "HB_CP_UPPER:", '"' + cUp + '"'
      ? "HB_CP_LOWER:", '"' + cLo + '"'
   endif
   ? Replicate( "=", 50 )
   ?

   if ! Empty( cdp )
      write_file( "cp" + Lower( cdp ) + ".c", ;
                  genCP( cdp, info, unicode, lBin, lWarn, lMixed, cUp, cLo ) )
   endif

   return

static function pad_letters( cUp, cLo, lBin )

   local lRet, cUp2, cLo2, cU, cL, i, j

   cUp2 := cLo2 := ""

   i := j := 1
   while i <= Len( cUp ) .or. j <= Len( cLo )
      cU := SubStr( cUp, i, 1 )
      cL := SubStr( cLo, j, 1 )
      if Upper( cL ) == cU .and. Lower( cU ) == cL
         ++i
         ++j
      elseif cL == "" .or. ( ! cU == "" .and. ! IsLower( Lower( cU ) ) )
         cL := " "
         ++i
      elseif cU == "" .or. ! IsUpper( Upper( cL ) )
         cU := " "
         ++j
      elseif Upper( cL ) $ SubStr( cUp, i + 1 )
         cL := Lower( cU )
         ++i
      else
         cU := Upper( cL )
         ++j
      endif
      if ! lBin .and. ( cU == " " .or. cU $ cUp2 ) .and. ;
                      ( cL == " " .or. cL $ cLo2 )
         lBin := .t.
      endif
      cUp2 += cU
      cLo2 += cL
   enddo

   lRet := !( cUp == cUp2 .and. cLo == cLo2 )
   cUp := cUp2
   cLo := cLo2

   return lRet

static function charval( c )
   return "'" + c + "' (" + LTrim( Str( Asc( c ) ) ) + ")"

static function charis( c )
   return "'" + c + "' (" + LTrim( Str( Asc( c ) ) ) + ":" + ;
      iif( IsAlpha( c ), "A", " " ) + ;
      iif( IsUpper( c ), "U", " " ) + ;
      iif( IsLower( c ), "L", " " ) + ;
      iif( IsDigit( c ), "D", " " ) + ")"

static function charinfo( c )
   return ;
      "ISALPHA->" + iif( IsAlpha( c ), "Y", "N" ) + ;
      ", ISUPPER->" + iif( IsUpper( c ), "Y", "N" ) + ;
      ", ISLOWER->" + iif( IsLower( c ), "Y", "N" ) + ;
      ", ISDIGIT->" + iif( IsDigit( c ), "Y", "N" ) + ;
      ", UPPER->'" + Upper( c ) + "'" + ;
      ", LOWER->'" + Lower( c ) + "'"


#ifdef __HARBOUR__
   #include "hbextcdp.ch"
   #define EOL          hb_eol()
#else
   #define EOL          Chr( 13 ) + Chr( 10 )
   #define hb_BLen( s ) Len( s )
#endif

#define HB_CDP_DIGIT    1
#define HB_CDP_ALPHA    2
#define HB_CDP_LOWER    4
#define HB_CDP_UPPER    8

static function write_file( cName, cBody )

   local lRet := .f.
   local hFile

   if ( hFile := FCreate( cName ) ) != F_ERROR
      lRet := FWrite( hFile, cBody ) == hb_BLen( cBody )
      FClose( hFile )
   endif

   return lRet

static function genCP( id, info, unicode, lBin, lWarn, lMixed, cUp, cLo )

   local flags[ 256 ], upper[ 256 ], lower[ 256 ], sort[ 256 ], tmp[ 256 ]
   local i, c

   id := Upper( id )
   if Empty( info )
      info := _NatSortVer()
   endif
   if Empty( unicode )
#ifdef __HARBOUR__
      unicode := hb_cdpUniID()
      if hb_LeftEq( unicode, "cp" )
         unicode := SubStr( unicode, 3 )
      elseif hb_LeftEq( unicode, "iso" ) .or. hb_LeftEq( unicode, "bg-" )
         unicode := SubStr( unicode, 4 )
      endif
      unicode := Upper( StrTran( unicode, "-", "_" ) )
      if hb_LeftEq( unicode, "KAM" ) .or. ;
         hb_LeftEq( unicode, "MAZ" ) .or. ;
         hb_LeftEq( unicode, "MIC" )
         unicode := Left( unicode, 3 )
      endif
#else
      unicode := "437"
#endif
   else
      unicode := Upper( unicode )
   endif

   for i := 1 to 256
      c := Chr( i - 1 )
      flags[ i ] := 0
      if IsDigit( c )
         flags[ i ] += HB_CDP_DIGIT
      endif
      if IsAlpha( c )
         flags[ i ] += HB_CDP_ALPHA
      endif
      if IsUpper( c )
         flags[ i ] += HB_CDP_UPPER
      endif
      if IsLower( c )
         flags[ i ] += HB_CDP_LOWER
      endif
      upper[ i ] := Asc( Upper( c ) )
      lower[ i ] := Asc( Lower( c ) )
      tmp[ i ] := i - 1
   next
   ASort( tmp,,, {| x, y | Chr( x ) + Chr( 0 ) < Chr( y ) + Chr( 0 ) } )
   for i := 1 to 256
      sort[ tmp[ i ] + 1 ] := i - 1
   next

   return genCPfile( id, info, unicode, flags, upper, lower, sort, ;
                     lBin, lWarn, lMixed, cUp, cLo )

static function genCPfile( id, info, unicode, flags, upper, lower, sort, ;
                           lBin, lWarn, lMixed, cUp, cLo )

   local cDef

   cDef := ;
      '/*' + EOL + ;
      ' * Harbour Project source code:' + EOL + ;
      ' * National Collation Support Module ($1)' + EOL + ;
      ' *' + EOL + ;
      ' * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>' + EOL + ;
      ' * www - http://harbour-project.org' + EOL + ;
      ' *' + EOL + ;
      ' * This file is generated automatically by cpinfo.prg' + EOL + ;
      ' */' + EOL + EOL + ;
      '#define HB_CP_ID        $1' + EOL + ;
      '#define HB_CP_INFO      "$2"' + EOL + ;
      '#define HB_CP_UNITB     HB_UNITB_$3' + EOL
   if ! lBin
      cDef += ;
         '#define HB_CP_ACSORT    HB_CDP_ACSORT_NONE' + EOL
      if lMixed
         cDef += '#define HB_CP_CSSORT    HB_CDP_CSSORT_MIXED' + EOL
      endif
#ifdef __HARBOUR__
      cDef += ;
         '#define HB_CP_UPPER     "' + hb_StrToUTF8( cUp ) + '"' + EOL + ;
         '#define HB_CP_LOWER     "' + hb_StrToUTF8( cLo ) + '"' + EOL + ;
         '#define HB_CP_UTF8' + EOL + ;
         EOL
#else
      cDef += ;
         '#define HB_CP_UPPER     "' + cUp + '"' + EOL + ;
         '#define HB_CP_LOWER     "' + cLo + '"' + EOL + ;
         EOL
#endif
      if lWarn
         cDef += ;
            '#if 0 /* TOVERIFY: binary tables */' + EOL
      endif
   endif
   if lBin .or. lWarn
      cDef += ;
         EOL + ;
         '#define HB_CP_RAW' + EOL + EOL + ;
         'static const unsigned char s_flags[ 256 ] = { $f };' + EOL + ;
         'static const unsigned char s_upper[ 256 ] = { $u };' + EOL + ;
         'static const unsigned char s_lower[ 256 ] = { $l };' + EOL + ;
         'static const unsigned char s_sort [ 256 ] = { $s };' + EOL + ;
         EOL
      if ! lBin
         cDef += ;
            '#endif' + EOL + EOL
      endif
   endif
   cDef += ;
      '/* include CP registration code */' + EOL + ;
      '#include "hbcdpreg.h"' + EOL

   cDef := StrTran( cDef, "$f", a2def( flags ) )
   cDef := StrTran( cDef, "$u", a2def( upper ) )
   cDef := StrTran( cDef, "$l", a2def( lower ) )
   cDef := StrTran( cDef, "$s", a2def( sort ) )
   cDef := StrTran( cDef, "$1", id )
   cDef := StrTran( cDef, "$2", info )
   cDef := StrTran( cDef, "$3", unicode )

   return cDef

static function a2def( a )

   local i, cData := ""

   for i := 1 to Len( a )
      cData += iif( i == 1, "", "," ) + LTrim( Str( a[ i ] ) )
   next

   return cData
