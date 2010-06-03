/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for GT full screen color output
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

function main( xBlink )
local bg, fg, n

CLS
?
? OS(), VERSION(), DATE(), TIME()
? HB_GTVERSION(), HB_GTVERSION(1)
?
inkey( 0 )
setblink( empty( xBlink ) )
for bg := 0 to 15
    for fg := 0 to 15
        n := bg * 16 + fg
        @ 5 + bg, 5 + fg * 4 say "["+NUM2HEX(n)+"]" color NTOCOLOR( n )
    next
next
?
?
while inkey(0)!=13; enddo
return nil

static function NTOCOLOR(nClr)
return ltrim( str( int( nClr % 16 ), 2 ) ) + "/" + ;
       ltrim( str( int( nClr / 16 ), 2 ) )

static function NUM2HEX(nVal)
local cHex := "", i, n
for i := 1 to 2
    n := nVal % 16
    cHex := chr( n + iif( n > 9, 55, 48 ) ) + cHex
    nVal := int( nVal / 16 )
next
return cHex
