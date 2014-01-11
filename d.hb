
//LOCAL cPayLoad := hb_MemoRead( "upgmail-body.txt" )
//hb_processRun( "openssl smime -sign -binary -signer userkey.pem", @cPayLoad )

local cStdOut := ""

hb_processRun( "sort", "z" + hb_eol() + "a" + hb_eol(), @cStdOut )

? cStdOut
