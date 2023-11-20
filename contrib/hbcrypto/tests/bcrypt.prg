/* Copyright 2015 Viktor Szakats */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   HBTEST Lower( hb_StrToHex( hb_bcrypt( "password"                     , "salt"                     ,  4, 32 ) ) ) IS "5bbf0cc293587f1c3635555c27796598d47e579071bf427e9d8fbe842aba34d9"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "password"                     , hb_BChar( 0 )              ,  4, 16 ) ) ) IS "c12b566235eee04c212598970a579a67"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( hb_BChar( 0 )                  , "salt"                     ,  4, 16 ) ) ) IS "6051be18c2f4f82cbf0efee5471b4bb9"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "password" + hb_BChar( 0 )     , "salt" + hb_BChar( 0 )     ,  4, 32 ) ) ) IS "7410e44cf4fa07bfaac8a928b1727fac001375e7bf7384370f48efd121743050"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "pass" + hb_BChar( 0 ) + "wor" , "sa" + hb_BChar( 0 ) + "l" ,  4, 16 ) ) ) IS "c2bffd9db38f6569efef4372f4de83c0"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "pass" + hb_BChar( 0 ) + "word", "sa" + hb_BChar( 0 ) + "lt",  4, 16 ) ) ) IS "4ba4ac3925c0e8d7f0cdb6bb1684a56f"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "password"                     , "salt"                     ,  8, 64 ) ) ) IS "e1367ec5151a33faac4cc1c144cd23fa15d5548493ecc99b9b5d9c0d3b27bec76227ea66088b849b20ab7aa478010246e74bba51723fefa9f9474d6508845e8d"
   HBTEST Lower( hb_StrToHex( hb_bcrypt( "password"                     , "salt"                     , 42, 16 ) ) ) IS "833cf0dcf56db65608e8f0dc0ce882bd"

   RETURN
