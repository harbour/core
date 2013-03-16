
#require "hbgt"
#require "hbtest"

PROCEDURE Main()

   HBTEST gt_AscPos( "Harbour", 1 )                                IS 72
   HBTEST gt_AtDiff( "This Is Harbour", "This Is Clipper" )        IS 9
   HBTEST gt_CharEven( "The_Power_Of_Harbour" )                    IS "h_oe_fHror"
   HBTEST gt_CharOdd( "The_Power_Of_Harbour" )                     IS "TePwrO_abu"
   HBTEST gt_ChrCount( "s", "she sells shells by the sea shore" )  IS 7
   HBTEST gt_ChrTotal( "sl", "she sells shells by the sea shore" ) IS 11
   HBTEST gt_CharMix( "CLIPPER", "harbour" )                       IS "ChLaIrPbPoEuRr"
   HBTEST gt_AsciiSum( "harbour" )                                 IS 755
   HBTEST gt_ChrFirst( "Ho", "the power of Harbour" )              IS 111
   HBTEST gt_StrCount( "the", "the cat sat on the mat" )           IS 2
   HBTEST gt_StrCSPN( "this is a test", "as " )                    IS 3
   HBTEST gt_StrCSPN( "this is a test", "elnjpq" )                 IS 11
   HBTEST gt_StrDiff( "the cat", "the rat" )                       IS "rat"
   HBTEST gt_StrExpand( "HARBOUR", 2, "-" )                        IS "H--A--R--B--O--U--R"
   HBTEST gt_StrLeft( "this is a test", "hsit " )                  IS 8
   HBTEST gt_StrPBRK( "this is a test", "sa " )                    IS "s is a test"
   HBTEST gt_StrRight( "this is a test", "teas " )                 IS 8

   RETURN
