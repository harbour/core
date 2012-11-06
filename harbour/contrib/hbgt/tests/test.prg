/*
 * $Id$
 */

#require "hbgt"

PROCEDURE Main()

   ? 'gt_AscPos( "Harbour", 1 )                                =>', gt_AscPos( "Harbour", 1 )
   ? 'gt_AtDiff( "This Is Harbour", "This Is Clipper" )        =>', gt_AtDiff( "This Is Harbour", "This Is Clipper" )
   ? 'gt_CharEven( "The_Power_Of_Harbour" )                    =>', gt_CharEven( "The_Power_Of_Harbour" )
   ? 'gt_CharOdd( "The_Power_Of_Harbour" )                     =>', gt_CharOdd( "The_Power_Of_Harbour" )
   ? 'gt_ChrCount( "s", "she sells shells by the sea shore" )  =>', gt_ChrCount( "s", "she sells shells by the sea shore" )
   ? 'gt_ChrTotal( "sl", "she sells shells by the sea shore" ) =>', gt_ChrTotal( "sl", "she sells shells by the sea shore" )
   ? 'gt_CharMix( "CLIPPER", "harbour" )                       =>', gt_CharMix( "CLIPPER", "harbour" )
   ? 'gt_AsciiSum( "harbour" )                                 =>', gt_AsciiSum( "harbour" )
   ? 'gt_ChrFirst( "Ho", "the power of Harbour" )              =>', gt_ChrFirst( "Ho", "the power of Harbour" )
   ? 'gt_StrCount( "the", "the cat sat on the mat" )           =>', gt_StrCount( "the", "the cat sat on the mat" )
   ? 'gt_StrCSPN( "this is a test", "as " )                    =>', gt_StrCSPN( "this is a test", "as " )
   ? 'gt_StrCSPN( "this is a test", "elnjpq" )                 =>', gt_StrCSPN( "this is a test", "elnjpq" )
   ? 'gt_StrDiff( "the cat", "the rat" )                       =>', gt_StrDiff( "the cat", "the rat" )
   ? 'gt_StrExpand( "HARBOUR", 2, "-" )                        =>', gt_StrExpand( "HARBOUR", 2, "-" )
   ? 'gt_StrLeft( "this is a test", "hsit " )                  =>', gt_StrLeft( "this is a test", "hsit " )
   ? 'gt_StrPBRK( "this is a test", "sa " )                    =>', gt_StrPBRK( "this is a test", "sa " )
   ? 'gt_StrRight( "this is a test", "teas " )                 =>', gt_strright( "this is a test", "teas " )

   RETURN
