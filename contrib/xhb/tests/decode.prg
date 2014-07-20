/* Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com> */

#require "xhb"

PROCEDURE Main()

   LOCAL aArray

   ? "hb_Decode() function tests"

   ? hb_Decode( 10 )                                   // Single, return empty value
   ? hb_Decode( 3, 1, "A", 2, "B", 3, "C" )            // A list
   ? hb_Decode( 4, 1, "A", 2, "B", 3, "C", "X" )       // A list with default
   ? hb_Decode( 2, { 1, "A", 2, "B", 3, "C" } )        // Using an array as list of values to check
   ? hb_Decode( 2, { 1, "A", 2, "B", 3, "C", "X" } )   // Using an array with default as list of values to check
   ? hb_Decode( 2, { 1 => "A", 2 => "B", 3 => "C" } )  // Using an hash as list
   ? hb_CStr( hb_Decode( 2, 1, {|| 1 }, 2, {|| 2 }, 3, {|| 3 } ) )  // Returning a codeblock

   aArray := { 1 }
   ? hb_Decode( aArray, aArray, "A", { 2 }, "B", { 3 }, "C" )  // Checking an array

   RETURN
