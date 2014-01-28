/*
 * xHarbour Project test code:
 * hb_regexReplace( cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch ) --> cReturn
 *
 * Copyright 2006 Francesco Saverio Giudice <info/at/fsgiudice.com>
 * www - http://www.xharbour.org
 *
 */

#require "xhb"

PROCEDURE Main()

   LOCAL cString, cRegex, cReplace

   ? "*** hb_regexReplace() test ***"
   ?
   ? "A simple replace, return is a single match, without submatches."
   ? "Using 1 to retrieve matches"
   cString  := "aaabbbcccddd111222333aaabbbcccddd111222333"
   cRegex   := "aaa"
   cReplace := "999"
   ? "String:", cString
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 1 )
   ?
   ? "A replace with a capturing match, return is a submatch."
   ? "Using 2 to retrieve 1st submatches"
   cString  := "aaabbbcccddd111222333aaabbbcccddd111222333"
   cRegex   := "(aaa)"
   cReplace := "999"
   ? "String:", cString
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multiline string searching text that is on single line."
   cString  := "Hi folks! This is a real" + hb_eol() + "multiline text. Try this as a test."
   cRegex   := "(?im)this (.*) a"
   cReplace := "<IT WORKS!>"
   ? "String:", cString
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multiline string searching text that is splitted on more lines."
   cString  := "Hi all. <text>This is a" + hb_eol() + ;
      "multiline text.</text> Try this as a test" + hb_eol() + ;
      "with <text>another line of text</text>."
   cRegex   := "(?ims)<text>(.*?)</text>"
   cReplace := "<IT WORKS!>"
   ? "String:", cString
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multifield string."
   cString  := "/C=IT/O=xHarbour/OU=www.xharbour.com/CN=GIUDICE_FRANCESCO_SAVERIO/email=info@fsgiudice.com"
   cRegex   := "(?:\/(\w+)=)([\w.@]+)"
   cReplace := "<IT WORKS!>"
   ? "String:", cString
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   RETURN
