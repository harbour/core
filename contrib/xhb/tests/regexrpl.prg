/* Copyright 2006 Francesco Saverio Giudice <info/at/fsgiudice.com> */

#require "xhb"

PROCEDURE Main()

   LOCAL cString, cRegex, cReplace

   ? "*** hb_regexReplace() test ***"
   ?
   ? "A simple replace, return is a single match, without submatches."
   ? "Using 1 to retrieve matches"
   cRegex   := "aaa"
   cReplace := "999"
   ? "String:", cString := "aaabbbcccddd111222333aaabbbcccddd111222333"
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 1 )
   ?
   ? "A replace with a capturing match, return is a submatch."
   ? "Using 2 to retrieve 1st submatches"
   cRegex   := "(aaa)"
   cReplace := "999"
   ? "String:", cString := "aaabbbcccddd111222333aaabbbcccddd111222333"
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multiline string searching text that is on single line."
   cRegex   := "(?im)this (.*) a"
   cReplace := "<IT WORKS!>"
   ? "String:", cString := "Hi folks! This is a real" + hb_eol() + "multiline text. Try this as a test."
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multiline string searching text that is splitted on more lines."
   cRegex   := "(?ims)<text>(.*?)</text>"
   cReplace := "<IT WORKS!>"
   ? "String:", cString := ;
      "Hi all. <text>This is a" + hb_eol() + ;
      "multiline text.</text> Try this as a test" + hb_eol() + ;
      "with <text>another line of text</text>."
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   ?
   ? "Replacing a multifield string."
   cRegex   := "(?:\/(\w+)=)([\w.@]+)"
   cReplace := "<IT WORKS!>"
   ? "String:", cString := "/C=IT/O=Example/OU=example.org/CN=GIUDICE_FRANCESCO_SAVERIO/email=info@example.org"
   ? "Result:", hb_regexReplace( cRegEx, cString, cReplace,,,, 2 )
   ?
   WAIT

   RETURN
