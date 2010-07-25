/*
 * $Id$
 */

// directory test
PROCEDURE Main( filespec, attribs, cshort )

   LOCAL adir := {}
   LOCAL x := 0, lShort := .f.

   IF !cshort == NIL .and. (Upper( cShort ) == "TRUE" .or. Upper( cShort ) == ".T.")
      lShort := .t.
   ENDIF

// adir := asort( directory(filespec,attribs,lShort),,, {|x,y|upper(x[1]) < upper(y[1])} )
   adir := directory(filespec,attribs,lShort)

   SET CENTURY ON

   FOR x := 1 TO len( adir )
      outstd(hb_eol())
      outstd(padr(adir[x,1], 20), "|", ;
             transform(adir[x,2], "9,999,999,999"), "|", ;
             adir[x,3], "|", ;
             adir[x,4], "|", ;
             adir[x,5])
   NEXT

   RETURN
