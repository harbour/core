/*
 * $Id$
 */

/*

This routine is to check Harbour source code versions and will work
in conjunction with file "entries.ini" which list the location for
files named "entries" in each CVS-repository sub directory.

To suit with anyone environment, "entries.ini" should be edited to
list the correct locations of files referred to above. To do this,
simply do this as an example :

  C:
  CD\HARBOUR
  DIR entries /s > entries.ini

Then edit as necessary, so "entries.ini" will only consists of file
names with full paths. Once everythings are OK, just issue :

  hversion.exe  [ > version.log ]

*/

#define IF_BUFFER 65516
#define CRLF      chr(13) + chr(10)
********************************************************************************
function main( )

local aentries := afilltext( "entries.ini" )
local i, y := len( aentries )
local j, m, t, lok
local aitems, cversion, cdir

set( _SET_EXACT, .T. )

for i := 1 to y
   if file( aentries[i] )
      cdir := lower( aentries[i] )
      lok  := .f.
      m    := len ( aitems := afilltext( aentries[i] ) )
      for t := 1 to m
         if !empty( cversion := parse_item( aitems[t] ) )
            if !lok
               outstd( cdir := strtran( cdir,"\cvs\entries","" ) + CRLF )
               outstd( replicate( "-", 54 ) + CRLF )
               outstd( padr( "File Name", 20) + padr("Version", 10 ) + "Last Change" + CRLF )
               outstd( replicate( "=", 54 ) + CRLF )
               lok := .t.
            endif
            outstd( cversion + CRLF )
         endif
      next
      outstd(CRLF)
   endif
next

return ( nil )

********************************************************************************
function afilltext( cfilename )

local aretvalue := {}, nhandle
local nPos,cfile, _13 := chr(13)

if (nhandle := fopen( cfilename )) <> -1
   cfile := space( IF_BUFFER )
   while fread(nHandle, @cFile, IF_BUFFER) > 0
      while (npos := at(_13,cfile)) > 1
         aadd( aretvalue, alltrim(left(cfile, npos - 1)) )
         cfile := substr(cfile, npos + 1)
      enddo
      cFile := space(IF_BUFFER)
      fread(nHandle, @cFile, IF_BUFFER)
   enddo
   fclose(nhandle)
endif

return( aretvalue )

********************************************************************************
function parse_item(s)

local aresult := {}
local cresult := ""
local t, l

while( s <> "" )
   if !empty( t := strtoken(s, 1,"/", @l) )
      aadd(aresult, t)
   endif
   s := substr(s, l + 2)
end

if len( aresult ) >= 3
   cresult := padr( aresult[1], 20 ) + padr( aresult[2], 10 ) + aresult[3]
endif

return ( cresult )
