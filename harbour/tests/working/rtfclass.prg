//
// $Id$
//

/*
harbour rtfclass demo
notes : - raw enough but it works
        - using hb_f*()
        - rtf is assumed to have association
initial release : 23 June 1999 Andi Jahja
*/

#define CRLF CHR(13) + CHR(10)

function main()

local ortf  := trtf():new("test.rtf")
local htest := fcreate( "rtf_test.txt")

// create a plain text file
fwrite( htest, "This is +bHarbour © RTF Class-b" + CRLF )
fwrite( htest, "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" + CRLF )
fwrite( htest, "+bTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-b" + CRLF )
fwrite( htest, "+iTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-i" + CRLF )
fwrite( htest, "+buTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bu" + CRLF )
fwrite( htest, "+buiTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bui" + CRLF )
fwrite( htest, "THE +bQUICK-b +buBROWN-bu +buiFOX-bui +iJUMPS-i +uOVER-u +ilTHE-il +uLAZY-u +buDOG-bu" + CRLF )
fclose( htest )

// convert text file to rtf
ortf:write("rtf_test.txt")
ortf:close()

// execute file association ( windows only )
if lower( os() ) == "windows"
   // assuming start.exe is exist
   __run( "start test.rtf" )
endif
return nil

function trtf()
static oclass

if oclass == nil
   oclass = tclass():new( "trtf" )
   oclass:adddata( "filename" )
   oclass:adddata( "nhandle" )
   oclass:addmethod( "new",  @new() )
   oclass:addmethod( "write", @write() )
   oclass:addmethod( "close", @close() )
   oclass:create()
endif
return oclass:instance()

static function new( cfilename )
local self := qself()
::filename = cfilename
::nhandle   = fcreate( cfilename )
fwrite( ::nhandle,;
    "{\rtf1\ansi\deff0{\fonttbl {\f0\fnil\fcharset0 Courier New;}{\f1\fnil\fcharset0 Arial;}}"+CRLF +;
    "\uc1\pard\lang1033\ulnone\f0\fs20"+CRLF)
return self

static function write( csource )
local self := qself()
local cchar, cline, xatt, i, _xatt
local n, nchar, xchar, y
// These are character attributes
local attrib := {;
   { "+b"  , "\b "            },;
   { "+bu" , "\ul\b "         },;
   { "+bi" , "\b\i "          },;
   { "+bui", "\ul\b\i "       },;
   { "+i"  , "\i "            },;
   { "+il" , "\ul\i "         },;
   { "+u"  , "\ul "           },;
   { "-b"  , "\b0 "           },;
   { "-bu" , "\b0\ulnone "    },;
   { "-bi" , "\b0\i0 "        },;
   { "-bui", "\b0\i0\ulnone " },;
   { "-i"  , "\i0 "           },;
   { "-il" , "\ulnone\i0 "    },;
   { "-u"  , "\ulnone "       } }

hb_fuse( csource )  // open source file
while !hb_feof()    // read the file line by line
   cline := hb_freadln() + "\par"
   y     := len( cline )
   for nchar := 1 to y
       cchar := substr( cline, nchar, 1 )

       // TODO : I Need Function DEC2HEX()
       // To Convert Numeric To 2-Characters HEX
       // ie   : DEC2HEX( "H" ) -> 48
       if cchar == "+" .or. cchar == "-"
          xatt := cchar                         + ;
                  substr( cline, nchar + 1, 1 ) + ;
                  substr( cline, nchar + 2, 1 ) + ;
                  substr( cline, nchar + 3, 1 )
          if ( i := ascan( attrib, { |e| e[1] ==  xatt } ) ) > 0
             fwrite( ::nhandle, attrib[i][2] )
             nchar := nchar + len( xatt ) - 1
          else
             // 3 attributes
             xatt := left( xatt, 3 )
             if ( i := ascan( attrib, { |e| e[1] == xatt } ) ) > 0
                fwrite( ::nhandle, attrib[i][2] )
                nchar := nchar + len( xatt ) - 1
             else
                // 2 attributes
                xatt := left( xatt, 2 )
                if ( i := ascan( attrib, { |e| e[1] == xatt } ) ) > 0
                   fwrite( ::nhandle, attrib[i][2] )
                   nchar := nchar + len( xatt ) - 1
                else
                   fwrite( ::nhandle, cchar )
                endif
             endif
          endif
      else
         fwrite( ::nhandle, cchar )
      endif
   next
   fwrite( ::nhandle, CRLF )
   hb_fskip() // next line
enddo
hb_fuse()
return ( self )

static function close()
local self := qself()
fwrite( ::nhandle, "\f1\fs16\par"+CRLF+"}" )
fclose( ::nhandle )

return self
