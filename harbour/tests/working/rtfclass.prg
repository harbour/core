//
// $Id$
//

/*
 * harbour rtfclass demo
 * notes : - raw enough but it works
           - using hb_f*() - some compilers are not friendly with this :(
           - rtf is assumed to have association
 * initial release : 23 June 1999 Andi Jahja
 * this program compiles fine on Borland C/C++ 5.0
 * tested under Windows 98 only with RTF associated to Winword
 * works with printable ascii only
 * placed in the public domain
*/

#define CRLF CHR(13) + CHR(10)

function main()

local ortf  := trtf():new("test.rtf")
local htest := fcreate( "rtf_test.txt")
local ctest := ""

// create a plain text file
ctest += "This is +bHarbour © RTF Class-b" + CRLF
ctest += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" + CRLF
ctest += "+bTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-b" + CRLF
ctest += "+iTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-i" + CRLF
ctest += "+buTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bu" + CRLF
ctest += "+buiTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bui" + CRLF
ctest += "THE +bQUICK-b +buBROWN-bu +buiFOX-bui +iJUMPS-i +uOVER-u +ilTHE-il +uLAZY-u +buDOG-bu" + CRLF

fwrite( htest, ctest )
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
   oclass:adddata( "nhandle" )
   oclass:addmethod( "new",  @new() )
   oclass:addmethod( "write", @write() )
   oclass:addmethod( "close", @close() )
   oclass:create()
endif
return oclass:instance()

static function new( cfilename )
local self := qself()
::nhandle   = fcreate( cfilename )
fwrite( ::nhandle,;
    "{\rtf1\ansi\deff0{\fonttbl {\f0\fnil\fcharset0 Courier New;}{\f1\fnil\fcharset0 Arial;}}"+;
    "\uc1\pard\lang1033\ulnone\f0\fs20"+CRLF)
return self

static function write( csource )
local self := qself()
local cchar, cline, xatt, i, _xatt
local n, nchar, xchar, y
// These are character attributes, self-defined
// + means a turn-on
// - means a turn-off
local attrib := {;
   { "+b"  , "\b "            } /* turn bold on*/                   ,;
   { "+bu" , "\ul\b "         } /* turn bold_underline on */        ,;
   { "+bi" , "\b\i "          } /* turn bold_italic on */           ,;
   { "+bui", "\ul\b\i "       } /* turn bold_underline_italic on */ ,;
   { "+i"  , "\i "            } /* turn italic on */                ,;
   { "+il" , "\ul\i "         } /* turn italic_underline on */      ,;
   { "+u"  , "\ul "           } /* turn underline on */             ,;
   { "-b"  , "\b0 "           } /* turn bold off */                 ,;
   { "-bu" , "\b0\ulnone "    } /* turn bold_underline off */       ,;
   { "-bi" , "\b0\i0 "        } /* turn bold_italic off */          ,;
   { "-bui", "\b0\i0\ulnone " } /* turn bold_underline_italic off */,;
   { "-i"  , "\i0 "           } /* turn italic off */               ,;
   { "-il" , "\ulnone\i0 "    } /* turn italic_underline off */     ,;
   { "-u"  , "\ulnone "       } /* turn underline off */ }

hb_fuse( csource )  // open source file
while !hb_feof()    // read the file line by line
   cline := hb_freadln() + "\par"
   y     := len( cline )
   for nchar := 1 to y
       cchar := substr( cline, nchar, 1 )

       // todo : i need function dec2hex()
       // to convert ascii to 2-characters hex
       // ie   : dec2hex( "H" ) -> 48
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
   hb_fskip() // read next line
enddo
hb_fuse()
return ( self )

static function close()
local self := qself()
fwrite( ::nhandle, "\f1\fs16\par"+CRLF+"}" )
fclose( ::nhandle )

return self
