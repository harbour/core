function Main()
   local i := TIniFile():New('harbour.ini')
   qout(i:readstring('test', 'hello', 'not found'))
   qout(i:readstring('not', 'there', 'not found'))

   i:writestring('not', 'there', 'there now!')
   qout(i:readstring('not', 'there', 'not found'))

return nil

function TIniFile()
   static oClass

   if oClass == nil
      oClass := TClass():New( "TINIFILE" ) // starts a new class definition

      oClass:AddData( "FileName" )           // define this class objects datas
      oClass:AddData( "Contents" )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "ReadString", @ReadString() )
      oClass:AddMethod( "WriteString", @WriteString() )

      oClass:Create()                     // builds this class
   endif
return oClass:Instance()                  // builds an object of this class

static function New(cFileName)
   local Self := QSelf()
   local hFile, cFile, cLine, cIdent, nPos
   local CurrArray

   if empty(cFileName)
      // raise an error?
      outerr('No filename passed to TIniFile():New()')
      return nil

   else
      ::FileName := cFilename
      ::Contents := {}
      CurrArray := ::Contents

      //if Hb_File(cFileName)
         hFile := fopen(cFilename, 0)

      //else
      //   hFile := fcreate(cFilename)
      //endif

      cFile := space(255)
      do while (nPos := fread(hFile, @cFile, 255)) > 0

         do while !Empty(cFile)
            if (nPos := At(Chr(13), cFile)) > 1
               cLine := alltrim(Left(cFile, nPos - 1))
               cFile := substr(cFile, nPos + 1)

            else
               cLine := cFile
               cFile := ''
            endif

            if !Empty(cLine)
               if Left(cLine, 1) == '[' // new section
                  if (nPos := At(']', cLine)) > 1
                     cLine := substr(cLine, 2, nPos - 2);
           
                  else
                     cLine := substr(cLine, 2)
                  endif

                  AAdd(::Contents, { cLine, { /* this will be CurrArray */ } } )
                  CurrArray := ::Contents[Len(::Contents)][2]

               elseif Left(cLine, 1) <> ';' // comment
                  if (nPos := At('=', cLine)) > 0
                     cIdent := Left(cLine, nPos - 1)
                     cLine := SubStr(cLine, nPos + 1)
                  
                     AAdd( CurrArray, { cIdent, cLine } )
     
                  else
                     AAdd( CurrArray, { cLine, '' } )
                  endif
               endif
            endif
         end

         cFile := space(255)
         fread(hFile, cFile, 255)
      end
      fclose(hFile)
   endif
return Self

static function ReadString(cSection, cIdent, cDefault)
   local Self := QSelf()
   local cResult := cDefault
   local j, i := AScan( ::Contents, {|x| x[1] == cSection} )

   if i > 0
      j := AScan( ::Contents[i][2], {|x| x[1] == cIdent} )

      if j > 0
         cResult := ::Contents[i][2][j][2]
      endif
   endif
return cResult


static procedure WriteString(cSection, cIdent, cString)
   local Self := QSelf()
   local j, i

   if Empty(cIdent)
      outerr('Must specifier an identifier')

   elseif Empty(cSection)
      j := AScan( ::Contents, {|x| x[1] == cIdent} )

      if j > 0
         ::Contents[j][2] := cString

      else
         AAdd(::Contents, {cIdent, cString})
      endif

   elseif (i := AScan( ::Contents, {|x| x[1] == cSection})) > 0
      j := AScan( ::Contents[i][2], {|x| x[1] == cIdent} )

      if j > 0
         ::Contents[i][2][j][2] := cString

      else
         AAdd( ::Contents[i][2], {cIdent, cString} )
      endif

   else
      AAdd( ::Contents, {{cSection, {cIdent, cString}}} )
   endif
return 
