/*
 * $Id$
 */

//*******************************************************************
// fnutil.prg: A fil‚ n‚v kezel‚ssel kapcsolatos vegyes utilitik.
// 1999, Csisz r Levente

/*
   1999.05.10, Csisz r Levente
   
      - dirFName() fggv‚ny.
      
   1998.09.08, Csisz r Levente

      - Indul¢ v ltozat, az afn az futil.prg-b“l  tv‚ve.

*/

/********************************************************************
Tartalma:

   - afn(fileDir,fileName)
      - ™sszef–zi a fileDir-t ‚s a fileName-t egy fil‚n‚vbe.
         afn("BEVONAL","KI216001.021")  --> "BEVONAL\KI216001.021"
         afn("BEVONAL\","KI216001.021") --> "BEVONAL\KI216001.021"
         afn("A:","KI216001.021")       --> "A:KI216001.021"
         afn("","KI216001.021")         --> "KI216001.021"

   - addFPath(fileDir,pathName)
     Ha a pathName-ben nem abszol£t path van, akkor a fileDir-t
     el‚teszi.

   - extractFName( filename )  // file.ext --> file
     Megadja a filename kieg‚sz¡t“ n‚lkli r‚sz‚t.

   - baseFName( filename )  // path\file.ext --> file.ext
     Megadja a filename nev‚t a path n‚lkl.

   - splitFName( pathName )  
     "path\file.ext" --> {"path\","file.ext"}
     A pathName-t felbontja path-ra ‚s name-re.

     Pl.: 
        "proba.prg" --> {"","proba.prg"}
        "a:proba.prg" --> {"a:","proba.prg"}
        "dir\proba.prg" --> {"dir\","proba.prg"}
        "dir/proba.prg" --> {"dir/","proba.prg"}


   - changeFExt(filename,kieg)
     A kieg‚sz¡t“t lecser‚li a kieg-re.
     Pl. addKieg("haz.msk",".say") --> "haz.say"
     Pl. addKieg("haz.",".say")    --> "haz.say"

   - addFExt(filename,kieg)
     Ha nincs kieg‚sz¡t“, akkor a kieg-et hozz adja.
     Pl. addEKieg("haz.msk",".say") --> "haz.msk"
     Pl. addEKieg("haz",".say")     --> "haz.say"

********************************************************************/
function afn(fileDir,fileName)
   if (empty(fileDir))
      return fileName
   elseif (right(filedir,1)==":" .or.;
           right(filedir,1)=="\" .or.;
           right(filedir,1)=="/")
      return fileDir+fileName
   endif
return fileDir+"\"+fileName

*********************************************************************
function addFPath(fileDir,pathName)
// Ha a pathName-ben nem abszol£t path van, akkor a fileDir-t
// el‚teszi.
   if (empty(fileDir))
      return pathName
   elseif (":"$pathName .or. left(pathName,1)$"\/")
      return pathName
   endif
return afn(fileDir,pathName)

*********************************************************************
static function findRev(str,charSet)
// A charSet-ben lev“ karaktereket keresi visszafel‚ az str-ben
// Ret. pos, ha tal lt, 0, ha nem.
local i,w

   i:=len(str)
   while (i>0)
      if (0!=(w:=at(substr(str,i,1),charSet)))
         return i
      endif
      i--
   end for
return 0

*********************************************************************
function extractFName( filename )  // file.ext --> file
// Megadja a filename kieg‚sz¡t“ n‚lkli r‚sz‚t.
local i
   if( empty(filename) )
       return ""
   end
   i:=findRev(fileName,".:\/")
   if (i==0)
      return fileName
   endif
   if (substr(fileName,i,1)=='.')
      return left(fileName,i-1)
   endif
return fileName

*********************************************************************
function baseFName( filename )  // path\file.ext --> file.ext
// Megadja a filename nev‚t a path n‚lkl.
local i
   if( empty(filename) )
       return ""
   end
   i:=findRev(fileName,":\/")
   if (i==0)
      return fileName
   endif
return substr(fileName,i+1)

*********************************************************************
function dirFName( filename )  // path\file.ext --> path
// Megadja a path-t a filename ‚s a z r¢ '\' n‚lkl.
local i
   if( empty(filename) )
       return ""
   end
   i:=findRev(fileName,":\/")
   if (i==0)
      return ""
   endif
return substr(fileName,1,i-1)

*********************************************************************
function splitFName( pathName )  
// "path\file.ext" --> {"path\","file.ext"}
// A pathName-t felbontja path-ra ‚s name-re.
/*
   Pl.: 
      "proba.prg" --> {"","proba.prg"}
      "a:proba.prg" --> {"a:","proba.prg"}
      "dir\proba.prg" --> {"dir\","proba.prg"}
      "dir/proba.prg" --> {"dir/","proba.prg"}
*/
local i

   if( empty(pathName) )
       return {"",""}
   end
   i:=findRev(pathName,":\/")
   if (i==0)
      return {"",pathName}
   endif
return {left(pathName,i),substr(pathName,i+1)}

*********************************************************************
function changeFExt(filename,kieg)
// A kieg‚sz¡t“t lecser‚li a kieg-re.
// Pl. addKieg("haz.msk",".say") --> "haz.say"
// Pl. addKieg("haz.",".say")    --> "haz.say"
local w
   w:=ExtractFName(filename)
   filename:=w+kieg
return filename

*********************************************************************
function addFExt(filename,kieg)
// Ha nincs kieg‚sz¡t“, akkor a kieg-et hozz adja.
// Pl. addEKieg("haz.msk",".say") --> "haz.msk"
// Pl. addEKieg("haz",".say")     --> "haz.say"
local w
   w:=ExtractFName(filename)
   if (w==filename)
      filename:=w+kieg
   endif

return filename

*********************************************************************
