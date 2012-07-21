/*
 * $Id$
 */

//-------------------------\\

#include             "hbvpdf.ch"

//-------------------------\\

function Main()

   local nWidth, nTab, nI, nJ, nK, nCol, nRow, aStyle, aFonts
   local nTop, nLeft, nBottom, nRight, cText, oPdf
   local aColor := { ;
   "FF0000", "8B0000", "800000", "FF4500", "D2691E", "B8860B", "FF8C00", "FFA500", "DAA520", "808000", "FFD700", "FFFF00", "ADFF2F", "9ACD32", "7FFF00", "7CFC00", "00FF00", "32CD32", "008000", "006400",;
   "66CDAA", "7FFFD4", "87CEFA", "87CEEB", "F0F8FF", "E0FFFF", "B0C4DE", "B0E0E6", "AFEEEE", "ADD8E6", "8FBC8F", "90EE90", "98FB98", "00FA9A", "00FF7F", "3CB371", "2E8B57", "228B22", "556B2F", "6B8E23",;
   "5F9EA0", "40E0D0", "48D1CC", "00CED1", "20B2AA", "008B8B", "008080", "2F4F4F", "00BFFF", "00FFFF", "00FFFF", "0000FF", "0000CD", "00008B", "000080", "1E90FF", "4169E1", "4682B4", "6495ED", "7B68EE",;
   "C71585", "FF1493", "FF00FF", "FF00FF", "9370DB", "DDADDD", "DB7093", "FF69B4", "DA70D6", "EE82EE", "BA55D3", "9932CC", "8A2BE2", "9400D3", "8B008B", "800080", "4B0082", "191970", "483D8B", "6A5ACD",;
   "DC143C", "B22222", "A52A2A", "CD5C5C", "FA8072", "E9967A", "FFA07A", "F5DEB3", "FFDEAD", "EEE8AA", "FFDAB9", "FFE4C4", "FFEFD5", "FFE4E1", "FFE4B5", "D2B48C", "DEB887", "F0E68C", "BDB76B", "F4A460",;
   "FDF5E6", "FFF8DC", "FAF0E6", "FAFAD2", "FFFACD", "FFEBCD", "FFFFE0", "FAEBD7", "FFF5EE", "FFF0F5", "D8BFD8", "FFC0CB", "FFB6C1", "BC8F8F", "F08080", "FF7F50", "FF6347", "8B4513", "A0522D", "CD853F",;
   "FFFAFA", "FFFFF0", "E6E6FA", "FFFAF0", "F8F8FF", "F0FFF0", "F5F5DC", "F0FFFF", "F5FFFA", "708090", "778899", "F5F5F5", "DCDCDC", "D3D3D3", "C0C0C0", "A9A9A9", "808080", "696969", "000000", "FFFFFF"}

   set date format "YYYY/MM/DD"

   aStyle := { "Normal", "Bold", "Italic", "BoldItalic" }

   aFonts := { { "Times",     .t., .t., .t., .t. }, ;
               { "Helvetica", .t., .t., .t., .t. }, ;
               { "Courier",   .t., .t., .t., .t. }  }

   oPdf := tPdf():New( 'test.pdf', 200, .t. )
   oPdf:EditOnHeader()
   oPdf:Image( 'files\color.tif', 0, 0, "M" )
   oPdf:EditOffHeader()
   oPdf:SaveHeader( 'test.hea' )
   oPdf:CloseHeader()
   oPdf:BookOpen()
   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Grids & Borders", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "Simple Grid",     2, oPdf:aReport[ REPORTPAGE ], 0 )

   for nI := 0 to 792 step 36
       oPdf:Box( nI, 0, nI, 612, 0.01, , "D" )
   next
   for nI := 0 to 612 step 36
       oPdf:Box( 0, nI, 792, nI, 0.01, , "D" )
   next


   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "10 dots border ", 2, 2, 0 )
   oPdf:Box( 0, 0, 792, 612, 10, , "D" )

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Boxes", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "Boxes", 2, oPdf:aReport[ REPORTPAGE ], 0 )

   nRow := 85
   nCol := 85
   oPdf:Box( nRow     , ( nCol * 2 )     , ( nRow * 3 )     , ( nCol * 4 )     , 1.00,  15, "D" )
   oPdf:Box( nRow + 10, ( nCol * 2 ) + 10, ( nRow * 3 ) + 10, ( nCol * 4 ) + 10, 0.50,  25, "D" )
   oPdf:Box( nRow + 20, ( nCol * 2 ) + 20, ( nRow * 3 ) + 20, ( nCol * 4 ) + 20, 0.25,  35, "D" )
   oPdf:Box( nRow + 30, ( nCol * 2 ) + 30, ( nRow * 3 ) + 30, ( nCol * 4 ) + 30, 0.15,  45, "D" )
   oPdf:Box( nRow + 40, ( nCol * 2 ) + 40, ( nRow * 3 ) + 40, ( nCol * 4 ) + 40, 0.10,  55, "D" )
   oPdf:Box( nRow + 50, ( nCol * 2 ) + 50, ( nRow * 3 ) + 50, ( nCol * 4 ) + 50, 0.05,  65, "D" )
   oPdf:Box( nRow + 60, ( nCol * 2 ) + 60, ( nRow * 3 ) + 60, ( nCol * 4 ) + 60, 0.01,  75, "D" )
   oPdf:Box( nRow + 70, ( nCol * 2 ) + 70, ( nRow * 3 ) + 70, ( nCol * 4 ) + 70, 0.01,  85, "D" )
   oPdf:Box( nRow + 80, ( nCol * 2 ) + 80, ( nRow * 3 ) + 80, ( nCol * 4 ) + 80, 0.01,  95, "D" )
   oPdf:Box( nRow + 90, ( nCol * 2 ) + 90, ( nRow * 3 ) + 90, ( nCol * 4 ) + 90, 0.01, 100, "D" )

   for nI := 1 to 7
      nRow := 150 + nI * 10
      for nJ := 1 to 20
          nCol := nJ * 10 - 3
          oPdf:Box( nRow, nCol, nRow + 10, nCol + 10, 0.01, nI * 10,"M", chr(253) + chr( cton( substr( aColor[ ( nI - 1 ) * 20 + nJ ], 1, 2 ), 16) ) + chr( cton( substr( aColor[ ( nI - 1 ) * 20 + nJ ], 3, 2 ), 16) ) + chr( cton( substr( aColor[ ( nI - 1 ) * 20 + nJ ], 5, 2 ), 16) ) )
      next
   next

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Color Boxes ", 2, oPdf:aReport[ REPORTPAGE ], 0 )
   for nI := 1 to 140
       nTop    := ( nI - 1 ) * 2.4
       nLeft   := ( nI - 1 ) * 2.1
       nBottom := oPdf:aReport[ PAGEY ] - ( nI - 1 ) * 2.47
       nRight  := oPdf:aReport[ PAGEX ] - ( nI - 1 ) * 2.18
       oPdf:Box1( nTop, nLeft, nBottom, nRight, 10, chr( cton( substr( aColor[ nI ], 1, 2 ), 16) ) + chr( cton( substr( aColor[ nI ], 3, 2 ), 16) ) + chr( cton( substr( aColor[ nI ], 5, 2 ), 16) ))
   next

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Memos", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "Different Styles & Colors", 2, oPdf:aReport[ REPORTPAGE ], 0 )
   nWidth := 90
   nTab   := 0
   cText  := memoread('files\test.txt')

   oPdf:Text( cText,  28, 107.95, nWidth, nTab, 3, 'M', chr(253) + chr(0) + chr(0) + chr(255) )
   oPdf:Text( cText,  58, 107.95, nWidth, nTab, 2, 'M', chr(253) + chr(0) + chr(255) + chr(0) )
   oPdf:Text( cText,  88, 107.95, nWidth, nTab, 1, 'M', chr(253) + chr(255) + chr(0) + chr(0) )
   oPdf:Text( cText, 118, 107.95 - nWidth / 2, nWidth, nTab, 4, 'M', chr(253) + chr(255) + chr(255) + chr(0) )

   oPdf:Text( cText,  34, 100,    nWidth, nTab, 3, 'R', chr(253) + chr(0) + chr(128) + chr(128) )//, pdfTextCount( memoread('files\test.txt'),  33, 100,    nWidth, nTab, 3, 'R' )
   oPdf:Text( cText,  41, 100,    nWidth, nTab, 2, 'R', chr(253) + chr(0) + chr(191) + chr(255) )//, pdfTextCount( memoread('files\test.txt'),  40, 100,    nWidth, nTab, 2, 'R' )
   oPdf:Text( cText,  48, 100,    nWidth, nTab, 1, 'R', chr(253) + chr(244) + chr(164) + chr(96) )//, pdfTextCount( memoread('files\test.txt'),  47, 100,    nWidth, nTab, 1, 'R' )
   oPdf:Text( cText,  55,  35,    nWidth, nTab, 4, 'R', chr(253) + chr(0) + chr(0) + chr(0) )//, pdfTextCount( memoread('files\test.txt'),  54,  35,    nWidth, nTab, 4, 'R' )

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Fonts", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "Different Styles", 2, oPdf:aReport[ REPORTPAGE ], 0 )
   nK := 6
   for nI := 1 to len( aFonts )  // Fonts
      ++nk
      for nJ := 1 to 4           // Styles
         if aFonts[ nI ][ nJ + 1 ]
            oPdf:SetFont( aFonts[ nI ][ 1 ], nJ - 1, oPdf:aReport[ FONTSIZE ] )
            oPdf:RJust("This is a test for " + aFonts[ nI ][ 1 ] + " " + ;
                       aStyle[ nJ ], nK++, oPdf:aReport[ REPORTWIDTH ], "R")
         endif
      next
    oPdf:RJust(oPdf:Underline("Underline"), nK++, oPdf:aReport[ REPORTWIDTH ], "R")
    oPdf:RJust(oPdf:Reverse("Test"), nK, oPdf:aReport[ REPORTWIDTH ], "R")
   next

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Pictures", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "TIFF", 2, oPdf:aReport[ REPORTPAGE ], 0 )
   //             file,    row, col, units, height, width
   oPdf:Image( 'files\color.tif', 0,   0,   "M" )
   oPdf:RJust( oPdf:Underline("TIFF"), nK++, oPdf:aReport[ REPORTWIDTH ], "R")

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "JPEG", 2, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:Image( 'files\color.jpg', 0, 0, "M" ) // file, row, col, units, height, width
   oPdf:RJust(oPdf:Underline("JPEG"), nK, oPdf:aReport[ REPORTWIDTH ], "R")

   oPdf:OpenHeader('test.hea')

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Headers", 1, oPdf:aReport[ REPORTPAGE ], 0 )
   oPdf:BookAdd( "Picture Header Page 8", 2, oPdf:aReport[ REPORTPAGE ], 0 )

   oPdf:AtSay( chr(253) + chr(255) + chr(0) + chr(0) + 'Red Sample of header repeating on pages 8-10', 1, 20, "R" )

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Picture Header Page 9", 2, oPdf:aReport[ REPORTPAGE ], 0 )

   oPdf:AtSay( chr(253) + chr(0) + chr(255) + chr(0) + 'Green Sample of header repeating on pages  8-10', 1, 20, "R" )

   oPdf:NewPage( "LETTER", "P", 6 )
   oPdf:BookAdd( "Picture Header Page 10", 2, oPdf:aReport[ REPORTPAGE ], 0 )

   oPdf:AtSay( chr(253) + chr(0) + chr(0) + chr(255) + 'Blue Sample of header repeating on pages  8-10', 1, 20, "R" )

   oPdf:Close()

#ifndef __XPP__
   oPdf:Execute( 'test.pdf' )
#endif

   // oPdf:FilePrint()

return nil

//-------------------------\\
//
// This function called only used in tstPdf.prg
//
static function cton( cString, nBase )
local cTemp, nI, cChar, n := 0, nLen

   nLen := len( cString )
   cTemp := ""
   for nI := nLen to 1 step -1
       cTemp += substr( cString, nI, 1 )
   next
   cTemp := upper( cTemp )

   for nI := 1 to nLen
      cChar := substr( cTemp, nI, 1 )
      if .not. IsDigit( cChar )
         n := n + ((Asc( cChar ) - 65) + 10) * ( nBase ^ ( nI - 1 ) )
      else
         n := n + (( nBase ^ ( nI - 1 )) * val( cChar ))
      endif
   next

return n

//-------------------------\\
