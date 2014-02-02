#require "hbvpdf"

PROCEDURE Main()

   LOCAL aReport
   LOCAL nWidth, nTab, nI, nJ, nK, nCol, nRow, aStyle, aFonts, aFont
   LOCAL nTop, nLeft, nBottom, nRight, cTestFile
   LOCAL aColor := { ;
      "FF0000", "8B0000", "800000", "FF4500", "D2691E", "B8860B", "FF8C00", "FFA500", "DAA520", "808000", "FFD700", "FFFF00", "ADFF2F", "9ACD32", "7FFF00", "7CFC00", "00FF00", "32CD32", "008000", "006400", ;
      "66CDAA", "7FFFD4", "87CEFA", "87CEEB", "F0F8FF", "E0FFFF", "B0C4DE", "B0E0E6", "AFEEEE", "ADD8E6", "8FBC8F", "90EE90", "98FB98", "00FA9A", "00FF7F", "3CB371", "2E8B57", "228B22", "556B2F", "6B8E23", ;
      "5F9EA0", "40E0D0", "48D1CC", "00CED1", "20B2AA", "008B8B", "008080", "2F4F4F", "00BFFF", "00FFFF", "00FFFF", "0000FF", "0000CD", "00008B", "000080", "1E90FF", "4169E1", "4682B4", "6495ED", "7B68EE", ;
      "C71585", "FF1493", "FF00FF", "FF00FF", "9370DB", "DDADDD", "DB7093", "FF69B4", "DA70D6", "EE82EE", "BA55D3", "9932CC", "8A2BE2", "9400D3", "8B008B", "800080", "4B0082", "191970", "483D8B", "6A5ACD", ;
      "DC143C", "B22222", "A52A2A", "CD5C5C", "FA8072", "E9967A", "FFA07A", "F5DEB3", "FFDEAD", "EEE8AA", "FFDAB9", "FFE4C4", "FFEFD5", "FFE4E1", "FFE4B5", "D2B48C", "DEB887", "F0E68C", "BDB76B", "F4A460", ;
      "FDF5E6", "FFF8DC", "FAF0E6", "FAFAD2", "FFFACD", "FFEBCD", "FFFFE0", "FAEBD7", "FFF5EE", "FFF0F5", "D8BFD8", "FFC0CB", "FFB6C1", "BC8F8F", "F08080", "FF7F50", "FF6347", "8B4513", "A0522D", "CD853F", ;
      "FFFAFA", "FFFFF0", "E6E6FA", "FFFAF0", "F8F8FF", "F0FFF0", "F5F5DC", "F0FFFF", "F5FFFA", "708090", "778899", "F5F5F5", "DCDCDC", "D3D3D3", "C0C0C0", "A9A9A9", "808080", "696969", "000000", "FFFFFF" }

   aReport := pdfInit()

   aStyle := { "Normal", "Bold", "Italic", "BoldItalic" }

   aFonts := { ;
      { "Times",     .T., .T., .T., .T. }, ;
      { "Helvetica", .T., .T., .T., .T. }, ;
      { "Courier",   .T., .T., .T., .T. } }

   pdfOpen( "test.pdf", 200, .T. )

   pdfEditOnHeader()
   pdfImage( "files" + hb_ps() + "color.tif", 0, 0, "M" ) // file, row, col, units, height, width
   pdfEditOffHeader()
   pdfSaveHeader( "test.hea" )
   pdfCloseHeader()

   pdfBookOpen()

   pdfNewPage( "LETTER", "P", 6 )

   pdfBookAdd( "Grids and Borders", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "Simple Grid", 2, aReport[ REPORTPAGE ], 0 )

   FOR nI := 0 TO 792 STEP 36
      pdfBox( nI, 0, nI, 612, 0.01, , "D" )
   NEXT
   FOR nI := 0 TO 612 STEP 36
      pdfBox( 0, nI, 792, nI, 0.01, , "D" )
   NEXT

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "10 dots border ", 2, aReport[ REPORTPAGE ], 0 )
   pdfBox( 0, 0, 792, 612, 10, , "D" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Boxes", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "Boxes", 2, aReport[ REPORTPAGE ], 0 )

   nRow := 85
   nCol := 85
   pdfBox( nRow +  0, ( nCol * 2 ) +  0, ( nRow * 3 ) +  0, ( nCol * 4 ) +  0, 1.00,  15, "D" )
   pdfBox( nRow + 10, ( nCol * 2 ) + 10, ( nRow * 3 ) + 10, ( nCol * 4 ) + 10, 0.50,  25, "D" )
   pdfBox( nRow + 20, ( nCol * 2 ) + 20, ( nRow * 3 ) + 20, ( nCol * 4 ) + 20, 0.25,  35, "D" )
   pdfBox( nRow + 30, ( nCol * 2 ) + 30, ( nRow * 3 ) + 30, ( nCol * 4 ) + 30, 0.15,  45, "D" )
   pdfBox( nRow + 40, ( nCol * 2 ) + 40, ( nRow * 3 ) + 40, ( nCol * 4 ) + 40, 0.10,  55, "D" )
   pdfBox( nRow + 50, ( nCol * 2 ) + 50, ( nRow * 3 ) + 50, ( nCol * 4 ) + 50, 0.05,  65, "D" )
   pdfBox( nRow + 60, ( nCol * 2 ) + 60, ( nRow * 3 ) + 60, ( nCol * 4 ) + 60, 0.01,  75, "D" )
   pdfBox( nRow + 70, ( nCol * 2 ) + 70, ( nRow * 3 ) + 70, ( nCol * 4 ) + 70, 0.01,  85, "D" )
   pdfBox( nRow + 80, ( nCol * 2 ) + 80, ( nRow * 3 ) + 80, ( nCol * 4 ) + 80, 0.01,  95, "D" )
   pdfBox( nRow + 90, ( nCol * 2 ) + 90, ( nRow * 3 ) + 90, ( nCol * 4 ) + 90, 0.01, 100, "D" )

   FOR nI := 1 TO 7
      nRow := 150 + nI * 10
      FOR nJ := 1 TO 20
         nCol := nJ * 10 - 3
         pdfBox( nRow, nCol, nRow + 10, nCol + 10, 0.01, nI * 10, "M", ;
            hb_BChar( 253 ) + ;
            hb_BChar( CToN( SubStr( aColor[ ( nI - 1 ) * 20 + nJ ], 1, 2 ), 16 ) ) + ;
            hb_BChar( CToN( SubStr( aColor[ ( nI - 1 ) * 20 + nJ ], 3, 2 ), 16 ) ) + ;
            hb_BChar( CToN( SubStr( aColor[ ( nI - 1 ) * 20 + nJ ], 5, 2 ), 16 ) ) )
      NEXT
   NEXT

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Color Boxes ", 2, aReport[ REPORTPAGE ], 0 )
   FOR nI := 1 TO 140
      nTop := ( nI - 1 ) * 2.4
      nLeft := ( nI - 1 ) * 2.1
      nBottom := aReport[ PAGEY ] - ( nI - 1 ) * 2.47
      nRight := aReport[ PAGEX ] - ( nI - 1 ) * 2.18
      pdfBox1( nTop, nLeft, nBottom, nRight, 10, ;
         hb_BChar( CToN( SubStr( aColor[ nI ], 1, 2 ), 16 ) ) + ;
         hb_BChar( CToN( SubStr( aColor[ nI ], 3, 2 ), 16 ) ) + ;
         hb_BChar( CToN( SubStr( aColor[ nI ], 5, 2 ), 16 ) ) )
   NEXT

   pdfNewPage( "LETTER", "P", 6 )
   pdfBox1( 0, 0, 100, 200, 10, hb_BChar( 0 ) + hb_BChar( 255 ) + hb_BChar( 0 ), hb_BChar( 255 ) + hb_BChar( 255 ) + hb_BChar( 0 ) )
   pdfBookAdd( "Memos", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "Different Styles and Colors", 2, aReport[ REPORTPAGE ], 0 )
   nWidth := 90
   nTab := 0

   cTestFile := MemoRead( "files" + hb_ps() + "test.txt" )

   pdfText( cTestFile,  28, 107.95, nWidth, nTab, 3, "M", hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 255 ) )   // ,              pdfTextCount( MemoRead("test.txt"),  28, 107.95, nWidth, nTab, 3, "M" )
   pdfText( cTestFile,  58, 107.95, nWidth, nTab, 2, "M", hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 255 ) + hb_BChar( 0 ) )   // ,              pdfTextCount( MemoRead("test.txt"),  58, 107.95, nWidth, nTab, 2, "M" )
   pdfText( cTestFile,  88, 107.95, nWidth, nTab, 1, "M", hb_BChar( 253 ) + hb_BChar( 255 ) + hb_BChar( 0 ) + hb_BChar( 0 ) )   // ,              pdfTextCount( MemoRead("test.txt"),  88, 107.95, nWidth, nTab, 1, "M" )
   pdfText( cTestFile, 118, 107.95 - nWidth / 2, nWidth, nTab, 4, "M", hb_BChar( 253 ) + hb_BChar( 255 ) + hb_BChar( 255 ) + hb_BChar( 0 ) ) // , pdfTextCount( MemoRead("test.txt"), 118, 107.95 - nWidth / 2, nWidth, nTab, 4, "M" )

   pdfText( cTestFile,  34, 100,    nWidth, nTab, 3, "R", hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 128 ) + hb_BChar( 128 ) )  // , pdfTextCount( MemoRead("test.txt"),  33, 100,    nWidth, nTab, 3, "R" )
   pdfText( cTestFile,  41, 100,    nWidth, nTab, 2, "R", hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 191 ) + hb_BChar( 255 ) )  // , pdfTextCount( MemoRead("test.txt"),  40, 100,    nWidth, nTab, 2, "R" )
   pdfText( cTestFile,  48, 100,    nWidth, nTab, 1, "R", hb_BChar( 253 ) + hb_BChar( 244 ) + hb_BChar( 164 ) + hb_BChar( 96 ) ) // , pdfTextCount( MemoRead("test.txt"),  47, 100,    nWidth, nTab, 1, "R" )
   pdfText( cTestFile,  55,  35,    nWidth, nTab, 4, "R", hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) )      // , pdfTextCount( MemoRead("test.txt"),  54,  35,    nWidth, nTab, 4, "R" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Fonts", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "Different Styles", 2, aReport[ REPORTPAGE ], 0 )
   nK := 6
   FOR EACH aFont IN aFonts
      ++nK
      FOR nJ := 1 TO 4
         IF aFont[ nJ + 1 ]
            pdfSetFont( aFont[ 1 ], nJ - 1, aReport[ FONTSIZE ] )
            pdfRJust( "0123456789 This is a test for " + aFont[ 1 ] + " " + ;
               aStyle[ nJ ], nK++, aReport[ REPORTWIDTH ], "R" )
         ENDIF
      NEXT
      pdfRJust( pdfUnderline( "Underline" ), nK++, aReport[ REPORTWIDTH ], "R" )
      pdfRJust( pdfReverse( "Test" ), nK, aReport[ REPORTWIDTH ], "R" )
   NEXT

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Pictures", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "TIFF", 2, aReport[ REPORTPAGE ], 0 )
   pdfImage( "files" + hb_ps() + "color.tif", 0, 0, "M" ) // file, row, col, units, height, width
   pdfRJust( pdfUnderline( "TIFF" ), nK++, aReport[ REPORTWIDTH ], "R" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "JPEG", 2, aReport[ REPORTPAGE ], 0 )
   pdfImage( "files" + hb_ps() + "color.jpg", 0, 0, "M" ) // file, row, col, units, height, width
   pdfRJust( pdfUnderline( "JPEG" ), nK, aReport[ REPORTWIDTH ], "R" )

   pdfOpenHeader( "test.hea" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Headers", 1, aReport[ REPORTPAGE ], 0 )
   pdfBookAdd( "Picture Header Page 8", 2, aReport[ REPORTPAGE ], 0 )

   // version 0.01
   pdfAtSay( hb_BChar( 253 ) + hb_BChar( 255 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + "Red Sample of header repeating on pages 8-10", 1, 20, "R" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Picture Header Page 9", 2, aReport[ REPORTPAGE ], 0 )

   // version 0.01
   pdfAtSay( hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 255 ) + hb_BChar( 0 ) + "Green Sample of header repeating on pages  8-10", 1, 20, "R" )

   pdfNewPage( "LETTER", "P", 6 )
   pdfBookAdd( "Picture Header Page 10", 2, aReport[ REPORTPAGE ], 0 )

   // version 0.01
   pdfAtSay( hb_BChar( 253 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 255 ) + "Blue Sample of header repeating on pages  8-10", 1, 20, "R" )

   pdfClose()

STATIC FUNCTION CToN( cString, nBase )

   LOCAL cTemp, nI, cChar, n := 0, nLen

   nLen := Len( cString )
   cTemp := ""
   FOR nI := nLen TO 1 STEP - 1
      cTemp += SubStr( cString, nI, 1 )
   NEXT
   cTemp := Upper( cTemp )

   FOR nI := 1 TO nLen
      cChar := SubStr( cTemp, nI, 1 )
      IF ! IsDigit( cChar )
         n += ( nBase ^ ( nI - 1 ) ) * ( ( Asc( cChar ) - 65 ) + 10 )
      ELSE
         n += ( nBase ^ ( nI - 1 ) ) * Val( cChar )
      ENDIF
   NEXT

   RETURN n
