/* encoding: cp850 */

#pragma DebugInfo=Off


/***
*  fileio2.prg
*
*  COPIA ADAPTADA DO fileio.prg
*  Marcos Augusto 16/11/94
*/

#include "common.ch"
#include "fileio.ch"


/* TRANSFORMAR FUNCOES GLOBAIS EM FUNCOES ESTATICAS, PARA EVITAR REDEFINICOES */

******************
FUNCTION FileSize2( nHandle )
******************
   LOCAL nCurrent
   LOCAL nLength

   // Get file position
   nCurrent := FilePos2( nHandle )

   // Get file length
   nLength := FSEEK( nHandle, 0, FS_END )

   // Reset file position
   FSEEK( nHandle, nCurrent )

   RETURN ( nLength )


/***
*
*  FilePos2( <nHandle> ) --> nPos
*
*  Report the current position of the file pointer in a binary file
*
*/
FUNCTION FilePos2( nHandle )
   RETURN ( FSEEK( nHandle, 0, FS_RELATIVE ) )

/***
*
*  FileTop2( <nHandle> ) --> nPos
*
*  Position the file pointer to the first byte in a binary file and return
*  the new file position (i.e., 0).
*
*/
FUNCTION FileTop2( nHandle )
     RETURN ( FSEEK( nHandle, 0 ) )


/***
*
*  FileBottom2( <nHandle> ) --> nPos
*
*  Position the file pointer to the last byte in a binary file and return
*  the new file position
*
*/
FUNCTION FileBottom2( nHandle )
   RETURN ( FSEEK( nHandle, 0, FS_END ) )


*****************
FUNCTION FReadLn2( nHandle, nLines, nLineLength, cDelim )
*****************
* Modificações:
*   - Nome modificado para FREADLN2()
*   - Permite retroceder no arquivo texto
*   - Retorna vetor de linhas (na ordem em que foram lidas)

   LOCAL nFileSize2     // The size of the file
   LOCAL nChrsToRead    // Number of character to read
   LOCAL nChrsRead      // Number of characters actually read
   LOCAL cBuffer        // buffer do tamanho de 1 bloco
   LOCAL cLinha         // 1 a n blocos de texto (contém ao menos 1 linha real)
   LOCAL vLinhas := {}  // Return value, the lines read
   LOCAL nCount         // Counts number of lines read
   LOCAL nEOLPos        // Position of EOL in cBuffer
   LOCAL bLerBloco      // define ações para ler 1 bloco e adicionar à cLinha
   LOCAL bEOLPos        // define a posição do delimitador na cLinha
   LOCAL bExtrai1       // define a extração de 1 linha real de dentro de cLinha
                        // caso exista somente 1 linha real em cLinha
   LOCAL bExtrai2       // define a extração de 1 linha real de dentro de cLinha
                        // caso exista mais de 1 linha real em cLinha
   LOCAL bPosiciona     // define o posicionamento do ponteiro no início de
                        // uma linha real
   LOCAL bContinua      // define a continuação da leitura
   DEFAULT nLines      TO 1
   DEFAULT nLineLength TO 80
   DEFAULT cDelim      TO (CHR(13)+CHR(10))

   nFileSize2:= FileSize2( nHandle )
   cBuffer   := SPACE( nLineLength )

   IF nLines > 0
      * Avançar no arquivo texto
      bLerBloco  := {|| nChrsToRead := MIN(nLineLength,nFileSize2-FilePos2(nHandle)) ,;
                        nChrsRead   := FREAD(nHandle,@cBuffer,nChrsToRead) ,;
                        cLinha      := cLinha+LEFT(cBuffer,nChrsRead) }
      bEOLPos    := {|| AT(cDelim,cLinha)                    }
      bExtrai1   := {|| cLinha                               }
      bExtrai2   := {|| LEFT(cLinha,nEOLPos-1)               }
      bPosiciona := {|| -(LEN(cLinha)-nEOLPos+1-LEN(cDelim)) }
      bContinua  := {|| FilePos2(nHandle) < nFileSize2         }
   ELSEIF nLines < 0
      * Retroceder no arquivo texto
      bLerBloco  := {|| nChrsToRead := MIN(nLineLength,FilePos2(nHandle)) ,;
                        FSEEK(nHandle,-nChrsToRead,FS_RELATIVE) ,;
                        nChrsRead   := FREAD(nHandle,@cBuffer,nChrsToRead) ,;
                        FSEEK(nHandle,-nChrsRead  ,FS_RELATIVE) ,;
                        cLinha     := LEFT(cBuffer,nChrsRead)+cLinha }
      bEOLPos    := {|| RAT(cDelim,LEFT(cLinha,LEN(cLinha)-LEN(cDelim)))      }
      bExtrai1   := {|| STRTRAN(cLinha,cDelim,"")                             }
      bExtrai2   := {|| STRTRAN(SUBSTR(cLinha,nEOLPos+LEN(cDelim)),cDelim,"") }
      bPosiciona := {|| nEOLPos+LEN(cDelim)-1                                 }
      bContinua  := {|| FilePos2(nHandle) > 0                                 }
   ELSEIF nLines == 0
      bContinua  := {|| .F.                                                   }
   ENDIF
   *
   nCount  := 1
   DO WHILE nCount <= ABS(nLines) .AND. EVAL(bContinua)
      cLinha := ""
      EVAL(bLerBloco)
      DO WHILE (nEOLPos := EVAL(bEOLPos)) == 0 .AND. nChrsToRead # 0
         EVAL(bLerBloco)
      ENDDO
      IF nEOLPos == 0
         AADD(vLinhas,EVAL(bExtrai1))
      ELSE
         AADD(vLinhas,EVAL(bExtrai2))
         FSEEK( nHandle, EVAL(bPosiciona), FS_RELATIVE )
      ENDIF
      nCount++
   ENDDO
   RETURN ( vLinhas )

****************
