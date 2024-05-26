// Office example 'doc1'
// From \contrib\hboffice\tests
// ..\..\..\bin\win\mingw64\hbmk2 doc1.prg hboffice.hbc -comp=mingw64
// ../../../bin/linux/gcc/hbmk2 doc1.prg hboffice.hbc
#include "hboffice.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
STAT FUNCTION FULL_PATH( C_FileName )
***********************************
#if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
    return DiskName() + ":\" + CurDir() + "\result\" + C_FileName
#else
    return "/" + CurDir() + "/result/" + C_FileName
#endif

***********************************
STAT FUNCTION OFFICE_ERROR( C_Text )
***********************************
LOCAL N_Err := HBOFFICE_LAST_ERROR()
LOCAL C_Err := HBOFFICE_ERROR_STR(N_Err)
LOCAL L_Err := .F.

IF N_Err != HBOFFICE_RES_OK
    ? C_Text + ": " + C_Err
    L_Err = .T.
ENDIF

RETURN L_Err

***********************************
PROC DOC_LINE(O_DOC, C_Line)
***********************************
HBOFFICE_DOC_INSERT_TEXT(O_DOC, C_Line)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)

***********************************
PROCEDURE Main()
***********************************
LOCAL O_DOC := NIL
LOCAL N_Page := 0
LOCAL N_Col, N_Row
LOCAL C_File := FULL_PATH("doc1.odt")
LOCAL C_PDF := FULL_PATH("doc1.pdf")

// Total chars by line
LOCAL N_Width := 80

// Field tabulations
LOCAL V_Tabs := {45, HBOFFICE_HALIGN_LEFT, 17, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT}
LOCAL C_Line := ""

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

HBOFFICE_INIT()

? "Creating office example: " + C_File

O_DOC := HBOFFICE_DOC_CREATE()

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

// Header, footer and page margins config
HBOFFICE_DOC_PAGE_HEADER_SHOW(O_DOC, .T.)
HBOFFICE_DOC_PAGE_HEADER_MARGINS(O_DOC, 254, 280, 508, 102, .T., .T.)
HBOFFICE_DOC_PAGE_FOOTER_SHOW(O_DOC, .T.)
HBOFFICE_DOC_PAGE_FOOTER_MARGINS(O_DOC, 354, 380, 608, 202, .T., .T.)
HBOFFICE_DOC_PAGE_MARGINS(O_DOC, 2819, 178, 1270, 533, 0)

// Create and empty header with some line breaks
HBOFFICE_DOC_TEXT_SPACE(O_DOC, HBOFFICE_TEXT_SPACE_HEADER)
HBOFFICE_DOC_FONT_FAMILY(O_DOC, "Times New Roman")
HBOFFICE_DOC_FONT_SIZE(O_DOC, 10.0)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)

// Set the text space to main page
HBOFFICE_DOC_TEXT_SPACE(O_DOC, HBOFFICE_TEXT_SPACE_PAGE)
HBOFFICE_DOC_FONT_FAMILY(O_DOC, "Courier New")
HBOFFICE_DOC_FONT_SIZE(O_DOC, 10.0)
HBOFFICE_DOC_PARAGRAPH_LSPACING(O_DOC, 330)
DOC_LINE(O_DOC, "Ceará")
DOC_LINE(O_DOC, "Governo Municipal de Piquet Carneiro")
DOC_LINE(O_DOC, "Consolidado")
DOC_LINE(O_DOC, "         T E R M O    D E    C O N F E R Ê N C I A    D E    C A I X A")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "       Nesta data, em  cumprimento às exigências estabelecidas por Lei, o abaixo")
DOC_LINE(O_DOC, "assinado, tesoureiro do(a)  Prefeitura  Municipal de Piquet Carneiro, procedeu à")
DOC_LINE(O_DOC, "verificação dos valores   existentes   no   Caixa   deste(a)  Prefeitura,  tendo")
DOC_LINE(O_DOC, "encontrado o seguinte:")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "a) CAIXA____:  R$ 0,00 (Zero Real).")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "b) BANCOS___:  R$ 12.794.126,25 (Doze  Milhões,  Setecentos  e  Noventa e Quatro")
DOC_LINE(O_DOC, "               Mil, Cento e Vinte e Seis Reais e Vinte e Cinco Centavos).")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "c) EXATORES.:  R$ 0,00 (Zero Real).")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "       Os valores acima  mencionados  foram  verificados  por  mim à vista do(a)")
DOC_LINE(O_DOC, "Sr.(a). PREFEITO MUNICIPAL  que  também  assina o presente e achado certo com os")
DOC_LINE(O_DOC, "registros do Livro  de  Caixa  existente nesta Tesouraria, nesta data, no ato do")
DOC_LINE(O_DOC, "encerramento do expediente.")
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "Visto:                                   Piquet Carneiro, 31 de Janeiro de 2023.")
HBOFFICE_DOC_FONT_SIZE(O_DOC, 6.0)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
HBOFFICE_DOC_INSERT_TEXT(O_DOC, "                          ")
HBOFFICE_DOC_INSERT_DASH(O_DOC, 38)
HBOFFICE_DOC_INSERT_TEXT(O_DOC, "   ")
HBOFFICE_DOC_INSERT_DASH(O_DOC, 38)
HBOFFICE_DOC_INSERT_NEW_LINE(O_DOC)
DOC_LINE(O_DOC, "                                 BISMARCK BARROS BEZERRA             WEYNE CESAR MACHADO DO NASCIMENTO")

// Save the document
HBOFFICE_DOC_SAVE(O_DOC, C_File)
IF OFFICE_ERROR("Erro ao salvar o documento")
    RETURN
ENDIF

// Save the pdf
HBOFFICE_DOC_PDF(O_DOC, C_PDF)
IF OFFICE_ERROR("Erro ao salvar PDF")
    RETURN
ENDIF

// Close the document (mandatory)
HBOFFICE_DOC_CLOSE(O_DOC)
IF OFFICE_ERROR("Erro ao fechar o documento")
    RETURN
ENDIF

? "Documento criado com sucesso."

HBOFFICE_FINISH()
RETURN
