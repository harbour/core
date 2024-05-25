// Typical welcome message
// From \contrib\hboffice\tests
// ..\..\..\bin\win\mingw64\hbmk2 sheet1.prg -comp=mingw64

// Replicate this example
// Anexo_01_Dem_da_receita_e_despesa_segundo_as_cat_economicasMA854_u.g._Consolidado__Exercicio_2023.ods

#include "hboffice.ch"

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

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
PROCEDURE Main()
***********************************
LOCAL O_XLS := NIL
LOCAL N_Page := 0
LOCAL N_Col, N_Row

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

HBOFFICE_INIT()

O_XLS := HBOFFICE_XLS_CREATE()

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Generate the spreadsheet data
HBOFFICE_XLS_NAME(O_XLS, N_Page, "DEMOSTRAÇAO RECEITA E DESPESA")
HBOFFICE_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 8000)    // A Width
HBOFFICE_XLS_COLUMN_WIDTH(O_XLS, N_Page, 1, 2200)    // B Width
HBOFFICE_XLS_COLUMN_WIDTH(O_XLS, N_Page, 2, 8000)    // C Width
HBOFFICE_XLS_COLUMN_WIDTH(O_XLS, N_Page, 3, 2200)    // D Width

N_Col := 0
N_Row := 0
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 0)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "BALANÇO GERAL")
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_CENTER)
HBOFFICE_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_VALIGN_CENTER)
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 1
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 1)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Governo Municipal de São Mateus do Maranhão")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 2
N_Row := 1
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 1)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "EXERCÍCIO FINANCEIRO DE 2023")
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_RIGHT)
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 2
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 2)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Consolidado")
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_LEFT)
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 3
N_Row := 2
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Adendo II")
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_LEFT)
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 3
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 2, 3)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Anexo 1, da Lei nº 4320, de 17/03/64. (Portaria SOF nº 8, de 04/02/85)")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 3
N_Row := 3
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Em R$ 1,00")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 5
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 5)
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_CENTER)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "DEMONSTRAÇÃO DA RECEITA E DESPESA")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 6
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 6)
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_CENTER)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "SEGUNDO AS CATEGORIAS ECONÔMICAS")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 8
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 8)
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_CENTER)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "R E C E I T A")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
HBOFFICE_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_RGB(205, 205, 205))

N_Col := 2
N_Row := 8
HBOFFICE_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 8)
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_CENTER)
HBOFFICE_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "D E S P E S A")
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
HBOFFICE_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
HBOFFICE_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_RGB(205, 205, 205))

N_Col := 1
N_Row := 11
HBOFFICE_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_HALIGN_RIGHT)
HBOFFICE_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_VALIGN_CENTER)
HBOFFICE_XLS_CELL_NUMFORMAT(O_XLS, N_Page, N_Col, N_Row, HBOFFICE_NUMFORMAT_INT)
HBOFFICE_XLS_CELL_VALUE(O_XLS, N_Page, N_Col, N_Row, 917038.94)
HBOFFICE_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
HBOFFICE_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 7)

// Protect the sheet
HBOFFICE_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
HBOFFICE_XLS_SAVE(O_XLS, "C:\harbour_nappgui\contrib\hboffice\sheet1.ods")
OFFICE_ERROR("Salvando a planilha")

// Close the spreadsheet (mandatory)
HBOFFICE_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

? "A planilha foi criada com sucesso."

// Open the result into a LibreOffice window
//HBOFFICE_BROWSE_DOC("sheet1.ods")

HBOFFICE_FINISH()
RETURN
