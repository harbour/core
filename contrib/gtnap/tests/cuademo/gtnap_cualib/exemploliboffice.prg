/* encoding: cp850 */
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"

***********************
PROC EXEMPLO_LIBOFFICE
***********************
LOCAL V_Janela

CUA20 @ 15,20,30,70 JANELA V_Janela ;
     TITULO "Escolha o tipo de operação" SUBTITULO "%T";
     AJUDA "T?????"

ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "Exportar .odt para .pdf" ;
   ACAO TST_ODT_PARA_PDF() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "Adicione células à planilha" ;
   ACAO TST_EDITAR_PLANILHA() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "Planilha exemplo 1" ;
   ACAO TST_PLANILHA_EXEMPLO_1() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "Planilha exemplo 2" ;
    ACAO TST_PLANILHA_EXEMPLO_2() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "Planilha exemplo 3" ;
    ACAO TST_PLANILHA_EXEMPLO_3() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "Planilha exemplo 4" ;
    ACAO TST_PLANILHA_EXEMPLO_4() AJUDA "P06685"

ATIVE(V_Janela)

***********************************
STAT PROC TST_ODT_PARA_PDF
***********************************
LOCAL V_Janela
LOCAL N_Ret := NAP_OFFICE_TEXT_TO_PDF({|| NAP_WORK_PATH() + "/../office/test.odt" }, {|| NAP_WORK_PATH() + "/../office/test.pdf" })
LOCAL C_ERR

IF N_Ret == SDKRES_OK
    MOSTRAR("M15566","Exportação test.odt para .pdf realizada com sucesso.")
ELSE
    C_ERR := NAP_OFFICE_ERROR_STR(N_Ret)
    MOSTRAR("M15566","Erro ao exportar para .pdf: " + C_ERR)
ENDIF

***********************************
STAT FUNCTION OFFICE_ERROR( C_Text )
***********************************
LOCAL N_Err := NAP_OFFICE_LAST_ERROR()
LOCAL C_Err := NAP_OFFICE_ERROR_STR(N_Err)
LOCAL L_Err := .F.

IF N_Err != SDKRES_OK
    MOSTRAR("M15566", C_Text + ": " + C_Err)
    L_Err = .T.
ENDIF

RETURN L_Err

***********************************
STAT PROC TST_EDITAR_PLANILHA
***********************************

// Open an existing spreadsheet
LOCAL O_XLS := NAP_XLS_OPEN( NAP_WORK_PATH() + "/../office/empty.ods" )

IF OFFICE_ERROR("Abrindo a planilha")
    RETURN
ENDIF

NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 0, "Hello World! (0,0)")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, 0, 0, 0, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, 0, 0, 0, 16.0)

NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 1, "Hello World! (0,1)")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, 0, 0, 1, "Times New Roman")
NAP_XLS_CELL_FONT_SIZE(O_XLS, 0, 0, 1, 20.0)
NAP_XLS_CELL_BOLD(O_XLS, 0, 0, 1, .T.)
NAP_XLS_CELL_ITALIC(O_XLS, 0, 0, 1, .T.)

NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 2, {|| "Hello World! (0,2)"})

NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 0, 6000)
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 1, 12000)
NAP_XLS_COLUMN_VISIBLE(O_XLS, 0, 2, .F.)

// Save an edited spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/edited.ods" })
OFFICE_ERROR("Salvando a planilha")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi editada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/edited.ods")

***********************************
STAT PROC TST_PLANILHA_EXEMPLO_1
***********************************
// Replicate this example
// Anexo_01_Dem_da_receita_e_despesa_segundo_as_cat_economicasMA854_u.g._Consolidado__Exercicio_2023.ods

LOCAL O_XLS := NAP_XLS_CREATE()
LOCAL N_Page := 0
LOCAL N_Col, N_Row

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Generate the spreadsheet data
NAP_XLS_NAME(O_XLS, N_Page, "DEMOSTRAÇAO RECEITA E DESPESA")
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 8000)    // A Width
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 1, 2200)    // B Width
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 2, 8000)    // C Width
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 3, 2200)    // D Width

N_Col := 0
N_Row := 0
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 0)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "BALANÇO GERAL")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_VALIGN_CENTER)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 1
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 1)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Governo Municipal de São Mateus do Maranhão")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 2
N_Row := 1
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 1)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "EXERCÍCIO FINANCEIRO DE 2023")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 2
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 2)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Consolidado")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_LEFT)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 3
N_Row := 2
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Adendo II")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_LEFT)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 3
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 2, 3)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Anexo 1, da Lei nº 4320, de 17/03/64. (Portaria SOF nº 8, de 04/02/85)")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 3
N_Row := 3
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Em R$ 1,00")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 5
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 5)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "DEMONSTRAÇÃO DA RECEITA E DESPESA")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 6
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 6)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "SEGUNDO AS CATEGORIAS ECONÔMICAS")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 8
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 8)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "R E C E I T A")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, NAP_OFFICE_RGB(205, 205, 205))

N_Col := 2
N_Row := 8
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 8)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "D E S P E S A")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, NAP_OFFICE_RGB(205, 205, 205))

N_Col := 1
N_Row := 11
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_VALIGN_CENTER)
NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, N_Col, N_Row, SDK_NUMFORMAT_INT)
NAP_XLS_CELL_VALUE(O_XLS, N_Page, N_Col, N_Row, 917038.94)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 7)

// Protect the sheet
NAP_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_01.ods" })
OFFICE_ERROR("Salvando a planilha")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_01.ods")


***********************************
STAT PROC CELL_TEXT( O_XLS, N_Page, N_Col, N_Row, C_Text, N_Size, L_Bold, N_RowHeight )
    NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, C_Text)
    NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "LucidaSansRegular")
    NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, N_Size)
    NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, L_Bold)
    NAP_XLS_CELL_WRAP(O_XLS, N_Page, N_Col, N_Row, .T.)

    IF N_RowHeight != 0
       NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, N_Row, N_RowHeight)
    ENDIF

RETURN

***********************************
STAT PROC ADD_PAGE( O_XLS, N_Id, C_Name )
    NAP_XLS_ADD(O_XLS)
    NAP_XLS_NAME(O_XLS, N_Id, C_Name)
RETURN

***********************************
STAT PROC TST_PLANILHA_EXEMPLO_2
***********************************
// Replicate this example
// SICONFI_RREO_2301604_20230104_V11.XLS

LOCAL O_XLS := NAP_XLS_CREATE()
LOCAL N_Page := 0
LOCAL N_Col, N_Row, N_Cont
LOCAL C_Formula := ""

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Generate the spreadsheet pages
NAP_XLS_NAME(O_XLS, 0, "RREO-Anexo 01")     // The first is allways created by an empty document
ADD_PAGE(O_XLS, 1, "RREO-Anexo 02")
ADD_PAGE(O_XLS, 2, "RREO-Anexo 03")
ADD_PAGE(O_XLS, 3, "RREO-Anexo 04")
ADD_PAGE(O_XLS, 4, "RREO-Anexo 06")
ADD_PAGE(O_XLS, 5, "RREO-Anexo 07")
ADD_PAGE(O_XLS, 6, "RREO-Anexo 13")
ADD_PAGE(O_XLS, 7, "RREO-Anexo 14")

FOR N_Page := 0 TO 7

// Remove all grid lines
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 0, 1000, 1000, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 15250)
NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 0, 2616)
NAP_XLS_CELL_IMAGE(O_XLS, N_Page, 0, 0, {|| NAP_WORK_PATH() + "/../office/ods/cell_image_01.png" })

NAP_XLS_FREEZE(O_XLS, N_Page, 1, 0)
CELL_TEXT(O_XLS, N_Page, 0, 2, "RELATÓRIO RESUMIDO DE EXECUÇÃO ORÇAMENTÁRIA: " + hb_ntos(N_Page), 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 3, "VERSÃO: v11: " + hb_ntos(N_Page), 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 4, "VIGÊNCIA: 04/01/2023: " + hb_ntos(N_Page), 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 6, "Ente: 2301604 - Assaré/CE: " + hb_ntos(N_Page), 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 7, "Poder: E - Executivo: " + hb_ntos(N_Page), 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 8, "Instituição: 2029 - Prefeitura Municipal de Assaré - CE: " + hb_ntos(N_Page), 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 9, "Exercício: 2023: " + hb_ntos(N_Page), 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 10, "Periodicidade: BIMESTRAL: " + hb_ntos(N_Page), 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 11, "Período: 5º bimestre: " + hb_ntos(N_Page), 10, .T., 0)

CELL_TEXT(O_XLS, N_Page, 0, 13, "Grupo: Tabela 1.0 - Balanço Orçamentário", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 14, "Quadro: Receitas Orçamentárias", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 15, "Rótulo: Padrão", 10, .T., 0)
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 0, 13, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 0, 14, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 0, 15, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 13, NAP_OFFICE_RGB(63, 103, 151))
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 14, NAP_OFFICE_RGB(63, 103, 151))
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 15, NAP_OFFICE_RGB(63, 103, 151))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 13, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 14, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 15, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 1, 0, 12, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 0, 1, 0, 12, SDK_LINE_STYLE_SOLID, 50, NAP_OFFICE_RGB(0, 0, 0))

NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 16, 1067)
NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 17, 1067)
NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 18, 1067)

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 0, 16, 0, 18)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 16, 0, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 0, 16, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 0, 16, "Receitas Orçamentárias", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 0, 16, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 0, 16, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 0, 16, 0, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 1, 4496)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 2, 5588)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 3, 7722)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 4, 7823)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 5, 4115)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 6, 7468)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 7, 7823)

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 1, 16, 7, 16)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 1, 16, 7, 16, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 1, 16, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 1, 16, "Estágios da Receita Orçamentária", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 1, 16, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 1, 16, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 1, 16, 7, 16, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 1, 17, 1, 18)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 1, 17, 1, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 1, 17, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 1, 17, "PREVISÃO INICIAL", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 1, 17, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 1, 17, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 1, 17, 1, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 2, 17, 2, 18)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 2, 17, 2, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 2, 17, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 2, 17, "PREVISÃO ATUALIZADA (a)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 2, 17, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 2, 17, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 2, 17, 2, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 3, 17, 6, 17)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 3, 17, 6, 17, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 3, 17, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 3, 17, "RECEITAS REALIZADAS", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 3, 17, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 3, 17, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 3, 17, 6, 17, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 3, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 3, 18, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 3, 18, "No Bimestre (b)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 3, 18, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 3, 18, SDK_VALIGN_CENTER)
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 3, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 4, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 4, 18, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 4, 18, "% (b/a)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 4, 18, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 4, 18, SDK_VALIGN_CENTER)
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 4, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 5, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 5, 18, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 5, 18, "Até o Bimestre (c)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 5, 18, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 5, 18, SDK_VALIGN_CENTER)
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 5, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 6, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 6, 18, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 6, 18, "% (c/a)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 6, 18, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 6, 18, SDK_VALIGN_CENTER)
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 6, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 7, 17, 7, 18)
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 7, 17, 7, 18, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 7, 17, NAP_OFFICE_RGB(255, 255, 255))
CELL_TEXT(O_XLS, N_Page, 7, 17, "SALDO (a-c)", 10, .T., 0)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 7, 17, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 7, 17, SDK_VALIGN_CENTER)
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 7, 17, 7, 18, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

CELL_TEXT(O_XLS, N_Page, 0, 19, "Receitas Orçamentárias", 10, .F., 0)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 19, NAP_OFFICE_RGB(219, 229, 241))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 19, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

CELL_TEXT(O_XLS, N_Page, 0, 20, "  RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)", 10, .F., 0)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 20, NAP_OFFICE_RGB(184, 204, 228))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 19, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

CELL_TEXT(O_XLS, N_Page, 0, 21, "    RECEITAS CORRENTES", 10, .F., 0)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 21, NAP_OFFICE_RGB(219, 229, 241))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 21, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

CELL_TEXT(O_XLS, N_Page, 0, 22, "      IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA", 10, .F., 0)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 0, 22, NAP_OFFICE_RGB(184, 204, 228))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 0, 22, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))

NAP_XLS_CELL_VALUE(O_XLS, N_Page, 1, 21, 136742327.00)
NAP_XLS_CELL_VALUE(O_XLS, N_Page, 1, 22, 7169100.07)
NAP_XLS_CELL_FORMULA(O_XLS, N_Page, 1, 20, "B22+B23")
NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, 1, 21, SDK_NUMFORMAT_DEC2_1000)
NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, 1, 22, SDK_NUMFORMAT_DEC2_1000)
NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, 1, 20, SDK_NUMFORMAT_DEC2_1000)

NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 24, 1067)
NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 25, 1067)
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 0, 24, 0, 25)

NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 24, 0, 25, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 0, 24, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 0, 24, 0, 25, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 0, 24, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 0, 24, SDK_VALIGN_CENTER)
CELL_TEXT(O_XLS, N_Page, 0, 24, "Notas Explicativas", 10, .T., 0)

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 1, 24, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 1, 24, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 1, 24, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 1, 24, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 1, 24, SDK_VALIGN_CENTER)
CELL_TEXT(O_XLS, N_Page, 1, 24, "Valores", 10, .T., 0)

NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, 1, 25, NAP_OFFICE_RGB(79, 129, 189))
NAP_XLS_CELL_COLOR(O_XLS, N_Page, 1, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_BORDER(O_XLS, N_Page, 1, 25, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(255, 255, 255))
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, 1, 25, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 1, 25, SDK_VALIGN_CENTER)
NAP_XLS_CELL_DATE(O_XLS, N_Page, 1, 25, 31, 10, 2023)
NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, 1, 25, SDK_NUMFORMAT_DATE_SYS_NNNNDMMMMYYYY)

NEXT

// Testing multi-page formula
NAP_XLS_CELL_VALUE(O_XLS, 0, 0, 28, 1000.23)
NAP_XLS_CELL_VALUE(O_XLS, 1, 0, 28, 543.15)
NAP_XLS_CELL_VALUE(O_XLS, 2, 0, 28, 823.45)
NAP_XLS_CELL_VALUE(O_XLS, 3, 0, 28, -330.89)
NAP_XLS_CELL_VALUE(O_XLS, 4, 0, 28, 829.00)
NAP_XLS_CELL_VALUE(O_XLS, 5, 0, 28, -100.99)
NAP_XLS_CELL_VALUE(O_XLS, 6, 0, 28, 43.74)
NAP_XLS_CELL_VALUE(O_XLS, 7, 0, 28, 600.64)
NAP_XLS_CELL_FORMULA(O_XLS, 0, 0, 29, "$'RREO-Anexo 01'.A29+$'RREO-Anexo 02'.A29+$'RREO-Anexo 03'.A29+$'RREO-Anexo 04'.A29+$'RREO-Anexo 06'.A29+$'RREO-Anexo 07'.A29+$'RREO-Anexo 13'.A29+$'RREO-Anexo 14'.A29")

// Compose the same formula using cell references
C_Formula:=""

FOR N_Page := 0 TO 7
    C_Formula += NAP_CELL_REF(O_XLS, N_Page, 0, 28)
    IF N_Page < 7
        C_Formula += "+"
    ENDIF
NEXT
NAP_XLS_CELL_FORMULA(O_XLS, 0, 0, 30, C_Formula)


// Protect the sheet
// NAP_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_02.ods" })
OFFICE_ERROR("Salvando a planilha")

// Export to PDF
NAP_XLS_PDF(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_02.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Print spreadsheet
NAP_XLS_PRINT(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Print_03.pdf" }, "", SDK_PAPER_ORIENT_PORTRAIT, SDK_PAPER_FORMAT_A4, 0, 0, 1, .F., "1-")
//NAP_XLS_PRINT(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Print_02.pdf" }, "Microsoft Print to PDF", SDK_PAPER_ORIENT_LANSCAPE, SDK_PAPER_FORMAT_USER, 30000, 60000, 1, .F., "")
OFFICE_ERROR("Imprimindo a planilha")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_02.ods")


***********************************
STAT PROC Text3(O_XLS, N_Page, N_Col, N_Row, C_Text, N_HAlign, N_Size, L_Bold)
    NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, C_Text)
    NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Times New Roman")
    NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, N_Size)
    NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, N_HAlign)
    NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, L_Bold)

***********************************
STAT PROC Value3(O_XLS, N_Page, N_Col, N_Row, N_Value, N_Size, L_Bold)
    NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Times New Roman")
    NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, N_Size)
    NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
    NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, L_Bold)
    NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, N_Col, N_Row, SDK_NUMFORMAT_DEC2_1000)
    NAP_XLS_CELL_VALUE(O_XLS, N_Page, N_Col, N_Row, N_Value)

***********************************
STAT PROC Head3(O_XLS, N_Page, N_Col, N_Row, C_Text)
    Text3(O_XLS, N_Page, N_Col, N_Row, C_Text, SDK_HALIGN_CENTER, 10, .T.)

***********************************
STAT PROC Title3(O_XLS, N_Page, N_Col, N_Row, C_Text)
    Text3(O_XLS, N_Page, N_Col, N_Row, C_Text, SDK_HALIGN_LEFT, 10, .F.)

***********************************
STAT PROC TitleB3(O_XLS, N_Page, N_Col, N_Row, C_Text)
    Text3(O_XLS, N_Page, N_Col, N_Row, C_Text, SDK_HALIGN_LEFT, 10, .T.)

***********************************
STAT PROC DataRow3(O_XLS, N_Page, N_Col, N_Row, V_Data)
    LOCAL N_Cont, N := LEN(V_Data)
    FOR N_Cont := 1 TO N
        Value3(O_XLS, N_Page, N_Col + N_Cont - 1, N_Row, V_Data[N_Cont], 8, .F.)
    NEXT

***********************************
STAT PROC TST_PLANILHA_EXEMPLO_3
***********************************
// Replicate this example
// RREO_CE05J_01012023_A_28022023.xls

LOCAL O_XLS := NAP_XLS_CREATE()
LOCAL N_Page := 0
LOCAL N_Col, N_Row, N_Cont
LOCAL C_Formula := ""

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

NAP_XLS_NAME(O_XLS, 0, "Anexo 3 - RCL Municípios")

// Remove all grid lines
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 0, 1000, 1000, NAP_OFFICE_RGB(255, 255, 255))

// Column widths
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 10973)
FOR N_Cont := 1 TO 12
    NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, N_Cont, 2337)
NEXT
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 13, 2642)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 14, 2642)

// Row heights
FOR N_Cont := 1 TO 11
    NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, N_Cont, 406)
NEXT
FOR N_Cont := 12 TO 39
    NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, N_Cont, 508)
NEXT

// Borders
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 0, 9, 0, 11, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 1, 9, 12, 10, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 13, 9, 13, 11, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
NAP_XLS_CELLS_BORDER(O_XLS, N_Page, 14, 9, 14, 11, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
FOR N_Cont := 1 TO 12
    NAP_XLS_CELL_BORDER(O_XLS, N_Page, N_Cont, 11, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
NEXT
FOR N_Cont := 0 TO 14
    NAP_XLS_CELLS_BORDER(O_XLS, N_Page, N_Cont, 12, N_Cont, 39, SDK_LINE_STYLE_SOLID, 25, NAP_OFFICE_RGB(0, 0, 0))
NEXT

// Gray headers
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 9, 14, 11, NAP_OFFICE_RGB(217, 217, 217))
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 1, 9, 12, 10)
Head3(O_XLS, N_Page, 0, 10, "ESPECIFICAÇÃO")
Head3(O_XLS, N_Page, 1, 9, "EVOLUÇÃO DA RECEITA REALIZADA NOS ÚLTIMOS 12 MESES")
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, 1, 9, SDK_VALIGN_CENTER)
Head3(O_XLS, N_Page, 1, 11, "Mar/2022")
Head3(O_XLS, N_Page, 2, 11, "Abr/2022")
Head3(O_XLS, N_Page, 3, 11, "Mai/2022")
Head3(O_XLS, N_Page, 4, 11, "Jun/2022")
Head3(O_XLS, N_Page, 5, 11, "Jul/2022")
Head3(O_XLS, N_Page, 6, 11, "Ago/2022")
Head3(O_XLS, N_Page, 7, 11, "Set/2022")
Head3(O_XLS, N_Page, 8, 11, "Out/2022")
Head3(O_XLS, N_Page, 9, 11, "Nov/2022")
Head3(O_XLS, N_Page, 10, 11, "Dez/2022")
Head3(O_XLS, N_Page, 11, 11, "Jan/2023")
Head3(O_XLS, N_Page, 12, 11, "Feb/2023")
Head3(O_XLS, N_Page, 13, 9, "TOTAL")
Head3(O_XLS, N_Page, 13, 10, "(ULTIMOS")
Head3(O_XLS, N_Page, 13, 11, "12 MESES)")
Head3(O_XLS, N_Page, 14, 9, "PREVISAO")
Head3(O_XLS, N_Page, 14, 10, "ATUALIZADA")
Head3(O_XLS, N_Page, 14, 11, "<EXERCICIO>")

// Row titles
TitleB3(O_XLS, N_Page, 0, 12, "RECEITAS CORRENTES (I)")
Title3(O_XLS, N_Page, 0, 13, "  Impostos, Taxas e Contribuições de Melhoria")
Title3(O_XLS, N_Page, 0, 14, "      IPTU")
Title3(O_XLS, N_Page, 0, 15, "      ISS")
Title3(O_XLS, N_Page, 0, 16, "      ITBI")
Title3(O_XLS, N_Page, 0, 17, "      IRRF")
Title3(O_XLS, N_Page, 0, 18, "    Outros Impostos, Taxas e Contribuições de Melhoria")
Title3(O_XLS, N_Page, 0, 19, "  Contribuições")
Title3(O_XLS, N_Page, 0, 20, "  Receita Patrimonial")
Title3(O_XLS, N_Page, 0, 21, "      Rendimentos de Aplicação Financeira")
Title3(O_XLS, N_Page, 0, 22, "      Outras Receitas Patrimoniais")
Title3(O_XLS, N_Page, 0, 23, "  Receita Agropecuária")
Title3(O_XLS, N_Page, 0, 24, "  Receita Industrial")
Title3(O_XLS, N_Page, 0, 25, "  Receita de Serviços")
Title3(O_XLS, N_Page, 0, 26, "  Transferências Correntes")
Title3(O_XLS, N_Page, 0, 27, "      Cota-Parte do FPM")
Title3(O_XLS, N_Page, 0, 28, "      Cota-Parte do ICMS")
Title3(O_XLS, N_Page, 0, 29, "      Cota-Parte do IPVA")
Title3(O_XLS, N_Page, 0, 30, "      Cota-Parte do ITR")
Title3(O_XLS, N_Page, 0, 31, "      Transferências da LC 61/1989")
Title3(O_XLS, N_Page, 0, 32, "      Transferências do FUNDEB")
Title3(O_XLS, N_Page, 0, 33, "      Outras Transferências Correntes")
Title3(O_XLS, N_Page, 0, 34, "  Outras Receitas Correntes")
TitleB3(O_XLS, N_Page, 0, 35, "DEDUÇÕES (II)")
Title3(O_XLS, N_Page, 0, 36, "  Contrib. do Servidor para o Plano de Previdência")
Title3(O_XLS, N_Page, 0, 37, "  Compensação Financ. entre Regimes Previdência")
Title3(O_XLS, N_Page, 0, 38, "  Rendimentos de Aplicações de Recursos Previdenciários")
Title3(O_XLS, N_Page, 0, 39, "  Dedução de Receita para Formação do FUNDEB")

// Row data
DataRow3(O_XLS, N_Page, 1, 12, {4193635.48, 3692660.69, 4676720.23, 4292836.84, 4972625.01, 4161792.35, 3664558.29, 3608630.96, 4407983.35, 5639442.57, 5077716.60, 4950347.71, 53338950.08, 53598740.65})
DataRow3(O_XLS, N_Page, 1, 13, {222063.65, 99440.64, 78137.74, 56888.01, 80325.47, 479368.18, 164715.95, 149389.74, 304492.89, 638963.62, 543154.34, 507868.50, 3324808.73, 1732000.00})
DataRow3(O_XLS, N_Page, 1, 14, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 18561.69, 19732.47, 2589.07, 40883.23, 2000.00})
DataRow3(O_XLS, N_Page, 1, 15, {189616.88, 53551.21, 33850.18, 24443.57, 51426.40, 63563.19, 126710.61, 58658.62, 263853.39, 565742.69, 415283.92, 447777.45, 2294478.11, 655000.00})
DataRow3(O_XLS, N_Page, 1, 16, {300.00, 450.00, 6240.00, 1050.00, 2600.00, 1120.00, 200.00, 3000.00, 2805.00, 0.00, 500.00, 6000.00, 24265.00, 15000.00})
DataRow3(O_XLS, N_Page, 1, 17, {31816.57, 44590.63, 37427.36, 31168.43, 24604.96, 58114.86, 35385.61, 87490.99, 37834.50, 54537.74, 101251.04, 46896.67, 591119.36, 1030000.00})
DataRow3(O_XLS, N_Page, 1, 18, {330.20, 848.80, 620.20, 226.01, 1694.11, 356570.13, 2419.73, 240.13, 0.00, 121.50, 6386.91, 4605.31, 374063.03, 30000.00})
DataRow3(O_XLS, N_Page, 1, 19, {10648.95, 10650.33, 10854.90, 11548.34, 11941.53, 11831.55, 10131.32, 8442.95, 8007.37, 11017.10, 11378.93, 9925.10, 126378.37, 142000.00})
DataRow3(O_XLS, N_Page, 1, 20, {29484.03, 26750.25, 37343.77, 41301.46, 50663.04, 56982.80, 46934.16, 41510.84, 39798.60, 49062.48, 49546.85, 45692.03, 515070.31, 371868.00})
DataRow3(O_XLS, N_Page, 1, 21, {29484.03, 26750.25, 37343.77, 41301.46, 50663.04, 56982.80, 46934.16, 41510.84, 39798.60, 49062.48, 49546.85, 45692.03, 515070.31, 369868.00})
DataRow3(O_XLS, N_Page, 1, 22, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2000.00})
DataRow3(O_XLS, N_Page, 1, 23, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00})
DataRow3(O_XLS, N_Page, 1, 24, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00})
DataRow3(O_XLS, N_Page, 1, 25, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2000.00})
DataRow3(O_XLS, N_Page, 1, 26, {3921342.58, 3549848.26, 4520083.77, 4180934.73, 4829694.97, 3613044.22, 3441335.79, 3408968.29, 4054832.47, 4940116.57, 4472080.60, 4385840.05, 49318122.30, 51208872.65})
DataRow3(O_XLS, N_Page, 1, 27, {1321464.88, 1566403.79, 1742467.92, 1630411.26, 2194430.96, 1594372.84, 1468394.23, 1397127.45, 1755934.88, 2728370.70, 1570107.30, 2360226.56, 21329712.77, 22570000.00})
DataRow3(O_XLS, N_Page, 1, 28, {706965.28, 548826.47, 594477.58, 550751.70, 586698.98, 579882.11, 567525.78, 557864.03, 587203.61, 549503.45, 636188.52, 528590.35, 6994477.86, 6600000.00})
DataRow3(O_XLS, N_Page, 1, 29, {35449.67, 31571.76, 24822.61, 27953.48, 11321.92, 9790.56, 5984.60, 6045.26, 9891.44, 9770.58, 98637.50, 78362.88, 349602.26, 500000.00})
DataRow3(O_XLS, N_Page, 1, 30, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 737.27, 7.19, 5.97, 0.00, 0.00, 750.43, 1000.00})
DataRow3(O_XLS, N_Page, 1, 31, {2059.03, 2094.08, 1113.29, 1570.75, 1619.05, 1289.62, 1701.29, 1736.30, 1262.54, 1793.82, 2062.60, 1470.88, 19773.25, 20000.00})
DataRow3(O_XLS, N_Page, 1, 32, {966577.94, 1120998.55, 1075233.55, 1057347.81, 1009761.58, 1050272.50, 1038698.15, 1049373.92, 1128145.90, 1132128.26, 1837081.05, 1036209.85, 13501829.06, 14957566.65})
DataRow3(O_XLS, N_Page, 1, 33, {888825.78, 279953.61, 1081968.82, 912899.73, 1025862.48, 377436.59, 359031.74, 396084.06, 572386.91, 518543.79, 328003.63, 380979.53, 7121976.67, 6560306.00})
DataRow3(O_XLS, N_Page, 1, 34, {10096.27, 5971.21, 30300.05, 2164.30, 0.00, 565.60, 1441.07, 319.14, 852.02, 282.80, 1555.88, 1022.03, 54570.37, 142000.00})
DataRow3(O_XLS, N_Page, 1, 35, {413187.79, 429779.23, 472576.28, 442137.41, 399585.45, 437067.02, 379951.13, 392702.03, 470859.85, 488067.71, 461399.10, 593730.09, 5381043.09, 5624200.00})
DataRow3(O_XLS, N_Page, 1, 36, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00})
DataRow3(O_XLS, N_Page, 1, 37, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00})
DataRow3(O_XLS, N_Page, 1, 38, {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00})
DataRow3(O_XLS, N_Page, 1, 39, {413187.79, 429779.23, 472576.28, 442137.41, 399585.45, 437067.02, 379951.13, 392702.03, 470859.85, 488067.71, 461399.10, 593730.09, 5381043.09, 5624200.00})

// Protect the sheet
// NAP_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_03.ods" })
OFFICE_ERROR("Salvando a planilha")

// Export to PDF
NAP_XLS_PDF(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_03.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_03.ods")

***********************************
STAT PROC Text4(O_XLS, N_Page, N_Col, N_Row, C_Text, N_HAlign, N_Size, L_Bold)
    NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, C_Text)
    NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
    NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, N_Size)
    NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, N_HAlign)
    NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, L_Bold)

***********************************
STAT PROC TST_PLANILHA_EXEMPLO_4
***********************************
// Replicate this example
// ce05g_Limoeiro_do_Norte_20231129001_027018_CONSTRUTORA_LAZIO_EIRRELI.xls

LOCAL O_XLS := NAP_XLS_CREATE()
LOCAL N_Page := 0
LOCAL N_Col, N_Row, N_Cont
LOCAL C_Formula := ""

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Column widths
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 900)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 1, 11000)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 2, 2010)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 3, 2490)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 4, 1804)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 5, 2010)
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 6, 2110)

// Row heights
FOR N_Cont := 0 TO 43
    NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, N_Cont, 458)
NEXT

// Merge Cells
FOR N_Cont := 0 TO 15
    NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 0, N_Cont, 6, N_Cont)
NEXT
NAP_XLS_CELLS_MERGE(O_XLS, N_Page, 0, 16, 6, 27)

// Background color
NAP_XLS_CELLS_BACKCOLOR(O_XLS, N_Page, 0, 0, 6, 0, NAP_OFFICE_RGB(255, 255, 0))

// Text
Text4(O_XLS, N_Page, 0, 0, "PREENCHER OS CAMPOS EM CINZA - NÃO ALTERAR A ESTRUTURA DA PLANILHA", SDK_HALIGN_CENTER, 8, .T.)
Text4(O_XLS, N_Page, 0, 1, "Solicitação de cotação de preços", SDK_HALIGN_CENTER, 12, .T.)
Text4(O_XLS, N_Page, 0, 2, "Cotação de preços No: 20231129001", SDK_HALIGN_CENTER, 10, .T.)
Text4(O_XLS, N_Page, 0, 3, "Prefeitura Municipal de Limoeiro do Norte", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 5, "RESPONSÁVEL :", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 6, "    NOME: CAMILA MARIA MAIA", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 9, "PROPONENTE :", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 10, "   NOME: CONSTRUTORA LAZIO EIRRELI", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 11, "   ENDEREÇO : AV SANTOS DUMONT 1740 SALA 105", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 12, "   BAIRRO : ALDEOTA   CIDADE : Fortaleza - CE", SDK_HALIGN_LEFT, 8, .T.)
Text4(O_XLS, N_Page, 0, 13, "   CNPJ : 10.697.540/0001-20", SDK_HALIGN_LEFT, 8, .T.)

// Protect the sheet
// NAP_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_04.ods" })
OFFICE_ERROR("Salvando a planilha")

// Export to PDF
NAP_XLS_PDF(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_04.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_04.ods")


