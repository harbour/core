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
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 0)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "BALANÇO GERAL")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_VALIGN_CENTER)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 1
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 1)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Governo Municipal de São Mateus do Maranhão")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 2
N_Row := 1
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 1)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "EXERCÍCIO FINANCEIRO DE 2023")
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 2
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 2)
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
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 2, 3)
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
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 5)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "DEMONSTRAÇÃO DA RECEITA E DESPESA")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 6
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 6)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "SEGUNDO AS CATEGORIAS ECONÔMICAS")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

N_Col := 0
N_Row := 8
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 8)
NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "R E C E I T A")
NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, NAP_OFFICE_RGB(205, 205, 205))

N_Col := 2
N_Row := 8
NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 8)
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
    NAP_XLS_CELL_WRAPPED(O_XLS, N_Page, N_Col, N_Row, .T.)

    IF N_RowHeight != 0
       NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, N_Row, N_RowHeight)
    ENDIF

RETURN

***********************************
STAT PROC TST_PLANILHA_EXEMPLO_2
***********************************
// Replicate this example
// SICONFI_RREO_2301604_20230104_V11.XLS

LOCAL O_XLS := NAP_XLS_CREATE()
LOCAL N_Page := 0
LOCAL N_Col, N_Row

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Generate the spreadsheet data
NAP_XLS_NAME(O_XLS, N_Page, "RREO-Anexo 01")
NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 0, 15250)
NAP_XLS_ROW_HEIGHT(O_XLS, N_Page, 0, 2616)

NAP_XLS_FREEZE(O_XLS, N_Page, 1, 0)
CELL_TEXT(O_XLS, N_Page, 0, 2, "RELATÓRIO RESUMIDO DE EXECUÇÃO ORÇAMENTÁRIA", 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 3, "VERSÃO: v11", 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 4, "VIGÊNCIA: 04/01/2023", 14, .T., 715)
CELL_TEXT(O_XLS, N_Page, 0, 6, "Ente: 2301604 - Assaré/CE", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 7, "Poder: E - Executivo", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 8, "Instituição: 2029 - Prefeitura Municipal de Assaré - CE", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 9, "Exercício: 2023", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 10, "Periodicidade: BIMESTRAL", 10, .T., 0)
CELL_TEXT(O_XLS, N_Page, 0, 11, "Período: 5º bimestre", 10, .T., 0)


// NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 1, 2200)    // B Width
// NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 2, 8000)    // C Width
// NAP_XLS_COLUMN_WIDTH(O_XLS, N_Page, 3, 2200)    // D Width

// N_Col := 0
// N_Row := 0
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 0)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "BALANÇO GERAL")
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
// NAP_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_VALIGN_CENTER)
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 1
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 1)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Governo Municipal de São Mateus do Maranhão")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 2
// N_Row := 1
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 1)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "EXERCÍCIO FINANCEIRO DE 2023")
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 2
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 2)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Consolidado")
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_LEFT)
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 10.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 3
// N_Row := 2
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Adendo II")
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_LEFT)
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 3
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 2, 3)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Anexo 1, da Lei nº 4320, de 17/03/64. (Portaria SOF nº 8, de 04/02/85)")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 3
// N_Row := 3
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "Em R$ 1,00")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 5
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 5)
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "DEMONSTRAÇÃO DA RECEITA E DESPESA")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 6
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 6)
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "SEGUNDO AS CATEGORIAS ECONÔMICAS")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)

// N_Col := 0
// N_Row := 8
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 1, 8)
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "R E C E I T A")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
// NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, NAP_OFFICE_RGB(205, 205, 205))

// N_Col := 2
// N_Row := 8
// NAP_XLS_CELL_MERGE(O_XLS, N_Page, N_Col, N_Row, 3, 8)
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_CENTER)
// NAP_XLS_CELL_TEXT(O_XLS, N_Page, N_Col, N_Row, "D E S P E S A")
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 9.0)
// NAP_XLS_CELL_BOLD(O_XLS, N_Page, N_Col, N_Row, .T.)
// NAP_XLS_CELL_BACKCOLOR(O_XLS, N_Page, N_Col, N_Row, NAP_OFFICE_RGB(205, 205, 205))

// N_Col := 1
// N_Row := 11
// NAP_XLS_CELL_HALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_HALIGN_RIGHT)
// NAP_XLS_CELL_VALIGN(O_XLS, N_Page, N_Col, N_Row, SDK_VALIGN_CENTER)
// NAP_XLS_CELL_NUMFORMAT(O_XLS, N_Page, N_Col, N_Row, SDK_NUMFORMAT_INT)
// NAP_XLS_CELL_VALUE(O_XLS, N_Page, N_Col, N_Row, 917038.94)
// NAP_XLS_CELL_FONT_FAMILY(O_XLS, N_Page, N_Col, N_Row, "Arial")
// NAP_XLS_CELL_FONT_SIZE(O_XLS, N_Page, N_Col, N_Row, 7)

// Protect the sheet
// NAP_XLS_PROTECT(O_XLS, N_Page, .T., "ASDF01234")

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_02.ods" })
OFFICE_ERROR("Salvando a planilha")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_02.ods")
