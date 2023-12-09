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
    C_ERR := NAP_OFFICE_ERROR(N_Ret)
    MOSTRAR("M15566","Erro ao exportar para .pdf: " + C_ERR)
ENDIF

***********************************
STAT FUNCTION OFFICE_ERROR( C_Text )
***********************************
LOCAL N_Err := NAP_OFFICE_LAST_ERROR()
LOCAL C_Err := NAP_OFFICE_ERROR(N_Err)
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

IF OFFICE_ERROR("Creando a planilha")
    RETURN
ENDIF

// Generate the spreadsheet data
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 0, 8000)    // A Width
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 1, 2200)    // B Width
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 2, 8000)    // C Width
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 3, 2200)    // D Width

NAP_XLS_CELL_MERGE(O_XLS, 0, 0, 0, 3)
NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 0, "BALANÇO GERAL")
NAP_XLS_CELL_HALIGN(O_XLS, 0, 0, 0, SDK_HALIGN_CENTER)
NAP_XLS_CELL_FONT_FAMILY(O_XLS, 0, 0, 0, "Arial")
NAP_XLS_CELL_FONT_SIZE(O_XLS, 0, 0, 0, 10.0)
NAP_XLS_CELL_BOLD(O_XLS, 0, 0, 0, .T.)

// Save the spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_01.ods" })
OFFICE_ERROR("Salvando a planilha")

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)
OFFICE_ERROR("Fechando planilha")

MOSTRAR("M15566", "A planilha foi criada com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_01.ods")
