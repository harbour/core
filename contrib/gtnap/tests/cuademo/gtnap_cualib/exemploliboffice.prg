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
LOCAL O_XLS := NAP_XLS_OPEN({|| NAP_WORK_PATH() + "/../office/empty.ods" })

IF OFFICE_ERROR("Abrindo a planilha")
    RETURN
ENDIF

// Save an edited spreadsheet
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/edited.ods" })

IF OFFICE_ERROR("Salvando a planilha")
    RETURN
ENDIF

// Close the spreadsheet (mandatory)
NAP_XLS_CLOSE(O_XLS)

IF OFFICE_ERROR("Fechando planilha")
    RETURN
ENDIF

MOSTRAR("M15566", "A planilha foi editada com sucesso.")

