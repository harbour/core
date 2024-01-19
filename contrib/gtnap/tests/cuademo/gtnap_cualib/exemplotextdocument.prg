/* encoding: cp850 */
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"

***********************
PROC EXEMPLO_TEXT_DOCUMENT
***********************
LOCAL V_Janela

CUA20 @ 15,20,30,70 JANELA V_Janela ;
     TITULO "Escolha o tipo de operação" SUBTITULO "%T";
     AJUDA "T?????"

ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "Create simple text document" ;
   ACAO TST_CREATE_DOCUMENT() AJUDA "P06685"

ATIVE(V_Janela)

***********************************
STAT PROC TST_CREATE_DOCUMENT
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

