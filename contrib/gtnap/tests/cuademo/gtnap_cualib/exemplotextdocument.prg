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
STAT PROC TST_CREATE_DOCUMENT
***********************************
// Just a simple example to create a text document

LOCAL O_DOC := NAP_DOC_CREATE()

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

NAP_DOC_INSERT(O_DOC, "Hello World! Hello Create LibreOffice text document.")

// Save the document
NAP_DOC_SAVE(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_Text_01.odt" })
OFFICE_ERROR("Erro ao salvar documento de texto")

// Close the document (mandatory)
NAP_DOC_CLOSE(O_DOC)
OFFICE_ERROR("Erro ao fechar o documento de texto")

MOSTRAR("M15566", "O documento de texto foi criado com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_Text_01.odt")
