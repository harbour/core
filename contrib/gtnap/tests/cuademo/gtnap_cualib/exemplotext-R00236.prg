/* encoding: cp850 */
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"

***********************************
PROC TST_R00236
***********************************
LOCAL O_DOC := NAP_DOC_CREATE()

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

NAP_DOC_FONT_FAMILY(O_DOC, "LucidaConsole")
NAP_DOC_FONT_SIZE(O_DOC, 12.0)
NAP_DOC_HALIGN(O_DOC, SDK_HALIGN_CENTER)
NAP_DOC_INSERT_TEXT(O_DOC, "PREFEITURA MUNICIPAL DE MACAPÁ")

// Save the document
NAP_DOC_SAVE(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.odt" })
OFFICE_ERROR("Erro ao salvar documento de texto")

// Export to PDF
NAP_DOC_PDF(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Close the document (mandatory)
NAP_DOC_CLOSE(O_DOC)
OFFICE_ERROR("Erro ao fechar o documento de texto")

MOSTRAR("M15566", "O documento de texto foi criado com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.odt")
