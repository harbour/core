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

// Header, footer and page margins config
NAP_DOC_PAGE_HEADER_SHOW(O_DOC, .T.)
NAP_DOC_PAGE_HEADER_MARGINS(O_DOC, 254, 280, 508, 102, .T., .T.)
NAP_DOC_PAGE_FOOTER_SHOW(O_DOC, .T.)
NAP_DOC_PAGE_FOOTER_MARGINS(O_DOC, 354, 380, 608, 202, .T., .T.)
NAP_DOC_PAGE_MARGINS(O_DOC, 2819, 178, 1270, 533, 0)

// Create the header with image, document data and table headers
NAP_DOC_TEXT_SPACE(O_DOC, SDK_TEXT_SPACE_HEADER)
NAP_DOC_FONT_FAMILY(O_DOC, "LucidaConsole")
NAP_DOC_FONT_SIZE(O_DOC, 10.0)
NAP_DOC_INSERT_IMAGE(O_DOC, SDK_ANCHOR_AT_PARAGRAPH, 2000, 2000, SDK_HALIGN_LEFT, SDK_VALIGN_CENTER, {|| NAP_WORK_PATH() + "/../office/ods/macapa.png"} )
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

NAP_DOC_TEXT_SPACE(O_DOC, SDK_TEXT_SPACE_PAGE)
NAP_DOC_FONT_FAMILY(O_DOC, "Courier New")
NAP_DOC_FONT_SIZE(O_DOC, 10.0)
NAP_DOC_PARAGRAPH_LSPACING(O_DOC, 330)
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")
DOC_LINE(O_DOC, "              PREFEITURA MUNICIPAL DE MACAPÁ")



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
