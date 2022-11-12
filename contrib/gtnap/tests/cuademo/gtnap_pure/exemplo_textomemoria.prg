#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_TEXTO_MEMORIA()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1
    LOCAL C_String := "Escolha o tipo de janela" + hb_eol() + "exibição/edição de texto em memória"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_MULTILINE()
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_MENUVERT_ADD(V_MenuVert, "Exibição de texto em memória", {|| TST_EXIBE_TEXTO_MEMORIA_READ_ONLY() })
    NAP_MENUVERT_ADD(V_MenuVert, "Edição de texto em memória com confirmações", {|| TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_CONFIRMACOES() })
    NAP_MENUVERT_ADD(V_MenuVert, "Edição de texto em memória com 'valid'", {|| TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_VALID() })
    NAP_MENUVERT_ADD(V_MenuVert, "Edição de texto em memória com filtro de tecla", {|| TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_FILTRO_TECLAS() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escolha o tipo de janela")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_EXIBE_TEXTO_MEMORIA_READ_ONLY()
    MOSTRAR("M?????", "Exibição de texto em memória", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_CONFIRMACOES()
    MOSTRAR("M?????", "Edição de texto em memória com confirmações", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_VALID()
    MOSTRAR("M?????", "Edição de texto em memória com 'valid'", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_FILTRO_TECLAS()
    MOSTRAR("M?????", "Edição de texto em memória com filtro de tecla", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/
