#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_AUXILIARES()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela auxiliar")
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de informação (com parada)", {|| TST_INFORMACAO_COM_PARADA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de informação (sem parada)", {|| TST_INFORMACAO_SEM_PARADA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de advertência", {|| TST_ADVERTENCIA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de erro", {|| TST_ERRO() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de confirmação", {|| TST_CONFIRMACAO() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela de pergunta", {|| TST_PERGUNTA() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escolha o tipo de janela")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_INFORMACAO_COM_PARADA()
    PERGUN("Teste de janela de informação" + hb_eol() + "(com parada)", {"Ok"}, 1, .F., "Informação", "info")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_INFORMACAO_SEM_PARADA()
    PERGUN("Teste de janela de informação" + hb_eol() + "(sem parada)", {"Ok"}, 1, .F., "Informação", "info")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ADVERTENCIA()
    PERGUN("Teste de janela de advertência", {"Ok"}, 1, .F., "Advertência", "warn")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ERRO()
    PERGUN("Teste de janela de erro", {"Ok"}, 1, .F., "Erro", "error")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_CONFIRMACAO()
    LOCAL N_Opt := PERGUN("Teste de rotina de confirmação." + hb_eol() + "Escolha entre 'sim' e 'não'", {"Sim", "Não"}, 1, .F., "Confirmação", "quest")

    IF N_Opt == 1
        MOSTRAR("M?????", "Foi escolhido 'Sim'", "Informação")
    ELSEIF N_Opt == 2
        MOSTRAR("M?????", "Foi escolhido 'Não'", "Informação")
    ELSE
        MOSTRAR("M?????", "Nenhuma opção escolhida", "Informação")
    ENDIF

    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_PERGUNTA()
    LOCAL N_Opt := PERGUN("Teste de rotina de pergunta" + hb_eol() + "ao usuário", {"Ok", "Cancelar", "Desistir"}, 1, .F., "Favor responder", "quest")

    IF N_Opt >= 1 .AND. N_Opt <= 3
        MOSTRAR("M?????", "A opção escolhida foi '" + hb_ntos(N_Opt) + "'", "Informação")
    ELSE
        MOSTRAR("M?????", "Nenhuma opção escolhida", "Informação")
    ENDIF

    RETURN
