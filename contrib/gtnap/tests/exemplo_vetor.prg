#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_BROWSE_VETOR()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela de browse de vetor")
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples, sem grade, nem toolbar", {|| TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM(.F.) })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção múltipla, com grade, com toolbar, e rolagem", {|| TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR(.T.) })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção estendida, com grade, sem barra de rolagem", {|| TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR(.F.) })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples com AUTOCLOSE", {|| TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM(.T.) })
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

STATIC PROCEDURE TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM(L_AutoClose)
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
    LOCAL V_MenuVert, V_Button
    LOCAL V_Label1
    LOCAL V_Image, V_ImageView
    LOCAL C_String, V_Vetor
    LOCAL N_Cont, N_NumOpts

    C_String := "Browse de vetor" + hb_eol() + "sem grade, sem toolbar" + hb_eol() + "sem barra de rolagem"

    V_Vetor := { "Leite condensado", "Arroz tipo 1", "Acarajé", "Doce de leite",;
                       "Doces diversos", "Churrasco de gado", "Rapadura preta",;
                       "Amendoim torrado", "Panelada de bucho" }

    N_NumOpts := LEN(V_Vetor)

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 3)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 1)
    V_MenuVert := NAP_MENUVERT_CREATE(10)  // 10 = Visible options (with vertical scroll)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_Image := NAP_IMAGE_FROM_FILE(DIRET_BMPS() + "logaspec.bmp")
    V_ImageView := NAP_IMAGEVIEW_CREATE()
    V_Button := NAP_BUTTON_PUSH()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_IMAGEVIEW_IMAGE(V_ImageView, V_Image)
    NAP_IMAGEVIEW_SIZE(V_ImageView, 100, 100)
    NAP_IMAGEVIEW_SCALE(V_ImageView, ekNAP_SCALE_ASPECTDW)

    FOR N_Cont := 1 TO N_NumOpts
        NAP_MENUVERT_ADD(V_MenuVert, V_Vetor[N_Cont], { || EXIBIR_ITEM_SELECIONADO(V_Vetor, V_MenuVert, L_AutoClose) })
    NEXT

    NAP_BUTTON_TEXT(V_Button, "F5 = exibir selecionados")
    NAP_BUTTON_ONCLICK(V_Button, {|| EXIBIR_ITEM_SELECIONADO(V_Vetor, V_MenuVert, L_AutoClose) })

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_IMAGEVIEW(V_Layout2, V_ImageView, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout2, V_MenuVert, 1, 0)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button, 0, 2)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Image in aligned to top
    NAP_LAYOUT_VALIGN(V_Layout2, 0, 0, ekNAP_ALIGN_TOP)
    // Horizontal space between image and menu
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 50)
    // Vertical space between menu and button
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 50)
    // Button is alignet to left
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    // Globar border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 30, 75)

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Browse de vector")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE EXIBIR_ITEM_SELECIONADO(V_Vetor, V_MenuVert, L_AutoClose)
    LOCAL N_Pos  := NAP_MENUVERT_SELECTED(V_MenuVert)
    MOSTRAR("M15664", "A posição selecionada foi " + LTRIM(STR(N_Pos)), "Informação")
    MOSTRAR("M15666", "A posição selecionada contém '" + V_Vetor[N_Pos] + "'", "Informação")

    IF L_AutoClose
        NAP_WINDOW_STOP_MODAL(0)
    ENDIF

    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR(L_ComRolagen)
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
    LOCAL V_Label1
    LOCAL V_Listbox
    LOCAL V_Button1, V_Button2, V_Button3
    LOCAL C_String, V_Vetor
    LOCAL N_Cont, N_NumOpts

    C_String := "Browse de vetor" + hb_eol() + "com grade, com toolbar" + hb_eol()

    IF L_ComRolagen
        C_String := C_String + "com barra de rolagem"
    ELSE
        C_String := C_String + "sem barra de rolagem"
    ENDIF

    V_Vetor := { "Leite condensado", "Arroz tipo 1", "Acarajé", "Doce de leite",;
                       "Doces diversos", "Churrasco de gado", "Rapadura preta",;
                       "Amendoim torrado", "Panelada de bucho" }

    N_NumOpts := LEN(V_Vetor)
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 5)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_Listbox := NAP_LISTBOX_CREATE()
    V_Button1 := NAP_BUTTON_PUSH()
    V_Button2 := NAP_BUTTON_PUSH()
    V_Button3 := NAP_BUTTON_PUSH()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    //NAP_LABEL_BGCOLOR(V_Label1, NAP_COLOR_RED())

    NAP_LISTBOX_CHECKBOX(V_Listbox, .T.)

    // NAppGUI ListBox shows scrollbar automatically when needed
    IF L_ComRolagen
        NAP_LISTBOX_SIZE(V_Listbox, 300, 60)
    ELSE
        NAP_LISTBOX_SIZE(V_Listbox, 300, 200)
    ENDIF

    FOR N_Cont := 1 TO N_NumOpts
        NAP_LISTBOX_ADD_ELEM(V_Listbox, V_Vetor[N_Cont])
    NEXT

    DEFAULT_SELECIONADOS(V_Listbox)

    NAP_BUTTON_TEXT(V_Button1, "Enter = exibir itens selecionados")
    NAP_BUTTON_TEXT(V_Button2, "Voltar seleção para o 'default'")
    NAP_BUTTON_TEXT(V_Button3, "Barra de espaço = marcar")
    NAP_BUTTON_ONCLICK(V_Button1, {|| EXIBIR_ITENS_SELECIONADO(V_Vetor, V_Listbox) })
    NAP_BUTTON_ONCLICK(V_Button2, {|| DEFAULT_SELECIONADOS(V_Listbox) })
    NAP_BUTTON_ONCLICK(V_Button3, {|| MARCAR_ITENS_SELECIONADO(V_Listbox) })

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LISTBOX(V_Layout1, V_Listbox, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button1, 0, 2)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button2, 0, 3)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button3, 0, 4)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Vertical space between listbox and buttons
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 20)
    // Vertical space between buttons
    NAP_LAYOUT_VMARGIN(V_Layout1, 2, 5)
    NAP_LAYOUT_VMARGIN(V_Layout1, 3, 5)
    // Buttons are alignet to left
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 3, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 4, ekNAP_ALIGN_LEFT)
    // Globar border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 40, 30, 40)

    // Adding toolbar
    V_Layout2 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout2)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Browse de vector")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE DEFAULT_SELECIONADOS(V_Listbox)
    LOCAL N_Cont, N_NumOpts
    LOCAL L_Check
    LOCAL V_Sel := {2, 4}

    N_NumOpts := NAP_LISTBOX_COUNT(V_Listbox)

    FOR N_Cont := 1 TO N_NumOpts
        L_Check := AScan(V_Sel, N_Cont) > 0
        NAP_LISTBOX_CHECK(V_Listbox, N_Cont, L_Check)
    NEXT

    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE EXIBIR_ITENS_SELECIONADO(V_Vetor, V_Listbox)
    LOCAL N_Cont, N_NumOpts
    LOCAL L_Check
    LOCAL C_Message := ""

    N_NumOpts := NAP_LISTBOX_COUNT(V_Listbox)

    FOR N_Cont := 1 TO N_NumOpts
        L_Check := NAP_LISTBOX_CHECKED(V_Listbox, N_Cont)
        IF L_Check
            C_Message := C_Message + hb_ntos(N_Cont) + "- " + V_Vetor[N_Cont] + hb_eol()
        ENDIF
    NEXT

    MOSTRAR("M?????", C_Message, "Itens Selecionado")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE MARCAR_ITENS_SELECIONADO(V_Listbox)
    LOCAL N_Cont, N_NumOpts
    LOCAL L_Sel, L_Check

    N_NumOpts := NAP_LISTBOX_COUNT(V_Listbox)

    FOR N_Cont := 1 TO N_NumOpts
        L_Sel := NAP_LISTBOX_SELECTED(V_Listbox, N_Cont)
        IF L_Sel
            L_Check := NAP_LISTBOX_CHECKED(V_Listbox, N_Cont)
            NAP_LISTBOX_CHECK(V_Listbox, N_Cont, .NOT. L_Check)
        ENDIF
    NEXT

    RETURN


