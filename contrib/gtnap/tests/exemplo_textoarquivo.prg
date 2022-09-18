#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_TEXTO_ARQUIVO()

    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
    LOCAL V_Label1, V_Button1
    LOCAL V_TextView
    LOCAL C_Text

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 3)
    V_Label1 := NAP_LABEL_WITH_TEXT("Escribe texto em disco")
    V_Button1 := NAP_BUTTON_PUSH()
    V_TextView := NAP_TEXTVIEW_CREATE()
    C_Text := READ_TEXT_FROM_FILE("dados\textotes.txt")
    NAP_BUTTON_TEXT(V_Button1, "F2=Fecha texto")
    NAP_TEXTVIEW_SIZE(V_TextView, 1000, 500)
    NAP_TEXTVIEW_FSIZE(V_TextView, 18)
    NAP_TEXTVIEW_WRITE(V_TextView, C_Text)
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_TEXTVIEW(V_Layout1, V_TextView, 0, 1)
    NAP_LAYOUT_BUTTON(V_Layout1, V_Button1, 0, 2)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 2, ekNAP_ALIGN_LEFT)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 10)
    NAP_LAYOUT_VMARGIN(V_Layout1, 1, 30)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 20, 20, 20)

    V_Layout2 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"copia.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Copiar", "Calculadora", "Ajuda", "Saida"})
    NAP_PANEL_LAYOUT(V_Panel, V_Layout2)

    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escribe texto em disco")
    NAP_WINDOW_FOCUS(V_Janela, V_Layout1, 0, 1)  // KeyBoard focus in textview by default
    NAP_WINDOW_MODAL(V_Janela)
    RETURN
