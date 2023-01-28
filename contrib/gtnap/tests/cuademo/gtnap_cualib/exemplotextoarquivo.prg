/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

**************************
PROC EXEMPLO_TEXTO_ARQUIVO
**************************
LOCAL N_Handle,V_Janela, L_OK
*
@ 5,15,MAXROW()-4,MAXCOL()-10 JANELA V_Janela ;
  TITU "Exibe texto em disco" ;
  SUBTITULO "%T" ;
  TECLAS {"F2=fecha texto"} ;
  AJUDA "T?????"

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    N_Handle := FOPEN("dados\textotes.txt")
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    N_Handle := FOPEN("/opt/cuadados/textotes.txt")
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif

//
//  Fran: This code works on Windows and Linux
//
N_Handle := FOPEN("../dados/textotes.txt")

ESPECIALIZE V_Janela ARQUIVOTEXTO N_Handle LARGURA 120 TERMINAR {K_F2}
*
L_OK :=  ATIVE(V_Janela)
DO WHILE L_OK
   MOSTRAR("M15600","Usuário teclou F2")
   L_OK :=  ATIVE(V_Janela)
ENDDO
FCLOSE(N_Handle)
DESTRUA V_Janela
*
*********************

