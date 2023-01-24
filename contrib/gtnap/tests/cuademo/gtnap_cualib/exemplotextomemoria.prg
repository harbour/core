/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

**************************
PROC EXEMPLO_TEXTO_MEMORIA
**************************
LOCAL V_Janela
*
CUA20 @ 13,20,25,80 JANELA V_Janela ;
     TITULO "Escolha o tipo de janela" SUBTITULO "%T;exibição/edição de texto em memória";
     AJUDA "T?????"
*
ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO"exibição de #texto em memória (read-only)" ;
   ACAO TST_EXIBE_TEXTO_MEMORIA_READ_ONLY() AJUDA "P14973"
ADDOPCAO V_Janela TEXTO"edição de texto em #memória com confirmações" ;
   ACAO TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_CONFIRMACOES() AJUDA "P06747"
ADDOPCAO V_Janela TEXTO "edição de texto em m#emória com 'valid'" ;
    ACAO TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_VALID() AJUDA "P14975"
ADDOPCAO V_Janela TEXTO "edição de texto em memó#ria com filtro de tecla" ;
    ACAO TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_FILTRO_TECLAS() AJUDA "P14977"
*
ATIVE(V_Janela)
*

// STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_ONLY
//     RETURN

*******************************************
STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_ONLY
*******************************************
LOCAL C_Texto,V_Janela, L_OK
*
@ 5,15,MAXROW()-4,MAXCOL()-10 JANELA V_Janela ;
  TITU "Exibe texto em memória" SUBTITULO "%T;Read-only" ;
  TECLAS {"F2=fecha texto"} ;
  AJUDA "T?????"

//
//  Fran: This code works on Windows and Linux
//
C_Texto := MEMOREAD("../dados/textotes.txt")

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    C_Texto := MEMOREAD("dados\textotes.txt")
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    C_Texto := MEMOREAD("/opt/cuadados/textotes.txt")
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif

ESPECIALIZE V_Janela TEXTO C_Texto TERMINAR {K_F2} EDITA .F.
*
L_OK :=  ATIVE(V_Janela)
DO WHILE L_OK
   MOSTRAR("M15602","Usuário teclou F2")
   L_OK :=  ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*


// STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_CONFIRMACOES
// RETURN

*************************************************************
STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_CONFIRMACOES
*************************************************************
LOCAL C_Texto,V_Janela, L_OK
*
@ 5,15,MAXROW()-4,MAXCOL()-10 JANELA V_Janela ;
  TITU "Exibe texto em memória" SUBTITULO "%T;com confirmações" ;
  TECLAS {"F2=fecha texto"} ;
  AJUDA "T?????"

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    C_Texto := MEMOREAD("dados\textotes.txt")
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    C_Texto := MEMOREAD("/opt/cuadados/textotes.txt")
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif

//
//  Fran: This code works on Windows and Linux
//
C_Texto := MEMOREAD("../dados/textotes.txt")

ESPECIALIZE V_Janela TEXTO C_Texto TERMINAR {K_F2} EDITA .T. ;
            CONFIRMAR CONFIRMA_DADOS() ;
            DESISTIR CONFIRMA_CANCELAMENTO()
*
L_OK :=  ATIVE(V_Janela)
DO WHILE L_OK
   MOSTRAR("M15604","Usuário teclou F2")
   L_OK :=  ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*


STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_VALID
RETURN

// ******************************************************
// STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_VALID
// ******************************************************
// LOCAL C_Texto,V_Janela, L_OK
// *
// @ 5,15,MAXROW()-4,MAXCOL()-10 JANELA V_Janela ;
//   TITU "Exibe texto em memória" ;
//   SUBTITULO "%T;com 'valid';(tem de iniciar com 'CASA')" ;
//   TECLAS {"F2=fecha texto"} ;
//   AJUDA "T?????"

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    C_Texto := MEMOREAD("dados\textotes.txt")
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    C_Texto := MEMOREAD("/opt/cuadados/textotes.txt")
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// ESPECIALIZE V_Janela TEXTO C_Texto TERMINAR {K_F2} EDITA .T. ;
//             VALID COMECAR_COM_CASA(C_Texto)
// *
// L_OK :=  ATIVE(V_Janela)
// DO WHILE L_OK
//    MOSTRAR("M15606","Texto editado com sucesso. Usuário teclou F2")
//    L_OK :=  ATIVE(V_Janela)
// ENDDO
// DESTRUA V_Janela
*
// **************************
// STAT FUNC COMECAR_COM_CASA(C_Texto)
// **************************
// LOCAL L_OK := .T.
// IF UPPER(LEFT(C_Texto,4)) # "CASA"
//    L_OK := .F.
//    ALARME("M?????","Texto deve iniciar com a palavra 'CASA'")
// ENDIF
// RETURN L_OK
*

STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_FILTRO_TECLAS
RETURN

// **************************************************************
// STAT PROC TST_EXIBE_TEXTO_MEMORIA_READ_WRITE_COM_FILTRO_TECLAS
// **************************************************************
// LOCAL C_Texto,V_Janela, L_OK
// *
// @ 5,15,MAXROW()-4,MAXCOL()-10 JANELA V_Janela ;
//   TITU "Exibe texto em memória" ;
//   SUBTITULO "%T;com filtro de tecla;(minúsculas transformadas em maiúsculas)" ;
//   TECLAS {"F2=fecha texto"} ;
//   AJUDA "T?????"

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    C_Texto := MEMOREAD("dados\textotes.txt")
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    C_Texto := MEMOREAD("/opt/cuadados/textotes.txt")
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// ESPECIALIZE V_Janela TEXTO C_Texto TERMINAR {K_F2} EDITA .T. ;
//             FILTRO CONVERTE_PARA_UPPER()
// *
// L_OK :=  ATIVE(V_Janela)
// DO WHILE L_OK
//    MOSTRAR("M15608","Usuário teclou F2")
//    L_OK :=  ATIVE(V_Janela)
// ENDDO
// DESTRUA V_Janela
*
// *****************************
// STAT FUNC CONVERTE_PARA_UPPER()
// *****************************
// LOCAL N_Lastkey := LASTKEY()
// LOCAL C_Retorno
// IF N_Lastkey >= 97 .AND. N_Lastkey <= 122   // letras minúsculas
//    C_Retorno := UPPER(CHR(N_Lastkey))
// ELSE
//    C_Retorno := NIL
// ENDIF
// RETURN C_Retorno
// *
// *********************


