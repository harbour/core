/* encoding: cp850 */
*
* Contém rotinas que a CUA cita, mas que estão em outros PRGs.
* Servem apenas para não dar erro de linkedição,
* não tendo tratamento dentro de cada rotina "stub".
*
#INCLUDE "ord.ch"
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"
#INCLUDE "common.ch"

***********
PROC XXHELP(C_CDJANE,C_CABEC,C_CDGET_CDOPCAO,V_LST_CDGET_CDOPCAO)
***********
LOCAL C_LST_CDGET_CDOPCAO := "", N_CT
IF C_CDJANE==NIL
   C_CDJANE := ""
ENDIF
IF C_CABEC==NIL
   C_CABEC := ""
ENDIF
IF C_CDGET_CDOPCAO==NIL
   C_CDGET_CDOPCAO := ""
ENDIF
IF V_LST_CDGET_CDOPCAO==NIL
   V_LST_CDGET_CDOPCAO := {}
ENDIF
*
FOR N_CT := 1 TO LEN (V_LST_CDGET_CDOPCAO)
   IF V_LST_CDGET_CDOPCAO[N_CT,1]==NIL    //!! DEPOIS REMOVER O IF E DEIXAR O ELSE
      C_LST_CDGET_CDOPCAO += "??????"+" - "+;       //!! QUANDO CDOPCAO FOR OBRIGAT´?¢RIO
                               ALLTRIM(V_LST_CDGET_CDOPCAO[N_CT,2])+";"
   ELSE
      C_LST_CDGET_CDOPCAO += V_LST_CDGET_CDOPCAO[N_CT,1]+" - "+;
                               ALLTRIM(V_LST_CDGET_CDOPCAO[N_CT,2])+";"
   ENDIF
NEXT
*
IF EMPTY(C_CDGET_CDOPCAO)
    MOSTRAR("M?????","A cláusula AJUDA contém ;'"+C_CdJane+"' para a tela,;"+;
                      "'"+C_CABEC+"' para título.")
ELSE
    MOSTRAR("M15562","A cláusula AJUDA contém ;'"+C_CdJane+"' para a tela,;"+;
                     "'"+C_CABEC+"' para título,;"+;
                     "'"+C_CDGET_CDOPCAO+"' para o campo atual e tem como lista de campos:;;"+;
                     C_LST_CDGET_CDOPCAO)
ENDIF
 // *
// **************
// FUNC TEM_SCOPE()
// **************
// LOCAL X_SCOPE_INI := DBORDERINFO(DBOI_SCOPETOP)
// LOCAL X_SCOPE_FIN := DBORDERINFO(DBOI_SCOPEBOTTOM)
// LOCAL L_USA_SCOPE := .F.
// LOCAL C_STRING    := ""
// LOCAL V_AUX_INI   := {}
// LOCAL V_AUX_FIN   := {}
// *
// IF X_SCOPE_INI # NIL
//    IF VALTYPE(X_SCOPE_INI) == "C"
//       C_STRING := "'" + X_SCOPE_INI + "'"
//    ENDIF
//    IF VALTYPE(X_SCOPE_INI) == "D"
//       C_STRING := "'" + DTOC(X_SCOPE_INI) + "'"
//    ENDIF
//    IF VALTYPE(X_SCOPE_FIN) == "L"
//       C_STRING := "'" + IIF(X_SCOPE_INI,".T.",".F.") + "'"
//    ENDIF
//    IF VALTYPE(X_SCOPE_INI) == "N"
//       C_STRING := "'" + ALLTRIM(STR(X_SCOPE_INI)) + "'"
//    ENDIF
//    *
//    DO WHILE LEN(C_STRING) > 50
//       AADD(V_AUX_INI,SUBSTR(C_STRING,1,50))
//       C_STRING := SUBSTR(C_STRING,51)
//    ENDDO
//    AADD(V_AUX_INI,C_STRING)
//    *
//    L_USA_SCOPE := .T.
// ENDIF
// *
// IF X_SCOPE_FIN # NIL
//    IF X_SCOPE_FIN # X_SCOPE_INI
//       IF VALTYPE(X_SCOPE_FIN) == "C"
//          C_STRING := "'" + X_SCOPE_FIN + "'"
//       ENDIF
//       IF VALTYPE(X_SCOPE_FIN) == "D"
//          C_STRING := "'" + DTOC(X_SCOPE_FIN) + "'"
//       ENDIF
//       IF VALTYPE(X_SCOPE_FIN) == "L"
//          C_STRING := "'" + IIF(X_SCOPE_FIN,".T.",".F.") + "'"
//       ENDIF
//       IF VALTYPE(X_SCOPE_FIN) == "N"
//          C_STRING := "'" + ALLTRIM(STR(X_SCOPE_FIN)) + "'"
//       ENDIF
//       *
//       DO WHILE LEN(C_STRING) > 50
//          AADD(V_AUX_FIN,SUBSTR(C_STRING,1,50))
//          C_STRING := SUBSTR(C_STRING,51)
//      ENDDO
//      AADD(V_AUX_FIN,C_STRING)
//       *
//       L_USA_SCOPE := .T.
//    ENDIF
// ENDIF
// *
// RETURN { L_USA_SCOPE, V_AUX_INI, V_AUX_FIN }
// *
// ***************
// FUNC GS_TIMEOUT
// ***************
// RETURN 60*60 // 1 hora
// *
// **************
// PROC DATAINVA
// *************
// ALARME("M?????","Esta data n´?¢o existe no calend´?¢rio")
// *
// **********************
// PROC LOGAFONT_GENERICO (N_RETROCEDE_CALLSTACK,C_TPLOGAFONT,;
//                         C_ALIAS_REAL,N_INDEXORD,C_MENSLOGA)



**********************
*
*********************************
FUNC LOGAINFO_ID_TELA_RELAT_BOTAO (C_STR_ID,C_CDTELA_CDRELA_CDBOTA)
*********************************
RETURN .T.
*
// **************************
// FUNC SERIE_EQUIPE_DE_TESTE()
// **************************
// RETURN .T.
// *
// ***************
// FUNC XDBSKIPPER (N_AVANCO)  // usada na sele´?¢´?¢o em arquivo
// ***************
// RETURN __DBSKIPPER(N_AVANCO)
// *
// ************
// PROC XDBGOTO (N_RECNO)
// ************
// DBGOTO(N_RECNO)
// *
// *************
// PROC XDBGOTOP
// *************
// DBGOTOP()
// *
// ****************
// PROC XDBGOBOTTOM
// ****************
// DBGOBOTTOM()
// *
// *************
// PROC GOTO_EOF
// *************
// DBGOTO(LASTREC()+1)
// *

// ************
// PROC XDBSKIP (N_AVANCO)
// ************
// DBSKIP(N_AVANCO)
// *
// ************
// FUNC GS_CDUSUA()
// ************
// RETURN "xxxxx"
// *
// *****************
// PROC LOGA_AJTELAT(C_CdTela,C_Cabec,V_Lst_CdGet)  // LOGAR conte´?¢do de telas
// *****************
// IF V_Lst_CdGet==NIL
//    V_Lst_CdGet := {}
// ENDIF
// *
// // ALERT(C_CDTELA+" logada, com "+LTRIM(STR(LEN(V_Lst_CdGet)))+" campos")
// *
// *****************
// PROC LOGA_AJMENST(C_CDTELA,C_CDGet,C_CdMens,C_SubCabec)  // LOGAR conte´?¢do de telas
// *****************
// // ALERT("ALARME() - "+C_CDTELA+", campo "+C_CDGet+", mensagem "+C_CDMENS+;
// //       " e texto com tamanho "+LTRIM(STR(LEN(C_SUBCABEC))))
// *
// **********************
// PROC LOGAFONT_AMBIENTE (C_TPLOGAFONT,C_ALIAS_REAL,C_CDBOTAO,C_MENSLOGA)
// **********************
// // ALERT(C_TPLOGAFONT+" "; "+C_ALIAS_REAL+" ; "+C_CDBOTAO+" ; "+C_MENSLOGA)
// *
// *******************
// PROC EXISTENCIA_EXE
// *******************
// PROC GS_EXTENSAO_EXE
// ********************
// FUNC DIREXE
// RETURN "q:\aspec\veratual\exe\"
// *******************
// PROC REFRESH_RECORD
// *******************
// * Rel´?¢ o registro corrente do arquivo corrente novamente.
// *
// * Muito embora nos exemplos fornecidos junto com Clipper conste que o comando
// * "SKIP 0" tem o mesmo efeito, na pr´?¢tica o "GOTO RECNO()" ´?¢ melhor pois:
// *  - O comando SKIP 0 libera os buffers da esta´?¢´?¢o para a rede, mas n´?¢o
// *    atualiza totalmente os buffers da esta´?¢´?¢o se houver modifica´?¢´?¢o nos
// *    arquivos executados por outra esta´?¢´?¢o.
// *  - Caso se esteja no EOF() e outra esta´?¢´?¢o preencha este registro, o SKIP 0
// *    atualiza o LASTREC(), os dados do registro, mas o EOF() e BOF()
// *    continuam .T.
// *  - J´?¢ o GOTO RECNO() atualizou todos os itens acima, da esta´?¢´?¢o para a
// *    rede e vice-versa.
// *
// GOTO RECNO()
// *
// *************
// FUNC GS_SERIE
// *************
// RETURN "99999"
// *
*************
FUNC G_SGSIST
*************
RETURN "EM"
*
****************
FUNC EHPRINCIPAL
****************
RETURN .T.
*
// ****************
// FUNC SERIE_TESTE
// ****************
// RETURN "99999"
// *
// **************************
// FUNC SERIE_DESENVOLVIMENTO
// **************************
// RETURN "99999"
// *
// **************
// FUNC SERIE_SAN
// **************
// RETURN "99999"
// *
// ****************************
// FUNC SERIE_UNIDADE_CONVERSAO
// ****************************
// RETURN "99999"
// *
// *******************
// FUNC SERIE_PRODUCAO
// *******************
// RETURN "99999"
// *
// ********************
// FUNC SERIE_ASPEC_INI
// ********************
// RETURN "99999"
*
**************
FUNCTION TROCA ( C_String , C_Alfa1 , C_Alfa2 )
**************
LOCAL N_Cont , N_Len
N_Len := LEN(C_Alfa1)
FOR N_Cont := 1 TO N_Len
   IF SUBSTR(C_Alfa1,N_Cont,1) $ C_String
      *
      #DEFINE B_CHR_ACHAR   SUBSTR(C_Alfa1,N_Cont,1)
      #DEFINE B_SUBSTITUTO  SUBSTR(C_Alfa2,N_Cont,1)
      *
      C_String := STRTRAN( C_String , B_CHR_ACHAR , B_SUBSTITUTO )
      *
      #UNDEF B_CHR_ACHAR
      #UNDEF B_SUBSTITUTO
   ENDIF
NEXT
RETURN C_String
// *
// #INCLUDE "xx.ch"
// ***************
// FUNCTION XUPPER ( C_String )
// ***************
// RETURN TROCA(UPPER(C_String), _AC_MINUS , _AC_MAIUS )
// *
// ***************
// FUNCTION XLOWER ( C_String )
// ***************
// RETURN TROCA(LOWER(C_String), _AC_MAIUS , _AC_MINUS )
// *
// ************
// FUNC TIRACEN (C_String)
// ************
// RETURN TROCA(C_String, _AC_UM_SIM , _AC_UM_NAO )
// *
// ****************
// PROCEDURE ERRCTRL (C_CDMENS,C_TXT_MENSAGEM)
// ****************
// * Estabelecer c´?¢digos gen´?¢ricos, se nenhum for especificado no comando "ASSUME"
// DEFAULT C_CDMENS       TO "M10694"
// DEFAULT C_TXT_MENSAGEM TO "Houve uma condi´?¢´?¢o invalida na execu´?¢´?¢o do programa;"+;
//                           "por isso ser´?¢ feito um cancelamento para evitar que;"+;
//                           "possa haver danos ´?¢s informa´?¢´?¢es armazenadas.       "
// *
// ALARME(C_CDMENS,C_TXT_MENSAGEM)
// *
// ***************
// FUNC HB_HASH_CI // CI=Case_Insensitive
// ***************
// LOCAL H_HASH_CI
// *
// H_HASH_CI := HB_HASH()
// HB_HCASEMATCH(H_HASH_CI,.F.)
// *
// RETURN H_HASH_CI
// *
// ********
// FUNC COR ( C_TpCor )
// ********
// LOCAL C_Cor
// *
// * Nas cores de frente o "+" indica intenso.
// * Nas cores de fundo o "*" foi configurado para intenso pelo SETBLINK().
// IF C_TpCor == _COR_MENSAGEM_ERRO
//    * reverso vermelho
//    C_Cor := "W/R,R/W,N,,W/R"
// ELSEIF C_TpCor == _COR_MENSAGEM_ADVERTENCIA
//    * reverso amarelo
//    C_Cor := "N/GR*,GR+/N,N,,N/GR*"
// ELSEIF C_TpCor == _COR_MENSAGEM_OK
//    * reverso VERDE
//    C_Cor := "N/G*,G+/N,N,,N/G*"
// ELSEIF C_TpCor == _COR_PADRAO
//    * padrao sert´?¢o
//    C_Cor := "N/W,R/W,N/BG,N,,N/BG"
// ENDIF ERRO
// *
// RETURN C_Cor
// *
// ***********
// FUNC TABMEM (V_TAB, V_POSMOSTRA, V_POSRETORNO, V_TITULO, L_REMOVER_CERQUILHA)
// ***********
// LOCAL V_RETORNO
// LOCAL V_LARGURA, V_ALTURA, V_CONT1, V_CONT2
// LOCAL V_JANVET, V_VET_MOSTRA := {}, V_CELULA, V_STRCELULA
// LOCAL V_OPCAO
// LOCAL V_LIN_INI, V_COL_INI
// LOCAL C_TITULO_AUX
// LOCAL N_MAX_LARGURA_VET_MOSTRA
// *
// DEFAULT V_POSMOSTRA         TO {}        // por "default" mostra todas as colunas
// DEFAULT V_POSRETORNO        TO  1        // por "default" retorna a 1a. coluna
// DEFAULT V_TITULO            TO "Selecione com Enter"
// DEFAULT L_REMOVER_CERQUILHA TO .F.
// *
// N_MAX_LARGURA_VET_MOSTRA := LEN(V_TITULO)
// *
// IF EMPTY(V_TAB)
//    V_RETORNO := NIL
//    ADVERTE("M11254","A tabela de ajuda (F4) n´?¢o ser´?¢ exibida,;"+;
//                     "pois n´?¢o existe nenhum item a sugerir...")
// ELSEIF LEN(V_TAB)==1       // se somente 1 elemento, selecionar automaticamente
//    V_RETORNO := V_TAB[1,V_POSRETORNO]
// ELSE
//    * preencher vetor para mostrar ao usu´?¢rio
//    FOR V_CONT1 := 1 TO LEN(V_TAB)
//        AADD(V_VET_MOSTRA,"")        // adicionar nova linha
//        FOR V_CONT2 := 1 TO LEN(V_TAB[V_CONT1])
//            IF LEN(V_POSMOSTRA)==0 .OR. ASCAN(V_POSMOSTRA,V_CONT2) # 0
//               V_CELULA := V_TAB[V_CONT1,V_CONT2]
//               IF VALTYPE(V_CELULA) == "C"
//                  V_STRCELULA := V_CELULA
//               ELSEIF VALTYPE(V_CELULA) == "N"
//                  IF V_CELULA < 999
//                     V_STRCELULA := STR(V_CELULA,03)
//                  ELSEIF V_CELULA < 999999
//                     V_STRCELULA := STR(V_CELULA,06)
//                  ELSEIF V_CELULA < 999999999
//                     V_STRCELULA := STR(V_CELULA,09)
//                  ELSE
//                     V_STRCELULA := STR(V_CELULA,12)
//                  ENDIF
//               ELSEIF VALTYPE(V_CELULA) == "D"
//                  V_STRCELULA := DTOC(V_CELULA)
//               ENDIF
//               V_VET_MOSTRA[V_CONT1] := V_VET_MOSTRA[V_CONT1] + " " + V_STRCELULA
//            ENDIF
//        NEXT
//        N_MAX_LARGURA_VET_MOSTRA := MAX(N_MAX_LARGURA_VET_MOSTRA,LEN(V_VET_MOSTRA[V_CONT1]))
//    NEXT
//    *
//    IF L_REMOVER_CERQUILHA
//       FOR V_CONT1 := 1 TO LEN(V_VET_MOSTRA)
//           V_VET_MOSTRA[V_CONT1] := STRTRAN(V_VET_MOSTRA[V_CONT1],"#"," ")
//       NEXT
//    ENDIF
//    *
//    * calcular "largura" e "altura" da tabela
//    * +4 - margens da janela
//    * +3 - espa´?¢o para a barra de rolagem vertical, se existir
//    V_LARGURA := MIN(N_MAX_LARGURA_VET_MOSTRA+4+4            ,MAXCOL())
//    V_ALTURA  := MIN(LEN(V_VET_MOSTRA)+LINHAS_TIT(V_TITULO)+4,MAXROW())
//    *
//    * ajustar posi´?¢´?¢o inicial da tabela em rela´?¢´?¢o ´?¢ tela
//    * (procurar deixar ao lado do campo)
//    *
//    V_LIN_INI := ROW()-2
//    V_LIN_INI := MAX(0,ROW()-2)  // evitar que fique negativo
//    IF V_LIN_INI+V_ALTURA  > MAXROW()
//       V_LIN_INI := MAXROW()-V_ALTURA
//    ENDIF
//    V_COL_INI := COL()+3
//    IF V_COL_INI+V_LARGURA > MAXCOL()
//       V_COL_INI := MAXCOL()-V_LARGURA
//    ENDIF
//    *
//    IF ";" $ V_TITULO
//       C_TITULO_AUX := "Selecione com Enter"  // modifica o t´?¢tulo da janela do Windows
//    ELSE
//       C_TITULO_AUX := V_TITULO
//    ENDIF
//    CUA20 @ V_LIN_INI,V_COL_INI,V_LIN_INI+V_ALTURA,V_COL_INI+V_LARGURA JANELA V_JANVET ;
//            TITULO C_TITULO_AUX SUBTITULO V_TITULO AJUDA "T02833"
//    *
//    CUA20 ESPECIALIZE V_JANVET SELECAO SIMPLES VETOR V_VET_MOSTRA ;
//          NAOROLAHORIZONTAL SEMGRADE SEMTOOLBAR AUTOCLOSE
//    *
//    IF (V_OPCAO := ATIVE(V_JANVET)) # 0
//       V_RETORNO := V_TAB[V_OPCAO,V_POSRETORNO]
//    ENDIF
//    *
// ENDIF
// *
// RETURN V_RETORNO
// *
// *********************
// PROC ADDESTRESTEN_DEF
// *********************
// *
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
// ********************
// STAT FUNC LINHAS_TIT(V_TITULO)  // Em Windows a rotina ´?¢ chamada de forma EST´?¢TICA.
// ********************
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
// ********************
// FUNC LINHAS_TIT(V_TITULO) // Em Linux foi necess´?¢rio deixa-la P´?¢BLICA. (Motivo desconhecido at´?¢ o momento)
// ********************
// #else
//    #erro "C´?¢digo n´?¢o adaptado para esta plataforma"
// #endif
// LOCAL N_LINHAS_TIT := IIF(EMPTY(V_TITULO),3,4)
// DO WHILE ";" $ V_TITULO
//    N_LINHAS_TIT++      // aumentar uma linha
//    V_TITULO := STRTRAN(V_TITULO,";","",,1)   // trocar s´?¢ uma ocorr´?¢ncia
// ENDDO
// RETURN N_LINHAS_TIT
// *

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)

//    #pragma begindump

//    #ifndef _WIN32_WINNT
//    #define _WIN32_WINNT 0x0501
//    #endif

//    #include <stdlib.h>
//    #include <time.h>
//    #include <windows.h>
//    #include <imagehlp.h>
//    #include <hbapi.h>
//    #include <hbapiitm.h>
//    #include <stdio.h>
//    #include <cpuid.h>
//    #include <string.h>
//    #include <ctype.h>

//    typedef int (__stdcall *MSGBOXAAPI)(IN HWND hWnd,
//            IN LPCSTR lpText, IN LPCSTR lpCaption,
//            IN UINT uType, IN WORD wLanguageId, IN DWORD dwMilliseconds);
//    typedef int (__stdcall *MSGBOXWAPI)(IN HWND hWnd,
//            IN LPCWSTR lpText, IN LPCWSTR lpCaption,
//            IN UINT uType, IN WORD wLanguageId, IN DWORD dwMilliseconds);

//    int MessageBoxTimeoutA(IN HWND hWnd, IN LPCSTR lpText,
//        IN LPCSTR lpCaption, IN UINT uType,
//        IN WORD wLanguageId, IN DWORD dwMilliseconds);
//    int MessageBoxTimeoutW(IN HWND hWnd, IN LPCWSTR lpText,
//        IN LPCWSTR lpCaption, IN UINT uType,
//        IN WORD wLanguageId, IN DWORD dwMilliseconds);

//    #ifdef UNICODE
//        #define MessageBoxTimeout MessageBoxTimeoutW
//    #else
//        #define MessageBoxTimeout MessageBoxTimeoutA
//    #endif

//    #define MB_TIMEDOUT 32000

//    int MessageBoxTimeoutA(HWND hWnd, LPCSTR lpText, LPCSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds)
//    {
//        static MSGBOXAAPI MsgBoxTOA = NULL;

//        if (!MsgBoxTOA)
//        {
//            HMODULE hUser32 = GetModuleHandle(TEXT("user32.dll"));
//            if (hUser32)
//            {
//                MsgBoxTOA = (MSGBOXAAPI)GetProcAddress(hUser32, "MessageBoxTimeoutA");
//            }
//            else
//            {
//                return 0;
//            }
//        }

//        if (MsgBoxTOA)
//        {
//            return MsgBoxTOA(hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds);
//        }

//        return 0;
//    }

//    int MessageBoxTimeoutW(HWND hWnd, LPCWSTR lpText, LPCWSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds)
//    {
//        static MSGBOXWAPI MsgBoxTOW = NULL;

//        if (!MsgBoxTOW)
//        {
//            HMODULE hUser32 = GetModuleHandle(TEXT("user32.dll"));
//            if (hUser32)
//            {
//                MsgBoxTOW = (MSGBOXWAPI)GetProcAddress(hUser32, "MessageBoxTimeoutW");
//            }
//            else
//            {
//                return 0;
//            }
//        }

//        if (MsgBoxTOW)
//        {
//            return MsgBoxTOW(hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds);
//        }

//        return 0;
//    }

//    /* 2013/09/19 Alexandre Alencar
//     * Fun´?¢´?¢o WVW_MESSAGEBOXTIMEOUT(hwnd, lpText, lpCation, uType, dwSeconds)
//     * Exibe uma MessageBox padr´?¢o Windows, com timeout, retorna o bot´?¢o clicado ou MB_TIMEOUT
//     * se nenhum bot´?¢o for clicado e o tempo expirou
//     */

//    HB_FUNC( WVW_MESSAGEBOXTIMEOUT )
//    {
//       //HMODULE hUser32 = LoadLibrary(TEXT("user32.dll"));
//       DWORD iRet = 0;

//       //if (hUser32)
//       //{
//           iRet = MessageBoxTimeout((HWND)hb_parni( 1 ), TEXT(hb_parcx( 2 )), TEXT(hb_parcx( 3 )), hb_parni ( 4 ), 0, hb_parni ( 5 ) * 1000);
//       //    FreeLibrary(hUser32);
//       //}

//       hb_retnl( iRet );
//    }

//    #pragma enddump

// #elif defined(__PLATFORM__LINUX)|| defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    //? MEMVAR->NAO_ADAPTADO_PARA_LINUX
// #else
//    #erro "C´?¢digo n´?¢o adaptado para esta plataforma"
// #endif
// *
// ***************************************
