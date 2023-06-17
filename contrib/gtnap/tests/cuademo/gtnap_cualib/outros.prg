/* encoding: cp850 */

//
//                                Aspec - Informatica
//                            Direitos Autorais Reservado
//                        Variantes especificas da classe janela
//
#INCLUDE "xx.ch"
#INCLUDE "inkey.ch"
#INCLUDE "janela.ch"          // para usar os pseudo mÈtodos do objeto janela
#INCLUDE "setcurs.ch"
#INCLUDE "cua.ch"
#INCLUDE "common.ch"
#INCLUDE "mousecua.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "hbgtinfo.ch"
#INCLUDE "gtnap.ch"

/*---------------------------------------------------------------------------*/

FUNCTION Setup_nap(C_Title, N_Lines, N_Cols, B_Init)
    NAP_INIT(C_Title, N_Lines, N_Cols, B_Init)
    RETURN (.T.)

/*---------------------------------------------------------------------------*/

FUNCTION DIRET_BMPS(C_DIRET_NEW)
    STATIC C_DIRET_BMPS := ""
    LOCAL C_DIRET_ANT := C_DIRET_BMPS
    IF C_DIRET_NEW # NIL
        C_DIRET_BMPS := C_DIRET_NEW
    ENDIF
    RETURN C_DIRET_ANT

/*---------------------------------------------------------------------------*/

***********************
STAT FUNCTION Perguntar (C_Cabec_x, C_SubCabec, VC_Menu, VC_TxtBotoes, N_Default)
***********************
*
LOCAL VX_Janela , N_CursorAnt , N_Opcao, N_Cont, N_Col, N_Row, N_Ret, N_OptLen
LOCAL N_DeslocaCabecalho := 4

* Tirar cÛpia do vetor, pois ser· alterado aqui dentro...
LOCAL VC_Menu_Aux := ACLONE(VC_Menu)

DEFAULT N_Default TO 1

N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor

FOR N_Cont := 1 TO LEN(VC_Menu_Aux)
    VC_Menu_Aux[N_Cont] := " "+VC_Menu_Aux[N_Cont]+" "
NEXT

VX_Janela := MontarJanela(C_Cabec_x,C_SubCabec,VC_Menu_Aux,VC_TxtBotoes)
N_TP_Jan  := _JAN_PERGUNTAR

IF SOB_MODO_GRAFICO()
   ADDIMAGEM VX_Janela ARQUIVO DIRET_BMPS()+"b99903.abm" ;      //"pergunta.abm"
       COORDENADAS 00,00,02,04 AJUDA "B19123"
ENDIF

Ative(VX_Janela)

IF SOB_MODO_GRAFICO()
    N_Row := Lin1Livre(VX_Janela)
    N_Col := Col1Livre(VX_Janela)+N_DeslocaCabecalho

    FOR N_Cont := 1 TO LEN(VC_Menu)
        N_OptLen := LEN(VC_Menu[N_Cont])
        NAP_BUTTON(N_WindowNum, N_Row, N_Col, N_Row, N_Col + N_OptLen, {|| VC_Menu[N_Cont]}, NIL, .T., .F.)
        N_Col += N_OptLen + 2
    NEXT

    NAP_CUALIB_DEFAULT_BUTTON(N_Default)

    N_Opcao := NAP_WINDOW_MODAL(N_WindowNum, 0)

    IF N_Opcao >= NAP_MODAL_BUTTON_AUTOCLOSE .AND. N_Opcao <= NAP_MODAL_BUTTON_AUTOCLOSE + NAP_MAX_BUTTONS
        N_Opcao -= NAP_MODAL_BUTTON_AUTOCLOSE
    ELSE
        N_Opcao := 0   // NO OPTION SELECTED
    ENDIF

    Destrua VX_Janela

ELSE // NOT SOB_MODO_GRAFICO
    * montar o menu horizontal
    N_Opcao := MenuHorizontal(VX_Janela,VC_Menu_Aux,N_Default)
    Destrua VX_Janela
ENDIF

SET(_SET_CURSOR,N_CursorAnt)   // restaurar modo do cursor

RETURN N_Opcao

******************************
STATIC FUNCTION MenuHorizontal ( VX_Janela , VC_Menu , N_Default )
******************************
* Only for text-based terminals (not graphics)
LOCAL N_Cont, N_Row, N_Col, C_Teclas := ""
LOCAL C_TeclaAtalho
LOCAL V_RegiaoOpcoes := {}
LOCAL N_DeslocaCabecalho := 0

DispBegin()

* posicionar cursor
SETPOS(Lin1Livre(VX_Janela),Col1Livre(VX_Janela)+N_DeslocaCabecalho)

N_Row := ROW()   // supor inicialmente que o "default" o item 1
N_Col := COL()

FOR N_Cont := 1 TO LEN(VC_Menu)
    C_TeclaAtalho := XUpper(Left(Troca(VC_Menu[N_Cont]," [",""),1))
    AADD(V_RegiaoOpcoes,{C_TeclaAtalho,ROW(),COL(), ROW(),COL()+LEN(VC_Menu[N_Cont])-1})

    IF N_Cont==N_Default
       N_Row := ROW()
       N_Col := COL()
    ENDIF
    @ ROW(),COL() SAY VC_Menu[N_Cont]+SPACE(02) // espaÁamento entre botoes

    C_Teclas += C_TeclaAtalho
NEXT

DispEnd()

N_Default := MENUTO(VC_Menu,N_Row,N_Col,C_Teclas,N_Default,V_RegiaoOpcoes)

RETURN N_Default

********************
STAT FUNCTION MENUTO( VC_Menu, N_Row, N_Col, C_Teclas, N_Default, V_RegiaoOpcoes )
********************
* Only for text-based terminals (not graphics)
LOCAL N_Tecla
LOCAL C_CorInten
LOCAL N_mRow, N_mCol, N_PosBotao

C_CorInten := SUBSPOS(SetColor(), ",", 2)

@ N_Row, N_Col SAY VC_Menu[N_Default] Color C_CorInten
WHILE N_Tecla # K_ENTER .AND. N_Tecla # K_ESC
N_Tecla := INKEYX(0)

IF N_Tecla == K_LEFT
    IF N_Default # 1
        * tirar a cor intensa do botao anterior
        @ N_Row, Col()-Len(VC_Menu[N_Default]) SAY VC_Menu[N_Default]
        * colocar a cor intensa no novo botao
        N_Default--
        @ N_Row, Col()-Len(VC_Menu[N_Default+1])-Len(VC_Menu[N_Default])-2 ;
        SAY VC_Menu[N_Default] Color C_CorInten
    ENDIF
ELSEIF N_Tecla == K_RIGHT
    IF N_Default # Len(VC_Menu)
        * tirar a cor intensa do botao anterior
        @ N_Row, Col()-Len(VC_Menu[N_Default]) ;
        SAY VC_Menu[N_Default]+SPACE(02)
        * colocar a cor intensa no novo botao
        N_Default++
        @ N_Row, Col() SAY VC_Menu[N_Default] Color C_CorInten
    ENDIF
ELSEIF N_Tecla == K_ESC
    N_Default := 0
ELSEIF XUpper(Chr(N_Tecla)) $ C_Teclas
    N_Default := At(XUpper(Chr(N_Tecla)),C_Teclas)
    N_Tecla   := K_ENTER
ELSEIF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
    N_mRow := mRow()
    N_mCol := mCol()
    N_PosBotao := ASCAN(V_RegiaoOpcoes,{|V_Botao| ;
                        N_MRow >= V_Botao[2]   .AND. ;
                        N_MRow <= V_Botao[4]   .AND. ;
                        N_MCol >= V_Botao[3] .AND. ;
                        N_MCol <= V_Botao[5] })
    IF N_PosBotao # 0
        KEYBOARD REPL(CHR(K_LEFT),LEN(VC_Menu)-1)+; // ir para a primeira opcao
                REPL(CHR(K_RIGHT),N_PosBotao-1)+;  // deslocar atÔøΩ a selecionada
                CHR(K_ENTER)                       // teclar ENTER
    ENDIF
ENDIF
ENDDO

RETURN N_Default

*********************
STAT FUNCTION SUBSPOS( C_String, C_Delim, N_Pos )
*********************
*
* -> Retira numa string a posiÔøΩÔøΩo desejada de acordo com os delimitadores
*
LOCAL I, N_Posatu, C_Strret := ""
N_Posatu := 1
FOR I := 1 TO LEN(C_String)
   IF Substr(C_String,I,1) == C_Delim
      N_Posatu++
   ELSEIF N_Posatu == N_Pos
      C_Strret += Substr(C_String,I,1)
   ENDIF
NEXT
RETURN C_Strret

**********************
STAT FUNCTION Informar ( C_Cabec_x, C_SubCabec, VC_TxtBotoes, L_Parar, N_Segundos, C_ArqImagem, L_TipoMsgAguarde, N_ProgressBar )
**********************
LOCAL N_Tecla, VX_Janela , N_CursorAnt
LOCAL N_PaintRefresh_Old

DEFAULT L_Parar TO .T.
DEFAULT N_Segundos TO 0
DEFAULT L_TipoMsgAguarde TO .F.

N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor

VX_Janela := MontarJanela(C_Cabec_x,C_SubCabec,,VC_TxtBotoes)
N_TP_Jan  := _JAN_INFORMAR

IF SOB_MODO_GRAFICO()
   ADDIMAGEM VX_Janela ARQUIVO DIRET_BMPS() + C_ArqImagem COORDENADAS 00,01,01,04 AJUDA "B19125"
ENDIF

IF L_TipoMsgAguarde
    SetJanTipoMsgAguarde(VX_Janela,L_TipoMsgAguarde)
    SetProgressBar(VX_Janela,N_ProgressBar)
ENDIF

Ative(VX_Janela)

IF SOB_MODO_GRAFICO()
    IF L_Parar
        NAP_WINDOW_MODAL(N_WindowNum, N_Segundos)
        Destrua VX_Janela
    ENDIF
ELSE
    IF L_Parar .AND. N_Segundos == 0
    *
    * esperar a teclagem de ENTER ou ESC
    *
    N_Tecla := Inkey_(.T.)             // igual ao INKEY(0) com SET KEY"s
    *
    DO WHILE N_Tecla # K_ENTER .AND. N_Tecla # K_ESC ;
        .AND. .NOT. CLICOU_NA_JANELA(VX_Janela,N_Tecla)
        N_Tecla := Inkey_(.T.)
    ENDDO
    Destrua VX_Janela
    ELSEIF L_Parar .AND. N_Segundos # 0
    * SET KEYs nao funcionam.
    * Janela fecha sozinha apÛs algum tempo.
    *
    InkeyX(N_Segundos)
    Destrua VX_Janela
    ELSE
    SETPOS(ROW(),COL()-1)       // retroceder cursor em 1 posiÔøΩÔøΩo
    ENDIF
ENDIF

SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor

IF L_Parar
   RETURN NIL
ENDIF

RETURN VX_Janela

**************************
STAT FUNC CLICOU_NA_JANELA (VX_Janela,N_Tecla)
**************************
LOCAL L_Clicou := .F.
LOCAL N_mRow, N_mCol, N_RegiaoMouse, N_Keyboard
*
IF N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
   N_mRow := mRow()
   N_mCol := mCol()
   N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                  Lin1Livre(VX_Janela),;
                                  Col1Livre(VX_Janela),;
                                  Lin2Livre(VX_Janela),;
                                  Col2Livre(VX_Janela),;
                                  @N_Keyboard)
  //#INCLUDE "mousecua.ch"
  IF N_RegiaoMouse # FORA_DA_JANELA
     L_Clicou := .T.
  ENDIF
ENDIF
*                      IF N_RegiaoMouse == AREA_UTIL
RETURN L_Clicou

****************************
STATIC FUNCTION MontarJanela (C_Cabec_x, C_SubCabec, VC_Menu, VC_TxtBotoes)
****************************
LOCAL N_Largura , N_LargAux , N_Altura, VC_Subtitulo, VX_Janela
LOCAL N_LinIni, N_ColIni
LOCAL N_DeslocaCabecalho

DEFAULT VC_Menu      TO {}
DEFAULT VC_TxtBotoes TO {}

VC_Subtitulo = StrToVet_(C_SubCabec)
*
* calcular largura do box
*
N_Largura := 0                       // procurar maior tÌtulo
AEVAL(VC_Subtitulo,{|C_Elemento| N_Largura := MAX(N_Largura,LEN(C_Elemento))} )
*
* + 1 = espaÁo ‡ esquerda do botao (acrescentado na TecFunc)
* + 1 = espaÁo ‡ direita do botao (acrescentado na TecFunc)
* + 2 = espaÁo entre botoes
*
N_LargAux  := 0                      // largura das teclas de funÁıes
AEVAL(VC_TxtBotoes,{|C_Elemento| N_LargAux := N_LargAux + LEN(C_Elemento) + 1 + 1 + 2 })
N_Largura := MAX(N_LargAux,N_Largura)
*
* + 2 = espaÁo entre opÁoes
*
N_LargAux  := 0                      // largura do menu
AEVAL(VC_Menu  ,{|C_Elemento| N_LargAux := N_LargAux + LEN(C_Elemento) + 2 })
N_Largura := MAX(N_LargAux,N_Largura)
*
* + 2 = colunas (do box)
* + 2 = espaÁamentos laterais (um de cada lado)
* + 2 = reservado para a SCROLLBAR VERTICAL
*
N_Largura := N_Largura + 2 + 2 + 2
*
N_DeslocaCabecalho := 0

N_Altura := 2+;       // linhas do box
            IIF(EMPTY(VC_Menu),0,2)+;
            IIF(EMPTY(VC_TxtBotoes),0,2)

IF SOB_MODO_GRAFICO()  // reservar um mÌnimo de 3 linhas para exibir imagem
   N_Altura := N_Altura  + MAX(LEN(VC_Subtitulo),3)
ELSE
   N_Altura := N_Altura  + LEN(VC_Subtitulo)
ENDIF
*
* testar os limites da janela, j· que s„o determinados automaticamente
*
N_Altura  := MIN(N_Altura ,MAXROW()+1)
N_Largura := MIN(N_Largura,MAXCOL()+1)
*
N_LinIni := ROUND((MAXROW()+1-N_Altura)/2,0)
N_ColIni := ROUND((MAXCOL()+1-N_Largura)/2,0)
*

@ N_LinIni,N_ColIni,N_LinIni+N_Altura,N_ColIni+N_Largura ;
  Jane VX_Janela TITULO C_Cabec_x SubTitulo C_SubCabec Tecl VC_TxtBotoes ;
  DESLOCACAB N_DeslocaCabecalho AJUDA "T05735"
*
AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre ‡ quantidade de botıes de funÁ„o
*
RETURN VX_Janela
*

****************
PROCEDURE ALARME (C_CDMENS,C_SubCabec,N_SEGUNDOS)
****************
LOCAL C_COR_ANT

IF C_SubCabec # NIL

    IF SOB_MODO_GRAFICO()
        MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,"Importante","erro.abm")
    ELSE
        TONE(750,0)
        C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ERRO))
        ALARME_CDMENS_ATIVO(C_CDMENS) // ser· usado pela ajuda ao usu·rio
        MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,"Importante","erro.abm")
        ALARME_CDMENS_ATIVO("")
        LOGA_AJMENST(GetCdTelaTopo(),GetCdGET_ou_Menu_Topo(),C_CdMens,C_SubCabec)  // LOGAR conte˙do de telas
        SETCOLOR(C_COR_ANT)
    ENDIF
ENDIF

************************
FUNC ALARME_CDMENS_ATIVO(C_CDMENS_NEW)
************************
STAT C_CDMENS_ATIVO := ""
LOCAL C_CDMENS_OLD
*
C_CDMENS_OLD := C_CDMENS_ATIVO
IF C_CDMENS_NEW # NIL
   * setar o novo
   C_CDMENS_ATIVO := C_CDMENS_NEW
ENDIF

RETURN C_CDMENS_OLD  // retornar o estado anterior

*****************
PROCEDURE ADVERTE (C_CDMENS, C_SubCabec,N_SEGUNDOS)
*****************
LOCAL C_COR_ANT

TONE(750,0)

IF C_SubCabec # NIL
    IF .NOT. SOB_MODO_GRAFICO()
        C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ADVERTENCIA))
    ENDIF

    MOSTRAR(C_CDMENS,C_SubCabec,{GL_ENTER+"=OK"},,.F.,N_SEGUNDOS,"AdvertÍncia","adverten.abm")

    IF .NOT. SOB_MODO_GRAFICO()
        SETCOLOR(C_COR_ANT)
    ENDIF
ENDIF

****************
FUNCTION MOSTRAR ( C_CDMENS, C_SubCabec , V_TECLAS , L_PARAR, L_MUDA_COR, N_SEGUNDOS, C_Cabec_x, C_ArqImagem )
****************
LOCAL C_COR_ANT, V_JAN
DEFAULT L_MUDA_COR TO .T.
DEFAULT C_Cabec_x     TO "InformaÁ„o"
DEFAULT C_ArqImagem TO "informac.abm"
*
ASSUME LEFT(C_CDMENS,1) == "M"
ASSUME LEN(TROCA(C_CDMENS,"M0123456789?",""))==0
ASSUME LEN(C_CDMENS)==6
C_SubCabec := C_SubCabec+";;("+C_CDMENS+")"
*
IF CABEC_TESTE_AUTOMATICO()
   C_Cabec_x +=" ("+C_CDMENS+")"
ENDIF
*
IF L_MUDA_COR .AND. .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_OK))
ENDIF

V_JAN := INFORMAR(C_Cabec_x,C_SubCabec,V_TECLAS,L_PARAR,N_SEGUNDOS,C_ArqImagem)

IF L_MUDA_COR .AND. .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF
*
RETURN V_JAN
*
*******************
FUNCTION MSGAGUARDE(C_CDMENS, C_Cabec_x, C_SubCabec, L_ComBotaoEsc, N_ProgressBar)
*******************
LOCAL C_COR_ANT, V_JAN
LOCAL V_TECLAS
LOCAL C_TXT1   := ""
LOCAL C_TXT2   := ""
LOCAL C_BARRA1 := ""
LOCAL C_BARRA2 := ""
LOCAL C_TEXTO  := ";"
*
DEFAULT C_Cabec_x        TO "Aguarde..."
DEFAULT C_SubCabec       TO "Aguarde... "
DEFAULT L_ComBotaoEsc    TO .T.
DEFAULT N_ProgressBar    TO 0

*
IF N_ProgressBar > 2
   ? MEMVAR->A_JANELA_NAO_TEM_MAIS_QUE_2_PROGRESSBAR
ENDIF
*
ASSUME LEFT(C_CDMENS,1) == "M"
ASSUME LEN(TROCA(C_CDMENS,"M0123456789?",""))==0
ASSUME LEN(C_CDMENS)==6
IF N_ProgressBar > 0    ///  So se for ProgressBar n„o aceita Multilinhas no subtitulo
   *
   C_SubCabec := IF( AT( ";", C_SubCabec ) > 0, LEFT(C_SubCabec, AT( ";", C_SubCabec )-1 ), C_SubCabec )
   C_SubCabec := IF( LEN(C_SubCabec) > 53, LEFT(C_SubCabec, 53), C_SubCabec )
   *
   IF .NOT. SOB_MODO_GRAFICO()
      C_BARRA1 := "[" + PADR(CHR(176), 49, CHR(176)) + "]"
      C_TXT1   := PADR( " ", 49 ) + STR(0,3)+"%"
      IF N_ProgressBar == 2
         C_BARRA2 := "[" + PADR(CHR(176), 49, CHR(176)) + "]"
         C_TXT2   := PADR( " ", 49 ) + STR(0,3)+"%"
      ENDIF
      *
      C_TEXTO := ";;"+IF(N_ProgressBar == 1, ";", "") + C_BARRA1 + ";" + C_TXT1 + ;
                     IF(N_ProgressBar == 2, ";;", ";") + C_BARRA2 + ";" + C_TXT2
   ELSE
      C_TEXTO := ";"+SPACE(50)
   ENDIF
ENDIF
*
C_SubCabec := C_SubCabec + C_TEXTO + ";("+C_CDMENS+")"
*
IF CABEC_TESTE_AUTOMATICO()
   C_Cabec_x +=" ("+C_CDMENS+")"
ENDIF
*
IF L_ComBotaoEsc
   V_TECLAS := {"Esc=sair"}
ENDIF
*
IF .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_OK))
ENDIF

V_JAN := INFORMAR(C_Cabec_x,C_SubCabec,V_TECLAS,.F.,NIL,"b99902.abm",.T.,N_ProgressBar)   //"informac.abm"
*
IF .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF
*
RETURN V_JAN
*

*********************
PROC MOSTRARMSGAGUARDE ( VX_Janela, N_DelaySeconds )
*********************
IF SOB_MODO_GRAFICO()
    NAP_WINDOW_MODAL(N_WindowNum, N_DelaySeconds)
ELSE
    INKEY(N_DelaySeconds)
ENDIF

*********************
PROC FECHARMSGAGUARDE ( VX_Janela )
*********************
IF .NOT. GetJanTipoMsgAguarde(VX_Janela)
   ? MEMVAR->ERRO_FECHAMENTO_SEM_DESTRUAJAN
ENDIF
*
SetJanTipoMsgAguarde(VX_Janela, .F.)    // tirar o atributo, para n„o gerar log na DESTRUA...
SetProgressBar(VX_Janela, NIL)          // Ao destruir uma MsgAguarde, setar NIL para o par‚metro de N_ProgressBar
*
DESTRUA VX_Janela

*****************
FUNCTION CONFIRME (C_SubCabec,N_DEFAULT)
*****************
LOCAL N_OPCAO
DEFAULT N_DEFAULT TO 1
N_OPCAO := PERGUN(C_SubCabec,{"Sim","N„o"},N_DEFAULT,,"ConfirmaÁ„o")
RETURN (N_OPCAO==1)

***********
FUNC PERGUN ( C_SubCabec, V_OPCOES, N_DEFAULT, L_PODE_ZERO, C_Cabec_x )
***********
LOCAL N_OPCAO
LOCAL C_COR_ANT
IF .NOT. SOB_MODO_GRAFICO()
   C_COR_ANT := SETCOLOR(COR(_COR_MENSAGEM_ADVERTENCIA))
ENDIF

DEFAULT N_DEFAULT   TO 1
DEFAULT L_PODE_ZERO TO .T.
DEFAULT C_Cabec_x   TO "InformaÁ„o"
*
* a confirma n„o aceita o ESC
*
N_OPCAO := PERGUNTAR(C_Cabec_x,C_SubCabec,V_OPCOES,,N_DEFAULT)
DO WHILE N_OPCAO == 0 .AND. .NOT. L_PODE_ZERO
   N_OPCAO := PERGUNTAR(C_Cabec_x,C_SubCabec,V_OPCOES,,N_DEFAULT)
ENDDO
IF .NOT. SOB_MODO_GRAFICO()
   SETCOLOR(C_COR_ANT)
ENDIF

RETURN N_OPCAO

*****************
PROC CALCULADORA    // Abra a calculadora na barra de ferramentas
*****************
RETURN

************************
FUNC TelaPrincipalHeight    // Altura APROXIMADO, em pixels, da tela principal
************************
LOCAL N_TelaPrincipalHeight := NAP_HEIGHT()
RETURN N_TelaPrincipalHeight

***********************
FUNC TelaPrincipalWidth     // Largura APROXIMADA, em pixels, da tela principal
***********************
LOCAL N_TelaPrincipalWidth := NAP_WIDTH()
RETURN N_TelaPrincipalWidth

***************************
FUNCTION GETCLIPBOARD_ASPEC
***************************
* NOTA: Os seguintes acentos em cÛdigo diferente na PC850 e na PC437: "¡¿¬√„ Õ”‘’ı⁄ß"
*
LOCAL C_RETORNO
*
IF SOB_MODO_GRAFICO()

ELSE
   * Tudo funciona normalmente (acentuaÁ„o vem correta do "clipboard")
   C_RETORNO := HB_GTINFO(HB_GTI_CLIPBOARDDATA)  // modo texto
ENDIF
*
RETURN C_RETORNO
*
***********************
PROC SETCLIPBOARD_ASPEC(C_Texto)
***********************
C_Texto := STRTRAN(C_Texto,CHR(141)," ")  // CHR(141)=="Ï"

IF SOB_MODO_GRAFICO()

ELSE
   * Tudo funciona normalmente (acentuaÁ„o vai correta para o "clipboard")
   HB_GTINFO(HB_GTI_CLIPBOARDDATA,C_Texto)  // modo texto
ENDIF

// *************** FIM DO outros.prg
