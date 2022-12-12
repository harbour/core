/* encoding: cp850 */
********************************
*
************************************************************************************
* Redefinicao de comandos/funcoes de manipulacao de arquivos via sistema operacional
* (obrigar informar o que é arquivo temporário e o que é arquivo persistente)
*

* Obrigar uso da DELE_FILE_?()  (P,T)
#command DELETE FILE <(file)>           => comando_delete_file_proibido()
#command ERASE <(file)>                 => comando_erase_proibido_use_delete_file()
#translate FERASE(<cFileName>)          => func_ferase_proibido_use_delete_file()

* Obrigar uso da FILE_?()  (P,T)
#translate FILE( <cFilespec> ) => func_file_proibido()

* Obrigar uso da COPY_FILE_?_?()  (P_P,P_T,T_P,T_T)
#command COPY FILE <(src)> TO <(dest)>  => comando_copy_file_proibido()

* Obrigar uso da RENAME_?_?()  (P_P,P_T,T_P,T_T)
#command RENAME <(old)> TO <(new)>      => comando_rename_proibido()
#translate FRENAME(<old>,<new>)         => func_frename_proibido()

* Obrigar uso da DIRECTORY_?()  (P,T)
#translate directory( <cDirSpec> ) => func_directory_proibido()
#translate directory( <cDirSpec> , [<cAttributes>] ) => func_directory_proibido()
#translate ADir([<cFileMask>],[<aName>]) => ;
           func_adir_proibido_use_directory()
#translate ADir([<cFileMask>],[<aName>],[<aSize>]) => ;
           func_adir_proibido_use_directory()
#translate ADir([<cFileMask>],[<aName>],[<aSize>],[<aDate>]) => ;
           func_adir_proibido_use_directory()
#translate ADir([<cFileMask>],[<aName>],[<aSize>],[<aDate>],[<aTime>]) => ;
           func_adir_proibido_use_directory()
#translate ADir([<cFileMask>],[<aName>],[<aSize>],[<aDate>],[<aTime>],[<aAttr>]) => ;
           func_adir_proibido_use_directory()

* Existe para livre uso as rotinas: MOVE_?_?()           (P_P,P_T,T_P,T_T)
*                                   RENAME_E_MOVE_?_?()  (P_P,P_T,T_P,T_T)


*********************************************************************
* Redefinição de funções gerais
#xtranslate Trim ( <cString>, <lAllWhiteSpace> ) => func_Trim_segundo_parametro_proibido_use_AllTrim()

*********************************************************************
* Redefinicao de comandos/funcoes de manipulacao de arquivos de dados 
* (obrigar informar o que é arquivo temporário e o que é arquivo persistente)

* Obrigar uso da USEREDE_?()  (P,T)
#translate userede( <c_file> ) => ;
           func_userede_proibido()
#translate userede( <c_file>,[l_only]) => ;
           func_userede_proibido()
#translate userede( <c_file>,[l_only],[l_excl]) => ;
           func_userede_proibido()
#translate userede( <c_file>,[l_only],[l_excl],[c_alias]) => ;
           func_userede_proibido()
*

* Obrigar uso da DBCREATE_?()  (P,T)
#translate dbCreate( <c_file> , <a_structure> ) => func_dbcreate_proibido()
#translate dbCreate( <c_file> , <a_structure> , [<c_driver>]  ) => func_dbcreate_proibido()
#translate DbCopyExtStruct( <cDatabaseExt> ) => func_DbCopyExtStruct_proibido()
#translate DbCopyStruct( <cDatabase> ) => func_DbCopyStruct_proibido()
#translate DbCopyStruct( <cDatabase>, [<aFieldList>] ) => func_DbCopyStruct_proibido()
#command COPY STRUCTURE [EXTENDED] [TO <(f)>] => func_DbCopyExtStruct_proibido()
#command COPY STRUCTURE [TO <(f)>] [FIELDS <fields,...>] => func_DbCopyStruct_proibido()

* Obrigar uso da COPY_TO_?()  (P,T)
//#command COPY [TO <(file)>] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] ;
//      => func_copy_to_proibido()

* Obrigar uso da COPY_TO_SDF()
#command COPY [TO <(file)>] <fmtsdf:SDF> => ;
         func_copy_sdf_proibido()

* Obrigar uso da APP_FROM_?()  (P,T)
//#command APPEND [FROM <(file)>] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] ;
//       => func_append_from_proibido()

* Obrigar uso da APP_FROM_SDF()
#command APPEND [FROM <(file)>] <fmtsdf:SDF> => ;
         func_append_sdf_proibido()

* Obrigar uso da APP_FROM_DELI()
#command APPEND [FROM <(file)>] <fmtdeli:DELIMITED> => ;
         func_append_deli_proibido()

* Não fechar arquivos a não ser explicitando o alias
*     OBS: Pelo arquivo ".ch" não dá certo detectar o uso da DBCLOSEAREA() sem o Alias.
*          Implementar esta obrigatoriedade lendo diretamente o prg.
*          
#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [CONNECTION <nConn>] [INDEX <(index1)> [, <(indexN)>]] => ;
         commando_use_proibido_usar_USEREDE()

#command USE <(db)> [VIA <rdd>] [ALIAS <a>] ;
         [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
         => commando_use_sem_clausula_new()

#command USE                     => comando_use_sozinho_usar_close()

#command CLOSE                   => comando_close_sem_especificar_alias()
#command CLOSE DATABASES         => comando_close_databases_proibido()
#command CLOSE ALL               => comando_close_all_proibido()
#translate DBUSEAREA()           => func_dbusearea_sem_especificar_alias()
#translate DBCLOSEALL()          => func_dbcloseall_proibido()

#translate dbCloseArea()         => xdbCloseArea() 
#command CLOSE <a>               => <a>->( xdbCloseArea() )
* Arquivos abertos em memória podem ser "descarregados" em um arquivo temporário físico...
#command CLOSE <a> COPY TO TEMPFILE <(file)> => <a>->( xdbCloseArea(<(file)>) )
* o comando abaixo repete exatamente o existente no "std.ch", para não dar erro.
#command CLOSE INDEXES           => dbClearIndex()

#translate OrdListClear()        => xOrdListClear() 
#translate dbClearIndex()        => xdbClearIndex() 
#translate OrdListAdd(<bag>)     => xOrdListAdd(<bag>)

#translate RLock()            => xRLock() 
#translate DbRLock()          => xDbRLock() 
#translate DbRLock(<recno>)   => xDbRLock(<recno>) 
#translate FLock()            => xFLock() 
#translate DbUnLock()         => xDbUnLock() 
#translate DbRUnLock()        => xDbRUnLock() 
#translate DbRUnLock(<recno>) => xDbRUnLock(<recno>) 

* Proibir o uso da DBF()
#translate DBF()         =>  DBF_proibido_use_alias()
#xtranslate TOleAuto() => TOleAuto_proibido_use_CreateObject()


*********************************************************************
* novas rotinas para abertura de arquivos
* (otimização futura para a SQLRDD)

* Os comandos USE_P e USE_T:
*   - internamente sempre usam o parâmetro NEW
*   - Tem como default:  READONLY e SHARED
*   - No futuro terá parâmetros: VIA, CONNECTION e CODEPAGE

#command USE_P <id> TABLE <(db)> [ALIAS <a>] ;
            [<cins: CANINSERT>] [<cupd: CANUPDATE>] ;
            [<cdel: CANDELETE>] [<crec: CANRECALL>] ;
            [<pind: PERINDEX>]  [<tind: TMPINDEX>] ;
            [<ex: EXCLUSIVE>] FINALUSE => ;
         dbUseArea_P(<(id)>,<(db)>,<(a)>,;
                     <.cins.>,<.cupd.>,<.cdel.>,<.crec.>,;
                     <.pind.>,<.tind.>,<.ex.>)

* NOTA: A rotina dbUseArea_T() em 5 parametros adicionais,
*       nao expostas aqui, para uso interno da REDE.PRG
#command USE_T <(id)> TABLE <(db)> [ALIAS <a>] ;
            [<cins: CANINSERT>] [<cupd: CANUPDATE>] ;
            [<cdel: CANDELETE>] [<crec: CANRECALL>] ;
            [<tind: TMPINDEX>] ;
            [<ex: EXCLUSIVE>] FINALUSE => ;
         dbUseArea_T(<(id)>,<(db)>,<(a)>,;
                     <.cins.>,<.cupd.>,<.cdel.>,<.crec.>,;
                     .F.,<.tind.>,<.ex.>)

#command USE_T MEM <(id)> ALIAS <a> ;
            [<cins: CANINSERT>] [<cupd: CANUPDATE>] ;
            [<cdel: CANDELETE>] [<crec: CANRECALL>] ;
            STRUCTURE <a_structure> ;
            [INDEX ON <key>] FINALUSE => ;
         dbUseArea_T_MEM_from_Structure(<(id)>,<(a)>,;
                     <.cins.>,<.cupd.>,<.cdel.>,<.crec.>,;
                     <a_structure>,<"key">, <{key}>)

#command USE_T MEM <(id)> ALIAS <a> ;
            [<cins: CANINSERT>] [<cupd: CANUPDATE>] ;
            [<cdel: CANDELETE>] [<crec: CANRECALL>] ;
            FROM TEMPFILE <(db)>  ;
            [INDEX ON <key>] FINALUSE => ;
         dbUseArea_T_MEM_from_TempFile(<(id)>,<(a)>,;
                     <.cins.>,<.cupd.>,<.cdel.>,<.crec.>,;
                     <(db)>,<"key">, <{key}>)

* possível comando a usar no futuro
* #command USE_P <(id)> QUERY <(qry)> ;
*             [ALIAS <a> [FILE <(arq)>]] ;
*             [ARRAY <vector>] 
*              => ;
*          ?_dbUseArea(<(id)>,<(qry)>,<(a)>,;
*                      <(arq)>,<vector>)


******************************************************************************
* Redefinicao de comandos/funcoes de manipulacao de arquivos de índices (BAGs)
* (obrigar informar a Alias() que está sendo usado)

* Falta explicitar o Alias() nos seguintes comandos/funções:
*  - Set Index to / Close Indexes / OrdListClear()
*  - Set Index To <x>  / OrdListAdd()  

    
*****************************************************************************
* Redefinicao de comandos/funcoes de manipulacao de arquivos de orders (TAGs)
* (obrigar informar a tag que está sendo usada)
*

* Comando destinado a detectar e dar erro em "INDEX ON" sem cláusula TAG
#command INDEX ON <key> TO <(cOrderBagName)>      ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         => commando_index_on_faltando_tag()

* Obrigar o uso da TAG na função OrdCreate_P() / OrdCreate_T()
#translate OrdCreate_P(<cIndexFile>,,<cIndexExpr>,<bIndexExpr>,[<lUnique>]) ;
        => func_ordcreate_sem_tag()
#translate OrdCreate_P(<cIndexFile>,,<cIndexExpr>,<bIndexExpr>) ;
        => func_ordcreate_sem_tag()
#translate OrdCreate_P(<cIndexFile>,,<cIndexExpr>) ;
        => func_ordcreate_sem_tag()
#translate OrdCreate_T(<cIndexFile>,,<cIndexExpr>,<bIndexExpr>,[<lUnique>]) ;
        => func_ordcreate_sem_tag()
#translate OrdCreate_T(<cIndexFile>,,<cIndexExpr>,<bIndexExpr>) ;
        => func_ordcreate_sem_tag()
#translate OrdCreate_T(<cIndexFile>,,<cIndexExpr>) ;
        => func_ordcreate_sem_tag()

* Transformar todos os OrdCreate() para OrdCreate_T(), a nível de pre-processador, para
* que constem sempre como temporários (a grande maioria) e que possam ser conferidos
* com a abertura do arquivo (USE_P / USE_T ... TMPINDEX) . 
* Como a CHECA_ID.EXE proibe o uso direto do OrdCreate(), a regra abaixo atingirá 
* somente o fonte onde se programou via comando "INDEX ON".
* Para criar um índice como persistente, o programador deverá trocar o "INDEX ON" pelo 
* uso direto do "OrdCreate_P()".
*
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>,<targ>,<kkey>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>,<targ>,<kkey>)
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>,<targ>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>,<targ>)
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,<unique>,<cons>)
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,<unique>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,<unique>)
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>)
* Harbour, gera a chamada de função, com a ",)" no final !
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>,<targ>,<kkey>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>,<targ>,<kkey>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>,<targ>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>,<targ>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,<unique>,<cons>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,<unique>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,<unique>)
* Harbour, gera a chamada de função, com a ",)" no final !
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>)
*
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,<unique>,,,<kkey>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,<unique>,,,<kkey>)
#translate ordCreate(<bag>,<tag>,<keyC>,<keyB>,,,,<kkey>) => ;
         ordCreate_T(<bag>,<tag>,<keyC>,<keyB>,,,,<kkey>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,<unique>,,,<kkey>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,<unique>,,,<kkey>)
#translate ordCreate(<bag>,     ,<keyC>,<keyB>,,,,<kkey>) => ;
         ordCreate_T(<bag>,     ,<keyC>,<keyB>,,,,<kkey>)

* Obrigar trocar uso da dbseek() pela TagDbSeek() ou pela TagDbXSeek() 
#translate dbSeek( <xpr> ) => seek_sem_tag()
#translate dbSeek( <xpr> , [<lsoftseek>] ) => seek_sem_tag()
#translate dbSeek( <xpr> , [<lsoftseek>] , [<lfindlast>] ) => seek_sem_tag()

#command TAG <(c_tag)> SEEK <xpr> [<soft: SOFTSEEK>] [<lastrecno: LAST>] ;
                                  [<fora_do_escopo:FORAESCOPO>] ;
      => TagDbSeek( <(c_tag)> , <xpr> , if( <.soft.>,.T.,NIL) , if(<.lastrecno.>,.T., NIL), iif(<.fora_do_escopo.>,.T., NIL))

#command TAG <(c_tag)> XSEEK <xpr> [<soft: SOFTSEEK>] [<lastrecno: LAST>]  ;
                                   [<fora_do_escopo:FORAESCOPO>] ;
      => TagDbXSeek( <(c_tag)> , <xpr> , if( <.soft.>,.T.,NIL) , if(<.lastrecno.>,.T., NIL), iif(<.fora_do_escopo.>,.T., NIL))

#translate ORDSETFOCUS(<xOrder>)                       => OrdSetFocus_incompleto() 
#translate ORDSETFOCUS(<xOrder>,<cOrdBag>)             => OrdSetFocus_incompleto() 

#translate ORDSETFOCUS(<xOrder>,,<nOrderTmp>)          => xOrdSetFocus(<xOrder>,,<nOrderTmp>) 
#translate ORDSETFOCUS(<xOrder>,<cOrdBag>,<nOrderTmp>) => xOrdSetFocus(<xOrder>,<cOrdBag>,<nOrderTmp>) 

* Obrigar uso da clausula TAG associada ao ORDER (2 parametros)
#command SET ORDER TO <nOrder>                                           ;
         => Set_Order_To_incompleto()

#command SET ORDER TO TAG (<xOrder>)                                     ;
         [IN <(cOrdBag)>]                                                ;
         => Set_Order_To_incompleto()

#command SET ORDER TO <nOrderTmp> TAG <(xOrder)>                           ;
         [IN <(cOrdBag)>]                                                  ;
         => xOrdSetFocus( <(xOrder)>,[<(cOrdBag)>],<nOrderTmp> )

* Proibir o uso da IndexKey() 
#translate INDEXKEY()           => IndexKey_proibido_use_OrdKey() 
#translate INDEXKEY(<nOrder>)   => IndexKey_proibido_use_OrdKey() 

* Proibir o uso da IndexExt() 
#translate INDEXEXT()           => IndexExt_proibido_use_OrdBagExt() 


************************************************************************
* Redefinicao de comandos/funcoes de posicionamento do registro corrente
* (obrigar informar a TAG que está sendo usada)

* Proibir o uso direto da OrdScope() ou da SET SCOPE TO 
#translate OrdScope(<nScope>,<expr>) => use_Tag_xxx_Set_Scope() 
#translate OrdScope(<nScope>)        => use_Tag_xxx_Set_Scope() 
#translate OrdScope()                => use_Tag_xxx_Set_Scope() 

#command TAG <(c_tag)> SCOPETOP TO                       => XordScope_Formato_Bloq()
#command TAG <(c_tag)> SCOPETOP TO <x>    KEY <chave>    => XordScope_Formato_Bloq()
#command TAG <(c_tag)> SCOPEBOTTOM TO                    => XordScope_Formato_Bloq()
#command TAG <(c_tag)> SCOPEBOTTOM TO <x> KEY <chave>    => XordScope_Formato_Bloq()
#command TAG <(c_tag)> SCOPE TO                          => XordScope(nil, nil, <(c_tag)>, nil, nil)
#command TAG <(c_tag)> SCOPE TO <x>, <y>  KEY <chave>    => XordScope(<x> , <y>, <(c_tag)>, <"chave">, <{chave}>)
#command TAG <(c_tag)> SCOPE TO <x>       KEY <chave>    => XordScope(<x> , <x>, <(c_tag)>, <"chave">, <{chave}>)
#command TAG <(c_tag)> SCOPE TO , <x>     KEY <chave>    => XordScope_Formato_Bloq()

* Redefinição da dbSkipper() tem de ficar antes da dbSkip()
#translate dbSkipper(<x>) => xDbSkipper(<x>)

#translate dbSkip(<x>) => xDbSkip(<x>)
#translate dbSkip()    => xDbSkip()

#command LOCATE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         x__dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#command XLOCATE                                                        ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
      => x__dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> ) ;;
           if .not. found() ; ? MEMVAR->ERRO_LOCATE ; endif
*
#command CONTINUE                => x__dbContinue()

#translate dbGoto(<x>)    => xDbGoto(<x>)
#translate dbGoTop()      => xDbGoTop()
#translate dbGoBottom()   => xDbGoBottom()

* Usar o GOTO TOP especial abaixo se tiver de existir ao menos um registro
* no arquivo / escopo / filtro ativo.
#command GOTO TOP NOT EOF => yDbGoTop()

****************************************************************
* Redefinicao de comandos/funcoes de tratamento de locks de rede
* (obrigar informar a Alias() que está sendo usado)

* Falta explicitar o Alias() nos seguintes comandos/funções:
*  - RLOCK() / FLOCK()
*  - UNLOCK / DBUNLOCK()
*  - DBRLOCK() / DBRUNLOCK()
*  - COPY TO
*  - APPEND FROM
*  - BOF() / EOF() / FOUND()
*  - SET FILTER / DBFILTER()
*  - FIELDGET()  
*  - ORDKEY() / ORDBAGEXT() / ORDSCOPE()

******************************************
* Redefinicao de comandos/funcoes diversas
* (obrigar informar a Alias() que está sendo usado)

#command COUNT [TO <v>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [ALL] => ;
         <v> := 0 ; YDBEval( {|| <v> := <v> + 1}, ;
                            <{for}>, <{while}>, <next>, <rec>, <.rest.>,"COUNT")

#command SUM [ <x1> [, <xN>]  TO  <v1> [, <vN>] ] ;
             [FOR <for>] [WHILE <while>] [NEXT <next>] ;
             [RECORD <rec>] [<rest:REST>] [ALL] => ;
         <v1> := [ <vN> := ] 0 ;;
         YDBEval( {|| <v1> := <v1> + <x1> [, <vN> := <vN> + <xN> ]}, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,"SUM")

*********************************************************
* Redefinicao de comandos/funcoes de modificação de dados
* (obrigar informar a Alias() que está sendo usado)

* Proibir o uso da COMMIT / dbCommitAll() 
#command COMMIT                  => Commit_proibido_use_dbCommit()

* Redefinir comandos APPEND BLANK / dbappend()
#command APPEND BLANK               => xdbAppend()
#translate dbAppend()               => xdbAppend()
#translate dbAppend(<lUnlockRecords>) => xdbAppend(<lUnlockRecords>)

* Permitir explicitar Alias() com comandos APPEND BLANK / dbappend()
#command ALIAS <(area)> APPEND BLANK => xdbAppend(,<(area)>)

* Redefinir comandos REPLACE
#command REPLACE [ <f1> WITH <x1> [, <fN> WITH <xN>] ] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| _FIELD-><f1> := <x1> [, _FIELD-><fN> := <xN>]}, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,"REPLACE")
#command REPLACE <f1> WITH <v1> [, <fN> WITH <vN> ] => ;
         xDBEvalCorrente({||_FIELD-><f1> := <v1> [, _FIELD-><fN> := <vN>]},"REPLACE")

* Permitir explicitar Alias() com comandos REPLACE
#command ALIAS <(area)> REPLACE [ <f1> WITH <x1> [, <fN> WITH <xN>] ] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| _FIELD-><f1> := <x1> [, _FIELD-><fN> := <xN>]}, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.>,"REPLACE",<(area)>)
#command ALIAS <(area)> REPLACE <f1> WITH <v1> [, <fN> WITH <vN> ] => ;
         xDBEvalCorrente({||_FIELD-><f1> := <v1> [, _FIELD-><fN> := <vN>]},"REPLACE",<(area)>)

// #translate FIELDPUT(<nFieldPos>,<xValue>)             => FIELDPUT_incompleto() 

#translate FIELDPUT(<nFieldPos>,<xValue>)          => xFIELDPUT(<nFieldPos>,<xValue>) 
#translate FIELDPUT(<nFieldPos>,<xValue>,<area>)   => xFIELDPUT(<nFieldPos>,<xValue>,<area>) 

* Redefinir comandos DELETE / DBDELETE()
#command DELETE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| ydbDelete()}, <{for}>, <{while}>, <next>, <rec>, <.rest.>,"DELETE")
#command DELETE                  =>  xdbDelete()
#translate dbDelete()            =>  xdbDelete()

* Permitir explicitar Alias() com comandos DELETE
#command ALIAS <(area)> DELETE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| ydbDelete()}, <{for}>, <{while}>, <next>, <rec>, <.rest.>,"DELETE",<(area)>)
#command ALIAS <(area)> DELETE   =>  xdbDelete(<(area)>)


* Redefinir comandos RECALL / DBRECALL()
#command RECALL [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| ydbRecall()}, <{for}>, <{while}>, <next>, <rec>, <.rest.>,"RECALL")
#command RECALL                  =>  xdbRecall()
#translate dbRecall()            =>  xdbRecall()

* Permitir explicitar Alias() com comandos RECALL
#command ALIAS <(area)> RECALL [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         xDBEval( {|| ydbRecall()}, <{for}>, <{while}>, <next>, <rec>, <.rest.>,"RECALL",<(area)> )
#command ALIAS <(area)> RECALL   =>  xdbRecall(<(area)>)

* Redefinir comando PACK
#command PACK                    => x__dbPack()

* Permitir explicitar Alias() com comandos PACK
#command ALIAS <(area)> PACK     => x__dbPack(<(area)>)

* Redefinir comando ZAP
#command ZAP                     => x__dbZap()

* Permitir explicitar Alias() com comandos ZAP
#command ALIAS <(area)> ZAP      => x__dbZap(<(area)>)

* Testar uso da DBCOMMIT()
#translate dbCommit(<area>)      => dbCommit_com_parametro_use_xdbcommit()
#translate XdbCommit()           => xdbCommit_sem_parametro()

****************************************************
* Redefinicao de comandos/funcoes relacionados a RDD
* (proibir o uso de rotinas desaconselhadas)

* Proibir o uso da DbSetDriver()
#translate DBSETDRIVER()         => DbSetDriver_proibido_use_rddsetdefault() 
#translate DBSETDRIVER(<cRDD>)   => DbSetDriver_proibido_use_rddsetdefault() 

*************************************

