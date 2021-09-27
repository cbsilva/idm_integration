/*------------------------------------------------------------------------
    File        : ESSEC008.p
    Purpose     : Programa para manutená∆o de usu†rios
    Description : Manutená∆o de usu†rios (SEC000AA)
    Author(s)   : Cleberson Silva - 4Make
    Created     : 13/09/2020
    Notes       :
 ------------------------------------------------------------------------*/
 {include/i-prgvrs.i ESSEC008 2.00.00.001}
  
{METHOD/dbotterr.i}

/* definicao de temp-tables */
DEFINE TEMP-TABLE ttUsuarios NO-UNDO SERIALIZE-NAME "Users"
   FIELD cod-usuario          AS CHAR FORMAT "x(12)"      SERIALIZE-NAME "User"
   FIELD nome-usuario         AS CHAR FORMAT "x(50)"      SERIALIZE-NAME "Name"
   FIELD email                AS CHAR FORMAT "x(150)"     SERIALIZE-NAME "Email"
   FIELD inativo              AS LOGICAL  INITIAL NO      SERIALIZE-NAME "InactiveUser"
   FIELD num-dias-valid-senha AS INTEGER                  SERIALIZE-NAME "NumberDaysOFValidity".


DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE ttGruposUsuarios NO-UNDO SERIALIZE-NAME "UserGroups"
    FIELD cod-usuario    AS CHAR SERIALIZE-NAME "User"
    FIELD cod-grupo      AS CHAR SERIALIZE-NAME "GroupCode"
    FIELD tipo-grupo     AS CHAR SERIALIZE-NAME "ActionInput". /*create, delete*/


DEFINE TEMP-TABLE tt-erros NO-UNDO
    FIELD cod-erro  AS INTE
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

/* definicao de variaveis*/
DEFINE VARIABLE h-bofn017   AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-id        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-grupo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mensagem  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-position  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-length    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-inativo   AS CHARACTER   NO-UNDO.


DEFINE DATASET fndUsuarios FOR ttUsuarios.
DEFINE DATASET fndGrupos   FOR ttGruposUsuarios.

/* Recebimento de parametros */
DEFINE INPUT PARAMETER DATASET FOR fndUsuarios.
DEFINE INPUT PARAMETER DATASET FOR fndGrupos.

DEFINE OUTPUT PARAMETER retorno AS CHARACTER NO-UNDO.


/* definicao de buffer */
DEFINE BUFFER b-usuar_mestre FOR usuar_mestre.
DEFINE BUFFER b_usuar_grp_usuar FOR usuar_grp_usuar.



IF NOT CAN-FIND(FIRST ttUsuarios NO-LOCK) THEN
DO:
    ASSIGN retorno ="17006 - Erro ao criar usuario, parametros inv†lidos".
    LOG-MANAGER:WRITE-MESSAGE("usuario nao sera criado a tabela esta vazia") NO-ERROR.
END.


FOR FIRST ttUsuarios NO-LOCK:
    
    LOG-MANAGER:WRITE-MESSAGE("INICIO DO PROGRAMA DE INTEGRACAO") NO-ERROR.
    
    // verifica tela de parametros integracao - essec008b.w
    FIND FIRST es-param-integ NO-LOCK NO-ERROR.
    IF NOT AVAIL es-param-integ THEN
    DO:
        ASSIGN retorno = "Usu†rio de integraá∆o n∆o parametrizado no programa ESSEC008B".
        RETURN.
    END.

    LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("Instancia Datasul &1",es-param-integ.idi-dtsul-inst)) NO-ERROR.

    // valida o usuario de integraao
    RUN btb/btapi910ze.p   (INPUT es-param-integ.cod-usuario,        /*USUARIO*/
                            INPUT "",                                /*SENHA*/  
                            INPUT es-param-integ.idi-dtsul-inst,     /*DATASUL INSTANCIA*/  
                            OUTPUT TABLE tt-erros).             

    IF CAN-FIND(FIRST tt-erros NO-LOCK) THEN
    DO:
        FOR EACH tt-erros NO-LOCK:
            IF retorno = "" THEN
                ASSIGN retorno = STRING(tt-erros.cod-erro) + " - " + tt-erros.desc-erro.
            ELSE 
                ASSIGN retorno = retorno + CHR(13) + STRING(tt-erros.cod-erro) + " - " + tt-erros.desc-erro.
        END.
        RETURN.        
    END.


    //limpa temp-table
    EMPTY TEMP-TABLE tt-usuar_mestre.
    
    /*recupera o id do ultimo usuario criado*/
    FIND LAST b-usuar_mestre NO-LOCK NO-ERROR.
    IF AVAIL b-usuar_mestre THEN ASSIGN i-id = b-usuar_mestre.idi_dtsul + 1.

    IF NOT CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUsuarios.cod-usuario NO-LOCK) THEN
    DO:


        createUser: 
        DO TRANS ON ERROR UNDO, LEAVE:
            DO ON QUIT UNDO createUser, LEAVE:
                DO ON ERROR UNDO, LEAVE:
                
                    CREATE tt-usuar_mestre.
                    ASSIGN tt-usuar_mestre.cod_usuario          = ttUsuarios.cod-usuario
                           tt-usuar_mestre.nom_usuario          = ttUsuarios.nome-usuario
                           tt-usuar_mestre.cod_senha            = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUsuarios.cod-usuario))))
                           tt-usuar_mestre.cod_senha_framework  = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUsuarios.cod-usuario))))
                           tt-usuar_mestre.ind_tip_usuar        = "comum" 
                           tt-usuar_mestre.ind_tip_aces_usuar   = "interno"
                           tt-usuar_mestre.dat_inic_valid       = TODAY
                           tt-usuar_mestre.dat_fim_valid        = TODAY + 30
                           tt-usuar_mestre.dat_valid_senha      = TODAY - 1
                           tt-usuar_mestre.num_dias_valid_senha = ttUsuarios.num-dias-valid-senha
                           tt-usuar_mestre.idi_dtsul            = i-id
                           tt-usuar_mestre.nom_dir_spool        = es-param-integ.nom-dir-spool
                           tt-usuar_mestre.nom_subdir_spool     = ttUsuarios.cod-usuario
                           tt-usuar_mestre.cod_servid_exec      = es-param-integ.cod-servid-exec
                           tt-usuar_mestre.nom_subdir_spool_rpw = ttUsuarios.cod-usuario
                           tt-usuar_mestre.cod_e_mail_local     = ttUsuarios.email
                           tt-usuar_mestre.cod_dialet           = es-param-integ.cod-dialet.
                    
                    
                    IF NOT VALID-HANDLE(h-bofn017) THEN
                        RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

                    RUN emptyRowErrors  IN h-bofn017.
                    RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.
                    RUN setRecord       IN h-bofn017(INPUT TABLE tt-usuar_mestre).
                    RUN createRecord    IN h-bofn017.
                    RUN getRowErrors    IN h-bofn017(OUTPUT TABLE RowErrors).

                    IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
                    DO:
                        FOR EACH RowErrors NO-LOCK:
                            ASSIGN retorno = RowErrors.errorDescription.
                        END.
                        UNDO, LEAVE.
                    END.

                    /*-- vincula usuario a empresa*/
                    IF NOT CAN-FIND(FIRST segur_empres_usuar WHERE segur_empres_usuar.cod_usuario = ttUsuarios.cod-usuario)
                    THEN DO:
                        CREATE segur_empres_usuar.
                        ASSIGN segur_empres_usuar.cod_usuario = ttUsuarios.cod-usuario
                               segur_empres_usuar.cod_empresa = es-param-integ.ep-codigo.

                        RELEASE segur_empres_usuar.
                    END.

                    /*-- vincula os grupos dos usuarios --*/
                    RUN pi-vincula-grupos (INPUT ttUsuarios.cod-usuario).
                    IF RETURN-VALUE = "NOK":U THEN 
                        UNDO, LEAVE.

                    IF VALID-HANDLE(h-bofn017) THEN DELETE PROCEDURE h-bofn017.

                    ASSIGN retorno = SUBSTITUTE("Usuario &1 registrado com sucesso.", ttUsuarios.cod-usuario).

                END.
            END.
        END.
    END.
    ELSE
    DO:

        updateUser: 
        DO TRANS ON ERROR UNDO, LEAVE:
            DO ON QUIT UNDO updateUser, LEAVE:
                DO ON ERROR UNDO, LEAVE:
                    
                    IF NOT VALID-HANDLE(h-bofn017) THEN                                       
                        RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.    
                    
                    EMPTY TEMP-TABLE tt-usuar_mestre.
                                                                                              
                    RUN emptyRowErrors  IN h-bofn017.                                          
                    RUN openQueryStatic IN h-bofn017 (INPUT "Main":U).          
                    RUN goToKey         IN h-bofn017 (INPUT ttUsuarios.cod-usuario).
                    RUN emptyRowErrors  IN h-bofn017.    
                    RUN getRecord       IN h-bofn017 (OUTPUT TABLE tt-usuar_mestre).
                    
                    FIND FIRST tt-usuar_mestre NO-ERROR.
                    IF AVAIL tt-usuar_mestre THEN
                    DO:
                        // quando modificar um usuario inativo, ajustar validade da senha
                        // dias de validade e resetar a senha, para dados do login
                        IF SUBSTRING(ttUsuarios.nome-usuario,1,7,"CHAR") = "Inativo"  THEN
                            ASSIGN tt-usuar_mestre.cod_senha            = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUsuarios.cod-usuario)))) 
                                   tt-usuar_mestre.cod_senha_framework  = BASE64-ENCODE(SHA1-DIGEST(LC(TRIM(ttUsuarios.cod-usuario)))) 
                                   tt-usuar_mestre.dat_valid_senha      = TODAY - 1   
                                   tt-usuar_mestre.num_dias_valid_senha = ttUsuarios.num-dias-valid-senha.       

                        
                        // variaveis Ç preenchida somente, quando usuario for inativo no IDM
                        ASSIGN c-inativo = IF ttUsuarios.inativo = YES THEN "Inativo. " ELSE ""
                               tt-usuar_mestre.nom_usuario       = c-inativo + ttUsuarios.nome-usuario
                               tt-usuar_mestre.cod_e_mail_local  = ttUsuarios.email.
                    
                        RUN setRecord IN h-bofn017(INPUT TABLE tt-usuar_mestre).                  
                        RUN updateRecord IN h-bofn017.                                            
                        RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).                    
                                                                                              
                        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN                                 
                        DO:                                                                       
                            FOR EACH RowErrors NO-LOCK:                                           
                                ASSIGN retorno = RowErrors.errorDescription.                      
                            END.                                                                  
                            UNDO, LEAVE.                                                               
                        END. 

                        // quando usuario for inativado remove os grupos
                        IF ttUsuarios.inativo = YES THEN
                            RUN pi-removeGrupos (INPUT ttUsuarios.cod-usuario).
                        ELSE
                        DO:
                            /*-- vincula os grupos dos usuarios --*/                                 
                            RUN pi-vincula-grupos (INPUT ttUsuarios.cod-usuario).                    
                            IF RETURN-VALUE = "NOK" THEN                                             
                                UNDO, LEAVE.                                                         
                        END.
                    
                        IF VALID-HANDLE(h-bofn017) THEN DELETE PROCEDURE h-bofn017.
                                                                                                                                                            
                        ASSIGN retorno = IF c-mensagem <> "" THEN c-mensagem ELSE SUBSTITUTE("Usuario &1 alterado com sucesso.", ttUsuarios.cod-usuario).   
                    
                    END.
                END.
            END.
        END.
    END.
END.



PROCEDURE pi-vincula-grupos:
    DEFINE INPUT PARAMETER p-usuario AS CHAR NO-UNDO.


    /*-- vincula os grupos dos usuarios --*/                                                               
    FOR EACH ttGruposUsuarios NO-LOCK WHERE ttGruposUsuarios.cod-usuario = p-usuario: 
        
        //verifica o tamanho do grupo enviado na integracao
        ASSIGN i-length   = LENGTH(ttGruposUsuarios.cod-grupo)
               i-position = i-length - 2.

        //pega os 3 ultimos caracteres do grupo
        ASSIGN c-cod-grupo = SUBSTRING(ttGruposUsuarios.cod-grupo,i-position,i-length, "CHAR").
        
        //verifica se o grupo existe no erp
        IF NOT CAN-FIND(FIRST grp_usuar WHERE grp_usuar.cod_grp_usuar = c-cod-grupo NO-LOCK) THEN
        DO:
            ASSIGN c-mensagem = SUBSTITUTE("O grupo &1 informando n∆o est† cadastrado",c-cod-grupo).
            RETURN "NOK":U.
        END.
                                                                                                           
        IF NOT CAN-FIND(FIRST usuar_grp_usuar NO-LOCK                                                              
                        WHERE usuar_grp_usuar.cod_grp_usuar = c-cod-grupo                      
                          AND usuar_grp_usuar.cod_usuario   = ttGruposUsuarios.cod-usuario) THEN                     
        DO:                                                                                                
             CREATE usuar_grp_usuar.                                                                       
             ASSIGN usuar_grp_usuar.cod_grp_usuar =  c-cod-grupo                            
                    usuar_grp_usuar.cod_usuario   =  ttGruposUsuarios.cod-usuario.                               
                                                                                                           
             RELEASE usuar_grp_usuar.                                                                      
        END. 
        ELSE
        DO:
            IF ttGruposUsuarios.tipo-grupo = "delete" THEN
            DO:
                FIND FIRST b_usuar_grp_usuar EXCLUSIVE-LOCK                                          
                     WHERE b_usuar_grp_usuar.cod_grp_usuar = c-cod-grupo   
                       AND b_usuar_grp_usuar.cod_usuario   = ttGruposUsuarios.cod-usuario NO-ERROR.
                IF AVAIL b_usuar_grp_usuar THEN
                    DELETE b_usuar_grp_usuar.

                RELEASE b_usuar_grp_usuar.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-removeGrupos:
/*----------------------------------------------------------
 Descricao: Elimina todos os grupos vinculados ao usuario
-----------------------------------------------------------*/
    DEFINE INPUT PARAM p-usuario AS CHAR NO-UNDO.

    FOR EACH b_usuar_grp_usuar EXCLUSIVE-LOCK
       WHERE b_usuar_grp_usuar.cod_usuario  = p-usuario:
        DELETE b_usuar_grp_usuar.
    END.
    RELEASE b_usuar_grp_usuar.

    RETURN "OK":U.

END PROCEDURE.
                                                                                              
