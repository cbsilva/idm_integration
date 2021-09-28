/*------------------------------------------------------------------------
    File        : deleteAccount.p
    Description : Programa para alterar o usuario para inativo 
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i essec0008e 2.00.00.001}
{METHOD/dbotterr.i}

{include/ttUpdateData.i}
{include/ttGenerics.i}
{include/ttResponse.i "'deleteAccountResponse'"}

/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE h-bofn017   AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-id        AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-grupo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mensagem  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-position  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-length    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-inativo   AS CHARACTER   NO-UNDO.

/* definicao de buffer */
DEFINE BUFFER b-usuar_mestre FOR usuar_mestre.
DEFINE BUFFER b_usuar_grp_usuar FOR usuar_grp_usuar.


/*-- dataset definitions --*/
DEFINE DATASET dsDeleteUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME "deleteAccount"
    FOR ttUserUpdate.

DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "deleteAccountResponse"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/* ***************************  Main Block  ************************** */
DEFINE INPUT  PARAMETER DATASET FOR dsDeleteUser.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.



RUN piValidUserAccount(INPUT TABLE ttUserUpdate).

LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>> VALOR DE RETURN-VALUE &1",RETURN-VALUE)) NO-ERROR.


CREATE ttRetorno.
ASSIGN ttRetorno.logUser = IF RETURN-VALUE = "OK":U THEN TRUE ELSE FALSE.



PROCEDURE piValidUserAccount:
/*---------------------------------------------------------------
 Purpose: Checks if the user sent exists in the database
---------------------------------------------------------------*/
    DEFINE INPUT  PARAM TABLE FOR  ttUserUpdate.

    LOG-MANAGER:WRITE-MESSAGE("INICIO DO PROGRAMA DE INTEGRACAO") NO-ERROR.

    FIND FIRST ttUserUpdate NO-LOCK NO-ERROR.
    IF NOT AVAIL ttUserUpdate THEN
    DO:
        RETURN "NOK":U.
    END.
    ELSE
    DO:
        LOG-MANAGER:WRITE-MESSAGE("VERIFICANDO SE N«O FOI INFORMADO UM USUARIO INCORRETO...") NO-ERROR.

        IF NOT CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUserUpdate.accountId NO-LOCK) THEN
            RETURN "NOK":U.

        updateUser:                                                                                                                                           
        DO TRANS ON ERROR UNDO, LEAVE:                                                                                                                        
            DO ON QUIT UNDO updateUser, LEAVE:                                                                                                                
                DO ON ERROR UNDO, LEAVE:                                                                                                                      
                                                                                                                                                              
                    IF NOT VALID-HANDLE(h-bofn017) THEN                                                                                                       
                        RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.                                                                                          
                                                                                                                                                              
                    EMPTY TEMP-TABLE tt-usuar_mestre.                                                                                                         
                                                                                                                                                              
                    RUN emptyRowErrors  IN h-bofn017.                                                                                                         
                    RUN openQueryStatic IN h-bofn017 (INPUT "Main":U).                                                                                        
                    RUN goToKey         IN h-bofn017 (INPUT ttUserUpdate.accountId).                                                                          
                    RUN emptyRowErrors  IN h-bofn017.                                                                                                         
                    RUN getRecord       IN h-bofn017 (OUTPUT TABLE tt-usuar_mestre).  

                    LOG-MANAGER:WRITE-MESSAGE("VALIDANDO RETORNO DA tt-usuar_mestre") NO-ERROR.
                                                                                                                                                              
                    FIND FIRST tt-usuar_mestre EXCLUSIVE-LOCK NO-ERROR.                                                                                                      
                    IF AVAIL tt-usuar_mestre THEN                                                                                                             
                    DO: 

                        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("###1 - VAMOS ALTERAR O REGISTRO &1", tt-usuar_mestre.cod_usuario)) NO-ERROR.
                        LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("###2 - VAMOS ALTERAR O REGISTRO &1", ttUserUpdate.accountId)) NO-ERROR.
                                                                                                                                                              
                        // O usuario no datasul n∆o Ç excluido, portanto antes do inicio do nome
                        // Ç acrescentado a palavra Inativo.
                        ASSIGN tt-usuar_mestre.nom_usuario       = "Inativo. " + tt-usuar_mestre.nom_usuario                                                    
                               tt-usuar_mestre.dat_fim_valid     = TODAY. 

                        VALIDATE tt-usuar_mestre.

                                                                                                                                                              
                        RUN setRecord IN h-bofn017(INPUT TABLE tt-usuar_mestre).                                                                              
                        RUN updateRecord IN h-bofn017.                                                                                                        
                        RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).                                                                                
                                                                                                                                                              
                        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN                                                                                             
                        DO:                                                                                                                                   
                            FOR EACH RowErrors NO-LOCK:                                                                                                       
                                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("ErrorCode &1 ErrorMessage &2", RowErrors.ErrorNumber, RowErrors.errorDescription)).
                                ASSIGN c-mensagem = c-mensagem + SUBSTITUTE("ErrorCode &1 ErrorMessage &2", RowErrors.ErrorNumber, RowErrors.errorDescription).                                                                         
                            END.                                                                                                                              
                            UNDO, RETURN "NOK":U.                                                                                                                      
                        END. 
                        
                        /******************************************
                            Remove todos os grupos vinculados ao 
                            usuario, com excecao do asterisco(*)
                        *******************************************/
                        //RUN pi-removeGrupos (INPUT ttUserUpdate.accountId).                                                                               
                                                                                                                                                              
                        IF VALID-HANDLE(h-bofn017) THEN DELETE PROCEDURE h-bofn017.                                                                        
                                                                                                                                                              
                    END.                                                                                                                                      
                END.                                                                                                                                          
            END.                                                                                                                                              
        END. 

        IF c-mensagem <> "" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("##### ERRO AO INATIVAR USUARIO")) NO-ERROR.
            LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("##### &1",c-mensagem )) NO-ERROR.
            RETURN "NOK":U.
                
        END.

        RETURN "OK":U.
    END.

END PROCEDURE.



PROCEDURE pi-removeGrupos:
/*----------------------------------------------------------
 Descricao: Elimina todos os grupos vinculados ao usuario
-----------------------------------------------------------*/
    DEFINE INPUT PARAM p-usuario AS CHAR NO-UNDO.

    FOR EACH b_usuar_grp_usuar EXCLUSIVE-LOCK
       WHERE b_usuar_grp_usuar.cod_usuario  = p-usuario:
        // nao eliminar o grupo * 
        IF b_usuar_grp_usuar.cod_grp_usuar = "*" THEN NEXT.
        DELETE b_usuar_grp_usuar.
    END.
    RELEASE b_usuar_grp_usuar.

    RETURN "OK":U.

END PROCEDURE.







  
