/*------------------------------------------------------------------------
    File        : checkUpdateAccount.p
    Description : Programa para checar as informaá‰es do usuario
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i getUser 2.00.00.001}
{METHOD/dbotterr.i}

{include/ttUpdateData.i}
{include/ttRoles.i "'Roles'" "'RoleData'"}
{include/ttResponse.i "'updateAccountResponse'"}


/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE c-cod-grupo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mensagem  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-position  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-length    AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-inativo   AS CHARACTER   NO-UNDO.

/* datasets definitions */
DEFINE DATASET dsUserData 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "updateAccount" 
    FOR ttUserUpdate, ttRoles, ttUserRoles
    DATA-RELATION drRoles     FOR ttUserUpdate, ttRoles  RELATION-FIELDS(accountId, accountId) NESTED
    DATA-RELATION drUserRoles FOR ttRoles, ttUserRoles   RELATION-FIELDS(accountId, accountId) NESTED.


DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "updateAccountResponse"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/* ***************************  Main Block  ************************** */
DEFINE INPUT  PARAMETER DATASET FOR dsUserData.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.


RUN piUpdateAccount(INPUT TABLE ttUserUpdate, INPUT TABLE ttUserRoles).

LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>> VALOR DE RETURN-VALUE &1",RETURN-VALUE)) NO-ERROR.


CREATE ttRetorno.
ASSIGN ttRetorno.logUser = IF RETURN-VALUE = "OK":U THEN TRUE ELSE FALSE.



PROCEDURE piUpdateAccount:
/*---------------------------------------------------------------
 Purpose: Checks if the user sent exists in the database
---------------------------------------------------------------*/
    DEFINE INPUT  PARAM TABLE FOR ttUserUpdate.
    DEFINE INPUT  PARAM TABLE FOR ttUserRoles.

    FIND FIRST ttUserUpdate NO-LOCK NO-ERROR.
    IF NOT AVAIL ttUserUpdate THEN
    DO:
        RETURN "NOK":U.
    END.
    ELSE
    DO:
        
        IF NOT CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUserUpdate.accountId NO-LOCK) THEN
            RETURN "NOK":U.


        FOR EACH ttUserRoles NO-LOCK:

            //verifica o tamanho do grupo enviado na integracao
            ASSIGN i-length   = LENGTH(ttUserRoles.groupCode)
                   i-position = i-length - 2.
    
            //pega os 3 ultimos caracteres do grupo
            ASSIGN c-cod-grupo = SUBSTRING(ttUserRoles.groupCode,i-position,i-length, "CHAR").
            
            //verifica se o grupo existe no erp
            IF NOT CAN-FIND(FIRST grp_usuar WHERE grp_usuar.cod_grp_usuar = c-cod-grupo NO-LOCK) THEN
            DO:
                LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE("O grupo &1 informando n∆o est† cadastrado",c-cod-grupo)) NO-ERROR.
                RETURN "NOK":U.
            END.
            
            //vincula usuario ao grupo
            IF NOT CAN-FIND(FIRST usuar_grp_usuar NO-LOCK                                                              
                            WHERE usuar_grp_usuar.cod_grp_usuar = c-cod-grupo                      
                              AND usuar_grp_usuar.cod_usuario   = ttUserUpdate.accountId) THEN                     
            DO:                                                                                                
                 CREATE usuar_grp_usuar.                                                                       
                 ASSIGN usuar_grp_usuar.cod_grp_usuar =  c-cod-grupo                            
                        usuar_grp_usuar.cod_usuario   =  ttUserUpdate.accountId.                               
                                                                                                               
                 RELEASE usuar_grp_usuar.                                                                      
            END.
        END.
        RETURN "OK":U.
    END.

END PROCEDURE.







  
