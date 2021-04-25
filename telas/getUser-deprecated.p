/*------------------------------------------------------------------------
    File        : getUser.p
    Purpose     : Programa para validar se o usua rio existe
    Description : Manuten‡Æo de usu rios (SEC000AA)
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 {include/i-prgvrs.i getUser 2.00.00.001}
  
{METHOD/dbotterr.i}

// variable definitions
DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.

{esp/essec008a.i ttUser "'userData'"}
{esp/essec008b.i ttGroups "'RoleData'"}


// temp-tables definitions 
DEFINE TEMP-TABLE ttRetorno NO-UNDO SERIALIZE-NAME ""
     FIELD codObjeto AS CHARACTER SERIALIZE-NAME "code"
     FIELD descricao AS CHARACTER SERIALIZE-NAME "detail".

//parametros de entrada
DEFINE INPUT PARAMETER  accountId AS CHARACTER FORMAT "x(12)" NO-UNDO.

//definicao dataset
DEFINE DATASET fnFault
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    SERIALIZE-NAME "Fault" FOR ttRetorno.

        
DEFINE DATASET dsUser XML-NODE-NAME 'getUserResponse' 
    SERIALIZE-HIDDEN
    FOR ttUser, ttGroups
            DATA-RELATION drUserData FOR ttUser, ttGroups 
                RELATION-FIELDS(ttUser.accountId, ttGroups.accountId).


DATASET dsUser:ADD-RELATION (
    BUFFER ttUser:HANDLE, BUFFER ttGroups:HANDLE,





                 
       /*
DEFINE DATASET dsGroupUser
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME 'Roles' FOR ttGroups.*/

//parametros de saida
DEFINE OUTPUT PARAMETER faultcode    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER faultstring  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsUser.
//DEFINE OUTPUT PARAMETER DATASET FOR dsGroupUser.

RUN piValidUserAccount (INPUT accountId, OUTPUT lResult) .
 
IF lResult = NO THEN
    ASSIGN faultcode    = "OBJECT_NOT_FOUND"                                          
           faultstring  = SUBSTITUTE("Search criteria did not match any object(s) &1 ", accountId). 
ELSE
DO:
    RUN piGetUser(INPUT accountId, 
                  OUTPUT TABLE ttUser, 
                  OUTPUT TABLE ttGroups).

END.


RETURN "OK":U.


{esp/essec008.i} /*-- procedures --*/



