/*------------------------------------------------------------------------
    File        : checkCreateAccount.p
    Purpose     : Programa verifica se a conta do usuario pode ser 
                  criada
    Description : Manuten‡Æo de usu rios (SEC000AA)
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
{METHOD/dbotterr.i}
{include/i-prgvrs.i checkCreateAccount 2.00.00.001}

/*-- temp-tables definitions --*/
{esp/essec008a.i ttUser "'userData'"}
{esp/essec008b.i ttGroups "'Roles'"}

DEFINE TEMP-TABLE ttRetorno NO-UNDO SERIALIZE-NAME "checkCreateAccount"
    FIELD logUser AS LOGICAL XML-NODE-TYPE "ELEMENT".



/*-- dataset definitions --*/
DEFINE DATASET dsUser 
    XML-NODE-NAME "checkCreateAccount"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN FOR ttUser.

DEFINE DATASET dsRetorno
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN FOR ttRetorno.                                


/*-- variable definitions --*/
DEFINE VARIABLE lRetorno AS LOGICAL INITIAL FALSE NO-UNDO.


/*
DEFINE TEMP-TABLE ttSenhaUsuario NO-UNDO SERIALIZE-NAME "item"
    FIELD tipo  AS CHARACTER SERIALIZE-NAME "key"
    FIELD valor AS CHARACTER SERIALIZE-NAME "value".


DEFINE DATASET userData XML-NODE-TYPE 'hidden'
     FOR ttSenhaUsuario.

 //definicao de parametros de entrada
 comentado para teste
DEFINE INPUT  PARAMETER accountId  AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER firstname  AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER lastname   AS CHARACTER NO-UNDO.        
DEFINE INPUT  PARAMETER email      AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER department AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER DATASET FOR userData.

*/




/* DEFINE DATASET dsRetorno XML-NODE-NAME 'checkCreateAccountResponse'  */
/*     SERIALIZE-HIDDEN FOR ttRetorno.                                  */


DEFINE INPUT  PARAMETER DATASET FOR dsUser.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.
//DEFINE OUTPUT PARAMETER pResponse AS LOGICAL 
//    COLUMN-LABEL "checkCreateAccountResponse"
//    NO-UNDO.

FIND FIRST ttUser NO-LOCK NO-ERROR.
IF NOT AVAIL ttUSer THEN
DO:
    CREATE ttRetorno.
    ASSIGN ttRetorno.logUser = FALSE.
    //ASSIGN pResponse = FALSE.
    RETURN "NOK":U.
END.
ELSE
DO:
    RUN piValidUserAccount(INPUT ttUser.accountId, OUTPUT lRetorno).
    CREATE ttRetorno.
    ASSIGN ttRetorno.logUser = lRetorno.
END.

{esp/essec008.i} /*-- procedures --*/  







  
