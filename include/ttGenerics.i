/*
    definicao de temp-tables genericas
*/


DEFINE TEMP-TABLE ttErrorMessage NO-UNDO
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    SERIALIZE-NAME 'faultcode'
    FIELD idmFault          AS CHARACTER XML-NODE-NAME "idmFault"
    FIELD errorCode         AS CHARACTER XML-NODE-NAME "code"
    FIELD ErrorDescription  AS CHARACTER XML-NODE-NAME "detail".


DEFINE TEMP-TABLE tt-erros NO-UNDO
    FIELD cod-erro  AS INTE
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-rowid AS ROWID.


