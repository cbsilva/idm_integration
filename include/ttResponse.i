/**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
***********************************************************/

DEFINE TEMP-TABLE ttRetorno NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    FIELD logUser AS LOGICAL XML-NODE-NAME {1}.
