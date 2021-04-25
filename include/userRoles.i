 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUserRoles NO-UNDO XML-NODE-NAME "Role"
    FIELD accountId AS CHAR XML-NODE-TYPE 'hidden'
    FIELD groupCode AS CHAR XML-NODE-NAME 'name'
    INDEX idxGroupCode IS PRIMARY accountId groupCode.
