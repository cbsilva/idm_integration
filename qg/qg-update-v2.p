
{METHOD/dbotterr.i}

DEFINE VARIABLE h-bofn017 AS HANDLE      NO-UNDO.
DEFINE VARIABLE p-usuario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-return  AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-rowid AS ROWID.
DEFINE TEMP-TABLE ttUsuario LIKE tt-usuar_mestre.

IF NOT VALID-HANDLE(h-bofn017) THEN                                       
RUN fnbo/bofn017.p PERSISTENT SET h-bofn017. 
            
RUN openQuery IN h-bofn017 (INPUT 1).                                    
                                                                          
//RUN findCodigo IN h-bofn017 (INPUT "adm", OUTPUT c-return).               
run goToKey in h-bofn017 ("adm").                                                                          
RUN getRecord IN h-bofn017 (OUTPUT TABLE ttUsuario).                     
                                                                        
                                                                                                                                                 
FOR EACH ttUsuario.                                                       
    DISP ttUsuario.cod_usuario ttUsuario.nom_usuario.                     
END.                                                                      
