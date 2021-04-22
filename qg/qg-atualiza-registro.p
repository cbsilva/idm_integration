{METHOD/dbotterr.i}

DEFINE VARIABLE h-bofn017 AS HANDLE      NO-UNDO.
DEFINE VARIABLE p-usuario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-return  AS CHARACTER   NO-UNDO.



DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE ttUsuario LIKE tt-usuar_mestre.


IF NOT VALID-HANDLE(h-bofn017) THEN                                       
RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.  

EMPTY TEMP-TABLE tt-usuar_mestre.

FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = "super" NO-LOCK NO-ERROR. 
IF AVAIL usuar_mestre THEN
DO:
    //MESSAGE usuar_mestre.cod_usuario
    //    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    //CREATE tt-usuar_mestre.
    //BUFFER-COPY usuar_mestre TO tt-usuar_mestre 
    //    ASSIGN tt-usuar_mestre.nom_usuario = "Adm Fluig Teste"
    //           tt-usuar_mestre.r-rowid = ROWID(usuar_mestre)
    //    
    //    NO-ERROR.

    
   // RUN findRowid IN h-bofn017 (INPUT tt-usuar_mestre.r-rowid).
    

    RUN setConstraintCodUsuario IN h-bofn017(INPUT "adm").
    RUN openQuery IN h-bofn017 (INPUT 2).

    RUN findCodigo IN h-bofn017 (INPUT "adm", OUTPUT c-return).

    RUN getRecord IN h-bofn017 (OUTPUT TABLE ttUsuario).

    

    FOR EACH ttUsuario.
        DISP ttUsuario.cod_usuario ttUsuario.nom_usuario.
    END.



/*

    RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) .
    
    RUN validateUpdate IN h-bofn017 (INPUT TABLE tt-usuar_mestre, 
                                     INPUT tt-usuar_mestre.r-rowid, 
                                     OUTPUT TABLE rowErrors).


*/
    FOR EACH RowErrors. 
        DISP rowErrors.ErrorNumber SKIP
             rowErrors.ErrorDescription FORMAT "x(80)" WITH WIDTH 333 1 COL.
    END.
    
END.



    /*

FIND FIRST tt-usuar_mestre EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL tt-usuar_mestre THEN
DO:

    /*



    ASSIGN tt-usuar_mestre.nom_usuario       = "Usu rio de Materiais Teste"       
           tt-usuar_mestre.cod_senha         = "abacate"            
           tt-usuar_mestre.cod_e_mail_local  = "meuemail@empresa.com.br"  . 

    RUN emptyRowErrors IN h-bofn017.   
    RUN validateUpdate IN h-bofn017 (INPUT TABLE tt-usuar_mestre,
                                     INPUT tt-usuar_mestre.r-rowid,
                                     OUTPUT TABLE RowErrors).
                                     
                                     */

    MESSAGE "passou aqui"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.





FOR EACH RowErrors:
DISP RowErrors WITH WIDTH 333 1 COL.
END.
*/


