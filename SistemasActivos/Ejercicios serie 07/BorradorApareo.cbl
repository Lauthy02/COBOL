       
       00050-Apareo.
      *    Repetir 00000-A hasta EOF
           IF (NRO-CLIE-CUEN = NRO-CLIE-SERV)
               COMPUTE DEUDA-AC = DEUDA-AC + MONTO
               COMPUTE LEIDOS-SERVICIOS = LEIDOS-SERVICIOS + 1
               PERFORM 00007-Leer-servicios
           ELSE 
               IF (NRO-CLIE-CUEN > NRO-CLIE-SERV)
                   INITIALIZE REGISTRO-SALIDA-INCIDENCIAS
                   MOVE NRO-CLIE-CUEN TO NRO-CLIE-INCI
                   MOVE "CUENTAS" TO NOMBRE-TABLA-INCI 
                   PERFORM 00060-Escribir-salida-incidencias
                   PERFORM 00007-Leer-servicios            
               ELSE
                   IF (NRO-CLIE-CUEN < NRO-CLIE-SERV)
                       IF (SALDO-CLIE-CUEN > DEUDA-AC)
                           COMPUTE NEW-SALDO = SALDO-CLIE-CUEN
                                   - DEUDA-AC
                           INITIALIZE REGISTRO-SALIDA-CUEN-ACT
                           MOVE NRO-CLIE-CUEN TO NRO-CLIE-CUEN-ACT
                           MOVE NOMBRE-CLIE-CUEN TO NOMBRE-CLIE-CUEN-ACT
                           MOVE NEW-SALDO TO SALDO-CLIE-CUEN-ACT
                           PERFORM 00061-Escribir-salida-cuen-act 
                       END-IF
                       IF (DEUDA-AC > SALDO-CLIE-CUEN)
                           INITIALIZE REGISTRO-SALIDA-RECHAZOS
                           MOVE NRO-CLIE-CUEN TO NRO-CLIE-RECH
                           MOVE NOMBRE-CLIE-CUEN TO NOMBRE-CLIE-RECH
                           MOVE SALDO-CLIE-CUEN TO SALDO-CLIE-RECH
                           MOVE DEUDA-AC TO DEUDA-CLIE-RECH
                           PERFORM 00062-Escribir-salida-rechazos
                       ELSE 
                           INITIALIZE REGISTRO-SALIDA-INCIDENCIAS
                           MOVE NRO-CLIE-CUEN TO NRO-CLIE-INCI
                           MOVE "SERVICIO" TO NOMBRE-TABLA-INCI 
                           PERFORM 00060-Escribir-salida-incidencias
                       END-IF
                   END-IF
               END-IF
           END-IF.

       00000-A.
      *    Repetir 00000-B hasta EOF o A<B
      *    Leer A

       00000-B.
           IF (A = B)
      *        Leer B
           END-IF
           IF (A > B)
      *        Leer B
           END-IF
           IF (A < B)
      *        Mover a INCIDENCIAS
           END-IF