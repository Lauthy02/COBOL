      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      * Son 4 archivos
      *    2 de entrada
      *        Cuentas
      *        Servicios
      *    2 de salida
      *        Rechazos
      *        Incidencias
      *
      *Apareo entre el archivo CUENTAS y SERVICIOS por numero de cliente
      * Hacer un 3er archivo de salida de CUENTAS actualizado
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 .
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               04/10/2022.
       DATE-COMPILED.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       ENVIRONMENT DIVISION.
      *-----------------------
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA-CUENTAS ASSIGN TO "CUENTAS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL 
           FILE STATUS FS-ENTRADA-CUENTAS.
      *
           SELECT ENTRADA-SERVICIOS ASSIGN TO "SERVICIOS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA-SERVICIOS.
      *
           SELECT SALIDA-RECHAZOS ASSIGN TO "RECHAZOS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA-RECHAZOS.
      *
           SELECT SALIDA-INCIDENCIAS ASSIGN TO "INCIDENCIAS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA-INCIDENCIAS.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *Archivo CUENTAS
      *Nro. Cliente | Tabla Ausencia. 
           FD ENTRADA1-CUENTAS.
               01 REGISTRO-ENTRADA-CUENTAS.
                   02 NRO-CLIE-CUEN            PIC 9(02).
                   02 NOMBRE-CLIE-CUEN         PIC X(10).
                   02 SALDO-CLIE-CUEN          PIC 9999V99.
      *
      *Archivo SERVICIOS
      *Nro. Cliente | Tabla Ausencia.                                   
           FD ENTRADA-SERVICIOS.
               01 REGISTRO-ENTRADA-SERVICIOS.
                   02 NRO-CLIE-SERV            PIC 9(02).
                   02 COD-SERV                 PIC X(03).
                   02 MONTO                    PIC 9999V99.
      * 
      *Archivo RECHAZOS
      *Nro. Cliente | Nombre| Saldo  actual de la cuenta | Imp. Deuda.                                        
           FD SALIDA-RECHAZOS.                   
               01 REGISTRO-SALIDA-RECHAZOS.              
                   02 NRO-CLIE-RECH            PIC 9(02).
                   02 NOMBRE-CLIE-RECH         PIC X(10).
                   02 SALDO-CLIE-RECH          PIC 9999V99.
                   02 DEUDA-CLIE-RECH          PIC 9999V99.
      * 
      *Archivo INCIDENCIAS
      *Nro. Cliente | Tabla Ausencia. 
           FD SALIDA-INCIDENCIAS.                 
               01 REGISTRO-SALIDA-INCIDENCIAS. 
                   02 NRO-CLIE-INCI            PIC 9(02).
                   02 NOMBRE-TABLA-INCI        PIC X(10).
      *-----------------------
       WORKING-STORAGE SECTION.
      *Variables del file status                                             
           01 FS-ENTRADA-CUENTAS               PIC X(02) VALUE ZEROES.
           01 FS-ENTRADA-SERVICIOS             PIC X(02) VALUE ZEROES.
           01 FS-SALIDA-RECHAZOS               PIC X(02) VALUE ZEROES.
           01 FS-SALIDA-INCIDENCIAS            PIC X(02) VALUE ZEROES.
      *     
      *Variables bool para saber si el archivo terminó 
           01 FIN-CUEN                         PIC X(01) VALUE "N".
               88 FIN-CUEN-SI                  VALUE "S".
               88 FIN-CUEN-NO                  VALUE "N".

           01 FIN-SERV                         PIC X(01) VALUE "N".
               88 FIN-SERV-SI                  VALUE "S".
               88 FIN-SERV-NO                  VALUE "N".
      *
      *Variables para usar de acumuladores.                                 
           01 LEIDOS-CUENTAS                   PIC 9(09) VALUE ZEROES.              
           01 LEIDOS-SERVICIOS                 PIC 9(09) VALUE ZEROES.               
           01 ESCRITOS-RECHAZOS                PIC 9(09) VALUE ZEROES.  
           01 ESCRITOS-INCIDENCIAS             PIC 9(09) VALUE ZEROES.
       
           01 ANULADO.
               02 ANULADO-OBJETO               PIC X(15).
               02 ANULADO-CODIGO               PIC X(05). 
               02 ANULADO-DESCRIPCION          PIC X(50).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 00001-Inicio-programa.
           PERFORM 00050-Apareo UNTIL FIN-CUEN-SI.
           PERFORM 00100-Fin-del-programa.
      *
      *-------------------------- Párrafos -----------------------------
      *
       00001-Inicio-programa.
           DISPLAY "----- El programa inició ----".
           DISPLAY " "
           PERFORM 00005-Abrir-archivos
           PERFORM 00006-LEER-CUENTAS
               IF FS-ENTRADA1 = "10"
                   MOVE "Archivo vacío" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA1 TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF
           PERFORM 00007-LEER-SERVICIOS
               IF FS-ENTRADA2 = "10"
                   MOVE "Archivo vacío" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA2 TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS" TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF.
      
       00005-Abrir-archivos.
           OPEN INPUT ENTRADA-CUENTAS
               IF FS-ENTRADA-CUENTAS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-CUENTAS TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF

           OPEN INPUT ENTRADA-SERVICIOS
               IF FS-ENTRADA2 NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-SERVICIOS TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF 

           OPEN OUTPUT SALIDA-RECHAZOS
               IF FS-SALIDA-RECHAZOS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-RECHAZOS TO ANULADO-CODIGO
                   MOVE "SALIDA-RECHAZOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF.

           OPEN OUTPUT SALIDA-INCIDENCIAS
               IF FS-SALIDA-INCIDENCIAS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-INCIDENCIAS TO ANULADO-CODIGO
                   MOVE "SALIDA-INCIDENCIAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF.

       00006-LEER-CUENTAS.
      *Leer y verificar q no sea fin de archivo
           READ ENTRADA-CUENTAS
               AT END
                   SET FIN-CUEN-SI  TO TRUE
                   MOVE HIGH-VALUES TO 
               NOT AT END
                   ADD 1 TO LEIDOS-CUENTAS. 

       00007-LEER-SERVICIOS.
           READ ENTRADA-SERVICIOS
               AT 
                   SET FIN-SERV-SI  TO TRUE
                   MOVE HIGH-VALUES TO CLAVE1
               NOT AT END
                   ADD 1 TO LEIDOS-SERVICIOS. 

       00010-Mostrar-anulado.
           DISPLAY " "
           DISPLAY "----- Error en el sistema ----"           
           DISPLAY "Objeto: "              ANULADO-OBJETO  
           DISPLAY "Código del error: "    ANULADO-CODIGO  
           DISPLAY "Descripción: "         ANULADO-DESCRIPCION
           DISPLAY " "
           STOP RUN.

       00050-Apareo.
           IF (NRO-CLIE-CUEN > NRO-CLIE-SERV)
                PERFORM 00007-LEER-SERVICIOS
           ELSE 
               IF (NRO-CLIE-CUEN < NRO-CLIE-SERV)
                   PERFORM 00006-LEER-CUENTAS
               ELSE
                   INITIALIZE REGISTRO-SALIDA
                   MOVE NOMBRE TO SAL-NOMBRE
                   MOVE APELLIDO TO SAL-APELLIDO 
                   MOVE EDAD   TO SAL-EDAD
      
                   MOVE CLAVE1 TO SAL-CLAVE
                   PERFORM 201-ESCRIBIR-SALIDA
      
                   PERFORM 00006-LEER-CUENTAS
               END-IF
           END-IF.           

       00099-Cerrar-archivos.
           CLOSE ENTRADA-CUENTAS
               IF FS-ENTRADA-CUENTAS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-CUENTAS TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF 

           CLOSE ENTRADA-SERVICIOS
               IF FS-ENTRADA-SERVICIOS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-SERVICIOS TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF 

           CLOSE SALIDA-RECHAZOS
               IF FS-SALIDA-RECHAZOS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-RECHAZOS TO ANULADO-CODIGO
                   MOVE "SALIDA-RECHAZOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF 

           CLOSE SALIDA-INCIDENCIAS
               IF FS-SALIDA-INCIDENCIAS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-INCIDENCIAS TO ANULADO-CODIGO
                   MOVE "SALIDA-INCIDENCIAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF.

       00100-Fin-del-programa.
           PERFORM 00099-Cerrar-archivos.
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           STOP RUN.