      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      * Son 5 archivos
      *    2 de entrada
      *        Cuentas
      *        Servicios
      *    3 de salida
      *        Rechazos
      *        Incidencias
      *        Cuentas actualizado (donde hago el apareo)
      *
      *Apareo entre el archivo CUENTAS y SERVICIOS por numero de cliente
      * Hacer un 3er archivo de salida de CUENTAS actualizado
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 ProgApareo.
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

           SELECT ENTRADA-SERVICIOS ASSIGN TO "SERVICIOS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA-SERVICIOS.

           SELECT SALIDA-RECHAZOS ASSIGN TO "RECHAZOS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA-RECHAZOS.

           SELECT SALIDA-INCIDENCIAS ASSIGN TO "INCIDENCIAS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA-INCIDENCIAS.

           SELECT SALIDA-CUEN-ACT ASSIGN TO "CUEN_ACT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA-CUEN-ACT.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *Archivo CUENTAS
      *Nro. Cliente | Nombre Cliente | Saldo Cuenta
           FD ENTRADA-CUENTAS.
               01 REGISTRO-ENTRADA-CUENTAS.
                   02 NRO-CLIE-CUEN            PIC 9(02).
                   02 NOMBRE-CLIE-CUEN         PIC X(10).
                   02 SALDO-CLIE-CUEN          PIC 999V99.
      *
      *Archivo SERVICIOS
      *Nro. Cliente | Cod Servicio | Monto
           FD ENTRADA-SERVICIOS.
               01 REGISTRO-ENTRADA-SERVICIOS.
                   02 NRO-CLIE-SERV            PIC 9(02).
                   02 COD-SERV                 PIC X(03).
                   02 MONTO                    PIC 999V99.
      *
      *Archivo RECHAZOS
      *Nro. Cliente | Nombre | Saldo actual de la cuenta | Imp. Deuda.
           FD SALIDA-RECHAZOS.
               01 REGISTRO-SALIDA-RECHAZOS.
                   02 NRO-CLIE-RECH            PIC 9(02).
                   02 NOMBRE-CLIE-RECH         PIC X(10).
                   02 SALDO-CLIE-RECH          PIC 999V99.
                   02 DEUDA-CLIE-RECH          PIC 999V99.
      *
      *Archivo INCIDENCIAS
      *Nro. Cliente | Tabla Ausencia.
           FD SALIDA-INCIDENCIAS.
               01 REGISTRO-SALIDA-INCIDENCIAS.
                   02 NRO-CLIE-INCI            PIC 9(02).
                   02 NOMBRE-TABLA-INCI        PIC X(10).
      *
      *Archivo CUENTAS actualizado (donde hago el apareo).
      *Nro. Cliente | Nombre Cliente | Saldo Cuenta Actualizado
           FD SALIDA-CUEN-ACT.
               01 REGISTRO-SALIDA-CUEN-ACT.
                   02 NRO-CLIE-CUEN-ACT        PIC 9(02).
                   02 NOMBRE-CLIE-CUEN-ACT     PIC X(10).
                   02 SALDO-CLIE-CUEN-ACT      PIC 999V99.
      *-----------------------
       WORKING-STORAGE SECTION.
      *Variables del file status
           01 FS-ENTRADA-CUENTAS               PIC X(02) VALUE ZEROES.
           01 FS-ENTRADA-SERVICIOS             PIC X(02) VALUE ZEROES.
           01 FS-SALIDA-RECHAZOS               PIC X(02) VALUE ZEROES.
           01 FS-SALIDA-INCIDENCIAS            PIC X(02) VALUE ZEROES.
           01 FS-SALIDA-CUEN-ACT               PIC X(02) VALUE ZEROES.
      *
      *Variables auxiliares.
           01 DEUDA-AC                         PIC 999V99.
           01 NEW-SALDO                        PIC 999V99.

           01 ANULADO.
               02 ANULADO-OBJETO               PIC X(15).
               02 ANULADO-CODIGO               PIC X(05).
               02 ANULADO-DESCRIPCION          PIC X(50).
      *Flags
           77 FLAG-CUENTAS                     PIC 9(01).
               88 FLAG-CUENTAS-NEW             VALUE 0.
               88 FLAG-CUENTAS-OLD             VALUE 1. 

      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      *-------------------------- Programa -----------------------------
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 00001-Inicio-programa.
           PERFORM 00050-Apareo.
           PERFORM 00100-Fin-del-programa.
      *
      *-------------------------- Parrafos -----------------------------
      *
       00001-Inicio-programa.
           DISPLAY "----- El programa inicio ----".
           DISPLAY " "
           PERFORM 00005-Abrir-archivos
           PERFORM 00006-Leer-cuentas
               IF FS-ENTRADA-CUENTAS = "10"
                   MOVE "Archivo vacio" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-CUENTAS TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF
           PERFORM 00007-Leer-servicios
               IF FS-ENTRADA-SERVICIOS = "10"
                   MOVE "Archivo vacio" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-SERVICIOS TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS" TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               END-IF.
      *************************** Apareo *******************************
       00050-Apareo.
           DISPLAY "Entre al 00050-Apareo"
           DISPLAY "El FS del ENTRADA-CUENTAS es: " FS-ENTRADA-CUENTAS
      *    Repetir 00000-A hasta EOF
      *        No entra al perform de abajo
           PERFORM 00000-A UNTIL FS-ENTRADA-CUENTAS = "10"
           DISPLAY "El FS del ENTRADA-CUENTAS es: " FS-ENTRADA-CUENTAS
           DISPLAY "Sali del 00050-Apareo".
           PERFORM 00000-Fin-de-servicios UNTIL 
                   FS-ENTRADA-SERVICIOS = "10".

       00000-A.
           DISPLAY "Entre al 00000-A"
      *    Repetir 00000-B hasta EOF o nro-cuen < nro-serv
           PERFORM 00000-B UNTIL (FS-ENTRADA-SERVICIOS = "10")
                   OR (NRO-CLIE-CUEN < NRO-CLIE-SERV).
           IF FLAG-CUENTAS-NEW
               DISPLAY "************ Cuentas que no estan en servicios"
               DISPLAY "El problematico es el cuentas"
               DISPLAY REGISTRO-ENTRADA-CUENTAS
               INITIALIZE REGISTRO-SALIDA-INCIDENCIAS
               MOVE NRO-CLIE-CUEN TO NRO-CLIE-INCI
               MOVE "CUENTAS   " TO NOMBRE-TABLA-INCI
               PERFORM 00060-Escribir-salida-inci 
           END-IF
           PERFORM 00006-Leer-cuentas.

       00000-B.
           DISPLAY "Entre al 00000-B"
           IF (NRO-CLIE-CUEN = NRO-CLIE-SERV)
               DISPLAY "Aca apareo registros"
               COMPUTE DEUDA-AC = DEUDA-AC + MONTO
               IF (DEUDA-AC <= SALDO-CLIE-CUEN)
                   COMPUTE NEW-SALDO = SALDO-CLIE-CUEN - DEUDA-AC
                   INITIALIZE REGISTRO-SALIDA-CUEN-ACT
                   MOVE NRO-CLIE-CUEN TO NRO-CLIE-CUEN-ACT
                   MOVE NOMBRE-CLIE-CUEN TO NOMBRE-CLIE-CUEN-ACT
                   MOVE NEW-SALDO TO SALDO-CLIE-CUEN-ACT
                   PERFORM 00061-Escribir-salida-cuen-act
                   MOVE 0 TO DEUDA-AC
               ELSE
                   INITIALIZE REGISTRO-SALIDA-RECHAZOS
                   MOVE NRO-CLIE-CUEN TO NRO-CLIE-RECH
                   MOVE NOMBRE-CLIE-CUEN TO NOMBRE-CLIE-RECH
                   MOVE SALDO-CLIE-CUEN TO SALDO-CLIE-RECH
                   MOVE DEUDA-AC TO DEUDA-CLIE-RECH
                   PERFORM 00062-Escribir-salida-rechazos
                   MOVE 0 TO DEUDA-AC
               END-IF
               SET FLAG-CUENTAS-OLD TO TRUE
           ELSE
      *        Aca entra cuando nro-cuen > nro-serv
               DISPLAY "************ Servicios que no esta en cuentas"
               DISPLAY "El problematico es el servicios"
               INITIALIZE REGISTRO-SALIDA-INCIDENCIAS
               MOVE NRO-CLIE-SERV TO NRO-CLIE-INCI
               MOVE "SERVICIOS " TO NOMBRE-TABLA-INCI
               PERFORM 00060-Escribir-salida-inci 
           END-IF
           PERFORM 00007-Leer-servicios.
       
       00000-Fin-de-servicios.
           DISPLAY " ************ Servicios que no esta en cuentas"
           DISPLAY "El problematico es el servicios"
           DISPLAY REGISTRO-ENTRADA-SERVICIOS
           INITIALIZE REGISTRO-SALIDA-INCIDENCIAS
           MOVE NRO-CLIE-SERV TO NRO-CLIE-INCI
           MOVE "SERVICIOS " TO NOMBRE-TABLA-INCI
           PERFORM 00060-Escribir-salida-inci 
           PERFORM 00007-Leer-servicios.
      *************************** Archivos *****************************
       00005-Abrir-archivos.
           OPEN INPUT ENTRADA-CUENTAS
               IF FS-ENTRADA-CUENTAS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-CUENTAS TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: CUENTAS"
               END-IF

           OPEN INPUT ENTRADA-SERVICIOS
               IF FS-ENTRADA-SERVICIOS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-SERVICIOS TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: SERVICIOS"
               END-IF

           OPEN OUTPUT SALIDA-RECHAZOS
               IF FS-SALIDA-RECHAZOS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-RECHAZOS TO ANULADO-CODIGO
                   MOVE "SALIDA-RECHAZOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: RECHAZOS"
               END-IF

           OPEN OUTPUT SALIDA-INCIDENCIAS
               IF FS-SALIDA-INCIDENCIAS NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-INCIDENCIAS TO ANULADO-CODIGO
                   MOVE "SALIDA-INCIDENCIAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: INCIDENCIAS"
               END-IF

           OPEN OUTPUT SALIDA-CUEN-ACT
               IF FS-SALIDA-CUEN-ACT NOT = "00"
                   MOVE "Error al abrir archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-CUEN-ACT TO ANULADO-CODIGO
                   MOVE "SALIDA-CUEN-ACT" TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: CUENTAS_ACT"
               END-IF.

       00006-Leer-cuentas.
           READ ENTRADA-CUENTAS
           IF FS-ENTRADA-CUENTAS NOT= "10"
               DISPLAY "Registro leido de CUENTAS: "
                       REGISTRO-ENTRADA-CUENTAS
               SET FLAG-CUENTAS-NEW TO TRUE
           END-IF.

       00007-Leer-servicios.
           READ ENTRADA-SERVICIOS
           IF FS-ENTRADA-SERVICIOS NOT= "10"
               DISPLAY "Registro leido de SERVICIOS: "
                       REGISTRO-ENTRADA-SERVICIOS
           END-IF.
       
       00060-Escribir-salida-inci.
           WRITE REGISTRO-SALIDA-INCIDENCIAS
           DISPLAY "Registro escrito en INCIDENCIAS: "
                   REGISTRO-SALIDA-INCIDENCIAS.
       
       00061-Escribir-salida-cuen-act.
           WRITE REGISTRO-SALIDA-CUEN-ACT
           DISPLAY "Registro escrito en CUENTAS ACT: "
                   REGISTRO-SALIDA-CUEN-ACT.

       00062-Escribir-salida-rechazos.
           WRITE REGISTRO-SALIDA-RECHAZOS
           DISPLAY "Registro escrito en RECHAZOS: "
                   REGISTRO-SALIDA-RECHAZOS.

       00099-Cerrar-archivos.
           CLOSE ENTRADA-CUENTAS
               IF FS-ENTRADA-CUENTAS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-CUENTAS TO ANULADO-CODIGO
                   MOVE "ENTRADA-CUENTAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: CUENTAS"
               END-IF

           CLOSE ENTRADA-SERVICIOS
               IF FS-ENTRADA-SERVICIOS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-ENTRADA-SERVICIOS TO ANULADO-CODIGO
                   MOVE "ENTRADA-SERVICIOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: SERVICIOS"
               END-IF

           CLOSE SALIDA-RECHAZOS
               IF FS-SALIDA-RECHAZOS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-RECHAZOS TO ANULADO-CODIGO
                   MOVE "SALIDA-RECHAZOS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: RECHAZOS"
               END-IF

           CLOSE SALIDA-INCIDENCIAS
               IF FS-SALIDA-INCIDENCIAS NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-INCIDENCIAS TO ANULADO-CODIGO
                   MOVE "SALIDA-INCIDENCIAS"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: INCIDENCIAS"
               END-IF

           CLOSE SALIDA-CUEN-ACT
               IF FS-SALIDA-CUEN-ACT NOT = "00"
                   MOVE "Error al cerrar archivo" TO ANULADO-DESCRIPCION
                   MOVE FS-SALIDA-CUEN-ACT TO ANULADO-CODIGO
                   MOVE "SALIDA-CUEN-ACT"  TO ANULADO-OBJETO
                   PERFORM 00010-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: CUEN_ACT"
               END-IF.
      *************************** Final ********************************
       00010-Mostrar-anulado.
           DISPLAY " "
           DISPLAY "----- Error en el sistema ----"
           DISPLAY "Objeto: "              ANULADO-OBJETO
           DISPLAY "Codigo del error: "    ANULADO-CODIGO
           DISPLAY "Descripcion: "         ANULADO-DESCRIPCION
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           STOP RUN.

       00100-Fin-del-programa.
           PERFORM 00099-Cerrar-archivos.
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           STOP RUN.
      *Termine
