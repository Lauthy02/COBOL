      *    La rutina q lee la tabla - tiene q leer el archivo y meterlo 
      *        en una tabla interna
      *    El programa central le va a dar un cod a la rituina y le 
      *    devuelve la definición
      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      *    Programa generador de archivo de paises
      *            (GENERAR EL ARCHIVO CON 4 NACIONALIDADES)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PGENPAIS.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               31/10/2022.
       DATE-COMPILED.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       ENVIRONMENT DIVISION.
      *-----------------------
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAISES ASSIGN TO "ARCH-PAISES.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-PAISES.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *Archivo ARCH-PAISES
      *Cod. Pais | Nombre de la PAIS
           FD PAISES.
               01 REGISTRO-ARCH-PAIS.
                   05 REG-PAIS-NRO-PAIS      PIC 9(03).
                   05 REG-PAIS-DESCRIP       PIC X(20).

      *-----------------------
       WORKING-STORAGE SECTION.
      *Variable file status
           01 FS-ARCHIVOS.
               02 FS-PAISES                  PIC X(02) VALUE ZEROES.
      *
      *Variables de entrada.

      *
      *Variables auxuliares
           01 WSV-ANULADO.
               02 WSV-ANULADO-OBJETO           PIC X(15).
               02 WSV-ANULADO-CODIGO           PIC X(05).
               02 WSV-ANULADO-DESCRIP          PIC X(50).

           01 WSV-CONTADORES.
               02 WSV-CONTADOR                 PIC 9(03).

           01 WSV-NOM-PAIS                     PIC X(25).

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      *-------------------------- Programa -----------------------------
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-Inicio-programa
           PERFORM 5000-Proceso
           PERFORM 9000-Fin-del-programa
           .
      *
      *-------------------------- Parrafos -----------------------------
      *
       1000-Inicio-programa.
           DISPLAY "----- El programa inicio ----".
           DISPLAY " "
      *     PERFORM 1100-Inicializar
           PERFORM 7000-Abrir-archivo
           .
       1100-Inicializar.
           INITIALIZE FS-ARCHIVOS
           INITIALIZE WSV-ANULADO
           INITIALIZE WSV-CONTADORES
           INITIALIZE REGISTRO-ARCH-PAIS
           .
      *.......................... Proceso ..............................
       5000-Proceso.
           DISPLAY "Tiene para ingresar 4 PAISES"
           MOVE 53 TO WSV-CONTADOR
           PERFORM 5100-Pedir-datos UNTIL WSV-CONTADOR = "057"
           .
       5100-Pedir-datos.
           ADD 1 TO WSV-CONTADOR
           DISPLAY "Pais numero [" WSV-CONTADOR "]: "
           DISPLAY "Ingrese el nombre: "
           ACCEPT WSV-NOM-PAIS
           PERFORM 5150-Verificar-datos
           .
       5150-Verificar-datos.
           IF(WSV-NOM-PAIS = " ")
             DISPLAY "**** No ingrese valores en blanco"
             DISPLAY " "
             SUBTRACT 1 FROM WSV-CONTADOR
           ELSE
             MOVE WSV-CONTADOR TO REG-PAIS-NRO-PAIS
             MOVE WSV-NOM-PAIS TO REG-PAIS-DESCRIP
             PERFORM 7000-Escribir-archivo
           END-IF
           .
      *************************** Archivo ******************************
       7000-Abrir-archivo.
           OPEN OUTPUT PAISES
               IF FS-PAISES NOT = "00"
                   MOVE "Error al abrir archivo" TO WSV-ANULADO-DESCRIP
                   MOVE FS-PAISES TO WSV-ANULADO-CODIGO
                   MOVE "ARCH-PAISES"  TO WSV-ANULADO-OBJETO
                   PERFORM 8900-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: ARCH-PAISES"
               END-IF
           .
       7000-Escribir-archivo.
           WRITE REGISTRO-ARCH-PAIS
           DISPLAY "Registro escrito en ARCH-PAISES: "
                   REGISTRO-ARCH-PAIS
           .
       7000-Cerrar-archivo.
           CLOSE PAISES
               IF FS-PAISES NOT = "00"
                   MOVE "Error al cerrar archivo" TO WSV-ANULADO-DESCRIP
                   MOVE FS-PAISES TO WSV-ANULADO-CODIGO
                   MOVE "ARCH-PAISES"  TO WSV-ANULADO-OBJETO
                   PERFORM 8900-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: PAISES"
               END-IF
           .
      ******************************************************************
       8900-Mostrar-anulado.
           DISPLAY " "
           DISPLAY "----- Error en el sistema ----"
           DISPLAY "Objeto: "              WSV-ANULADO-OBJETO
           DISPLAY "Codigo del error: "    WSV-ANULADO-CODIGO
           DISPLAY "Descripcion: "         WSV-ANULADO-DESCRIP
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           PERFORM 9999-Stop-Run
           .
       9000-Fin-del-programa.
           PERFORM 7000-Cerrar-archivo.
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           PERFORM 9999-Stop-Run
           .
       9999-Stop-Run.
           STOP RUN.
