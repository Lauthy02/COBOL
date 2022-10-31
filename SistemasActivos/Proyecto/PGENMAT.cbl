      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      *    Programa generador de archivo de materias
      *                 (GENERAR EL ARCHIVO CON 8 MATERIAS)
      *                     NRO-MATERIA DE 1  A 8
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PGENMAT.
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
           SELECT MATERIAS ASSIGN TO "ARCH-MATERIAS.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-MATERIAS.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *Archivo ARCH-MATERIAS
      *Cod. Materia | Nombre de la materia
           FD MATERIAS.
               01 REGISTRO-ARCH-MATERIA.
                   05 REG-MAT-NRO-MATERIA      PIC 9(02).
                   05 REG-MAT-DESCRIPCION      PIC X(25).

      *-----------------------
       WORKING-STORAGE SECTION.
      *Variable file status
           01 FS-ARCHIVOS.
               02 FS-MATERIAS                  PIC X(02) VALUE ZEROES.
      *
      *Variables de entrada.

      *
      *Variables auxuliares
           01 WSV-ANULADO.
               02 WSV-ANULADO-OBJETO           PIC X(15).
               02 WSV-ANULADO-CODIGO           PIC X(05).
               02 WSV-ANULADO-DESCRIP          PIC X(50).

           01 WSV-CONTADORES.
               02 WSV-CONTADOR                 PIC 9(01).

           01 WSV-NOM-MAT                      PIC X(25).

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
           PERFORM 1100-Inicializar
           PERFORM 7000-Abrir-archivo
           .
       1100-Inicializar.
           INITIALIZE FS-ARCHIVOS
           INITIALIZE WSV-ANULADO
           INITIALIZE WSV-CONTADORES
           INITIALIZE REGISTRO-ARCH-MATERIA
           .
      *.......................... Proceso ..............................
       5000-Proceso.
           DISPLAY "Tiene para ingresar 8 materias"
           PERFORM 5100-Pedir-datos UNTIL WSV-CONTADOR = "8"
           .

       5100-Pedir-datos.
           ADD 1 TO WSV-CONTADOR
           DISPLAY "Materia numero [" WSV-CONTADOR "]: "
           DISPLAY "Ingrese el nombre: "
           ACCEPT WSV-NOM-MAT
           PERFORM 5150-Verificar-datos
           .
       5150-Verificar-datos.
           IF(WSV-NOM-MAT = " ")
             DISPLAY "**** No ingrese valores en blanco"
             DISPLAY " "
             SUBTRACT 1 FROM WSV-CONTADOR
           ELSE
              MOVE WSV-CONTADOR TO REG-MAT-NRO-MATERIA
              MOVE WSV-NOM-MAT TO REG-MAT-DESCRIPCION
              PERFORM 7000-Escribir-archivo
           END-IF
           .
      *************************** Archivo ******************************
       7000-Abrir-archivo.
           OPEN OUTPUT MATERIAS
               IF FS-MATERIAS NOT = "00"
                   MOVE "Error al abrir archivo" TO WSV-ANULADO-DESCRIP
                   MOVE FS-MATERIAS TO WSV-ANULADO-CODIGO
                   MOVE "ARCH-MATERIAS"  TO WSV-ANULADO-OBJETO
                   PERFORM 8900-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: ARCH-MATERIAS"
               END-IF
           .
       7000-Escribir-archivo.
           WRITE REGISTRO-ARCH-MATERIA
           DISPLAY "Registro escrito en ATCH-MATERIAS: "
                   REGISTRO-ARCH-MATERIA
           .
       7000-Cerrar-archivo.
           CLOSE MATERIAS
               IF FS-MATERIAS NOT = "00"
                   MOVE "Error al cerrar archivo" TO WSV-ANULADO-DESCRIP
                   MOVE FS-MATERIAS TO WSV-ANULADO-CODIGO
                   MOVE "ARCH-MATERIAS"  TO WSV-ANULADO-OBJETO
                   PERFORM 8900-Mostrar-anulado
               ELSE
                   DISPLAY "Pude cerrar el archivo: MATERIAS"
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
