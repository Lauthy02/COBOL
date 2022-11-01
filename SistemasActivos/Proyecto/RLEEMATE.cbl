      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      *    Programa rutina para leer el archivo de materias y lo guarde
      *    en una tabla
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 RGENMATE.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               01/11/2022.
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
      *Tabla 
           01 WST-MATRIZ.
               02 WST-FILA OCCURS 8 TIMES.
                   03 WST-COLUMNA OCCURS 2 TIMES.
                       04 WST-NUMERO           PIC 9(02).
                       04 WST-DESCRI           PIC 9(25).

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      *-------------------------- Programa -----------------------------
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-Inicio-programa
           PERFORM 5000-Proceso
           PERFORM 9000-Fin-programa
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
           DISPLAY "Sumando columnas"
           PERFORM VARYING WSV-CONT-FIL FROM 1
           BY 1 UNTIL WSV-CONT-FIL > 8
               PERFORM VARYING WSV-CONT-COL FROM 1
               BY 1 UNTIL WSV-CONT-COL > 2
                   
               END-PERFORM
               MOVE WSV-TOT-COL TO WST-GASTOS(13,WSV-CONT-COL)
               MOVE 0 TO WSV-TOT-COL
           END-PERFORM.
       .
      *************************** Archivo ******************************
       7000-Abrir-archivo.
           OPEN INPUT MATERIAS
               IF FS-MATERIAS NOT = "00"
                   MOVE "Error al abrir archivo" TO WSV-ANULADO-DESCRIP
                   MOVE FS-MATERIAS TO WSV-ANULADO-CODIGO
                   MOVE "ARCH-MATERIAS"  TO WSV-ANULADO-OBJETO
                   PERFORM 8900-Mostrar-anulado
               ELSE
                   DISPLAY "Pude abrir el archivo: ARCH-MATERIAS"
               END-IF
           .
       7000-Leer-archivo.
           READ REGISTRO-ARCH-MATERIA
           DISPLAY "Registro leido en ARCH-MATERIAS: "
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
       9000-Fin-programa.
           PERFORM 7000-Cerrar-archivo.
           DISPLAY " "
           DISPLAY "---- Fin del programa ----"
           PERFORM 9999-Stop-Run
           .
       9999-Stop-Run.
           STOP RUN.
