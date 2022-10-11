      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Definir la tabla   WST-MESES , dándole valor a los campos con 
      *los nombre de los meses y mostrar todos los campos , con DISPLAY

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                EJE009A1.
       AUTHOR.                    Lautaro-Rojas
       DATE-WRITTEN.              09/10/2022.
       DATE-COMPILED.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       ENVIRONMENT DIVISION.
      *-----------------------
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.

       01  WSV-CONTADOR    PIC 9(2)    VALUE 0.
      *WSC --> Para constantes
       01  WSC-MES.
           02 FILLER PIC x(10) VALUE "Enero     ".
           02 FILLER PIC x(10) VALUE "Febrero   ".
           02 FILLER PIC x(10) VALUE "Marzo     ".
           02 FILLER PIC x(10) VALUE "Abril     ".
           02 FILLER PIC x(10) VALUE "Mayo      ".
           02 FILLER PIC x(10) VALUE "Junio     ".
           02 FILLER PIC x(10) VALUE "Julio     ".
           02 FILLER PIC x(10) VALUE "Agosto    ".
           02 FILLER PIC x(10) VALUE "Septiembre".
           02 FILLER PIC x(10) VALUE "Octubre   ".
           02 FILLER PIC x(10) VALUE "Noviembre ".
           02 FILLER PIC x(10) VALUE "Diciembre ".
       
      *WST --> Para las tablas
       01  WST-MESES-TAB REDEFINES WSC-MES.
           02 WST-MES PIC X(10) OCCURS 12.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-IMPRIMIR UNTIL WSV-CONTADOR > 12.
           PERFORM 000003-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
       
       000002-IMPRIMIR.
           DISPLAY WST-MES(WSV-CONTADOR).
           ADD 1 TO WSV-CONTADOR.

       000003-FIN-DEL-PROGRAMA.
           DISPLAY " "
           DISPLAY "----El programa finalizó----".
           STOP RUN.

