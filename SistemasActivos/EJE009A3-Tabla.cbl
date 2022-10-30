      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Indicar con DISPLAY que se debe entrar el nombre de un mes
      *El programa debe permitir ingresar ese dato, con ACCEPT. 
      *Mostrar el número que corresponde al mes ingresado 

      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                EJE009A3.
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

       01  WSV-VALOR    PIC 9(2)       VALUE 0.
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
           PERFORM 000002-INGRESO-DE-FECHA.
           PERFORM 000003-IMPRIMIR.
           PERFORM 000004-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
       
       000002-INGRESO-DE-FECHA.
           DISPLAY "Debe ingresar un número del 1 al 12".
           ACCEPT WSV-VALOR.
       
       000003-IMPRIMIR.
           DISPLAY WST-MES(WSV-VALOR).

       000004-FIN-DEL-PROGRAMA.
           DISPLAY "----El programa finalizó----".
           STOP RUN.