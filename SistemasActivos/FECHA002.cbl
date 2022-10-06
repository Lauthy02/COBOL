      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 FECHA002.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               05/10/2022.
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
       01  SMV-Fecha.
           02 Dia          PIC 9(2)    VALUE ZEROES.
           02 Mes          PIC 9(2)    VALUE ZEROES.
           02 Anio         PIC 9(4)    VALUE ZEROES.

       77  WSS-DIA         PIC XX      VALUE "OK".
           88 DIA-INIC                 VALUE "IN".
           88 DIA-OK                   VALUE "OK".

       77  WSS-MES         PIC XX      VALUE "OK".
           88 MES-INIC                 VALUE "IN".
           88 MES-OK                   VALUE "OK".

       77  WSS-ANIO        PIC XX      VALUE "OK".
           88 ANIO-INIC                VALUE "IN".
           88 ANIO-OK                  VALUE "OK".

       01  MSV-CONTADOR    PIC 9       VALUE 0.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       000000-MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-INGRESO-DE-FECHA.
           PERFORM 000003-VALIDAR-FECHA.               
           IF (DIA-OK AND MES-OK AND ANIO-OK) 
               PERFORM 000004-MUESTRA-FECHA
               PERFORM 000005-FIN-DEL-PROGRAMA
           ELSE
               DISPLAY "Ingresar fecha nuevamente"
               PERFORM 000000-MAIN-PROCEDURE
           END-IF.

       000001-INICIO-DEL-PROGRAMA.
           IF (MSV-CONTADOR = 0)
               DISPLAY "El programa inició"
               ADD 1 TO MSV-CONTADOR
           ELSE
               DISPLAY " "
           END-IF.

       000002-INGRESO-DE-FECHA.
           DISPLAY " "
           DISPLAY    "Ingrese el dia: "
           ACCEPT Dia
           DISPLAY    "Ingrese el mes: "
           ACCEPT Mes
           DISPLAY    "Ingrese el anio: "
           ACCEPT Anio.

       000003-VALIDAR-FECHA.
           PERFORM 0000031-VALIDAR-DIA.
           PERFORM 0000032-VALIDAR-MES.
           PERFORM 0000033-VALIDAR-ANIO.

       0000031-VALIDAR-DIA.
           IF Dia >= 1 AND Dia <= 31
               SET DIA-OK TO TRUE
           ELSE
               SET DIA-INIC TO TRUE
               DISPLAY "Día incorrecto"
           END-IF.

       0000032-VALIDAR-MES.
           IF Mes >= 1 AND Mes <= 12
               SET MES-OK TO TRUE
           ELSE
               SET MES-INIC TO TRUE
               DISPLAY "Mes incorrecto"
           END-IF.

       0000033-VALIDAR-ANIO.
           IF Anio >= 1990 AND Anio <= 2040
               SET ANIO-OK TO TRUE
           ELSE
               SET ANIO-INIC TO TRUE
               DISPLAY "Anio incorrecto"
           END-IF.

       000004-MUESTRA-FECHA.
           DISPLAY " "
           DISPLAY "La fecha es: "Dia"/"Mes"/"Anio.

       000005-FIN-DEL-PROGRAMA.
           DISPLAY " "
           DISPLAY "El programa terminó".
           STOP RUN. 


