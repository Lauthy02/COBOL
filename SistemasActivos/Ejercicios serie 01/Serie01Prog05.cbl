      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Realice el código fuente de un programa que pida al usuario las 
      *tres partes de la fecha del Problema #1, y las presente con los 
      *guiones en pantalla. Considerando que la salida del mismo deberá 
      *ser como sigue.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0005.
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
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       77  WSS-RIESGO    PIC 9(1)      VALUE 0.
           88 WSS-APROBAR-RIESGO       VALUE 1 2 3.
           88 WSS-DESAPROBAR-RIESGO    VALUE 4 5 6 7 8 9.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 00001-Inicio-programa.
           PERFORM 00010-Pedir-datos.
           PERFORM 00011-Verificar-riesgo.
           PERFORM 00020-Fin-del-programa.
       
       00001-Inicio-programa.
           DISPLAY "-----El programa inició----"
           DISPLAY " ".
       
       00010-Pedir-datos.
           DISPLAY "Ingrese riesgo de la operación (1 a 9): "
           ACCEPT WSS-RIESGO.
       
       00011-Verificar-riesgo.
           IF WSS-APROBAR-RIESGO 
               DISPLAY "Operación de riesgo " WSS-RIESGO " aprobada."
           ELSE 
               DISPLAY "Operación de riesgo " WSS-RIESGO 
                   " desaprobada."
           END-IF.
       
       00020-Fin-del-programa.
           DISPLAY " "
           DISPLAY "----Fin del programa----"
           STOP RUN.