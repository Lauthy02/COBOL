      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Realice el código fuente de un programa que pida al usuario las 
      *tres partes de la fecha del Problema #1, y las presente con los 
      *guiones en pantalla. Considerando que la salida del mismo deberá 
      *ser como sigue.

000001 IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0002
       AUTHOR.                     Lautaro-Rojas
       DATE-WRITTEN.               04/10/2022
       DATE-COMPILED.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
000002 ENVIRONMENT DIVISION.
      *-----------------------
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
000003 DATA DIVISION.
      *-----------------------
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 Fecha.
           02 Dia PIC 9(2).
           02 Mes PIC 9(2).
           02 Anio PIC 9(4).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
000004 PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY    "Ingrese el dia: "
           ACCEPT Dia
           DISPLAY    "Ingrese el mes: "
           ACCEPT Mes
           DISPLAY    "Ingrese el anio: "
           ACCEPT Anio
           DISPLAY Dia"/"Mes"/"Anio
           STOP RUN.
       END PROGRAM PROG0002.