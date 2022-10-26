      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *
000001 IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0004.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               04/10/2022.
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
       77  WSS-ESTADO-CIVIL    PIC X.
           88 WSS-CASADO       VALUE "C".
           88 WSS-SOLTERO      VALUE "S".
           88 WSS-VIUDO        VALUE "V".
           88 WSS-DIVORCIADO   VALUE "D".
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
000004 PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *Compila bien, no tiene errores.
           STOP RUN.