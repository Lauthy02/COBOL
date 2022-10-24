      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Defina una estructura en memoria conveniente para almacenar una 
      *fecha en formato Día-Mes-Año de la forma DD-MM-AAAA (2 dígitos 
      *para el día, 2 dígitos para el mes, 4 dígitos para el año). 
      *Compile el código fuente requerido para verificar la sintaxis.

000001 IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0001.
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
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
000004 PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY    "SIUUUUUUUUUUU"
            STOP RUN.
       END PROGRAM PROG0001.