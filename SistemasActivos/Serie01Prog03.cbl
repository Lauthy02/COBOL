      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Al final del año 2000, la misma sufrió una modificación 
      *incorporando a la estructura el concepto de siglo (de forma que 
      *para el año 2000 se asuma valor de siglo 20 lo que tiene un fin 
      *práctico ya que el siglo en realidad debería ser 19), como 
      *rediseñaría dicha estructura para incorporar la variable siglo. 
      *Compile el código fuente requerido para verificar la sintaxis.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0003.
       AUTHOR.                     Lautaro-Rojas
       DATE-WRITTEN.               03/10/2022
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
       01 FECHA.
           02 Dia PIC 9(02).
           02 FILLER PIC X(1)
                 VALUE "/".
           02 Mes PIC 9(02).
           02 FILLER PIC X(1)
                 VALUE "/".
           02 Anio.
                03 SiGLO PIC 9(2).
                03 ANIO-ACT PIC 9(2).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       END PROGRAM PROG0003.