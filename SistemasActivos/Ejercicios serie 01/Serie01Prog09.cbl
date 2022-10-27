      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG0009.
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
       01  NOMBRE-COMPLETO.
           02 NOMBRE       PIC X(15).
           02 APELLIDO     PIC X(15).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 00001-Inicio-programa.
           PERFORM 00002-Pedir-datos.
           PERFORM 00003-Imprimir.
           PERFORM 00020-Fin-del-programa.
       
       00001-Inicio-programa.
           DISPLAY "-----El programa inició----".
           DISPLAY " ".

       00002-Pedir-datos.
           DISPLAY "Ingrese el nombre: "
           ACCEPT NOMBRE
           DISPLAY "Ingrese el apellido: "
           ACCEPT APELLIDO.

       00003-Imprimir.
           DISPLAY "El nombre es: " NOMBRE 
           DISPLAY "El apellido es: " APELLIDO.

       00020-Fin-del-programa.
           DISPLAY " "
           DISPLAY "----Fin del programa----"
           STOP RUN.