      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *
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
       01  WSR-CTA-BANCARIA
           02 WSR-SUCURSAL     PIC 9(03).
           02 WSR-PRODUCTO     PIC 9(02).
              88 WSR-CAJA-DE-AHORRO        VALUE 40.
              88 WSR-PRESTAMO-PRENDARIO    VALUE 50.
              88 WSR-PRESTAMO-HIPOTEC      VALUE 60.
              88 WSR-CTA-CORRIENTE         VALUE 45.
           02 WSR-NRO-CUENTA   PIC 9(05).
           02 WSR-DIGITO-VERIF PIC 9(01).
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
           DISPLAY "Ingrese el número de cuenta (xxx-xx-xxxxx-x): "
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