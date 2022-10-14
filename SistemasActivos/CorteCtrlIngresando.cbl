      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      *Ingresar clientes y hacer corte de control cada vez q cambia el 
      *cliente
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 CorteCrtlIngresando.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               13/10/2022.
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
       01  WSV-VARIANLES.
           02 WSV-NRO-CLIENTE          PIC 9(03).
           02 WSV-NRO-CLIENTE-ANTERIOR PIC 9(03).
           02 WSV-OPERACION            PIC 9(03).
           02 WSV-SALDO-CUENTA         PIC 9(05).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       000000-MAIN-PROCEDURE.
           PERFORM 000010-INICIO-DEL-PROGRAMA.
           PERFORM 000011-DEFINICIION-DE-CLIENTE.
           PERFORM 000012-INGRESO-DE-DATOS UNTIL WSV-OPERACION = 0
           PERFORM 000040-FIN-DEL-PROGRAMA.

       000010-INICIO-DEL-PROGRAMA.
           DISPLAY "El programa inició"
           DISPLAY " ".
           
       000011-DEFINICIION-DE-CLIENTE.
           DISPLAY "Cual es el número de cliente (4 cifras xxxx): "
           ACCEPT WSV-NRO-CLIENTE.

       000012-INGRESO-DE-DATOS.
           DISPLAY " "
           DISPLAY "Ingreso de datos"
           DISPLAY " "
           DISPLAY "Ingrese las operaciones del cliente: " 
               WSV-NRO-CLIENTE
           DISPLAY "Ingrese 0 para dejar de ingresar operaciones"
           MOVE WSV-NRO-CLIENTE TO WSV-NRO-CLIENTE-ANTERIOR
           PERFORM 000013-CORTE UNTIL WSV-NRO-CLIENTE <> 
               WSV-NRO-CLIENTE-ANTERIOR.

       000013-CORTE.
           IF WSV-OPERACION > 0
               COMPUTE WSV-SALDO-CUENTA = WSV-SALDO-CUENTA 
                   + WSV-OPERACION
           ELSE
               IF WSV-SALDO-CUENTA = 0
                   DISPLAY "El saldo de la cuenta está en 0"
               ELSE
                   COMPUTE WSV-SALDO-CUENTA = WSV-SALDO-CUENTA 
                           - WSV-OPERACION.       

       000040-FIN-DEL-PROGRAMA.
           DISPLAY " "
           DISPLAY "El programa terminó"
           STOP RUN. 