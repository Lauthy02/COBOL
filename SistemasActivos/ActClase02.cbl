      *Hacer una matriz de 3*3, cargarla con números y sumar la diagonal
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                ActClase02.
       AUTHOR.                    Lautaro-Rojas.
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

       01  WST-MATRIZ.
           02 WST-FILA OCCURS 3 TIMES.
             03 WST-COLUMNA OCCURS 3 TIMES.
               04 WS-NUMERO PIC 9(02).

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-IMPRIMIR.
           PERFORM 000003-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
       
       000002-IMPRIMIR.
           PERFORM VARYING WSV-CONT FROM 1 BY 1 UNTIL WSV-CONT > 12
               DISPLAY WST-MES(WSV-CONT)
           END-PERFORM.

       000003-FIN-DEL-PROGRAMA.
           DISPLAY " ".
           DISPLAY "----El programa finalizó----".
           STOP RUN.