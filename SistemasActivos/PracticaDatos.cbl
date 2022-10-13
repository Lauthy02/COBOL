      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8
      *Entrar fecha y validar
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PracticaDatos.
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
      *----Tabla
       01  WSC-TAB.
           02 FILLER PIC 9(2) VALUE 10.
           02 FILLER PIC 9(2) VALUE 20.
           02 FILLER PIC 9(2) VALUE 05.
           02 FILLER PIC 9(2) VALUE 30.
           02 FILLER PIC 9(2) VALUE 14.
           02 FILLER PIC 9(2) VALUE 29.
           02 FILLER PIC 9(2) VALUE 08.
           02 FILLER PIC 9(2) VALUE 09.
           02 FILLER PIC 9(2) VALUE 19.
           02 FILLER PIC 9(2) VALUE 18.
       
      *----Redefino la tabla
       01  WST-TAB-VAL REDEFINES WSC-TAB.
           02 WST-TAB PIC 9(2) OCCURS 10.

      *----Variables
       01  WSV-VAR.
           02 WSV-MAX      PIC 9(2).
           02 WSV-MIN      PIC 9(2).
           02 INDICE       PIC 9(2).
           02 TOTAL        PIC 9(3).
           02 PROMEDIO     PIC 9(3).
           02 TOT-PRI-5    PIC 9(3).
           02 TOT-ULT-5    PIC 9(3).
           02 PROM-PRI-5   PIC 9(3).
           02 CONTADOR-PRI PIC 9(3).
           02 PROM-ULT-5   PIC 9(3).
           02 CONTADOR-UTL PIC 9(3).
           02 AUXILIAR     PIC 9(3).
           02 I            PIC 9(3).
           02 J            PIC 9(3).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       000000-MAIN-PROCEDURE.
           PERFORM 000010-INICIO-DEL-PROGRAMA.
           PERFORM 000012-MOSTRAR-TABLA.
           PERFORM 000011-CALC-TOTS.
           PERFORM 000020-MAXIMO.
           PERFORM 000021-MINIMO.
           PERFORM 000022-PROMEDIO-DE-TODO.
           PERFORM 000023-PROMEDIO-PRIMEROS-5.
           PERFORM 000024-PROMEDIO-ULTIMOS-5.
           PERFORM 000025-BURBUJEO.
           PERFORM 000040-FIN-DEL-PROGRAMA.

       000010-INICIO-DEL-PROGRAMA.
           DISPLAY "El programa inició"
           DISPLAY " ".

       000011-CALC-TOTS.
      *Promedio de los primeros 5 y desp el de los otros 5
      *Entonces caluclo los totales y uso contadores para saber por q 
      *dividir
       PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 10
           IF INDICE <= 5
               COMPUTE TOT-PRI-5 = TOT-PRI-5 + WST-TAB(INDICE)
               COMPUTE CONTADOR-PRI = CONTADOR-PRI + 1
           ELSE 
               COMPUTE TOT-ULT-5 = TOT-ULT-5 + WST-TAB(INDICE)
               COMPUTE CONTADOR-UTL = CONTADOR-UTL + 1
           END-IF
       END-PERFORM.

       000012-MOSTRAR-TABLA.
           DISPLAY " "
           DISPLAY "Mostrando tabla"
           DISPLAY " "
           MOVE ZEROES TO INDICE
           PERFORM VARYING INDICE FROM 1
           BY 1 UNTIL INDICE > 10
               DISPLAY "| " WST-TAB(INDICE)" "
               WITH NO ADVANCING 
               DISPLAY "| " WITH NO ADVANCING 
               DISPLAY " "
           END-PERFORM.

       000020-MAXIMO.
           MOVE ZEROES TO WSV-MAX
           MOVE ZEROES TO INDICE
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 10
               IF WST-TAB(INDICE) > WSV-MAX
                   MOVE WST-TAB(INDICE) TO WSV-MAX
               END-IF
           END-PERFORM.
       
       000021-MINIMO.
           MOVE ZEROES TO INDICE
           MOVE WST-TAB(1) TO WSV-MIN
           PERFORM VARYING INDICE FROM 2 BY 1 UNTIL INDICE > 10
               IF WST-TAB(INDICE) < WSV-MIN
                   MOVE WST-TAB(INDICE) TO WSV-MIN
               END-IF
           END-PERFORM.

       000022-PROMEDIO-DE-TODO.
           MOVE ZEROES TO TOTAL
           MOVE ZEROES TO INDICE
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 10
               ADD WST-TAB(INDICE) TO TOTAL
           END-PERFORM
           SUBTRACT 1 FROM INDICE 
           DIVIDE TOTAL BY INDICE GIVING PROMEDIO.

       000023-PROMEDIO-PRIMEROS-5.
           DIVIDE TOT-PRI-5 BY CONTADOR-PRI GIVING PROM-PRI-5.

       000024-PROMEDIO-ULTIMOS-5.
           DIVIDE TOT-ULT-5 BY CONTADOR-UTL GIVING PROM-ULT-5.
       
       000025-BURBUJEO.
       DISPLAY " "
       DISPLAY "Iniciando burbujeo"
       DISPLAY " "
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > (10 - I)
               IF WST-TAB(J) > WST-TAB(J + 1)
                   MOVE WST-TAB(J) TO AUXILIAR
                   MOVE WST-TAB(J + 1) TO WST-TAB(J)
                   MOVE AUXILIAR TO WST-TAB(J + 1)
               END-IF
           END-PERFORM
           PERFORM 000012-MOSTRAR-TABLA
       END-PERFORM.

       000040-FIN-DEL-PROGRAMA.
           DISPLAY "El max es: " WSV-MAX
           DISPLAY "El min es: " WSV-MIN
           DISPLAY "Promedio: "TOTAL"/"INDICE"= "PROMEDIO
           DISPLAY "Promedio primeros 5: "
               TOT-PRI-5"/"CONTADOR-PRI"= "PROM-PRI-5
           DISPLAY "Promedio ultimos 5: "
               TOT-ULT-5"/"CONTADOR-UTL"= "PROM-ULT-5
           DISPLAY " "
           DISPLAY "El programa terminó"
           STOP RUN. 