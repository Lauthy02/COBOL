      * Problema #3 
      *Realizar un programa que cargue una matriz por filas y luego 
      *la muestre en pantalla por columnas. Compile y ejecute el 
      *programa y verifique que el resultado sea el esperado.
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                Prob3-Matriz.
       AUTHOR.                    Lautaro-Rojas.
       DATE-WRITTEN.              12/10/2022.
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

       01  WSC-MES.
           05 FILLER PIC X(30) VALUE '00345-00500-00445-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00360-00455-00440-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00333-00521-00446-00094-00000-'.
           05 FILLER PIC X(30) VALUE '00300-00654-00443-00100-00000-'.
           05 FILLER PIC X(30) VALUE '00345-00590-00454-00089-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00490-00101-00000-'.
           05 FILLER PIC X(30) VALUE '00323-00600-00435-00092-00000-'.
           05 FILLER PIC X(30) VALUE '00299-00532-00390-00085-00000-'.
           05 FILLER PIC X(30) VALUE '00346-00534-00449-00090-00000-'.
           05 FILLER PIC X(30) VALUE '00321-00536-00545-00095-00000-'.
           05 FILLER PIC X(30) VALUE '00344-00569-00345-00093-00000-'.
           05 FILLER PIC X(30) VALUE '00380-00566-00390-00075-00000-'.
           05 FILLER PIC X(30) VALUE '00000-00000-00000-00000-00000-'.

       01  WST-MESES REDEFINES WSC-MES.
           03 WST-MESES-DET OCCURS 13.
               05 WST-GASTOS-DET OCCURS 5.
                   10 WST-GASTOS   PIC 9(05).
                   10 FILLER       PIC X.
       01  WSV-VAR.
           02  WSV-CONT-COL        PIC 9(02).
           02  WSV-CONT-FIL        PIC 9(02).
           02  WSV-TOT-COL         PIC 9(05).
           02  WSV-TOT-FIL         PIC 9(05).
           02  WSV-TOTAL-COLUMNA-5 PIC 9(05).
           02  WSV-TOTAL-FILA-13   PIC 9(05).

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000006-IMPRIMIR-MATRIZ.
           PERFORM 000002-PROCESO-SUMAR-COLUMNAS.
           PERFORM 000006-IMPRIMIR-MATRIZ.
           PERFORM 000003-PROCESO-SUMAR-FILAS.
           PERFORM 000006-IMPRIMIR-MATRIZ.
           PERFORM 000004-PROCESO-SUMAR-TOTALES.
           PERFORM 000005-PROCESO-VALIDAR-SUMAS.
           PERFORM 000007-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
           DISPLAY " ".
       
       000002-PROCESO-SUMAR-COLUMNAS.
           DISPLAY " "
           DISPLAY "Sumando columnas"
           DISPLAY " "
           PERFORM VARYING WSV-CONT-COL FROM 1
           BY 1 UNTIL WSV-CONT-COL > 4
               PERFORM VARYING WSV-CONT-FIL FROM 1
               BY 1 UNTIL WSV-CONT-FIL > 13
                   ADD WST-GASTOS(WSV-CONT-FIL,WSV-CONT-COL) 
                   TO WSV-TOT-COL
               END-PERFORM
               MOVE WSV-TOT-COL TO WST-GASTOS(13,WSV-CONT-COL)
               MOVE 0 TO WSV-TOT-COL
           END-PERFORM.

       000003-PROCESO-SUMAR-FILAS.
           DISPLAY " "
           DISPLAY "Sumando filas"
           DISPLAY " "
           PERFORM VARYING WSV-CONT-FIL FROM 1
           BY 1 UNTIL WSV-CONT-FIL > 12
               PERFORM VARYING WSV-CONT-COL FROM 1
               BY 1 UNTIL WSV-CONT-COL > 5
                   ADD WST-GASTOS(WSV-CONT-FIL,WSV-CONT-COL) 
                   TO WSV-TOT-FIL
               END-PERFORM
               MOVE WSV-TOT-FIL TO WST-GASTOS(WSV-CONT-FIL,5)
               MOVE 0 TO WSV-TOT-FIL
           END-PERFORM.

       000004-PROCESO-SUMAR-TOTALES.
           DISPLAY " "
           DISPLAY "Sumando el total de columnas"
           DISPLAY " "
           PERFORM VARYING WSV-CONT-COL FROM 1
           BY 1 UNTIL WSV-CONT-COL > 5
               ADD WST-GASTOS(13,WSV-CONT-COL) TO WSV-TOTAL-COLUMNA-5
           END-PERFORM
           DISPLAY " "
           DISPLAY "Sumando el total de filas"
           DISPLAY " "
           PERFORM VARYING WSV-CONT-FIL FROM 1
           BY 1 UNTIL WSV-CONT-FIL > 12
               ADD WST-GASTOS(WSV-CONT-FIL,5) TO WSV-TOTAL-FILA-13
           END-PERFORM.

       000005-PROCESO-VALIDAR-SUMAS.
           DISPLAY " "
           DISPLAY "Validando totales"
           DISPLAY " "
           IF WSV-TOTAL-COLUMNA-5 = WSV-TOTAL-FILA-13
               MOVE WSV-TOTAL-COLUMNA-5 TO WST-GASTOS(13,5)
               PERFORM 000006-IMPRIMIR-MATRIZ
           ELSE
               DISPLAY "La suma de columnas y filas son distintas"
               DISPLAY "Total fila: " WSV-TOTAL-FILA-13
               DISPLAY "Total columna: " WSV-TOTAL-COLUMNA-5.

       000006-IMPRIMIR-MATRIZ.
           DISPLAY " "
           DISPLAY "Mostrando matriz"
           DISPLAY " "
           PERFORM VARYING WSV-CONT-FIL FROM 1
           BY 1 UNTIL WSV-CONT-FIL > 13
               PERFORM VARYING WSV-CONT-COL FROM 1
               BY 1 UNTIL WSV-CONT-COL > 5
                   DISPLAY "| " WST-GASTOS(WSV-CONT-FIL,WSV-CONT-COL)" "
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY "| " WITH NO ADVANCING 
               DISPLAY " "
           END-PERFORM.
               
       000007-FIN-DEL-PROGRAMA.
           DISPLAY " ".
           DISPLAY "----El programa finalizó----".
           STOP RUN.