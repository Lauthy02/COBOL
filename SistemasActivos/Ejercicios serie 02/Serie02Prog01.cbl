      *Problema #1
      *Realizar un programa que pida al usuario los números para cargar 
      *los valores de una matriz de 5 x 5 por filas y luego calcule el 
      *valor de la suma de los elementos de la diagonal principal. 
      *Compile y ejecute el programa y verifique que el resultado sea 
      *el esperado.
      *      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 Serie02Prog01.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               03/10/2022.
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
           02  WST-MATRIZ-FILA OCCURS 5 TIMES.
               03  WST-MATRIZ-COLUMNA OCCURS 5 TIMES.
                   04  WST-MATRIZ-NUM  PIC 9(02).
       01  WSV-VAR.
           02  WSV-FILA                PIC 9(01).
           02  WSV-COLUMNA             PIC 9(01).
           02  WSV-TOTAL               PIC 9(03).
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-CARGAR-DATOS.
           PERFORM 000003-IMPRIMIR-MATRIZ.
           PERFORM 000004-CALCULAR-DIAGONAL.
           PERFORM 000005-FIN-DEL-PROGRAMA.
       
       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "El programa inició"
           DISPLAY " ".

       000002-CARGAR-DATOS.
           DISPLAY " "
           DISPLAY "Cargando datos"
           DISPLAY " "
           PERFORM VARYING WSV-FILA FROM 1
           BY 1 UNTIL WSV-FILA > 5
               PERFORM VARYING WSV-COLUMNA FROM 1
               BY 1 UNTIL WSV-COLUMNA > 5
                   DISPLAY "Ingrese número en " WSV-FILA";"WSV-COLUMNA
                   ACCEPT WST-MATRIZ-NUM(WSV-FILA,WSV-COLUMNA) 
               END-PERFORM
           END-PERFORM.  

       000003-IMPRIMIR-MATRIZ.
           DISPLAY " "
           DISPLAY "Mostrando matriz"
           DISPLAY " "
           PERFORM VARYING WSV-FILA FROM 1
           BY 1 UNTIL WSV-FILA > 5
               PERFORM VARYING WSV-COLUMNA FROM 1
               BY 1 UNTIL WSV-COLUMNA > 5
                   DISPLAY "| " WST-MATRIZ-NUM(WSV-FILA,WSV-COLUMNA)" "
                   WITH NO ADVANCING 
               END-PERFORM
               DISPLAY "| " WITH NO ADVANCING 
               DISPLAY " "
           END-PERFORM.

       000004-CALCULAR-DIAGONAL.
           DISPLAY " "
           DISPLAY "Calculando diagonal"
           DISPLAY " "
           PERFORM VARYING WSV-FILA FROM 1
           BY 1 UNTIL WSV-FILA > 5
               PERFORM VARYING WSV-COLUMNA FROM 1
               BY 1 UNTIL WSV-COLUMNA > 5
                   IF WSV-FILA = WSV-COLUMNA
                       ADD WST-MATRIZ-NUM(WSV-FILA,WSV-COLUMNA)
                       TO WSV-TOTAL
               END-PERFORM
           END-PERFORM
           DISPLAY "El total de la diagonal es: " WSV-TOTAL.
       
       000005-FIN-DEL-PROGRAMA.
           DISPLAY " "
           DISPLAY "El programa terminó"
           STOP RUN.          