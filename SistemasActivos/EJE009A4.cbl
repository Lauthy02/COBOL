      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                EJE009A4.
       AUTHOR.                    Lautaro-Rojas.
       DATE-WRITTEN.              11/10/2022.
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

       01 WSC-MES .
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

       01 WST-MESES REDEFINES WSC-MES.
           03 WST-MESES-DET OCCURS 13.
               05 WST-GASTOS-DET OCCURS 5.
                   10 WST-GASTOS PIC 9(5).
                   10 FILLER PIC X.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      * TENGO Q MOSTRAR LA MATRIZ AL ARRANQUE Y AL FINAL PARA VER COMO 
      * QUEDA
      *la 5 col y la fila 13 es para sumatoria, y la celda 13 y 5 es 
      *para suma de la condicion
      *usar perform varying
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-
           PERFORM 000007-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
       
       000002-PROCESO-SUMAR-COLUMNAS.
       000003-PROCESO-SUMAR-FILAS.
       000004-PROCESO-SUMAR-TOTALES.
       000005-PROCESO-VALIDAR-SUMAS.
       000006-IMPRIMIR-MATRIZ.

       000007-FIN-DEL-PROGRAMA.
           DISPLAY " ".
           DISPLAY "----El programa finalizó----".
           STOP RUN.