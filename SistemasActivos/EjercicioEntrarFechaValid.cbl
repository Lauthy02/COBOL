      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *Hacer un programa q solicite la fecha y q tenga
      *A-un párrafo INGRESO-DE-FECHA
      *B-un párrafo que muestra la fecha ingresada MUESTRA-FECHA
      *C-un párrafo para el fin   FIN-DEL-PROGRAMA
      *D-Hacer párrafo de ejecución 000000-CONTROL

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROGIngFech.
       AUTHOR.                     Lautaro-Rojas.
       DATE-WRITTEN.               05/10/2022.
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
       01 Fecha.
           02 Dia PIC 9(2).
           02 Mes PIC 9(2).
           02 Anio PIC 9(4).

       77 ValidDia PIC XX
           88 ValidDiaOK VALUE 01.
           88 ValidDiaNoOK VALUE 00.

       77 ValidMes PIC XX
           88 ValidMesOK VALUE 01.
           88 ValidMesNoOK VALUE 00.

       77 ValidAnio PIC XX
           88 ValidAnioOK VALUE 01.
           88 ValidAnioNoOK VALUE 00.

      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       000000-CONTROL.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-INGRESO-DE-FECHA.
           PERFORM 000003-MUESTRA-FECHA.
           PERFORM 000004-FIN-DEL-PROGRAMA.

       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "El programa inició".

       000002-INGRESO-DE-FECHA.
           DISPLAY    "Ingrese el dia: ".
           ACCEPT Dia.
           DISPLAY    "Ingrese el mes: ".
           ACCEPT Mes.
           DISPLAY    "Ingrese el anio: ".
           ACCEPT Anio.

       000003-MUESTRA-FECHA.
           DISPLAY Dia"/"Mes"/"Anio.
           DISPLAY Fecha.

       000004-VALIDAR-DIA.
           if Dia < 0 or Dia > 31 then
               MOVE TRUE to ValidDia
           else
               MOVE FALSE to ValidDia
       
           end-if

       00000X-FIN-DEL-PROGRAMA.
           DISPLAY "El programa terminó".
           STOP RUN. 


