      *Registro ed 80 posiciones alfanuméricos con un texto al azar
      *
      *Vamos a trabajar con este campo con un value el cual puede tener 
      *una frase como "Hoy es mi cumple" y
      *al terminar esa frase tiene un ;
      *
      *El ejercicio es: 
      *Recorrer este registro caracter por caracter hasta encontrar el ;
      *Hay q redefinir este registro de 80 en una tabla de un caracter 
      *solo de una x OCCURS 80
      *
      *Para buscar el ; usar PERFOR VARYING, obtenido el ; hacer 
      *display de este registro de 80 seccionado (1) y 
      *la posición del ; -1
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                ActClase01.
       AUTHOR.                    Lautaro-Rojas
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
       01  WSV-POS         PIC 9(2)    VALUE 0.
       01  WSV-CONT        PIC 9(2)    VALUE 0.
      *
       01  WSC-REG-80      PIC X(80).
      *
       01  WST-80 REDEFINES WSC-REG-80.
           02 WST-REG80    PIC X(1)    OCCURS 80.
      *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 000001-INICIO-DEL-PROGRAMA.
           PERFORM 000002-INGRESAR-TXT.
           PERFORM 000003-IMPRIMIR.
           PERFORM 000004-FIN-DEL-PROGRAMA.
      
       000001-INICIO-DEL-PROGRAMA.
           DISPLAY "----El programa inició----".
           DISPLAY " ".
      
       000002-INGRESAR-TXT.
           DISPLAY "Ingrese el texto deseado (max 80 caracteres)".
           DISPLAY "NO OLIVAR TERMINAR LA FRASE CON ;".
           ACCEPT WSC-REG-80.
      
       000003-IMPRIMIR.
           PERFORM VARYING WSV-CONT FROM 1 BY 1 UNTIL WSV-CONT > 80
               IF  WST-REG80(WSV-CONT) = ";"
                   ADD WSV-CONT TO WSV-POS
               END-IF
           END-PERFORM.
           DISPLAY "El ; está en la psosición: " WSV-POS.
           SUBTRACT WSV-POS FROM 1 GIVING WSV-POS.
           DISPLAY "El texto es: " WSC-REG-80(1:WSV-POS).
      
       000004-FIN-DEL-PROGRAMA.
           DISPLAY " ".
           DISPLAY "----El programa finalizó----".
           STOP RUN.