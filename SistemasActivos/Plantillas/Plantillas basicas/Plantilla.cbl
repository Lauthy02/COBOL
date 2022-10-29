      *--1----.----2----.----3----.----4----.----5----.----6----.----7----.----8 
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                <NOMBRE-DEL-PROGRAMA>
       AUTHOR.                    <NOMBRE-DEL-AUTOR>
       INSTALLATION.
       DATE-WRITTEN.              <XX/XX/XXXX>
       DATE-COMPILED.
       SECURITY.                  
      *-------------------------------------------------------------------------
       ENVIRONMENT DIVISION.
      * Coneccion del programa con el exterior
      *--------
       CONFIGURATION SECTION.
       SOURCE COMPUTER.            IBM-3183.
       OBJECT COMPUTER.            IBM-3083.
      *--------
       INPUT-OUTPUT SECTION.
      * 
       FILE-CONTROL.
           SELECT <NOMBRE> ASSIGN TO UT-S-<NOMBRE>.
           SELECT <NOMBRE> ASSIGN TO UT-S-<NOMBRE>.
      *-------------------------------------------------------------------------
       DATA DIVISION.
      *--------
       FILE SECTION.
      *
      *--------
       WORKING-STORAGE SECTION.
      * Definicion de todas las variables q vamos a usar
      *--------
       LINKAGE SECTION
      *-------------------------------------------------------------------------
       PROCEDURE DIVISION.