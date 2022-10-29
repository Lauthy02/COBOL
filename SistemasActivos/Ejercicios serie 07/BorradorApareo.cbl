       
       00050-Apareo.
      *    Repetir 00000-A hasta EOF
      
       00000-A.
      *    Repetir 00000-B hasta EOF o A<B
      *    Leer A

       00000-B.
           IF (A = B)
      *        Leer B
           END-IF
           IF (A > B)
      *        Leer B
           END-IF
           IF (A < B)
      *        Mover a INCIDENCIAS
           END-IF