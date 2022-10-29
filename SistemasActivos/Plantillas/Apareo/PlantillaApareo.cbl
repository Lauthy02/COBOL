       00001-Inicio-programa.
           DISPLAY "----- El programa inicio ----".
           DISPLAY " "
           PERFORM 00005-Abrir-archivos
      *    maestro es de la izquierda/maestro
           PERFORM 00006-Leer-maestro
      *    detalle es el de la derecha/detalle
           PERFORM 00007-Leer-detalle

      *************************** Apareo *******************************
       00050-Apareo.
      *    Repetir 00000-A hasta EOF
           PERFORM 00000-A UNTIL FILE-STATUS-MAESTRO = "10"
           DISPLAY "Sali del 00050-Apareo".

       00000-A.
           DISPLAY "Entre al 00000-A"
      *    Repetir 00000-B hasta EOF o A<B
           PERFORM 00000-B UNTIL (FILE-STATUS-DETALLE = "10")
                   OR (INDICE-MAESTRO < INDICE-DETALLE).
           PERFORM 00006-Leer-maestro.

       00000-B.
           DISPLAY "Entre al 00000-B"
           IF (INDICE-MAESTRO = INDICE-DETALLE)
      *        Es donde están apareados los registos y donde debo hacer 
      *        todas las cuentas
               PERFORM 00007-Leer-detalle
           ELSE
               IF (INDICE-MAESTRO > INDICE-DETALLE)
      *            No hay en detalle q si hay en maestro
      *            entonces sigo avanzando con el detalle
                   PERFORM 00007-Leer-detalle
               ELSE
      *            Hay algo en maestro que no está en detalle
               END-IF
           END-IF.