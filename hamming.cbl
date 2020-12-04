******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WKS-hamming.
               05 WS-H PIC 9 OCCURS 33 TIMES.
      *    VARIABLES PARA CORRECCION
           01 WKS-CODIGO-COPIA.
               05 WS-CC PIC 9 OCCURS 33 TIMES.
           01 WKS-PARIDADES-1.
               05 WS-P1 PIC 9 OCCURS 4 TIMES.
           01 WKS-PARIDADES-2.
               05 WS-P2 PIC 9 OCCURS 4 TIMES.
           01 WKS-PARIDADES-3.
               05 WS-P3 PIC 9 OCCURS 4 TIMES.

           77 WKS-CONTADOR-COLUMNAS PIC 99.
           77 WKS-BITCOPIA1 PIC 99.
           77 WKS-BITCOPIA2 PIC 99.


      *    VARIABLES PARA CREACION
           77 WKS-letra PIC A.
           77 WKS-BASURA PIC 9.
           77 WKS-AUX PIC 9.
           77 WKS-AUX2 PIC 9.
           77 WKS-posBitError PIC 99.

           01 WS-CODIGO.
               05 WS-A PIC 9 OCCURS 7 TIMES.

           77 WKS-opcion PIC 9 VALUE 0.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WKS-opcion = 3
               DISPLAY "1.-Crea tu codigo Hamming (33,7)"
               DISPLAY "2.-Deteccion de errores(33,7)"
               DISPLAY "3.-Salir"
               ACCEPT WKS-opcion
               EVALUATE TRUE
                   WHEN WKS-opcion = 1
                       PERFORM CREAR-HAMMING
                   WHEN WKS-opcion = 2
                       PERFORM CORREGIR-ERROR
                   WHEN WKS-opcion = 3
                       DISPLAY "adios"
                   WHEN OTHER
                       DISPLAY "opcion no valida"
               END-EVALUATE
           END-PERFORM.

           STOP RUN.

           CREAR-HAMMING.
               DISPLAY "introduce el codigo binario".
               ACCEPT WS-CODIGO.

               MOVE WS-A(1) TO WS-H(3),WS-H(14),WS-H(25).
               MOVE WS-A(2) TO WS-H(5),WS-H(16),WS-H(27).
               MOVE WS-A(3) TO WS-H(6),WS-H(17),WS-H(28).
               MOVE WS-A(4) TO WS-H(7),WS-H(18),WS-H(29).
               MOVE WS-A(5) TO WS-H(9),WS-H(20),WS-H(31).
               MOVE WS-A(6) TO WS-H(10),WS-H(21),WS-H(32).
               MOVE WS-A(7) TO WS-H(11),WS-H(22),WS-H(33).

               COMPUTE WKS-AUX = WS-H(3)+WS-H(5)+WS-H(7)+WS-H(9)
               +WS-H(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-H(1).
               MOVE WS-H(1) TO WS-H(12),WS-H(23).

               COMPUTE WKS-AUX = WS-H(3)+WS-H(6)+WS-H(7)+WS-H(10)
               +WS-H(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-H(2).
               MOVE WS-H(2) TO WS-H(13),WS-H(24).

               COMPUTE WKS-AUX = WS-H(5)+WS-H(6)+WS-H(7).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-H(4).
               MOVE WS-H(4) TO WS-H(15),WS-H(26).

               COMPUTE WKS-AUX = WS-H(9)+WS-H(10)+WS-H(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-H(8).
               MOVE WS-H(8) TO WS-H(19),WS-H(30).

               DISPLAY WKS-hamming.

           CORREGIR-ERROR.
               DISPLAY "introduce el codigo hamming a corregir: ".
               ACCEPT WKS-hamming.
               MOVE WKS-hamming TO WKS-CODIGO-COPIA.

               PERFORM COMPARAR-COLUMNAS THRU COMPROBAR-PARIDADES.
               DISPLAY "HABIA ERRORES EN LOS INDICES: ".
               PERFORM VARYING WKS-CONTADOR-COLUMNAS FROM 1 BY 1 UNTIL
               WKS-CONTADOR-COLUMNAS=34
                   IF WS-H(WKS-CONTADOR-COLUMNAS)<>
                       WS-CC(WKS-CONTADOR-COLUMNAS) THEN
                        DISPLAY WKS-CONTADOR-COLUMNAS,","
                        WITH NO ADVANCING
                   END-IF
               END-PERFORM.
               DISPLAY " ".
               DISPLAY "EL CODIGO CORREGIDO ES: "WKS-CODIGO-COPIA.

           COMPARAR-COLUMNAS.
      *    COMPARANDO ENTRE COMPIAS
               COMPUTE WKS-CONTADOR-COLUMNAS = 1
               PERFORM VARYING WKS-CONTADOR-COLUMNAS FROM 1 BY 1 UNTIL
               WKS-CONTADOR-COLUMNAS=12

               COMPUTE WKS-BITCOPIA1 = WKS-CONTADOR-COLUMNAS + 11
               COMPUTE WKS-BITCOPIA2 = WKS-CONTADOR-COLUMNAS + 22

               IF WS-CC(WKS-CONTADOR-COLUMNAS)=
                   WS-CC(WKS-BITCOPIA1) AND WS-CC(WKS-CONTADOR-COLUMNAS)
                   =WS-CC(WKS-BITCOPIA2) THEN
               ELSE
                   IF WS-CC(WKS-CONTADOR-COLUMNAS)=
                   WS-CC(WKS-BITCOPIA1) AND WS-CC(WKS-CONTADOR-COLUMNAS)
                   <> WS-CC(WKS-BITCOPIA2) THEN
                       MOVE WS-CC(WKS-CONTADOR-COLUMNAS) TO
                       WS-CC(WKS-BITCOPIA2)
                   END-IF
                   IF WS-CC(WKS-CONTADOR-COLUMNAS)<>
                   WS-CC(WKS-BITCOPIA1) AND WS-CC(WKS-CONTADOR-COLUMNAS)
                   =WS-CC(WKS-BITCOPIA2) THEN
                       MOVE WS-CC(WKS-CONTADOR-COLUMNAS) TO
                       WS-CC(WKS-BITCOPIA1)
                   END-IF
                   IF WS-CC(WKS-CONTADOR-COLUMNAS)<>
                   WS-CC(WKS-BITCOPIA1) AND WS-CC(WKS-CONTADOR-COLUMNAS)
                   <>WS-CC(WKS-BITCOPIA2) THEN
                       MOVE WS-CC(WKS-BITCOPIA1) TO
                       WS-CC(WKS-CONTADOR-COLUMNAS)
                   END-IF

               END-IF
               END-PERFORM.

           COMPROBAR-PARIDADES.
      *PRIMERA COPIA

      *Comprobando paridad 1
               COMPUTE WKS-AUX =WS-CC(1)+WS-CC(3)+WS-CC(5)+WS-CC(7)
               +WS-CC(9)+WS-CC(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P1(1).

      *Comprobando paridad 2
               COMPUTE WKS-AUX =WS-CC(2)+WS-CC(3)+WS-CC(6)+WS-CC(7)
               +WS-CC(10)+WS-CC(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P1(2).

      *Comprobando paridad 4
               COMPUTE WKS-AUX =WS-CC(4)+WS-CC(5)+WS-CC(6)+WS-CC(7).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P1(3).

      *Comprobando paridad 8
               COMPUTE WKS-AUX =WS-CC(8)+ WS-CC(9)+WS-CC(10)+WS-CC(11).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P1(4).
      *Buscando el indice del error
               IF WS-P1(1)=1
                   ADD 1 to WKS-posBitError
               END-IF.
               IF WS-P1(2)=1
                   ADD 2 to WKS-posBitError
               END-IF.
               IF WS-P1(3)=1
                   ADD 4 to WKS-posBitError
               END-IF.
               IF WS-P1(4)=1
                   ADD 8 to WKS-posBitError
               END-IF.

               IF WKS-posBitError >0 AND WKS-posBitError < 11 THEN
                   IF WS-CC(WKS-posBitError)=0 THEN
                       MOVE 1 TO WS-CC(WKS-posBitError)
                   ELSE
                       MOVE 0 TO WS-CC(WKS-posBitError)
                   END-IF
               END-IF.


      *SEGUNDA COPIA
               COMPUTE WKS-posBitError = 0.

      *Comprobando paridad 1
               COMPUTE WKS-AUX =WS-CC(12)+WS-CC(14)+WS-CC(16)+WS-CC(18)
               +WS-CC(20)+WS-CC(22).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P2(1).

      *Comprobando paridad 2
               COMPUTE WKS-AUX =WS-CC(13)+WS-CC(14)+WS-CC(17)+WS-CC(18)
               +WS-CC(21)+WS-CC(22).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P2(2).

      *Comprobando paridad 4
               COMPUTE WKS-AUX =WS-CC(15)+WS-CC(16)+WS-CC(17)+WS-CC(18).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P2(3).

      *Comprobando paridad 8
               COMPUTE WKS-AUX =WS-CC(19)+WS-CC(20)+WS-CC(21)+WS-CC(22).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P2(4).
      *Buscando el indice del error
               IF WS-P2(1)=1
                   ADD 1 to WKS-posBitError
               END-IF.
               IF WS-P2(2)=1
                   ADD 2 to WKS-posBitError
               END-IF.
               IF WS-P2(3)=1
                   ADD 4 to WKS-posBitError
               END-IF.
               IF WS-P2(4)=1
                   ADD 8 to WKS-posBitError
               END-IF.
               IF WKS-posBitError > 0 AND WKS-posBitError < 22 THEN
                   ADD 11 TO WKS-posBitError

                   IF WS-CC(WKS-posBitError)=0 THEN
                       MOVE 1 TO WS-CC(WKS-posBitError)
                   ELSE
                       MOVE 0 TO WS-CC(WKS-posBitError)
                   END-IF
               END-IF.

      *TERCERA COPIA
               COMPUTE WKS-posBitError = 0.

      *Comprobando paridad 1
               COMPUTE WKS-AUX =WS-CC(23)+WS-CC(25)+WS-CC(27)+WS-CC(29)
               +WS-CC(31)+WS-CC(33).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P3(1).

      *Comprobando paridad 2
               COMPUTE WKS-AUX =WS-CC(24)+WS-CC(25)+WS-CC(28)+WS-CC(29)
               +WS-CC(32)+WS-CC(33).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P3(2).

      *Comprobando paridad 4
               COMPUTE WKS-AUX =WS-CC(26)+WS-CC(27)+WS-CC(28)+WS-CC(29).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P3(3).

      *Comprobando paridad 8
               COMPUTE WKS-AUX =WS-CC(30)+WS-CC(31)+WS-CC(32)+WS-CC(33).
               DIVIDE 2 INTO WKS-AUX GIVING WKS-BASURA
               REMAINDER WS-P3(4).
      *Buscando el indice del error
               IF WS-P3(1)=1
                   ADD 1 to WKS-posBitError
               END-IF.
               IF WS-P3(2)=1
                   ADD 2 to WKS-posBitError
               END-IF.
               IF WS-P3(3)=1
                   ADD 4 to WKS-posBitError
               END-IF.
               IF WS-P3(4)=1
                   ADD 8 to WKS-posBitError
               END-IF.
               IF WKS-posBitError > 0 AND WKS-posBitError < 33 THEN
                   ADD 22 TO WKS-posBitError

                   IF WS-CC(WKS-posBitError)=0 THEN
                       MOVE 1 TO WS-CC(WKS-posBitError)
                   ELSE
                       MOVE 0 TO WS-CC(WKS-posBitError)
                   END-IF
               END-IF.
