      OPEN "listado.cvr" FOR OUTPUT AS #1

pepe:
      FOR ind = 0 TO 511

         PRINT #1, "JP9  JP8  JP7  JP6  JP5  JP4  JP3  JP2  JP1"

         DIM a AS INTEGER
         DIM b AS INTEGER

         a = &H100
         FOR n = 1 TO 9
            b = ind AND a
            IF b > 0 THEN
               PRINT #1, "X    ";
            ELSE
               PRINT #1, "-    ";
            END IF
            a = a / 2
         NEXT n

         PRINT #1, "          "; ind + 1
         PRINT #1, ""
   NEXT ind

   PRINT #1, ""
   PRINT #1, ""
   PRINT #1, "NOTA:"
   PRINT #1, "X = +5V"
   PRINT #1, "- = GND"
   CLOSE #1


         

            

