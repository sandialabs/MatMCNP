!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE title_comment
     IMPLICIT NONE

     !This subroutine created on June 10,2004, by Karen Kajder. It reads in 
     !the compound's title and comment cards and prints them to the output deck.
     !
     !Modifications to NWC Data by K. Russell DePriest (July 2020), so the
     ! version number is incremented.

     !Variables.
     CHARACTER(LEN=65):: title
     CHARACTER(LEN=72)::comment_cards
     INTEGER:: comment_number, i

     !Read from the input file the title and the number of comment cards.
     READ (UNIT=10,FMT=110) title, comment_number
      
     !Output to output file.
     WRITE (UNIT=11,FMT=111) title
     
     !Loop to get each comment card and display to output file.
     DO i=1, comment_number
        READ (UNIT=10,FMT=210) comment_cards
        WRITE (UNIT=11,FMT=222) comment_cards
     END DO

     !output titles of matmcnp calculations
     WRITE (UNIT=11,FMT=333)

     !Format statements
     110 FORMAT(A65/I2)
     111 FORMAT("C"/"C",2X,A65/"C")
     210 FORMAT(A72)
     222 FORMAT("C",2X,A72)
     333 FORMAT("C"/"C",3X,"Summary of MatMCNP (Version 4.1) Calculations:"/"C"/"C",2X,"Isotope",2X, &
                "Number Fraction",4X,"Weight Fraction",7X,"Atoms/b-cm")

   END SUBROUTINE title_comment
