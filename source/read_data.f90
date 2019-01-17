!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE read_data(compound_density,atom_or_weight,number_elements, z_of_element, &
                        natural_or_enriched,a_or_w_percent,mat_number)
     USE NWC_2000
     USE NWC_DATABASE
     IMPLICIT NONE

     !Subroutine to read in the rest of the data from the input deck. 
     !Created on June 21,2004, by Karen Kajder.
     !
     !This subroutine needs to be modified to allow more freedom of 
     !input formats.  Probably some intrinsic string functions and
     !logic tests would allow MatMCNP to move to free format reads.

     !Dummy Variables     
     REAL,INTENT(OUT):: compound_density
     REAL, DIMENSION(92),INTENT(OUT):: a_or_w_percent
     CHARACTER(LEN=6),INTENT(OUT)::atom_or_weight
     CHARACTER(LEN=5),INTENT(OUT)::mat_number
     CHARACTER(LEN=3), DIMENSION(92),INTENT(OUT)::natural_or_enriched
     INTEGER,INTENT(OUT):: number_elements
     INTEGER, DIMENSION(92),INTENT(OUT)::z_of_element

     !Local variables
     INTEGER:: i
     REAL:: holder

     !Initialize
     holder = 0.0

     !Read in density, whether using weight percents or atom percents, 
     !and the number of elements in the compound from input file.
     READ (UNIT=10,FMT=1) compound_density
     READ (UNIT=10,FMT=2) atom_or_weight
     READ (UNIT=10,FMT=3) number_elements

     !Allow only 92 element entries
     IF (number_elements > 92) THEN
        !Print error message for two many entries
        WRITE (UNIT=11, FMT=7)
        STOP
     END IF
     
     !Loop to read in whether each element is natural or enriched, the z of each
     !element and the atomic or weight percent.
     DO i=1, number_elements
        READ (10,*) natural_or_enriched(i), z_of_element(i),a_or_w_percent(i)
        holder = holder + a_or_w_percent(i)
        !
        !Check for Z > 92
        IF (z_of_element(i) .GT. 92) THEN
           !Print error message if Z is greater than 92. 
            WRITE (UNIT=11, FMT=6) 
            STOP
        END IF
        !
        IF (natural_or_enriched(i) == 'nat') THEN
            CONTINUE
        ELSE IF (natural_or_enriched(i) == 'enr') THEN
            CALL enriched(natural_or_enriched,z_of_element,number_elements)
        ELSE
            !Print error message if user does not specify whether element is natural or enriched. 
            WRITE (UNIT=11, FMT=5) 
            STOP
        END IF
     END DO

     !Read in material number.
     READ (UNIT=10,FMT=4) mat_number

     !Normalize atom or weight percent to 1.0 (or 100%)
     a_or_w_percent = (a_or_w_percent/holder)
     
     !Format Statements
     1 FORMAT(F20.3)
     2 FORMAT(A6)
     3 FORMAT(I2)
     4 FORMAT(A5)
     5 FORMAT(/"Error: Need to specify whether the element(s) is natural or enriched &
              &(nat or enr)."/" Make sure it is not capitalized. ")
     6 FORMAT(/"Error: Elements with Z > 92 are not implemented in MatMCNP.")
     7 FORMAT(/,"Error: MatMCNP limits the number of element entries to 92 or less.")

   END SUBROUTINE
