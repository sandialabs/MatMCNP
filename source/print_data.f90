!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE print_data(total_atom_b_cm,iso_atom_b_cm,mat_number,z_of_element)
     USE NWC_2000
     USE NWC_DATABASE
     IMPLICIT NONE

     !This subroutine contains the rest of the format statements for the output file.
     !It outputs the symbol of each isotope, their atom fraction, their weight fraction,
     !their atom density, the total atom density for the mixture, the MCNP material number,
     !and each isotope's zaid.
     !Created on June 24,2004 by Karen Kajder.
     !
     !Modified by K. Russell DePriest on December 19, 2011 to account for new xsec
     ! data available in the mcnp format.
     ! 
     ! - Additional changes with the release of ENDF/B-VII release 1
     !    made on 05/09/2014
     ! - Clean up to improve output formatting
     !

     !Dummy variables
     REAL,DIMENSION(300),INTENT(IN):: iso_atom_b_cm
     REAL,INTENT(IN)::total_atom_b_cm
     CHARACTER(LEN=5)::mat_number
     INTEGER, DIMENSION(92)::z_of_element

     !Local variables
     INTEGER::i,j, adj_num_iso
     REAL::holder

     !Initialize
     holder = 0.0
     
     !Loop to print calculated info. with each isotope's symbol to the output file.
     DO i=1,num_iso
        WRITE (UNIT=11,FMT=444) output_array(i)%symbol,output_array(i)%atom_percent,&
                                output_array(i)%weight_percent,iso_atom_b_cm(i)
     END DO
     
     !Print the total atoms/b-cm
     WRITE (UNIT=11, FMT=555) total_atom_b_cm

     !Print message for elements that could be modified by an S(alpha,beta).
     DO i=1,num_iso
        IF (z_of_element(i) == 1) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        ELSE IF (z_of_element(i) == 4) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        ELSE IF (z_of_element(i) == 6) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        ELSE IF (z_of_element(i) == 8) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        ELSE IF (z_of_element(i) == 13) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        ELSE IF (z_of_element(i) == 40) THEN
            WRITE (UNIT=11, FMT=39)
            EXIT
        END IF
     END DO

     !Stop running the program if the compound contains elements (or isotopes) that do not have a zaid.
     DO i=1, num_iso
        IF (output_array(i)%z == 10) THEN  !Neon does not have the zaids necessary to create material card
            WRITE (UNIT=11, FMT=38)
            STOP
        ELSE IF (output_array(i)%z == 70) THEN  !Ytterbium does not have the zaids necessary to create material card
            WRITE (UNIT=11, FMT=38)
            STOP
        ELSE IF (output_array(i)%z == 76) THEN  !Osmium does not have the zaids necessary to create material card
            WRITE (UNIT=11, FMT=38)
            STOP
        ELSE
            CONTINUE
        END IF
     END DO

     ! Call subroutine that will adjust the information for elements that use a natural zaid.
     CALL naturalzaid(adj_num_iso)

     !Print info. in MatMCNP format.
     WRITE (UNIT=11,FMT=777) mat_number,output_array(1)%zaid,output_array(1)%atom_percent

     !If there is more than one isotope in the mixture, then print out that information.
     IF (adj_num_iso > 1) THEN
         DO i=2,adj_num_iso
            WRITE (UNIT=11,FMT=888) output_array(i)%zaid,output_array(i)%atom_percent
         END DO
     END IF

     !Print caution messages for the elements that use a natural zaid 
     ! or elements that have a "quirk" like oxygen, carbon, and platinum.
     DO i=1,num_iso
        IF (z_of_element(i) == 8) THEN
            WRITE (UNIT=11, FMT=999)
        ELSE IF (z_of_element(i) == 6) THEN
            WRITE (UNIT=11, FMT=20)
        ELSE IF (z_of_element(i) == 78) THEN
            WRITE (UNIT=11, FMT=34)
        END IF
     END DO

  
     !Format statements
     444 FORMAT ("C",2X,A6,6X,F9.6,10X,F9.6,10X,F9.7)
     555 FORMAT ("C"/"C",2X,"The total compound atom density (atom/b-cm):  ",F9.7,/"C")
     777 FORMAT ("M",A5,2X,A9,2X,F9.6)
     888 FORMAT (8X,A9,2X,F9.6)
     999 FORMAT ("C"/"C",2X,"Caution: The O-18 has been set to O-16.")
      20 FORMAT ("C"/"C",2X,"Caution: The natural zaid is used for Carbon.")
      34 FORMAT ("C"/"C",2X,"Caution: The natural zaid is used for Platinum.")
      38 FORMAT ("C"/"C",2X, "One or more of the elements in the compound does not have a cross-section"/ &
      &                    "C",2X,"and therefore the MCNP Card will not be created.")
      39 FORMAT ("C"/"C",2X,"This material contains an isotope that is often modified by "/"C",2X,"an S(alpha,beta).&
      & Check MCNP Manual Appendix G to see if an"/"C",2X,"S(alpha,beta) card (i.e., an MTn card) is required."/"C")

   END SUBROUTINE print_data
