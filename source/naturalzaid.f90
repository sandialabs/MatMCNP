!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE naturalzaid(adj_num_iso)
     USE NWC_2000
     USE NWC_DATABASE 
     IMPLICIT NONE

     !This subroutine takes elements that use a natural zaid and adds 
     !together each of their isotopes atom fractions. It then puts this
     !combined atom fraction in the output file with the natural zaid for
     !the element. Created on July 22,2004 by Karen Kajder and Russell 
     !DePriest.
     !
     !Modified by K. Russell DePriest on December 19, 2011 to account for
     ! new xsec data available in mcnp format.
     ! 
     ! - Additional changes with the release of ENDF/B-VII release 1
     !    made on 05/09/2014

     ! Dummy Variables
     INTEGER, INTENT (OUT):: adj_num_iso


     !Local Variables
     INTEGER:: i,k,shift

     !Initialize
     adj_num_iso = num_iso

     !Loop through the number of isotopes.
     DO i=1, num_iso
        !If the element uses a natural zaid,
        IF (output_array(i)%z == 6) THEN
           !
           ! then add together its atom fractions.
           !
           output_array(i)%atom_percent = output_array(i)%atom_percent + output_array(i+1)%atom_percent
           !
           !Subtract the isotope space no longer being used since combined into one.
           !
           adj_num_iso = num_iso - 1
           shift = i + 1
           !
           !Shift the other element's isotopes up in the list since elements with natural zaid only requiring one spot.
           !
           DO k=shift, num_iso
              output_array(k) = output_array(k+1)
           END DO
           !
           !Adjust the total number of isotopes to the reduced number.
           !
           num_iso = adj_num_iso
        ELSE IF (output_array(i)%z == 78) THEN
           output_array(i)%atom_percent = output_array(i)%atom_percent +   output_array(i+1)%atom_percent + &
                                        & output_array(i+2)%atom_percent + output_array(i+3)%atom_percent + &
                                        & output_array(i+4)%atom_percent + output_array(i+5)%atom_percent 
           adj_num_iso = num_iso - 5
           shift = i + 1
           DO k=shift, num_iso
              output_array(k) = output_array(k+5)
           END DO
           num_iso = adj_num_iso
        ELSE
           !
           !Continue if there are no natural elements in compound.
           !
           CONTINUE
        END IF
     END DO

   END SUBROUTINE naturalzaid
