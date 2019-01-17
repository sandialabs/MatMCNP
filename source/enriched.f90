!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE enriched(natural_or_enriched,z_of_element,number_elements)
     USE NWC_2000
     USE NWC_DATABASE
     IMPLICIT NONE

     !Subroutine to deal with enriched elements.
     !At present, only Lithium, Boron, and Uranium can use this option
     !Created on July 2, 2004 by Karen Kajder.
     !
     !Modified on December 21, 2011 by K. Russell DePriest
     !Equations for almost all of the enrichment were incorrectly implemented.
     !These modifications provided correct answers now for both atomic or weight fraction
     !input.  The program assumes that enrichment is WEIGHT % regardless of elemental
     !input option type.

     !Dummy Variables
     CHARACTER(LEN=3), DIMENSION(92),INTENT(IN)::natural_or_enriched
     INTEGER, DIMENSION(92),INTENT(IN)::z_of_element
     INTEGER,INTENT(IN):: number_elements

     !Local Variables
     INTEGER::j,k,z
     REAL,DIMENSION(92):: w_holder
     REAL::holder, frac_holder, inverse_mass
     INTEGER, DIMENSION(92)::isotope
     REAL::li6_atom_dens, li7_atom_dens, li_total_atom_dens
     REAL::b10_atom_dens, b11_atom_dens, b_total_atom_dens
     REAL::u234_atom_dens, u235_atom_dens, u238_atom_dens, u_total_atom_dens

     !Initialize
     holder = 0.0
     frac_holder = 0.0

     DO j=1, number_elements
       z = z_of_element(j)

       SELECT CASE (z)
         !If the z of the element is not lithium, boron, or uranium, continue back to the main program.
         CASE DEFAULT
            CONTINUE
         CASE (3)
            DO k=1, 2
               !Read in isotope number and its corresponding new weight percent.
               READ (UNIT=10,FMT=*) isotope(k), w_holder(k)
               IF (isotope(k) == 6) THEN
                    lithium_array(1)%weight_percent = w_holder(k)
               ELSE IF (isotope(k) == 7) THEN 
                    lithium_array(2)%weight_percent = w_holder(k)
               END IF
            END DO
            !
            !
            !Normalize lithium weight to 100
            frac_holder = lithium_array(1)%weight_percent + lithium_array(2)%weight_percent
            lithium_array(1)%weight_percent = (lithium_array(1)%weight_percent/frac_holder) * 100
            lithium_array(2)%weight_percent = (lithium_array(2)%weight_percent/frac_holder) * 100
            !
            !
            !Calculate new mass of Lithium
            inverse_mass = (lithium_array(1)%weight_percent / lithium_array(1)%isotopic_mass + &
                          & lithium_array(2)%weight_percent / lithium_array(2)%isotopic_mass) / 100
            !
            !
            li_mass = 1.0 / inverse_mass
            !
            !
            !Assume a mass density of 1.0 g/cc and calculate an atom density in order to compute
            ! an atom percent for the enriched material
            !
            ! N(sub-i) = w(sub-i)*rho*Avog#/M(sub-i) where M(sub-i) is the isotopic mass
            !
            li6_atom_dens = lithium_array(1)%weight_percent * 1.0 * 0.6022 / lithium_array(1)%isotopic_mass / 100
            li7_atom_dens = lithium_array(2)%weight_percent * 1.0 * 0.6022 / lithium_array(2)%isotopic_mass / 100
            !
            li_total_atom_dens = li6_atom_dens + li7_atom_dens
            !
            lithium_array(1)%atom_percent = li6_atom_dens / li_total_atom_dens * 100
            lithium_array(2)%atom_percent = li7_atom_dens / li_total_atom_dens * 100

         CASE (5)
            DO k=1, 2
               !Read in isotope number and its corresponding new weight percent.
               READ (UNIT=10,FMT=*) isotope(k), w_holder(k)
               IF (isotope(k) == 10) THEN
                   boron_array(1)%weight_percent = w_holder(k)
               ELSE IF (isotope(k) == 11) THEN 
                   boron_array(2)%weight_percent = w_holder(k)
               END IF
            END DO
            !
            !
            !Normalize boron weight to 100
            frac_holder = boron_array(1)%weight_percent + boron_array(2)%weight_percent
            boron_array(1)%weight_percent = (boron_array(1)%weight_percent/frac_holder) * 100
            boron_array(2)%weight_percent = (boron_array(2)%weight_percent/frac_holder) * 100
            !
            !
            !Calculate new mass of Boron
            inverse_mass = (boron_array(1)%weight_percent / boron_array(1)%isotopic_mass + &
                          & boron_array(2)%weight_percent / boron_array(2)%isotopic_mass) / 100
            !
            !
            b_mass = 1.0 / inverse_mass
            !
            !
            !Assume a mass density of 1.0 g/cc and calculate an atom density in order to compute
            ! an atom percent for the enriched material
            !
            ! N(sub-i) = w(sub-i)*rho*Avog#/M(sub-i) where M(sub-i) is the isotopic mass
            !
            b10_atom_dens = boron_array(1)%weight_percent * 1.0 * 0.6022 / boron_array(1)%isotopic_mass / 100
            b11_atom_dens = boron_array(2)%weight_percent * 1.0 * 0.6022 / boron_array(2)%isotopic_mass / 100
            !
            b_total_atom_dens = b10_atom_dens + b11_atom_dens
            !
            boron_array(1)%atom_percent = b10_atom_dens / b_total_atom_dens * 100
            boron_array(2)%atom_percent = b11_atom_dens / b_total_atom_dens * 100
               
         CASE (92)
            DO k=1,3
               !Read in isotope number and its corresponding new weight percent.
               READ (UNIT=10,FMT=*) isotope(k), w_holder(k)
               IF (isotope(k) == 234) THEN
                  uranium_array(1)%weight_percent = w_holder(k)
               ELSE IF (isotope(k) == 235) THEN 
                  uranium_array(2)%weight_percent = w_holder(k)
               ELSE IF (isotope(k) == 238) THEN 
                  uranium_array(3)%weight_percent = w_holder(k)
               END IF
             END DO
            !
            !
            !Normalize uranium weight to 100
            frac_holder = uranium_array(1)%weight_percent + uranium_array(2)%weight_percent + &
                        & uranium_array(3)%weight_percent
            uranium_array(1)%weight_percent = (uranium_array(1)%weight_percent/frac_holder) * 100
            uranium_array(2)%weight_percent = (uranium_array(2)%weight_percent/frac_holder) * 100
            uranium_array(3)%weight_percent = (uranium_array(3)%weight_percent/frac_holder) * 100
            !
            !
            !Calculate new mass of Uranium
            inverse_mass = (uranium_array(1)%weight_percent / uranium_array(1)%isotopic_mass + &
                          & uranium_array(2)%weight_percent / uranium_array(2)%isotopic_mass + &
                          & uranium_array(3)%weight_percent / uranium_array(3)%isotopic_mass) / 100
            !
            !
            u_mass = 1.0 / inverse_mass
            !
            !
            !Assume a mass density of 1.0 g/cc and calculate an atom density in order to compute
            ! an atom percent for the enriched material
            !
            ! N(sub-i) = w(sub-i)*rho*Avog#/M(sub-i) where M(sub-i) is the isotopic mass
            !
            u234_atom_dens = uranium_array(1)%weight_percent * 1.0 * 0.6022 / uranium_array(1)%isotopic_mass / 100
            u235_atom_dens = uranium_array(2)%weight_percent * 1.0 * 0.6022 / uranium_array(2)%isotopic_mass / 100
            u238_atom_dens = uranium_array(3)%weight_percent * 1.0 * 0.6022 / uranium_array(3)%isotopic_mass / 100
            !
            u_total_atom_dens = u234_atom_dens + u235_atom_dens + u238_atom_dens
            !
            uranium_array(1)%atom_percent = u234_atom_dens / u_total_atom_dens * 100
            uranium_array(2)%atom_percent = u235_atom_dens / u_total_atom_dens * 100
            uranium_array(3)%atom_percent = u238_atom_dens / u_total_atom_dens * 100

          END SELECT
     END DO                              

     !Format Statements
     ! 6 FORMAT(A5,2X,F10.1)

   END SUBROUTINE
