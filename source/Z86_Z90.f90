!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z86_Z90
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE


     !This subroutine contains elements 86-90 (radon, francium, radium,
     !actinium, and thorium) and their naturally occuring isotopes. Radon,
     !francium,radium,and actinium do not occur naturally in nature and 
     !are therefore not included in the subroutine.
     !
     !Created with the Current Nuclear Wallet Cards (2004)
     !on June 2, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 6, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Thorium Isotopes
     th_size = 1

     thorium_array(1)%z = 90
     thorium_array(1)%a = 232
     thorium_array(1)%symbol ="Th-232"
     thorium_array(1)%mass_defect = 35.4526
     thorium_array(1)%atom_percent = 100.0
     thorium_array(1)%zaid = "90232.80c"

     !Calculate isotopic mass for Thorium
     DO i=1, th_size
          holder = thorium_array(i)%a + (thorium_array(i)%mass_defect/931.494)

          thorium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     th_mass = 0.0
     holder = 0.0

     !Calculate mass of Thorium
     DO i=1, th_size
          th_mass = holder + (thorium_array(i)%isotopic_mass* &
          &  thorium_array(i)%atom_percent)/100

          holder = th_mass
     END DO
      
     !Calculate weight percent of Thorium
     DO i=1, th_size
          thorium_array(i)%weight_percent = (thorium_array(i)%isotopic_mass &
          &   * thorium_array(i)%atom_percent)/th_mass
     END DO 

  END SUBROUTINE Z86_Z90
