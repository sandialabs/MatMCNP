!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z91_Z92
    USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE


     !This subroutine contains elements 91-92 (protactinium and uranium)
     !and their naturally occuring isotopes. Protactinium does not 
     !occur naturally in nature and is therefore not included in 
     !the subroutine.
     !
     !Created with the Current Nuclear Wallet Cards (2004)
     !on June 2, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 15, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Uranium Isotopes
     u_size = 3

     uranium_array(1)%z = 92
     uranium_array(1)%a = 234
     uranium_array(1)%symbol ="U-234 "
     uranium_array(1)%mass_defect = 38.1480
     uranium_array(1)%atom_percent = 0.0054
     uranium_array(1)%zaid = "92234.80c"

     uranium_array(2)%z = 92
     uranium_array(2)%a = 235
     uranium_array(2)%symbol ="U-235 "
     uranium_array(2)%mass_defect = 40.9218
     uranium_array(2)%atom_percent = 0.7204
     uranium_array(2)%zaid = "92235.80c"

     uranium_array(3)%z = 92
     uranium_array(3)%a = 238
     uranium_array(3)%symbol ="U-238 "
     uranium_array(3)%mass_defect = 47.3100
     uranium_array(3)%atom_percent = 99.2742
     uranium_array(3)%zaid = "92238.80c"

     !Calculate isotopic mass for Uranium
     DO i=1, u_size
          holder = uranium_array(i)%a + (uranium_array(i)%mass_defect/931.494)

          uranium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     u_mass = 0.0
     holder = 0.0

     !Calculate mass of Uranium
     DO i=1, u_size
          u_mass = holder + (uranium_array(i)%isotopic_mass* &
          &  uranium_array(i)%atom_percent)/100

          holder = u_mass
     END DO
     
     !Calculate weight percent of Uranium
     DO i=1, u_size
          uranium_array(i)%weight_percent = (uranium_array(i)%isotopic_mass &
          &   * uranium_array(i)%atom_percent)/u_mass
     END DO 

   END SUBROUTINE Z91_Z92
