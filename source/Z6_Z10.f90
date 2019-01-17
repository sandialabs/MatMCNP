!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z6_Z10
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 6-10 (carbon,nitrogen,oxygen,fluorine,and
     !neon) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 6, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
  
     ! Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Carbon Isotopes
     c_size = 2

     carbon_array(1)%z = 6
     carbon_array(1)%a = 12
     carbon_array(1)%symbol =" C-12 "
     carbon_array(1)%mass_defect = 0.0000
     carbon_array(1)%atom_percent = 98.93
     carbon_array(1)%zaid = "06000.80c"

     carbon_array(2)%z = 6
     carbon_array(2)%a = 13
     carbon_array(2)%symbol =" C-13 "
     carbon_array(2)%mass_defect = 3.1250
     carbon_array(2)%atom_percent = 1.07
     carbon_array(2)%zaid = "06000.80c"

     !Calculate isotopic mass for Carbon
     DO i=1, c_size
          holder = carbon_array(i)%a + (carbon_array(i)%mass_defect/931.494)

          carbon_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     c_mass = 0.0
     holder = 0.0

     !Calculate mass of Carbon
     DO i=1, c_size
          c_mass = holder + (carbon_array(i)%isotopic_mass*carbon_array(i)%atom_percent)/100

          holder = c_mass
     END DO
      
     !Calculate weight percent of Carbon
     DO i=1, c_size
          carbon_array(i)%weight_percent = (carbon_array(i)%isotopic_mass &
          &   * carbon_array(i)%atom_percent)/c_mass
     END DO

     !Number of Nitrogen Isotopes
     n_size = 2

     nitrogen_array(1)%z = 7
     nitrogen_array(1)%a = 14
     nitrogen_array(1)%symbol =" N-14 "
     nitrogen_array(1)%mass_defect = 2.8634
     nitrogen_array(1)%atom_percent = 99.636
     nitrogen_array(1)%zaid = "07014.80c"

     nitrogen_array(2)%z = 7
     nitrogen_array(2)%a = 15
     nitrogen_array(2)%symbol =" N-15 "
     nitrogen_array(2)%mass_defect = 0.1014
     nitrogen_array(2)%atom_percent = 0.364
     nitrogen_array(2)%zaid = "07015.80c"

     !Calculate isotopic mass for Nitrogen
     DO i=1, n_size
          holder = nitrogen_array(i)%a + (nitrogen_array(i)%mass_defect/931.494)

          nitrogen_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     n_mass = 0.0
     holder = 0.0

     !Calculate mass of Nitrogen
     DO i=1, n_size
          n_mass = holder + (nitrogen_array(i)%isotopic_mass*nitrogen_array(i)%atom_percent)/100

          holder = n_mass
     END DO
      
     !Calculate weight percent of Nitrogen
     DO i=1, n_size
          nitrogen_array(i)%weight_percent = (nitrogen_array(i)%isotopic_mass &
          &   * nitrogen_array(i)%atom_percent)/n_mass
     END DO

     !Number of Oxygen Isotopes
     o_size = 3

     oxygen_array(1)%z = 8
     oxygen_array(1)%a = 16
     oxygen_array(1)%symbol =" O-16 "
     oxygen_array(1)%mass_defect = -4.7370
     oxygen_array(1)%atom_percent = 99.757
     oxygen_array(1)%zaid = "08016.80c"

     oxygen_array(2)%z = 8
     oxygen_array(2)%a = 17
     oxygen_array(2)%symbol =" O-17 "
     oxygen_array(2)%mass_defect = -0.8087 
     oxygen_array(2)%atom_percent = 0.038
     oxygen_array(2)%zaid = "08017.80c"

     oxygen_array(3)%z = 8
     oxygen_array(3)%a = 18
     oxygen_array(3)%symbol =" O-18 "
     oxygen_array(3)%mass_defect = -0.7828
     oxygen_array(3)%atom_percent = 0.205
     oxygen_array(3)%zaid = "08016.80c"

     !Calculate isotopic mass for Oxygen
     DO i=1, o_size
          holder = oxygen_array(i)%a + (oxygen_array(i)%mass_defect/931.494)

          oxygen_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     o_mass = 0.0
     holder = 0.0

     !Calculate mass of Oxygen
     DO i=1, o_size
          o_mass = holder + (oxygen_array(i)%isotopic_mass*oxygen_array(i)%atom_percent)/100

          holder = o_mass
     END DO
      
     !Calculate weight percent of Oxygen
     DO i=1, o_size
          oxygen_array(i)%weight_percent = (oxygen_array(i)%isotopic_mass &
          &   * oxygen_array(i)%atom_percent)/o_mass
     END DO

     !Number of Fluorine Isotopes
     f_size = 1

     fluorine_array(1)%z = 9
     fluorine_array(1)%a = 19
     fluorine_array(1)%symbol =" F-19 "
     fluorine_array(1)%mass_defect = -1.4874
     fluorine_array(1)%atom_percent = 100.0
     fluorine_array(1)%zaid = "09019.80c"

     !Calculate isotopic mass for Fluorine
     DO i=1, f_size
          holder = fluorine_array(i)%a + (fluorine_array(i)%mass_defect/931.494)

          fluorine_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     f_mass = 0.0
     holder = 0.0

     !Calculate mass of Fluorine
     DO i=1, f_size
          f_mass = holder + (fluorine_array(i)%isotopic_mass*	&
          &	fluorine_array(i)%atom_percent)/100

          holder = f_mass
     END DO
      
     !Calculate weight percent of Fluorine
     DO i=1, f_size
          fluorine_array(i)%weight_percent = (fluorine_array(i)%isotopic_mass &
          &   * fluorine_array(i)%atom_percent)/f_mass
     END DO

     !Number of Neon Isotopes
     ne_size = 3

     neon_array(1)%z = 10
     neon_array(1)%a = 20
     neon_array(1)%symbol ="Ne-20 "
     neon_array(1)%mass_defect = -7.0419
     neon_array(1)%atom_percent = 90.48
     neon_array(1)%zaid = "10020.42c"

     neon_array(2)%z = 10
     neon_array(2)%a = 21
     neon_array(2)%symbol ="Ne-21 "
     neon_array(2)%mass_defect = -5.7317
     neon_array(2)%atom_percent = 0.27
     neon_array(2)%zaid = " no zaid "

     neon_array(3)%z = 10
     neon_array(3)%a = 22
     neon_array(3)%symbol ="Ne-22 "
     neon_array(3)%mass_defect = -8.0247
     neon_array(3)%atom_percent = 9.25
     neon_array(3)%zaid = " no zaid "

     !Calculate isotopic mass for Neon
     DO i=1, ne_size
          holder = neon_array(i)%a + (neon_array(i)%mass_defect/931.494)

          neon_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ne_mass = 0.0
     holder = 0.0

     !Calculate mass of Neon
     DO i=1, ne_size
          ne_mass = holder + (neon_array(i)%isotopic_mass*neon_array(i)%atom_percent)/100

          holder = ne_mass
     END DO
      
     !Calculate weight percent of Neon
     DO i=1, ne_size
          neon_array(i)%weight_percent = (neon_array(i)%isotopic_mass &
          &   * neon_array(i)%atom_percent)/ne_mass

     END DO

   END SUBROUTINE Z6_Z10
