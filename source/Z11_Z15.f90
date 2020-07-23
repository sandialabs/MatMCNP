!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z11_Z15
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 11-15 (sodium,magnesium,aluminum,silicon,and
     !phosphorus) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 6, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
     !
     ! - ENDF/B-VIII.0 cross sections at 293.6 K (that is, .00c) made the default 
     !   on 07/22/2020 by K. Russell DePriest.
     !
     ! - Nuclear Wallet Card values verified on 07/22/2020 using 
     !   "Nuclear Wallet Cards database version of 7/10/2019" on nndc.bnl.gov website.
 
     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Sodium Isotopes
     na_size = 1

     sodium_array(1)%z = 11
     sodium_array(1)%a = 23
     sodium_array(1)%symbol ="Na-23 "
     sodium_array(1)%mass_defect = -9.5298
     sodium_array(1)%atom_percent = 100.0
     sodium_array(1)%zaid = "11023.00c"

     !Calculate isotopic mass for Sodium
     DO i=1, na_size
          holder = sodium_array(i)%a + (sodium_array(i)%mass_defect/MeV_amu)

          sodium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     na_mass = 0.0
     holder = 0.0

     !Calculate mass of Sodium
     DO i=1, na_size
          na_mass = holder + (sodium_array(i)%isotopic_mass*     &
          &     sodium_array(i)%atom_percent)/100

          holder = na_mass
     END DO
     
     !Calculate weight percent of Sodium
     DO i=1, na_size
          sodium_array(i)%weight_percent = (sodium_array(i)%isotopic_mass &
          &   * sodium_array(i)%atom_percent)/na_mass
     END DO

     !Number of Magnesium Isotopes
     mg_size = 3

     magnesium_array(1)%z = 12
     magnesium_array(1)%a = 24
     magnesium_array(1)%symbol ="Mg-24 "
     magnesium_array(1)%mass_defect = -13.9335
     magnesium_array(1)%atom_percent = 78.99
     magnesium_array(1)%zaid = "12024.00c"

     magnesium_array(2)%z = 12
     magnesium_array(2)%a = 25
     magnesium_array(2)%symbol ="Mg-25 "
     magnesium_array(2)%mass_defect = -13.1927
     magnesium_array(2)%atom_percent = 10.00
     magnesium_array(2)%zaid = "12025.00c"

     magnesium_array(3)%z = 12
     magnesium_array(3)%a = 26
     magnesium_array(3)%symbol ="Mg-26 "
     magnesium_array(3)%mass_defect = -16.2145
     magnesium_array(3)%atom_percent = 11.01
     magnesium_array(3)%zaid = "12026.00c"

     !Calculate isotopic mass for Magnesium
     DO i=1,mg_size
          holder = magnesium_array(i)%a + (magnesium_array(i)%mass_defect/MeV_amu)

          magnesium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     mg_mass = 0.0
     holder = 0.0

     !Calculate mass of Magnesium
     DO i=1, mg_size
          mg_mass = holder + (magnesium_array(i)%isotopic_mass*         &
          &     magnesium_array(i)%atom_percent)/100

          holder = mg_mass
     END DO
     
     !Calculate weight percent of Magnesium
     DO i=1, mg_size
          magnesium_array(i)%weight_percent = (magnesium_array(i)%isotopic_mass &
          &   * magnesium_array(i)%atom_percent)/mg_mass
     END DO

     !Number of Aluminum Isotopes
     al_size = 1

     aluminum_array(1)%z = 13
     aluminum_array(1)%a = 27
     aluminum_array(1)%symbol ="Al-27 "
     aluminum_array(1)%mass_defect = -17.1968
     aluminum_array(1)%atom_percent = 100.0
     aluminum_array(1)%zaid = "13027.00c"

     !Calculate isotopic mass for Aluminum
     DO i=1, al_size
          holder = aluminum_array(i)%a + (aluminum_array(i)%mass_defect/MeV_amu)

          aluminum_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     al_mass = 0.0
     holder = 0.0

     !Calculate mass of Aluminum
     DO i=1, al_size
          al_mass = holder + (aluminum_array(i)%isotopic_mass*     &
          &     aluminum_array(i)%atom_percent)/100

          holder = al_mass
     END DO
     
     !Calculate weight percent of Aluminum
     DO i=1, al_size
          aluminum_array(i)%weight_percent = (aluminum_array(i)%isotopic_mass &
          &   * aluminum_array(i)%atom_percent)/al_mass
     END DO

     !Number of Silicon Isotopes
     si_size = 3

     silicon_array(1)%z = 14
     silicon_array(1)%a = 28
     silicon_array(1)%symbol ="Si-28 "
     silicon_array(1)%mass_defect = -21.4927
     silicon_array(1)%atom_percent = 92.223
     silicon_array(1)%zaid = "14028.00c"

     silicon_array(2)%z = 14
     silicon_array(2)%a = 29
     silicon_array(2)%symbol ="Si-29 "
     silicon_array(2)%mass_defect = -21.8950
     silicon_array(2)%atom_percent = 4.685
     silicon_array(2)%zaid = "14029.00c"

     silicon_array(3)%z = 14
     silicon_array(3)%a = 30
     silicon_array(3)%symbol ="Si-30 "
     silicon_array(3)%mass_defect = -24.4329
     silicon_array(3)%atom_percent = 3.092
     silicon_array(3)%zaid = "14030.00c"

     !Calculate isotopic mass for Silicon
     DO i=1,si_size
          holder = silicon_array(i)%a + (silicon_array(i)%mass_defect/MeV_amu)

          silicon_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     si_mass = 0.0
     holder = 0.0

     !Calculate mass of Silicon
     DO i=1, si_size
          si_mass = holder + (silicon_array(i)%isotopic_mass*         &
          &     silicon_array(i)%atom_percent)/100

          holder = si_mass
     END DO
     
     !Calculate weight percent of Silicon
     DO i=1, si_size
          silicon_array(i)%weight_percent = (silicon_array(i)%isotopic_mass &
          &   * silicon_array(i)%atom_percent)/si_mass
     END DO

     !Number of Phosphorus Isotopes
     p_size = 1

     phosphorus_array(1)%z = 15
     phosphorus_array(1)%a = 31
     phosphorus_array(1)%symbol =" P-31 "
     phosphorus_array(1)%mass_defect = -24.4405
     phosphorus_array(1)%atom_percent = 100.0
     phosphorus_array(1)%zaid = "15031.00c"

     !Calculate isotopic mass for Phosphorus
     DO i=1, p_size
          holder = phosphorus_array(i)%a + (phosphorus_array(i)%mass_defect/MeV_amu)

          phosphorus_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     p_mass = 0.0
     holder = 0.0

     !Calculate mass of Phosphorus
     DO i=1, p_size
          p_mass = holder + (phosphorus_array(i)%isotopic_mass*     &
          &     phosphorus_array(i)%atom_percent)/100

          holder = p_mass
     END DO

     !Calculate weight percent of Phosphorus
     DO i=1, p_size
          phosphorus_array(i)%weight_percent = (phosphorus_array(i)%isotopic_mass &
          &   * phosphorus_array(i)%atom_percent)/p_mass
     END DO

   END SUBROUTINE Z11_Z15
