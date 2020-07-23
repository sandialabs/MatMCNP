!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z41_Z45
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 41-45 (niobium, molybdenum, technetium,
     !ruthenium, and rhodium) and their naturally occuring isotopes. 
     !Technetium does not naturally occur in nature and is therefore 
     !left out of the subroutine. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 26, 2004 by Karen Kajder.
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

     !Number of Niobium Isotopes
     nb_size = 1

     niobium_array(1)%z = 41
     niobium_array(1)%a = 93
     niobium_array(1)%symbol ="Nb-93 "
     niobium_array(1)%mass_defect = -87.2128
     niobium_array(1)%atom_percent = 100.0
     niobium_array(1)%zaid = "41093.00c"

     !Calculate isotopic mass for Niobium
     DO i=1, nb_size
          holder = niobium_array(i)%a + (niobium_array(i)%mass_defect/MeV_amu)

          niobium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     nb_mass = 0.0
     holder = 0.0

     !Calculate mass of Niobium
     DO i=1, nb_size
          nb_mass = holder + (niobium_array(i)%isotopic_mass* &
          & niobium_array(i)%atom_percent)/100

          holder = nb_mass
     END DO
      
     !Calculate weight percent of Niobium
     DO i=1, nb_size
          niobium_array(i)%weight_percent = (niobium_array(i)%isotopic_mass &
          &   * niobium_array(i)%atom_percent)/nb_mass
     END DO

     !Number of Molybdenum Isotopes
     mo_size = 7

     molybdenum_array(1)%z = 42
     molybdenum_array(1)%a = 92
     molybdenum_array(1)%symbol ="Mo-92 "
     molybdenum_array(1)%mass_defect = -86.8085
     molybdenum_array(1)%atom_percent = 14.53
     molybdenum_array(1)%zaid = "42092.00c"

     molybdenum_array(2)%z = 42
     molybdenum_array(2)%a = 94
     molybdenum_array(2)%symbol ="Mo-94 "
     molybdenum_array(2)%mass_defect = -88.4140
     molybdenum_array(2)%atom_percent = 9.15
     molybdenum_array(2)%zaid = "42094.00c"

     molybdenum_array(3)%z = 42
     molybdenum_array(3)%a = 95
     molybdenum_array(3)%symbol ="Mo-95 "
     molybdenum_array(3)%mass_defect = -87.7118
     molybdenum_array(3)%atom_percent = 15.84
     molybdenum_array(3)%zaid = "42095.00c"

     molybdenum_array(4)%z = 42
     molybdenum_array(4)%a = 96
     molybdenum_array(4)%symbol ="Mo-96 "
     molybdenum_array(4)%mass_defect = -88.7948
     molybdenum_array(4)%atom_percent = 16.67
     molybdenum_array(4)%zaid = "42096.00c"

     molybdenum_array(5)%z = 42
     molybdenum_array(5)%a = 97
     molybdenum_array(5)%symbol ="Mo-97 "
     molybdenum_array(5)%mass_defect = -87.5446
     molybdenum_array(5)%atom_percent = 9.60
     molybdenum_array(5)%zaid = "42097.00c"

     molybdenum_array(6)%z = 42
     molybdenum_array(6)%a = 98
     molybdenum_array(6)%symbol ="Mo-98 "
     molybdenum_array(6)%mass_defect = -88.1159
     molybdenum_array(6)%atom_percent = 24.39
     molybdenum_array(6)%zaid = "42098.00c"

     molybdenum_array(7)%z = 42
     molybdenum_array(7)%a = 100
     molybdenum_array(7)%symbol ="Mo-100"
     molybdenum_array(7)%mass_defect = -86.1930
     molybdenum_array(7)%atom_percent = 9.82
     molybdenum_array(7)%zaid = "42100.00c"

     !Calculate isotopic mass for Molybdenum
     DO i=1,mo_size
          holder = molybdenum_array(i)%a + (molybdenum_array(i)%mass_defect/MeV_amu)

          molybdenum_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     mo_mass = 0.0
     holder = 0.0

     !Calculate mass of Molybdenum
     DO i=1, mo_size
          mo_mass = holder + (molybdenum_array(i)%isotopic_mass* &
          & molybdenum_array(i)%atom_percent)/100

          holder = mo_mass
     END DO
      
     !Calculate weight percent of Molybdenum
     DO i=1, mo_size
          molybdenum_array(i)%weight_percent = (molybdenum_array(i)%isotopic_mass &
          &   * molybdenum_array(i)%atom_percent)/mo_mass
     END DO

     !Number of Ruthenium Isotopes
     ru_size = 7

     ruthenium_array(1)%z = 44
     ruthenium_array(1)%a = 96
     ruthenium_array(1)%symbol ="Ru-96 "
     ruthenium_array(1)%mass_defect = -86.0803
     ruthenium_array(1)%atom_percent = 5.54
     ruthenium_array(1)%zaid = "44096.00c"

     ruthenium_array(2)%z = 44
     ruthenium_array(2)%a = 98
     ruthenium_array(2)%symbol ="Ru-98 "
     ruthenium_array(2)%mass_defect = -88.2248
     ruthenium_array(2)%atom_percent = 1.87
     ruthenium_array(2)%zaid = "44098.00c"

     ruthenium_array(3)%z = 44
     ruthenium_array(3)%a = 99
     ruthenium_array(3)%symbol ="Ru-99 "
     ruthenium_array(3)%mass_defect = -87.6253
     ruthenium_array(3)%atom_percent = 12.76
     ruthenium_array(3)%zaid = "44099.00c"

     ruthenium_array(4)%z = 44
     ruthenium_array(4)%a = 100
     ruthenium_array(4)%symbol ="Ru-100"
     ruthenium_array(4)%mass_defect = -89.2273
     ruthenium_array(4)%atom_percent = 12.60
     ruthenium_array(4)%zaid = "44100.00c"

     ruthenium_array(5)%z = 44
     ruthenium_array(5)%a = 101
     ruthenium_array(5)%symbol ="Ru-101"
     ruthenium_array(5)%mass_defect = -87.9581
     ruthenium_array(5)%atom_percent = 17.06
     ruthenium_array(5)%zaid = "44101.00c"

     ruthenium_array(6)%z = 44
     ruthenium_array(6)%a = 102
     ruthenium_array(6)%symbol ="Ru-102"
     ruthenium_array(6)%mass_defect = -89.1064
     ruthenium_array(6)%atom_percent = 31.55
     ruthenium_array(6)%zaid = "44102.00c"

     ruthenium_array(7)%z = 44
     ruthenium_array(7)%a = 104
     ruthenium_array(7)%symbol ="Ru-104"
     ruthenium_array(7)%mass_defect = -88.0957
     ruthenium_array(7)%atom_percent = 18.62
     ruthenium_array(7)%zaid = "44104.00c"

     !Calculate isotopic mass for Ruthenium
     DO i=1,ru_size
          holder = ruthenium_array(i)%a + (ruthenium_array(i)%mass_defect/MeV_amu)

          ruthenium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ru_mass = 0.0
     holder = 0.0

     !Calculate mass of Ruthenium
     DO i=1, ru_size
          ru_mass = holder + (ruthenium_array(i)%isotopic_mass* &
          &  ruthenium_array(i)%atom_percent)/100

          holder = ru_mass
     END DO
      
     !Calculate weight percent of Ruthenium
     DO i=1, ru_size
          ruthenium_array(i)%weight_percent = (ruthenium_array(i)%isotopic_mass &
          &   * ruthenium_array(i)%atom_percent)/ru_mass
     END DO

     !Number of Rhodium Isotopes
     rh_size = 1

     rhodium_array(1)%z = 45
     rhodium_array(1)%a = 103
     rhodium_array(1)%symbol ="Rh-103"
     rhodium_array(1)%mass_defect = -88.0317
     rhodium_array(1)%atom_percent = 100.0
     rhodium_array(1)%zaid = "45103.00c"

     !Calculate isotopic mass for Rhodium
     DO i=1, rh_size
          holder = rhodium_array(i)%a + (rhodium_array(i)%mass_defect/MeV_amu)

          rhodium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     rh_mass = 0.0
     holder = 0.0

     !Calculate mass of Rhodium
     DO i=1, rh_size
          rh_mass = holder + (rhodium_array(i)%isotopic_mass* &
          & rhodium_array(i)%atom_percent)/100

          holder = rh_mass
     END DO
      
     !Calculate weight percent of Rhodium
     DO i=1, rh_size
          rhodium_array(i)%weight_percent = (rhodium_array(i)%isotopic_mass &
          &   * rhodium_array(i)%atom_percent)/rh_mass
     END DO

   END SUBROUTINE Z41_Z45
