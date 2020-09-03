!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z26_Z30
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 26-30 (iron,cobalt,nickel,copper,and zinc)
     !and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 13, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
     ! - Zinc changed from elemental to isotopic
     !
     ! - ENDF/B-VIII.0 cross sections at 293.6 K (that is, .00c) made the default 
     !   on 07/22/2020 by K. Russell DePriest.
     !
     ! - Nuclear Wallet Card values verified on 07/22/2020 using 
     !   "Nuclear Wallet Cards database version of 7/10/2019" on nndc.bnl.gov website.

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Iron Isotopes
     fe_size = 4

     iron_array(1)%z = 26
     iron_array(1)%a = 54
     iron_array(1)%symbol ="Fe-54 "
     iron_array(1)%mass_defect = -56.2545
     iron_array(1)%atom_percent = 5.845
     iron_array(1)%zaid = "26054.00c"

     iron_array(2)%z = 26
     iron_array(2)%a = 56
     iron_array(2)%symbol ="Fe-56 "
     iron_array(2)%mass_defect = -60.6070
     iron_array(2)%atom_percent = 91.754
     iron_array(2)%zaid = "26056.00c"

     iron_array(3)%z = 26
     iron_array(3)%a = 57
     iron_array(3)%symbol ="Fe-57 "
     iron_array(3)%mass_defect = -60.1818
     iron_array(3)%atom_percent = 2.119
     iron_array(3)%zaid = "26057.00c"

     iron_array(4)%z = 26
     iron_array(4)%a = 58
     iron_array(4)%symbol ="Fe-58 "
     iron_array(4)%mass_defect = -62.1551
     iron_array(4)%atom_percent = 0.282
     iron_array(4)%zaid = "26058.00c"

     !Calculate isotopic mass for Iron
     DO i=1,fe_size
          holder = iron_array(i)%a + (iron_array(i)%mass_defect/MeV_amu)

          iron_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     fe_mass = 0.0
     holder = 0.0

     !Calculate mass of Iron
     DO i=1, fe_size
          fe_mass = holder + (iron_array(i)%isotopic_mass*  &
          &     iron_array(i)%atom_percent)/100

          holder = fe_mass
     END DO
      
     !Calculate weight percent of Iron
     DO i=1, fe_size
          iron_array(i)%weight_percent = (iron_array(i)%isotopic_mass &
          &   * iron_array(i)%atom_percent)/fe_mass
     END DO

     !Number of Cobalt Isotopes
     co_size = 1

     cobalt_array(1)%z = 27
     cobalt_array(1)%a = 59
     cobalt_array(1)%symbol ="Co-59 "
     cobalt_array(1)%mass_defect = -62.2297
     cobalt_array(1)%atom_percent = 100.0
     cobalt_array(1)%zaid = "27059.00c"

     !Calculate isotopic mass for Cobalt
     DO i=1,co_size
          holder = cobalt_array(i)%a + (cobalt_array(i)%mass_defect/MeV_amu)

          cobalt_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     co_mass = 0.0
     holder = 0.0

     !Calculate mass of Cobalt
     DO i=1, co_size
          co_mass = holder + (cobalt_array(i)%isotopic_mass*          &
          &     cobalt_array(i)%atom_percent)/100

          holder = co_mass
     END DO
      
     !Calculate weight percent of Cobalt
     DO i=1, co_size
          cobalt_array(i)%weight_percent = (cobalt_array(i)%isotopic_mass &
          &   * cobalt_array(i)%atom_percent)/co_mass
     END DO

     !Number of Nickel Isotopes
     ni_size = 5

     nickel_array(1)%z = 28
     nickel_array(1)%a = 58
     nickel_array(1)%symbol ="Ni-58 "
     nickel_array(1)%mass_defect = -60.2287
     nickel_array(1)%atom_percent = 68.077
     nickel_array(1)%zaid = "28058.00c"

     nickel_array(2)%z = 28
     nickel_array(2)%a = 60
     nickel_array(2)%symbol ="Ni-60 "
     nickel_array(2)%mass_defect = -64.4731
     nickel_array(2)%atom_percent = 26.223
     nickel_array(2)%zaid = "28060.00c"

     nickel_array(3)%z = 28
     nickel_array(3)%a = 61
     nickel_array(3)%symbol ="Ni-61 "
     nickel_array(3)%mass_defect = -64.2219
     nickel_array(3)%atom_percent = 1.1399
     nickel_array(3)%zaid = "28061.00c"

     nickel_array(4)%z = 28
     nickel_array(4)%a = 62
     nickel_array(4)%symbol ="Ni-62 "
     nickel_array(4)%mass_defect = -66.7463
     nickel_array(4)%atom_percent = 3.6346
     nickel_array(4)%zaid = "28062.00c"

     nickel_array(5)%z = 28
     nickel_array(5)%a = 64
     nickel_array(5)%symbol ="Ni-64 "
     nickel_array(5)%mass_defect = -67.0989
     nickel_array(5)%atom_percent = 0.9255
     nickel_array(5)%zaid = "28064.00c"

     !Calculate isotopic mass for Nickel
     DO i=1,ni_size
          holder = nickel_array(i)%a + (nickel_array(i)%mass_defect/MeV_amu)

          nickel_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ni_mass = 0.0
     holder = 0.0

     !Calculate mass of Nickel
     DO i=1, ni_size
          ni_mass = holder + (nickel_array(i)%isotopic_mass*          &
          &     nickel_array(i)%atom_percent)/100

          holder = ni_mass
     END DO
      
     !Calculate weight percent of Nickel
     DO i=1, ni_size
          nickel_array(i)%weight_percent = (nickel_array(i)%isotopic_mass &
          &   * nickel_array(i)%atom_percent)/ni_mass
     END DO

     !Number of Copper Isotopes
     cu_size = 2

     copper_array(1)%z = 29
     copper_array(1)%a = 63
     copper_array(1)%symbol ="Cu-63 "
     copper_array(1)%mass_defect = -65.5797
     copper_array(1)%atom_percent = 69.15
     copper_array(1)%zaid = "29063.00c"

     copper_array(2)%z = 29
     copper_array(2)%a = 65
     copper_array(2)%symbol ="Cu-65 "
     copper_array(2)%mass_defect = -67.2636
     copper_array(2)%atom_percent = 30.85
     copper_array(2)%zaid = "29065.00c"

     !Calculate isotopic mass for Copper
     DO i=1,cu_size
          holder = copper_array(i)%a + (copper_array(i)%mass_defect/MeV_amu)

          copper_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     cu_mass = 0.0
     holder = 0.0

     !Calculate mass of Copper
     DO i=1, cu_size
          cu_mass = holder + (copper_array(i)%isotopic_mass*          &
          &     copper_array(i)%atom_percent)/100

          holder = cu_mass
     END DO

     !Calculate weight percent of Copper
     DO i=1, cu_size
          copper_array(i)%weight_percent = (copper_array(i)%isotopic_mass &
          &   * copper_array(i)%atom_percent)/cu_mass
     END DO

     !Number of Zinc Isotopes
     zn_size = 5

     zinc_array(1)%z = 30
     zinc_array(1)%a = 64
     zinc_array(1)%symbol ="Zn-64 "
     zinc_array(1)%mass_defect = -66.0040
     zinc_array(1)%atom_percent = 49.17
     zinc_array(1)%zaid = "30064.00c"

     zinc_array(2)%z = 30
     zinc_array(2)%a = 66
     zinc_array(2)%symbol ="Zn-66 "
     zinc_array(2)%mass_defect = -68.8991
     zinc_array(2)%atom_percent = 27.73
     zinc_array(2)%zaid = "30066.00c"

     zinc_array(3)%z = 30
     zinc_array(3)%a = 67
     zinc_array(3)%symbol ="Zn-67 "
     zinc_array(3)%mass_defect = -67.8803
     zinc_array(3)%atom_percent = 4.04
     zinc_array(3)%zaid = "30067.00c"

     zinc_array(4)%z = 30
     zinc_array(4)%a = 68
     zinc_array(4)%symbol ="Zn-68 "
     zinc_array(4)%mass_defect = -70.0070
     zinc_array(4)%atom_percent = 18.45
     zinc_array(4)%zaid = "30068.00c"

     zinc_array(5)%z = 30
     zinc_array(5)%a = 70
     zinc_array(5)%symbol ="Zn-70 "
     zinc_array(5)%mass_defect = -69.5647
     zinc_array(5)%atom_percent = 0.61
     zinc_array(5)%zaid = "30070.00c"

     !Calculate isotopic mass for Zinc
     DO i=1,zn_size
          holder = zinc_array(i)%a + (zinc_array(i)%mass_defect/MeV_amu)

          zinc_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     zn_mass = 0.0
     holder = 0.0

     !Calculate mass of Zinc
     DO i=1, zn_size
          zn_mass = holder + (zinc_array(i)%isotopic_mass*          &
          &     zinc_array(i)%atom_percent)/100

          holder = zn_mass
     END DO
      
     !Calculate weight percent of Zinc
     DO i=1, zn_size
          zinc_array(i)%weight_percent = (zinc_array(i)%isotopic_mass &
          &   * zinc_array(i)%atom_percent)/zn_mass
     END DO

   END SUBROUTINE Z26_Z30
