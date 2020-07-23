!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z21_Z25
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 21-25 (scandium,titanium,vanadium,chromium,and
     !manganese) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 12, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
     ! - Vanadium changed from elemental to isotopic
     !
     ! - ENDF/B-VIII.0 cross sections at 293.6 K (that is, .00c) made the default 
     !   on 07/22/2020 by K. Russell DePriest.
     !
     ! - Nuclear Wallet Card values verified on 07/22/2020 using 
     !   "Nuclear Wallet Cards database version of 7/10/2019" on nndc.bnl.gov website.

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Scandium Isotopes
     sc_size = 1

     scandium_array(1)%z = 21
     scandium_array(1)%a = 45
     scandium_array(1)%symbol ="Sc-45 "
     scandium_array(1)%mass_defect = -41.0718
     scandium_array(1)%atom_percent = 100.0
     scandium_array(1)%zaid = "21045.00c"

     !Calculate isotopic mass for Scandium
     DO i=1,sc_size
          holder = scandium_array(i)%a + (scandium_array(i)%mass_defect/MeV_amu)

          scandium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     sc_mass = 0.0
     holder = 0.0

     !Calculate mass of Scandium
     DO i=1, sc_size
          sc_mass = holder + (scandium_array(i)%isotopic_mass*		&
          &	scandium_array(i)%atom_percent)/100

          holder = sc_mass
     END DO
      
     !Calculate weight percent of Scandium
     DO i=1, sc_size
          scandium_array(i)%weight_percent = (scandium_array(i)%isotopic_mass &
          &   * scandium_array(i)%atom_percent)/sc_mass   
     END DO

     !Number of Titanium Isotopes
     ti_size = 5

     titanium_array(1)%z = 22
     titanium_array(1)%a = 46
     titanium_array(1)%symbol ="Ti-46 "
     titanium_array(1)%mass_defect = -44.1277
     titanium_array(1)%atom_percent = 8.25
     titanium_array(1)%zaid = "22046.00c"

     titanium_array(2)%z = 22
     titanium_array(2)%a = 47
     titanium_array(2)%symbol ="Ti-47 "
     titanium_array(2)%mass_defect = -44.9373
     titanium_array(2)%atom_percent = 7.44
     titanium_array(2)%zaid = "22047.00c"

     titanium_array(3)%z = 22
     titanium_array(3)%a = 48
     titanium_array(3)%symbol ="Ti-48 "
     titanium_array(3)%mass_defect = -48.4927
     titanium_array(3)%atom_percent = 73.72
     titanium_array(3)%zaid = "22048.00c"

     titanium_array(4)%z = 22
     titanium_array(4)%a = 49
     titanium_array(4)%symbol ="Ti-49 "
     titanium_array(4)%mass_defect = -48.5637
     titanium_array(4)%atom_percent = 5.41
     titanium_array(4)%zaid = "22049.00c"

     titanium_array(5)%z = 22
     titanium_array(5)%a = 50
     titanium_array(5)%symbol ="Ti-50 "
     titanium_array(5)%mass_defect = -51.4316
     titanium_array(5)%atom_percent = 5.18
     titanium_array(5)%zaid = "22050.00c"

     !Calculate isotopic mass for Titanium
     DO i=1,ti_size
          holder = titanium_array(i)%a + (titanium_array(i)%mass_defect/MeV_amu)

          titanium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ti_mass = 0.0
     holder = 0.0

     !Calculate mass of Titanium
     DO i=1, ti_size
          ti_mass = holder + (titanium_array(i)%isotopic_mass*          &
          &     titanium_array(i)%atom_percent)/100

          holder = ti_mass
     END DO
      
     !Calculate weight percent of Titanium
     DO i=1, ti_size
          titanium_array(i)%weight_percent = (titanium_array(i)%isotopic_mass &
          &   * titanium_array(i)%atom_percent)/ti_mass
     END DO

     !Number of Vanadium Isotopes
     v_size = 2

     vanadium_array(1)%z = 23
     vanadium_array(1)%a = 50
     vanadium_array(1)%symbol =" V-50 "
     vanadium_array(1)%mass_defect = -49.2240
     vanadium_array(1)%atom_percent = 0.250
     vanadium_array(1)%zaid = "23050.00c"

     vanadium_array(2)%z = 23
     vanadium_array(2)%a = 51
     vanadium_array(2)%symbol =" V-51 "
     vanadium_array(2)%mass_defect = -52.2038
     vanadium_array(2)%atom_percent = 99.750
     vanadium_array(2)%zaid = "23051.00c"

     !Calculate isotopic mass for Vanadium
     DO i=1,v_size
          holder = vanadium_array(i)%a + (vanadium_array(i)%mass_defect/MeV_amu)

          vanadium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     v_mass = 0.0
     holder = 0.0

     !Calculate mass of Vanadium
     DO i=1, v_size
          v_mass = holder + (vanadium_array(i)%isotopic_mass*          &
          &     vanadium_array(i)%atom_percent)/100

          holder = v_mass
     END DO
      
     !Calculate weight percent of Vanadium
     DO i=1, v_size
          vanadium_array(i)%weight_percent = (vanadium_array(i)%isotopic_mass &
          &   * vanadium_array(i)%atom_percent)/v_mass
     END DO

     !Number of Chromium Isotopes
     cr_size = 4

     chromium_array(1)%z = 24
     chromium_array(1)%a = 50
     chromium_array(1)%symbol ="Cr-50 "
     chromium_array(1)%mass_defect = -50.2620
     chromium_array(1)%atom_percent = 4.345
     chromium_array(1)%zaid = "24050.00c"

     chromium_array(2)%z = 24
     chromium_array(2)%a = 52
     chromium_array(2)%symbol ="Cr-52 "
     chromium_array(2)%mass_defect = -55.4192
     chromium_array(2)%atom_percent = 83.789
     chromium_array(2)%zaid = "24052.00c"

     chromium_array(3)%z = 24
     chromium_array(3)%a = 53
     chromium_array(3)%symbol ="Cr-53 "
     chromium_array(3)%mass_defect = -55.2869
     chromium_array(3)%atom_percent = 9.501
     chromium_array(3)%zaid = "24053.00c"

     chromium_array(4)%z = 24
     chromium_array(4)%a = 54
     chromium_array(4)%symbol ="Cr-54 "
     chromium_array(4)%mass_defect = -56.9347
     chromium_array(4)%atom_percent = 2.365
     chromium_array(4)%zaid = "24054.00c"

     !Calculate isotopic mass for Chromium
     DO i=1,cr_size
          holder = chromium_array(i)%a + (chromium_array(i)%mass_defect/MeV_amu)

          chromium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     cr_mass = 0.0
     holder = 0.0

     !Calculate mass of Chromium
     DO i=1, cr_size
          cr_mass = holder + (chromium_array(i)%isotopic_mass*          &
          &     chromium_array(i)%atom_percent)/100

          holder = cr_mass
     END DO
      
     !Calculate weight percent of Chromium
     DO i=1, cr_size
          chromium_array(i)%weight_percent = (chromium_array(i)%isotopic_mass &
          &   * chromium_array(i)%atom_percent)/cr_mass
     END DO

     !Number of Manganese Isotopes
     mn_size = 1

     manganese_array(1)%z = 25
     manganese_array(1)%a = 55
     manganese_array(1)%symbol ="Mn-55 "
     manganese_array(1)%mass_defect = -57.7124
     manganese_array(1)%atom_percent = 100.0
     manganese_array(1)%zaid = "25055.00c"

     !Calculate isotopic mass for Manganese
     DO i=1,mn_size
          holder = manganese_array(i)%a + (manganese_array(i)%mass_defect/MeV_amu)

          manganese_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     mn_mass = 0.0
     holder = 0.0

     !Calculate mass of Manganese
     DO i=1, mn_size
          mn_mass = holder + (manganese_array(i)%isotopic_mass*          &
          &     manganese_array(i)%atom_percent)/100

          holder = mn_mass
     END DO
      
     !Calculate weight percent of Manganese
     DO i=1, mn_size
          manganese_array(i)%weight_percent = (manganese_array(i)%isotopic_mass &
          &   * manganese_array(i)%atom_percent)/mn_mass       
     END DO

   END SUBROUTINE Z21_Z25
