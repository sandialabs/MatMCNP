!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z31_Z35
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE
	
     !This subroutine contains elements 31-35 (gallium, germanium, arsenic,
     !selenium, and bromine) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 14, 2011.
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

     !Number of Gallium Isotopes
     ga_size = 2

     gallium_array(1)%z = 31
     gallium_array(1)%a = 69
     gallium_array(1)%symbol ="Ga-69 "
     gallium_array(1)%mass_defect = -69.3278
     gallium_array(1)%atom_percent = 60.108
     gallium_array(1)%zaid = "31069.00c"

     gallium_array(2)%z = 31
     gallium_array(2)%a = 71
     gallium_array(2)%symbol ="Ga-71 "
     gallium_array(2)%mass_defect = -70.1391
     gallium_array(2)%atom_percent = 39.892
     gallium_array(2)%zaid = "31071.00c"

     !Calculate isotopic mass for Gallium
     DO i=1,ga_size
          holder = gallium_array(i)%a + (gallium_array(i)%mass_defect/MeV_amu)

          gallium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ga_mass = 0.0
     holder = 0.0

     !Calculate mass of Gallium
     DO i=1, ga_size
          ga_mass = holder + (gallium_array(i)%isotopic_mass* &
          &     gallium_array(i)%atom_percent)/100

          holder = ga_mass
     END DO

     !Calculate weight percent of Gallium
     DO i=1, ga_size
          gallium_array(i)%weight_percent = (gallium_array(i)%isotopic_mass &
          &   * gallium_array(i)%atom_percent)/ga_mass
     END DO

     !Number of Germanium Isotopes
     ge_size = 5

     germanium_array(1)%z = 32
     germanium_array(1)%a = 70
     germanium_array(1)%symbol ="Ge-70 "
     germanium_array(1)%mass_defect = -70.5618
     germanium_array(1)%atom_percent = 20.57
     germanium_array(1)%zaid = "32070.00c"

     germanium_array(2)%z = 32
     germanium_array(2)%a = 72
     germanium_array(2)%symbol ="Ge-72 "
     germanium_array(2)%mass_defect = -72.5859
     germanium_array(2)%atom_percent = 27.45
     germanium_array(2)%zaid = "32072.00c"

     germanium_array(3)%z = 32
     germanium_array(3)%a = 73
     germanium_array(3)%symbol ="Ge-73 "
     germanium_array(3)%mass_defect = -71.2975
     germanium_array(3)%atom_percent = 7.75
     germanium_array(3)%zaid = "32073.00c"

     germanium_array(4)%z = 32
     germanium_array(4)%a = 74
     germanium_array(4)%symbol ="Ge-74 "
     germanium_array(4)%mass_defect = -73.4224
     germanium_array(4)%atom_percent = 36.50
     germanium_array(4)%zaid = "32074.00c"

     germanium_array(5)%z = 32
     germanium_array(5)%a = 76
     germanium_array(5)%symbol ="Ge-76 "
     germanium_array(5)%mass_defect = -73.2128
     germanium_array(5)%atom_percent = 7.73
     germanium_array(5)%zaid = "32076.00c"

     !Calculate isotopic mass for Germanium
     DO i=1,ge_size
          holder = germanium_array(i)%a + (germanium_array(i)%mass_defect/MeV_amu)

          germanium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ge_mass = 0.0
     holder = 0.0

     !Calculate mass of Germanium
     DO i=1, ge_size
          ge_mass = holder + (germanium_array(i)%isotopic_mass* &
          &     germanium_array(i)%atom_percent)/100

          holder = ge_mass
     END DO
      
     !Calculate weight percent of Germanium
     DO i=1, ge_size
          germanium_array(i)%weight_percent = (germanium_array(i)%isotopic_mass &
          &   * germanium_array(i)%atom_percent)/ge_mass
     END DO

     !Number of Arsenic Isotopes
     as_size = 1

     arsenic_array(1)%z = 33
     arsenic_array(1)%a = 75
     arsenic_array(1)%symbol ="As-75 "
     arsenic_array(1)%mass_defect = -73.0341
     arsenic_array(1)%atom_percent = 100.0
     arsenic_array(1)%zaid = "33075.00c"

     !Calculate isotopic mass for Arsenic
     DO i=1,as_size
          holder = arsenic_array(i)%a + (arsenic_array(i)%mass_defect/MeV_amu)

          arsenic_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     as_mass = 0.0
     holder = 0.0

     !Calculate mass of Arsenic
     DO i=1, as_size
          as_mass = holder + (arsenic_array(i)%isotopic_mass* &
          &     arsenic_array(i)%atom_percent)/100

          holder = as_mass
     END DO
      
     !Calculate weight percent of Arsenic
     DO i=1, as_size
          arsenic_array(i)%weight_percent = (arsenic_array(i)%isotopic_mass &
          &   * arsenic_array(i)%atom_percent)/as_mass
     END DO

     !Number of Selenium Isotopes
     se_size = 6

     selenium_array(1)%z = 34
     selenium_array(1)%a = 74
     selenium_array(1)%symbol ="Se-74 "
     selenium_array(1)%mass_defect = -72.2132
     selenium_array(1)%atom_percent = 0.89
     selenium_array(1)%zaid = "34074.00c"

     selenium_array(2)%z = 34
     selenium_array(2)%a = 76
     selenium_array(2)%symbol ="Se-76 "
     selenium_array(2)%mass_defect = -75.2519
     selenium_array(2)%atom_percent = 9.37
     selenium_array(2)%zaid = "34076.00c"

     selenium_array(3)%z = 34
     selenium_array(3)%a = 77
     selenium_array(3)%symbol ="Se-77 "
     selenium_array(3)%mass_defect = -74.5994
     selenium_array(3)%atom_percent = 7.63
     selenium_array(3)%zaid = "34077.00c"

     selenium_array(4)%z = 34
     selenium_array(4)%a = 78
     selenium_array(4)%symbol ="Se-78 "
     selenium_array(4)%mass_defect = -77.0259
     selenium_array(4)%atom_percent = 23.77
     selenium_array(4)%zaid = "34078.00c"

     selenium_array(5)%z = 34
     selenium_array(5)%a = 80
     selenium_array(5)%symbol ="Se-80 "
     selenium_array(5)%mass_defect = -77.7594
     selenium_array(5)%atom_percent = 49.61
     selenium_array(5)%zaid = "34080.00c"

     selenium_array(6)%z = 34
     selenium_array(6)%a = 82
     selenium_array(6)%symbol ="Se-82 "
     selenium_array(6)%mass_defect = -77.5938
     selenium_array(6)%atom_percent = 8.73
     selenium_array(6)%zaid = "34082.00c"

     !Calculate isotopic mass for Selenium
     DO i=1,se_size
          holder = selenium_array(i)%a + (selenium_array(i)%mass_defect/MeV_amu)

          selenium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     se_mass = 0.0
     holder = 0.0

     !Calculate mass of Selenium
     DO i=1, se_size
          se_mass = holder + (selenium_array(i)%isotopic_mass* &
          &  selenium_array(i)%atom_percent)/100

          holder = se_mass
     END DO
      
     !Calculate weight percent of Selenium
     DO i=1, se_size
          selenium_array(i)%weight_percent = (selenium_array(i)%isotopic_mass &
          &   * selenium_array(i)%atom_percent)/se_mass
     END DO

     !Number of Bromine Isotopes
     br_size = 2

     bromine_array(1)%z = 35
     bromine_array(1)%a = 79
     bromine_array(1)%symbol ="Br-79 "
     bromine_array(1)%mass_defect = -76.0680
     bromine_array(1)%atom_percent = 50.69
     bromine_array(1)%zaid = "35079.00c"

     bromine_array(2)%z = 35
     bromine_array(2)%a = 81
     bromine_array(2)%symbol ="Br-81 "
     bromine_array(2)%mass_defect = -77.9770
     bromine_array(2)%atom_percent = 49.31
     bromine_array(2)%zaid = "35081.00c"

     !Calculate isotopic mass for Bromine
     DO i=1,br_size
          holder = bromine_array(i)%a + (bromine_array(i)%mass_defect/MeV_amu)

          bromine_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     br_mass = 0.0
     holder = 0.0

     !Calculate mass of Bromine
     DO i=1, br_size
          br_mass = holder + (bromine_array(i)%isotopic_mass* &
          &   bromine_array(i)%atom_percent)/100

          holder = br_mass
     END DO

     !Calculate weight percent of Bromine
     DO i=1, br_size
          bromine_array(i)%weight_percent = (bromine_array(i)%isotopic_mass &
          &   * bromine_array(i)%atom_percent)/br_mass
     END DO

   END SUBROUTINE Z31_Z35
