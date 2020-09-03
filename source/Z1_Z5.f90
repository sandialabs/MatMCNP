!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z1_Z5 
     USE NWC_2000
     USE NWC_DATABASE
     IMPLICIT NONE

     !This subroutine contains first five elements (hydrogen,helium,lithium,beryllium,and
     !boron.) and their naturally occuring isotopes.
     !Created with the Current Nuclear Wallet Cards (2004) on
     !May 24, 2004 by Karen Kajder.
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
     

     ! Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Hydrogen Isotopes
     h_size = 2

     hydrogen_array(1)%z = 1
     hydrogen_array(1)%a = 1
     hydrogen_array(1)%symbol =" H-1  "
     hydrogen_array(1)%mass_defect = 7.2889
     hydrogen_array(1)%atom_percent = 99.9885
     hydrogen_array(1)%zaid = "01001.00c"

     hydrogen_array(2)%z = 1
     hydrogen_array(2)%a = 2
     hydrogen_array(2)%symbol =" H-2  "
     hydrogen_array(2)%mass_defect = 13.1357
     hydrogen_array(2)%atom_percent = 0.0115
     hydrogen_array(2)%zaid = "01002.00c"

     !Calculate isotopic mass for Hydrogen
     DO i=1, h_size
          holder = hydrogen_array(i)%a + (hydrogen_array(i)%mass_defect/MeV_amu)

          hydrogen_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     h_mass = 0.0
     holder = 0.0

     !Calculate mass of Hydrogen
     DO i=1, h_size
          h_mass = holder + (hydrogen_array(i)%isotopic_mass*hydrogen_array(i)%atom_percent)/100

          holder = h_mass
     END DO
      
     !Calculate weight percent of Hydrogen
     DO i=1, h_size
          hydrogen_array(i)%weight_percent = (hydrogen_array(i)%isotopic_mass &
          &   * hydrogen_array(i)%atom_percent)/h_mass
     END DO

     !Number of Helium Isotopes
     he_size = 2

     helium_array(1)%z = 2
     helium_array(1)%a = 3
     helium_array(1)%symbol =" He-3 "
     helium_array(1)%mass_defect = 14.9312
     helium_array(1)%atom_percent = 0.000134
     helium_array(1)%zaid = "02003.00c"

     helium_array(2)%z = 2
     helium_array(2)%a = 4
     helium_array(2)%symbol =" He-4 "
     helium_array(2)%mass_defect = 2.4249
     helium_array(2)%atom_percent = 99.999866
     helium_array(2)%zaid = "02004.00c"

     !Calculate isotopic mass for Helium
     DO i=1, he_size
          holder = helium_array(i)%a + (helium_array(i)%mass_defect/MeV_amu)

          helium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     he_mass = 0.0
     holder = 0.0

     !Calculate mass of Helium
     DO i=1, he_size
          he_mass = holder + (helium_array(i)%isotopic_mass*helium_array(i)%atom_percent)/100

          holder = he_mass
     END DO
      
     !Calculate weight percent of Helium
     DO i=1, he_size
          helium_array(i)%weight_percent = (helium_array(i)%isotopic_mass &
          &   * helium_array(i)%atom_percent)/he_mass
     END DO

     !Number of Lithium Isotopes
     li_size = 2

     lithium_array(1)%z = 3
     lithium_array(1)%a = 6
     lithium_array(1)%symbol =" Li-6 "
     lithium_array(1)%mass_defect = 14.0868
     lithium_array(1)%atom_percent = 7.59
     lithium_array(1)%zaid = "03006.00c"

     lithium_array(2)%z = 3
     lithium_array(2)%a = 7
     lithium_array(2)%symbol =" Li-7 "
     lithium_array(2)%mass_defect = 14.9071
     lithium_array(2)%atom_percent = 92.41
     lithium_array(2)%zaid = "03007.00c"

     !Calculate isotopic mass for Lithium
     DO i=1, li_size
          holder = lithium_array(i)%a + (lithium_array(i)%mass_defect/MeV_amu)

          lithium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     li_mass = 0.0
     holder = 0.0

     !Calculate mass of Lithium
     DO i=1, li_size
          li_mass = holder + (lithium_array(i)%isotopic_mass*lithium_array(i)%atom_percent)/100

          holder = li_mass
     END DO
      
     !Calculate weight percent of Lithium
     DO i=1, li_size
          lithium_array(i)%weight_percent = (lithium_array(i)%isotopic_mass &
          &   * lithium_array(i)%atom_percent)/li_mass   
     END DO

     !Number of Beryllium Isotopes
     be_size = 1

     beryllium_array(1)%z = 4
     beryllium_array(1)%a = 9
     beryllium_array(1)%symbol =" Be-9 "
     beryllium_array(1)%mass_defect = 11.3484
     beryllium_array(1)%atom_percent = 100
     beryllium_array(1)%zaid = "04009.00c"


     !Calculate isotopic mass for Beryllium
     DO i=1, be_size
          holder = beryllium_array(i)%a + (beryllium_array(i)%mass_defect/MeV_amu)

          beryllium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     be_mass = 0.0
     holder = 0.0

     !Calculate mass of Beryllium
     DO i=1, be_size
          be_mass = holder + (beryllium_array(i)%isotopic_mass*beryllium_array(i)%atom_percent)/100

          holder = be_mass
     END DO
      
     !Calculate weight percent of Beryllium
     DO i=1, be_size
          beryllium_array(i)%weight_percent = (beryllium_array(i)%isotopic_mass &
          &   * beryllium_array(i)%atom_percent)/be_mass
     END DO
	   
     !Number of Boron Isotopes
     b_size = 2

     boron_array(1)%z = 5
     boron_array(1)%a = 10
     boron_array(1)%symbol =" B-10 "
     boron_array(1)%mass_defect = 12.0506
     boron_array(1)%atom_percent = 19.9
     boron_array(1)%zaid = "05010.00c"

     boron_array(2)%z = 5
     boron_array(2)%a = 11
     boron_array(2)%symbol =" B-11 "
     boron_array(2)%mass_defect = 8.6677
     boron_array(2)%atom_percent = 80.1
     boron_array(2)%zaid = "05011.00c"

     !Calculate isotopic mass for Boron
     DO i=1, b_size
          holder = boron_array(i)%a + (boron_array(i)%mass_defect/MeV_amu)

          boron_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     b_mass = 0.0
     holder = 0.0

     !Calculate mass of Boron
     DO i=1, b_size
          b_mass = holder + (boron_array(i)%isotopic_mass*boron_array(i)%atom_percent)/100

          holder = b_mass
     END DO
      
     !Calculate weight percent of Boron
     DO i=1, b_size
          boron_array(i)%weight_percent = (boron_array(i)%isotopic_mass &
          &   * boron_array(i)%atom_percent)/b_mass
     END DO

  END SUBROUTINE Z1_Z5
