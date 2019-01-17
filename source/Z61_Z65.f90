!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z61_Z65
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 61-65 (promethium, samarium, europium,
     !gadolinium, and terbium) and their naturally occuring isotopes. Promethium
     !does not occur naturally in nature so it has been left out.  
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

     !Number of Samarium Isotopes
     sm_size = 7 

     samarium_array(1)%z = 62
     samarium_array(1)%a = 144
     samarium_array(1)%symbol ="Sm-144"
     samarium_array(1)%mass_defect = -81.9657
     samarium_array(1)%atom_percent = 3.07
     samarium_array(1)%zaid = "62144.80c"

     samarium_array(2)%z = 62
     samarium_array(2)%a = 147
     samarium_array(2)%symbol ="Sm-147"
     samarium_array(2)%mass_defect = -79.2657
     samarium_array(2)%atom_percent = 14.99
     samarium_array(2)%zaid = "62147.80c"

     samarium_array(3)%z = 62
     samarium_array(3)%a = 148
     samarium_array(3)%symbol ="Sm-148"
     samarium_array(3)%mass_defect = -79.3358
     samarium_array(3)%atom_percent = 11.24
     samarium_array(3)%zaid = "62148.80c"

     samarium_array(4)%z = 62
     samarium_array(4)%a = 149
     samarium_array(4)%symbol ="Sm-149"
     samarium_array(4)%mass_defect = -77.1350
     samarium_array(4)%atom_percent = 13.82
     samarium_array(4)%zaid = "62149.80c"

     samarium_array(5)%z = 62
     samarium_array(5)%a = 150
     samarium_array(5)%symbol ="Sm-150"
     samarium_array(5)%mass_defect = -77.0504
     samarium_array(5)%atom_percent = 7.38
     samarium_array(5)%zaid = "62150.80c"

     samarium_array(6)%z = 62
     samarium_array(6)%a = 152
     samarium_array(6)%symbol ="Sm-152"
     samarium_array(6)%mass_defect = -74.7622
     samarium_array(6)%atom_percent = 26.75
     samarium_array(6)%zaid = "62152.80c"

     samarium_array(7)%z = 62
     samarium_array(7)%a = 154
     samarium_array(7)%symbol ="Sm-154"
     samarium_array(7)%mass_defect = -72.4549
     samarium_array(7)%atom_percent = 22.75
     samarium_array(7)%zaid = "62154.80c"

     !Calculate isotopic mass for Samarium
     DO i=1,sm_size
          holder = samarium_array(i)%a + (samarium_array(i)%mass_defect/931.494)

          samarium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     sm_mass = 0.0
     holder = 0.0

     !Calculate mass of Samarium
     DO i=1, sm_size
          sm_mass = holder + (samarium_array(i)%isotopic_mass* &
          &  samarium_array(i)%atom_percent)/100

          holder = sm_mass
     END DO
     
     !Calculate weight percent of Samarium
     DO i=1, sm_size
          samarium_array(i)%weight_percent = (samarium_array(i)%isotopic_mass &
          &   * samarium_array(i)%atom_percent)/sm_mass
     END DO

     !Number of Europium Isotopes
     eu_size = 2

     europium_array(1)%z = 63
     europium_array(1)%a = 151
     europium_array(1)%symbol ="Eu-151"
     europium_array(1)%mass_defect = -74.6517
     europium_array(1)%atom_percent = 47.81
     europium_array(1)%zaid = "63151.80c"

     europium_array(2)%z = 63
     europium_array(2)%a = 153
     europium_array(2)%symbol ="Eu-153"
     europium_array(2)%mass_defect = -73.3661
     europium_array(2)%atom_percent = 52.19
     europium_array(2)%zaid = "63153.80c"

     !Calculate isotopic mass for Europium
     DO i=1,eu_size
          holder = europium_array(i)%a + (europium_array(i)%mass_defect/931.494)

          europium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     eu_mass = 0.0
     holder = 0.0

     !Calculate mass of Europium
     DO i=1, eu_size
          eu_mass = holder + (europium_array(i)%isotopic_mass* &
          &  europium_array(i)%atom_percent)/100

          holder = eu_mass
     END DO

     !Calculate weight percent of Europium
     DO i=1, eu_size
          europium_array(i)%weight_percent = (europium_array(i)%isotopic_mass &
          &   * europium_array(i)%atom_percent)/eu_mass
     END DO

     !Number of Gadolinium Isotopes
     gd_size = 7 

     gadolinium_array(1)%z = 64
     gadolinium_array(1)%a = 152
     gadolinium_array(1)%symbol ="Gd-152"
     gadolinium_array(1)%mass_defect = -74.7065
     gadolinium_array(1)%atom_percent = 0.20
     gadolinium_array(1)%zaid = "64152.80c"

     gadolinium_array(2)%z = 64
     gadolinium_array(2)%a = 154
     gadolinium_array(2)%symbol ="Gd-154"
     gadolinium_array(2)%mass_defect = -73.7055
     gadolinium_array(2)%atom_percent = 2.18
     gadolinium_array(2)%zaid = "64154.80c"

     gadolinium_array(3)%z = 64
     gadolinium_array(3)%a = 155
     gadolinium_array(3)%symbol ="Gd-155"
     gadolinium_array(3)%mass_defect = -72.0694
     gadolinium_array(3)%atom_percent = 14.80
     gadolinium_array(3)%zaid = "64155.80c"

     gadolinium_array(4)%z = 64
     gadolinium_array(4)%a = 156
     gadolinium_array(4)%symbol ="Gd-156"
     gadolinium_array(4)%mass_defect = -72.5345
     gadolinium_array(4)%atom_percent = 20.47
     gadolinium_array(4)%zaid = "64156.80c"

     gadolinium_array(5)%z = 64
     gadolinium_array(5)%a = 157
     gadolinium_array(5)%symbol ="Gd-157"
     gadolinium_array(5)%mass_defect = -70.8230
     gadolinium_array(5)%atom_percent = 15.65
     gadolinium_array(5)%zaid = "64157.80c"

     gadolinium_array(6)%z = 64
     gadolinium_array(6)%a = 158
     gadolinium_array(6)%symbol ="Gd-158"
     gadolinium_array(6)%mass_defect = -70.6891
     gadolinium_array(6)%atom_percent = 24.84
     gadolinium_array(6)%zaid = "64158.80c"

     gadolinium_array(7)%z = 64
     gadolinium_array(7)%a = 160
     gadolinium_array(7)%symbol ="Gd-160"
     gadolinium_array(7)%mass_defect = -67.9409
     gadolinium_array(7)%atom_percent = 21.86
     gadolinium_array(7)%zaid = "64160.80c"

     !Calculate isotopic mass for Gadolinium
     DO i=1,gd_size
          holder = gadolinium_array(i)%a + (gadolinium_array(i)%mass_defect/931.494)

          gadolinium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     gd_mass = 0.0
     holder = 0.0

     !Calculate mass of Gadolinium
     DO i=1, gd_size
          gd_mass = holder + (gadolinium_array(i)%isotopic_mass* &
          &  gadolinium_array(i)%atom_percent)/100

          holder = gd_mass
     END DO
     
     !Calculate weight percent of Gadolinium
     DO i=1, gd_size
          gadolinium_array(i)%weight_percent = (gadolinium_array(i)%isotopic_mass &
          &   * gadolinium_array(i)%atom_percent)/gd_mass
     END DO

     !Number of Terbium Isotopes
     tb_size = 1

     terbium_array(1)%z = 65
     terbium_array(1)%a = 159
     terbium_array(1)%symbol ="Tb-159"
     terbium_array(1)%mass_defect = -69.5315
     terbium_array(1)%atom_percent = 100.0
     terbium_array(1)%zaid = "65159.80c"

     !Calculate isotopic mass for Terbium
     DO i=1, tb_size
          holder = terbium_array(i)%a + (terbium_array(i)%mass_defect/931.494)

          terbium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     tb_mass = 0.0
     holder = 0.0

     !Calculate mass of Terbium
     DO i=1, tb_size
          tb_mass = holder + (terbium_array(i)%isotopic_mass* &
          &  terbium_array(i)%atom_percent)/100

          holder = tb_mass
     END DO
     
     !Calculate weight percent of Terbium
     DO i=1, tb_size
          terbium_array(i)%weight_percent = (terbium_array(i)%isotopic_mass &
          &   * terbium_array(i)%atom_percent)/tb_mass
     END DO

   END SUBROUTINE Z61_Z65
