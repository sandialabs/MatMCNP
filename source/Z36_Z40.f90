!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z36_Z40
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 36-40 (krypton, rubidium, strontium,
     !yttrium, and zirconium) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 26, 2004 by Karen Kajder.
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

     !Number of Krypton Isotopes
     kr_size = 6

     krypton_array(1)%z = 36
     krypton_array(1)%a = 78
     krypton_array(1)%symbol ="Kr-78 "
     krypton_array(1)%mass_defect = -74.1782
     krypton_array(1)%atom_percent = 0.355
     krypton_array(1)%zaid = "36078.00c"

     krypton_array(2)%z = 36
     krypton_array(2)%a = 80
     krypton_array(2)%symbol ="Kr-80 "
     krypton_array(2)%mass_defect = -77.8933
     krypton_array(2)%atom_percent = 2.286
     krypton_array(2)%zaid = "36080.00c"

     krypton_array(3)%z = 36
     krypton_array(3)%a = 82
     krypton_array(3)%symbol ="Kr-82 "
     krypton_array(3)%mass_defect = -80.5917
     krypton_array(3)%atom_percent = 11.593
     krypton_array(3)%zaid = "36082.00c"

     krypton_array(4)%z = 36
     krypton_array(4)%a = 83
     krypton_array(4)%symbol ="Kr-83 "
     krypton_array(4)%mass_defect = -79.9906
     krypton_array(4)%atom_percent = 11.500
     krypton_array(4)%zaid = "36083.00c"

     krypton_array(5)%z = 36
     krypton_array(5)%a = 84
     krypton_array(5)%symbol ="Kr-84 "
     krypton_array(5)%mass_defect = -82.4393
     krypton_array(5)%atom_percent = 56.987
     krypton_array(5)%zaid = "36084.00c"

     krypton_array(6)%z = 36
     krypton_array(6)%a = 86
     krypton_array(6)%symbol ="Kr-86 "
     krypton_array(6)%mass_defect = -83.2656
     krypton_array(6)%atom_percent = 17.279
     krypton_array(6)%zaid = "36086.00c"

     !Calculate isotopic mass for Krypton
     DO i=1,kr_size
          holder = krypton_array(i)%a + (krypton_array(i)%mass_defect/MeV_amu)

          krypton_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     kr_mass = 0.0
     holder = 0.0

     !Calculate mass of Krypton
     DO i=1, kr_size
          kr_mass = holder + (krypton_array(i)%isotopic_mass* &
          &  krypton_array(i)%atom_percent)/100

          holder = kr_mass
     END DO
      
     !Calculate weight percent of Krypton
     DO i=1, kr_size
          krypton_array(i)%weight_percent = (krypton_array(i)%isotopic_mass &
          &   * krypton_array(i)%atom_percent)/kr_mass
     END DO

     !Number of Rubidium Isotopes
     rb_size = 2

     rubidium_array(1)%z = 37
     rubidium_array(1)%a = 85
     rubidium_array(1)%symbol ="Rb-85 "
     rubidium_array(1)%mass_defect = -82.1673
     rubidium_array(1)%atom_percent = 72.17
     rubidium_array(1)%zaid = "37085.00c"

     rubidium_array(2)%z = 37
     rubidium_array(2)%a = 87
     rubidium_array(2)%symbol ="Rb-87 "
     rubidium_array(2)%mass_defect = -84.5977
     rubidium_array(2)%atom_percent = 27.83
     rubidium_array(2)%zaid = "37087.00c"

     !Calculate isotopic mass for Rubidium
     DO i=1,rb_size
          holder = rubidium_array(i)%a + (rubidium_array(i)%mass_defect/MeV_amu)

          rubidium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     rb_mass = 0.0
     holder = 0.0

     !Calculate mass of Rubidium
     DO i=1, rb_size
          rb_mass = holder + (rubidium_array(i)%isotopic_mass* &
          &  rubidium_array(i)%atom_percent)/100

          holder = rb_mass
     END DO

     !Calculate weight percent of Rubidium
     DO i=1, rb_size
          rubidium_array(i)%weight_percent = (rubidium_array(i)%isotopic_mass &
          &   * rubidium_array(i)%atom_percent)/rb_mass
     END DO

     !Number of Strontium Isotopes
     sr_size = 4

     strontium_array(1)%z = 38
     strontium_array(1)%a = 84
     strontium_array(1)%symbol ="Sr-84 "
     strontium_array(1)%mass_defect = -80.6495
     strontium_array(1)%atom_percent = 0.56
     strontium_array(1)%zaid = "38084.00c"

     strontium_array(2)%z = 38
     strontium_array(2)%a = 86
     strontium_array(2)%symbol ="Sr-86 "
     strontium_array(2)%mass_defect = -84.5230
     strontium_array(2)%atom_percent = 9.86
     strontium_array(2)%zaid = "38086.00c"

     strontium_array(3)%z = 38
     strontium_array(3)%a = 87
     strontium_array(3)%symbol ="Sr-87 "
     strontium_array(3)%mass_defect = -84.8800
     strontium_array(3)%atom_percent = 7.00
     strontium_array(3)%zaid = "38087.00c"

     strontium_array(4)%z = 38
     strontium_array(4)%a = 88
     strontium_array(4)%symbol ="Sr-88 "
     strontium_array(4)%mass_defect = -87.9216
     strontium_array(4)%atom_percent = 82.58
     strontium_array(4)%zaid = "38088.00c"

     !Calculate isotopic mass for Strontium
     DO i=1,sr_size
          holder = strontium_array(i)%a + (strontium_array(i)%mass_defect/MeV_amu)

          strontium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     sr_mass = 0.0
     holder = 0.0

     !Calculate mass of Strontium
     DO i=1, sr_size
          sr_mass = holder + (strontium_array(i)%isotopic_mass* &
          & strontium_array(i)%atom_percent)/100

          holder = sr_mass
     END DO
      
     !Calculate weight percent of Strontium
     DO i=1, sr_size
          strontium_array(i)%weight_percent = (strontium_array(i)%isotopic_mass &
          &   * strontium_array(i)%atom_percent)/sr_mass
     END DO

     !Number of Yttrium Isotopes
     y_size = 1

     yttrium_array(1)%z = 39
     yttrium_array(1)%a = 89
     yttrium_array(1)%symbol =" Y-89 "
     yttrium_array(1)%mass_defect = -87.7083
     yttrium_array(1)%atom_percent = 100.0
     yttrium_array(1)%zaid = "39089.00c"

     !Calculate isotopic mass for Yttrium
     DO i=1,y_size
          holder = yttrium_array(i)%a + (yttrium_array(i)%mass_defect/MeV_amu)

          yttrium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     y_mass = 0.0
     holder = 0.0

     !Calculate mass of Yttrium
     DO i=1, y_size
          y_mass = holder + (yttrium_array(i)%isotopic_mass* &
          & yttrium_array(i)%atom_percent)/100

          holder = y_mass
     END DO
      
     !Calculate weight percent of Yttrium
     DO i=1, y_size
          yttrium_array(i)%weight_percent = (yttrium_array(i)%isotopic_mass &
          &   * yttrium_array(i)%atom_percent)/y_mass
     END DO

     !Number of Zirconium Isotopes
     zr_size = 5

     zirconium_array(1)%z = 40
     zirconium_array(1)%a = 90
     zirconium_array(1)%symbol ="Zr-90 "
     zirconium_array(1)%mass_defect = -88.7725
     zirconium_array(1)%atom_percent = 51.45
     zirconium_array(1)%zaid = "40090.00c"

     zirconium_array(2)%z = 40
     zirconium_array(2)%a = 91
     zirconium_array(2)%symbol ="Zr-91 "
     zirconium_array(2)%mass_defect = -87.8955
     zirconium_array(2)%atom_percent = 11.22
     zirconium_array(2)%zaid = "40091.00c"

     zirconium_array(3)%z = 40
     zirconium_array(3)%a = 92
     zirconium_array(3)%symbol ="Zr-92 "
     zirconium_array(3)%mass_defect = -88.4490
     zirconium_array(3)%atom_percent = 17.15
     zirconium_array(3)%zaid = "40092.00c"

     zirconium_array(4)%z = 40
     zirconium_array(4)%a = 94
     zirconium_array(4)%symbol ="Zr-94 "
     zirconium_array(4)%mass_defect = -87.2693
     zirconium_array(4)%atom_percent = 17.38
     zirconium_array(4)%zaid = "40094.00c"

     zirconium_array(5)%z = 40
     zirconium_array(5)%a = 96
     zirconium_array(5)%symbol ="Zr-96 "
     zirconium_array(5)%mass_defect = -85.4388
     zirconium_array(5)%atom_percent = 2.80
     zirconium_array(5)%zaid = "40096.00c"

     !Calculate isotopic mass for Zirconium
     DO i=1,zr_size
          holder = zirconium_array(i)%a + (zirconium_array(i)%mass_defect/MeV_amu)

          zirconium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     zr_mass = 0.0
     holder = 0.0

     !Calculate mass of Zirconium
     DO i=1, zr_size
          zr_mass = holder + (zirconium_array(i)%isotopic_mass* &
          & zirconium_array(i)%atom_percent)/100

          holder = zr_mass
     END DO
      
     !Calculate weight percent of Zirconium
     DO i=1, zr_size
          zirconium_array(i)%weight_percent = (zirconium_array(i)%isotopic_mass &
          &   * zirconium_array(i)%atom_percent)/zr_mass
     END DO

   END SUBROUTINE Z36_Z40
