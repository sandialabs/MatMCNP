!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z56_Z60
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 56-60 (barium, lanthanum, cerium,
     !praseodymium, and neodymium) and their naturally occuring isotopes.  
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 26, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 15, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Barium Isotopes
     ba_size = 7 

     barium_array(1)%z = 56
     barium_array(1)%a = 130
     barium_array(1)%symbol ="Ba-130"
     barium_array(1)%mass_defect = -87.2618
     barium_array(1)%atom_percent = 0.106
     barium_array(1)%zaid = "56130.80c"

     barium_array(2)%z = 56
     barium_array(2)%a = 132
     barium_array(2)%symbol ="Ba-132"
     barium_array(2)%mass_defect = -88.4349
     barium_array(2)%atom_percent = 0.101
     barium_array(2)%zaid = "56132.80c"

     barium_array(3)%z = 56
     barium_array(3)%a = 134
     barium_array(3)%symbol ="Ba-134"
     barium_array(3)%mass_defect = -88.9501
     barium_array(3)%atom_percent = 2.417
     barium_array(3)%zaid = "56134.80c"

     barium_array(4)%z = 56
     barium_array(4)%a = 135
     barium_array(4)%symbol ="Ba-135"
     barium_array(4)%mass_defect = -87.8508
     barium_array(4)%atom_percent = 6.592
     barium_array(4)%zaid = "56135.80c"

     barium_array(5)%z = 56
     barium_array(5)%a = 136
     barium_array(5)%symbol ="Ba-136"
     barium_array(5)%mass_defect = -88.8872
     barium_array(5)%atom_percent = 7.854
     barium_array(5)%zaid = "56136.80c"

     barium_array(6)%z = 56
     barium_array(6)%a = 137
     barium_array(6)%symbol ="Ba-137"
     barium_array(6)%mass_defect = -87.7215
     barium_array(6)%atom_percent = 11.232
     barium_array(6)%zaid = "56137.80c"

     barium_array(7)%z = 56
     barium_array(7)%a = 138
     barium_array(7)%symbol ="Ba-138"
     barium_array(7)%mass_defect = -88.2619
     barium_array(7)%atom_percent = 71.698
     barium_array(7)%zaid = "56138.80c"

     !Calculate isotopic mass for Barium
     DO i=1,ba_size
          holder = barium_array(i)%a + (barium_array(i)%mass_defect/931.494)

          barium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ba_mass = 0.0
     holder = 0.0

     !Calculate mass of Barium
     DO i=1, ba_size
          ba_mass = holder + (barium_array(i)%isotopic_mass* &
          & barium_array(i)%atom_percent)/100

          holder = ba_mass
     END DO
     
     !Calculate weight percent of Barium
     DO i=1, ba_size
          barium_array(i)%weight_percent = (barium_array(i)%isotopic_mass &
          &   * barium_array(i)%atom_percent)/ba_mass
     END DO

     !Number of Lanthanum Isotopes
     la_size = 2

     lanthanum_array(1)%z = 57
     lanthanum_array(1)%a = 138
     lanthanum_array(1)%symbol ="La-138"
     lanthanum_array(1)%mass_defect = -86.5215
     lanthanum_array(1)%atom_percent = 0.08881
     lanthanum_array(1)%zaid = "57138.80c"

     lanthanum_array(2)%z = 57
     lanthanum_array(2)%a = 139
     lanthanum_array(2)%symbol ="La-139"
     lanthanum_array(2)%mass_defect = -87.2282
     lanthanum_array(2)%atom_percent = 99.9119
     lanthanum_array(2)%zaid = "57139.80c"

     !Calculate isotopic mass for Lanthanum
     DO i=1,la_size
          holder = lanthanum_array(i)%a + (lanthanum_array(i)%mass_defect/931.494)

          lanthanum_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     la_mass = 0.0
     holder = 0.0

     !Calculate mass of Lanthanum
     DO i=1, la_size
          la_mass = holder + (lanthanum_array(i)%isotopic_mass* &
          &  lanthanum_array(i)%atom_percent)/100

          holder = la_mass
     END DO

     !Calculate weight percent of Lanthanum
     DO i=1, la_size
          lanthanum_array(i)%weight_percent = (lanthanum_array(i)%isotopic_mass &
          &   * lanthanum_array(i)%atom_percent)/la_mass
     END DO

     !Number of Cerium Isotopes
     ce_size = 4

     cerium_array(1)%z = 58
     cerium_array(1)%a = 136
     cerium_array(1)%symbol ="Ce-136"
     cerium_array(1)%mass_defect = -86.4736
     cerium_array(1)%atom_percent = 0.185
     cerium_array(1)%zaid = "58136.80c"

     cerium_array(2)%z = 58
     cerium_array(2)%a = 138
     cerium_array(2)%symbol ="Ce-138"
     cerium_array(2)%mass_defect = -87.5643
     cerium_array(2)%atom_percent = 0.251
     cerium_array(2)%zaid = "58138.80c"

     cerium_array(3)%z = 58
     cerium_array(3)%a = 140
     cerium_array(3)%symbol ="Ce-140"
     cerium_array(3)%mass_defect = -88.0786
     cerium_array(3)%atom_percent = 88.450
     cerium_array(3)%zaid = "58140.80c"

     cerium_array(4)%z = 58
     cerium_array(4)%a = 142
     cerium_array(4)%symbol ="Ce-142"
     cerium_array(4)%mass_defect = -84.5320
     cerium_array(4)%atom_percent = 11.114
     cerium_array(4)%zaid = "58142.80c"

     !Calculate isotopic mass for Cerium
     DO i=1,ce_size
          holder = cerium_array(i)%a + (cerium_array(i)%mass_defect/931.494)

          cerium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ce_mass = 0.0
     holder = 0.0

     !Calculate mass of Cerium
     DO i=1, ce_size
          ce_mass = holder + (cerium_array(i)%isotopic_mass* &
          &  cerium_array(i)%atom_percent)/100

          holder = ce_mass
     END DO
     
     !Calculate weight percent of Cerium
     DO i=1, ce_size
          cerium_array(i)%weight_percent = (cerium_array(i)%isotopic_mass &
          &   * cerium_array(i)%atom_percent)/ce_mass
     END DO

     !Number of Praseodymium Isotopes
     pr_size = 1

     praseodymium_array(1)%z = 59
     praseodymium_array(1)%a = 141
     praseodymium_array(1)%symbol ="Pr-141"
     praseodymium_array(1)%mass_defect = -86.0158
     praseodymium_array(1)%atom_percent = 100.0
     praseodymium_array(1)%zaid = "59141.80c"

     !Calculate isotopic mass for Praseodymium
     DO i=1, pr_size
          holder = praseodymium_array(i)%a + (praseodymium_array(i)%mass_defect/931.494)

          praseodymium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     pr_mass = 0.0
     holder = 0.0

     !Calculate mass of Praseodymium
     DO i=1, pr_size
          pr_mass = holder + (praseodymium_array(i)%isotopic_mass* &
          &  praseodymium_array(i)%atom_percent)/100

          holder = pr_mass
     END DO
     
     !Calculate weight percent of Praseodymium
     DO i=1, pr_size
          praseodymium_array(i)%weight_percent = (praseodymium_array(i)%isotopic_mass &
          &   * praseodymium_array(i)%atom_percent)/pr_mass
     END DO

     !Number of Neodymium Isotopes
     nd_size = 7 

     neodymium_array(1)%z = 60
     neodymium_array(1)%a = 142
     neodymium_array(1)%symbol ="Nd-142"
     neodymium_array(1)%mass_defect = -85.9493
     neodymium_array(1)%atom_percent = 27.152
     neodymium_array(1)%zaid = "60142.80c"

     neodymium_array(2)%z = 60
     neodymium_array(2)%a = 143
     neodymium_array(2)%symbol ="Nd-143"
     neodymium_array(2)%mass_defect = -84.0015
     neodymium_array(2)%atom_percent = 12.174
     neodymium_array(2)%zaid = "60143.80c"

     neodymium_array(3)%z = 60
     neodymium_array(3)%a = 144
     neodymium_array(3)%symbol ="Nd-144"
     neodymium_array(3)%mass_defect = -83.7473
     neodymium_array(3)%atom_percent = 23.798
     neodymium_array(3)%zaid = "60144.80c"

     neodymium_array(4)%z = 60
     neodymium_array(4)%a = 145
     neodymium_array(4)%symbol ="Nd-145"
     neodymium_array(4)%mass_defect = -81.4312
     neodymium_array(4)%atom_percent = 8.293
     neodymium_array(4)%zaid = "60145.80c"

     neodymium_array(5)%z = 60
     neodymium_array(5)%a = 146
     neodymium_array(5)%symbol ="Nd-146"
     neodymium_array(5)%mass_defect = -80.9252
     neodymium_array(5)%atom_percent = 17.189
     neodymium_array(5)%zaid = "60146.80c"

     neodymium_array(6)%z = 60
     neodymium_array(6)%a = 148
     neodymium_array(6)%symbol ="Nd-148"
     neodymium_array(6)%mass_defect = -77.4068
     neodymium_array(6)%atom_percent = 5.756
     neodymium_array(6)%zaid = "60148.80c"

     neodymium_array(7)%z = 60
     neodymium_array(7)%a = 150
     neodymium_array(7)%symbol ="Nd-150"
     neodymium_array(7)%mass_defect = -73.6832
     neodymium_array(7)%atom_percent = 5.638
     neodymium_array(7)%zaid = "60150.80c"

     !Calculate isotopic mass for Neodymium
     DO i=1,nd_size
          holder = neodymium_array(i)%a + (neodymium_array(i)%mass_defect/931.494)

          neodymium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     nd_mass = 0.0
     holder = 0.0

     !Calculate mass of Neodymium
     DO i=1, nd_size
          nd_mass = holder + (neodymium_array(i)%isotopic_mass* &
          &  neodymium_array(i)%atom_percent)/100

          holder = nd_mass
     END DO
     
     !Calculate weight percent of Neodymium
     DO i=1, nd_size
          neodymium_array(i)%weight_percent = (neodymium_array(i)%isotopic_mass &
          &   * neodymium_array(i)%atom_percent)/nd_mass
     END DO

   END SUBROUTINE Z56_Z60
