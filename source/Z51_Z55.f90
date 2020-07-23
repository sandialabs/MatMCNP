!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z51_Z55
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 51-55 (antimony, tellurium, iodine, 
     !xenon, and cesium) and their naturally occuring isotopes.  
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

     !Number of Antimony Isotopes
     sb_size = 2

     antimony_array(1)%z = 51
     antimony_array(1)%a = 121
     antimony_array(1)%symbol ="Sb-121"
     antimony_array(1)%mass_defect = -89.6003
     antimony_array(1)%atom_percent = 57.21
     antimony_array(1)%zaid = "51121.00c"

     antimony_array(2)%z = 51
     antimony_array(2)%a = 123
     antimony_array(2)%symbol ="Sb-123"
     antimony_array(2)%mass_defect = -89.2240
     antimony_array(2)%atom_percent = 42.79
     antimony_array(2)%zaid = "51123.00c"

     !Calculate isotopic mass for Antimony
     DO i=1,sb_size
          holder = antimony_array(i)%a + (antimony_array(i)%mass_defect/MeV_amu)

          antimony_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     sb_mass = 0.0
     holder = 0.0

     !Calculate mass of Antimony
     DO i=1, sb_size
          sb_mass = holder + (antimony_array(i)%isotopic_mass* &
          & antimony_array(i)%atom_percent)/100

          holder = sb_mass
     END DO

     !Calculate weight percent of Antimony
     DO i=1, sb_size
          antimony_array(i)%weight_percent = (antimony_array(i)%isotopic_mass &
          &   * antimony_array(i)%atom_percent)/sb_mass
     END DO

     !Number of Tellurium Isotopes
     te_size = 8

     tellurium_array(1)%z = 52
     tellurium_array(1)%a = 120
     tellurium_array(1)%symbol ="Te-120"
     tellurium_array(1)%mass_defect = -89.3679
     tellurium_array(1)%atom_percent = 0.09
     tellurium_array(1)%zaid = "52120.00c"

     tellurium_array(2)%z = 52
     tellurium_array(2)%a = 122
     tellurium_array(2)%symbol ="Te-122"
     tellurium_array(2)%mass_defect = -90.3144
     tellurium_array(2)%atom_percent = 2.55
     tellurium_array(2)%zaid = "52122.00c"

     tellurium_array(3)%z = 52
     tellurium_array(3)%a = 123
     tellurium_array(3)%symbol ="Te-123"
     tellurium_array(3)%mass_defect = -89.172
     tellurium_array(3)%atom_percent = 0.89
     tellurium_array(3)%zaid = "52123.00c"

     tellurium_array(4)%z = 52
     tellurium_array(4)%a = 124
     tellurium_array(4)%symbol ="Te-124"
     tellurium_array(4)%mass_defect = -90.5253
     tellurium_array(4)%atom_percent = 4.74
     tellurium_array(4)%zaid = "52124.00c"

     tellurium_array(5)%z = 52
     tellurium_array(5)%a = 125
     tellurium_array(5)%symbol ="Te-125"
     tellurium_array(5)%mass_defect = -89.0229
     tellurium_array(5)%atom_percent = 7.07
     tellurium_array(5)%zaid = "52125.00c"

     tellurium_array(6)%z = 52
     tellurium_array(6)%a = 126
     tellurium_array(6)%symbol ="Te-126"
     tellurium_array(6)%mass_defect = -90.0653
     tellurium_array(6)%atom_percent = 18.84
     tellurium_array(6)%zaid = "52126.00c"

     tellurium_array(7)%z = 52
     tellurium_array(7)%a = 128
     tellurium_array(7)%symbol ="Te-128"
     tellurium_array(7)%mass_defect = -88.9937
     tellurium_array(7)%atom_percent = 31.74
     tellurium_array(7)%zaid = "52128.00c"

     tellurium_array(8)%z = 52
     tellurium_array(8)%a = 130
     tellurium_array(8)%symbol ="Te-130"
     tellurium_array(8)%mass_defect = -87.3529
     tellurium_array(8)%atom_percent = 34.08
     tellurium_array(8)%zaid = "52130.00c"

     !Calculate isotopic mass for Tellurium
     DO i=1,te_size
          holder = tellurium_array(i)%a + (tellurium_array(i)%mass_defect/MeV_amu)

          tellurium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     te_mass = 0.0
     holder = 0.0

     !Calculate mass of Tellurium
     DO i=1, te_size
          te_mass = holder + (tellurium_array(i)%isotopic_mass* &
          &  tellurium_array(i)%atom_percent)/100

          holder = te_mass
     END DO

     !Calculate weight percent of Tellurium
     DO i=1, te_size
          tellurium_array(i)%weight_percent = (tellurium_array(i)%isotopic_mass &
          &   * tellurium_array(i)%atom_percent)/te_mass
     END DO

     !Number of Iodine Isotopes
     i_size = 1

     iodine_array(1)%z = 53
     iodine_array(1)%a = 127
     iodine_array(1)%symbol =" I-127"
     iodine_array(1)%mass_defect = -88.9839
     iodine_array(1)%atom_percent = 100.0
     iodine_array(1)%zaid = "53127.00c"

     !Calculate isotopic mass for Iodine
     DO i=1, i_size
          holder = iodine_array(i)%a + (iodine_array(i)%mass_defect/MeV_amu)

          iodine_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     i_mass = 0.0
     holder = 0.0

     !Calculate mass of Iodine
     DO i=1, i_size
          i_mass = holder + (iodine_array(i)%isotopic_mass* &
          &  iodine_array(i)%atom_percent)/100

          holder = i_mass
     END DO
     
     !Calculate weight percent of Iodine
     DO i=1, i_size
          iodine_array(i)%weight_percent = (iodine_array(i)%isotopic_mass &
          &   * iodine_array(i)%atom_percent)/i_mass
     END DO

     !Number of Xenon Isotopes
     xe_size = 9

     xenon_array(1)%z = 54
     xenon_array(1)%a = 124
     xenon_array(1)%symbol ="Xe-124"
     xenon_array(1)%mass_defect = -87.6614
     xenon_array(1)%atom_percent = 0.0952
     xenon_array(1)%zaid = "54124.00c"

     xenon_array(2)%z = 54
     xenon_array(2)%a = 126
     xenon_array(2)%symbol ="Xe-126"
     xenon_array(2)%mass_defect = -89.1469
     xenon_array(2)%atom_percent = 0.0890
     xenon_array(2)%zaid = "54126.00c"

     xenon_array(3)%z = 54
     xenon_array(3)%a = 128
     xenon_array(3)%symbol ="Xe-128"
     xenon_array(3)%mass_defect = -89.8602
     xenon_array(3)%atom_percent = 1.9102
     xenon_array(3)%zaid = "54128.00c"

     xenon_array(4)%z = 54
     xenon_array(4)%a = 129
     xenon_array(4)%symbol ="Xe-129"
     xenon_array(4)%mass_defect = -88.6960
     xenon_array(4)%atom_percent = 26.4006
     xenon_array(4)%zaid = "54129.00c"

     xenon_array(5)%z = 54
     xenon_array(5)%a = 130
     xenon_array(5)%symbol ="Xe-130"
     xenon_array(5)%mass_defect = -89.8804
     xenon_array(5)%atom_percent = 4.0710
     xenon_array(5)%zaid = "54130.00c"

     xenon_array(6)%z = 54
     xenon_array(6)%a = 131
     xenon_array(6)%symbol ="Xe-131"
     xenon_array(6)%mass_defect = -88.4135
     xenon_array(6)%atom_percent = 21.232 
     xenon_array(6)%zaid = "54131.00c"

     xenon_array(7)%z = 54
     xenon_array(7)%a = 132
     xenon_array(7)%symbol ="Xe-132"
     xenon_array(7)%mass_defect = -89.2789
     xenon_array(7)%atom_percent = 26.9086
     xenon_array(7)%zaid = "54132.00c"

     xenon_array(8)%z = 54
     xenon_array(8)%a = 134
     xenon_array(8)%symbol ="Xe-134"
     xenon_array(8)%mass_defect = -88.1258
     xenon_array(8)%atom_percent = 10.4357
     xenon_array(8)%zaid = "54134.00c"

     xenon_array(9)%z = 54
     xenon_array(9)%a = 136
     xenon_array(9)%symbol ="Xe-136"
     xenon_array(9)%mass_defect = -86.4291
     xenon_array(9)%atom_percent = 8.8573
     xenon_array(9)%zaid = "54136.00c"

     !Calculate isotopic mass for Xenon
     DO i=1,xe_size
          holder = xenon_array(i)%a + (xenon_array(i)%mass_defect/MeV_amu)

          xenon_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     xe_mass = 0.0
     holder = 0.0

     !Calculate mass of Xenon
     DO i=1, xe_size
          xe_mass = holder + (xenon_array(i)%isotopic_mass* &
          & xenon_array(i)%atom_percent)/100

          holder = xe_mass
     END DO
     
     !Calculate weight percent of Xenon
     DO i=1, xe_size
          xenon_array(i)%weight_percent = (xenon_array(i)%isotopic_mass &
          &   * xenon_array(i)%atom_percent)/xe_mass
     END DO

     !Number of Cesium Isotopes
     cs_size = 1

     cesium_array(1)%z = 55
     cesium_array(1)%a = 133
     cesium_array(1)%symbol ="Ce-133"
     cesium_array(1)%mass_defect = -88.0709
     cesium_array(1)%atom_percent = 100.0
     cesium_array(1)%zaid = "55133.00c"

     !Calculate isotopic mass for Cesium
     DO i=1, cs_size
          holder = cesium_array(i)%a + (cesium_array(i)%mass_defect/MeV_amu)

          cesium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     cs_mass = 0.0
     holder = 0.0

     !Calculate mass of Cesium
     DO i=1, cs_size
          cs_mass = holder + (cesium_array(i)%isotopic_mass* &
          &  cesium_array(i)%atom_percent)/100

          holder = cs_mass
     END DO

     !Calculate weight percent of Cesium
     DO i=1, cs_size
          cesium_array(i)%weight_percent = (cesium_array(i)%isotopic_mass &
          &   * cesium_array(i)%atom_percent)/cs_mass
     END DO

   END SUBROUTINE Z51_Z55
