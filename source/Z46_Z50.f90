!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z46_Z50
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 46-50 (palladium, silver, cadmium,
     !indium, and tin) and their naturally occuring isotopes.  
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

     !Number of Palladium Isotopes
     pd_size = 6

     palladium_array(1)%z = 46
     palladium_array(1)%a = 102
     palladium_array(1)%symbol ="Pd-102"
     palladium_array(1)%mass_defect = -87.9031
     palladium_array(1)%atom_percent = 1.02
     palladium_array(1)%zaid = "46102.00c"

     palladium_array(2)%z = 46
     palladium_array(2)%a = 104
     palladium_array(2)%symbol ="Pd-104"
     palladium_array(2)%mass_defect = -89.3951
     palladium_array(2)%atom_percent = 11.14
     palladium_array(2)%zaid = "46104.00c"

     palladium_array(3)%z = 46
     palladium_array(3)%a = 105
     palladium_array(3)%symbol ="Pd-105"
     palladium_array(3)%mass_defect = -88.4178
     palladium_array(3)%atom_percent = 22.33
     palladium_array(3)%zaid = "46105.00c"

     palladium_array(4)%z = 46
     palladium_array(4)%a = 106
     palladium_array(4)%symbol ="Pd-106"
     palladium_array(4)%mass_defect = -89.9075
     palladium_array(4)%atom_percent = 27.33
     palladium_array(4)%zaid = "46106.00c"

     palladium_array(5)%z = 46
     palladium_array(5)%a = 108
     palladium_array(5)%symbol ="Pd-108"
     palladium_array(5)%mass_defect = -89.5242
     palladium_array(5)%atom_percent = 26.46
     palladium_array(5)%zaid = "46108.00c"

     palladium_array(6)%z = 46
     palladium_array(6)%a = 110
     palladium_array(6)%symbol ="Pd-110"
     palladium_array(6)%mass_defect = -88.3309
     palladium_array(6)%atom_percent = 11.72
     palladium_array(6)%zaid = "46110.00c"

     !Calculate isotopic mass for Palladium
     DO i=1,pd_size
          holder = palladium_array(i)%a + (palladium_array(i)%mass_defect/MeV_amu)

          palladium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     pd_mass = 0.0
     holder = 0.0

     !Calculate mass of Palladium
     DO i=1, pd_size
          pd_mass = holder + (palladium_array(i)%isotopic_mass* &
          & palladium_array(i)%atom_percent)/100

          holder = pd_mass
     END DO
      
     !Calculate weight percent of Palladium
     DO i=1, pd_size
          palladium_array(i)%weight_percent = (palladium_array(i)%isotopic_mass &
          &   * palladium_array(i)%atom_percent)/pd_mass
     END DO

     !Number of Silver Isotopes
     ag_size = 2

     silver_array(1)%z = 47
     silver_array(1)%a = 107
     silver_array(1)%symbol ="Ag-107"
     silver_array(1)%mass_defect = -88.4066
     silver_array(1)%atom_percent = 51.839
     silver_array(1)%zaid = "47107.00c"

     silver_array(2)%z = 47
     silver_array(2)%a = 109
     silver_array(2)%symbol ="Ag-109"
     silver_array(2)%mass_defect = -88.7194
     silver_array(2)%atom_percent = 48.161
     silver_array(2)%zaid = "47109.00c"

     !Calculate isotopic mass for Silver
     DO i=1,ag_size
          holder = silver_array(i)%a + (silver_array(i)%mass_defect/MeV_amu)

          silver_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ag_mass = 0.0
     holder = 0.0

     !Calculate mass of Silver
     DO i=1, ag_size
          ag_mass = holder + (silver_array(i)%isotopic_mass* &
          & silver_array(i)%atom_percent)/100

          holder = ag_mass
     END DO

     !Calculate weight percent of Silver
     DO i=1, ag_size
          silver_array(i)%weight_percent = (silver_array(i)%isotopic_mass &
          &   * silver_array(i)%atom_percent)/ag_mass
     END DO

     !Number of Cadmium Isotopes
     cd_size = 8

     cadmium_array(1)%z = 48
     cadmium_array(1)%a = 106
     cadmium_array(1)%symbol ="Cd-106"
     cadmium_array(1)%mass_defect = -87.1321
     cadmium_array(1)%atom_percent = 1.25
     cadmium_array(1)%zaid = "48106.00c"

     cadmium_array(2)%z = 48
     cadmium_array(2)%a = 108
     cadmium_array(2)%symbol ="Cd-108"
     cadmium_array(2)%mass_defect = -89.2524
     cadmium_array(2)%atom_percent = 0.89
     cadmium_array(2)%zaid = "48108.00c"

     cadmium_array(3)%z = 48
     cadmium_array(3)%a = 110
     cadmium_array(3)%symbol ="Cd-110"
     cadmium_array(3)%mass_defect = -90.3479
     cadmium_array(3)%atom_percent = 12.49
     cadmium_array(3)%zaid = "48110.00c"

     cadmium_array(4)%z = 48
     cadmium_array(4)%a = 111
     cadmium_array(4)%symbol ="Cd-111"
     cadmium_array(4)%mass_defect = -89.2522
     cadmium_array(4)%atom_percent = 12.80
     cadmium_array(4)%zaid = "48111.00c"

     cadmium_array(5)%z = 48
     cadmium_array(5)%a = 112
     cadmium_array(5)%symbol ="Cd-112"
     cadmium_array(5)%mass_defect = -90.5748
     cadmium_array(5)%atom_percent = 24.13
     cadmium_array(5)%zaid = "48112.00c"

     cadmium_array(6)%z = 48
     cadmium_array(6)%a = 113
     cadmium_array(6)%symbol ="Cd-113"
     cadmium_array(6)%mass_defect = -89.0432
     cadmium_array(6)%atom_percent = 12.22
     cadmium_array(6)%zaid = "48113.00c"

     cadmium_array(7)%z = 48
     cadmium_array(7)%a = 114
     cadmium_array(7)%symbol ="Cd-114"
     cadmium_array(7)%mass_defect = -90.0149
     cadmium_array(7)%atom_percent = 28.73
     cadmium_array(7)%zaid = "48114.00c"

     cadmium_array(8)%z = 48
     cadmium_array(8)%a = 116
     cadmium_array(8)%symbol ="Cd-116"
     cadmium_array(8)%mass_defect = -88.7124 
     cadmium_array(8)%atom_percent = 7.49
     cadmium_array(8)%zaid = "48116.00c"

     !Calculate isotopic mass for Cadmium
     DO i=1,cd_size
          holder = cadmium_array(i)%a + (cadmium_array(i)%mass_defect/MeV_amu)

          cadmium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     cd_mass = 0.0
     holder = 0.0

     !Calculate mass of Cadmium
     DO i=1, cd_size
          cd_mass = holder + (cadmium_array(i)%isotopic_mass* &
          & cadmium_array(i)%atom_percent)/100

          holder = cd_mass
     END DO
     
     !Calculate weight percent of Cadmium
     DO i=1, cd_size
          cadmium_array(i)%weight_percent = (cadmium_array(i)%isotopic_mass &
          &   * cadmium_array(i)%atom_percent)/cd_mass
     END DO

     !Number of Indium Isotopes
     in_size = 2

     indium_array(1)%z = 49
     indium_array(1)%a = 113
     indium_array(1)%symbol ="In-113"
     indium_array(1)%mass_defect = -89.3671
     indium_array(1)%atom_percent = 4.29
     indium_array(1)%zaid = "49113.00c"

     indium_array(2)%z = 49
     indium_array(2)%a = 115
     indium_array(2)%symbol ="In-115"
     indium_array(2)%mass_defect = -89.5363
     indium_array(2)%atom_percent = 95.71
     indium_array(2)%zaid = "49115.00c"

     !Calculate isotopic mass for Indium
     DO i=1,in_size
          holder = indium_array(i)%a + (indium_array(i)%mass_defect/MeV_amu)

          indium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     in_mass = 0.0
     holder = 0.0

     !Calculate mass of Indium
     DO i=1, in_size
          in_mass = holder + (indium_array(i)%isotopic_mass* &
          &  indium_array(i)%atom_percent)/100

          holder = in_mass
     END DO

     !Calculate weight percent of Indium
     DO i=1, in_size
          indium_array(i)%weight_percent = (indium_array(i)%isotopic_mass &
          &  * indium_array(i)%atom_percent)/in_mass
     END DO

     !Number of Tin Isotopes
     sn_size = 10

     tin_array(1)%z = 50
     tin_array(1)%a = 112
     tin_array(1)%symbol ="Sn-112"
     tin_array(1)%mass_defect = -88.6550
     tin_array(1)%atom_percent = 0.97
     tin_array(1)%zaid = "50112.00c"

     tin_array(2)%z = 50
     tin_array(2)%a = 114
     tin_array(2)%symbol ="Sn-114"
     tin_array(2)%mass_defect = -90.5597
     tin_array(2)%atom_percent = 0.66
     tin_array(2)%zaid = "50114.00c"

     tin_array(3)%z = 50
     tin_array(3)%a = 115
     tin_array(3)%symbol ="Sn-115"
     tin_array(3)%mass_defect = -90.0338
     tin_array(3)%atom_percent = 0.34
     tin_array(3)%zaid = "50115.00c"

     tin_array(4)%z = 50
     tin_array(4)%a = 116
     tin_array(4)%symbol ="Sn-116"
     tin_array(4)%mass_defect = -91.5259
     tin_array(4)%atom_percent = 14.54
     tin_array(4)%zaid = "50116.00c"

     tin_array(5)%z = 50
     tin_array(5)%a = 117
     tin_array(5)%symbol ="Sn-117"
     tin_array(5)%mass_defect = -90.3977
     tin_array(5)%atom_percent = 7.68
     tin_array(5)%zaid = "50117.00c"

     tin_array(6)%z = 50
     tin_array(6)%a = 118
     tin_array(6)%symbol ="Sn-118"
     tin_array(6)%mass_defect = -91.6528
     tin_array(6)%atom_percent = 24.22
     tin_array(6)%zaid = "50118.00c"

     tin_array(7)%z = 50
     tin_array(7)%a = 119
     tin_array(7)%symbol ="Sn-119"
     tin_array(7)%mass_defect = -90.0650
     tin_array(7)%atom_percent = 8.59
     tin_array(7)%zaid = "50119.00c"

     tin_array(8)%z = 50
     tin_array(8)%a = 120
     tin_array(8)%symbol ="Sn-120"
     tin_array(8)%mass_defect = -91.0983
     tin_array(8)%atom_percent = 32.58
     tin_array(8)%zaid = "50120.00c"

     tin_array(9)%z = 50
     tin_array(9)%a = 122
     tin_array(9)%symbol ="Sn-122"
     tin_array(9)%mass_defect = -89.9413
     tin_array(9)%atom_percent = 4.63
     tin_array(9)%zaid = "50122.00c"

     tin_array(10)%z = 50
     tin_array(10)%a = 124
     tin_array(10)%symbol ="Sn-124"
     tin_array(10)%mass_defect = -88.2341
     tin_array(10)%atom_percent = 5.79
     tin_array(10)%zaid = "50124.00c"

     !Calculate isotopic mass for Tin
     DO i=1,sn_size
          holder = tin_array(i)%a + (tin_array(i)%mass_defect/MeV_amu)

          tin_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     sn_mass = 0.0
     holder = 0.0

     !Calculate mass of Tin
     DO i=1, sn_size
          sn_mass = holder + (tin_array(i)%isotopic_mass* &
          & tin_array(i)%atom_percent)/100

          holder = sn_mass
     END DO
 
     !Calculate weight percent of Tin
     DO i=1, sn_size
          tin_array(i)%weight_percent = (tin_array(i)%isotopic_mass &
          &   * tin_array(i)%atom_percent)/sn_mass
     END DO

   END SUBROUTINE Z46_Z50
