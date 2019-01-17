!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z81_Z85
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 81-85 (thallium, lead, bismuth,
     !polonium,and astatine) and their naturally occuring isotopes.  
     !Polonium and astatine do not occur naturally in nature and are 
     !therefore not included in the subroutine.
     !
     !Created with the Current Nuclear Wallet Cards (2004)
     !on June 2, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 15, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
     ! - New cross sections available for Tl

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Thallium Isotopes
     tl_size = 2

     thallium_array(1)%z = 81
     thallium_array(1)%a = 203
     thallium_array(1)%symbol ="Tl-203"
     thallium_array(1)%mass_defect = -25.7620
     thallium_array(1)%atom_percent = 29.524
     thallium_array(1)%zaid = "81203.80c"

     thallium_array(2)%z = 81
     thallium_array(2)%a = 205
     thallium_array(2)%symbol ="Tl-205"
     thallium_array(2)%mass_defect = -23.8215
     thallium_array(2)%atom_percent = 70.476
     thallium_array(2)%zaid = "81205.80c"

     !Calculate isotopic mass for Thallium
     DO i=1, tl_size
          holder = thallium_array(i)%a + (thallium_array(i)%mass_defect/931.494)

          thallium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     tl_mass = 0.0
     holder = 0.0

     !Calculate mass of Thallium
     DO i=1, tl_size
          tl_mass = holder + (thallium_array(i)%isotopic_mass* &
          &  thallium_array(i)%atom_percent)/100

          holder = tl_mass
     END DO
     
     !Calculate weight percent of Thallium
     DO i=1, tl_size
          thallium_array(i)%weight_percent = (thallium_array(i)%isotopic_mass &
          &   * thallium_array(i)%atom_percent)/tl_mass
     END DO 

     !Number of Lead Isotopes
     pb_size = 4

     lead_array(1)%z = 82
     lead_array(1)%a = 204
     lead_array(1)%symbol ="Pb-204"
     lead_array(1)%mass_defect = -25.1105
     lead_array(1)%atom_percent = 1.4
     lead_array(1)%zaid = "82204.80c"

     lead_array(2)%z = 82
     lead_array(2)%a = 206
     lead_array(2)%symbol ="Pb-206"
     lead_array(2)%mass_defect = -23.7862
     lead_array(2)%atom_percent = 24.1
     lead_array(2)%zaid = "82206.80c"

     lead_array(3)%z = 82
     lead_array(3)%a = 207
     lead_array(3)%symbol ="Pb-207"
     lead_array(3)%mass_defect = -22.4527
     lead_array(3)%atom_percent = 22.1
     lead_array(3)%zaid = "82207.80c"

     lead_array(4)%z = 82
     lead_array(4)%a = 208
     lead_array(4)%symbol ="Pb-208"
     lead_array(4)%mass_defect = -21.7492
     lead_array(4)%atom_percent = 52.4
     lead_array(4)%zaid = "82208.80c"

     !Calculate isotopic mass for Lead
     DO i=1,pb_size
          holder = lead_array(i)%a + (lead_array(i)%mass_defect/931.494)

          lead_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     pb_mass = 0.0
     holder = 0.0

     !Calculate mass of Lead
     DO i=1, pb_size
          pb_mass = holder + (lead_array(i)%isotopic_mass* &
          &  lead_array(i)%atom_percent)/100

          holder = pb_mass
     END DO
     
     !Calculate weight percent of Lead
     DO i=1, pb_size
          lead_array(i)%weight_percent = (lead_array(i)%isotopic_mass &
          &  * lead_array(i)%atom_percent)/pb_mass
     END DO

     !Number of Bismuth Isotopes
     bi_size = 1

     bismuth_array(1)%z = 83
     bismuth_array(1)%a = 209
     bismuth_array(1)%symbol ="Bi-209"
     bismuth_array(1)%mass_defect = -18.2593
     bismuth_array(1)%atom_percent = 100.0
     bismuth_array(1)%zaid = "83209.80c"

     !Calculate isotopic mass for Bismuth
     DO i=1, bi_size
          holder = bismuth_array(i)%a + (bismuth_array(i)%mass_defect/931.494)

          bismuth_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     bi_mass = 0.0
     holder = 0.0

     !Calculate mass of Bismuth
     DO i=1, bi_size
          bi_mass = holder + (bismuth_array(i)%isotopic_mass* &
          &  bismuth_array(i)%atom_percent)/100

          holder = bi_mass
     END DO
     
     !Calculate weight percent of Bismuth
     DO i=1, bi_size
          bismuth_array(i)%weight_percent = (bismuth_array(i)%isotopic_mass &
          &   * bismuth_array(i)%atom_percent)/bi_mass
     END DO 

  END SUBROUTINE Z81_Z85
