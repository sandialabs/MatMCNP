!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z71_Z75
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 71-75 (lutetium, hafnium, tantalum,
     !tungsten, and rhenium) and their naturally occuring isotopes.  
     !Created with the Current Nuclear Wallet Cards (2004)
     !on June 2, 2004 by Karen Kajder.
     !
     !Modified to latest NWC electronic version values and ENDF/B-VII cross 
     !sections by K. Russell DePriest on December 15, 2011.
     !
     ! - ENDF/B-VII Release 1 cross sections (that is, .80c) made the default
     !   on 05/09/2014 by K. Russell DePriest.
     ! - Ta-180 "fixed"
     ! - W-180 "fixed"

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i
     
     !Number of Lutetium Isotopes
     lu_size = 2

     lutetium_array(1)%z = 71
     lutetium_array(1)%a = 175
     lutetium_array(1)%symbol ="Lu-175"
     lutetium_array(1)%mass_defect = -55.1661
     lutetium_array(1)%atom_percent = 97.401
     lutetium_array(1)%zaid = "71175.80c"

     lutetium_array(2)%z = 71
     lutetium_array(2)%a = 176
     lutetium_array(2)%symbol ="Lu-176"
     lutetium_array(2)%mass_defect = -53.3828
     lutetium_array(2)%atom_percent = 2.599
     lutetium_array(2)%zaid = "71176.80c"

     !Calculate isotopic mass for Lutetium
     DO i=1, lu_size
          holder = lutetium_array(i)%a + (lutetium_array(i)%mass_defect/931.494)

          lutetium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     lu_mass = 0.0
     holder = 0.0

     !Calculate mass of Lutetium
     DO i=1, lu_size
          lu_mass = holder + (lutetium_array(i)%isotopic_mass* &
          &  lutetium_array(i)%atom_percent)/100

          holder = lu_mass
     END DO
     
     !Calculate weight percent of Lutetium
     DO i=1, lu_size
          lutetium_array(i)%weight_percent = (lutetium_array(i)%isotopic_mass &
          &   * lutetium_array(i)%atom_percent)/lu_mass
     END DO

     !Number of Hafnium Isotopes
     hf_size = 6 

     hafnium_array(1)%z = 72
     hafnium_array(1)%a = 174
     hafnium_array(1)%symbol ="Hf-174"
     hafnium_array(1)%mass_defect = -55.8455
     hafnium_array(1)%atom_percent = 0.16
     hafnium_array(1)%zaid = "72174.80c"

     hafnium_array(2)%z = 72
     hafnium_array(2)%a = 176
     hafnium_array(2)%symbol ="Hf-176"
     hafnium_array(2)%mass_defect = -54.5769
     hafnium_array(2)%atom_percent = 5.26
     hafnium_array(2)%zaid = "72176.80c"

     hafnium_array(3)%z = 72
     hafnium_array(3)%a = 177
     hafnium_array(3)%symbol ="Hf-177"
     hafnium_array(3)%mass_defect = -52.8850
     hafnium_array(3)%atom_percent = 18.60
     hafnium_array(3)%zaid = "72177.80c"

     hafnium_array(4)%z = 72
     hafnium_array(4)%a = 178
     hafnium_array(4)%symbol ="Hf-178"
     hafnium_array(4)%mass_defect = -52.4396
     hafnium_array(4)%atom_percent = 27.28
     hafnium_array(4)%zaid = "72178.80c"

     hafnium_array(5)%z = 72
     hafnium_array(5)%a = 179
     hafnium_array(5)%symbol ="Hf-179"
     hafnium_array(5)%mass_defect = -50.4673
     hafnium_array(5)%atom_percent = 13.62
     hafnium_array(5)%zaid = "72179.80c"

     hafnium_array(6)%z = 72
     hafnium_array(6)%a = 180
     hafnium_array(6)%symbol ="Hf-180"
     hafnium_array(6)%mass_defect = -49.7838
     hafnium_array(6)%atom_percent = 35.08
     hafnium_array(6)%zaid = "72180.80c"

     !Calculate isotopic mass for Hafnium
     DO i=1,hf_size
          holder = hafnium_array(i)%a + (hafnium_array(i)%mass_defect/931.494)

          hafnium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     hf_mass = 0.0
     holder = 0.0

     !Calculate mass of Hafnium
     DO i=1, hf_size
          hf_mass = holder + (hafnium_array(i)%isotopic_mass* &
          &  hafnium_array(i)%atom_percent)/100

          holder = hf_mass
     END DO
     
     !Calculate weight percent of Hafnium
     DO i=1, hf_size
          hafnium_array(i)%weight_percent = (hafnium_array(i)%isotopic_mass &
          &   * hafnium_array(i)%atom_percent)/hf_mass
     END DO

     !Number of Tantalum Isotopes
     ta_size = 2

     tantalum_array(1)%z = 73
     tantalum_array(1)%a = 180
     tantalum_array(1)%symbol ="Ta-180"
     tantalum_array(1)%mass_defect = -49.0136
     tantalum_array(1)%atom_percent = 0.01201
     tantalum_array(1)%zaid = "73180.80c"

     tantalum_array(2)%z = 73
     tantalum_array(2)%a = 181
     tantalum_array(2)%symbol ="Ta-181"
     tantalum_array(2)%mass_defect = -48.4419 
     tantalum_array(2)%atom_percent = 99.98799
     tantalum_array(2)%zaid = "73181.80c"

     !Calculate isotopic mass for Tantalum
     DO i=1, ta_size
          holder = tantalum_array(i)%a + (tantalum_array(i)%mass_defect/931.494)

          tantalum_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ta_mass = 0.0
     holder = 0.0

     !Calculate mass of Tantalum
     DO i=1, ta_size
          ta_mass = holder + (tantalum_array(i)%isotopic_mass* &
          &  tantalum_array(i)%atom_percent)/100

          holder = ta_mass
     END DO
     
     !Calculate weight percent of Tantalum
     DO i=1, ta_size
          tantalum_array(i)%weight_percent = (tantalum_array(i)%isotopic_mass &
          &   * tantalum_array(i)%atom_percent)/ta_mass
     END DO

     !Number of Tungsten Isotopes
     w_size = 5

     tungsten_array(1)%z = 74
     tungsten_array(1)%a = 180
     tungsten_array(1)%symbol =" W-180"
     tungsten_array(1)%mass_defect = -49.6365
     tungsten_array(1)%atom_percent = 0.12
     tungsten_array(1)%zaid = "74180.80c"

     tungsten_array(2)%z = 74
     tungsten_array(2)%a = 182
     tungsten_array(2)%symbol =" W-182"
     tungsten_array(2)%mass_defect = -48.2475
     tungsten_array(2)%atom_percent = 26.50
     tungsten_array(2)%zaid = "74182.80c"

     tungsten_array(3)%z = 74
     tungsten_array(3)%a = 183
     tungsten_array(3)%symbol =" W-183"
     tungsten_array(3)%mass_defect = -46.3671
     tungsten_array(3)%atom_percent = 14.31
     tungsten_array(3)%zaid = "74183.80c"

     tungsten_array(4)%z = 74
     tungsten_array(4)%a = 184
     tungsten_array(4)%symbol =" W-184"
     tungsten_array(4)%mass_defect = -45.7075
     tungsten_array(4)%atom_percent = 30.64
     tungsten_array(4)%zaid = "74184.80c"

     tungsten_array(5)%z = 74
     tungsten_array(5)%a = 186
     tungsten_array(5)%symbol =" W-186"
     tungsten_array(5)%mass_defect = -42.5109
     tungsten_array(5)%atom_percent = 28.43
     tungsten_array(5)%zaid = "74186.80c"

     !Calculate isotopic mass for Tungsten
     DO i=1,w_size
          holder = tungsten_array(i)%a + (tungsten_array(i)%mass_defect/931.494)

          tungsten_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     w_mass = 0.0
     holder = 0.0

     !Calculate mass of Tungsten
     DO i=1, w_size
          w_mass = holder + (tungsten_array(i)%isotopic_mass* &
          &  tungsten_array(i)%atom_percent)/100

          holder = w_mass
     END DO
     
     !Calculate weight percent of Tungsten
     DO i=1, w_size
          tungsten_array(i)%weight_percent = (tungsten_array(i)%isotopic_mass &
          &   * tungsten_array(i)%atom_percent)/w_mass
     END DO

     !Number of Rhenium Isotopes
     re_size = 2 

     rhenium_array(1)%z = 75
     rhenium_array(1)%a = 185
     rhenium_array(1)%symbol ="Re-185"
     rhenium_array(1)%mass_defect = -43.8225
     rhenium_array(1)%atom_percent = 37.40
     rhenium_array(1)%zaid = "75185.80c"

     rhenium_array(2)%z = 75
     rhenium_array(2)%a = 187
     rhenium_array(2)%symbol ="Re-187"
     rhenium_array(2)%mass_defect = -41.2184
     rhenium_array(2)%atom_percent = 62.60
     rhenium_array(2)%zaid = "75187.80c"

     !Calculate isotopic mass for Rhenium
     DO i=1,re_size
          holder = rhenium_array(i)%a + (rhenium_array(i)%mass_defect/931.494)

          rhenium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     re_mass = 0.0
     holder = 0.0

     !Calculate mass of Rhenium
     DO i=1, re_size
          re_mass = holder + (rhenium_array(i)%isotopic_mass* &
          &  rhenium_array(i)%atom_percent)/100

          holder = re_mass
     END DO
     
     !Calculate weight percent of Rhenium
     DO i=1, re_size
          rhenium_array(i)%weight_percent = (rhenium_array(i)%isotopic_mass &
          &   * rhenium_array(i)%atom_percent)/re_mass
     END DO

   END SUBROUTINE Z71_Z75
