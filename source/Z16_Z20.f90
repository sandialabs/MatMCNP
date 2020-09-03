!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z16_Z20
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 16-20 (sulfur,chlorine,argon,potassium,and
     !calcium) and their naturally occuring isotopes. 
     !Created with the Current Nuclear Wallet Cards (2004)
     !on May 25, 2004 by Karen Kajder.
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

     !Local variables 
     REAL::holder, mass_of_element
     INTEGER:: i

     !Number of Sulfur Isotopes
     s_size = 4

     sulfur_array(1)%z = 16
     sulfur_array(1)%a = 32
     sulfur_array(1)%symbol =" S-32 "
     sulfur_array(1)%mass_defect = -26.0155
     sulfur_array(1)%atom_percent = 94.99
     sulfur_array(1)%zaid = "16032.00c"

     sulfur_array(2)%z = 16
     sulfur_array(2)%a = 33
     sulfur_array(2)%symbol =" S-33 "
     sulfur_array(2)%mass_defect = -26.5858
     sulfur_array(2)%atom_percent = 0.75
     sulfur_array(2)%zaid = "16033.00c"

     sulfur_array(3)%z = 16
     sulfur_array(3)%a = 34
     sulfur_array(3)%symbol =" S-34 "
     sulfur_array(3)%mass_defect = -29.9316
     sulfur_array(3)%atom_percent = 4.25
     sulfur_array(3)%zaid = "16034.00c"

     sulfur_array(4)%z = 16
     sulfur_array(4)%a = 36
     sulfur_array(4)%symbol =" S-36 "
     sulfur_array(4)%mass_defect = -30.6641
     sulfur_array(4)%atom_percent = 0.01
     sulfur_array(4)%zaid = "16036.00c"

     !Calculate isotopic mass for Sulfur
     DO i=1,s_size
          holder = sulfur_array(i)%a + (sulfur_array(i)%mass_defect/MeV_amu)

          sulfur_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     s_mass = 0.0
     holder = 0.0

     !Calculate mass of Sulfur
     DO i=1, s_size
          s_mass = holder + (sulfur_array(i)%isotopic_mass*		&
          &	sulfur_array(i)%atom_percent)/100

          holder = s_mass
     END DO
      
     !Calculate weight percent of Sulfur
     DO i=1, s_size
          sulfur_array(i)%weight_percent = (sulfur_array(i)%isotopic_mass &
          &   * sulfur_array(i)%atom_percent)/s_mass
     END DO

     !Number of Chlorine Isotopes
     cl_size = 2

     chlorine_array(1)%z = 17
     chlorine_array(1)%a = 35
     chlorine_array(1)%symbol ="Cl-35 "
     chlorine_array(1)%mass_defect = -29.0135
     chlorine_array(1)%atom_percent = 75.76
     chlorine_array(1)%zaid = "17035.00c"

     chlorine_array(2)%z = 17
     chlorine_array(2)%a = 37
     chlorine_array(2)%symbol ="Cl-37 "
     chlorine_array(2)%mass_defect = -31.7615
     chlorine_array(2)%atom_percent = 24.24
     chlorine_array(2)%zaid = "17037.00c"

     !Calculate isotopic mass for Chlorine
     DO i=1,cl_size
          holder = chlorine_array(i)%a + (chlorine_array(i)%mass_defect/MeV_amu)

          chlorine_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     cl_mass = 0.0
     holder = 0.0

     !Calculate mass of Chlorine
     DO i=1, cl_size
          cl_mass = holder + (chlorine_array(i)%isotopic_mass*		&
          &	chlorine_array(i)%atom_percent)/100

          holder = cl_mass
     END DO
      
     !Calculate weight percent of Chlorine
     DO i=1, cl_size
          chlorine_array(i)%weight_percent = (chlorine_array(i)%isotopic_mass &
          &   * chlorine_array(i)%atom_percent)/cl_mass
     END DO

     !Number of Argon Isotopes
     ar_size = 3

     argon_array(1)%z = 18
     argon_array(1)%a = 36
     argon_array(1)%symbol ="Ar-36 "
     argon_array(1)%mass_defect = -30.2315
     argon_array(1)%atom_percent = 0.3336
     argon_array(1)%zaid = "18036.00c"

     argon_array(2)%z = 18
     argon_array(2)%a = 38
     argon_array(2)%symbol ="Ar-38 "
     argon_array(2)%mass_defect = -34.7148
     argon_array(2)%atom_percent = 0.0629
     argon_array(2)%zaid = "18038.00c"

     argon_array(3)%z = 18
     argon_array(3)%a = 40
     argon_array(3)%symbol ="Ar-40 "
     argon_array(3)%mass_defect = -35.0398
     argon_array(3)%atom_percent = 99.6035
     argon_array(3)%zaid = "18040.00c"

     !Calculate isotopic mass for Argon
     DO i=1,ar_size
          holder = argon_array(i)%a + (argon_array(i)%mass_defect/MeV_amu)

          argon_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.

     ar_mass = 0.0
     holder = 0.0

     !Calculate mass of Argon
     DO i=1, ar_size
          ar_mass = holder + (argon_array(i)%isotopic_mass*		&
          &	argon_array(i)%atom_percent)/100

          holder = ar_mass
     END DO
      
     !Calculate weight percent of Argon
     DO i=1, ar_size
          argon_array(i)%weight_percent = (argon_array(i)%isotopic_mass &
          &   * argon_array(i)%atom_percent)/ar_mass
     END DO

     !Number of Potassium Isotopes
     k_size = 3

     potassium_array(1)%z = 19
     potassium_array(1)%a = 39
     potassium_array(1)%symbol =" K-39 "
     potassium_array(1)%mass_defect = -33.8071
     potassium_array(1)%atom_percent = 93.2581
     potassium_array(1)%zaid = "19039.00c"

     potassium_array(2)%z = 19
     potassium_array(2)%a = 40
     potassium_array(2)%symbol =" K-40 "
     potassium_array(2)%mass_defect = -33.5354
     potassium_array(2)%atom_percent = 0.0117
     potassium_array(2)%zaid = "19040.00c"

     potassium_array(3)%z = 19
     potassium_array(3)%a = 41
     potassium_array(3)%symbol =" K-41 "
     potassium_array(3)%mass_defect = -35.5595
     potassium_array(3)%atom_percent = 6.7302
     potassium_array(3)%zaid = "19041.00c"

     !Calculate isotopic mass for Potassium
     DO i=1,k_size
          holder = potassium_array(i)%a + (potassium_array(i)%mass_defect/MeV_amu)

          potassium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     k_mass = 0.0
     holder = 0.0

     !Calculate mass of Potassium
     DO i=1, k_size
          k_mass = holder + (potassium_array(i)%isotopic_mass*		&
          &	potassium_array(i)%atom_percent)/100

          holder = k_mass
     END DO
      
     !Calculate weight percent of Potassium
     DO i=1, k_size
          potassium_array(i)%weight_percent = (potassium_array(i)%isotopic_mass &
          &   * potassium_array(i)%atom_percent)/k_mass
     END DO

     !Number of Calcium Isotopes
     ca_size = 6

     calcium_array(1)%z = 20
     calcium_array(1)%a = 40
     calcium_array(1)%symbol ="Ca-40 "
     calcium_array(1)%mass_defect = -34.8463
     calcium_array(1)%atom_percent = 96.94
     calcium_array(1)%zaid = "20040.00c"

     calcium_array(2)%z = 20
     calcium_array(2)%a = 42
     calcium_array(2)%symbol ="Ca-42 "
     calcium_array(2)%mass_defect = -38.5472
     calcium_array(2)%atom_percent = 0.647
     calcium_array(2)%zaid = "20042.00c"

     calcium_array(3)%z = 20
     calcium_array(3)%a = 43
     calcium_array(3)%symbol ="Ca-43 "
     calcium_array(3)%mass_defect = -38.4088
     calcium_array(3)%atom_percent = 0.135
     calcium_array(3)%zaid = "20043.00c"

     calcium_array(4)%z = 20
     calcium_array(4)%a = 44
     calcium_array(4)%symbol ="Ca-44 "
     calcium_array(4)%mass_defect = -41.4686
     calcium_array(4)%atom_percent = 2.09
     calcium_array(4)%zaid = "20044.00c"

     calcium_array(5)%z = 20
     calcium_array(5)%a = 46
     calcium_array(5)%symbol ="Ca-46 "
     calcium_array(5)%mass_defect = -43.1393
     calcium_array(5)%atom_percent = 0.004
     calcium_array(5)%zaid = "20046.00c"

     calcium_array(6)%z = 20
     calcium_array(6)%a = 48
     calcium_array(6)%symbol ="Ca-48 "
     calcium_array(6)%mass_defect = -44.2246
     calcium_array(6)%atom_percent = 0.187
     calcium_array(6)%zaid = "20048.00c"

     !Calculate isotopic mass for Calcium
     DO i=1,ca_size
          holder = calcium_array(i)%a + (calcium_array(i)%mass_defect/MeV_amu)

          calcium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ca_mass = 0.0
     holder = 0.0

     !Calculate mass of Calcium
     DO i=1, ca_size
          ca_mass = holder + (calcium_array(i)%isotopic_mass*		&
          &	calcium_array(i)%atom_percent)/100

          holder = ca_mass
     END DO
      
     !Calculate weight percent of Calcium
     DO i=1, ca_size
          calcium_array(i)%weight_percent = (calcium_array(i)%isotopic_mass &
          &   * calcium_array(i)%atom_percent)/ca_mass
     END DO

   END SUBROUTINE Z16_Z20
