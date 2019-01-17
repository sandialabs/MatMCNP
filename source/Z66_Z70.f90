!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE Z66_Z70
     USE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !This subroutine contains elements 66-70 (dysprosium,holmium,erbium,thulium,and
     !ytterbium) and their naturally occuring isotopes.  
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

     !Number of Dysprosium Isotopes
     dy_size = 7 

     dysprosium_array(1)%z = 66
     dysprosium_array(1)%a = 156
     dysprosium_array(1)%symbol ="Dy-156"
     dysprosium_array(1)%mass_defect = -70.5223
     dysprosium_array(1)%atom_percent = 0.056
     dysprosium_array(1)%zaid = "66156.80c"

     dysprosium_array(2)%z = 66
     dysprosium_array(2)%a = 158
     dysprosium_array(2)%symbol ="Dy-158"
     dysprosium_array(2)%mass_defect = -70.4049
     dysprosium_array(2)%atom_percent = 0.095
     dysprosium_array(2)%zaid = "66158.80c"

     dysprosium_array(3)%z = 66
     dysprosium_array(3)%a = 160
     dysprosium_array(3)%symbol ="Dy-160"
     dysprosium_array(3)%mass_defect = -69.6711
     dysprosium_array(3)%atom_percent = 2.329
     dysprosium_array(3)%zaid = "66160.80c"

     dysprosium_array(4)%z = 66
     dysprosium_array(4)%a = 161
     dysprosium_array(4)%symbol ="Dy-161"
     dysprosium_array(4)%mass_defect = -68.0541
     dysprosium_array(4)%atom_percent = 18.889
     dysprosium_array(4)%zaid = "66161.80c"

     dysprosium_array(5)%z = 66
     dysprosium_array(5)%a = 162
     dysprosium_array(5)%symbol ="Dy-162"
     dysprosium_array(5)%mass_defect = -68.1798
     dysprosium_array(5)%atom_percent = 25.475
     dysprosium_array(5)%zaid = "66162.80c"

     dysprosium_array(6)%z = 66
     dysprosium_array(6)%a = 163
     dysprosium_array(6)%symbol ="Dy-163"
     dysprosium_array(6)%mass_defect = -66.3795
     dysprosium_array(6)%atom_percent = 24.896
     dysprosium_array(6)%zaid = "66163.80c"

     dysprosium_array(7)%z = 66
     dysprosium_array(7)%a = 164
     dysprosium_array(7)%symbol ="Dy-164"
     dysprosium_array(7)%mass_defect = -65.9663
     dysprosium_array(7)%atom_percent = 28.260
     dysprosium_array(7)%zaid = "66164.80c"

     !Calculate isotopic mass for Dysprosium
     DO i=1,dy_size
          holder = dysprosium_array(i)%a + (dysprosium_array(i)%mass_defect/931.494)

          dysprosium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     dy_mass = 0.0
     holder = 0.0

     !Calculate mass of Dysprosium
     DO i=1, dy_size
          dy_mass = holder + (dysprosium_array(i)%isotopic_mass* &
          &  dysprosium_array(i)%atom_percent)/100

          holder = dy_mass
     END DO
     
     !Calculate weight percent of Dysprosium
     DO i=1, dy_size
          dysprosium_array(i)%weight_percent = (dysprosium_array(i)%isotopic_mass &
          &   * dysprosium_array(i)%atom_percent)/dy_mass
     END DO

     !Number of Holmium Isotopes
     ho_size = 1

     holmium_array(1)%z = 67
     holmium_array(1)%a = 165
     holmium_array(1)%symbol ="Ho-165"
     holmium_array(1)%mass_defect = -64.8977
     holmium_array(1)%atom_percent = 100.0
     holmium_array(1)%zaid = "67165.80c"

     !Calculate isotopic mass for Holmium
     DO i=1, ho_size
          holder = holmium_array(i)%a + (holmium_array(i)%mass_defect/931.494)

          holmium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     ho_mass = 0.0
     holder = 0.0

     !Calculate mass of Holmium
     DO i=1, ho_size
          ho_mass = holder + (holmium_array(i)%isotopic_mass* &
          &  holmium_array(i)%atom_percent)/100

          holder = ho_mass
     END DO
     
     !Calculate weight percent of Holmium
     DO i=1, ho_size
          holmium_array(i)%weight_percent = (holmium_array(i)%isotopic_mass &
          &   * holmium_array(i)%atom_percent)/ho_mass
     END DO

     !Number of Erbium Isotopes
     er_size = 6 

     erbium_array(1)%z = 68
     erbium_array(1)%a = 162
     erbium_array(1)%symbol ="Er-162"
     erbium_array(1)%mass_defect = -66.3329
     erbium_array(1)%atom_percent = 0.139
     erbium_array(1)%zaid = "68162.80c"

     erbium_array(2)%z = 68
     erbium_array(2)%a = 164
     erbium_array(2)%symbol ="Er-164"
     erbium_array(2)%mass_defect = -65.9415
     erbium_array(2)%atom_percent = 1.601
     erbium_array(2)%zaid = "68164.80c"

     erbium_array(3)%z = 68
     erbium_array(3)%a = 166
     erbium_array(3)%symbol ="Er-166"
     erbium_array(3)%mass_defect = -64.9245
     erbium_array(3)%atom_percent = 33.503
     erbium_array(3)%zaid = "68166.80c"

     erbium_array(4)%z = 68
     erbium_array(4)%a = 167
     erbium_array(4)%symbol ="Er-167"
     erbium_array(4)%mass_defect = -63.2897
     erbium_array(4)%atom_percent = 22.869
     erbium_array(4)%zaid = "68167.80c"

     erbium_array(5)%z = 68
     erbium_array(5)%a = 168
     erbium_array(5)%symbol ="Er-168"
     erbium_array(5)%mass_defect = -62.9897
     erbium_array(5)%atom_percent = 26.978
     erbium_array(5)%zaid = "68168.80c"

     erbium_array(6)%z = 68
     erbium_array(6)%a = 170
     erbium_array(6)%symbol ="Er-170"
     erbium_array(6)%mass_defect = -60.1080
     erbium_array(6)%atom_percent = 14.910
     erbium_array(6)%zaid = "68170.80c"

     !Calculate isotopic mass for Erbium
     DO i=1,er_size
          holder = erbium_array(i)%a + (erbium_array(i)%mass_defect/931.494)

          erbium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     er_mass = 0.0
     holder = 0.0

     !Calculate mass of Erbium
     DO i=1, er_size
          er_mass = holder + (erbium_array(i)%isotopic_mass* &
          &  erbium_array(i)%atom_percent)/100

          holder = er_mass
     END DO
     
     !Calculate weight percent of Erbium
     DO i=1, er_size
          erbium_array(i)%weight_percent = (erbium_array(i)%isotopic_mass &
          &   * erbium_array(i)%atom_percent)/er_mass
     END DO

     !Number of Thulium Isotopes
     tm_size = 1

     thulium_array(1)%z = 69
     thulium_array(1)%a = 169
     thulium_array(1)%symbol ="Tm-169"
     thulium_array(1)%mass_defect = -61.2745
     thulium_array(1)%atom_percent = 100.0
     thulium_array(1)%zaid = "69169.80c"

     !Calculate isotopic mass for Thulium
     DO i=1, tm_size
          holder = thulium_array(i)%a + (thulium_array(i)%mass_defect/931.494)

          thulium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     tm_mass = 0.0
     holder = 0.0

     !Calculate mass of Thulium
     DO i=1, tm_size
          tm_mass = holder + (thulium_array(i)%isotopic_mass* &
          &  thulium_array(i)%atom_percent)/100

          holder = tm_mass
     END DO
     
     !Calculate weight percent of Thulium
     DO i=1, tm_size
          thulium_array(i)%weight_percent = (thulium_array(i)%isotopic_mass &
          &   * thulium_array(i)%atom_percent)/tm_mass
     END DO

     !Number of Ytterbium Isotopes
     yb_size = 7 

     ytterbium_array(1)%z = 70
     ytterbium_array(1)%a = 168
     ytterbium_array(1)%symbol ="Yb-168"
     ytterbium_array(1)%mass_defect = -61.5804
     ytterbium_array(1)%atom_percent = 0.123
     ytterbium_array(1)%zaid = " no zaid "

     ytterbium_array(2)%z = 70
     ytterbium_array(2)%a = 170
     ytterbium_array(2)%symbol ="Yb-170"
     ytterbium_array(2)%mass_defect = -60.7636
     ytterbium_array(2)%atom_percent = 2.982
     ytterbium_array(2)%zaid = " no zaid "

     ytterbium_array(3)%z = 70
     ytterbium_array(3)%a = 171
     ytterbium_array(3)%symbol ="Yb-171"
     ytterbium_array(3)%mass_defect = -59.3068
     ytterbium_array(3)%atom_percent = 14.09
     ytterbium_array(3)%zaid = " no zaid "

     ytterbium_array(4)%z = 70
     ytterbium_array(4)%a = 172
     ytterbium_array(4)%symbol ="Yb-172"
     ytterbium_array(4)%mass_defect = -59.2550
     ytterbium_array(4)%atom_percent = 21.68
     ytterbium_array(4)%zaid = " no zaid "

     ytterbium_array(5)%z = 70
     ytterbium_array(5)%a = 173
     ytterbium_array(5)%symbol ="Yb-173"
     ytterbium_array(5)%mass_defect = -57.5510
     ytterbium_array(5)%atom_percent = 16.103
     ytterbium_array(5)%zaid = " no zaid "

     ytterbium_array(6)%z = 70
     ytterbium_array(6)%a = 174
     ytterbium_array(6)%symbol ="Yb-174"
     ytterbium_array(6)%mass_defect = -56.9443
     ytterbium_array(6)%atom_percent = 32.026
     ytterbium_array(6)%zaid = " no zaid "

     ytterbium_array(7)%z = 70
     ytterbium_array(7)%a = 176
     ytterbium_array(7)%symbol ="Yb-176"
     ytterbium_array(7)%mass_defect = -53.4885
     ytterbium_array(7)%atom_percent = 12.996
     ytterbium_array(7)%zaid = " no zaid "

     !Calculate isotopic mass for Ytterbium
     DO i=1,yb_size
          holder = ytterbium_array(i)%a + (ytterbium_array(i)%mass_defect/931.494)

          ytterbium_array(i)%isotopic_mass = holder
     END DO

     !Set mass and holder to zero.
     yb_mass = 0.0
     holder = 0.0

     !Calculate mass of Ytterbium
     DO i=1, yb_size
          yb_mass = holder + (ytterbium_array(i)%isotopic_mass* &
          &  ytterbium_array(i)%atom_percent)/100

          holder = yb_mass
     END DO
     
     !Calculate weight percent of Ytterbium
     DO i=1, yb_size
          ytterbium_array(i)%weight_percent = (ytterbium_array(i)%isotopic_mass &
          &   * ytterbium_array(i)%atom_percent)/yb_mass
     END DO

   END SUBROUTINE Z66_Z70
