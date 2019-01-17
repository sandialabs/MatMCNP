!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE atom_density(w_percent,compound_density,z_of_element,&
                           total_atom_b_cm,number_elements,iso_atom_b_cm)
     USE NWC_2000
     USE NWC_DATABASE
     IMPLICIT NONE

     !This subroutine calculates the atoms/barn-centimeter of each isotope 
     !and the total density for the mixture in atoms/barn-centimeter.
     !Created on June 23,2004 by Karen Kajder.
     !
     !Modified/Reviewed by K. Russell DePriest on December 16, 2011
     ! - Changes were essentially formatting/readability modifications.

     !Dummy variables
     REAL,INTENT(IN):: compound_density
     REAL,INTENT(OUT)::total_atom_b_cm
     INTEGER,INTENT(IN)::number_elements
     INTEGER, DIMENSION(*),INTENT(IN)::z_of_element
     REAL, DIMENSION(*),INTENT(IN)::w_percent
     REAL, DIMENSION(*),INTENT(OUT)::iso_atom_b_cm

     !Local variables
     INTEGER:: i,j,z,k,counter,counterplus,float

     !Initialize
     counter = 0
     counterplus = 1
     total_atom_b_cm = 0.0

     !Loop for each element.
     DO j=1, number_elements
        !and for each element's isotope
        z = z_of_element(j)
           
        SELECT CASE (z)
          
        CASE(1)     
          counter = counter + h_size
          DO k= counterplus, counter
             float = k - counterplus + 1
             output_array(k) = hydrogen_array(float)
             !
             !Calculate isotopic weight percent in the mixture. 
             output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
             iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1
                              
        CASE(2)          
          counter = counter + he_size     
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = helium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(3)          
          counter = counter + li_size     
               
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = lithium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(4)
          counter = counter + be_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = beryllium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(5)          
          counter = counter + b_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = boron_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(6)          
          counter = counter + c_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = carbon_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(7)     
          counter = counter + n_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = nitrogen_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(8)     
          counter = counter + o_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = oxygen_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(9)     
          counter = counter + f_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = fluorine_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(10)     
          counter = counter + ne_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = neon_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(11)     
          counter = counter + na_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = sodium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(12)     
          counter = counter + mg_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = magnesium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(13)     
          counter = counter + al_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = aluminum_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(14)     
          counter = counter + si_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = silicon_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(15)     
          counter = counter + p_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = phosphorus_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(16)     
          counter = counter + s_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = sulfur_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(17)     
          counter = counter + cl_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = chlorine_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(18)     
          counter = counter + ar_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = argon_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(19)     
          counter = counter + k_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = potassium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(20)     
          counter = counter + ca_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = calcium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(21)     
          counter = counter + sc_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = scandium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(22)     
          counter = counter + ti_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = titanium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(23)     
          counter = counter + v_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = vanadium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(24)     
          counter = counter + cr_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = chromium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(25)     
          counter = counter + mn_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = manganese_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(26)     
          counter = counter + fe_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = iron_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(27)     
          counter = counter + co_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = cobalt_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(28)     
          counter = counter + ni_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = nickel_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(29)     
          counter = counter + cu_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = copper_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(30)     
          counter = counter + zn_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = zinc_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(31)     
          counter = counter + ga_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = gallium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(32)     
          counter = counter + ge_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = germanium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(33)     
          counter = counter + as_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = arsenic_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(34)     
          counter = counter + se_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = selenium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(35)     
          counter = counter + br_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = bromine_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(36)     
          counter = counter + kr_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = krypton_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(37)     
          counter = counter + rb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = rubidium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(38)     
          counter = counter + sr_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = strontium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(39)     
          counter = counter + y_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = yttrium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(40)     
          counter = counter + zr_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = zirconium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(41)     
          counter = counter + nb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = niobium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(42)     
          counter = counter + mo_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = molybdenum_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(44)     
          counter = counter + ru_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = ruthenium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(45)     
          counter = counter + rh_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = rhodium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(46)     
          counter = counter + pd_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = palladium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(47)     
          counter = counter + ag_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = silver_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(48)     
          counter = counter + cd_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = cadmium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(49)     
          counter = counter + in_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = indium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(50)     
          counter = counter + sn_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = tin_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(51)     
          counter = counter + sb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = antimony_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
              counterplus = counter + 1

        CASE(52)     
          counter = counter + te_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = tellurium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(53)     
          counter = counter + i_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = iodine_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(54)     
          counter = counter + xe_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = xenon_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(55)     
          counter = counter + cs_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = cesium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(56)     
          counter = counter + ba_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = barium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(57)     
          counter = counter + la_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = lanthanum_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(58)     
          counter = counter + ce_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = cerium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(59)     
          counter = counter + pr_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = praseodymium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(60)     
          counter = counter + nd_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = neodymium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(62)     
          counter = counter + sm_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = samarium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(63)     
          counter = counter + eu_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = europium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(64)     
          counter = counter + gd_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = gadolinium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(65)     
          counter = counter + tb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = terbium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(66)     
          counter = counter + dy_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = dysprosium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(67)     
          counter = counter + ho_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = holmium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(68)     
          counter = counter + er_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = erbium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(69)     
          counter = counter + tm_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = thulium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(70)     
          counter = counter + yb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = ytterbium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(71)     
          counter = counter + lu_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = lutetium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(72)     
          counter = counter + hf_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = hafnium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(73)     
          counter = counter + ta_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = tantalum_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(74)     
          counter = counter + w_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = tungsten_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(75)     
          counter = counter + re_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = rhenium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(76)     
          counter = counter + os_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = osmium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(77)     
          counter = counter + ir_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = iridium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(78)     
          counter = counter + pt_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = platinum_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(79)     
          counter = counter + au_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = gold_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(80)     
          counter = counter + hg_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = mercury_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(81)     
          counter = counter + tl_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = thallium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(82)     
          counter = counter + pb_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = lead_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(83)     
          counter = counter + bi_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = bismuth_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(90)     
          counter = counter + th_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = thorium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1

        CASE(92)     
          counter = counter + u_size          
          DO k= counterplus, counter
              float = k - counterplus + 1
              output_array(k) = uranium_array(float)
              !Calculate isotopic weight percent in the mixture. 
              output_array(k)%weight_percent = w_percent(j) * output_array(k)%weight_percent
              iso_atom_b_cm(k) = (output_array(k)%weight_percent * compound_density * 0.602214129)/ &
                         & output_array(k)%isotopic_mass
             total_atom_b_cm = total_atom_b_cm + iso_atom_b_cm(k)
          END DO
          counterplus = counter + 1
          

          END SELECT

     END DO

     num_iso = counter

     total_atom_b_cm = total_atom_b_cm/100

   END SUBROUTINE
