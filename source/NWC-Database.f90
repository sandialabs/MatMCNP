!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   MODULE NWC_DATABASE
     USE NWC_2000
     IMPLICIT NONE

     !Module that holds all of the variables for the element subroutines, the weight_percent
     !subroutine, the atom_density subroutine, and the print_data subroutine.
     !Created June 23,2004 by Karen Kajder


     !Module variables
     INTEGER::h_size,he_size,li_size,be_size,b_size,c_size,n_size,o_size,f_size,ne_size,&
          na_size,mg_size,al_size,si_size,p_size,s_size,cl_size,ar_size,k_size,ca_size,&
          sc_size,ti_size,v_size,cr_size,mn_size,fe_size,co_size,ni_size,cu_size, &
          zn_size,ga_size,ge_size,as_size,se_size,br_size,kr_size,rb_size,sr_size, &
          y_size,zr_size,nb_size,mo_size,ru_size,rh_size,pd_size,ag_size,cd_size, &
          in_size,sn_size,sb_size,te_size,i_size,xe_size,cs_size,ba_size,la_size,ce_size,&
          pr_size,nd_size,sm_size,eu_size,gd_size,tb_size,dy_size,ho_size,er_size,tm_size,&
          yb_size,lu_size,hf_size,ta_size,w_size,re_size,os_size,ir_size,pt_size,au_size, &
          hg_size,tl_size,pb_size,bi_size,th_size,u_size, num_iso

     REAL::h_mass,he_mass,li_mass,be_mass,b_mass,c_mass,n_mass,o_mass,f_mass,ne_mass, &
          na_mass,mg_mass,al_mass,si_mass,p_mass,s_mass,cl_mass,ar_mass,k_mass,ca_mass,&
          sc_mass,ti_mass,v_mass,cr_mass,mn_mass,fe_mass,co_mass,ni_mass,cu_mass, &
          zn_mass,ga_mass,ge_mass,as_mass,se_mass,br_mass,kr_mass,rb_mass,sr_mass, &
          y_mass,zr_mass,nb_mass,mo_mass,ru_mass,rh_mass,pd_mass,ag_mass,cd_mass, &
          in_mass,sn_mass,sb_mass,te_mass,i_mass,xe_mass,cs_mass,ba_mass,la_mass,ce_mass,&
          pr_mass,nd_mass,sm_mass,eu_mass,gd_mass,tb_mass,dy_mass,ho_mass,er_mass,tm_mass,&
          yb_mass,lu_mass,hf_mass,ta_mass,w_mass,re_mass,os_mass,ir_mass,pt_mass,au_mass, &
          hg_mass,tl_mass,pb_mass,bi_mass,th_mass,u_mass
           
     !NWC Data with various dimension attributes
     TYPE(nwc_data), DIMENSION(1)::beryllium_array, fluorine_array,sodium_array, &
               aluminum_array,phosphorus_array,scandium_array,manganese_array,cobalt_array,&
               arsenic_array,yttrium_array,niobium_array,rhodium_array,iodine_array,     &
               cesium_array,praseodymium_array,terbium_array,holmium_array,thulium_array, &
               gold_array,bismuth_array,thorium_array
     TYPE(nwc_data), DIMENSION(2)::hydrogen_array, helium_array, lithium_array, & 
               boron_array,carbon_array, nitrogen_array,chlorine_array,vanadium_array, &
               copper_array,gallium_array,bromine_array,rubidium_array,silver_array,     &
               indium_array,antimony_array,lanthanum_array,europium_array,lutetium_array, &
               tantalum_array,rhenium_array,iridium_array,thallium_array
     TYPE(nwc_data), DIMENSION(3)::oxygen_array,neon_array,magnesium_array,     &
               silicon_array,argon_array,potassium_array,uranium_array
     TYPE(nwc_data), DIMENSION(4)::sulfur_array,chromium_array,iron_array,strontium_array,&
               cerium_array,lead_array
     TYPE(nwc_data), DIMENSION(5)::titanium_array,nickel_array,zinc_array,     &
               germanium_array,zirconium_array,tungsten_array
     TYPE(nwc_data), DIMENSION(6)::calcium_array,selenium_array,krypton_array,&
               palladium_array,erbium_array,hafnium_array,platinum_array
     TYPE(nwc_data), DIMENSION(7)::molybdenum_array,ruthenium_array,barium_array, &
               neodymium_array,samarium_array,gadolinium_array,dysprosium_array,     &
               ytterbium_array,osmium_array,mercury_array
     TYPE(nwc_data), DIMENSION(8)::cadmium_array,tellurium_array
     TYPE(nwc_data), DIMENSION(9)::xenon_array
     TYPE(nwc_data), DIMENSION(10)::tin_array
     TYPE(nwc_data), DIMENSION(500):: output_array

   END MODULE NWC_DATABASE
