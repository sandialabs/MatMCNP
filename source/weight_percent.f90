!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE weight_percent(w_percent,z_of_element,number_elements,a_or_w_percent)
     USE NWC_DATABASE
     IMPLICIT NONE

     !Subroutine that calculates the weight percent through the use 
     ! of a case select statement.
     !Created on June 23, 2004 by Karen Kajder.
     
     !Dummy Variables
     INTEGER, INTENT(IN),DIMENSION(*)::z_of_element
     INTEGER,INTENT(IN):: number_elements
     REAL, DIMENSION(92),INTENT(IN):: a_or_w_percent
     REAL,DIMENSION(92),INTENT(OUT):: w_percent
     
     !Local Variables
     INTEGER::j,z
     REAL:: total_weight
     REAL,DIMENSION(92)::holder_weight

     !Initialize everything
     holder_weight = 0.0
     total_weight = 0.0

     DO j=1,number_elements
       z = z_of_element(j)
       SELECT CASE (z)

       CASE(1)                    
         holder_weight(j) = h_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)                         
                    
       CASE(2)                    
         holder_weight(j) = he_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)          
                    
       CASE(3)                    
         holder_weight(j) = li_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)          
                    
       CASE(4)
         holder_weight(j) = be_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)          
                    
       CASE(5)                    
         holder_weight(j) = b_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)          
                    
       CASE(6)                    
         holder_weight(j) = c_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)          
                    
       CASE(7)     
         holder_weight(j) = n_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(8)     
         holder_weight(j) = o_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(9)     
         holder_weight(j) = f_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(10)     
         holder_weight(j) = ne_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(11)     
         holder_weight(j) =na_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(12)     
         holder_weight(j) = mg_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(13)     
         holder_weight(j) = al_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(14)     
         holder_weight(j) = si_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(15)     
         holder_weight(j) = p_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(16)     
         holder_weight(j) = s_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(17)     
         holder_weight(j) = cl_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(18)     
         holder_weight(j) = ar_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(19)     
         holder_weight(j) = k_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(20)     
         holder_weight(j) = ca_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(21)     
         holder_weight(j) = sc_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(22)     
         holder_weight(j) = ti_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(23)     
         holder_weight(j) = v_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(24)     
         holder_weight(j) = cr_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(25)     
         holder_weight(j) = mn_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(26)     
         holder_weight(j) = fe_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(27)     
         holder_weight(j) = co_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(28)     
         holder_weight(j) = ni_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(29)     
         holder_weight(j) = cu_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(30)     
         holder_weight(j) = zn_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(31)     
         holder_weight(j) = ga_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(32)     
         holder_weight(j) = ge_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(33)     
         holder_weight(j) = as_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(34)     
         holder_weight(j) = se_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(35)     
         holder_weight(j) = br_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(36)     
         holder_weight(j) = kr_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(37)     
         holder_weight(j) = rb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(38)     
         holder_weight(j) = sr_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(39)     
         holder_weight(j) = y_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(40)     
         holder_weight(j) = zr_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(41)     
         holder_weight(j) = nb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
     
       CASE(42)     
         holder_weight(j) = mo_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                              
       CASE(44)     
         holder_weight(j) = ru_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                              
       CASE(45)     
         holder_weight(j) = rh_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                              
       CASE(46)     
         holder_weight(j) = pd_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(47)     
         holder_weight(j) = ag_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(48)     
         holder_weight(j) = cd_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(49)     
         holder_weight(j) = in_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(50)     
         holder_weight(j) = sn_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(51)     
         holder_weight(j) = sb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(52)     
         holder_weight(j) = te_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(53)     
         holder_weight(j) = i_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(54)     
         holder_weight(j) = xe_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(55)     
         holder_weight(j) = cs_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(56)     
         holder_weight(j) = ba_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(57)     
         holder_weight(j) = la_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(58)     
         holder_weight(j) = ce_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
               
       CASE(59)     
         holder_weight(j) = pr_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(60)     
         holder_weight(j) = nd_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(62)     
         holder_weight(j) = sm_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(63)     
         holder_weight(j) = eu_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(64)     
         holder_weight(j) = gd_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(65)     
         holder_weight(j) = tb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(66)     
         holder_weight(j) = dy_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(67)     
         holder_weight(j) = ho_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(68)     
         holder_weight(j) = er_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(69)     
         holder_weight(j) = tm_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(70)     
         holder_weight(j) = yb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                        
       CASE(71)     
         holder_weight(j) = lu_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(72)     
         holder_weight(j) = hf_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                                   
       CASE(73)     
         holder_weight(j) = ta_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(74)     
         holder_weight(j) = w_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
               
       CASE(75)     
         holder_weight(j) = re_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(76)     
         holder_weight(j) = os_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(77)     
         holder_weight(j) = ir_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(78)     
         holder_weight(j) = pt_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(79)     
         holder_weight(j) = au_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(80)     
         holder_weight(j) = hg_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(81)     
         holder_weight(j) = tl_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(82)     
         holder_weight(j) = pb_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(83)     
         holder_weight(j) = bi_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     
                    
       CASE(90)     
         holder_weight(j) = th_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       CASE(92)     
         holder_weight(j) = u_mass * a_or_w_percent(j)
         total_weight = total_weight + holder_weight(j)     

       END SELECT

     END DO

     w_percent = holder_weight/total_weight

   END SUBROUTINE
