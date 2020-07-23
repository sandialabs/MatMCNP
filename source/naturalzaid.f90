!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
   SUBROUTINE naturalzaid(adj_num_iso)
     IMPLICIT NONE

     !This subroutine takes elements that use a natural zaid and adds 
     !together each of their isotopes atom fractions. It then puts this
     !combined atom fraction in the output file with the natural zaid for
     !the element. Created on July 22,2004 by Karen Kajder and Russell 
     !DePriest.
     !
     !Modified by K. Russell DePriest on December 19, 2011 to account for
     ! new xsec data available in mcnp format.
     ! 
     ! - Additional changes with the release of ENDF/B-VII release 1
     !    made on 05/09/2014
     ! 
     !This subroutine has been made obsolete with the ENDF/B-VIII.0 cross
     ! section release. (July 2020)

     CONTINUE


   END SUBROUTINE naturalzaid
