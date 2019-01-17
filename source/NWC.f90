!
!  Copyright (c) 2019 National Technology & Engineering Solutions of 
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with 
!  NTESS, the U.S. Government retains certain rights in this software.
!
   MODULE NWC_2000
     IMPLICIT NONE

     !This is the database type which will be used throughout the program.
     !We are creating a derived datatype to hold the data from the 
     ! Nuclear Wallet Cards.

     TYPE nwc_data
       INTEGER:: z, a
       CHARACTER(LEN=6):: symbol
       REAL:: mass_defect, atom_percent, weight_percent, isotopic_mass
       CHARACTER(LEN=9):: zaid
     END TYPE nwc_data

   END MODULE NWC_2000    
