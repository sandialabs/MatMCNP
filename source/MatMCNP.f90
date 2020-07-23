!
!  Copyright (c) 2019 National Technology & Engineering Solutions of
!  Sandia, LLC (NTESS). Under the terms of Contract DE-NA0003525 with
!  NTESS, the U.S. Government retains certain rights in this software.
!
!****************************************************************************
!
!  PROGRAM: MatMCNP
!
!  PURPOSE:  This program calculates the material cards for MCNP/MCNPX input
!            decks.  The atomic and weight fractions are calculated for each
!            isotope in the material regardless of whether or not the cross
!            section data exists at the isotopic level for the elements in
!            the material.  In addition to the material computation, the
!            FM card for converting neutron and gamma fluences to dose in
!            the material is also generated.
!
!  Version Notes:
!   Version 4.0
!   - First open source code release
!   Version 4.1
!    - NWC Data updated to "Nuclear Wallet Cards database version of 7/10/2019"
!    - Default zaids for material cards become ENDF/B-VIII.0 at 293.6 K (*.00c)
!    - New xsec evaluations for O-18, Neon, Ytterbium, Osmium, and Platinum.
!    - Carbon must now be treated isotopically rather than as an element.
!    - The "naturalzaid" routine is no longer is used. It is left as a "CONTINUE"
!        statement in the Fortran, but it isn't called.
!    - Command line arguments for input/output file are now allowed.
!
!****************************************************************************
!
PROGRAM MatMCNP
   USE NWC_2000
   USE NWC_DATABASE
   IMPLICIT NONE

   !Variables
   CHARACTER(LEN=11)::infile,outfile

   REAL:: compound_density,total_atom_b_cm,total_fake_atomic_weight,FM_value
   REAL, DIMENSION(92):: a_or_w_percent, w_percent,a_percent, fake_atomic_weight
   REAL,DIMENSION(300):: iso_atom_b_cm
   INTEGER:: number_elements,i
   INTEGER, DIMENSION(92)::z_of_element
   CHARACTER(LEN=3), DIMENSION(92)::natural_or_enriched
   CHARACTER(LEN=6):: atomic, weight,symbol,atom_or_weight
   CHARACTER(LEN=5)::mat_number
	
   !Call to subroutine with the elements and their nuclear card information
   !in groups of five.
   CALL Z1_Z5
   CALL Z6_Z10
   CALL Z11_Z15
   CALL Z16_Z20
   CALL Z21_Z25
   CALL Z26_Z30
   CALL Z31_Z35
   CALL Z36_Z40
   CALL Z41_Z45
   CALL Z46_Z50
   CALL Z51_Z55
   CALL Z56_Z60
   CALL Z61_Z65
   CALL Z66_Z70
   CALL Z71_Z75
   CALL Z76_Z80
   CALL Z81_Z85
   CALL Z86_Z90
   CALL Z91_Z92

   !Assign names to input and output file.
   ! - Eventually I plan to make this an commandline option, so that
   !   the user can specify both the input and output file names.
   !   Currently, the MatMCNP program runs with a script that
   !   performs the file name moves for the user instead.
   infile = "matmcnp.inp"
   outfile = "matmcnp.out"

   ! Open the input and output file.
   OPEN (UNIT = 10, FILE = infile, STATUS = "OLD")
   OPEN (UNIT = 11, FILE = outfile, STATUS = "UNKNOWN")
   
   !Read the title of the mixture and the comment cards from the input.
   CALL title_comment
   
   !Read the density, whether the fractions are atomic or weight, the number of elements,
   !the z of each element, whether each element is natural or enriched, the atomic (or weight)
   !fraction, and the MCNP material number.
   CALL read_data(compound_density,atom_or_weight,number_elements,z_of_element, &
                  natural_or_enriched,a_or_w_percent,mat_number)

   !IF statement for atomic or weight percent
   IF (atom_or_weight == 'atomic') THEN

     a_percent = a_or_w_percent

     !Calculate the weight percent.
     CALL weight_percent(w_percent,z_of_element,number_elements,a_or_w_percent)

     !Calculate the atom density of each isotope and the total.
     CALL atom_density(w_percent,compound_density,z_of_element,total_atom_b_cm,&
                       number_elements,iso_atom_b_cm)

     iso_atom_b_cm = iso_atom_b_cm/100

   ELSE IF (atom_or_weight == 'weight') THEN
   
     w_percent = a_or_w_percent
     !Calculate the atom density of each isotope and the total density.
     CALL atom_density(w_percent,compound_density,z_of_element,total_atom_b_cm,&
                       number_elements,iso_atom_b_cm)

     iso_atom_b_cm = iso_atom_b_cm/100

   ELSE
     !Error message if user does not specify "atomic" or "weight" in the input file.
     WRITE (UNIT=11,FMT=104) 
     STOP
   END IF

   DO i=1, num_iso
     !
     !Calculate each isotope's atom fraction (at this point, iso_atom_b_cm 
     !and total_atom_b_cm have already been divided by 100).
     !
     output_array(i)%atom_percent = iso_atom_b_cm(i)/total_atom_b_cm
     !
     !Divide weight percent of each isotope by 100 to get the weight fraction.
     !
     output_array(i)%weight_percent = output_array(i)%weight_percent/100

   END DO

   !Initialize the "atomic weight" to be used for calculating the FM card
   total_fake_atomic_weight = 0.0

   !FM card.  The formula used here can be found on page 3-97 of the MCNP5
   ! User's Manual.
   DO i=1,num_iso
       fake_atomic_weight = output_array(i)%atom_percent * output_array(i)%isotopic_mass
       total_fake_atomic_weight = total_fake_atomic_weight + fake_atomic_weight(i)
   END DO

   FM_value = (((6.02214129E23 * 1E-24)/total_fake_atomic_weight) * 1.602E-8)

   !Print all of the data to the output file.
   CALL print_data(total_atom_b_cm,iso_atom_b_cm,mat_number,z_of_element)
   WRITE(UNIT=11,FMT=105)
   WRITE(UNIT=11,FMT=106)   FM_value, mat_number
   WRITE(UNIT=11,FMT=107)   FM_value, mat_number

   !Format Statement
   104 FORMAT (/"Error: Need to specify whether atomic or weight percent.")
   105 FORMAT ("C"/"C",2X,"To convert a particle flux to rad[Material]")
   106 FORMAT ("C",2X,"use FM ",ES14.7," ",A5,"-4  1 for neutrons")
   107 FORMAT ("C",2X," or FM ",ES14.7," ",A5,"-5 -6 for photons."/"C")

END PROGRAM MatMCNP

