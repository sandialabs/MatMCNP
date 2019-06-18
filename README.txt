README file for MatMCNP
-------------------------------

   ************************************************************
   *                   MatMCNP Version 4.0                    *
   *                                                          *
   *     Copyright 2019 National Technology & Engineering     *
   *             Solutions of Sandia, LLC (NTESS)             *
   *                                                          *
   * Under the terms of Contract DE-NA0003525 with NTESS, the *
   * U.S. Government retains certain rights in this software. *
   *                                                          *
   ************************************************************

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

                  *************************
                  *                       *
                  *                       *
                  *      DISCLAIMER       *
                  *                       *
                  *                       *
                  ********                *
                         *                *
                         *  No guarantee  *
                         * or warranty is *
                         *   implied or   *
                         *   given with   *
                         * this software. *
                         *                *
     *********************                *********************
     *                                                        *
     *                                                        *
     *     The software represents the best effort of the     *
     *  programmers and was developed for specific purposes   *
     *   at Sandia National Laboratories (SNL). Any use of    *
     * this software, either internal to SNL or external, is  *
     *          the sole responsibility of the user.          *
     *                                                        *
     *                                                        *
     *     Sandia National Laboratories is a multimission     *
     *          **********   laboratory   **********          *
     *          *        *  managed and   *        *          *
     *          *        *  operated by   *        *          *
     *          *        *    National    *        *          *
     *          *        * Technology and *        *          *
     *          *        *  Engineering   *        *          *
     *       ****        *  Solutions of  *        ****       *
     *       *           *  Sandia LLC,   *           *       *
     *       *           * a wholly owned *           *       *
     *       *           * subsidiary of  *           *       *
     *       *          **   Honeywell    **          *       *
     *       *          ** International  **          *       *
     *       *          **  Inc. for the  **          *       *
     *********          ** Department of  **          *********
                        **    Energy's    **
                        **    National    **
                        **    Nuclear     **
                        **    Security    **
                        ** Administration **
                        ** under contract **
                        **  DE-NA0003525. **
                        **                **
                        ********************

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

   MatMCNP Code Release
   Version 4.0
   January 2019

   K. Russell DePriest
   Principal R&D Scientist and Engineer
   Applied Nuclear Technologies, Org. 1384
   Sandia National Laboratories
   P. O. Box 5800, MS 1146
   Albuquerque, NM  87185-1146
   krdepri@sandia.gov
   (505) 845-8141

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

Files in distribution:

MatMCNP Directory

 (1) MatMCNP_V4.0_CopyrightNotice_License.pdf
 (2) MatMCNP - C-shell script for running code on UNIX/Linux system
 (3) MatMCNP.bat - DOS batch file for running code on Windows PC system
 (4) matmcnp.pl  - Perl script for running the code and moving files on UNIX/Linux system
 (5) matmcnp-PC.pl - Perl script for running the code and moving files on Windows PC system
 (6) README.txt - This file
 (7) test.inp - An input deck for testing the installation of the code
 (8) test.out - An output file with correct results from testing the code
 (9) SAND2014-17693_UUR.pdf - Sandia report documenting the MatMCNP code
 (10) bin subdirectory

   A. MatMCNP.exe - MatMCNP Version 4.0 Windows executable
   B. xmatmcnp - MatMCNP Version 4.0 Linux excutable

(10) source subdirectory (source files for compiling code independently)

   A. atom_density.f90
   B. enriched.f90
   C. MatMCNP.f90
   D. naturalzaid.f90
   E. NWC.f90
   F. NWC-Database.f90
   G. print_data.f90
   H. read_data.f90
   I. title_comment.f90
   J. weight_percent.f90
   K. Z1_Z5.f90
   L. Z6_Z10.f90
   M. Z11_Z15.f90
   N. Z16_Z20.f90
   O. Z21_Z25.f90
   P. Z26_Z30.f90
   Q. Z31_Z35.f90
   R. Z36_Z40.f90
   S. Z41_Z45.f90
   T. Z46_Z50.f90
   U. Z51_Z55.f90
   V. Z56_Z60.f90
   W. Z61_Z65.f90
   X. Z66_Z70.f90
   Y. Z71_Z75.f90
   Z. Z76_Z80.f90
  AA. Z81_Z85.f90
  AB. Z86_Z90.f90
  AC. Z91_Z92.f90
  AD. makefile - Makefile with options for building Intel version on Linux

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

Documentation: 
  K. Russell DePriest and Karen C. Saavedra. MatMCNP: A Code for Producing Material
    Cards for MCNP. Sandia Report, SAND2014-17693. Sandia National Laboratories,
    Albuquerque, NM. September 2014. (Unclassified Unlimited Release)

If you use the code and CHOOSE to cite it, the preferred reference is given below:

  K. R. DePriest and K. C. Saavedra. MatMCNP: A Code for Producing Material Cards
  for MCNP. Sandia Report, SAND2014-17693. Sandia National Laboratories, 
  Albuquerque, NM. September 2014.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

Installation (assumes Perl is installed on the computer):

(1) Copy entire MatMCNP folder to desired working location

(2) For Windows PC systems, the code is "installed". The program requires the use
    of the Command Prompt.
    
    A. Start > All Programs > Accessories > Command Prompt
    B. Using command line arguments, change to the working directory

******

    C. Run Example
      
c:\Work\MatMCNP> MatMCNP test.inp
 *
 *
 ************************************************************
 *    Copyright 2019 National Technology and Engineering    *
 *             Solutions of Sandia, LLC (NTESS)             *
 *                                                          *
 * Under the terms of Contract DE-NA0003525 with NTESS, the *
 * U.S. Government retains certain rights in this software. *
 *                                                          *
 ************************************************************
 *
 *
        1 file(s) copied.
        1 file(s) moved.
        1 file(s) moved.
MatMCNP Calculation Complete

******

(3) An executable file is included for Linux-based systems. However, some Linux/UNIX
    systems may require compilation of the code.

    A. A makefile with options for Intel Fortran (ifort).
    B. Compiling is accomplished by using the make utility.

******

    C. Compile Example

Work/MatMCNP/source> make
ifort -c NWC.f90
ifort -c title_comment.f90
ifort -c NWC-Database.f90
ifort -c atom_density.f90
ifort -c enriched.f90
ifort -c MatMCNP.f90
ifort -c naturalzaid.f90
ifort -c print_data.f90
ifort -c read_data.f90
ifort -c weight_percent.f90
ifort -c Z1_Z5.f90
ifort -c Z6_Z10.f90
ifort -c Z11_Z15.f90
ifort -c Z16_Z20.f90
ifort -c Z21_Z25.f90
ifort -c Z26_Z30.f90
ifort -c Z31_Z35.f90
ifort -c Z36_Z40.f90
ifort -c Z41_Z45.f90
ifort -c Z46_Z50.f90
ifort -c Z51_Z55.f90
ifort -c Z56_Z60.f90
ifort -c Z61_Z65.f90
ifort -c Z66_Z70.f90
ifort -c Z71_Z75.f90
ifort -c Z76_Z80.f90
ifort -c Z81_Z85.f90
ifort -c Z86_Z90.f90
ifort -c Z91_Z92.f90
ifort -o xmatmcnp  NWC.o NWC-Database.o  atom_density.o  enriched.o MatMCNP.o  naturalzaid.o  print_data.o  read_data.o title_comment.o  weight_percent.o  Z1_Z5.o  Z6_Z10.o Z11_Z15.o  Z16_Z20.o  Z21_Z25.o  Z26_Z30.o Z31_Z35.o Z36_Z40.o  Z41_Z45.o  Z46_Z50.o  Z51_Z55.o Z56_Z60.o Z61_Z65.o  Z66_Z70.o  Z71_Z75.o  Z76_Z80.o Z81_Z85.o Z86_Z90.o  Z91_Z92.o 
mv xmatmcnp ../bin/.

******

    D. To run the code on a Linux system, change to the working directory

    E. Run Example

Work/MatMCNP> MatMCNP test 

************************************************************
*     Copyright 2019 National Technology & Engineering     *
*             Solutions of Sandia, LLC (NTESS)             *
*                                                          *
* Under the terms of Contract DE-NA0003525 with NTESS, the *
* U.S. Government retains certain rights in this software. *
*                                                          *
************************************************************

 
 
   Running MatMCNP Version 4.0   
 
         Moving output file.    
 
   MatMCNP Calculation Complete. 

******
 
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

Questions about installation, compilation, and/or running MatMCNP should be
  sent to K. Russell DePriest (krdepri@sandia.gov).



