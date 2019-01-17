#
#
# This makefile works for Intel fortran (ifort). Additional 
#  targets can be created.
#
#
objects =   NWC.o NWC-Database.o  atom_density.o  enriched.o \
            MatMCNP.o  naturalzaid.o  print_data.o  read_data.o  \
            title_comment.o  weight_percent.o  Z1_Z5.o  Z6_Z10.o \
            Z11_Z15.o  Z16_Z20.o  Z21_Z25.o  Z26_Z30.o Z31_Z35.o \
            Z36_Z40.o  Z41_Z45.o  Z46_Z50.o  Z51_Z55.o Z56_Z60.o \
            Z61_Z65.o  Z66_Z70.o  Z71_Z75.o  Z76_Z80.o Z81_Z85.o \
            Z86_Z90.o  Z91_Z92.o	

# Fortran compiler
intel : fortcomp = ifort
gnu : fortcomp = gfortran
# Compile flags
intel : OFLAGS = -c
gnu : OFLAGS = -c

intel :  $(objects) 
	$(fortcomp) -o xmatmcnp  $(objects)
	mv xmatmcnp ../bin/.

gnu :  $(objects) 
	$(fortcomp) -o xmatmcnp  $(objects)
	mv xmatmcnp ../bin/.

%.o : %.f90
	$(fortcomp) $(OFLAGS) $<

#	
# State the dependency of each of the object files
#
NWC-Database.o : NWC.o
atom_density.o : NWC.o NWC-Database.o 
enriched.o : NWC.o NWC-Database.o 
MatMCNP.o : NWC.o NWC-Database.o 
naturalzaid.o : NWC.o NWC-Database.o 
print_data.o : NWC.o NWC-Database.o 
read_data.o : NWC.o NWC-Database.o
weight_percent.o : NWC.o NWC-Database.o 
Z1_Z5.o : NWC.o NWC-Database.o
Z6_Z10.o : NWC.o NWC-Database.o 
Z11_Z15.o : NWC.o NWC-Database.o 
Z16_Z20.o : NWC.o NWC-Database.o 
Z21_Z25.o : NWC.o NWC-Database.o
Z26_Z30.o : NWC.o NWC-Database.o 
Z31_Z35.o : NWC.o NWC-Database.o 
Z36_Z40.o : NWC.o NWC-Database.o
Z41_Z45.o : NWC.o NWC-Database.o 
Z46_Z50.o : NWC.o NWC-Database.o
Z51_Z55.o : NWC.o NWC-Database.o
Z56_Z60.o : NWC.o NWC-Database.o
Z61_Z65.o : NWC.o NWC-Database.o
Z66_Z70.o : NWC.o NWC-Database.o
Z71_Z75.o : NWC.o NWC-Database.o 
Z76_Z80.o : NWC.o NWC-Database.o 
Z81_Z85.o : NWC.o NWC-Database.o
Z86_Z90.o : NWC.o NWC-Database.o
Z91_Z92.o : NWC.o NWC-Database.o 


clean:
	rm -f *.o *.mod 






