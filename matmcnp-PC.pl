#!/usr/local/bin/perl
#
# usage: matmcnp.pl {job}
# matmcnp program execution
#
#

      $cpx = "copy";
      $rm = "del";
      $mv = "move";


      $ar = 0;
      $nar = 1;
      $job = $ARGV[$ar];
      if ($job =~ /(\S+).inp$/) {$job = $1} # Remove .inp, if user added it at command-line.


      $numarg = $#ARGV + 1;
      if  ($numarg != $nar) {
          print "\n\n   illegal number of arguments, arguments = $numarg  \n";
          print "   usage: $0 {job} \n";
          die "   try again $!\n";
      }

#
#     cleanup files
#
      if( -e "matmcnp.inp" ) {
        system("$rm matmcnp.inp");
      }
      if( -e "matmcnp.out" ) {
        system("$rm matmcnp.out");
      }
 
      $file = ${job}.".inp";
      $filout = "matmcnp.inp";

      if  (-r $file ) {
          rename ($file, $filout);
      }
      else {
          print " \n \n ERROR ${file} file does not exist \n \n";
          die "   try again $!\n";
      }

#
#    run MatMCNP
#
# 
     system("MatMCNP");
     system("$mv matmcnp.out ${job}.out");
     system("$mv $filout  ${job}.inp");
#
#
#
exit 0;
