#!/usr/local/bin/perl
#
# usage: matmcnp.pl {job}
# matmcnp program execution
#
#
#  This could be made generic with "SYSTEMTYPE" options.
#  These command variables assume a UNIX or Linux system.
# 
  $cpx = "cp";
  $rmx = "rm";
  $mvx = "mv";

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
# Cleanup files for new run.  Should be already clean.
#
  $input = "matmcnp.inp";
  $output = "matmcnp.out";
  if (-e $input) {
      system("$rmx  matmcnp.inp");
  }
  if (-e $output) {
      system("$rmx  matmcnp.out");
  }
#

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
     print "\n\n   Running MatMCNP Version 4.0   \n";
     system("$cpx bin/xmatmcnp .");
     system("xmatmcnp");
     print "\n         Moving output file.    \n";
     system("$mvx matmcnp.out ${job}.out");
     system("$mvx $filout  ${job}.inp");
     system("$rmx xmatmcnp");
     print "\n   MatMCNP Calculation Complete. \n\n\n";
#
#
#
exit 0;
