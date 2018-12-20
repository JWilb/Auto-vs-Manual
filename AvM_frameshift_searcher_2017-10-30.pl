#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Carp;
use Data::Dumper;
use Cwd;

my $n = "\n";
my $t = "\t";

################################################
# a-v-m_frameshift-owner_searcher.pl
# 
# Input: directory containing a-v-m_extractor.log files
#
# - for each gene in log line, find start codon
# - for each overlapping automatic annotation, find start codon
# - calculate difference
#		- modulo when divided by 3:
#		- modulo 0 = same frame
#		- modulo 1 / 2 = frame shift
#
# - how many owners with how many genes?
#
# Output: frameshift_searcher.log
#	owner_searcher.log
#
################################################

my $usage = "
a-v-m_gff3_frameshift-owner_searcher.pl

perl a-v-m-frameshift-owner_searcher.pl dir=PATH/DIR/ suffix=bla

This tool uses the .log file of a-v-m_extractor_nolog_nonew.pl (reading 
from dir) to compare starting points of manually curated genes and their 
predecessors to identify frameshifts. 

Only genes on the same strand are compared.

Output files are extended with 'suffix'.

Note that the start of gene and CDS can differ, thus this does not fully
replace a frameshift analysis on CDS basis.

Additionally, the number of curators and how many non-new genes they curated is exploited.

Already existing output files are overwritten...


Jeanne Wilbrandt, 10 Apr 2017 / 30 Oct 2017
";


# Get Options
my ($help);

my $opt_success = GetOptions('help|h' 	=> \$help,
			      );
if (! $opt_success) {
    die "FATAL: Command line parse error.$n$usage";
}

# Print help if needed
if ($help){
	print $usage;
	exit;
}

# Get arguments and check input
print "# Checking arguments...$n";

my @args = @ARGV;

die "FATAL: You need to specify 2 arguments: dir, suffix. $n$usage" if scalar @args != 2;

my ($dir, $suffix);
foreach my $arg (@args) {
	$arg =~ /^(\w*)=(.*)$/ or die "FATAL: Argument '$arg' does not match the requirements!$n$usage";
	my $val = $2;
	
	if ($1 =~ 'dir') {
		$dir = $val;
	}
	elsif ($1 =~ 'suffix') {
		$suffix = $val;
	}
	else {
		die "FATAL: Arguments do not match the requirements!$n$usage";
	}
}

# Prepare output files

my $cwd = getcwd;

#print $dir.'/frameshift_searcher.log', $n;
open (my $out, '>', $cwd.'/frameshift_searcher_'.$suffix.'.log') or die "Cannot open out file '${cwd}/frameshift_searcher.log':$!$n";
print $out #"File${t}"
		"ManCur ID${t}ManCur Scaffold${t}ManCur Start${t}Auto ID${t}Auto Scaff${t}Auto Start${t}Start Difference${t}Remainder$n";

open (my $out_owners, '>', $cwd.'/owner_searcher_'.$suffix.'.log') or die "Cannot open out file '${cwd}/frameshift_searcher_owners.log':$!$n";
print $out_owners "File${t}Name${t}Curated genes${t}(of these) New genes$n";



#### START ####

# Read dir and go through all found .log files.
my @files = read_dir($dir);

my %shifts;

foreach my $file (sort @files) {
	# skip all files not ending with '.log'
	next if $file !~ /^\w{4}_.*\.log$/;
	
	print "# Reading file $file...$n";
	my @log_lines = slurp_file($dir.$file);
	
	my %cur_by;
	#my %new_by;
	my $analyzed_genes = 0;
	my $no_change	= 0;
	my $no_shift 	= 0;
	my $one_shift 	= 0;
	my $two_shift	= 0;
	
	# go through all lines, identify start of man and auto
	foreach my $line (@log_lines) {
		
		# skip line if it begins with '#'
		next if $line =~ /^#/;
		# skip head line
		next if $line =~ /^ManCur/;
		
		my @items = split ("\t", $line);
		# line items are: 	0	man cur ID
		#					1	man cur scaff
		#					2	man cur start
		#					3	man cur stop
		#					4	man cur strand
		#					5	man cur note (ID=...; owner=...;)
		#					6	auto id
		#					7	auto scaff
		#					8	auto start
		#					9	auto stop
		#					10	auto strand
		#					11	auto note (ID=...;)
		
		# store position info
		my $man_scaff = $items[1];
		my $man_start = $items[2];
		my $man_strand = $items[4];
		my $auto_scaff = $items[7];
		my $auto_start = $items[8];
		my $auto_strand = $items[10];
		
		
		# ignore manually added = new genes
		next if scalar @items != 12;
		# ignore genes on opposite strands
		next if $man_strand ne $auto_strand;
		
		++$analyzed_genes;
		
		# get owner (how often?)
		my $owner = 'NA';
		if ($items[5] =~ /;owner=(\w*\.?\w*);?/) {
			$owner = $1;
		}
		else {
			print 'no owner: ', $items[5], $n;
		}
		++$cur_by{$owner};
		
		# get ids
		my ($man_ID, $auto_ID) = 'NotProvided';
		if ($items[5] =~ /^ID=(\w*\.?\w*);?/) {
			$man_ID = $1;
		}
		else {
			print 'no manID: ', $items[5], $n;
		}
		
		if ($items[11] =~ /^ID=(\w*\.?\w*);?/) {
			$auto_ID = $1;
		}
		else {
			print 'no autoID: ', $items[11], $n;
		}
		
		# compare starts
		my $difference	= abs($items[2] - $items[8]);		
		my $remainder	= $difference % 3;	
		
		# count cases
		++$no_change 	if $difference == 0;
		++$no_shift 	if $remainder == 0;
		++$one_shift 	if $remainder == 1;
		++$two_shift	if $remainder == 2; 
		
		
		# print to out file
		print $out join ($t, #$file, 
					    $man_ID, $man_scaff, $man_start,
					    $auto_ID, $auto_scaff, $auto_start, 
					    $difference, $remainder), $n; 
		
	}
	push @{$shifts{$file}}, ($no_change, $no_shift, $one_shift, $two_shift);
	
	# print owners to out file
	foreach my $owner (sort keys %cur_by) {
		print $out_owners join ($t, $file, $owner, $cur_by{$owner}), $n;
	}

}

	print $out "# Sums:${t}no change${t}no shift${t}one shifted${t}two shifted$n";
	
	foreach my $sp (sort keys %shifts) {
		print $out "# ", join ($t, $sp, $shifts{$sp}[0], $shifts{$sp}[1], $shifts{$sp}[2], $shifts{$sp}[3]), $n;
	}

print "Done!$n";




##############################################################################

=head2 read_dir
 
 Usage   : my @files = read_dir( $path )
 Function: Reads all file and directory names
           in a directory, hides system files (start with .)
 Calls   : -
 Returns : Array (all file and directory names)
 Args    : Path to directory
 
=cut

sub read_dir {
    my $path = shift @_; 

    # Open directory handle
    opendir ( my $dir, $path ) or 
        croak "Couldn't find path \"$path\": $!";

    # Read file names
    my @files = readdir( $dir ) or
        croak "Couldn't read directory \"$path\": $!";

    # Close directory handle
    closedir ( $dir ) or
        croak "Couldn't close directory \"$path\": $!";

    # Filter hidden system files out
    @files = grep {! /^\./ } @files;                                                    

    return @files;

}

=head2 slurp_file
 
 Usage   : my @lines = slurp_file( $file);
 Function: Read the entire file content, converts it
           to UNIX-style line breaks and returns all lines
           in an array
 Calls   : -
 Returns : Array (all lines of the file)
 Args    : File ( with path )
 
=cut

sub slurp_file {
    my $file = shift @_; 

    # Open filehandle
    open ( my $FILE, '<', $file )
        or croak "Couldn't open \"$file\": $!";

    # Read in entire file content at once 
    my $file_content = do { local $/; <$FILE> };

    # Close filehandle
    close $FILE
        or croak "Could't close \"$file\": $!";

    # Convert to unix-style line breaks
    $file_content =~ s/\r\n?/\n/g;

    # Split file content on line breaks
    my @file_lines = split ( '\n', $file_content );

    return @file_lines;

}
