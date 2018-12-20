#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;

my $n = "\n";

################################################
# a-v-m_clean_ManCur.pl
# 
# Input: OGS_ManCur.gff3 (only manually curated models)
#
# - for each gene in manually curated models, check whether mRNA is present 
# - remove all genes without mRNA (i.e., non-coding genes)
#
# Output: OGS_ManCur_coding.gff3
#
################################################


my $usage = "
a-v-m_clean_ManCur.pl

perl a-v-m-clean_ManCur.pl man=/PATH/to/OGS.gff3

This tool checks the manually curated models to remove non-coding genes.

Already existing output files are overwritten...

Input from i5k expected.


Jeanne Wilbrandt, 03.02.2017
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
print "Checking arguments...$n";

my @args = @ARGV;

die "FATAL: You need to specify 1 argument: man. $n$usage" if scalar @args != 1;

my ($man);
foreach my $arg (@args) {
	$arg =~ /^(\w*)=(.*)$/ or die "FATAL: Argument '$arg' does not match the requirements!$n$usage";
	my $val = $2;
	
	if($1 =~ 'man') {
		$man = $val;
		die "FATAL: Manually curated file file '$man' does not exist!$n$usage" if !-e $man;
	}
	else {
		die "FATAL: Arguments do not match the requirements!$n$usage";
	}
}


# Slurp ManCur.gff3
print "Slurping ManCur file...$n";
my @man_lines = slurp_file($man);

# Go through file and build hash (gene line -> child lines)
my %man_hash;

my $gene_id = 'NA';

foreach my $line (@man_lines) {
	next if $line =~ /^#/;
	
	#print $line, $n if $line =~ /CLEC004898/;
	
	my @items = split("\t", $line);
	if ($items[2] =~ /gene/) {
		$items[8] =~ /ID=([\w|-]*);?/;
		$gene_id = $1;
		push @{$man_hash{$gene_id}}, $line;
	}
	else {
		push @{$man_hash{$gene_id}}, $line if $items[8] =~ /$gene_id/;
	}
	
} 

# Go through hash and drop entries without mRNA
print "Found ", scalar keys %man_hash, " genes. Checking for non-coding genes...$n";

foreach my $gene_id (keys %man_hash) {
	my @lines = @{$man_hash{$gene_id}};
	my $drop_flag = 0;
	
	# Check all lines of the model to find 'mRNA'
	foreach my $line (@lines) {
		if ($line =~ /mRNA/) {
			$drop_flag = 0;
			last;
		}
		else {
			$drop_flag = 1;
		}
	}
	if ($drop_flag) {
		print "Deleting entry of $gene_id.$n\t", $lines[1], $n;
		delete $man_hash{$gene_id}; 
	}
}

print scalar keys %man_hash, " genes remain.$n";

# Print cleaned ManCur file
open (my $out, '>', $man.'.coding') or die "Cannot open file '${man}.coding': $!$n";
print $out "##gff-version 3$n";

foreach my $gene_id (keys %man_hash) {
	my @lines = @{$man_hash{$gene_id}};
	print $out join ($n, @lines), $n;	
}

print "Done!$n";



################# SUBS ###############################

=head2 slurp_file

 Usage   : @file_lines = slurp_file($file);
 Function: Returns array of lines in a given (incl PATH) file
 Returns : @file_lines
 Args    : /PATH/file
 
=cut

sub slurp_file {
	my $file = shift @_; 
	
	# Open filehandle
	open my $FILE, '<', $file or die "Couldn't open \"$file\": $!";
	
	# Read in entire file content at once 
	my $file_content = do { local $/; <$FILE> };
	
	# Close filehandle
	close $FILE or die "Could't close \"$file\": $!";
	
	# Convert to unix-style line breaks
	$file_content =~ s/\r\n?/\n/g;
	
	# Split file content on line breaks
	my @file_lines = split ( '\n', $file_content );

	return @file_lines;
}

###############################################
