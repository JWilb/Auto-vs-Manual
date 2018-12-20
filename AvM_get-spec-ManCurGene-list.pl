#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;

my $n = "\n";

################################################
# a-v-m_get-spec-ManCurGene-list.pl
# 
# Input:	auto_list.txt (list of gene IDs of interest)
#			extractor.log (list of manual genes with their respecitive predecessors/overlapping auto genes)
#			ManCur.gff	  (gff with all manually annotated coding genes)
#
# - for each gene from list, find line where 'auto note' matches ID=...;
# - get ID of manual gene from 'ManCur note'
# - find this gene in ManCur.gff and print to new gff (ManCur_interest.gff)
#
# Output: ManCur_interest.gff3
#
################################################


my $usage = "
a-v-m_get-spec-ManCurGene-list.pl

perl a-v-m_get-spec-ManCurGene-list.pl \\
	species=SPEC \\
	type=text \\
	list=/PATH/to/auto-list.txt \\
	log=/PATH/to/extractor.log \\
	gff=/PATH/to/ManCur.gff3

This tool extracts the manually curated genes overlapping those auto-genes 
that meet certain criteria (i.e., genes of interest, e.g. long).
The found genes are printed to a new gff file.

Already existing output files are overwritten...

Input from i5k expected.


Jeanne Wilbrandt, 08.02.2017
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

die "FATAL: You need to specify 5 arguments: species, type, list, log, and gff. $n$usage" if scalar @args != 5;

my ($species, $type, $list, $log, $gff);
foreach my $arg (@args) {
	$arg =~ /^(\w*)=(.*)$/ or die "FATAL: Argument '$arg' does not match the requirements!$n$usage";
	my $val = $2;
	
	if ($1 =~ 'species') {
		$species = $val;
	}
	elsif($1 =~ 'type') {
		$type = $val;
	}	
	elsif($1 =~ 'list') {
		$list = $val;
		die "FATAL: List file '$list' does not exist!$n$usage" if !-e $list;
	}	
	elsif($1 =~ 'log') {
		$log = $val;
		die "FATAL: Log file file '$log' does not exist!$n$usage" if !-e $log;
	}
	elsif($1 =~ 'gff') {
		$gff = $val;
		die "FATAL: GFF file file '$gff' does not exist!$n$usage" if !-e $gff;
	}
	else {
		die "FATAL: Arguments do not match the requirements!$n$usage";
	}
}

# Slurp list
print "# Slurping files...$n";
my @list_lines 	= slurp_file($list);
my @log_lines 	= slurp_file($log);
my @gff_lines	=slurp_file($gff);

# Find ManCur genes that overlap searched Auto genes
print "# Searching genes...$n";

my %ManCur_of;

## For each entry (ID) in list, find log line where $col[11] matches ID
foreach my $ID (@list_lines) {
	foreach my $line (@log_lines) {
		next if $line =~ /^#/;
		next if $line =~ /^ManCur/;
		
		my @cols = split ("\t", $line);
		
		next if scalar @cols < 11;

		# match $col[11] = auto note?
		if ($cols[11] =~ /ID=$ID;/) {
			# get manCur ID from ManCur note
			$cols[5] =~ /ID=(\w*);/;
			# put into storage hash (autoID => ManCurID, ...)
			push @{$ManCur_of{$ID}}, $1; 
		}
	}
}

# Extract genes of interest from gff
print "# Getting genes from gff...$n";

my %gff_lines_of;

## For each found manCur ID, find gff lines
foreach my $auto_ID (keys %ManCur_of){
	my @ManCur_IDs = @{$ManCur_of{$auto_ID}};
	
	foreach my $ManCur_ID (@ManCur_IDs) {
		next if !$ManCur_ID;
		foreach my $line (@gff_lines) {
			if ($line =~ /ID=$ManCur_ID/) {
				# if line matches ID, put into hash (ID => line)
				push @{$gff_lines_of{$ManCur_ID}}, $line;
			}
		}
	}
}

# Print to new gff
print "# Printing new gff...$n";

open (my $out_gff, '>', $species.'_ManCur_'.$type.'.gff3') or die "Cannot open file '${species}_ManCur_${type}.gff3'': $!$n";

print $out_gff "##gff-version 3$n";

foreach my $ID (keys %gff_lines_of) {
	print $out_gff join ($n, @{$gff_lines_of{$ID}}), $n; 
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
