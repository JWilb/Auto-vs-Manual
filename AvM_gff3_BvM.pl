#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;

use FindBin;
use lib "$FindBin::RealBin/"; # find GAL
use File::Path qw(remove_tree);
use Cwd;

use GAL::Annotation;
use GAL::List;

my $n = "\n";

################################################
# a-v-m_gff3_BvM.pl
# 
# Input: BRAKER-models.gff3, MAKER-models.gff3, Species name
#
# - for each exon in BRAKER models
#	- how many exons does the respecitive gene have?
#	- is there an overlapping MAKER gene model?
#		- if yes: does it have more exons?
# - for each exon in MAKER models (if not already covered)
#	- does a BRAKER gene overlap?
# - write log file to see what happened
# - print new fa if desired
#	- BRAKER's single-exon genes that do not overlap with MAKER
#	- Header with gff data
#	- nucleotide seq
#
# Output: Species_BvM.log
#
################################################


my $usage = "
a-v-m_gff3_BvM.pl

perl a-v-m-gff3_BvM.pl species=SPECIES A=/PATH/to/models.gff3 B=/PATH/to/other-models.gff3 [--ign]

This tool compares the (automatic) annotations of file A to file B to answer the question 
'How many genes found by annotator A overlap an annotation by B (and vice versa)?'. 
[! This was designed for A=BRAKER, B=MAKER and assumes that both annotations are of
the same species using the same genome assembly. !]

A log file is written, where internal ID, scaffold, start, stop, strand and count of exons 
of A-models is kept together with the same data for overlapping B-models. (Species_BvM.log)

--ign
 - ignore strandedness of models
-- segs FASTA
 - print new fa if desired 
	- BRAKER's single-exon genes that do not overlap with MAKER
	- Header with gff data
	- nucleotide seq

Already existing output files are overwritten...

Input from i5k expected.


Jeanne Wilbrandt, 20.07.2017
";


# Get Options
my ($help, $segs, $ignore_strand);

my $opt_success = GetOptions('help|h' 	=> \$help,
				'segs=s'		=> \$segs,
				'ign|i'	=> \$ignore_strand,
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
die "FATAL: You need to specify 3 arguments: species, A and B. $n$usage" if scalar @args != 3;

my ($species, $A, $B);
foreach my $arg (@args) {
	$arg =~ /^(\w*)=(.*)$/ or die "FATAL: Argument '$arg' does not match the requirements!$n$usage";
	my $val = $2;
	
	if ($1 =~ 'species') {
		$species = $val;
	}
	elsif($1 =~ 'A') {
		$A = $val;
		die "FATAL: Annotation file '$A' does not exist!$n$usage" if !-e $A;
	}	
	elsif($1 =~ 'B') {
		$B = $val;
		die "FATAL: Annotation file file '$B' does not exist!$n$usage" if !-e $B;
	}
	else {
		die "FATAL: Arguments do not match the requirements!$n$usage";
	}
}

print "# Loading annotations...$n";
	# Load A annotation
	
		## Get annotation
		my $A_annotation	= GAL::Annotation->new($A, $segs) or die "FATAL: Cannot load (A) annotation into annotationDB! ($!)$n";
	
		## Get annotated features and types
		my $A_features	= $A_annotation->features or die "FATAL: Cannot load (A) annotation into featureDB! ($!)$n";
		
	# Load B annotation
	
		## Get annotation
		my $B_annotation	= GAL::Annotation->new($B) or die "FATAL: Cannot load (B) annotation into annotationDB! ($!)$n";
	
		## Get annotated features and types
		my $B_features	= $B_annotation->features or die "FATAL: Cannot load (B) annotation into featureDB! ($!)$n";
		

my $overlaps_found 	= 0;
my $no_overlaps 	= 0;


print "# Populating hashes...$n";
#			ID => locus strand exon count
my %A_data = get_geneInfo($A_features, 'A');
my %B_data = get_geneInfo($B_features, 'B');

print "... Loaded ", scalar keys %A_data, " transcripts for file A ($A)$n";
print "... Loaded ", scalar keys %B_data, " transcripts for file B ($B)$n";


print "# Searching overlaps...$n";

#			B_ID => B_locus B_strand B_e-count M_ID M_locus M_strand M_e-count
my %overlaps;

my %segs if $segs;

foreach my $A_gene (keys %A_data) {
	my $A_gene_overlap_flag = 0;
	
	# extract loc etc info
	${$A_data{$A_gene}}[0] =~ /^(.*?):(\d*)-(\d*)$/;
	my $loc_id 	= $1; 
	my $start 	= $2;
	my $stop 	= $3;
	my $strand	= ${$A_data{$A_gene}}[1];
	my $e_count	= ${$A_data{$A_gene}}[2];
	my $nuc_seq	= ${$A_data{$A_gene}}[3];
	
	foreach my $B_gene (keys %B_data) {
		# next if already checked!
		#next if grep (/^$pot_overlap_M$/, keys %overlapping_transcripts);
		
		# extract query loc etc info
		${$B_data{$B_gene}}[0] =~ /^(.*?):(\d*)-(\d*)$/;
		my $B_loc_id 	= $1;
		my $B_start 	= $2;
		my $B_stop 		= $3;
		my $B_strand	= ${$B_data{$B_gene}}[1];
		my $B_e_count	= ${$B_data{$B_gene}}[2];
		
		# must be on same scaffold and same strand
		next if $loc_id ne $B_loc_id;
		next if $strand ne $B_strand and !$ignore_strand;
		
		# check for overlap by negative control: if M_stop is before B_start or M_start past B_stop, it does not overlap!
		my $keep_flag = 1;
		if ($B_stop < $start) {
			$keep_flag = 0;
		}
		elsif ($B_start > $stop) {
			$keep_flag = 0;
		}
		
		# overlap confirmed?
		if ($keep_flag) { 
			# store data for printing
			push @{$overlaps{$A_gene}}, join ("\t", ${$A_data{$A_gene}}[0], $strand, $e_count, 
											$B_gene, ${$B_data{$B_gene}}[0], $B_strand, $B_e_count);
			# raise counter
			++$overlaps_found;
			# remember something was found
			$A_gene_overlap_flag = 1;
		}
	}
	
	# if nothing was found
	if ($A_gene_overlap_flag == 0) {
		# raise counter of nothing-founds
		++$no_overlaps;
		# store data incl info 'nothing found' for printing
		push @{$overlaps{$A_gene}}, join ("\t", ${$A_data{$A_gene}}[0], $strand, $e_count, 
										'NA', 'NA', 'NA', 'NA');
										
					
		# if single-exon-gene of A without overlap in B
		if ($segs) {
			# prepare header for output fa
			my $head = '>'.$A_gene.'|'.$loc_id.'|'.$start.'|'.$stop.'|'.$strand.$n;
			# push into segs-hash (header => nucleotide sequence
			$segs{$head} = $nuc_seq;
		}
	}
	
}

print "... Found overlaps: $overlaps_found$n...Without overlap: $no_overlaps$n";


print "# Printing...$n";

open (my $out_FH, '>', $species.'_BvM.log') or die "Cannot open ${species}_BvM.log! $1$n";
# print input files
print $out_FH "# (A): $A$n";
print $out_FH "# (B): $B$n";
# print header
print $out_FH join ("\t", "(A) gene ID","(A) locus","(A) strand","(A) exon count","(B) gene ID","(B) locus","(B) strand","(B) exon count"), $n;

# count genes that overlap multiple times (and of those single-exon genes)
my $multi_lap_count = 0;
my $single_e_multi = 0;

# go through collected data
foreach my $key (keys %overlaps) {
	my @lines = @{$overlaps{$key}};
	
	# if multiple overlaps were found
	if (scalar @lines >=2) {
		# raise counter
		++$multi_lap_count;
		# check whether it is a single-exon gene
		my @items 		= split ("\t", $lines[0]);
		my $A_e_count 	= $items[2];
		# if so, raise counter
		++$single_e_multi if $A_e_count == 1;
	}
	
	# print each data line
	foreach my $line (@lines) {
		print $out_FH $key, "\t", $line, $n;
	}
}

print "... Found $multi_lap_count (A) genes overlapping multiple others in (B)$n";
print "... of which $single_e_multi (A) genes are single-exonic.$n";

if ($segs) {
	open (my $out_fa_FH, '>', $species.'_single_exon_genes_noOverlap.fa') or die "Cannot open ${species}_single_exon_genes_noOverlap.fa! $1$n";
	
	foreach my $head (keys %segs) {
		print $out_fa_FH $head, $segs{$head}, $n;
	}
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
=head2 get_geneInfo

 Usage   : %data = get_geneInfo($genes);
 Function: Returns hash: geneID => (locus, strand, exon count)
 Returns : %data
 Args    : $genes (GAL iterator)
 
=cut


sub get_geneInfo {
	
	my $features		= shift @_;
	my $A_or_B			= shift @_;
	
	# Get an iterator for all genes
	my $genes = $features->search({type => 'gene'});
	## Die if no genes are in the gff, something is wrong/file cannot be used
	die "FATAL ERROR: No genes available in your (A) annotation file! $n" if $genes == 0;
	
	my %data;

	while (my $gene = $genes->next) {
		## Get all transcripts assigned to one gene
		my $transcripts 		= $gene->mRNAs;
				
		## If there are none, go to next gene
		next if $transcripts	== 0;
			
		## Only take the longest transcript per gene
		my $transcript 		= $transcripts->longest;
				
		## determine strand
		my $strand			= $transcript->strand;
				
		## determine number of exons
		my $exon_count		= $transcript->exons->all;
				
		## determine locus (scaffold:start-stop)
		my $locus 			= $transcript->locus;
				
		## remember gene ID
		my $geneID 			= $gene->feature_id;
		
		my $nuc = 'NA';
		if ($segs and $A_or_B eq 'A') {
			$nuc = $gene->seq;
		}
		
		push @{$data{$geneID}}, $locus, $strand, $exon_count, $nuc;
	}
	
	return %data;
}



###############################################
