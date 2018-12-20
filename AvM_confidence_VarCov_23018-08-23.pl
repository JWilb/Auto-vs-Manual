#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;
use Statistics::R;

$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Maxdepth = 2;

my $n = "\n";

################################################
# confidence_VarCov.pl
#
# for the AvM project
#	for each species (7)
#	for each set (3: auto, manual, new)
#	for each parameter combination in Fig 2 (5)
#		- exon count vs transcript length
#		- exon GC vs exon count
#		- exon length vs exon count
#		- intron GC vs intron count
#		- intron length vs intron count
#
# get complete set
#	take 5000 samples of size 1000
#	calculate correlation coefficient for each sample
#	draw confidence interval plot
#	add correlation coefficient for complete set and original sample (analyzed)
#
###############################################

my $sample_R = 1000; # how many samples -> 5000
my $sample_N = 1000; # how many values inside each sample -> 1000

### Prepare data structure

# data sets
# - Species array (7)
#	- set array (3)
#		- sample_id array (5000 + complete + analyzed)
#			- value array (1000/#complete/#analyzed)

my @species = ('AGLA', 'LDEC', 'AROS', 'OABI', 'CLEC', 'OFAS', 'FOCC');
my @sets	= ('auto', 'man', 'new');
my @combos	= ('tL_eC', 'eGC_eC', 'eL_eC', 'iGC_iC', 'iL_iC');


### Get data
my %data; 

# read files
my $path_1 = "/data/home/Desktop/PhD/02c_AvM/COGNATE_a-v-m_May/"; # auto compl and man compl
my $path_2 = "/u/people/jeannewb/Documents/00_Projects/AvM/COGNATE_analyzed_2018-08/"; # auto subset, man subset, new subset

# complete
foreach my $sp (@species) {
	foreach my $set (@sets) {
		next if $set eq 'new'; # new is only analyzed
		
		my %c_lines = combine_data($path_1, $sp, $set, 'compl'); # get the five parameters; key 1 = header
		# tr_ID, tr length, exon count, exon length, exon GC, intron count, intron length, intron GC
		
		# keep all lines as complete set
		push @{$data{$sp}{$set}}, {'complete' => \%c_lines};
		
		# draw samples
		foreach my $sample (1..$sample_R) { # how many samples? - > 5000
			# get random indices
			my @rand_indices;
			
			for (my $i = 0; $i <= $sample_N; ++$i) { # how large will the sample be? -> 1000
				my $rand_nr = int(rand(scalar keys %c_lines));
				push @rand_indices, ($rand_nr -1);
			}
			
			# get data for random indices
			my %sample_lines;
			foreach my $index (@rand_indices) {
				next unless defined $c_lines{$index};
				@{$sample_lines{$index}} = @{$c_lines{$index}};
			}
			
			push @{$data{$sp}{$set}}, {$sample => \%sample_lines};
		}
		
	}
} 


# analyzed (original samples)
foreach my $sp (@species) {
	foreach my $set (@sets) {
				
		my %c_lines = combine_data($path_2, $sp, $set, 'subset'); # get the five parameters; key 1 = header
		# tr_ID, tr length, exon count, exon length, exon GC, intron count, intron length, intron GC
		
		# keep all lines as complete set
		push @{$data{$sp}{$set}}, {'analyzed' => \%c_lines};
		
	}
} 


	#print Dumper \%data; exit;
	#print $data->{'AGLA'}{'auto'}{'analyzed'}{'1'}, $n; exit;

# Calculate
# Create a communication bridge with R and start R
my $R = Statistics::R->new();


open (my $out, '>', 'confid_VarCov_results.csv') or die "Cannot open file confid_VarCov_results.csv: $!$n";
print $out join("\t", 'Species', 'Set', 'Table', 'Sample size', 'eC_tL', 'eGC_eC', 'eL_eC', 'iGC_iC', 'iL_iC'), $n; # header


foreach my $species (sort keys %data) {
	#print "Species: ", $species, $n;
	my %hoh = %{$data{$species}};
	
	foreach my $set (sort keys %hoh) {
		#print "Set: ", $set, $n; 
		my @aohoh = @{$hoh{$set}};
		#print Dumper \@{$data{$species}{$set}}; exit;
		#print @{$data{$species}{$set}}, $n; exit; # list of hash references
		
		foreach my $tab_ref (@aohoh) {
			#print "Table ref: ", $tab_ref, $n; 
			my %hohohoh = %{$tab_ref};
			
			foreach my $tab (sort keys %hohohoh) {
				print "$species : $set : $tab$n";
				my %hohohohoh = %{$hohohoh{$tab}};
				
				# get columns as vectors
				my @tr_lengths;
				my @e_counts;
				my @e_lengths;
				my @e_GCs;
				my @i_counts;
				my @i_lengths;
				my @i_GCs;
				
				foreach my $line (keys %hohohohoh) {
					#print "Line: ", $line, $n; 
					my @items = @{$hohohohoh{$line}};
					#print Dumper \@items; exit;

					push @tr_lengths, 	$items[1];
					push @e_counts, 	$items[2];
					push @e_lengths, 	$items[3];
					push @e_GCs, 		$items[4];
					push @i_counts, 	$items[5] if $items[5] ne 'NA';
					push @i_lengths,	$items[6] if $items[5] ne 'NA';
					push @i_GCs, 		$items[7] if $items[5] ne 'NA';
				}
				
				# get spearman correlation coefficient for each combo
				my $sample_size = scalar @tr_lengths; 
				
				# exon count vs transcript length
				my $eC_tL 	= get_coeff(\@e_counts, \@tr_lengths);
				#print $eC_tL, $n;
				# exon GC vs exon count
				my $eGC_eC 	= get_coeff(\@e_GCs, \@e_counts);
				#print $eGC_eC, $n;
				# exon length vs exon count
				my $eL_eC	= get_coeff(\@e_lengths, \@e_counts);
				#print $eL_eC, $n;
				# intron GC vs intron count
				my $iGC_iC	= get_coeff(\@i_GCs, \@i_counts);
				#print $iGC_iC, $n;
				# intron length vs intron count
				my $iL_iC	= get_coeff(\@i_lengths, \@i_counts);
				#print $iL_iC, $n;
				
				print $out join("\t", $species,$set,$tab,$sample_size,$eC_tL, $eGC_eC, $eL_eC, $iGC_iC, $iL_iC), $n;
			}
		}	
	}
}

close($out);











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

#########################################################

sub combine_data{
	my $path 	= shift @_;
	my $sp 		= shift @_;
	my $set		= shift @_;
	my $suffix	= shift @_;
	
	my %c_lines;
	
	# read files
	my @tr_lines;
	my @e_lines;
	my @i_lines;
	
	if ($suffix eq 'compl') {
		@tr_lines = slurp_file($path."COGNATE_${sp}_${set}_${suffix}/COGNATE_${sp}_${set}_${suffix}_07-transcript_general.tsv");
		@e_lines  = slurp_file($path."COGNATE_${sp}_${set}_${suffix}/COGNATE_${sp}_${set}_${suffix}_09-transcript_exons.tsv");
		@i_lines  = slurp_file($path."COGNATE_${sp}_${set}_${suffix}/COGNATE_${sp}_${set}_${suffix}_10-transcript_introns.tsv");
	}
	else {
		@tr_lines = slurp_file($path."COGNATE_${sp}_${set}_${suffix}_07-transcript_general.tsv");
		@e_lines  = slurp_file($path."COGNATE_${sp}_${set}_${suffix}_09-transcript_exons.tsv");
		@i_lines  = slurp_file($path."COGNATE_${sp}_${set}_${suffix}_10-transcript_introns.tsv");
	}
	
	
	# skip header lines
	shift @tr_lines;
	shift @e_lines;
	shift @i_lines;
	
	# get relevant line items
	my $line_count = 1; 
	foreach my $line (@tr_lines) {
		my @items 	= split ("\t", $line);
		my @keep	= ($items[0], $items[1]); # tr_id, tr_length
		push @{$c_lines{$line_count}}, @keep;
		++$line_count;
	}
	
	$line_count = 1; # reset line count
	foreach my $line (@e_lines) {
		my @items 	= split ("\t", $line);
		my @keep	= ($items[1], $items[3], $items[5]); # count, length, GC
		push @{$c_lines{$line_count}}, @keep;
		++$line_count;
	}
	
	$line_count = 1; 
	foreach my $line (@i_lines) {
		my @items 	= split ("\t", $line);
		my @keep	= ($items[1], $items[3], $items[5]); # count, length, GC
		push @{$c_lines{$line_count}}, @keep;
		++$line_count;
	}
	
	return %c_lines;
}

##################################################

sub get_coeff {
	my $x = shift @_;
	my $y = shift @_;
	
	$R->set('x', $x);
	$R->set('y', $y);
	#$R->run(qq'coeff <- cor(as.numeric(x), as.numeric(y),use="complete",method="spearman")'); # spearman correlation coefficient
	
	$R->run(q'coeff <- summary(lm(formula = as.numeric(x) ~ as.numeric(y)))$coefficients[2,1]'); # slope of the linear regression
	my $coeff = $R->get('coeff');
	return $coeff;
}
