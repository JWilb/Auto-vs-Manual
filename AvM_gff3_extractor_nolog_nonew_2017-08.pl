#!/usr/bin/perl


use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;

my $n = "\n";

################################################
# a-v-m_gff3_extractor_nolog_nonew.pl
# 
# Input: models.gff3 (automatic annotation), OGS_ManCur.gff3 (only manually curated models)
#
# - for each gene in manually curated models, find the original which is overlapped by it
# - extract original / automatic models from models.gff3 and 
#		write to auto_models.gff3
# - write log file to see what happened
#
# Output: auto_models.gff3
#
################################################


my $usage = "
a-v-m_gff3_extractor_nolog_nonew.pl

perl a-v-m-gff3_extractor.pl species=SPECIES auto=/PATH/to/NALmod.gff3 man=/PATH/to/OGS_mancur.gff3

This tool uses the manually curated models to find their predecessors. It writes them to 
a new file: predecessors.gff3. 

A log file is written, where internal ID, scaffold, start, stop, strand and notes of manual models is kept
together with the same data for overlapping automatically annotated models. (extractor.log)

Another new file is produced: ManCur.gff3.nonew, where all genes of ManCur are included that have 
at least one overlapping auto model (no newly annotated genes).

Already existing output files are overwritten...

Input from i5k expected.


Jeanne Wilbrandt, 01.02.2017
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

die "FATAL: You need to specify 3 arguments: species, auto and man. $n$usage" if scalar @args != 3;

my ($species, $auto, $man);
foreach my $arg (@args) {
	$arg =~ /^(\w*)=(.*)$/ or die "FATAL: Argument '$arg' does not match the requirements!$n$usage";
	my $val = $2;
	
	if ($1 =~ 'species') {
		$species = $val;
	}
	elsif($1 =~ 'auto') {
		$auto = $val;
		die "FATAL: Automatic annotation file '$auto' does not exist!$n$usage" if !-e $auto;
	}	
	elsif($1 =~ 'man') {
		$man = $val;
		die "FATAL: Manually curated file file '$man' does not exist!$n$usage" if !-e $man;
	}
	else {
		die "FATAL: Arguments do not match the requirements!$n$usage";
	}
}


# Slurp ManCur.gff3
print "# Slurping ManCur file ($man)...$n";
my @man_lines = slurp_file($man);

## important fields: scaffold (col 0), gene (2), start (3), stop (4), strand (6), notes (8)
## extract this data from man_lines and store in man_hash
my %man_hash; # internal id => {gene => (gene items)}, {lines => (lines)}

#my $gene_id 	= 'NA';
#my $mrna_parent = 'NA';
#my $mrna_id		= 'NA';

my $id = 1;

foreach my $line (@man_lines) {
	
	next if $line =~ /#/;
	
	my @items = split("\t", $line);
	if ($items[2] =~ /gene/) {
		push @{$man_hash{$id}}, ($items[0], $items[3], $items[4], $items[6], $items[8]);
		++ $id;
	}	
	
	
	
	################################################# Völlig unnötig kompliziert, brauche ja nicht man 
	############################################################################ so ausführlich (hab ich ja schon, sondern auto -_-
	#next if $line =~ /#/;
	
	#my @items = split("\t", $line);
	#if ($items[2] =~ /gene/) {
		## get gene id
		#$items[8] =~ /^ID\=([^;]*);/ or die "Cannot find ID: $line$n";
		#$gene_id = $1;
		
		#push @{$man_hash{$gene_id}{'gene'}}, ($items[0], $items[3], $items[4], $items[6], $items[8]);
		#push @{$man_hash{$gene_id}{'lines'}}, $line;

	#}
	#elsif ($items[2] =~ /mRNA/) {
		## get mRNA id
		#$items[8] =~ /^ID\=([^;]*);.*Parent\=([^;]*);?/ or die "Cannot find Parent: $line$n";
		#$mrna_id = $1;
		#$mrna_parent = $2;
		
		#push @{$man_hash{$mrna_parent}{'lines'}}, $line; # mrna_parent = gene id
		
	#}
	#else {
		#$items[8] =~ /Parent\=([^;]*);?/ or die "Cannot find Parent: $line$n";
		#my $parent = $1;
		
		#if ($parent eq $mrna_id) {
			#push @{$man_hash{$gene_id}{'lines'}}, $line;
		#}
		#else {
			#print "WARN: Line $line is left over! $n";
		#}
	#}
	
} 


print "   Found ", scalar keys %man_hash, " manually curated genes.$n";

# Slurp auto_annot file
print "# Slurping Auto file ($auto)...$n";
my @auto_lines = slurp_file($auto);

## find gene that is overlapped by ManCur -------------------------------------note: split/merged gene spanning multiple scaffolds will not be noticed!
## give same id as ManCur, store all lines belonging to the gene in auto_hash
my %auto_hash;

print "# Searching for overlaps...$n";
foreach my $id (sort keys %man_hash) {
	#my $q_scaff 	= $man_hash{$id}{'gene'}[0];
	#my $q_start 	= $man_hash{$id}{'gene'}[1];
	#my $q_stop		= $man_hash{$id}{'gene'}[2];
	#my $q_strand	= $man_hash{$id}{'gene'}[3];
	
	my $q_scaff 	= $man_hash{$id}[0];
	my $q_start 	= $man_hash{$id}[1];
	my $q_stop		= $man_hash{$id}[2];
	my $q_strand	= $man_hash{$id}[3];
	
	my $gene_id = 'NA';
	
	foreach my $line (@auto_lines) {
		next if $line =~ /#/;
		
		my @items = split ("\t", $line);
		
		# find same scaffold as query
		if ($items[0] =~ /^$q_scaff$/) {
			# find line with gene
			if ($items[2] =~ /gene/) {
				
				my $start 	= $items[3];
				my $stop	= $items[4];
				my $strand	= $items[6];
				
				my $keep_flag = 1;
				
				# check for overlap by negative control: if M_stop is before B_start or M_start past B_stop, it does not overlap!
				if ($q_stop < $start) {
					$keep_flag = 0;
				}
				elsif ($q_start > $stop) {
					$keep_flag = 0;
				}
				
				# do not keep if not on the same strand!
				$keep_flag = 0 if ($q_strand ne $strand);
				
				# if overlap found, collect all lines for this gene
				if ($keep_flag == 1) {
					$items[8] =~ /ID=(\w*);/;
					$gene_id = $1;
					push @{$auto_hash{$id.'_'.$gene_id}}, $line;
					
					#print "line: $line $n";
				}
			} # gene
			
			# or keep line belonging to already found gene
			else {
				if ($gene_id !~ /NA/) {
					push @{$auto_hash{$id.'_'.$gene_id}}, $line if $items[8] =~ /$gene_id/;
				}
			} # no gene 
		} # scaffold 
	} # auto lines
} # man_hash ids


print "   Found ", scalar keys %auto_hash, " overlaps of man in auto genes.$n";


# print predecessors.gff3 (models that were predecessors of ManCur models)
print "# Printing predecessor gff$n";
open (my $out_orig, '>', $species.'_predecessors.gff3') or die "Cannot open file '${species}predecessors.gff3': $!$n";

print $out_orig "##gff-version 3$n";

foreach my $id (keys %auto_hash) {
	print $out_orig join ($n, @{$auto_hash{$id}}), $n;	
}


# print log file to see what happened
print "# Printing log file$n";
open (my $out_log, '>', $species.'_a-v-m_extractor_overlapping-genes.log') or die "Cannot open file '${species}a-v-m_extreactor.log': $!$n";

print $out_log "# Auto file: $auto$n";
print $out_log "# Man file: $man$n"; 

print $out_log join ("\t",  'ManCur ID', 'ManCur Scaff', 'ManCur Start', 'ManCur Stop', 'ManCur Strand', 'ManCur Note',
							'auto ID', 	'auto Scaff', 'auto Start', 'auto Stop', 'auto Strand', 'auto Note',
				), $n;
				
my @auto_ids = sort keys %auto_hash;

#print Dumper \%man_hash;
my %found_hash;
my %ManCur_of;
my @keep_mans;

foreach my $id (sort {$a cmp $b} keys %man_hash) {
	# first man feats
	print $out_log join ("\t", $id, @{$man_hash{$id}});
	print $out_log "\t";
	#print "id: $id$n";
	#print join ("\t", $id, @{$man_hash{$id}}), $n;
	
	my $gene_count = 0;
	# then extract the relevant predecessor feats
	my $found_count = 0;
	foreach my $auto_id (@auto_ids) {
		if ($auto_id =~ /^${id}_(.*)/) { 
			my $Man_ID = $1;
			
			++$found_count;
			
			#print "auto id: $auto_id$n";
			my @lines = @{$auto_hash{$auto_id}};
	
			foreach my $line (@lines) {
				if ($line =~ /gene/) {
					print $out_log join ("\t", "\t", @{$man_hash{$id}}), "\t" if $gene_count > 0;
					my @items = split("\t", $line); 
					# to identify split genes, put auto_id => man_ID into hash
					$items[8] =~ /ID=([^;]);/;
					my $Auto_ID = $1;
					push @{$ManCur_of{$Auto_ID}}, $Man_ID;
					
					print $out_log join ("\t", ($auto_id, $items[0], $items[3], $items[4], $items[6], $items[8]));
					print $out_log $n;
					#print join ("\t", $items[0], $items[3], $items[4], $items[8]), $n;
		
					++$gene_count;
					#print "gene count $gene_count$n";
				}
			}
		}
	}
	print $out_log $n if $found_count == 0;
	push @keep_mans, $id if $found_count != 0;
	
	++$found_hash{$found_count}
}

# Print new ManCur.gff without new models
print "# Printing new ManCur gff without new models$n";
print "   Keeping ", scalar @keep_mans, " manually curated genes (no new models)$n";

open (my $out_newManCur, '>', $species.'_ManCur_models_noNew.gff3') or die "Cannot open file '${man}.noNew': $!$n";
print $out_newManCur "##gff-version 3$n";

my %owners;

foreach my $int_id (@keep_mans) {
	my $note = $man_hash{$int_id}[4];
	$note =~ /ID=([^;]*);/;
	my $ID = $1;
	# go through gff and find lines matching this ID, print to file
	foreach my $line (@man_lines) {
		next if $line =~ /#/;
	
		my @items = split("\t", $line);
	
		# find line for this ID
		if ($items[8] =~ /$ID/) {
			print $out_newManCur $line, $n;
			# count annotations made by owner
			if ($items[2] =~ /gene/) {
				$items[8] =~ /owner=([^;]*)(;|$)/;
				my $name = 'NA';
				if ($1) {
					$name = $1;
				}
				++$owners{$name};
			}
		}
	}
}

# Print collected data

foreach my $amount (sort keys %found_hash) {
	if ($amount == 0) {
		print "$found_hash{$amount}\t ManCur models with $amount overlapping auto models\t NEW models$n";
	}
	elsif ($amount == 1) {
		print "$found_hash{$amount}\t ManCur models with $amount overlapping auto models\t UPDATED models$n";
	}
	elsif ($amount > 1) {
		print "$found_hash{$amount}\t ManCur models with $amount overlapping auto models\t MERGED models$n";
	}
}

my %split_count;
foreach my $auto_id (keys %ManCur_of) {
	++$split_count{scalar @{$ManCur_of{$auto_id}}}
}

foreach my $splits (sort keys %split_count) {
	if ($splits == 1) {
		print "$split_count{$splits}\t auto models were not split$n";
	}
	else {
		print "$split_count{$splits}\t auto models were split into $splits ManCur models$n";
	}
}

print "Owners:$n";
foreach my $name (sort keys %owners) {
	print $name, "\t", $owners{$name}, $n;
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
