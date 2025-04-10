#!/usr/bin/perl -w


#==============================================================================
# Script : JAM.pl
# Version 1.0
# Author :   Olivier Kraif 
# Purpose : Just A Multialigner - Multilingual sentence aligner designed for more than 2 languages

# Synopsis : perl -w JAM.pl InputFile* [options*]
# Otions :
#	--inputPath					: if no input files are set, take all the files of the input dir for which names follows the pattern : NAME.LAN.EXT
#	--inputFilePattern			: an alternative name pattern to use with inputPath
#	--outputPath the/output/path: set the output path for output files
#	--inputFormat				: txt (with one string per line) or ces
#	--encoding					: utf8|iso-8859-1|ansi etc.
#	--outputFormat				: txt (with one string per line), or ces, or tmx, or csv
#	--languages 				: list of languages corresponding to the input files (useless if the input files are in the forme *.LANG.ces where LANG indicates the language code
#	--combinations				: ordered list of language combinations for language tiling (e.g. en-fr-es fr-it-en es-pt-en pt-es-it nl-en-fr sv-da-nl de-da-nl fi-en-sv el-gr-en)
#	--pivot						: if no combination is given, a language may be used as a pivot
#	--anchorPointFile CSVFILE	: load some anchor points to initialize aligning process
#	--anchorPointMarkup 		: anchor points markup for hard segmentation
#	--finalCompletion			: use the Gale & Church algorithme to finally complete aligning process
#	--printPairwiseAlignment	: print all the alignments two by two
#	--printMergedAlignment		: print all the pairwise alignments merged into a single file
#	--verbose					: print messages while processing
#	--printLog					: print messages in log file
# 	--printTokenNum				: print the token numbers for each language combination
#	--printTokenList			: print the token list for each language combination
#	--printBestPairComb			: print the best pairwize a posteriori combination
#	--lexicon					: name of the file containing a multilingual lexicon


#==============================================================================



# Pragma and dependencies
use locale;
use strict;
use IO::Handle;
use lib "/home/kraifo/perl5/lib/perl5/";
#~ use Chart::Gnuplot;
STDOUT->autoflush(1); 
no warnings 'recursion';
use Getopt::Long;
#~ use Data::Dumper;

#==============================================================================
# reading arguments

my $inputPath=".";
my $inputFilePattern=qr/(.*)[.]\w\w[.]\w\w+$/;
my $outputPath=".";
my @files;
my $anchorPointFile;
my $anchorPointMarkup;
my $finalCompletion='';		# general completion at the end, after the reordering step, using the DTW algorithme
my $printPairwiseAlignment='';
my $printMergedAlignment='';
my $printTokenNum='';
my $printTokenList='';
my $printBestPairComb='';
my $inputFormat="ces";
my $outputFormat="ces";
my $pivot;
my @allLang;			# contains the list of all the aligned lang e. g. : ("en", "fr", "it", "es") 
my @combinations;
my $verbose=0;
my $encoding="utf8";
my $printLog=1;				# 1 to print detailed execution log in LOG file
my $logDir=".";
my $lexicon="";

# reading ARGS
if (@ARGV) {
	my $i=0;
	while ($i<=$#ARGV) {
		if ($ARGV[$i]=~/^--/) {
			last;
		}
		push(@files,$ARGV[$i++]);
	}
	GetOptions (
		'inputPath=s' => \$inputPath,
		'inputFilePattern=s' => \$inputFilePattern,
		'outputPath=s' => \$outputPath,
		'inputFormat=s' => \$inputFormat,
		'outputFormat=s' => \$outputFormat,
		'verbose=s' => \$verbose,
		'printLog' => \$printLog,
		'anchorPointFile=s' => \$anchorPointFile,
		'anchorPointMarkup=s' => \$anchorPointMarkup,
		'languages=s' => \@allLang,
		'combinations=s' => \@combinations,
		'encoding=s' => \$encoding,
		'pivot=s' => \$pivot,
		'finalCompletion' =>  \$finalCompletion,
		'printPairwiseAlignment' => \$printPairwiseAlignment,
		'printMergedAlignment' => \$printMergedAlignment,
		'printTokenNum' => \$printTokenNum,
		'printBestPairComb' => \$printBestPairComb,
		'printTokenList' => \$printTokenList,
		'lexicon=s' => \$lexicon,
		'logDir=s' => \$logDir,
	);
} else {
	print qq |USAGE : perl -w 
	
	|;
}

# reading inputPath
if (! @files && $inputPath) {
	opendir(INPUT_PATH,$inputPath);
	@files=grep { $inputFilePattern } $inputPath;
	closedir(INPUT_PATH);
	
}

welcome();

# TODO : use only $numLang as keys (not $lang - > must be used only in the output) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

##############################################################
# Global constants and parameters

my $printAnchorPoints=1;		# print the anchor points in a CSV and a CES file
my $distanceGaleChurch=1;	# use the Gale & Church original distance
my $completionInInterval=0; 	# simple completion between each point during iterative anchor point extraction

my $version=8;


# transfuge parameters
my $level=3;				# Choice between three different levels of constraint for the transfuge identification
									# level 1 : transfuge must contain at least a digit and transfuge length must be > 1 
									# level 2 : transfuge of length >= $minLenghtLevel2 beginning with an uppercase character
									# level 3 : level 1 or level 2 transfuge or transfuge longer than $minLengthLevel3
my $thresholdOccRatio=0.5;	# 0.5; # 2/3 # threshold for the ratio between occurrence numbers for valid transfuges
my $minLenghtLevel2=4; 		# minimal length for an upper case transfuge (level 2)
my $minLengthLevel3=6;		# 4; # 2 # minimal length for a non-numeric and non-upper case transfuge (level 3)

# cognate parameters
my $useCognate=1;			# if true, cognate pairs are searched in every aligned sentence pair, in order to get new valid anchors
my $cognateMinLength=7;	# min length of form to search potential cognates - must be higher than $minLengthLevel3
if ($cognateMinLength<$minLengthLevel3) {
	print "Warning : cognateMinLength is set to $minLengthLevel3\n";
	$cognateMinLength=$minLengthLevel3;
}
my $caseInsensitiveForCognates=1;	# if 1, cognate comparison is not case sensitive
my $diffMaxSCM=3;					# max "gap" of different letters between potential cognate
my $commonSubStringRatio=0.8;	# the min ratio of common letters to consider potential cognate pair

# anchor points parameters
my $thresholdMinDist=-1;			# minimal inter distance between two occurrences of a same valid anchor (cognate or transfuge)
my $minFrequencyRatio=0.0001;	# if the ratio between the global frequency of a token, and the number of sentences, is below this threshold, the token is ignored (for function word elimination )
my $maxFrequencyRatio=1;			# if the ratio between the global frequency of a token, and the number of sentences, is over this threshold, the token is ignored (for function word elimination )
my $sameOccNumForEachLanguage=0; # if 1, frequency of cognate must be the same for each language in order to yield valid anchors

# iterative algorithm parameters
my $heuristic=2;					# 1 : the best $combinPercentage % of language combinations are computed
my $maxCombinSize=2;				# max size of the various language combinations that have to be treated. P.ex. if ==3, combinations are processed from 3 to 2
my $minCombinSize=2;				# max size of the various language combinations that have to be treated. P.ex. if ==2, combinations are processed from 3 to 2
my $combinPercentage=1;			# percentage of the complete set of language combinations that will effectively be aligned (the best ones are taken)
my $nbCombin=3;					# number of combinations to consider for each language
my $nbMinAnchorPointsToReIterate=40; # if one iteration yields fewer new points than this limit, the iterations are ended
my $maxScopeForCompletion=20; 	# the maximum size of the interval between two points to try 1-to-1 completion

# the following values defines the interval [inf,sup] for the minimum number of similar chains pairs to record a pairwise alignment
my $minMatchNumberSup=2;			# value for the first loop of iterations
my $minMatchNumberInf=1;			# value for the last loop of iterations
my $thresholdLengthRatio=0.75; 	# min ratio between the lengths of aligned sentences (in char.) - used in completionInInterval() 

# geometric constraint between points
# the following variables were previously named $minDiffInterval and $maxDiffInterval
my @maxDiffIntervals=([50,0.5],[150,0.4],[400,0.3],[600,0.25]);	# list of pairs [size,maxDiff] if interval size for one coordinate in inferior to size, then the difference between two intervals must be inferior to maxDiff in order to decide whether the interval are compatible
my $excludeDeviationOnBothSide=1;
my $geometricalTriangulation=0; # for each new point, check parallelism between previous and next point using two other languages

# final completion parameter
my $minDistForfinalCompletion=2;

# Dynamic time warping algo parameters
my $chainingPointsAndJumpingGaps=1; #  If 1, during aligning completion (DTW algo) gaps that are other $maxIntervalSizeForDtw are ignored. If 0, the full alignement is simply computed between the complete space (begin->end)
my $maxIntervalSizeForDtw=300;	# superior limit of size allowed to align an interval with DTW algo
my $useMaxCharDiff=1;				# if 1, the parameter maxCharDiffForDTW is used to discard hazardous points far from the diag during dtw
my $maxCharDiffForDTW=1000;		# max  difference in char length for two interval to be aligned by DTW
my $minMatrixWidth=500;			# min size (in chars) of a matrix side to apply the maxCharDiffForDTW constraint
#~ my $maxDistToAnchorPoint=1; 		# during aligning completion (DTW algo) if the current point is at more than $maxDistToAnchorPoint of the existing anchor point, the current point is discarded

# Gale & Church parameters
my %probTransition=('1:1'=>0.89,'1:0'=>0.0099,'0:1'=>0.0099,'1:2'=>0.089,'2:1'=>0.089,'1:3'=>0.005,'3:1'=>0.005,'2:2'=>0.005);
my $sqrt2pi=sqrt(2*3.14159265358979);
my $dt=0.05;
my $varianceGC=2.608;
my $minusInf=-10;
my $maxDistanceGaleChurch=25;
my %sigma;
# alternative distance computation using basic weighting
my %distFactor=('1:1'=>1,'1:0'=>1,'0:1'=>1,'1:2'=>2,'2:1'=>2,'1:3'=>4,'3:1'=>4,'2:2'=>4);

# list of tokens to be ignored
my %stopList=("o"=>1,"der"=>1,"den"=>1,"in"=>1,"del"=>1,"il"=>1,"la"=>1,"se"=>1,"we"=>1);
my $infinite=10E10;


##############################################################
# Global data structures for languages and tokens
	
my $nbLang=0;		# integer : the language number e. g. : 4
my %langVec=();		# hash : key->tokens (or cognateId), value-> list of languages in which the token occurs
my %numLang = ();	# hash : key->language, value-> language number e.g. : $numLang{"it"}=2
my %occVec=();		# hash of hash : key1->language, key2->token (of cognateId), value-> list of occurrences
my %tokenList=();	# hash : key->language combinations, value-> list of tokens that occur for the languages of the combination 
								# e.g. $tokenList{"en fr") = ["rendez-vous","intention", etc...], $tokenList{"en fr it es") = ["Madame","Bovary", etc...]
								# Note : a token cannot occur in more than one of these lists
my %maxSId=();		# hash : key->language, value-> max sentence number e.g. : $numLang{"it"}=512
my $maxSentNumber=0;# the max value of %maxSId
my %num_Id=();		# hash : key->language, value-> list of correspondances between sentence number and ID
my %id_num=();		# hash : key->language, value-> list of correspondances between sentence ID and sentence Number
my %num_length=(); 	# hash : key->language, value-> list of sentence length for the given language
my %num_offset=();	# hash : key->language, value-> list of sentence offsets for the given language

# to identify candidate cognate, we have to record the complete sentences in %num_sent
my %num_sent=();	# hash : key->language, value-> list of splitted sentences the given language
my %num_sentRaw=(); # hash : key->language, value-> list of raw sentences the given language
my %tok_idCognate=(); #  hash of hash : key1->language, key2->token, value-> cognateId
my $currentCognateId=0; # global counter to generate the next cognateId
my %cognateSearchIsDone;	# hash : keys->lang1-num1-lang2-num2 which records that the corresponding sentences have already been processed for cognate search
my %cognateComparisonIsDone;	# hash : key->form1_form2 which records that the corresponding form pair has already been compared

my %dtw;	# global hash to store the intermediate results of DTW algorithm

# cognateId have the following form : "#numid#". cognateId represents abstract token which groups several similar token forms (eg. fr-résolution, en-resolution, nl-resolutie)
# cognateId are treated as normal tokens in %langVec and %occVec
# When different forms are found to be "cognate", if they are already linked to a cognateId (through %tok_idCognate) the id are merged


# Global data structures for anchor points (Two hashes for each language)
# The algorithm uses a single data structure to record the points for every language combination : $anchor{$lang}{$coordinate}=@point
# All anchor points are n-tuples, with -1 for the missing language : (23,-1,232,1)
# For instance, if we have the point : (60,59,57,45) we will have :
# $allLang[0]="en", $allLang[1]="es", $allLang[2]="fr", $allLang[3]="it", 
#	$anchor{0}{60}= (60,59,57,45)
#	$anchor{1}{45}= (60,59,57,45)
#	$anchor{2}{59}= (60,59,57,45)
#	$anchor{3}{57}= (60,59,57,45)

#	The missing coordinates are filled with -1 :
#	For instance, if we have the point : (-1,55,53,-1) we will have :
#	$anchor{1}{55}= (-1,55,53,-1)
#	$anchor{2}{53}= (-1,55,53,-1)

my %anchors;	#hash of list key1->language name, value list of sentence numbers that define the primary anchor points

my %anchor=();			# hash of hash : key1->language num, key2->sentence number, value-> the corresponding anchor point (if exists).
						# e.g. $anchor{1}{3}=[4,3,-1,-1] if $allLang[1]='es'
my %nextAnc=();			# hash of hash : key1->language num, key2->sentence number, value-> the key of the next anchor point for this language (%nextAnc{$numLang} is only related  to $numLang, not to a specific language combination).
						# e.g. $nextAnc{1}{3}=8; if $allLang[1]='es'
my @orderedAnchor=();	# global array which contains the ordered merging of all the anchor{$lang} hashes
my $totAnchorPoints=0;

my @beginPoint=();
my @endPoint=();
my @bilingualPoints=();

my @mergedAlignment=(); 		# list of hash corresponding to aligned tupples : key->language,value->list of sentence numbers
my %mergedAlignmentIndex=(); 	# key1 -> language, key2 -> sentence number, value -> tuple number in the @mergedAlignment list

################################################################### BEGINNING OF MAIN 
# Stats initialization

my $nbDtwCall=0;
my $sumInt1=0;
my $sumInt2=0;
my $maxInt1=0;
my $maxInt2=0;

$printLog && open(LOG,">:encoding($encoding)","$logDir/JAM.pl.log");
$printLog && LOG->autoflush();
my $time0=time;

printLog("Beginning at $time0.\n");


# test parameters
#~ @files=qw |  europarl/ep-00-01-17.da.ces europarl/ep-00-01-17.de.ces europarl/ep-00-01-17.el.ces europarl/ep-00-01-17.en.ces europarl/ep-00-01-17.es.ces europarl/ep-00-01-17.fi.ces europarl/ep-00-01-17.fr.ces europarl/ep-00-01-17.it.ces europarl/ep-00-01-17.nl.ces europarl/ep-00-01-17.pt.ces europarl/ep-00-01-17.sv.ces  europarl/ep-00-01-17.gr.ces |;
#~ $outputPath= "europarl/jam/resultats";
#~ @combinations=('en-fr','fr-it','es-fr','fr-pt','da-fr','fr-nl','fr-sv','de-fr','fr-fi','fr-gr','fr-el');

# creating output path if necessary
if (! -d $outputPath) {
	mkdir($outputPath);
}

######### Step 0 : filename checking
$verbose && print "\nStep 1 : file name checking and language identification\n";

# alphabetic language sorting
@files=sort { sub lang{ $_=shift; /\.(\w\w)\....$/; return $1}; lang($a) cmp lang($b)} @files;

# Filename checking if languages have not been listed : files must have the form XXXXX.LANG.EXT
my $fileName;
if (! @allLang) {
	foreach my  $element (@files) {
		if ($element=~/([^\\\/]*)\.(\w{2})\.\w{3}$/) {
			$fileName=$1;
			push(@allLang,$2);
		}
		else {
			print "Non conform file names (the language must be specified as the last occurrence of 'dot char char dot' \nFor instance : name.en.ces name.fr.ces name.it.ces\n";
			print "If you do no want to change file names, you must specify the --languages parameter (e.g. --languages en fr it)\n";
			die;
		}
	}
}

$nbLang=@allLang;
for (my $i=0;$i<=$#allLang;$i++) {
	$numLang{$allLang[$i]} = $i;
}

# initialization of @bilingualPoints array, which records, for each language pair $i,$j where $j>$i the keys of all bilingual points
# This array is used to estimate the number of anchor for each pair and to estimate the best language pair combination
for (my $i=0;$i<$nbLang-1;$i++) {
	for (my $j=$i+1;$j<$nbLang;$j++) {
		$bilingualPoints[$i][$j]={};
	}
}

$verbose && print "Identified languages : ".join(" ",@allLang)."\n";

######### Step 1 : combination computation
$verbose && print "\nStep 1 : initializing language list and combinations\n";

if (@combinations) {
	foreach my $comb (@combinations) {
		$comb=[split(/-/,$comb)];
	}
} elsif ($pivot) {
	# creation of a all binary combinations with pivot language p.ex. ([en,fr],[en,es],[en,it])
	foreach my $lang (@allLang) {
		if ($pivot ne $lang) {
			push(@combinations,[sort $lang $pivot]);
		}
	}
} else {
	# the first combination contains all languages
	push(@combinations,\@allLang);

	# in this case combinations are made of chained pairs, e.g. [[da','de'],['de','el'],['el','en'],['en','es'],['es','fr'],['fr','fi'],['fi','gr'],['gr','it'],['it','nl'],['nl','pl'],['pt','sv']]
	for (my $i=0;$i<=$#allLang-1;$i++) {
		push(@combinations,[$allLang[$i],$allLang[$i+1]]);
	}
	# linking first and last languages
	push(@combinations,[$allLang[-1],$allLang[0]]);
}

$verbose && print "Combinations that will be iteratively aligned : ".join(",",map {$_->[0]."-".$_->[1]} @combinations)."\n";

######### Step 2 : file Reading and building of a hash for each language, aiming at storing the occurrence vectors 
# mainly builds %occVec, %langVec
$verbose && print "\nStep 2 : reading input files\n";

for (my $i=0;$i<=$#files;$i++) {
	readFile($files[$i],$allLang[$i]),
}

# deleting the tokens which have a too high frequency
foreach my $token (keys %langVec) {
	# if this anchor is defined for the @listLang combination and is a valid anchor

	if (! lowFrequency($token)) {
		foreach my $lang (@{$langVec{$token}}) {
			#~ print "delete occVec{$lang}{$token}\n";
			delete $occVec{$lang}{$token};
		}
		delete($langVec{$token});
		printLog($token." has been deleted (too high frequency)\n");
	} 
}


######### Step 3 : builds %tokenList, with lists of tokens that fit different kinds of language combination
$verbose && print "\nStep3 : building token lists for each language combination.\n";

foreach my $token (keys %langVec) {
	my @ln=@{$langVec{$token}};
	my $LangNb=$#ln+1;
	if ($LangNb > 1 && validAnchor($token,$level)) {
		# add the token to the tokenList for a specific language combination
		# print "combin($token) = @ln \n";
		my $key=join("",@ln);
		if (exists ($tokenList{$key})) { 
			push(@{$tokenList{$key}},$token);
		}
		else {
			$tokenList{$key}=[$token];
			printLog("Adding a new combination for $token : $key.\n");
		}
	}
}


######### Step 3 bis : reading lexicon if any
if ($lexicon) {
	if (-f $lexicon) {
		printLog("\nStep3 bis : reading lexicon.\n");
		open(LEX,"<:utf8",$lexicon);
		my $nbLines;
		while (<LEX>) {
			chomp;
			my @equivalents=grep {$_} split(/\t/);
			my $lang_form=pop(@equivalents);
			if ($lang_form=~/^(\w\w)-(.*)/) {
				my ($lang,$form)=($1,$2);
				$nbLines++;
				$tok_idCognate{$lang}{$form}=$form;
				foreach my $lang2_form2 (@equivalents) {
					if ($lang2_form2=~/^(\w\w)-(.*)/) {
						my ($lang2,$form2)=($1,$2);
						$tok_idCognate{$lang2}{$form2}=$form;
						if (exists($occVec{$lang2}{$form2})) {
							my $l2=$occVec{$lang2}{$form2};
							$occVec{$lang2}{$form}=$l2;
							push(@{$langVec{$form}},$lang2); # $idCognate becomes a virtual form of $lang2 and inherits the occurrences of $form2
						}
					}
				}
			}
		}
		close(LEX);
		printLog( "\n$nbLines have been read\n");
	} else {
		print "\n************* Warning --> The file $lexicon cannot be opened !\n";
		printLog("\n************* Warning --> The file $lexicon cannot be opened !\n");
	}
}

######### Step 4 : iterative anchor point extraction 
$verbose && print "\nStep4 : iterative anchor point extraction.\n";

# initialisation of the anchor points hashes
foreach my $lang (@allLang) {
	push(@beginPoint,0);
	push(@endPoint,$maxSId{$lang});
}

for (my $i=0;$i<=$#allLang;$i++) {
	$anchor{$i}={};
	$anchor{$i}{0}=[@beginPoint];
	$anchor{$i}{$maxSId{$allLang[$i]}}=[@endPoint];
	$nextAnc{$i}={};
	$nextAnc{$i}{0}=$maxSId{$allLang[$i]};
	$nextAnc{$i}{$maxSId{$allLang[$i]}}=-2;	# The point next to the last point is [-2,-2,-2,..]
}


my $languages=join("-",@allLang);

# Points may be loaded from a file
if ($anchorPointFile) {
	loadPoints($anchorPointFile);
}

# primary anchor Points may be found in the files (using special markers
if ($anchorPointMarkup) {
	addPrimaryAnchorPoints();

}

# extraction of anchor tokens, for each language combination  : this list will then be completed by cognates
my %validAnchors;

foreach my $combin (@combinations) {
	my $comb=join(" ",sort @{$combin});
	$validAnchors{$comb}=[validAnchors($combin,$level)];
	align($combin,$validAnchors{$comb},$level);
	completion($combin,$validAnchors{$comb});
	align($combin,$validAnchors{$comb},$level);
}
@orderedAnchor=reorder();

######### Step 6 : general 1-to-1 completion
$verbose && print "\nStep 5 : Final completion\n";

if ($finalCompletion) {
	finalCompletion(\%validAnchors);
	@orderedAnchor=reorder();
} else {
	$verbose && print "\n==> skipped\n";
}

######### Step 7 : printing output files
$verbose && print "\nStep 6 : printing output files\n";

if ($printAnchorPoints) { 
	printAnchorPoints();
}
if ($printTokenList) {
	printTokenList();
}
if ($printTokenNum) {
	printTokenNum();
}
if ($printBestPairComb) {
	my @best=findBestPairwiseCombination();
	print "Best tiling with language pairs could be : ".join(" ",@best)."\n\n";
}

my $elapsed=time-$time0;
printLog("Ending at time ".time." - $elapsed s. elapsed\n");
print "Ending at time ".time." - $elapsed s. elapsed\n";

$printLog && close LOG;


###########################################################################
# Sub routines
###########################################################################

# hello message
sub welcome {
	print "\n+------------------------------------------------------+\n";
	print "| JAM (Just A Multialigner)                            |\n";
	print "| (cc) 2004-2015 - Olivier Kraif and Bettina Schrader  |\n";
	print "| LIDILEM - Univ. Grenoble Alpes                       |\n";
	print "| Freely distributed under GPL Licence                 |\n";
	print "+------------------------------------------------------+\n";
	print "\n";
}

######################### Function readFile(FILE,LANG)
# fill the %occVec, %num_Id, %id_num, %num_length, %langVec hashes

sub readFile {
	
	my $file=shift;
	my $lang=shift;

	$occVec{$lang} = {};
	my $num=0;
	$num_Id{$lang} = [];
	$num_Id{$lang}[0]=0;
	
	$num_length{$lang} = [];	
	$num_length{$lang}[0] =0;	
	
	$id_num{$lang} = {};

	open(FILE,"<:encoding($encoding)","$file") or die "$file : file not found !\n";
	printLog("Reading of file $file\n");
	$verbose && print "Reading of file $file\n";
	# for XML format, the records are separated by character '>'
	if ($inputFormat eq "ces") {
		$/='>';
	}
	my $offset=0;
	while (<FILE>) {
		$_=~s/[\x0A\x0D]+$//g; # chomp
		# adding a primary anchor point if a markup is encountered
		if ($anchorPointMarkup && /$anchorPointMarkup/) {
			if (! exists($anchors{$lang})) {
				$anchors{$lang}=[];
			}
			push(@{$anchors{$lang}},$num);
		}
		# get sentence number
		if (/<s [^>]*id="([^"]*)"/i or $inputFormat eq "txt") {
			$num++;
			if ($1) {
				$num_Id{$lang}[$num]=$1; # recording correspondances between id and num
				$id_num{$lang}{$1}=$num;
			} else {
				$num_Id{$lang}[$num]=$num; # if not id, num is taken for id
				$id_num{$lang}{$num}=$num;
			}
		}
		# there's a sentence !
		if ($inputFormat eq "txt" or /<\/s>$/) {
			s/\s*<\/s//s; # suppressing </s
			s/^\s*//s; # ltrim
			my @sentence = map { $_ =~s/^\s+|\s+$//g; $_ } split(/\b/,$_);
			# recording of the sentence length
			$num_length{$lang}[$num]=@sentence;
			$num_sent{$lang}[$num]= [@sentence];
			$num_sentRaw{$lang}[$num]= $_;
			$offset+=length($_);
			$num_offset{$lang}[$num]=$offset; # the offset of sentence N is the position of its last character
			# for each read token, update the occurrence vector in the corresponding language
			foreach my $token (@sentence) {
				if (validAnchor($token,$level)) {
					if (exists($occVec{$lang}{$token})) {
						# if the token has already occured
						push(@{$occVec{$lang}{$token}},$num);
					}
					else {
						# if the token occurs for the 1st time
						$occVec{$lang}{$token} = [$num];
						# building of $langVec, the list of languages in which the token exists
						if (exists($langVec{$token})) {
							push(@{$langVec{$token}},$lang);
						}
						else {					
							$langVec{$token} = [$lang];
						}
					}
				}
			}
		}
	}

	close(FILE);
	$/="\n"; # default End of Record
	
	if ($num>$maxSentNumber) {
		$maxSentNumber=$num;
	}
	$maxSId{$lang}=$num;
	
	# the complete text length is recorded for $num=0
	$num_offset{$lang}[0]=$offset;

	printLog("$num sentences has been read ! offset=$offset\n");
	$verbose && print "$num sentences has been read ! offset=$offset\n";
}

######################### Function validAnchor
# Determines whether a token is valid or not to build anchor points
# The validity is defined for length > 2

sub validAnchor {
	my $token=shift @_;
	my $level=shift @_;
	if (exists($stopList{lc($token)})) { return 0 };
	
	if ($level==1) {	# level 1 : length > 1 and includes at least one digit
		return ($token =~/.[0-9]|[0-9]./);
	}
	elsif ($level==2) {	# level 2 : first uppercase and length>=$minLengthLevel2
		return (length($token)>=$minLenghtLevel2) && ($token =~/^[A-Z]/);
	}
	# level 3 : level 1 U level 2 U length>$minLengthLevel3 U cognates
	elsif ($level==3) {
		# Niveau 3 : de longueur > $minLengthLevel3 ou contenant un chiffre + un car.
		return ($token =~/.[0-9]|[0-9]./) || (length($token)>$minLengthLevel3) || ($token =~/^[A-Z]./ && length($token)>=$minLenghtLevel2);
	}
	elsif ($level==4) {	# Niveau 4 : contient un chiffre
		return ($token =~/[0-9]/);
	}
	elsif ($level==5) {	# Niveau 5 : commence par une majuscule
		return ($token =~/^[A-Z]/);
	}
	elsif ($level==6) {	# Niveau 6 : quelconque différent de .,;
		return ($token ne ",") && ($token ne ".") && ($token ne ";");
	}
	return 1	
} 

######################### Function lowFrequency
# Determines, for each language, whether a token is not a function word with a too high relative frequency (>0.2)
sub lowFrequency {
	my $token=shift @_;
	# the occurrence number must not exceed a certain frequency (to avoid function words)
	if (! exists($langVec{$token})) {
		print "ERROR : langVec{$token} does not exist ! !\n";
		die;
	}
	foreach my $lang (@{$langVec{$token}}) {
		# if the texts have already been fully parsed, it is possible to test the relative frequency in each language, in order to eliminate
		if (exists($maxSId{$lang}) && $maxSId{$lang}>0) {
			
			if (! exists($occVec{$lang}{$token})) {
				printLog("ERROR : occVec{$lang}{$token} does not exist - langVec = @{$langVec{$token}}!\n");
				print "ERROR : occVec{$lang}{$token} does not exist - langVec = @{$langVec{$token}}!\n";
				$occVec{$lang}{$token}=[];
				die;
			}
			my $ratio= @{$occVec{$lang}{$token}} / $maxSId{$lang};
			if ($ratio < $minFrequencyRatio || $ratio > $maxFrequencyRatio ) {
				return 0;
			}
		}
	}
	return 1;
}


######################### Function printTokenList
# Prints all the data concerning the languages and tokens
# output : file TokenList.txt

sub printTokenList {
	open(OUT,">",$outputPath."/TokenList.txt");
	foreach my $langComb (keys %tokenList) {
		print OUT "Language combination -> ".$langComb."\n\n";
		foreach my $token (@{$tokenList{$langComb}}) {
			print OUT "$token\n";
			foreach my $lang (@{$langVec{$token}}) {
				my @vec=@{$occVec{$lang}{$token}};
				print OUT "$lang ($#vec) : @vec\n";
			}
		}
	}
	close OUT;
}

######################## Function printTokenNum
# Prints the token number for each language combination
# output : numbers in tokenNum.txt file

sub printTokenNum {
	open(RESULTS,">>",$outputPath."/TokenNum.txt");
	print RESULTS "------------------------------\n";
	foreach my $langComb (keys %tokenList) {
		my $nbTok = @{$tokenList{$langComb}};
		my $nbOcc=0;
		foreach my $token (@{$tokenList{$langComb}}) {
			foreach my $lang (@{$langVec{$token}}) {
				$nbOcc+=@{$occVec{$lang}{$token}};
			}
		}
		my $nbOccMoy=$nbOcc/(length($langComb)/2);
		print RESULTS "Language combination $langComb \tNbTok=$nbTok\tNbOcc=$nbOcc\tNbOccMoy=$nbOccMoy\n";
	}
	close RESULTS;
}


######################### Function printAnchorPoints
# Prints all the anchor points
# output : file AnchorPoints.txt

sub printAnchorPoints {
	my $languages = join("-",@allLang);
	my $OUT;
	
	open($OUT,">:encoding($encoding)",$outputPath."/$fileName.$languages.$outputFormat");
	printOutputHeader($OUT,$outputFormat,\@allLang);
	
	for (my $i=1;$i<=$#orderedAnchor;$i++) { # !!! beginning at 0
		my $links=links(@{$orderedAnchor[$i]});
		printTuple($OUT,$outputFormat,$links,$i,\@allLang);
	}

	#~ if ($outputFormat eq "ces") {
		#~ for (my $i=1;$i<=$#orderedAnchor;$i++) { # !!! beginning at 0
			#~ my $links=links(@{$orderedAnchor[$i]});
			#~ printTuple($OUT,$outputFormat,$links,$i,\@allLang);
		#~ }
	#~ } elsif ($outputFormat eq "tmx") {
		#~ for (my $i=1;$i<=$#orderedAnchor;$i++) { # !!! beginning at 0
			#~ print $OUT "\t\t<tu tuid='$i'>\n";
			#~ for (my $j=1;$j<=$#allLang;$j++) {
				#~ if ($orderedAnchor[$i][$j] !=-1) {
					#~ print $OUT "\t\t\t<tuv id='$allLang[$j]-$orderedAnchor[$i][$j]' xml:lang='$allLang[$j]'>\n";
					#~ print $OUT "\t\t\t\t<seg>".$num_sentRaw{$allLang[$j]}[$orderedAnchor[$i][$j]]."</seg>\n";
					#~ print $OUT "\t\t\t</tuv>\n";
				#~ } 
			#~ }
			#~ print $OUT "\t\t</tu>\n";
		#~ }
		#~ close OUT;
	#~ }elsif ($outputFormat eq "txt") {
		#~ for (my $i=1;$i<=$#orderedAnchor;$i++) { # !!! beginning at 0
			#~ for (my $j=1;$j<=$#allLang;$j++) {
				#~ if ($orderedAnchor[$i][$j] !=-1) {
					#~ print $OUT $allLang[$j]."\t".$num_sentRaw{$allLang[$j]}[$orderedAnchor[$i][$j]]."\n";
				#~ } else {
					#~ print $OUT $allLang[$j]."\t\n";
				#~ }
			#~ }
		#~ }
		#~ print $OUT "\n";
	#~ }elsif ($outputFormat eq "csv") {
		#~ for (my $i=1;$i<=$#orderedAnchor;$i++) {
			#~ print $OUT join("\t",@{$orderedAnchor[$i]})."\n";
		#~ }
	#~ }
	
	printOutputFooter($OUT,$outputFormat);
	close $OUT;
}


# printing output header according to various formats
sub printOutputHeader {
	my ($OUT,$outputFormat,$languageList)=@_;
	my $languages=join("-",@$languageList);
	if ($outputFormat eq "ces") {
		print {$OUT} "<cesAlign type=\"sent\" version=\"1.6\" languages=\"$languages\">\n";
		print {$OUT} "\t<cesHeader version=\"2.3\">\n";
		print {$OUT} "\t\t<translations>\n";
		foreach my $l (@$languageList) {
			print {$OUT} "\t\t\t<translation lang=\"$l\" />\n";
		}
		print {$OUT} "\t\t</translations>\n";
		print {$OUT} "\t</cesHeader>\n";
		print {$OUT} "\t<linkList>\n";
		print {$OUT} "\t\t<linkGrp targType=\"s\">\n";
	} elsif ($outputFormat eq "tmx") {
		print {$OUT} qq|<?xml version="1.0"?>
<?xml-stylesheet href=\"/webAlignToolkit/tmx2html.xsl\" type=\"text/xsl\"?>
<tmx version="1.4">
	<header creationtool="JAM" creationtoolversion="Version $version" datatype="PlainText" segtype="sentence" o-encoding="$encoding">
	</header>
	<body>	
|;
	}
}

# printing aligned tuples according to various formats
sub printTuple {
	my ($OUT,$outputFormat,$links,$i,$languageList)=@_;
	
	if ($outputFormat eq "ces") {
		print {$OUT} "\t\t\t<link xtargets=\"$links\">\n";
	} elsif ($outputFormat eq "tmx") {
		print {$OUT} "\t\t<tu tuid='$i'>\n";
		my @groups=split(/\s*;\s*/,$links);
		for (my $j=0;$j<=$#$languageList;$j++) {
			my $lang=$languageList->[$j];
			if ($groups[$j]) {
				print {$OUT} "\t\t\t<tuv id='$lang-$groups[$j]' xml:lang='$lang'>\n";
				foreach my $id (split(/ /,$groups[$j])) {
					my $num=$id_num{$lang}{$id};
					if (! defined($num_sentRaw{$lang}[$num])) {
						print "num_sentRaw{$lang}[$num] not defined\n";
					} else {
						print {$OUT} "\t\t\t\t<seg>".toXML($num_sentRaw{$lang}[$num])."</seg>\n";
					}
				}
				print {$OUT} "\t\t\t</tuv>\n";
			} else {
				print {$OUT} "\t\t\t<tuv xml:lang='$lang'><seg></seg></tuv>\n";
			}
		}
		print {$OUT} "\t\t</tu>\n";
	}  elsif ($outputFormat eq "txt") {
		my @groups=split(/\s*;\s*/,$links);
		for (my $j=0;$j<=$#$languageList;$j++) {
			my $lang=$languageList->[$j];
			if ($groups[$j]) {
				print {$OUT} "$lang\t";
				foreach my $id (split(/ /,$groups[$j])) {
					print {$OUT} "[$id] ".$num_sentRaw{$lang}[$id]."\t";
				}
				print {$OUT} "\n";
			}
		}
	} elsif ($outputFormat eq "csv") {
		my @groups=split(/\s*;\s*/,$links);
		my @tuples;
		for (my $j=0;$j<=$#$languageList;$j++) {
			my $lang=$languageList->[$j];
			if ($groups[$j]) {
				my $tuple="";
				foreach my $id (split(/ /,$groups[$j])) {
					$tuple.=" [$id] ".$num_sentRaw{$lang}[$id];
				}
				push (@tuples,$tuple);
			}
		}
		print {$OUT} join("\t",@tuples)."\n";
	}
}

sub toXML {
	my $string=shift;
	
	$string=~s/&/&amp;/g;
	$string=~s/</&lt;/g;
	$string=~s/>/&gt;/g;

	return $string;
}

# printing output footer according to various formats
sub printOutputFooter {
	my $OUT=shift;
	my $outputFormat=shift;
	if ($outputFormat eq "ces") {
		print {$OUT} "\t\t</linkGrp>\n";
		print {$OUT} "\t</linkList>\n";
		print {$OUT} "</cesAlign>\n";
	} elsif ($outputFormat eq "tmx") {
		print {$OUT} "\t</body>\n";
		print {$OUT} "</tmx>\n";
	}
}

# compute the links attribute value for a given point, using ids instead of nums
sub links {
	my @point=@_;
	my $links="";
	if ($point[0]!=-1) {
		if (defined($num_Id{$allLang[0]}[$point[0]])) {
			$links=$num_Id{$allLang[0]}[$point[0]];
		} else {
			$links=$point[0];
		}
	}
	for (my $j=1;$j<=$#point;$j++) {
		if ($point[$j]!=-1) {
			if (defined($num_Id{$allLang[$j]}[$point[$j]])) {
				$links.=" ; ".$num_Id{$allLang[$j]}[$point[$j]];
			}  else {
				$links.=" ; ".$point[$j];
			}
		} else {
			$links.=" ; "
		}
	}
	return $links;
}
######################### Function validAnchors()
# Extract the valid common anchor chains for a given language combination 
# I.e. only the tokens which occurs the same number of times for all the languages in @listLang can yields anchor points
# arg1 (list) : list of the concerned languages
# arg2 : anchor level
# arg3 : max frequency
# output (list of string) : @validAnchors
sub  validAnchors {
	my @listLang=sort @{shift @_};
	my $anchorLevel=shift @_;
	my %validAnchors;
	
	$verbose && print "\n\n*** Extracting valid anchors for the @listLang combination. Transfuge level : $anchorLevel\n\n";
	printLog( "\n\n*** Extracting valid anchors for the @listLang combination. Transfuge level : $anchorLevel\n\n");

	# building the list of valid anchors for the languages in @listLang
	foreach my $token (keys %langVec) {
		# if this anchor is defined for the @listLang combination and is a valid anchor

		if ((included(\@listLang,$langVec{$token}))) {
			my $fMax=0;
			my $valid=1;
			foreach my $lang (@{$langVec{$token}}) {
				if (@{$occVec{$lang}{$token}}>$fMax) {
					# when $sameOccNumForEachLanguage is true, frequency must be identical for each language. If not, anchor is rejected
					if ($sameOccNumForEachLanguage && $fMax>0) {
						if ((@{$occVec{$lang}{$token}} != $fMax)) {
							$valid=0;
							last;
						}
					}
					$fMax=@{$occVec{$lang}{$token}};
				}
			}
			if ($valid) {
				$validAnchors{$token}=$fMax;
			}
		} 
	}
	
	printLog( "Valid anchors for : @listLang\t".join(",",sort {$validAnchors{$a} <=> $validAnchors{$b}} keys %validAnchors)."\n");
	# tokens are sorted by increasing frequency
	return sort {$validAnchors{$a} <=> $validAnchors{$b}} keys %validAnchors;
}


######################### Function Align()
# Run the anchor point extraction for a given language combination 
# I.e. only the tokens which occurs the same number of times for all the languages in @listLang can yields anchor points
# This condition of equi-occurence must be satisfied inside intervalles that are defined by the previously extracted anchor points 
# arg1 (list ref) : list of the concerned languages
# arg2 (list ref) : list of the valid anchors
# output : modification of the global hashes %anchor and %nextAnc 

sub align {
	my @listLang=sort @{shift @_};
	my $validAnchors= shift;
	printLog( "\n\n*** Aligning of the @listLang combination. \n\n");
	$verbose && print "\n\n*** Aligning of the @listLang combination.\n";
	my @listNumLang=map {$numLang{$_}} @listLang;
	my $lang=0;
	my $nbAnchorPoints=0;
	my $oldNbAnchorPoints;
	my $passNum=0;
	my @inf=();
	my @sup=();
	my $nbOcc;
	my $equal;
	my @prevPoint;
	my $ok;
	my $minDist;
	my $dist;
	my $minNbOccTot;
	my $maxNbOccTot;
	my $nbOccTot;
	
	my $minMatchNumber=$minMatchNumberSup;
	while ($minMatchNumber>=$minMatchNumberInf) {
		$oldNbAnchorPoints=-1-$nbMinAnchorPointsToReIterate;
		while ($nbAnchorPoints>$oldNbAnchorPoints+$nbMinAnchorPointsToReIterate) {
			$passNum++;
			$verbose && print "\nIteration $passNum (minMatchNumber=$minMatchNumber)\n";
			$oldNbAnchorPoints=$nbAnchorPoints;
			
			my $candidatePairs={};
			my @candidatePoints; # records the list of candidate Points that have been added token after token. 
			# this list is composed by several sublists (one for each token) : @beginPoint, @point1.1, @point1.2...., @beginPoint, @point2.1, @point2.2
			
			# foreach possible anchor
			foreach my $token (@{$validAnchors}) {
				@inf=@beginPoint;
				@sup=nextCommonPoint(\@inf,\@listNumLang);
				# processing of the  [@inf , @sup] interval
				while ($#sup>-1) {
					# printLog("Aligning $token in interval [@inf ; @sup]\n");
					my $nbOcc=-1;
					my $equal=1;
					my ($minDist,$minNbOccTot,$maxNbOccTot);
					my %occ=(); # key->$lang, value-> the occurrences of the token in language $lang in the considered interval
					
					foreach my $lang (@listLang) {
						# $nbOccTot contains the total number of occurrences of the token in a given language
						my $nbOccTot=@{$occVec{$lang}{$token}};
						# $occ{$lang} contains the occurrences of the token in language $lang in the considered interval
						$occ{$lang}=[occIn($inf[$numLang{$lang}],$sup[$numLang{$lang}],$occVec{$lang}{$token})];
						$dist=interPointDist($occ{$lang});
						if ($nbOcc==-1) {
							$minDist=$dist;
							$minNbOccTot=$nbOccTot;
							$maxNbOccTot=$nbOccTot;
							$nbOcc=@{$occ{$lang}};
						} else {
							if ($dist<$minDist) {$minDist=$dist};
							if ($nbOccTot<$minNbOccTot) {$minNbOccTot=$nbOccTot};
							if ($nbOccTot>$maxNbOccTot) {$maxNbOccTot=$nbOccTot};
							$equal=$equal&&($nbOcc==@{$occ{$lang}});
						}
					}

					# three requirements to valid points : 
					# 1/ for each language, the number of occurrences in the interval must be the same
					# 2/ the ratio between the total number of occurrences, for each language, must be over $thresholdOccRatio (tipically 2/3 or 1/2) 
					# 3/ the interpoint distance must be strictly over thresholdMinDist
					
					if ($equal && ($nbOcc>0) && (($minNbOccTot/$maxNbOccTot)>$thresholdOccRatio) && $minDist>$thresholdMinDist) {

						my @prevPoint=@inf;
						for (my $i=0;$i<$nbOcc;$i++) {
							my @newPoint=();
							
							for (my $numL=0;$numL<$nbLang;$numL++) {
								if (exists $occ{$allLang[$numL]}) {
									$newPoint[$numL]=$occ{$allLang[$numL]}[$i];
								}else {
									$newPoint[$numL]=-1;
								}
							}
							
														
							incCandidatePairsCount(\@newPoint,\@prevPoint,\@listNumLang,$candidatePairs);
							push(@candidatePoints,[[@newPoint],[@prevPoint]]);
							@prevPoint=@newPoint;
						}
						#chainPoint(\@sup,\@prevPoint);
					}
					@inf=@sup;
					@sup=nextCommonPoint(\@sup,\@listNumLang);
					# check for infinite loop
					my $b=1;
					for (my $z=0;$z<=$#sup;$z++) {
						$b=$b && ($inf[$z]>=$sup[$z]);
					}
					if ($b && $#sup>-1) { 
						die "Problem with $token for @inf and @sup in @listLang\n";
					}
				}
			}
			
			# every candidate point can now be inserted, if the coordinates are validated by the counts in $candidatePairs hash

			foreach my $ref (@candidatePoints) {
				my $newPoint=$ref->[0];

				if (addPoints($newPoint,\@listNumLang,$candidatePairs,$minMatchNumber,$validAnchors)==1) {
					$verbose && print ".";
					$nbAnchorPoints++;
					$totAnchorPoints++;
					#printLog( "Adding @{$newPoint}\n");
				}
			}


			printLog( "\nLoop number $passNum -> $nbAnchorPoints points extracted. Total : $totAnchorPoints\n");
		}
		$minMatchNumber--;
	}
}

sub diffInterval {
	my $int1=shift;
	my $int2=shift;
	if (($int1+$int2)==0) {  return 0  };
	return (2*abs($int1-$int2)/abs($int1+$int2));
}
######################## Function completion()
# complete the anchor points for a given language combination 
# All intervals between existing anchor points are considered
# If the intervals involve the same number of sentence in each language, and if the sentence lengths are matching in all the interval, then anchor points are added in order to align the sentences
# arg1 (list) : list of the concerned languages
# output : modification of the global hashes %anchor and %nextAnc 

sub completion {
	my @listLang=sort @{shift @_};
	my $validAnchors=shift;
	
	my @listNumLang=map {$numLang{$_}} @listLang;
	
	if (! $completionInInterval) {
		return;
	}
	printLog( "\n\n*** Completion for the @listLang combination.\n\n");

	my $nbAnchorPoints=0;
	my @inf=();
	my @sup=();

	@inf=@beginPoint;
	@sup=nextCommonPoint(\@inf,\@listNumLang);
	# processing of the  [@inf , @sup] interval
	while ($#sup>-1) {

		$nbAnchorPoints+=completionInInterval(\@inf,\@sup,\@listNumLang,$validAnchors);

		@inf=@sup;
		@sup=nextCommonPoint(\@sup,\@listNumLang);
		
		# check for infinite loop
		my $b=1;
		for (my $z=0;$z<=$#sup;$z++) {
			$b=$b && ($inf[$z]>=$sup[$z]);
		}
		if ($b && $#sup>-1) { 
			die "Problem for @inf and @sup in @listLang\n";
		}

	}

	$verbose && print  "\n$nbAnchorPoints points were added by completion. Total : $totAnchorPoints\n";
	printLog( "\n$nbAnchorPoints points were added by completion. Total : $totAnchorPoints\n");
}

# completion of the alignment for \@listLang in interval @inf @sup
sub completionInInterval {
	my @inf=@{shift @_};
	my @sup=@{shift @_};
	my @listNumLang=@{shift @_};
	my $validAnchors=shift;
	my $nbAnchorPoints=0;
	
	printLog( "Completion in interval [@inf ; @sup]\n");

	my $prevIntervalSize=0;
	my $completionIsPossible=1;
	my $maxNbLoop=$infinite;
	# looking at the intervalSize betwen @inf and @sup for each language. 
	# we compare the intervals using the offset width of the interval divided by the complete offset for each text : if the ratio is near to 1, ok completion is possible
	foreach my $i (@listNumLang) {
		my $inter=0;
		if ($sup[$i]-$inf[$i]<$maxNbLoop) {
			$maxNbLoop=$sup[$i]-$inf[$i];
		}

		# TODO : use diffInteval() here !!!!!!!!!!!!!!
		my $intervalSize=calcIntervalNormalisedLength(\@inf,\@sup,$i);
		if ($prevIntervalSize!=0) {
			my $ratio=($prevIntervalSize<$intervalSize)?$prevIntervalSize/$intervalSize:$intervalSize/$prevIntervalSize;
			if ($ratio<$thresholdLengthRatio) {
				printLog( "Rejected : ratio = $ratio\n");
				$completionIsPossible=0;
				last;
			}
		}
		$prevIntervalSize=$intervalSize;
	}

	# while all the sentence lengths are matching, then completion is made starting from @inf to @sup, and then in reverse order starting from @sup back to @inf
	if ($completionIsPossible && $maxNbLoop>1) {
		##### first step : completing the points starting from @inf
		# initially @currentPoint is a copy of @inf with -1 for every coordinate that do not belong to @listLang
		my @currentPoint=(-1) x $nbLang;
		foreach my $num (@listNumLang) {
			$currentPoint[$num]=$inf[$num];
		}
		my $firstLoop=1;
		my $i=0;
		ASCENDING:while (validPoint(@currentPoint)) {
			if (!$firstLoop) {
				# the new point must have a new reference
				my $newPoint=[@currentPoint];
				my $add=addPoints($newPoint,\@listNumLang,0,0,$validAnchors);
				if ($add!=-1) {
					if ($add==1) {
						$verbose && print "x";
						printLog( "Completion : adding @{$newPoint}\n");
						$nbAnchorPoints++;
						$totAnchorPoints++;
					}
				} else {
					printLog( "Rejected \n");
					last ASCENDING; # when reaching @sup addPoint() returns 0
				}
			}
			$firstLoop=0;
					
			foreach my $num (@listNumLang) {
				$currentPoint[$num]=$currentPoint[$num]+1;
			}
			$i++;
			if ($i>=$maxNbLoop) {
				last ASCENDING;
				#~ die "error : $i iteration while ascending\n";
			}
		}
		
		##### second step : completing the points starting from @sup, and decreasing to @inf
		@currentPoint=(-1) x $nbLang;
		foreach my $num (@listNumLang) {
			$currentPoint[$num]=$sup[$num];
		}
		$firstLoop=1;
		$i=0;
		DESCENDING:while (validPoint(@currentPoint)) {
			if (!$firstLoop) {
				my $newPoint=[@currentPoint];
				my $add=addPoints($newPoint,\@listNumLang,0,0,$validAnchors);
				if ($add!=-1) {
					if ($add==1) {
						$verbose && print "x" ;
						printLog( "Completion : adding @{$newPoint}\n");
						$nbAnchorPoints++;
						$totAnchorPoints++;
					}
				} else {
					last DESCENDING;
				}
			}
			$firstLoop=0;
					
			foreach my $num (@listNumLang) {
				if ($currentPoint[$num]>0) {
					$currentPoint[$num]=$currentPoint[$num]-1;
				} else {
					last DESCENDING;
				}
			}
			$i++;
			if ($i>=$maxNbLoop) {
				last DESCENDING;
				#~ die "error : $i iterations while descending\n";
			}

		}
	}
	return $nbAnchorPoints;
}

# Fill the gap with 1-0 and 0-1 transition between (infI,infJ) and (supI,supJ)
sub dtwGap {
	my $i=shift;
	my $j=shift;
	my $infI=shift;
	my $infJ=shift;
	my $supI=shift;
	my $supJ=shift;
	my $path;
	
	# The hash %dtw records the result for already computed path, in order to reduce recursivity
	my $dtwKey="$supI-$supJ";
	if (exists($dtw{$dtwKey})) {
		return @{$dtw{$dtwKey}};
	}
	print "DTW GAP for : $dtwKey\n";
	printLog("DTW GAP for : $dtwKey\n");

	my $dtw=0;
	my @path=();
	# first adding point (supI,supJ) with 1-1 transition
	my $lI=calcLength($i,$supI);
	my $lJ=calcLength($j,$supJ);
	$dtw+=distanceDTW($lI,$lJ,'1:1');
	unshift(@path,[$supI,$supJ]);	
	
	# then adding 1-0 transition until (infI,supJ)
	while ($supI>$infI) {
		my $lI=calcLength($i,$supI);
		$dtw+=distanceDTW($lI,0,'1:0');
		unshift(@path,[$supI,undef]);
		$supI--;
	}
	# then adding 0-1 transition until (infI,infJ)
	while ($supJ>$infJ) {
		my $lJ=calcLength($j,$supJ);
		$dtw+=distanceDTW(0,$lJ,'0:1');
		unshift(@path,[undef,$supJ]);
		$supJ--;
	}
	
	if (($infI>0 || $infJ >0) && ! exists($dtw{"$infI-$infJ"})) {
		die "Path has not been computed for point ($infI,$infJ) languages=($i;$j)\n";
	}
	my ($pathInf,$dtwInf)=@{$dtw{"$infI-$infJ"}};
	$dtw+=$dtwInf;
	$path=[@{$pathInf},@path];
	
	# update of %dtw hash
	$dtw{$dtwKey}=[$path,$dtw];
	return ($path,$dtw);
}


# compute the bestpath (a list of [I,J] pairs) and the corresponding score (the minimum distance)
sub dtw {
	my $i=shift;
	my $j=shift;
	my $infI=shift;
	my $infJ=shift;
	my $supI=shift;
	my $supJ=shift;
	my $path;
	

	# The hash %dtw records the result for already computed path, in order to reduce recursivity
	my $dtwKey="$supI-$supJ";
	if (exists($dtw{$dtwKey})) {
		return @{$dtw{$dtwKey}};
	}
	printLog("DTW for : $dtwKey\n");
	
	# points that cross the ($infI,$infJ) anchor points are discarded
	if ($supI<$infI or $supJ<$infJ) {
		$dtw{$dtwKey}=[[],$infinite];
		return ([],$infinite);
	}
	
	my $matrixWidth=max(calcInterLength($infI,$supI,$allLang[$i]),calcInterLength($infJ,$supJ,$allLang[$j]));
	
	#~ # end of recursivity if the distance from diagonal is greater than $maxCharDiffForDTW
	if ($useMaxCharDiff &&  $matrixWidth>$minMatrixWidth && charDiff($infI,$supI,$allLang[$i],$infJ,$supJ,$allLang[$j])> $maxCharDiffForDTW ) {
		printLog("($supI,$supJ) discarded : interval I = ".calcInterLength($infI,$supI,$allLang[$i])." interval J = ".calcInterLength($infJ,$supJ,$allLang[$j])." - too far from the diagonal according to ($infI,$infJ) - Diff> $maxCharDiffForDTW\n");
		$dtw{$dtwKey}=[[],$infinite];
		return ([],$infinite);
	}
	my $lI=calcLength($i,$supI); 
	my $lI1=calcLength($i,$supI-1);
	my $lI2=calcLength($i,$supI-2);
	my $lJ=calcLength($j,$supJ);
	my $lJ1=calcLength($j,$supJ-1);
	my $lJ2=calcLength($j,$supJ-2);
	
	# end of recursivity if the current coordinates reach the @inf point : the path must end here
	if ($supI==0 && $supJ==0) {
		return ([],0);
	}
	# normaly the point $dtw{"$supI-$supJ"} should have been computed previously (anchor points chaining from the first to the last)
	if ($supI==$infI && $supJ==$infJ) {
		die "The point $dtw{'$supI-$supJ'} should have been computed previously (anchor points chaining from the first to the last)\n";
	}


	my ($dtw1_0,$dtw0_1,$dtw1_1,$dtw1_2,$dtw2_1,$dtw1_3,$dtw3_1,$dtw2_2)=($infinite,$infinite,$infinite,$infinite,$infinite,$infinite,$infinite,$infinite);
	my ($path1_0,$path0_1,$path1_1,$path2_1,$path1_2,$path3_1,$path1_3,$path2_2)=([],[],[],[],[],[],[],[]);
	
	# The following constraints must guarantee that ($infI,$infJ) anchor point is included in the group - special conditions are made for the points that overlap ($infI,$infJ)
	
	# case 1 : current point must not be ($infI,$infJ+/-n)
	if ($supI!=$infI) {
		($path1_0,$dtw1_0)=dtw($i,$j,$infI,$infJ,$supI-1,$supJ);
		$dtw1_0+=distanceDTW($lI,0,'1:0');
	}
	# case 1 bis : current point must not be ($infI+/-n,$infJ) 
	if ($supJ!=$infJ ) {
		($path0_1,$dtw0_1)=dtw($i,$j,$infI,$infJ,$supI,$supJ-1);
		$dtw0_1+=distanceDTW(0,$lJ,'0:1');
	}

	# case 2 : current point must not be ($infI,$infJ+/-n) or ($infI+/-n,$infJ)
	if ($supI!=$infI && $supJ!=$infJ) { 
		($path1_1,$dtw1_1)=dtw($i,$j,$infI,$infJ,$supI-1,$supJ-1);
		$dtw1_1+=distanceDTW($lI,$lJ,'1:1');
	}
	# case 3 : if $supJ==$infJ then $supI MUST be equal to $infI+1 and reciprocally
	if (! ($supI!=$infI+1 && $supJ==$infJ) && ! ( $supI==$infI+1 && $supJ!=$infJ)) {
		($path2_1,$dtw2_1)=dtw($i,$j,$infI,$infJ,$supI-2,$supJ-1);
		$dtw2_1+=distanceDTW($lI+$lI1,$lJ,'2:1');
	}
	# case 4 : if $supI==$infI then $supJ MUST be equal to $infJ+1 and reciprocally
	if (! ($supI==$infI && $supJ!=$infJ+1) && ! ($supI!=$infI && $supJ==$infJ+1)) {
		($path1_2,$dtw1_2)=dtw($i,$j,$infI,$infJ,$supI-1,$supJ-2);
		$dtw1_2+=distanceDTW($lI,$lJ+$lJ1,'1:2');
	}
	# case 5 : if $supI==$infI+1 then $supJ MUST be equal to $infJ+1 and reciprocally
	if (! ($supI==$infI+1 && $supJ!=$infJ+1) && ! ($supI!=$infI+1 && $supJ==$infJ+1)) {
		($path2_2,$dtw2_2)=dtw($i,$j,$infI,$infJ,$supI-2,$supJ-2);
		$dtw2_2+=distanceDTW($lI+$lI1,$lJ+$lJ1,'2:2');
	}
	# case 7 : if $supI==$infI+1 or  $supI==$infI+2 then $supJ MUST be equal to $infJ and reciprocally
	if ($supI==$infI+1 or $supI==$infI+2) {
		if ($infJ==$supJ) {
			($path3_1,$dtw3_1)=dtw($i,$j,$infI,$infJ,$supI-3,$supJ-1);
			$dtw3_1+=distanceDTW($lI+$lI1+$lI2,$lJ,'3:1');
		}
	} elsif ($infJ!=$supJ) {
		($path3_1,$dtw3_1)=dtw($i,$j,$infI,$infJ,$supI-3,$supJ-1);
		$dtw3_1+=distanceDTW($lI+$lI1+$lI2,$lJ,'3:1');
	}
	# case 8 : if $supJ==$infJ+1 or  $supJ==$infJ+2 then $supI MUST be equal to $infI and reciprocally
	if ($supJ==$infJ+1 or $supJ==$infJ+2) {
		if ($infI==$supI) {
			($path1_3,$dtw1_3)=dtw($i,$j,$infI,$infJ,$supI-1,$supJ-3);
			$dtw1_3+=distanceDTW($lI,$lJ+$lJ1+$lJ2,'1:3');
		}
	} elsif ($infI!=$supI) { 
		($path1_3,$dtw1_3)=dtw($i,$j,$infI,$infJ,$supI-1,$supJ-3);
		$dtw1_3+=distanceDTW($lI,$lJ+$lJ1+$lJ2,'1:3');
	}
	
	#$dtw1_2=$dtw2_1=$infinite;
	my $dtw=min($dtw1_1,$dtw1_0,$dtw0_1,$dtw1_2,$dtw2_1,$dtw2_2,$dtw3_1,$dtw1_3);
	# follow 1-1 path
	if ($dtw1_1==$dtw) {
		#~ if (! defined($path1_1)) {
			#~ print "dtw1_1=$dtw1_1, infI=$infI,  infJ=$infJ,  supI=$supI,  supJ=$supJ\n";
			#~ die;
		#~ }
		$path=[@{$path1_1},[$supI,$supJ]];
	# follow 1-2 path
	} elsif ($dtw1_2==$dtw) {
		$path=[@{$path1_2},[$supI,$supJ]];
	# follow 2-1 path
	} elsif ($dtw2_1==$dtw) {
		$path=[@{$path2_1},[$supI,$supJ]];
	# follow 1-3 path
	} elsif ($dtw1_3==$dtw) {
		$path=[@{$path1_3},[$supI,$supJ]];
	# follow 3-1 path
	} elsif ($dtw3_1==$dtw) {
		$path=[@{$path3_1},[$supI,$supJ]];
	#~ # follow 1-0 path
	} elsif ($dtw1_0==$dtw) {
		$path=[@{$path1_0},[$supI,undef]];
	# follow 0-1 path
	} elsif ($dtw0_1==$dtw) {
		$path=[@{$path0_1},[undef,$supJ]];
	# follow 2-2 path
	} else {
		$path=[@{$path2_2},[$supI,$supJ]];
	}
	# storing the result for later recursive call
	$dtw{$dtwKey}=[$path,$dtw];
	return ($path,$dtw);
}

# return the deviation in character between two intervals (with a normalisation according to the first language offset)
sub charDiff {
	my ($infI,$supI,$langI,$infJ,$supJ,$langJ)=@_;
	return int(abs(calcInterLength($infI,$supI,$langI)-calcInterLength($infJ,$supJ,$langJ)*$num_offset{$langI}[0]/$num_offset{$langJ}[0]));
}


# run dtw step by step for blocks of 10x10 in order to minimize recursivity
sub runDtwIteratively {
	my ($i,$j,$b_i,$b_j,$e_i,$e_j)=@_;

	for (my $ii=1;$ii<=$e_i-$b_i;$ii++) {
		my $jj=int($ii*($e_j-$b_j)/($e_i-$b_i));
		#~ printLog("iteration dtw($i,$j,$b_i,$b_j,".($b_i+$ii).",".($b_j+$jj).")\n");
		dtw($i,$j,$b_i,$b_j,$b_i+$ii,$b_j+$jj);
	}

	return dtw($i,$j,$b_i,$b_j,$e_i,$e_j);
}


# completion of the alignment for language $i $j
sub extractCompleteAlignment {
	my $i=shift;
	my $j=shift;
	my $recordPointsInAnchorTable=shift;
	my $validAnchors=shift;
	
	my $nbAnchorPoints=0;
	
	printLog( "*** Extraction of complete alignment for coordinates $i and $j\n");

	#~ $maxDiffForDTW=0;
	%dtw=();
	# initialization for the NULL path
	$dtw{"0-0"}=[[],0];
	
	printLog( "dtw($i,$j,1,1,$endPoint[$i],$endPoint[$j])\n");
	my ($bestPath,$score);
	
	if ($chainingPointsAndJumpingGaps) {
		my $inf= \@beginPoint;
		my $sup= [nextCommonPoint($inf,[$i,$j])];
		# processing of the  [@inf , @sup] interval
		while (@{$sup}>0) {
			
			my $intervalI=calcIntervalNormalisedLength($inf,$sup,$i);
			my $intervalJ=calcIntervalNormalisedLength($inf,$sup,$j);
		
			my $ratio;
			if ($intervalJ*$intervalI==0) {
				printLog( "[@{$inf},@{$sup}] : Interval is null\n");
				$ratio=0;
			} else {
				$ratio=($intervalJ<$intervalI)?$intervalJ/$intervalI:$intervalI/$intervalJ;
			}
			# identification of a gap which must be excluded from alignment
			if ( ($sup->[$i] - $inf->[$i]) > $maxIntervalSizeForDtw ||  ($sup->[$j] - $inf->[$j]) > $maxIntervalSizeForDtw || ($useMaxCharDiff && charDiff($inf->[$i],$sup->[$i],$allLang[$i],$inf->[$j],$sup->[$j],$allLang[$j]) > $maxCharDiffForDTW) ) {
				printLog( "$allLang[$i] - $allLang[$j] : [$inf->[$i],$inf->[$j]],[$sup->[$i],$sup->[$j]]] DTW GAP : ratio = $ratio\n");
				$verbose && print "$allLang[$i] - $allLang[$j] : [$inf->[$i],$inf->[$j]],[$sup->[$i],$sup->[$j]]] DTW GAP : ratio = $ratio\n";
				
				# jumping the gap
				dtwGap($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j]);
				# the new beginning is updated to the current point
			} else {
				# aligning from the beginning of the interval to the beginning of the gap
				print "Lancement de dtw entre deux points : dtw($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j])\n";
				printLog( "Lancement de dtw entre deux points : dtw($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j])\n");
				#~ $verbose && print "Lancement de dtw entre deux points : dtw($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j])\n";
				
				# path implying transition 1-0 or 0-1 from current anchor are discarded
				my $dtwKey1_0=$sup->[$i]."-".($sup->[$j]-1);
				$dtw{$dtwKey1_0}=[[],$infinite];
				my $dtwKey0_1=($sup->[$i]-1)."-".$sup->[$j];
				$dtw{$dtwKey0_1}=[[],$infinite];
				
				runDtwIteratively($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j]);
				#~ dtw($i,$j,$inf->[$i],$inf->[$j],$sup->[$i],$sup->[$j]);
				
				# stats
				$nbDtwCall++;
				$sumInt1+=($sup->[$i] - $inf->[$i]);
				$sumInt2+=($sup->[$j] - $inf->[$j]);
				if (($sup->[$i] - $inf->[$i])>$maxInt1) {
					$maxInt1=$sup->[$i] - $inf->[$i];
				}
				if (($sup->[$j] - $inf->[$j])>$maxInt2) {
					$maxInt2=$sup->[$j] - $inf->[$j];
				}
			}
			
			$inf=$sup;
			$sup=[nextCommonPoint($sup,[$i,$j])];
			
			# check for infinite loop
			my $b=1;
			for (my $z=0;$z<@{$sup};$z++) {
				$b=$b && ($inf->[$z]>=$sup->[$z]);
			}
			if ($b && @{$sup}>0) { 
				die "Problem for @$inf and @$sup in listLang\n";
			}
		}
		
	} 
	($bestPath,$score)=@{$dtw{"$endPoint[$i]-$endPoint[$j]"}};
	printLog( "Best Path=".@{$bestPath}." score=$score\n");
	
	my $l1=$allLang[$i];
	my $l2=$allLang[$j];
	my $OUT;
	if ($printPairwiseAlignment) {
		printLog( "Printing pairwise alignment for $l1-$l2\n");
		open($OUT,">:encoding($encoding)",$outputPath."/$fileName.full.$l1-$l2.$outputFormat");
		printOutputHeader($OUT,$outputFormat,[$l1,$l2]);
	}
	
	for (my $k=0;$k<@{$bestPath}-1;$k++) {
		my $pair=$bestPath->[$k];
		my $nextPair=$bestPath->[$k+1];
		my $left;
		my ($num1,$num2); # to record the 1-1 pairs in the anchor table
		my $mergedTupleNumber=@mergedAlignment;
		if (!defined($nextPair->[0])) {
			$left=" ";
		} elsif (defined($pair->[0]) && $nextPair->[0]-$pair->[0]==2) {
			$left=($num_Id{$l1}[$nextPair->[0]-1])." ".$num_Id{$l1}[$nextPair->[0]]." ";
			$mergedTupleNumber=addToMergedAlignment($l1,$mergedTupleNumber,$num_Id{$l1}[$nextPair->[0]-1],$num_Id{$l1}[$nextPair->[0]]);
		} elsif (defined($pair->[0]) && $nextPair->[0]-$pair->[0]==3) {
			$left=$num_Id{$l1}[$nextPair->[0]-2]." ".$num_Id{$l1}[$nextPair->[0]-1]." ".$num_Id{$l1}[$nextPair->[0]]." ";
			$mergedTupleNumber=addToMergedAlignment($l1,$mergedTupleNumber,$num_Id{$l1}[$nextPair->[0]-2],$num_Id{$l1}[$nextPair->[0]-1],$num_Id{$l1}[$nextPair->[0]]);
		} else {
			$left=$num_Id{$l1}[$nextPair->[0]]." ";
			$num1=$nextPair->[0];
			$mergedTupleNumber=addToMergedAlignment($l1,$mergedTupleNumber,$num_Id{$l1}[$nextPair->[0]]);
		}
		
		my $right;
		
		if (!defined($nextPair->[1])) {
			$right=" ";
		} elsif (defined($pair->[1]) && $nextPair->[1] - $pair->[1]==2) {
			$right=" ".$num_Id{$l2}[$nextPair->[1]-1]." ".$num_Id{$l2}[$nextPair->[1]];
			$mergedTupleNumber=addToMergedAlignment($l2,$mergedTupleNumber,$num_Id{$l2}[$nextPair->[1]-1],$num_Id{$l2}[$nextPair->[1]]);
		} elsif (defined($pair->[1]) && $nextPair->[1] - $pair->[1]==3) {
			$right=" ".$num_Id{$l2}[$nextPair->[1]-2]." ".$num_Id{$l2}[$nextPair->[1]-1]." ".$num_Id{$l2}[$nextPair->[1]];
			$mergedTupleNumber=addToMergedAlignment($l2,$mergedTupleNumber,$num_Id{$l2}[$nextPair->[1]-2],$num_Id{$l2}[$nextPair->[1]-1],$num_Id{$l2}[$nextPair->[1]]);
		} else  {
			$right=" ".$num_Id{$l2}[$nextPair->[1]];
			$num2=$nextPair->[1];
			$mergedTupleNumber=addToMergedAlignment($l2,$mergedTupleNumber,$num_Id{$l2}[$nextPair->[1]]);
		}
		
		if ($printPairwiseAlignment) {
			printTuple($OUT,$outputFormat,"$left;$right",$k+1,[$l1,$l2]);
		}
		
		# The more reliable 1-1 points are recorded in the global anchor table
		if ($recordPointsInAnchorTable && defined($num1) && defined($num2)) {
			my @newPoint=(-1)x $nbLang;
			$newPoint[$i]=$num1;
			$newPoint[$j]=$num2;
			my $l1=calcNormalisedLength($i,$num1);
			my $l2=calcNormalisedLength($j,$num2);
			
			# only sentence pairs with comparable length are added
			if ($l1>0 && $l2>0 && min($l1/$l2,$l2/$l1)>=$thresholdLengthRatio) {
				printLog( "Adding pair : $newPoint[$i],$newPoint[$j] (@newPoint)\n");
				my $add=addPoints([@newPoint],[$i,$j],0,0,$validAnchors);
				if ($add!=-1) {
					if ($add==1) {
						$verbose && print "x" ;
						printLog( "Completion : adding @newPoint\n");
						$nbAnchorPoints++;
						$totAnchorPoints++;
					}
				}
			}
		}
	}
	if ($printPairwiseAlignment) {
		printOutputFooter($OUT,$outputFormat);
	}
	
	if ($recordPointsInAnchorTable ) {
		$verbose && print  "\n$nbAnchorPoints points were added by completion. Total : $totAnchorPoints\n";
		printLog( "\n$nbAnchorPoints points were added by completion. Total : $totAnchorPoints\n");
	}
	
	return;
}

# Adding a group of sentence numbers for a given language to a given tuple in the merged alignment hash. 
# We look to the first tuple that is linked to one of the sentence numbers, if it is smaller than the number given in input. 
# Then all the numbers are linked to this first tuple, and the previously linked tuples are merged to this tuple

sub addToMergedAlignment {
	my $lang=shift;
	my $inputTupleNumber=shift;
	
	# looking for the smallest number
	my $min=$inputTupleNumber;
	foreach my $num (@_) {
		if (exists ($mergedAlignmentIndex{$lang}{$num})) {
			$min=min($inputTupleNumber,$mergedAlignmentIndex{$lang}{$num});
		}
	}
	# if the tuple is new and has to be created as the last element of @mergedAlignment
	if (! defined($mergedAlignment[$min])) {
		$mergedAlignment[$min]={};
	}
	# merging all the existing tuple to tuple $min 
	foreach my $num (@_) {
		# if one tuple is already defined for num 
		if (exists ($mergedAlignmentIndex{$lang}{$num})) {
			# if this tuple is different from $min then merging both tuple to $min
			if ($mergedAlignmentIndex{$lang}{$num}>$min) {
				mergeTuple($min,$mergedAlignmentIndex{$lang}{$num});	
				$mergedAlignmentIndex{$lang}{$num}=$min;
			}
		# if no tuple is defined for num, simply add it
		} else {
			if (! exists($mergedAlignment[$min]{$lang})) {
				$mergedAlignment[$min]{$lang}=[];
			} 
			push(@{$mergedAlignment[$min]{$lang}},$num);
			$mergedAlignmentIndex{$lang}{$num}=$min;
		}
	}
	# the input tuple number must be merged with $min if different
	if ($inputTupleNumber>$min) {
		mergeTuple($min,$inputTupleNumber);
	}
	# the new tuple number is returned for the following language if any
	return $min;
}

# merging two tuples of the @mergedAlignment table, where $tupleNum2 is merged to $tupleNum1 and then $mergedAlignment[$tupleNum2] is cleared
sub mergeTuple {
	my $tupleNum1=shift; # tupleNum1 is smaller than $num2
	my $tupleNum2=shift; 
	
	if (defined($mergedAlignment[$tupleNum2])) {
		foreach my $lang (keys %{$mergedAlignment[$tupleNum2]}) {
			foreach my $num (@{$mergedAlignment[$tupleNum2]{$lang}}) {
				if (! exists($mergedAlignment[$tupleNum1]{$lang})) {
					$mergedAlignment[$tupleNum1]{$lang}=[];
				} 
				push(@{$mergedAlignment[$tupleNum1]{$lang}},$num);
				$mergedAlignmentIndex{$lang}{$num}=$tupleNum1;
			}
		}
		undef($mergedAlignment[$tupleNum2]);
	}
}


# compute the normalised length of an interval between two points @inf and @sup, for the language $i
sub calcIntervalNormalisedLength {
	my $inf=shift;
	my $sup=shift;
	my $i=shift;

	@{$sup}==0 && die "$sup vide\n";
	my $lang=$allLang[$i];
	my $numInf=$inf->[$i];
	my $numSup=$sup->[$i];
	return calcInterNormalisedLength($numInf,$numSup,$lang);
}

sub calcInterNormalisedLength {
	my $numInf=shift;
	my $numSup=shift;
	my $lang=shift;

	if (defined($num_offset{$lang}[0]) && $num_offset{$lang}[0]!=0) {
		return calcInterLength($numInf,$numSup,$lang)/$num_offset{$lang}[0];
	}
	return 0;
}

# between too points for the language $i
sub calcIntervalLength {
	my $inf=shift;
	my $sup=shift;
	my $i=shift;

	@{$sup}==0 && die "$sup vide\n";
	my $lang=$allLang[$i];
	
	my $numInf=$inf->[$i];
	my $numSup=$sup->[$i];
	return calcInterLength($numInf,$numSup,$lang);
}

# return the length (in chars) of the specified interval between too num for the language $i

sub calcInterLength {
	my $numInf=shift;
	my $numSup=shift;
	my $lang=shift;
	
	if ($numInf>=$numSup) {
		return 0;
	}
	if ($numInf==0 && defined($num_offset{$lang}[$numSup])) {
		return $num_offset{$lang}[$numSup];
	}
	if (defined($num_offset{$lang}[$numSup]) && defined($num_offset{$lang}[$numInf])) {
		return $num_offset{$lang}[$numSup]-$num_offset{$lang}[$numInf];
	}
	return 0;
}


# compute the normalised length of the sentence with $coordI number for language $i
sub calcNormalisedLength {
	my $i=shift;
	my $coordI=shift;
	my $lang=$allLang[$i];
	
	if (defined($num_offset{$lang}[0]) && $num_offset{$lang}[0]!=0) {
		return calcLength($i,$coordI)/$num_offset{$lang}[0];
	}
	return 0;
}

sub calcLength {
	my $i=shift;
	my $coordI=shift;
	my $lang=$allLang[$i];
	
	if($coordI==0) {
		return 0;
	}
	# for (1,1,1,1, .... ) we do not have to compute the offset difference between the current and the previous sentence
	if ($coordI==1 && defined($num_offset{$lang}[1])) {
		return $num_offset{$lang}[1];
	}
	
	if (defined($num_offset{$lang}[$coordI]) && defined($num_offset{$lang}[$coordI-1])) {
		return ($num_offset{$lang}[$coordI]- $num_offset{$lang}[$coordI-1]);
	}
	return 0;
}
	

# check that the sentence lengths are similar for every language of the point
# The comparison is done pairwise for each sentence and the preceding one 
sub validPoint {
	my @point=@_;
	my $prevRelativeLength=0;
	for (my $i=0;$i<$nbLang;$i++) {
		if ($point[$i]>-1) {
			my $relativeLength=calcNormalisedLength($i,$point[$i]);
			if ($relativeLength==0) {
				return 0;
			}
			# TODO : use diffInterval() here
			if ($prevRelativeLength!=0) {
				my $ratio = ($relativeLength<$prevRelativeLength)? $relativeLength/$prevRelativeLength:$prevRelativeLength/$relativeLength;
				if ($ratio < $thresholdLengthRatio) {
					printLog( "@point is not valid (ratio=$ratio)\n");
					return 0;
				}
			}
			$prevRelativeLength=$relativeLength;
		}
	}
	return 1;
}

sub minMax {
	my @l=@_;
	if (@l>=1) {
		my $first=pop(@l);
		my ($min,$max)=($first,$first);
		
		foreach my $e (@l) {
			if ($e<$min) { $min=$e; }
			if ($e>$max) { $max=$e; }
		}
		return ($min,$max);
	}
	return ();
}

######################### Function finalCompletion()
# complete the anchor points after the reordering step
# For each ordered anchor point, the following relevant point are considered for each language. If two or more languages share the same followers, then the interval width are compared.
# For every group of languages with the same interval width, 1-to-1 pairing are made, if sentence lengths are roughly matching
# input : @orderedAnchor must be completed
# output : modification of the global hashes %anchor and %nextAnc 

sub finalCompletion {
	my $validAnchors=shift;
	
	printLog( "\n\n*** General completion by computing complete alignment for best language pairs first\n\n");
	$verbose && print "\n\nFinal completion\n";

	my $totAnchorPoints0=$totAnchorPoints;

	# stats for DTW
	$nbDtwCall=0;
	$sumInt1=0;
	$sumInt2=0;
	$maxInt1=0;
	$maxInt2=0;
	my $time0=time;
	
	@mergedAlignment=();
	%mergedAlignmentIndex=();
	# sorting the pairs by decreasing number of bipoints
	foreach my $ij (orderPairwizeCombination()) {
		my ($i,$j)=split(/-/,$ij);
		my $comb=join(" ",sort $allLang[$i],$allLang[$j]);
		$verbose && print "Full alignment extraction for language pair $comb\n";
		printLog("Full alignement extraction for language pair $comb\n");
		if (! exists($validAnchors->{$comb})) {
			$validAnchors->{$comb}=[];
		}
		extractCompleteAlignment($i,$j,1,$validAnchors->{$comb});
	}
	
	if ($printMergedAlignment) {
		my $OUT;
		printLog( "Printing merged alignment for @allLang\n");
		open($OUT,">:encoding($encoding)",$outputPath."/$fileName.full.".join("-",@allLang).".$outputFormat");
		printOutputHeader($OUT,$outputFormat,\@allLang);
		
		# sorting all the points 
		# initialisation 
		my @unsorted= grep {my $found=0;foreach my $lang (@allLang) { if (exists($_->{$lang}) && @{$_->{$lang}}>0 ) {$found=1}}; $found } @mergedAlignment;
		my @sorted=();

		# sorting iteratively for each language, with conservation of the previous order (the problem is that the order is partial, and that sort is not conservative)
		foreach my $lang (@allLang) {
			my @unsortedPart= grep { ! ( exists($_->{$lang}) && @{$_->{$lang}}>0) } @unsorted;
			my @sortedPart= sort { $a->{$lang}[0] <=> $b->{$lang}[0] } grep { exists($_->{$lang}) && @{$_->{$lang}}>0 } @unsorted;
			if (@sortedPart == 0) {
				print "$lang - Sorted=".@sorted."\n";
				print "Unsorted=".@unsorted."\n";
			}
			@unsorted=@unsortedPart;
			# points in @sortedPart are inserted into @sorted or into @unsorted (when the order is not determined)
			merge(\@sortedPart,\@sorted,\@unsorted,1);
		}
		printLog("Adding point with undeterminate order\n");
		# last iteration : strict mode must be relaxed
		merge(\@unsorted,\@sorted,[],0);

		sub merge {
			my $sortedPart=shift;
			my $sorted=shift;
			my $unsortedPart=shift;
			my $strictMode=shift;
			
			my $pos=0;
			foreach my $point (@{$sortedPart}) {
				
				# case 0 : the point has to be added in first position of @sorted
				if (sup($sorted->[0],$point)) {
					unshift(@{$sorted},$point);
					printLog("Adding %{$point} at the beginning\n");
				} else {
					my $i=$pos;
					# looking for the first point that is superior to $point
					while (defined($sorted->[$i]) && !sup($sorted->[$i],$point) && $i<$#$sorted) {
						$i++;
					}
					# case 1 : the point has to be added in last position of @sorted (if strictMode == 1, $point must be superior to the last point)
					if ( @{$sorted}==0 || ((sup($point,$sorted->[$i]) || !$strictMode) && $i==$#$sorted)) {
						printLog("Adding %{$point} at the end\n");
						push(@{$sorted},$point);
						$pos=$i;

					} elsif ((sup($point,$sorted->[$i-1])|| ! $strictMode) && sup($sorted[$i],$point)) {
					# case 2 : the point has to be added between pos $i and $i+1 of @sorted (if strictMode == 1, $point must be superior to $sorted[$i-1])
						printLog( "Adding %{$point} at pos $i\n");
						splice(@{$sorted},$i,0,$point);
						$pos=$i;
					# case 3 : indetermination : the point is added to @unsortedPart
					} else {
						printLog( "Undeterminate order for ");
						foreach my $lang (@allLang) {
							printLog("$lang:");
							if (exists($point->{$lang})) {
								printLog(join(" ",@{$point->{$lang}}));
							}
							printLog(",");
						}
						printLog("\n");
						push(@{$unsortedPart},$point);
					}
				}
			}
		}

		# superior or equal
		sub sup {
			my $point1=shift;
			my $point2=shift;
			
			#~ my $p1= Data::Dumper->Dump([$point1]);
			#~ my $p2= Data::Dumper->Dump([$point2]);
			#~ printLog("comparing $p1 >= $p2\n");
			
			foreach my $lang (@allLang) {
				if (exists($point1->{$lang}) && exists($point2->{$lang}) && @{$point1->{$lang}}>0 && @{$point2->{$lang}}>0) {
					if ($point1->{$lang}[0] >= $point2->{$lang}[0]) {
						return 1;
						printLog("Sup !!!!\n");
					} 
					#~ else {
						#~ printLog("NO !!!!\n");
					#~ }
				}
			}
			return 0;
		}

		
		my $i=1;
		foreach my $tuple (grep { my $found=0; foreach my $lang (@allLang) { if (exists($_->{$lang})) { $found=1 }}; $found }  @sorted) {
			if (defined($tuple)) {
				my $tu=join(" ; ", map { (exists($tuple->{$_}))? join(" ",sort @{$tuple->{$_}}):""; } @allLang) ;
				printTuple($OUT,$outputFormat,$tu,$i++,\@allLang);
			}
		}
		printOutputFooter($OUT,$outputFormat);
	}
	
	
	my $nbAnchorPoints=$totAnchorPoints-$totAnchorPoints0;
	printLog( "\n$nbAnchorPoints points were added by completion. Total : $totAnchorPoints - Elapsed : ".(time-$time0)."\n");
	my $averageInt1=$sumInt1/$nbDtwCall;
	my $averageInt2=$sumInt2/$nbDtwCall;
	printLog( "\n$nbDtwCall calls of DTW. averageInt1=$averageInt1, averageInt2=$averageInt2, maxInt1=$maxInt1, maxInt2=$maxInt2\n");
}






# compute the a posteriori best pairwise combination
sub orderPairwizeCombination {
	my %nbBiPoints;
	# computing of the number of bipoints for every language pair
	for (my $i=0;$i<$nbLang-1;$i++) {
		for (my $j=$i+1;$j<$nbLang;$j++) {
			$nbBiPoints{$i."-".$j}=keys %{$bilingualPoints[$i][$j]};
		}
	}
	# sorting the pairs by decreasing number of bipoints
	return sort {$nbBiPoints{$b} <=> $nbBiPoints{$a}} keys %nbBiPoints;
}

# compute an estimation of the best pairwise tiling
sub findBestPairwiseCombination {
	my @pairs=orderPairwizeCombination();
	my %notUsed=map {$_,1} @allLang;
	my %lang2groups; # record all the groups linked to a given language
	my @groups;
	my @bestGroups;

	while (keys %notUsed) {
		my ($l1,$l2)=split(/-/,shift @pairs);
		if (exists($notUsed{$l1}) or exists($notUsed{$l2})) {
			push @groups,"$l1-$l2";
			delete($notUsed{$l1});
			delete($notUsed{$l2});
			$lang2groups{$l1}.=" $l1-$l2";
			$lang2groups{$l2}.=" $l1-$l2";
		}
	}
	# Tiling: adding missing links for tiling
	my $previousGroup=$groups[1];
	foreach my $group (@groups) {
		push @bestGroups,$group;
		my ($l1,$l2)=split(/-/,$group);
		
		# if the current group is not overlapping another group then link it to the previous group
		if ($lang2groups{$l1} eq " ".$group && $lang2groups{$l2} eq " ".$group) {
			my ($l3,$l4)=split(/-/,$previousGroup);
			push @bestGroups, $l4."-".$l1;
			$lang2groups{$l1}.=" $l4-$l1";
			$lang2groups{$l4}.=" $l4-$l1";
		}
		$previousGroup=$group;
	}
	return @bestGroups;
}

######################### Function interPointDist()
#  returns the smalest offset between two coordinate of @vec
# arg1 (list adr) : a coordinate vector
# output (integer) : the smalest offset

sub interPointDist {
	my $vec=shift @_;
	
	if (@{$vec}<=1) { return $thresholdMinDist+1 };
	my $prec=$vec->[1];
	my $min=$vec->[1]-$vec->[0];
	my $dist=0;
	for (my $i=2;$i<@{$vec};$i++) {
		$dist=$vec->[$i]-$prec;
		if ($dist<$min) { $min=$dist };
		$prec=$vec->[$i];
	}
	return $min;
}


######################### Function sublist()
# returns all the n-1 sublist of a list of length n, where n>2
# e.g. [en,fr,it,es]->[[fr,it,es],[en,it,es],[en,fr,es],[en,fr,it]]
# arg1 (list adr) : a list
# output (list of list) : the list of the sublist

sub sublist {
	my @fullList=@_;
	my $n=$#fullList+1;
	my @slist=();
	
	
	unless ($n<3) {
		for (my $i=0;$i<$n;$i++) {
			my @list=@fullList;
			splice(@list,$i,1);
			push(@slist,[@list]);
			}
	}
	return (@slist);
}

######################### Function combinations($n,@list)
# returns all the n lenght sublist of a list @l
# e.g. combinations(3,en,fr,it,es)->([fr,it,es],[en,it,es],[en,fr,es],[en,fr,it])
# arg1 (integer) :  n, the size of sublists
# arg2 (liste) ; the liste @l that contains all sublists
# output (list of list) : the list of the sublists

sub combinations {
	my $n=shift;
	my @list=@_;
	my @result;

	# end of recursivity : if $n <=0
	if ($n<=0) {
		return ();
	}
	# end of recursivity : return the list of all single list with each elements 
	if ($n==1) {
		foreach my $elt (@list) {
			push(@result,[$elt]);
		}
		return @result;
	}
	# end of recursivity : if $n >= @list length
	if ($n>=@list) {
		push(@result,[@list]);
		return @result;
	}
	# recursive call
	my $firstElt=shift(@list);
	# computing all the sublists that include $firstElt
	@result=combinations($n-1,@list);
	foreach my $list (@result) {
		push(@{$list},$firstElt);
	}
	# computing all the sublists that don't include $firstElt
	push(@result, combinations($n,@list));
	return @result;
}


######################### Function allCombinations(@list)
# returns all the sublists of a list @l
# e.g. combinations(en,fr,it,es)->([fr,it,es],[en,it,es],[en,fr,es],[en,fr,it],[en,fr],[en,it],[en,es],[fr,es],[fr,it],[it,es],[en],[fr],[it],[es])
# arg1 (liste) ; the liste @l that contains all sublists
# output (list of list) : the list of the sublists

sub allCombinations {
	my @list=@_;
	my @result;
	if (@list==0) {
		return ();
	}
	# end of recursivity : return the list of all single list with each elements 
	if (@list==1) {
		return ([$list[0]]);
	}
	# recursive call
	my $firstElt=shift(@list);
	# computing all the sublists that don't include $firstElt
	@result=allCombinations(@list);
	# adding all the sublists that include $firstElt
	foreach my $list(@result) {
		my $new=[$firstElt,@{$list}];
		push(@result,$new);
	}
	return @result;
}


######################### Function previousCommonPoint()
# Given an anchor point, and a language combination (en,fr,it), the nextPoint function returns the next point shared by all the languages
# that fits with the language combination.
# arg1 (list adr) : an anchor point
# arg2 (list adr): a num language combination
# output (list): the previous anchor point 
# NB : in this version, the anchor point may not have coordinates for all the num of arg2 language combination
sub previousCommonPoint {
	my @point=@{shift @_};
	my @listNumLang=@{shift @_};
	my $i=0;
	while ($i<$#listNumLang && $point[$listNumLang[$i]]==-1) {
		$i++;
	}
	my $firstNonNullCoord=$listNumLang[$i];
	my $pt=$point[$firstNonNullCoord];
	if ($pt==-1) {
		print "ERROR : languages in @listNumLang are not defined for point @point\n";
		die;
	}
 	while ($pt>0) {
      	$pt--;
      	if (exists($anchor{$firstNonNullCoord}{$pt})) {
			@point=@{$anchor{$firstNonNullCoord}{$pt}};
			my $fitsLangCombin=1;
			foreach my $num (@listNumLang) {
				$fitsLangCombin=$fitsLangCombin && ($point[$num]>-1);
			}
			if ($fitsLangCombin) {return (@point)};
		}
	}
	return @beginPoint;
}

######################### Function nextCommonPoint()
# Given an anchor point, and a language combination (en,fr,it), the nextPoint function returns the next point shared by all the languages
# that fits with the language combination.
# arg1 (list adr) : an anchor point
# arg2 (list adr): a num language combination
# output (list): the next anchor point 

sub nextCommonPoint {
	my @point=@{shift @_};
	my @listNumLang=@{shift @_};
	my $i=0;
	while ($i<$#listNumLang && $point[$listNumLang[$i]]==-1) {
		$i++;
	}
	my $firstNonNullCoord=$listNumLang[$i];
	my $pt=$point[$firstNonNullCoord];
	if ($pt==-1 or ! defined($firstNonNullCoord)) {
		print "ERROR ($i): languages in @listNumLang are not defined for point @point\n";
		die;
		return ();
	}
 	while ($pt<$endPoint[$firstNonNullCoord]) {
		$pt++;
 		if ( exists($anchor{$firstNonNullCoord}{$pt})) {
			@point=@{$anchor{$firstNonNullCoord}{$pt}};
			my $fitsLangCombin=1;
			foreach my $num (@listNumLang) {
				$fitsLangCombin=$fitsLangCombin && ($point[$num]>-1);
			}
			if ($fitsLangCombin) {return (@point)};
		}
	}
	# after @endPoint, return empty list
	return ();
}

######################### Function occIn()
# Given two values, and an occurrence vector, the interval function return the part of the vector which occurs between (strictly) these two values
# arg1 (integer): the lower bound of the interval
# arg2 (integer): the upper bound of the interval
# arg3 (list) : the occurence vector
# output (list): the part of the occurrence vector which is (strictly) included in the interval
sub occIn {
	my $inf=shift @_;
	my $sup=shift @_;
	my @occVec=@{shift @_};
	my $i=0;
	my $j=$#occVec;
	while (($i<=$j)&&($occVec[$i]<=$inf)) {
		$i++;
	}
	while (($i<=$j)&&($occVec[$j]>=$sup)) {
		$j--;
	}
	if ($i<=$j) { 
		return (@occVec[($i..$j)]);
		}
	else {
		return ();
	}
}


# if points are not rejected, every coordinate pair count is incremented in $candidatePairs hash
# arg1 (adr): the new point
# arg2 (adr): the previous point
# arg3 : the concerned languages for which newpoint is added
# arg4 : the ref to the hash $candidatePairs
# output : modification of the global hashes %anchor and %nextAnc
# return value : 1 if the point is a valid candidate

sub incCandidatePairsCount {
	my @newPoint=@{shift @_};
	my @prevPoint=@{shift @_};
	my @listNumLang=@{shift @_};
	my $candidatePairs=shift @_;
	
	my $lang="";
	
	# the point is now added in the candidate hash. This hash records, for each language pair of @listLang, the number of times one sentence pair has been elected
	for (my $i=0;$i<=$#listNumLang-1;$i++) {
		for (my $j=$i+1;$j<=$#listNumLang;$j++) {
			$candidatePairs->{$listNumLang[$i]."-".$listNumLang[$j].":".$newPoint[$listNumLang[$i]].",".$newPoint[$listNumLang[$j]]}++;
		}
	}
} 

# arg1 (adr): the new point
# arg2 (adr): the previous point
# arg3 : the concerned languages for which newpoint is added
# arg4 : the ref to the hash $candidatePairs
# Return value : 
# 1 : the point is new and has been added
# 0 : the point has not been added because it is included or unsafe
# -1 : the point is discarded because conflicting
sub addPoints {
	my $newPoint=shift @_;
	my @listNumLang=@{shift @_};
	my @listLang=map {$allLang[$_]} @listNumLang;
	my $candidatePairs=shift;
	my $minMatchNumber=shift;
	my $validAnchors=shift;
	

	# when $minMatchNumber == 0, $candidatePairs is not used - all the coordinates of the current point are considered to be valid, event if elected one time
	if ($minMatchNumber>1 && $candidatePairs) {
		# All the language pairs inside the point are considered : if, for a given pair, the point has been elected more than one time, the coordinates are considered to be valid, and are recorded as keys of this hash
		my @validCoords;
		for (my $i=0;$i<=$#listNumLang-1;$i++) {
			for (my $j=$i+1;$j<=$#listNumLang;$j++) {
				if ($candidatePairs->{$listNumLang[$i]."-".$listNumLang[$j].":".$newPoint->[$listNumLang[$i]].",".$newPoint->[$listNumLang[$j]]} >=$minMatchNumber) {
					$validCoords[$listNumLang[$i]]=1;
					$validCoords[$listNumLang[$j]]=1;
				}
			}
		}
		# now all the coordinates that have not been validated are discarded
		for (my $i=0;$i<=$#allLang;$i++) {
			if (!defined($validCoords[$i])) {
				$newPoint->[$i]=-1;
			}
		}
	}
	
	# suppress the anomalous coordinates
	checkCoordinateParallelism($newPoint,0);
	
	# computing the first non null coord
	my $firstNonNullCoord=0;
	while ($firstNonNullCoord<=$#allLang && $newPoint->[$firstNonNullCoord]==-1) {
		$firstNonNullCoord++;
	}
	# if the point is null e.g. (-1 -1 -1 -1) return
	if ($firstNonNullCoord>$#allLang) {
		return 0;
	}
	
	# If the new anchor point overlap a previous one, the two points are merged if possible 
	# For example : @newpoint=(-1,15,14,-1) @oldPoint=(8,15,-1,-1) -> @NewPoint=(8,15,14,-1)
	LANG:foreach my $num (@listNumLang) {
		if (exists($anchor{$num}{$newPoint->[$num]})) {
			my @oldPoint=@{$anchor{$num}{$newPoint->[$num]}};
			for (my $i=0;$i<$nbLang;$i++) {
				if ($oldPoint[$i] != -1) {
					if ($newPoint->[$i] == -1) {
						$newPoint->[$i] = $oldPoint[$i];	# add this line to apply transitivity !
					} elsif ($newPoint->[$i] != $oldPoint[$i]) {
						printLog( "Conflict with merged point : point @{$newPoint} is discarded\n");
						return -1;
					}
				} 
			}
		}
	}


	# For each coord, searching for the predecessor and the successor and checking if there is no crossing between points
	# If there is a conflict, we try to solve it by modifying either the newPoint either the prevPoint, using geometrical triangulation
	# Then, coordinates of predecessor and successor are stored in @previousCoords and @nextCoords
	my @previousCoords;
	my @nextCoords;
	SEARCH_PREV_NEXT:for (my $i=0;$i<$nbLang;$i++) {
		if ($newPoint->[$i] != -1) {
			my $lang=$allLang[$i];
			my $newCoord=$newPoint->[$i];
			
			# looking for predecessor and conflict resolution
			my $previousPoint=previousPoint($newCoord,$i);
			if (! defined($previousPoint)) {
				print "Error : previous point does not exist for $newCoord (lang num $i)\n";
				die;
			}
			if (anchor1InfAnchor2($previousPoint,$newPoint) != 1) {
				printLog( "Conflict with previous point : point @{$newPoint} is discarded\n");
				return -1;
			}

			# looking for a successor and conflict resolution
			my $nextPoint=nextPoint($newCoord,$i);

			if (anchor1InfAnchor2($newPoint,$nextPoint) != 1) {
				printLog( "Conflict with next point : point @{$newPoint} is discarded\n");
				return -1;
			}
			
			# at this step, the predecessor and successor are ok
			$previousCoords[$i]=$previousPoint->[$i];
			$nextCoords[$i]=$nextPoint->[$i];

		}
	}
	
	# if the newPoint already exists, it is rejected
	if (exists($anchor{$firstNonNullCoord}{$newPoint->[$firstNonNullCoord]})) {
		# print "Comparaison de @{$anchor{$allLang[$firstNonNullCoord]}{$newPoint->[$firstNonNullCoord]}} avec @newPoint\n";
		if (isIncludedPoint($newPoint,$anchor{$firstNonNullCoord}{$newPoint->[$firstNonNullCoord]})) {
			printLog( "point @{$newPoint} included in a previous one\n");
			return 0;
		}
	}
	

	####################################### POINT IS RECORDED !
	# if no crossing coord has been encountered, the point is recorded and chaining is done
	for (my $i=0;$i<$nbLang;$i++) {
		if ($newPoint->[$i] != -1) {
			# chaining
			$nextAnc{$i}{$previousCoords[$i]}=$newPoint->[$i];
			$nextAnc{$i}{$newPoint->[$i]}=$nextCoords[$i];
			# adding anchor coordinate in %anchor
			$anchor{$i}{$newPoint->[$i]}=$newPoint;
		}
	}
	
	# update of @bilingualPoints array, which records, for each language pair $i,$j where $j>$i, the keys of all bilingual points
	
	for (my $i=0;$i<$nbLang-1;$i++) {
		for (my $j=$i+1;$j<$nbLang;$j++) {
			if ($newPoint->[$i] != -1 && $newPoint->[$j]!=-1 && ! exists($bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"})) {
				$bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"}=1;
			}
		}
	}
	

	printLog( "Adding @{$newPoint}\n");
	my $key=join(" ",@{$newPoint});

	
	# aligned sentences are compared in order to find new candidate cognate, for each language pair in the current anchor point
	if ($useCognate) {
		LANG1:for (my $i=0;$i<=$nbLang-2;$i++) {
			if ($newPoint->[$i] == -1) {
				next  LANG1;
			}
			my $lang1=$allLang[$i];
			my $coord1=$newPoint->[$i];
			my $sent1=$num_sent{$lang1}[$coord1];
			
			LANG2: for (my $j=$i+1;$j<=$nbLang-1;$j++) {
				if ($newPoint->[$j] == -1) {
					next LANG2;
				}
				my $lang2=$allLang[$j];
				my $coord2=$newPoint->[$j];
				if (! exists($cognateSearchIsDone{"$lang1-$coord1-$lang2-$coord2"})) {
					printLog("Looking for cognates in the sentences $lang1-$coord1 : $lang2-$coord2\n");
					$cognateSearchIsDone{"$lang1-$coord1-$lang2-$coord2"}=1;
					my $sent2=$num_sent{$lang2}[$coord2];
					foreach my $form1 (grep { exists($langVec{$_}) } @{$sent1}) { # too frequent forms are ignored (when $langVec{$form} does not exists)
						if (length($form1)>$cognateMinLength && $form1!~/\d/) { # cognates must not include digit
							FORM2:foreach my $form2 (grep { exists($langVec{$_}) } @{$sent2}) { # too frequent forms are ignored (when $langVec{$form} does not exists)
								# cognates must be different
								if ($form1 eq $form2) {
									next FORM2;
								}
								if (length($form2)>$cognateMinLength && $form2!~/\d/) {
									my $areCognates=0;
									if (exists($cognateComparisonIsDone{"$form1\t$form2"})) {
										# if the comparison have already been done
										$areCognates=$cognateComparisonIsDone{"$form1\t$form2"};
									} else {
										$areCognates=validCognates($form1,$form2);
										# the result is recorded for later use
										$cognateComparisonIsDone{"$form1\t$form2"}=$areCognates;
									}
									# if the pair corresponds to candidate cognates
									if ($areCognates) {
										printLog("$form1 && $form2 are cognates !!!\n");
										my ($idCognate1,$idCognate2,$idCognate);
										# $f1 and $f2 represents either the forms either the cognates associated to them
										my ($f1,$f2)=($form1,$form2);
										# if $lang1-$form1 is linked to a cognate, $f1 is set to it
										if (exists($tok_idCognate{$lang1}{$form1})) {
											$idCognate1=$tok_idCognate{$lang1}{$form1};
											$f1=$idCognate1;
										} 
										# if $lang2-$form2 is linked to a cognate, $f2 is set to it
										if (exists($tok_idCognate{$lang2}{$form2})) {
											$idCognate2=$tok_idCognate{$lang2}{$form2};
											$f2=$idCognate2;
										} 
										
										# case 1 : if $idCognate1 exists, both forms will be linked to it
										if ($idCognate1) {
											$idCognate=$idCognate1;
											$tok_idCognate{$lang2}{$form2}=$idCognate;
										# case 2 : else ig $idCognate exists both forms will be linked to it
										} elsif ($idCognate2) {
											$idCognate=$idCognate2;
											$tok_idCognate{$lang1}{$form1}=$idCognate;
										# case 3 : a new idCognate has to be created, and both forms will be linked to it
										} else {
											$currentCognateId++;
											$idCognate="##$currentCognateId##$form1#$form2";
											$tok_idCognate{$lang1}{$form1}=$idCognate;
											$tok_idCognate{$lang2}{$form2}=$idCognate;
										}

										# now all the occurrence vector of both forms/cognate will be merged, for each language
										foreach my $lang (@allLang) {
											my $l1=[];
											my $l2=[];
											if (exists($occVec{$lang}{$f1})) {
												$l1=$occVec{$lang}{$f1};
											}
											if (exists($occVec{$lang}{$f2})) {
												$l2=$occVec{$lang}{$f2};
											}
											if ( @{$l1}>0 || @{$l2}>0) {
												#~ printLog( "Merging of occVec{$lang}{$f1}=".@{$l1}." and occVec{$lang}{$f2}=".@{$l2}." for $lang\n");
												$occVec{$lang}{$idCognate}=mergeLists($l1,$l2);
												push(@{$langVec{$idCognate}},$lang);
											} 
										}

										# adding the new anchor to @validAnchors if not already in
										if ($validAnchors && included(\@listLang,$langVec{$idCognate}) && ! member($validAnchors,$idCognate)) {
											printLog( "Adding $idCognate to validAnchors\n");
											push(@{$validAnchors{join(" ",sort $lang1, $lang2) }},$idCognate);
										} 											
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return 1;
}


# Eliminate points with high deviation from the diagonals traced from previous and next point
sub checkCoordinateParallelism {
	my $point=shift;
	my $updateHashes=shift;
	my @deletedCoord;

	if ($updateHashes) {
		return; # !!!!
	}
		
	# counting coordinates that are <> from -1
	my @coordinates;
	for(my $i=0;$i<$nbLang;$i++) {
		if ($point->[$i]!=-1) {
			push(@coordinates,$i);
		}
	}
	
	if (@coordinates<2) {
		return;
	}
	
	my @inf=previousCommonPoint($point,\@coordinates);
	my @sup=nextCommonPoint($point,\@coordinates);
	if (@sup==0) {
		return;
	}
	my $inf=\@inf;
	my $sup=\@sup;
	
	# testing coordinates 2 by 2
	TWOBYTWO1:for (my $i=0;$i<=$#coordinates-1;$i++) {
		my $numLang1=$coordinates[$i];
		# if the coord has been deleted, next
		if (member(\@deletedCoord,$numLang1)) {
			next TWOBYTWO1;
		}
		TWOBYTWO2:for (my $j=$i+1;$j<=$#coordinates;$j++) {
			my $numLang2=$coordinates[$j];
			# if the coord has been deleted, next
			if (member(\@deletedCoord,$numLang2)) {
				next TWOBYTWO2;
			}

			my @inf=previousCommonPoint($point,[$numLang1,$numLang2]);
			my @sup=nextCommonPoint($point,[$numLang1,$numLang2]);
			
			# computes the distance between $point and $inf for $numLang1 and $numLang2 
			my $pointIntervalInf1=calcIntervalLength(\@inf,$point,$numLang1);
			my $pointIntervalInf2=calcIntervalLength(\@inf,$point,$numLang2);
			
			# computes the distance between $point and $sup for $numLang1 and $numLang2 
			my $pointIntervalSup1=calcIntervalLength($point,\@sup,$numLang1);
			my $pointIntervalSup2=calcIntervalLength($point,\@sup,$numLang2);

			# if both distances are different than the coordinates have to be ignored
			my $deviation=1;
			if ($excludeDeviationOnBothSide) {
				if (compatibleInterval($pointIntervalInf1,$pointIntervalInf2) && compatibleInterval($pointIntervalSup1,$pointIntervalSup2)){
					$deviation=0;
				}
			} else {
				if (compatibleInterval($pointIntervalInf1,$pointIntervalInf2) or compatibleInterval($pointIntervalSup1,$pointIntervalSup2)){
					$deviation=0;
				} 
			}
			if  ($deviation) {
				$point->[$numLang1]=-1;
				push(@deletedCoord,$numLang1);
				next TWOBYTWO1;
			} 
		}
	}
	
	return @deletedCoord;
}

# Decides whether intervals are compatible (comparable length or not)
# Various theshold are used according the interval lengths
sub compatibleInterval {
	my ($int1,$int2)=@_;
	
	for (my $i=0;$i<=$#maxDiffIntervals-1;$i++) {
		if ($int1<$maxDiffIntervals[$i]->[0] or $int2<$maxDiffIntervals[$i]->[0]) {
			if (diffInterval($int1,$int2)>$maxDiffIntervals[$i]->[1]) {
				return 0;
			} else {
				return 1;
			}
		}
	}
	if (diffInterval($int1,$int2)>$maxDiffIntervals[$#maxDiffIntervals]->[1]) {
		return 0;
	} else {
		return 1;
	}
}

# list merging for occurrence vector without doublons and with order conservation
# returns a reference to the merged list
sub mergeLists {
	my $l1=shift;
	my $l2=shift;
	my @l;
	
	# case of empty lists
	if (@{$l1}==0) {
		return $l2;
	}
	if (@{$l2}==0) {
		return $l1;
	}
	my $i=0;
	my $j=0;
	# for two non empty lists
	while ($i<@{$l1} && $j<@{$l2}) {
		my $e1=$l1->[$i];
		my $e2=$l2->[$j];
		if ($e1==$e2) {
			push(@l,$e1);
			$i++;
			$j++;
		} elsif ($e1<$e2) {
			push(@l,$e1);
			$i++;
		} else {
			push(@l,$e2);
			$j++;
		}
	}
	if ($i < @{$l1}) {
		push(@l,$l1->[$i]);
	}
	if ($j < @{$l2}) {
		push(@l,$l2->[$j]);
	}
	return \@l;
}

# cognate comparison
# return 1 if both the forms share more than $commonSubStringRatio letters
sub validCognates {
	my $form1=shift;
	my $form2=shift;
	if ($caseInsensitiveForCognates) {
		$form1=lc($form1);
		$form2=lc($form2);
	}
	my $min;
	if (length($form1)>length($form2)) { 
		$min=length($form2);
	} else {
		$min=length($form1);
	}
	my @l1=split(//,$form1);
	my @l2=split(//,$form2);
	my $scmTab=[];
	my $scm=maxSubString(0,0,\@l1,\@l2,$scmTab);
	if ($scm/$min>=$commonSubStringRatio) {
		#print "$form1 et $form2 -> Cognats !!!!!!\n";
		return 1;
	}
	return 0;
}

# Renvoie la longueur de la plus longue sous chaîne max entre les tableaux de car. $l1 et $l2, à partir des coordonnées ($i,$j) - les longueurs déjà calculées étant stockées dans le tableau $scmTab. 
# Au premier appel on fait donc : $scmTab=[]; $scm=maxSubString(0,0,split(//,$string1),split(//,$string2),$scmTab)
sub maxSubString {
    my ($i,$j,$l1,$l2,$scmTab)=@_;

	# la récursivité s'arrête si un des deux indices arrive en fin de tableau
    if (($i==@{$l1}) || ($j==@{$l2}) )  {
        return 0;
    }
	# si l'écart entre $i et $j est supérieur Ã  $diffMax, fin de la récursivité
	if ($diffMaxSCM && abs($i-$j)>$diffMaxSCM) {
		return 0;
	}
	
	# si la valeur a déjà été calculé lors d'un précéent appel, on ne refait pas le calcul
    if (defined($scmTab->[$i]) && defined($scmTab->[$i][$j])) {
		return $scmTab->[$i][$j];
    }
   
	# récursivité selon $i
	my $tete1 = $l1->[$i];
	my $index=-1;
	for (my $k=$j;$k<@{$l2};$k++) {
		if ($l2->[$k] eq $tete1) {
			$index=$k;
			last;
		}
	}

	# initialisation de $SCM[$i+1]
	if (!defined($scmTab->[$i+1])) {
		$scmTab->[$i+1]=[];
	}
	
	 # s'il n'y a pas d'occurrence de la $tete1 dans @sent2[$j..n]
	if ($index == -1) {
		my $length=maxSubString($i+1,$j,$l1,$l2,$scmTab);
		$scmTab->[$i+1][$j]=$length;
		return $length;
	} else {
		# premier calcul : on tient compte du matching de $tete1, et on relance le calcul récursivement pour les listes restante
		my $length1= maxSubString($i+1,$index+1,$l1,$l2,$scmTab);
		$scmTab->[$i+1][$index+1]=$length1;

		# deuxième calcul : on ne tient pas compte du matching de tête1, et on relance le calcul pour $i+1,$j
		my $length2 = maxSubString($i+1,$j,$l1,$l2,$scmTab);
		$scmTab->[$i+1][$j]=$length2;

		# on valide le meilleur des deux chemins
		if ($length1+1 >= $length2) {
			return $length1+1;
		} else {
			return $length2;
		}
	}
}
	
# Function isIncludedPoint : returns 1 if point in arg1 is included in point in arg2 (inclusion means that every coordinate different from -1 are equal)
sub isIncludedPoint {
	my @l1=@{shift @_};
	my @l2=@{shift @_};
	for (my $i=0;$i<=$#l1;$i++) {
		if ($l1[$i]!=-1 && $l1[$i]!=$l2[$i]) { return 0; }
	}
	return 1;
}


######################### Function reorder
# Merge all the %anchor{$lang} in a single hash
# Pointers (stored in @currentCoord) indicate the current position for each language. At each step, all the pointed points
# are compared, and the "inferior" point is added to the ordered set. Then the pointer moves to the next point, 
# for every language which is at the same level as the "inferior" point.

# returns  the array @orderedAnchor
sub reorder {
	my $anchorNum=0;
	my $indexMin=0;
	my @orderedAnchor=(\@beginPoint);
	my %insertedAnchor;
	
	# adding singletons : tuples that concerns only one language
	
	my @currentCoord=@beginPoint;	 	# records the current coordinates to look at newpoints
	my @lastCoord=@beginPoint; 			# records the coordinates of the last inserted point. We have @lastCoord < @currentCoord and  @lastCoord < @newPoint
	while (not equal(\@currentCoord,\@endPoint)) {
		#print "arg1 : $allLang[0]    arg2 : $currentCoord[0]\n";$name = <STDIN>;
		# looking for the various anchors that share a coordinate with @currentCoord, and taking of the first one according to the order relation
		my @anchorMin=@{$anchor{0}{$currentCoord[0]}};
		my @anchorMins; # contains all the anchors that are in an indeterminate position regarding @anchorMin
		for (my $i=1;$i<$nbLang;$i++) {
			if (!exists($anchor{$i}{$currentCoord[$i]})) {
				die "anchor{$i}{$currentCoord[$i]} n'existe pas\n";
			}
			my @anc=@{$anchor{$i}{$currentCoord[$i]}};
			my $infAnc=anchor1InfAnchor2(\@anc,\@anchorMin);
			my $infLast=anchor1InfAnchor2(\@lastCoord,\@anc);
			# In any case, the new@anc must be strictly > @lastCoord
			# @anc is strictly inferior : it is the new unique @anchorMin
			if ($infAnc==1 && $infLast==1) {
				@anchorMin=@anc;
				@anchorMins=();
			# @anc is in an undetermined position regarding @anchorMin or they are matching but not equal :  both the points have to be considered
			} elsif (($infAnc==-2 || $infAnc==0.5) && $infLast==1) {
				#~ print "infAnc $infAnc infLast $infLast\n";
				push(@anchorMins,[@anc]);
			}
		}
		# adding @anchorMins in the set
		if (anchor1InfAnchor2(\@lastCoord,\@anchorMin)==1) {
			unshift(@anchorMins,\@anchorMin);
		}

		# if @anchorMins is empty, the next @currentCoord point is @nextAnc{currentCoord}
		if (@anchorMins==0) {
			for (my $i=0;$i<$nbLang;$i++) {			
				if (!exists($nextAnc{$i}{$currentCoord[$i]})) {
					die "nextAnc{$i}{$currentCoord[$i]} n'existe pas\n";
				}
				if (! defined($nextAnc{$i}{$currentCoord[$i]})){
					print "nextAnc{$i}{$currentCoord[$i]} n'est pas défini\n";
					printLog( "nextAnc{$i}{$currentCoord[$i]} n'est pas défini\n");
				} elsif ($nextAnc{$i}{$currentCoord[$i]}!=-2 ) {
					#~ print "Mise à jour de currentCoord[$i] (en l'absence d'anchorMin compatible) : currentCoord[$i]=$currentCoord[$i] ; next=$nextAnc{$i}{$currentCoord[$i]}\n";
					$currentCoord[$i]=$nextAnc{$i}{$currentCoord[$i]};
				}
			}
		} else {
			# If @anchorMins is not empty
			# the set is sorted because order may be not undetermined between all anchorMins
			@anchorMins=sort anchor1SupAnchor2 @anchorMins;
			# adding the first anchor point of @anchorMins that is really new
			my $newAnchor;
			while ( $newAnchor=shift(@anchorMins)) {
				if (! exists($insertedAnchor{join("-",@{$newAnchor})})) {
					$anchorNum++;
					if ((grep {!/-1/} @{$newAnchor}) >1) {
						$verbose && print "Ajout d'un point orderedAnchor[$anchorNum]=[@{$newAnchor}]\n";
						push(@orderedAnchor,[@{$newAnchor}]);
						$insertedAnchor{join("-",@{$newAnchor})}=1;
					}
					last;
				} 
			}
			
			# computation of the new coordinates of @currentCoord. For each coordinate of $anchorMin, the nextAnc coordinate is taken
			for (my $i=0;$i<$nbLang;$i++) {
				my $coord=$newAnchor->[$i];
			
				# taking the next coordinate for $coord at language $i
				if ($coord > -1) {
					if (exists($nextAnc{$i}{$coord}  )) {
						if ($nextAnc{$i}{$coord} <$lastCoord[$i]) {
							print  "Anomalie : max=$coord lang=$allLang[$i] lastCoord[$i]=$lastCoord[$i] > Next=$nextAnc{$i}{$coord} - NextNext= $nextAnc{$i}{$nextAnc{i}{$coord}}\n";
						} else {
							$lastCoord[$i]=$coord;
							$currentCoord[$i]=$nextAnc{$i}{$coord};
							#~ print "Mise à jour de currentCoord[$i] : anchorMin(i)=$coord ; next=$nextAnc{$i}{$coord}\n";
						}
					} else {
						die "Pas de nextAnc{$i}{$coord}  pour @anchorMin coord $i\n";
					}
					#print "Nouvelle coordonnée pour $i : $currentCoord[$i]\n";
				}
			}
		}
	}
	push(@orderedAnchor,[@endPoint]);
	return @orderedAnchor;
}

######################### Function anchor1InfAnchor2
# Return 1 if anchor1 is before than anchor2. The problem is that anchor1 and anchor2 do not share any language. 
# Thus we have to look at the previous and next point of anchor1 and anchor2, to determine the order
# arg1 (list address) : pointer to anchor1
# arg2 (list address) : pointer to anchor2
# output : 
# 1 if strictly inferior (if and only if anchor1 should appear before anchor2)
# 0.5 if matching but not equal
# 0 if  strictly equal
# -1 if superior 
# -2 if undetermined
# -3 if crossing points

sub anchor1InfAnchor2 {
	my @anchor1=@{shift @_};
	my @anchor2=@{shift @_};
	my $i;
	my $j;
	my $numL;
	my $matching=0;
	my $notEqual=0;
	my $inf=0;
	my $sup=0;
	# simple case : @anchor1 and @anchor2 share a common language
	for ($numL=0;$numL<$nbLang;$numL++) {
		if ($anchor1[$numL]!=-1 && $anchor2[$numL]!=-1) {
			if ($anchor1[$numL]<$anchor2[$numL]) {
				$inf++;
			} elsif ($anchor1[$numL]>$anchor2[$numL]) {
				$sup++;
			} else {
				$matching++;
			}
		} elsif ($anchor1[$numL]!=-1 || $anchor2[$numL]!=-1) {
			$notEqual=1;
		}
	}
	
	# simple case : at least one coordinate is stricty inferior with no crossing
	if ($inf>0 && $sup==0 && $matching==0) {
		return 1;
	}
	# simple case : at least one coordinate is stricty superior with no crossing
	if ($sup>0 && $inf==0 && $matching==0) {
		return -1;
	}
	# simple case : all the common coordinates are equal but there are differing coordinate 
	# such point should already be merged
	if ($matching>0 && $notEqual && $inf==0 && $sup==0) {
		return 0.5;
	}	# simple case : all the  coordinates are strictly equal
	if ($matching>0 && $inf==0 && $sup==0) {
		return 0;
	}
	
	# crossing and partially overlapping point
	if ($inf>0 && $matching>0 || $sup>0 && $matching>0) {
		#~ print "crossing points\n";
		return -3;
	}
	
	# strictly crossing points
	if ($inf>0 && $sup>0) {
		#~ print "crossing points\n";
		return -3;
	}
	
	#~ print "Comparaison des points sans langage commun : @anchor1 et @anchor2 \n";

	# difficult case : @anchor1 and @anchor2 do not share a common language
	for ($i=0;$i<$nbLang;$i++) {
		if ($anchor1[$i]!=-1) {
			for ($j=0;$j<$nbLang;$j++) {
				if ($anchor2[$j]!=-1) {
					my $p1=prevCoord(\@anchor1,$j);
					my $n1=nextCoord(\@anchor1,$j);
					my $p2=prevCoord(\@anchor2,$i);
					my $n2=nextCoord(\@anchor2,$i);
					if ($n1<$anchor2[$j] && $p2>$anchor1[$i]) {
						# a point after @anchor1 is strictly before @anchor2, or a point before @anchor2 is strictly after @anchor1 : @anchor1<@anchor2
						return 1;
					}
					if ($n2<$anchor1[$i] && $p1>$anchor2[$j]) {
						# a point after @anchor2 is strictly before @anchor1, or a point before @anchor1 is strictly after @anchor2 : @anchor1>@anchor2
						return 0;
					}
				}
			}
		}
	}
	#~ print "undetermined\n";
	# the order is undeterminate so far.
	return -2;
}

# building of the sort function
sub anchor1SupAnchor2 ($$) {
	my ($anc1,$anc2)=@_;
	my $inf= anchor1InfAnchor2($anc1,$anc2);
	if ($inf==1) {return -1;} # $a < $b
	if ($inf==-1) { return 1;} # $a > $b
	# equal or undetermined or crossing
	return 0;
}

######################### Function prevPoint
# Given one coordinate for a given language number nearest previous point for this language
# arg1 (integer) : the coordinate
# arg2 (integer) : the number of the language
# output : the previous point

sub previousPoint {
	my $coord=shift;
	my $numL=shift;
	
	
	while ($coord>0) {
		$coord--;
		if (exists($anchor{$numL}{$coord})) {
			return $anchor{$numL}{$coord};
		}
	}
	return $anchor{$numL}{0};
}

######################### Function nextPoint
# Given one coordinate for a given language number nearest next point for this language
# arg1 (integer) : the coordinate
# arg2 (integer) : the number of the language
# output : the next point

sub nextPoint {
	my $coord=shift;
	my $numL=shift;
	
	while ($coord<$endPoint[$numL]) {
		$coord++;
		if (exists($anchor{$numL}{$coord})) {
			return $anchor{$numL}{$coord};
		}
	}
	return \@endPoint;
}

######################### Function prevCoord
# Given one point, and given a language number (which is missing in the point), return the coordinate 
# (for the searched language) of the nearest previous point that concerns the missing language
# arg1 (list adress) : the point 
# arg2 (integer) : the number of the missing language
# output : the coordinate of the nearest previous point

sub prevCoord {
	my $anc=shift @_;
	my $searchedNum=shift @_;
	my $max=0;

	my $coordk;
	# the coordinate is searched in all the hashes of the language involved by the point
	for (my $k=0;$k<$nbLang;$k++){
		my $coordK=$anc->[$k];
		if ($coordK!=-1){
			while ($coordK=>0) {	# scanning of the hash from the point to the beginning
				$coordK--;
				if (exists($anchor{$k}{$coordK})) {
					my $coord=$anchor{$k}{$coordK}[$searchedNum];
					if ($coord!=-1){
						if ($max<$coord) {
							$max=$coord;
						}else{
							$coordK=-1;
						}
					}
				}
			}
		}
	}
	return $max;
}

######################### Function nextCoord
# Given one point, and given a language number (which is missing in the point), return the coordinate 
# (for the searched language) of the nearest next point that concerns the missing language
# arg1 (list adress) : the point 
# arg2 (integer) : the number of the missing language
# output : the coordinate of the nearest next point

sub nextCoord {
	my @anc=@{shift @_};
	my $searchedNum=shift @_;
	my $min=$endPoint[$searchedNum];
	# the coordinate is searched in all the hashes of the language involved by the point
	for (my $k=0;$k<$nbLang;$k++) {
		my $coordK=$anc[$k];
		if ($coordK!=-1) {
			while ($coordK<=$endPoint[$k]) {	# scanning of the hash from the point to the beginning
				$coordK++;
				if (exists($anchor{$k}{$coordK})) {
					my $coord=$anchor{$k}{$coordK}[$searchedNum];
					if ($coord!=-1) {
						if ($min>$coord) {
							$min=$coord;
						}
						else {
							$coordK=$endPoint[$k]+1;
						}
					}
				}
			}
		}
	}
	return $min;
}

######################### Function equal
# List comparaison
# arg1 (list address) : first list
# arg2 (list address) : second list
# output : 1 if both lists have the same content

sub equal {
	my @l1=@{shift @_};
	my @l2=@{shift @_};
	my $e=1;
	my $i;
	if ($#l1!=$#l2) {return 0};
	for ($i=0;$i<=$#l1;$i++) {
		$e=$e&&($l1[$i]==$l2[$i]);
	}
	return $e;
}

######################### Function incluse
# List comparaison
# arg1 (list address) : first list
# arg2 (list address) : second list
# output : 1 if @l1 is included in @l2, with the same order

sub included {
	my @l1=@{shift @_};
	my @l2=@{shift @_};
	my $j=0;
	for (my $i=0;$i<=$#l1;$i++) {
		while ($l1[$i] ne $l2[$j]) {
			$j++;
			if ($j>$#l2) { return 0 };
		}
	}
	return 1;
}

# loading a set of points from a file
# arg1 : the name of the file that must be csv encoded with a name of the form : name.l1-l2-l3-...-ln.ext
sub loadPoints {
	my $file=shift;

	my $totAnchorPoints=0;
	
	#~ %anchor=();
	#~ %nextAnc=();
	@orderedAnchor=(\@beginPoint);
	#~ %numLang=();
	#~ %num_Id=();
	#~ 
	#~ if ($file=~/\.((\w\w-)+\w\w)\./) {
		#~ @allLang=sort split(/-/,$1);
		#~ for (my $i=0;$i<=$#allLang;$i++) {
			#~ $numLang{$allLang[$i]}=$i;
			#~ $num_Id{$allLang[$i]}=[];
			#~ $id_num{$allLang[$i]}={};
		#~ }
		#~ $nbLang=@allLang;
	#~ };
	#~ @beginPoint=(0) x $nbLang;
	my @lastCoord=(0) x $nbLang; # record the last encountered coord for each language
	
	my @currentPoint;
	if (! -f $file) {
		die $file." does not exists.\n";
	}
	open(POINTS,$file);
	while (!eof(POINTS)) {
		my $line=<POINTS>;
		chomp $line;
		if ($line) {
			@currentPoint=split(/\t/,$line);
			if (@currentPoint!=$#allLang+1) {
				die "Wrong number of columns in anchor point file $file \n";
			}
			my $newPoint=[@currentPoint];
			push(@orderedAnchor,$newPoint);
			$totAnchorPoints++;
			for (my $i=0;$i<$nbLang;$i++) {
				if ($currentPoint[$i]!=-1) {
					#~ $num_Id{$allLang[$i]}[$currentPoint[$i]]=$currentPoint[$i];
					#~ $id_num{$allLang[$i]}{$currentPoint[$i]}=$currentPoint[$i];
					# storing the point
					$anchor{$i}{$currentPoint[$i]}=$newPoint;
					# chaining the previous point to the current point
					$nextAnc{$i}{$lastCoord[$i]}=$currentPoint[$i];
					# record the current coordinate for the future chaining
					$lastCoord[$i]=$currentPoint[$i];
				}
			}
			
			# update of @bilingualPoints which record the number of bipoints for each language pair
			for (my $i=0;$i<$nbLang-1;$i++) {
				for (my $j=$i+1;$j<$nbLang;$j++) {
					if ($newPoint->[$i] != -1 && $newPoint->[$j]!=-1 && ! exists($bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"})) {
						$bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"}=1;
					}
				}
			}
		}
	}
	close(POINTS);
	@endPoint=@currentPoint;
	for (my $i=0;$i<$nbLang;$i++) {
		$nextAnc{$i}{$endPoint[$i]}=-2;
	}
	print "$totAnchorPoints anchor points has been read\n";
}

# if primary anchor points are defined, adding
sub addPrimaryAnchorPoints {

	my $totAnchorPoints=0;
	
	@orderedAnchor=(\@beginPoint);
	
	my @lastCoord=(0) x $nbLang; # record the last encountered coord for each language
	
	my @currentPoint;

	for (my $i=0;$i<@{$anchors{$allLang[0]}};$i++) {
		@currentPoint=();
		for (my $langNum=0;$langNum<=$#allLang;$langNum++) {
			my $newCoord=$anchors{$allLang[$langNum]}[$i];
			if (! defined($newCoord)) {
				print "ERROR : missing primary anchor point markup for language $allLang[$langNum]. Next points will be ignored...\n";
				return;
			}
			push(@currentPoint,$newCoord);
		}
			
		print "ancrage @currentPoint\n";
		my $newPoint=[@currentPoint];
		push(@orderedAnchor,$newPoint);
		$totAnchorPoints++;
		for (my $i=0;$i<$nbLang;$i++) {
			if ($currentPoint[$i]!=-1) {
				# storing the point
				$anchor{$i}{$currentPoint[$i]}=$newPoint;
				# chaining the previous point to the current point
				$nextAnc{$i}{$lastCoord[$i]}=$currentPoint[$i];
				# record the current coordinate for the future chaining
				$lastCoord[$i]=$currentPoint[$i];
			}
		}
		
		# update of @bilingualPoints which record the number of bipoints for each language pair
		for (my $i=0;$i<$nbLang-1;$i++) {
			for (my $j=$i+1;$j<$nbLang;$j++) {
				if ($newPoint->[$i] != -1 && $newPoint->[$j]!=-1 && ! exists($bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"})) {
					$bilingualPoints[$i][$j]{"$newPoint->[$i]\t$newPoint->[$j]"}=1;
				}
			}
		}
	}

	@endPoint=@currentPoint;
	for (my $i=0;$i<$nbLang;$i++) {
		$nextAnc{$i}{$endPoint[$i]}=-2;
	}
	printLog( "$totAnchorPoints primary anchor points has been read\n");
}

sub pointId {
	my $point=shift;
	my $id="";
	for (my $j=0;$j<$nbLang;$j++) {
		if ($point->[$j]!=-1) {
			#~ print " ".$allLang[$j]."-".$num_Id{$allLang[$j]}." [".$point->[$j]."]\n";
			$id.=" ".$allLang[$j]."-".$num_Id{$allLang[$j]}[$point->[$j]];
		}
	}
	return $id;
}

sub member {
	my $l=shift;
	my $e=shift;
	foreach my $elt (@{$l}) {
		if ($elt eq $e) {
			return 1;
		}
	}
	return 0;
}

sub min {
	my $min=shift @_;
	my @l=@_;
	foreach my $e (@l) {
		if ($e<$min) {
			$min=$e;
		}
	}
	return $min;
}

sub max {
	my $max=shift @_;
	my @l=@_;
	foreach my $e (@l) {
		if ($e>$max) {
			$max=$e;
		}
	}
	return $max;
}


# LOG and debug trace handling
sub printLog {
	my $msg=shift @_;
	if ($printLog) {
		print LOG $msg;
	}

}

sub distanceDTW {
	my $length1=shift;
	my $length2=shift;
	my $transition=shift;
	if ($distanceGaleChurch) {
		return distanceGaleChurch($length1,$length2,$transition);
	} else {
		return abs($length1-$length2)*$distFactor{$transition};
	}
}

# computation of Gale & Church 1991 distance, according to $length1, $length2, $transition=(11|10|01|12|21|13|31|22)
# first implementation
sub distanceGaleChurch {
	my $length1=shift;
	my $length2=shift;
	my $transition=shift;
	
	my $dLength;
	my $delta;
	if ($length1*$length2==0) {
		$dLength=$maxDistanceGaleChurch;
	} else {
		$delta=int(abs($length2-$length1)/($varianceGC*sqrt($length1))/$dt)*$dt;
		if ($delta>10) {
			$dLength=$maxDistanceGaleChurch;
		} else {
			my $sigma=sigma($delta);
			if (2*(1-sigma($delta)/$sqrt2pi)<=0) {
				$dLength=$maxDistanceGaleChurch;
			} else {
				$dLength=-log(2*(1-sigma($delta)/$sqrt2pi));
			}
		}
	}
	my $dTransition=-log($probTransition{$transition});
	#~ print "$transition, $delta : $dLength+$dTransition\n";<STDIN>;
	return $dLength+$dTransition;
}

sub sigma {
	my $delta=shift;
	if (exists($sigma{$delta})) {
		return $sigma{$delta};
	}
	if ($delta<$minusInf) {
		return 0;
	}
	if ($delta>-$minusInf) {
		return $sqrt2pi;
	}
	$sigma{$delta}=sigma($delta-$dt)+exp(-($delta*$delta)/2)*$dt;
	return $sigma{$delta};
}

# original formula of Gale & Church

sub distanceGC {
	my $length1=shift;
	my $length2=shift;
	my $transition=shift;
	
	my $dLenght=match($length1,$length2);
	my $dTransition=-log($probTransition{$transition});
	return $dLenght+$dTransition;
}

sub match {
	my $length1=shift;
	my $length2=shift;
	
	my $c=1;
	my $s2=6.8;
	
	if ($length1==0 && $length2==0) {
		return 0;
	}
	my $mean = ($length1+$length2/$c)/2;
	my $z=abs($c*$length1-$length2)/sqrt($s2*$mean);
	my $pd=2*(1-pnorm($z));
	if ($pd >0) {
		return -log($pd);
	} else {
		return $maxDistanceGaleChurch;
	}
}

sub pnorm {
	my $z=shift;
	my $t=1/(1+0.2316419*$z);
	my $pd=1-0.3989423*exp(-$z*$z/2)*((((1.330274429*$t-1.821255978)*$t+1.781477937)*$t-0.356563782)*$t+0.319381530)*$t;
	# see Abramowitz, M., and I. Stegun (1964), 26.2.17 p. 932 
	return($pd); 
}
