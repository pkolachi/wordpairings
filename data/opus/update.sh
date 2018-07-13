#!/bin/bash

LANGS=( ar as bg bn ca cs da de el en es et eu fi fr ga gu he hi hu ia is it ja kn ko la lv ml mr mt nl no or pa pl pt ro ru sv zh )
DATASET=( dic freq )
CORPUS=( OpenSubtitles2018 EUbookshop DGT Europarl EMEA ParaCrawl JRC-Acquis )
GIZADIR=false 

for corpname in ${CORPUS[@]} 
do 
    echo "Creating directory for corpus ${corpname}" ; 
    mkdir -p ${corpname} ; 
    
    for dataset in ${DATASET[@]}
    do 
	mkdir -p "${corpname}/${dataset}" ;
    done

    for lang in ${LANGS[@]}
    do 
	# download freq dataset
	url="http://opus.nlpl.eu/download.php?f=${corpname}%2Ffreq%2F${corpname}.${lang}.gz" 
	outfile="${corpname}/freq/${corpname}.${lang}.gz" ; 
	if [[ `wget -S --spider ${url} 2>&1 | grep 'HTTP/1.1 200 OK'` ]]; then
	    echo "Downloading frequency list for ${lang} from ${url} to ${outfile}" ;
	    wget -O ${outfile} ${url} ; 
	    exits=$? ; 
	    if [[ $exits != 0 ]] ; then 
		echo "rm -i ${outfile}" ; 
	    fi
	fi
    done

    echo "Take 5 .... 2 times" ; 
    sleep 10 ; 

    for srclang in ${LANGS[@]}
    do
	for tgtlang in ${LANGS[@]}
	do 
	    if [[ ${srclang} != ${tgtlang} ]] ; then
		pairname="${srclang}-${tgtlang}" ;
		url="http://opus.nlpl.eu/download.php?f=${corpname}%2Fdic%2F${pairname}.dic.gz"
		outfile="${corpname}/dic/${pairname}.dic.gz" ;

		if [[ `wget -S --spider ${url} 2>&1 | grep 'HTTP/1.1 200 OK'` ]]; then
		    echo "Downloading dictionary for ${pairname} from ${url} to ${outfile}" ;
		    wget -O ${outfile} ${url} ; 
		    exits=$? ; 
		    if [[ $exits != 0 ]] ; then
			echo "rm -i ${outfile}" ; 
		    fi

		    # since language pair is confirmed to exist; also download probabilistic dictionaries 
		    if [[ ${GIZADIR} ]] ; then
			mkdir -p "${corpname}/${pairname}/model" ; 
			url="http://opus.nlpl.eu/download/${corpname}/${pairname}/model/lex.e2f.gz" ; 
			outfile="${corpname}/${pairname}/model/lex.e2f.gz" ; 
			wget -O ${outfile} ${url} ; 
			url="http://opus.nlpl.eu/download/${corpname}/${pairname}/model/lex.f2e.gz" ; 
			outfile="${corpname}/${pairname}/model/lex.f2e.gz" ; 
			wget -O ${outfile} ${url} ; 
		    fi
		fi
	    fi
	done
	echo "Take 5 ...." ;
	sleep 5 ; 
    done

done

