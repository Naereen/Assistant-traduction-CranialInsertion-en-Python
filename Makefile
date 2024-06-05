# Makefile for https://github.com/Naereen/Assistant-traduction-CranialInsertion-en-Python/
SHELL=/usr/bin/env /bin/bash

all: activate-venv import-the-latest-article translate-card-names

clean:
	-mv -vf ./id_article.txt /tmp/
	-mv -vf ./articles/*/*.md /tmp/

activate-venv:
	. ./venv/bin/activate

import-the-latest-article:
	./venv/bin/jupyter execute import-the-latest-article.ipynb
	echo id_article.txt
	cat id_article.txt

translate-card-names:
	./venv/bin/jupyter execute translate-card-names.ipynb
	ls -larth articles/english/* | tail -n1
	ls -larth articles/francais/* | tail -n1

