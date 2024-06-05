# Makefile for https://github.com/Naereen/Assistant-traduction-CranialInsertion-en-Python/
SHELL=/usr/bin/env /bin/bash

all: activate-venv import-the-latest-article translate-card-names

activate-venv:
	. ./venv/bin/activate

import-the-latest-article:	activate-venv
	./venv/bin/jupyter execute import-the-latest-article.ipynb

translate-card-names:	activate-venv
	./venv/bin/jupyter execute translate-card-names.ipynb

