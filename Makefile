# Makefile for https://github.com/Naereen/Assistant-traduction-CranialInsertion-en-Python/
all: import-the-latest-article translate-card-names

import-the-latest-article:
	runipy import-the-latest-article.ipynb

translate-card-names:
	runipy translate-card-names.ipynb

