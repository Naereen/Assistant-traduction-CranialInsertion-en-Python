# Assistant à la traduction de Cranial-Insertion (en Python)
> Encore un projet amusant et pas très utile, mais cela me fera réviser des vieilles compétences !
> En web scrapping, en utilisation d'API web avec une surcouche Python, etc.

## Objectifs

Aider à la traduction des articles sur <https://www.cranial-insertion.com/>, que l'on doit chaque weekend traduire de l'anglais américain vers le français.

## Choses à faire

Idéalement, je cherche à automatiser autant que possible, mais je vais commencer modestement :

- [ ] Idéalement : automatiser tout ce qui serait possible d'être automatisés...
- [x] Première étape : je veux un script qui utilise mes cookies de connexion pour automatiquement aller chercher le texte (fichier `.md`) du dernier article en anglais (depuis <https://www.cranial-insertion.com/staff/articles>) et le sauvegarde dans `articles/english/NUMERO.md` (eg. [`articles/english/4238.md`](articles/english/4238.md)). En plus, cet identifiant est sauvegardé [ici (`id_article.txt`)](id_article.txt) ;

```bash
$ make import-the-latest-article
./venv/bin/jupyter execute import-the-latest-article.ipynb
[NbClientApp] Executing import-the-latest-article.ipynb
[NbClientApp] Executing notebook with kernel: python3
echo id_article.txt
id_article.txt
cat id_article.txt
4238
```

- [x] Deuxième étape : je veux un script qui remplace les morceaux citant des cartes, par leur nom de cartes en français (avec [Scrython](https://github.com/NandaScott/Scrython) probablement, ou un accès web manuel à [Magic Ville](https://www.magic-ville.com/fr/) ou [Scryfall](https://scryfall.com/)). Par exemple je veux remplacer :

```markdown
[q]If there's a [c]Collector Ouphe[/c] in play, can I still tap [c]Talisman of Creativity[/c] for mana?[/q]

==>

[q]If there's a [c=Collector Ouphe]Orphe collectionneur[/c] in play, can I still tap [c=Talisman of Creativity]Talisman de créativité[/c] for mana?[/q]
```

```bash
$ make translate-card-names
./venv/bin/jupyter execute translate-card-names.ipynb
[NbClientApp] Executing translate-card-names.ipynb
[NbClientApp] Executing notebook with kernel: python3
ls -larth articles/english/* | tail -n1
-rw-rw-r-- 1 lilian lilian 13K juin   5 18:20 articles/english/4238.md
ls -larth articles/francais/* | tail -n1
-rw-rw-r-- 1 lilian lilian 13K juin   5 18:21 articles/francais/4238.md
```

- [ ] Troisième étape : je rêve d'un script qui traduise tout seul le document pré-modifié. Cela semble difficile, ou plutôt, pas très malin : cette traduction automatique ne sera forcément pas  parfaite,, et il faudra donc y revenir. Autant juste avoir une base en anglais, avec les trucs pénibles déjà traduits (cf. étape précédente), et la traduire plus manuellement avec <https://DeepL.com/>, question par question, en éditant ce qu'il faut. Si besoin, DeepL a une API en Python, gratuite à 500 000 mots par mois, avec [ce module `deepl`](https://pypi.org/project/deepl/).

----

## Tests à faire

- [ ] Prendre des anciens articles, et vérifier manuellement la qualité de la traduction proposée à la fin par mon outil ;
- [ ] Chaque semaine, m'occuper de mettre à jour le *Google doc* partagé (privé) que l'on utilise pour synchroniser nos traductions, en prémâchant le plus de boulot !

## Autres idées

- [ ] Raffiner l'entraînement d'un LLM spécialement conçu pour la traduction English US -> Français, sur le corpus de texte (de toutes les questions) de tous les articles Cranial déjà rédigés et traduits ;
- [ ] Écrire une extension Google Chrome qui fasse ce boulot ? Ce serait chiant car il faudrait l'écrire en JavaScript et j'y connais rien aux API permettant d'écrire des extensions de navigateur.
- [ ] Écrire un bot en PHP qui puisse s'ajouter au site Cranial-Insertion pour faire ce boulot ? Idem, j'ai jamais fait de PHP...

----

## :scroll: License ? [![GitHub license](https://img.shields.io/github/license/Naereen/Assistant-traduction-CranialInsertion-en-Python.svg)](https://github.com/Naereen/Assistant-traduction-CranialInsertion-en-Python/blob/master/LICENSE)
[MIT Licensed](https://lbesson.mit-license.org/) (file [LICENSE](LICENSE)).
© [Lilian Besson](https://GitHub.com/Naereen), 2024.

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/Assistant-traduction-CranialInsertion-en-Python/graphs/commit-activity)
[![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
[![ForTheBadge built-with-swag](http://ForTheBadge.com/images/badges/built-with-swag.svg)](https://GitHub.com/Naereen)
[![ForTheBadge uses-badges](http://ForTheBadge.com/images/badges/uses-badges.svg)](http://ForTheBadge.com)
[![ForTheBadge uses-git](http://ForTheBadge.com/images/badges/uses-git.svg)](https://GitHub.com/)
[![Awesome Badges](https://img.shields.io/badge/badges-awesome-green.svg)](https://github.com/Naereen/badges)
[![ForTheBadge 20-30](http://ForTheBadge.com/images/badges/ages-20-30.svg)](http://ForTheBadge.com)
