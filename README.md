# Assistant à la traduction de Cranial-Insertion (en Python)

## Objectifs

Aider à la traduction des articles sur <https://www.cranial-insertion.com/>, que l'on doit chaque weekend traduire de l'anglais américain vers le français.

## Choses à faire

Idéalement, je cherche à automatiser autant que possible, mais je vais commencer modestement :

- [ ] Automatiser tout ce qui serait possible d'être automatisés ;
- [ ] Première étape : je veux un script qui utilise mes cookies de connexion pour automatiquement aller chercher le texte (fichier `.md`) du dernier article en anglais (depuis <https://www.cranial-insertion.com/staff/articles>) et le sauvegarde dans `articles/english/NUMERO.md` (eg. `articles/english/4238.md`) ;
- [ ] Deuxième étape : je veux un script qui remplace les morceaux citant des cartes, par leur nom de cartes en français, avec [Scrython](https://github.com/NandaScott/Scrython) probablement, ou un accès web manuel à [Magic Ville](https://www.magic-ville.com/fr/) ou [Scryfall](https://scryfall.com/). Par exemple je veux remplacer :

```markdown
[q]If there's a [c]Collector Ouphe[/c] in play, can I still tap [c]Talisman of Creativity[/c] for mana?[/q]

==>

[q]If there's a [c]Collector Ouphe[/c] in play, can I still tap [c]Talisman of Creativity[/c] for mana?[/q]
```

- [ ] Troisième étape : je veux un script qui traduise tout seul le document pré-modifié

## Tests à faire

- [ ] Prendre des anciens articles, et vérifier manuellement la qualité de la traduction proposée à la fin par mon outil ;
- [ ] Chaque semaine, m'occuper de mettre à jour le *Google doc* partagé (privé) que l'on utilise pour synchroniser nos traductions ;

## Autres idées

- [ ] Raffiner l'entraînement d'un LLM spécialement conçu pour la traduction English US -> Français, sur le corpus de texte (de toutes les questions) de tous les articles Cranial déjà rédigés et traduits ;
- [ ] Écrire une extension Google Chrome qui fasse ce boulot ?
- [ ] Écrire un bot en PHP qui puisse s'ajouter au site Cranial-Insertion pour faire ce boulot ?

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
