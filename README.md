rmnlib est une bibliothèque de fonctions pour la prévision numérique du temps
utilisée principalement par Environnement et Changement climatique Canada.

Ses principaux composants sont les fichiers Standard RPN et
l'interpolateur EZ.

# Documentation
  * [Référence des fonction accessible à partir d\u2019Internet (Anglais)](https://science:science@collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/)
  * [Documentation plus complète sur le Wiki du CMC](https://wiki.cmc.ec.gc.ca/wiki/Librmn)

# Instruction d'installation

## Créer un répertoire pour la compilation
```
mkdir build
cd build
```

## Configuration de la compilation

Les options pour configurer la compilation doivent être ajoutées lors de
l'appel de la commande `cmake` avec le préfix `-D`.

CMAKE_INSTALL_PREFIX
: Chemin d'accès du répertoire pour l'installation (make install)
BUILD_SHARED_LIBS
: `(yes|no)` Indique si les bibliothèques de fonctions dynamiques doivent être produites.
COMPILER
: `(GNU|Intel|XL|...)` Suite de compilateurs à utiliser.  Défaut: `GNU`


Par exenple:
```
cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local -DBUILD_SHARED_LIBS=yes
```

## Lancez cmake avec les options désirée, suivit de la commande make
```
cmake .. <options>
make -j
make install
```
