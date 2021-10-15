rmnlib est une bibliothèque de fonctions pour la prévision numérique du temps
utilisée principalement par Environnement et Changement climatique Canada.

Ses principaux composants sont les fichiers Standard RPN et
l'interpolateur EZ.


# Documentation
  * [Référence des fonctions accessible à partir d'Internet (Anglais)](https://science:science@collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/)
  * [Documentation plus complète sur le Wiki du CMC](https://wiki.cmc.ec.gc.ca/wiki/Librmn)


# Instructions d'installation

## Créer un répertoire pour la compilation
```
mkdir $build_dir_path
cd $build_dir_path
```

## Configuration de la compilation

Les options pour configurer la compilation doivent être ajoutées lors de
l'appel de la commande `cmake` avec le préfix `-D`.

CMAKE_BUILD_TYPE
: `(Release|RelWithDebInfo|Debug)` Mode de compilation.  Défaut: `RelWithDebInfo`

CMAKE_INSTALL_PREFIX
: Chemin d'accès du répertoire pour l'installation (`make install`)

BUILD_SHARED_LIBS
: `(yes|no)` Indique si les bibliothèques de fonctions dynamiques doivent être produites.  Défaut: `no`

COMPILER_SUITE
: `(GNU|Intel|XL|...)` Suite de compilateurs à utiliser.  Défaut: `GNU`

WITH_OPENMP
: `(yes|no)` Indique si le support pour OpenMP doit être activée.  Défaut: `yes`


## Exemple de compilation
```
cmake \
    -DCMAKE_C_STANDARD=99 \
    -DCMAKE_C_EXTENSIONS=OFF \
    -DCMAKE_INSTALL_PREFIX=$install_dir_path \
    -DBUILD_SHARED_LIBS=no \
    -DWITH_DOC=no \
    -DWITH_OPENMP=no \
    $src_dir_path
make -j $a_resonable_number
make install
```

## Compilation dans l'environnement d'ECCC

Les scripts CMake de __cmake_rpn__ vont automatiquement détecter le compilateur
chargé via la variable d'environement __EC_ARCH__.  Il n'est donc pas nécessaire
de spécifier explicitement la suite de compilateurs à utiliser
(`-DCOMPILER_SUITE=...`).  Vous devez toutefois charger le compilateur désiré
avant d'effectuer la configuration de la compilation.

Puisque la version par défaut de CMake disponible sur les systèmes Ubuntu 18.04
est trop vieille, vous devez charger une version plus récente.  Par exemple:
`. ssmuse-sh -d /fs/ssm/main/opt/cmake/cmake-3.16.4/`


# Exemple de compilation de la branche dev sur un système non-ECCC
````
git clone --recurse-submodules -b dev https://github.com/ECCC-ASTD-MRD/librmn.git 
mkdir librmn_build
cd librmn_build
cmake ../librmn -DBUILD_SHARED_LIBS=FALSE -DWITH_DOC=NO -DCMAKE_INSTALL_PREFIX=~/opt/
make -j4 install
```


## Exemple d'utilisation dans une application cliente

Nous avons développé un exemple très simple d'application qui utilise librmn.
Il peut être utilisé comme référence pour construire un projet CMake qui utilise
la librmn.

Il peut être consulté aux adresses suivantes:
- [https://gitlab.science.gc.ca/RPN-SI/librmn-client-example](https://gitlab.science.gc.ca/RPN-SI/librmn-client-example) (Sur le réseau d'ECCC)
- [https://github.com/ECCC-ASTD-MRD/librmn-client-example](https://github.com/ECCC-ASTD-MRD/librmn-client-example) (Public)