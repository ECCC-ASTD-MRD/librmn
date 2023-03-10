rmnlib est une bibliothèque de fonctions pour la prévision numérique du temps
utilisée principalement par Environnement et Changement climatique Canada.

Ses principaux composants sont les fichiers Standard RPN et
l'interpolateur EZ.


## Documentation
  * [Référence des fonctions accessible à partir d'Internet (Anglais)](https://science:science@collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/)
  * [Documentation plus complète sur le Wiki du CMC](https://wiki.cmc.ec.gc.ca/wiki/Librmn)


## Obtenir le code

Le projet est hébergé à l'adresse https://gitlab.science.gc.ca/RPN-SI/librmn
Le code est disponible via Git aux adresse suivates:
  * Sur le réseau ECCC: git@gitlab.science.gc.ca:RPN-SI/librmn.git
  * Pour les utilisateurs à l'extérieur d'ECCC: https://github.com/ECCC-ASTD-MRD/librmn.git

`cmake_rpn` est inclus via des sous-modules de Git, alors veuillez exécuter la commande suivante après avoir cloné le dépôt:
`git submodule update --init --recursive`
pour récupérer les sous-modules.


## Instructions d'installation

### Créer un répertoire pour la compilation
```
mkdir $build_dir_path
cd $build_dir_path
```

### Configuration de la compilation

Les options pour configurer la compilation doivent être ajoutées lors de
l'appel de la commande `cmake` avec le préfix `-D`.

CMAKE_BUILD_TYPE
: `(Release|RelWithDebInfo|Debug)` Mode de compilation.  Défaut: `RelWithDebInfo`

CMAKE_INSTALL_PREFIX
: Chemin d'accès du répertoire pour l'installation (`make install`)

COMPILER_SUITE
: `(GNU|Intel|XL|...)` Suite de compilateurs à utiliser.  Sur les systèmes d'ECCC,
le compilateur chargé sera utilisé.  Si les vairables d'environnement propres à
ECCC ne sont pas trouvées, la valeur par défaut est `GNU`.

WITH_OPENMP
: `(yes|no)` Indique si le support pour OpenMP doit être activée.  Défaut: `yes`

### Exemple de compilation
```
cmake \
    -DCMAKE_INSTALL_PREFIX=$install_dir_path \
    -DWITH_OPENMP=no \
    $src_dir_path
make -j $a_resonable_number
make install
```

### Compilation dans l'environnement d'ECCC

Les scripts CMake de __cmake_rpn__ vont automatiquement détecter le compilateur
chargé via la variable d'environement __EC_ARCH__.  Il n'est donc pas nécessaire
de spécifier explicitement la suite de compilateurs à utiliser
(`-DCOMPILER_SUITE=...`).  Vous devez toutefois charger le compilateur désiré
avant d'effectuer la configuration de la compilation.

Puisque la version par défaut de CMake disponible sur les systèmes Ubuntu 18.04
est trop vieille, vous devez charger une version plus récente.  Par exemple:
`. ssmuse-sh -d /fs/ssm/main/opt/cmake/cmake-3.16.4/`


### Exemple de compilation de la branche dev sur un système non-ECCC
```
git clone -b dev https://github.com/ECCC-ASTD-MRD/librmn.git 
git submodule update --init --recursive
mkdir librmn_build
cd librmn_build
cmake ../librmn -DCMAKE_INSTALL_PREFIX=~/opt/
make -j4 install
```

### Documentation
une cible `doc` est crée par cmake pour généré la documentation. Ceci nécessite cependant
Doxygen et graphviz.
```
make doc
```

### Changement de compilateur

Étant donné que la bibliothèque de fonctions contient un module Fortran, il
faut utiliser le même compilateur qui a produit la biliothèque pour les
applications clientes.

Ainsi, il faut nettoyer le répertoire de compilation et ré-exécuter les
instructions de compilation en spécifiant un répertoire d'installation
(`-DCMAKE_INSTALL_PREFIX=$install_dir_path`) différent.


## Exemple d'utilisation dans une application cliente

Nous avons développé un exemple très simple d'application qui utilise librmn.
Il peut être utilisé comme référence pour construire un projet CMake qui utilise
la librmn.

Il peut être consulté aux adresses suivantes:
- [https://gitlab.science.gc.ca/RPN-SI/librmn-client-example](https://gitlab.science.gc.ca/RPN-SI/librmn-client-example) (Sur le réseau d'ECCC)
- [https://github.com/ECCC-ASTD-MRD/librmn-client-example](https://github.com/ECCC-ASTD-MRD/librmn-client-example) (Public)
