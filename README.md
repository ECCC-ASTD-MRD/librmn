rmnlib est une bibliothèque de fonctions pour la prévision numérique du temps
utilisée principalement par Environnement et Changement climatique Canada.

Ses principaux composants sont les fichiers Standard RPN et
l'interpolateur EZ.

# Documentation
  * [Référence des fonction accessible à partir d\u2019Internet (Anglais)](https://science:science@collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/)
  * [Documentation plus complète sur le Wiki du CMC](https://wiki.cmc.ec.gc.ca/wiki/Librmn)
  
# Installation
## Avant tout, assurez-vous de sourcez le bon [Compilateur](https://wiki.cmc.ec.gc.ca/wiki/RPN-SI/HPC_Upgrade_1#Platforms_And_Compiler)
## Dans l'immédiat, utilisez code_tools 3.2.0 sous sidr000 
```
. r.load.dot /fs/homeu1/eccc/mrd/ords/rpnsi/sidr000/ssm_test_dom/code-tools_3.2.0_all
```

## ou sous ECCC
```
export EC_CMAKE_MODULE_PATH="/users/dor/afsr/005/Projects/RPN/stage_2020/modules/;/users/dor/afsr/005/Projects/RPN/stage_2020/modules/compiler_rules;/users/dor/afsr/005/Projects/RPN/stage_2020/modules/compiler_rules/${BASE_ARCH}"
```

## Déplacez vous dans un répertoire de build
```
mkdir build
cd build
```
## Supprimez le build précédent si il y en a éja un 
```
\rm -r *
```
## Lancez cmake avec les options désirée, suivit de la commande make
```
cmake .. [-DWITH_OPENMP=[yes|no] -DSHARED=[yes|no] -DEC_COMPILER=[gnu|intel|pgi|...]]
make -j
make install
```
## Les installations se feront sous le répertoire libs
```
ls ../../libs
```
