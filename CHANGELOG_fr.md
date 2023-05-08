# Ajouts

- IgnoreTypeKindRank.hf permet d'indiquer aux compilateurs Fortran de ne pas se
soucier du type, kind et rank. C'est utilisé lorsqu'il faut appeler des
fonctions C qui s'attendent à un `void *`.

- Les outils propres au CMC ne sont plus nécessaires pour la compilation et le
processus est grandement simplifié grâce à l'utilisation de
[CMake](https://cmake.org/documentation/)

- Ajout d'une interface explicite pour fnom et fclos.  Elle peut être
utilisée avec `#include <fnom.hf>`.

- Ajout d'une interface explicite pour les fonctions de MGI.  Il suffit
d'ajouter la ligne `#include <mgi.hf>` pour l'utiliser.

- Ajout d'un nouveau type de coordonnée vertical pour IP1 (KIND=7) pour la profondeur
en metres sous le niveau de la mer

- Le contrôle du niveau de verbosité se fait maintenant a travers le package [App](https://gitlab.science.gc.ca/RPN-SI/App) et 
on peut controler le niveau pour les différentes composantes avec les méthodes suivantes:

  - C & Fortran: 
     - previous=Lib_LogLevel(APP_LIB`[lib]`,"`[niveau]`")  
     - previous=Lib_LogLevelNo(APP_LIB`[lib]`,`[niveau]`)
  
  - Environment: 
     - export APP_VERBOSE_LIB`[lib]`=`[niveau]`
    
  - Où: 
     - `[lib]` = RMN,FST,WB,GMM,VGRID ou INTERPV
     - `[niveau]` = ERROR,WARNING,INFO,TRIVIAL,DEBUG,EXTRA ou QUIET

# Bris de compatibilité

La version 20 est une version majeure et elle a fait l'objet d'un
nettoyage considérable du code. Des fonctions et des constructions
désuètes ont été retirées.

- La casse des noms de fichiers ne sera plus modifiée si le nom de fichier
en entrée contient uniquement des majuscules.

- Les types de données suivants ne sont plus disponibles. Il faut utiliser les
types correspondants accessibles via `#include <stdint.h>`

| Ancien   | Nouveau  |
| ---------| -------- |
| INT_32   |  int32_t |
| INT_64   |  int64_t |
| word     | uint32_t |
| wordint  |  int32_t |
| ftnfloat |    float |

- La macro `Amd64` a été supprimée.  Lorsque nécessaire, le code devrait
utiliser des types dont la taille est explicite tels que `int32_t` ou
`int64_t`.

- La macro `string_copy` a été supprimée.  La fonction C standard
`strncpy` devrait être utilisée à la place.

- La fonction Fortran `longueur(str)` n'est plus disponible. En Fortran,
il faut utiliser `len_trim(str)` à la place. Pour le C, vous pouvez
utiliser la fonction `ftnStrLen(str, maxLen)`

- La macro `tell` a été supprimée.  Veuillez utiliser la fonction POSIX
équivalente: `ftell`.

- La macro `CLIB_SUFFIX` a été supprimée.

- arch_specific.hf a été supprimé c'est maintenant vide

- Le support pour WIN32 a été retiré.  Le code n'avait pas été testé sur
cette plateforme depuis plusieurs années et était probablement
non-fonctionnel.

- La fonction `IOPDATM` a été supprimée.  Ça ne fonctionnait plus depuis
le passage à U2.  Les opération vont fournir un autre moyen pour
récupérer les dates des passes.

- La variable d'environement `AFSISIO` n'existe plus et a été remplacée par
`CMCCONST`. Cette variable est utilisée pour accéder aux constantes du CMC
et aux tables BURF

- Les fonctions `second()`, `_second()` et `__second()` permettant
d'interroger le temps CPU utilisé ont été supprimmées.  Des outils
externes permettent d'obtenir cette information ou de faire du
profilage.

- Tous les fichiers d'en-tête sauf le fichier principal, `rmn.h`, ont
été déplacés dans le sous répertoire `rmn`.  Ainsi, il sera nécessaire
de corriger le chemin d'accès dans les directives `#include`.  Par
exemple, `#include <rpnmacros.h>` devient `#include <rmn/rpnmacros.h>`.

- L'exécutable librmn_version n'est plus disponible. L'information de 
version, architecture et bien plus peuvent être récupéré avec le script 
`rmn-config`. ex: `rmn-config --version --arch`

- GMM est maintenant disponible sous forme de module (`rmn_gmm`) et non
plus sous forme de multiples fichiers *.hf

- Toutes les fonctions de dmms sont obsolètes et ne devraient plus être
utilisées.
    - memoirc
    - dmmsdbg
    - dmmsnabt
    - hpalloc
    - hpdeallc
    - ca_alloc
    - ca_deallc
    - memoirh
    - memoir

- Les composants suivants ont été retirés de la bibliothèque de fonctions,
car ils ne devraient déjà plus être utilisés:
    - VMM
    - BMF
        - bmf_catalog
        - bmf_char2i
        - bmf_i2char
        - bmf_clear
        - bmf_connect
        - bmf_error
        - bmf_get
        - bmf_get2
        - bmf_geth
        - bmf_gobe
        - bmf_init
        - bmf_list
        - bmf_perturb
        - bmf_splitend
        - bmf_splithalo
        - bmf_splithole
        - bmf_splitinit
        - bmf_splitname
        - bmf_splitoptions
        - bmf_splitstart
        - bmf_splitwrall
        - bmf_splitwrite
        - bmf_splitwrite2
        - bmf_splitwriteh
        - bmf_splitwriteh2
        - bmf_splitwritex
        - bmf_splitwritex2
        - bmf_splitwritexy
        - bmf_splitwritexy2
        - bmf_splitwritey
        - bmf_splitwritey2
        - bmf_statfld
        - bmf_write
        - bmf_write2
    - lamineur
        - slabopt
        - slabini
        - slabig34
        - slabdsc
        - slabxtr
        - slabxtrf
        - slabxtr
        - slabend
    - Spectral
        - ALPAS2
        - ALPAS8
        - ALPDL2
        - ALPDL8
        - ALPDR2
        - ALPDR8
        - ALPNM2
        - ALPNM8
        - DIMCAL
        - EPSIL2
        - EPSIL8
        - EWFDR2
        - FASP2
        - FASP8
        - GGASP0
        - GGASP8
        - GWAQD0
        - GWAQD8
        - PERM
        - SCOF2
        - SPAF2
        - SPAF8
        - SPAGG0
        - SPAGG8
    - twinbuffer
        - open_db_file
        - close_db_file
        - read_db_file
        - write_db_file
        - rewind_db_file
    - Base
        - AFIX
        - AFIX8
        - AMAX
        - AMEAN
        - AMIN
        - GDADCN
        - GDADGD
        - GDMPGD
        - GDSQRT
        - SLL
        - VLL
        - VPOLAGR
        - VPS
        - grid_to_grid
        - grid_to_grid_interp
        - grid_to_grid_coef
        - DEFVEC
        - FCONW
        - mxma
    - primitives
        - unstakw
        - stkmemw


# Trouver des appels de fonctions obsolètes dans votre code

Voici une commande qui peut aider à identifier si votre code contient
des appels aux fonctions et sous-routines qui ont été identifiées pour
être retirées de la librmn:

```
for fnctName in AFIX AFIX8 ALPAS2 ALPAS8 ALPDL2 ALPDL8 ALPDR2 ALPDR8 \
    ALPNM2 ALPNM8 AMAX AMEAN AMIN DEFVEC \
    DIMCAL EPSIL2 EPSIL8 EWFDR2 FASP2 FASP8 FCONW GDADCN \
    GDADGD GDMPGD GDSQRT GGASP0 GGASP8 GWAQD0 GWAQD8 IOPDATM PERM \
    SCOF2 SLL SPAF2 SPAF8 SPAGG0 SPAGG8 VLL \
    VPOLAGR VPS bmf_catalog bmf_char2i bmf_clear bmf_connect bmf_error \
    bmf_get bmf_get2 bmf_geth bmf_gobe bmf_i2char bmf_init bmf_list \
    bmf_perturb bmf_splitend bmf_splithalo bmf_splithole bmf_splitinit \
    bmf_splitname bmf_splitoptions bmf_splitstart bmf_splitwrall \
    bmf_splitwrite bmf_splitwrite2 bmf_splitwriteh bmf_splitwriteh2 \
    bmf_splitwritex bmf_splitwritex2 bmf_splitwritexy \
    bmf_splitwritexy2 bmf_splitwritey bmf_splitwritey2 bmf_statfld \
    bmf_write bmf_write2 ca_alloc ca_deallc close_db_file dmmsdbg \
    dmmsnabt grid_to_grid grid_to_grid_coef grid_to_grid_interp \
    hpalloc hpdeallc memoir memoirc memoirh mxma open_db_file \
    read_db_file rewind_db_file slabdsc slabend slabig34 slabini \
    slabopt slabxtr slabxtr slabxtrf stkmemw unstakw write_db_file; do
    grepOut=$(grep --color=always -RinP "(?>\W)${fnctName}(?>\W)" | grep -v tests)
    if [[ -n "${grepOut}" ]]; then
        echo ${fnctName};
        echo "${grepOut}";
        echo "";
    fi
done
```
