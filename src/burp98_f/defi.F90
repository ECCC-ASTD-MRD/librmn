module rmn_burp_defi
    implicit none
    save

    real,    parameter :: RMANQUE   = 1.0E30    !< VALEUR REELLE POUR DONNEES MANQUANTES
    integer, parameter :: MAXNELE   = 4000      !< LONGUEUR MAXIMALE DE LA LISTE D'ELEMENTS
    integer, parameter :: NSTATMAX  = 12        !< LONGUEUR DU VECTEUR CONTENANT LES STATISTIQUES
    integer, parameter :: NPRIDEF   = 18        !< NOMBRE DE CLEFS PRIMAIRES
    integer, parameter :: NAUXDEF   = 5         !< NOMBRE DE CLEFS AUXILIAIRES
    integer, parameter :: NPRITOT   = 18        !< NOMBRE TOTAL DE CLEFS PRIMAIRES (PRIM+SUP)
    integer, parameter :: NAUXTOT   = 5         !< NOMBRE TOTAL DE CLEFS AUXILIAIRES
    integer, parameter :: NPRISUP   = 0         !< NOMBRE DE CLEFS PRIMAIRES SUPPLEMENTAIRES
    integer, parameter :: NAUXSUP   = 0         !< NOMBRE DE CLEFS AUXILIAIRES SUPPLEMENTAIRES
    integer, parameter :: NBENTR    = 320       !< NOMBRE DE BITS DANS ENTETE GENERALE DE RAPPORT
    integer, parameter :: NBENTB    = 128       !< NOMBRE DE BITS DANS ENTETE DE BLOC DE DONNEES
    integer, parameter :: MAXFIL    = 16
    integer, parameter :: GRONELE   = 128       !< DIMENSION MAX POUR NELE EN 7 BITS
    integer, parameter :: GROSDIM   = 256       !< DIMENSION MAX POUR NVALE ET NT EN 8 BITS 
    integer, parameter :: UNITES    = 64
    integer, parameter :: LDESCB    = 27
    integer, parameter :: PAGSIZ    = 255
    integer, parameter :: NCELLMAX  = 4
    integer, parameter :: LBKSTP    = 4         !< LONGUEUR DU SOUS-TYPE DE BTYP
    integer, parameter :: NPRMMAX   = 10
    integer, parameter :: TROIELM   = 3         !< nombre d'elements dans l'entete du bloc
    integer, parameter :: BP6B      = 8
    integer, parameter :: BP2B      = 14
!     LDESCB   = LONGUEUR TOTALE DES TROIS DESCRIPTEURS DE BLOC
#if defined (ALL64)
    integer, parameter :: DIMENT    = 2
    integer, parameter :: MAXREP    = 1024      !< NOMBRE DE MOTS POUR LE VECTEUR DE BITS DES REPETITIFS
#else
    integer, parameter :: DIMENT    = 4
    integer, parameter :: MAXREP    = 2048      !< NOMBRE DE MOTS POUR LE VECTEUR DE BITS DES REPETITIFS
#endif

end module rmn_burp_defi
