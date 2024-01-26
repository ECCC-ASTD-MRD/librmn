module rmn_burp_defi
    implicit none
    save

    real,    parameter :: rmanque  = 1.0E30    !< Valeur reelle pour donnees manquantes
    integer, parameter :: maxnele  = 4000      !< Longueur maximale de la liste d'elements
    integer, parameter :: nstatmax = 12        !< Longueur du vecteur contenant les statistiques
    integer, parameter :: npridef  = 18        !< Nombre de clefs primaires
    integer, parameter :: nauxdef  = 5         !< Nombre de clefs auxiliaires
    integer, parameter :: npritot  = 18        !< Nombre total de clefs primaires (prim+sup)
    integer, parameter :: nauxtot  = 5         !< Nombre total de clefs auxiliaires
    integer, parameter :: nprisup  = 0         !< Nombre de clefs primaires supplementaires
    integer, parameter :: nauxsup  = 0         !< Nombre de clefs auxiliaires supplementaires
    integer, parameter :: nbentr   = 320       !< Nombre de bits dans entete generale de rapport
    integer, parameter :: nbentb   = 128       !< Nombre de bits dans entete de bloc de donnees
    integer, parameter :: maxfil   = 16
    integer, parameter :: gronele  = 128       !< Dimension max pour nele en 7 bits
    integer, parameter :: grosdim  = 256       !< Dimension max pour nvale et nt en 8 bits 
    integer, parameter :: unites   = 64
    integer, parameter :: ldescb   = 27        !< Longueur totale des trois descripteurs de bloc
    integer, parameter :: pagsiz   = 255
    integer, parameter :: ncellmax = 4
    integer, parameter :: lbkstp   = 4         !< Longueur du sous-type de btyp
    integer, parameter :: nprmmax  = 10
    integer, parameter :: troielm  = 3         !< Nombre d'elements dans l'entete du bloc
    integer, parameter :: bp6b     = 8
    integer, parameter :: bp2b     = 14
#if defined (ALL64)
    integer, parameter :: diment   = 2
    integer, parameter :: maxrep   = 1024      !< Nombre de mots pour le vecteur de bits des repetitifs
#else
    integer, parameter :: diment   = 4
    integer, parameter :: maxrep   = 2048      !< Nombre de mots pour le vecteur de bits des repetitifs
#endif

end module rmn_burp_defi
