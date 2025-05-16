module rmn_burp
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

    !> Masque pour bit 28 de btyp signe de bkstp
    integer, parameter :: sgbkstp = 268435456
    !> Masque de 7 bits pour le type de btyp
    integer, parameter :: bktypmsk = 127
    !> Masque pour la famille du bloc
    integer, parameter :: fammsk = 63
    !> Masque pour bit 29 de btyp signe de bktyp
    integer, parameter :: sgbktyp = 536870912
    !> Masque pour le sous-type de btyp
    integer, parameter :: bkstpmsk = 15
    !> Masque pour le descripteur du bloc
    integer, parameter :: desmsk = 63
    !> Masque pour bit 30 de btyp signe de bknat
    integer, parameter :: sgbknat = 1073741824
    !> Masque de deux bits
    integer, parameter :: msk2b = 3
    !> Masque de six bits
    integer, parameter :: msk6b = 63
    !> Masque pour le type du bloc
    integer, parameter :: typmsk = 32767
    !> Masque de huit bits
    integer, parameter :: msk8b = 255
    !> Masque de 4 bits pour la nature de btyp
    integer, parameter :: bknatmsk = 15
    !> Masque pour famdesc
    integer, parameter :: fmdmsk = 4095

    !> Position du type de btyp
    integer, parameter :: bpbktyp = 4
    !> Position de nature de btyp
    integer, parameter :: bpbknat = 11
    !> Longueur du descripteur de bloc fusionne bfam+bdesc
    integer, parameter :: lfmdsc = 12
    !> Premiere dim du bloc si tropgros
    integer, parameter :: bpnele16 = 95
    integer, parameter :: bpbit0 = 63
    !> Position du descripteur de type du bloc
    integer, parameter :: bptyp = 26
    integer, parameter :: bpnbit = 31
    integer, parameter :: bpdatyp = 43
    !> Position du descripteur de bloc fusionne bfam+bdesc
    integer, parameter :: bpfmdsc = 11
    integer, parameter :: bpnele = 71
    integer, parameter :: bpnt = 39
    !> Troisieme dim du bloc si tropgros
    integer, parameter :: bpnt16 = 127
    integer, parameter :: bpnval = 79
    !> Deuxieme dim du bloc si tropgros
    integer, parameter :: bpnval16 = 111
    integer, parameter :: lbit0 = 20
    integer, parameter :: ldatyp = 4
    integer, parameter :: lnbit = 5
    integer, parameter :: lnele = 8
    integer, parameter :: lnele16 = 16
    integer, parameter :: lnt = 8
    integer, parameter :: lnt16 = 16
    integer, parameter :: lnval = 8
    integer, parameter :: lnval16 = 16
    !> Longueur du descripteur de type du bloc
    integer, parameter :: ltyp = 15

    integer, parameter :: ldate = 20
    integer, parameter :: ldrcv = 11
    integer, parameter :: ldx = 12
    integer, parameter :: ldy = 12
    integer, parameter :: lelev = 13
    integer, parameter :: lflgs = 24
    integer, parameter :: lheur = 6
    integer, parameter :: lidtp = 8
    integer, parameter :: llati = 16
    integer, parameter :: llong = 16
    integer, parameter :: lmin = 6
    integer, parameter :: lnblk = 16
    integer, parameter :: loars = 16
    integer, parameter :: lrunn = 8
    integer, parameter :: lsti1 = 8
    integer, parameter :: lsti2 = 8
    integer, parameter :: lsti3 = 8
    integer, parameter :: lsti4 = 8
    integer, parameter :: lsti5 = 8
    integer, parameter :: lsti6 = 8
    integer, parameter :: lsti7 = 8
    integer, parameter :: lsti8 = 8
    integer, parameter :: lsti9 = 8

    integer, parameter :: bpdate = 147
    integer, parameter :: bpdrcv = 55
    integer, parameter :: bpdx = 159
    integer, parameter :: bpdy = 179
    integer, parameter :: bpelev = 44
    integer, parameter :: bpflgs = 95
    integer, parameter :: bpheur = 185
    integer, parameter :: bpidtp = 167
    integer, parameter :: bplati = 111
    integer, parameter :: bplong = 127
    integer, parameter :: bpmin = 191
    integer, parameter :: bpnblk = 15
    integer, parameter :: bpoars = 31
    integer, parameter :: bprunn = 63
    integer, parameter :: bpsti1 = 7
    integer, parameter :: bpsti2 = 15
    integer, parameter :: bpsti3 = 23
    integer, parameter :: bpsti4 = 31
    integer, parameter :: bpsti5 = 39
    integer, parameter :: bpsti6 = 47
    integer, parameter :: bpsti7 = 55
    integer, parameter :: bpsti8 = 63
    integer, parameter :: bpsti9 = 71

#include <rmn/codes.cdk>

    logical :: enforc8

    !> valeur a donner aux valeurs reelles manquantes
    real :: manque = rmanque

    !> vecteur de bits ou un bit allume indique que l'element pourtant le numero du bit est repetitif
    integer, dimension(maxrep) :: rpetitif = 0

    !> prend la valeur 1 lorsqu'on utilise une autre table qe la tableburp officielle
    integer :: badtbl = 0
end module rmn_burp
