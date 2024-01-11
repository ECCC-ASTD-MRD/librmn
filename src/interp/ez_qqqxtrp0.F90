module rmn_ez_qqqxtrp0
    implicit none
    save

    integer, parameter :: voisin  =   0
    integer, parameter :: lineair =   1
    integer, parameter :: cubique =   3
    
    integer, parameter :: oui     =   1
    integer, parameter :: minimum =   4
    integer, parameter :: maximum =   5
    integer, parameter :: valeur  =   6
    integer, parameter :: abort   =  13
    
    logical :: flgxtrap = .false.
    logical :: outlmit
    integer :: codxtrap = oui
    integer :: ordint = 3
    real    :: valxtrap
    
end module rmn_ez_qqqxtrp0
