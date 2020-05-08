***S/P  QQQOENV - OBTENIR UN NOM DE CLEF OU UNE VALEUR D'UN ARGUMENT
*                 A PARTIR D'UNE VARIABLE D'ENVIRONNEMENT CONTENANT LA
*                 SEQUENCE D'APPEL COMPLETE
*
        INTEGER FUNCTION QQQOENV(ARGP,CCARD_ARGS,L)
	IMPLICIT NONE
        CHARACTER * 256 ARGP, CCARD_ARGS
	INTEGER L
*
*AUTEUR          M. Lepine,  Fevrier 2003
*
*OBJET(QQQOBM)
*       FONCTION QUI PERMET D'ALLER CHERCHER LES ARGUMENTS D'UNE SEQUENCE
*       D'APPEL A UN PROGRAMME ET D'EN EXTRAIRE LES NOMS DE CLEFS ET LES
*       VALEURS A DONNER A CES CLEFS.
*       LA FONCTION QQQOBM RETOURNE:
*            - UNE VALEUR DE 1 SI ARG CONTIENT UN NOM DE CLEF
*            - UNE VALEUR DE 2 SI ARG CONTIENT UNE VALEUR A DONNER A UNE CLEF
*            - UNE VALEUR DE 3 SI ARG CONTIENT UN ARGUMENT POSITIONEL
*            - UNE VALEUR DE 5 SI ON DEMANDE LA SEQUENCE D'APPEL
*            - UNE VALEUR DE 0 LORSQUE TOUT EST FINI
*
*ARGUMENT:
*         ARGP    SORTIE     NOM DE CLEF OU VALEUR RETOURNE
*
**
	Integer pos, i, ic, indfin
	Logical debut, pudcle
	character c, quotechar
        character * 256 arg
        integer  longueur
        external longueur
	SAVE	pos,debut
	DATA	debut, pudcle /.true., .false./

	if (debut) then
	  pos = 1
	  debut = .false.
	endif

 100    continue
	i = 1
	arg = ' '
        argp = ' '

        if (pos > L) then
           qqqoenv = 0        ! termine, fin de la sequence d'appel
           return
        endif

	c = ccard_args(pos:pos)
        getarg: DO                            ! obtenir la prochaine cle, valeur
	  if ((pos > L) .or. (c .eq. ' ') .or. (c .eq. ':')) EXIT getarg
	  if ((c .ne. '''') .and. (c .ne. '"')) then
	    arg(i:i) = c
	    pos = pos +1
	    i = i + 1
            if (pos <= L) c = ccard_args(pos:pos)
	  else
            quotechar = c
	    pos = pos + 1
	    c = ccard_args(pos:pos)
	    quote: DO
       		if (c .eq. quotechar) EXIT quote
	  	if (pos > L) then
	          print *,'CCARD, qqqoenv error: unmatched quote'
		  EXIT quote
		endif
		arg(i:i) = c
	 	i = i + 1
		pos = pos +1	
	        if (pos <= L) c = ccard_args(pos:pos)
	    END DO quote
            pos = pos +1
            if (pos <= L) c = ccard_args(pos:pos)          
	  endif
	END DO getarg

        indfin = longueur(arg)

        if ((arg(1:1) .eq. '-') .and. (.not. pudcle)) then
           qqqoenv = 1           ! nom de cle
           ic = 2                ! position de copie apres '-'
           if (arg(2:2) .eq. '-') then     ! -- passage en mode positionel
              pudcle = .true. 
              goto 100                     ! prochain argument positionel 
           endif
        else
           if (pudcle) then                ! argument positionel
              qqqoenv = 3
              ic = 1
           else
              qqqoenv = 2                  ! valeur
              ic = 1
           endif
           if (arg(1:1) .eq. '=') ic = 2   ! passer le caractere d'escape
        endif
        argp = arg(ic:indfin)
	if ((qqqoenv == 1) .and. (indfin == 2) .and.
     % ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-H'))) then
	  qqqoenv = 5                      ! sequence d'appel demande
        endif
        pos = pos + 1          ! positionnement au debut du prochain argument

	return
	end
