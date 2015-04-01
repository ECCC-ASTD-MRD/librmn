*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
***FONCTION EXDB    IMPRESSION DE BOITE DE DEBUT D'EXECUTION
*
*
      INTEGER FUNCTION EXDB(in_TITRE,REVIS,FLAG)
      IMPLICIT NONE
      CHARACTER*(*) in_TITRE,REVIS,FLAG
*
*AUTEUR  M.VALIN RPN MARS 1983
*Revision 002 M. Lepine Octobre 1998 - Ajout de la signature RMNLIB
*
*LANGAGE RATFOR
*        IL Y A DES DEPENDANCES CDC NOS/SCOPE 2
*
*OBJET(EXDB)
*        IMPRIMER UNE BOITE INDIQUANT LE DEBUT DE L'EXECUTION
*        D'UN PROGRAMME FORTRAN. EXDB RENVOIE EGALEMENT LE
*        DATE TIME STAMP (NOMBRE ENTIER DE 10 CHIFFRES) DE
*        LA PASSE OPERATIONNELLE COURANTE.
*
*        EXFIN IMPRIME UNE BOITE INDIQUANT LA FIN DE L'EXECUTION
*        D'UN PROGRAMME FORTRAN. LA VALEUR RENVOYEE EST ZERO.
*
*CALL STDLIB
*
*ARGUMENTS
*  E     TITRE     CHAINE DE CARACTERES. SEULS LES 7 PREMIERS
*                  SERONT IMPRIMES.
*  E     REVIS     CHAINE DE CARACTERES INDIQUANT LA VERSION DU
*                  PRODUIT (EXDB) OU UN MESSAGE DE FIN (EXFIN). SEULS LES
*                  DIX PREMIERS CARACTERES SERONT IMPRIMES.
*  E     FLAG      CHAINE DE CARACTERES (7), FLAG PEUT ETRE
*                  - 'NON' AUQUEL CAS IOPDATM RENVOIE 2010101011.
*                  - UNE CLE DE 7 CARACTERES OU MOINS CORRESPONDANT
*                    A D'AUTRES "DATE TIME STAMP".
*                  - UN NOMBRE DE 7 CHIFFRES YYJJJZZ OU YY EST L'ANNEE,
*                    JJJ LE JOUR DANS L'ANNEE (1 A 366) ET ZZ L'HEURE
*                    (00 - 24).
*                  - IOPDATM RENVOIE ALORS LE DATE TIME STAMP QUI
*                    CORRESPOND A CETTE DATE ET CETTE HEURE.
*                  - UN NOM DE FICHIER. IOPDATM LIRA LE PREMIER MOT DE CE
*                    FICHIER EN LE CONSIDERANT COMME UN DATE TIME STAMP.
*                    SI IOPDATM RENCONTRE UNE ERREUR, LE "STAMP" RENVOYE
*                    SERA 1010101011.
*
*MESSAGES
*        EXDB ET EXFIN ECRIVENT SUR L'UNITE FORTRAN 6. L'USAGER
*        EST RESPONSABLE D'OUVRIR  CETTE UNITE CORRECTEMENT.
*
*MODULES
*        IOPDATM   POUR OBTENIR LE DATE TIME STAMP  (RMNLIB5)
*        DATE      POUR OBTENIR LA DATE DU SYSTEME  (RMNLIB5)
*        DATMGP2   POUR RECONSTITUER LE DATE TIME STAMP  (RMNLIB5)
*        TIME      POUR OBTENIR L'HEURE  (LIBRAIRIE FORTRAN)
*        SECOND    POUR OBTENIR LE TEMPS CPU  (LIBRAIRIE FORTRAN)
*
*NOTES
*        EXDB VA CHERCHER DANS RA+56 A RA+63 L'IMAGE DE LA CARTE
*        DE CONTROLE QUI A SERVI A APPELER LE PROGRAMME PRINCIPAL.
*        CECI EST UNE DEPENDANCE CDC  (NOS OU SCOPE 2)
*        EXDB SE SERT DU FICHIER FORTRAN 88, L'OUVRE ET LE
*        RETOURNE.
*
**
*


      CHARACTER *24 CDATIM
      CHARACTER *80 VERSION,titre
      INTEGER EXFIN,IOPDATM,I,IDATIM(14)
      REAL T1,SECOND
      EXTERNAL FDATE
      external memoirc, flush_stdout
      SAVE T1

      titre = ' '
      titre(1:min(len(in_titre),80)) = in_titre(1:min(len(in_titre),80))
      IDATIM(14) = IOPDATM(FLAG)
      CALL DATMGP2(IDATIM)
      CALL FDATE(CDATIM)
* Obtenir la version de rmnlib utilisee
      CALL RMNLIB_version(VERSION,.false.)
      WRITE(6,500) TITRE,REVIS,VERSION,CDATIM
      IF (FLAG.NE.'NON') THEN
         write(6,450) flag
         WRITE(6,700) (IDATIM(I),I=7,14)
      ENDIF
      WRITE(6,800) 'BEGIN  EXECUTION     '
 450  format(3X,'*',90X,'*',/3x,'*',8x,a8,t95,'*')
 500  FORMAT(1H1,
     %     /,3X,'*',90('*'),'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A57,3X,A10,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',8X,A80,2X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A24,46X,10X,'*')
 600  FORMAT(1H1,
     %     /,3X,'*',90('*'),'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A7,53X,A10,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A10,50X,A10,10X,'*')
 700  FORMAT(3X,'*',90X,'*',
     %     /,3X,'*',9X,7A4,I12,41X,'*')
 800  FORMAT(3X,'*',90X,'*',
     %     /,3X,'*',10X,A20,60X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',90('*'),'*')

      T1 = SECOND( )
      EXDB = IDATIM(14)


      RETURN
* ENTREE EXFIN   BOITE DE FIN D'EXECUTION
      ENTRY EXFIN(in_TITRE,REVIS,FLAG)

      call flush_stdout()
      titre = ' '
      titre(1:min(len(in_titre),80)) = in_titre(1:min(len(in_titre),80))
      CALL FDATE(CDATIM)
      WRITE(6,501) TITRE,REVIS,CDATIM,'END EXECUTION       ',
     %     SECOND( )-T1
 501  FORMAT(
     %     /,3X,'*',90('*'),'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A57,3X,A10,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A24,46X,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A20,60X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,'CP SECS = ',F10.3,60X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',90('*'),'*')
 601  FORMAT(
     %     /,3X,'*',90('*'),'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A7,53X,A10,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A10,50X,A10,10X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,A20,60X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',10X,'CP SECS = ',F10.3,60X,'*',
     %     /,3X,'*',90X,'*',
     %     /,3X,'*',90('*'),'*')

      call memoirc(0)

      EXFIN = 0
      

      RETURN
      END
