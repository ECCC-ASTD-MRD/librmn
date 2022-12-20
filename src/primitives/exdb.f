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
****FONCTION EXDB    IMPRESSION DE BOITE DE DEBUT D'EXECUTION
*
*
      INTEGER FUNCTION EXDB(in_TITRE,REVIS,FLAG)
      IMPLICIT NONE
      CHARACTER*(*) in_TITRE,REVIS,FLAG
*
      INTEGER EXDBPLUS
      EXTERNAL EXDBPLUS

      CHARACTER *90 UNUSEDSTRING
      EXDB=EXDBPLUS(in_TITRE,REVIS,FLAG,UNUSEDSTRING,0)
      RETURN
      END
****FONCTION EXDBPLUS    IMPRESSION DE BOITE DE DEBUT D'EXECUTION
*
*
      INTEGER FUNCTION EXDBPLUS(in_TITRE,REVIS,FLAG,SUPP,NSUP)
      IMPLICIT NONE
      INTEGER NSUP
      CHARACTER*(*) in_TITRE,REVIS,FLAG,SUPP(NSUP)
*
*AUTEUR  M.VALIN RPN MARS 1983
*Revision 002 M. Lepine Octobre 1998 - Ajout de la signature RMNLIB
*Revision 003 M. Lepine Octobre 2008 - Anciennement exdb, ajout de lignes d'impression
*                                      supplementaires en option
*Revision 004 M. Valin  Mars 2022    - eliminer l'enonce ENTRY pour EXFIN et 
*                                      en faire une fonction a part
*                                      suppression de format inutilise
*
*OBJET(EXDB)
*        IMPRIMER UNE BOITE INDIQUANT LE DEBUT DE L'EXECUTION
*        D'UN PROGRAMME FORTRAN. EXDB RENVOIE EGALEMENT LE
*        DATE TIME STAMP (NOMBRE ENTIER DE 10 CHIFFRES) DE
*        LA PASSE OPERATIONNELLE COURANTE.
*
*ARGUMENTS
*  E     TITRE     CHAINE DE CARACTERES. SEULS LES 7 PREMIERS
*                  SERONT IMPRIMES.
*  E     REVIS     CHAINE DE CARACTERES INDIQUANT LA VERSION DU
*                  PRODUIT (EXDB) OU UN MESSAGE DE FIN (EXFIN). SEULS LES
*                  DIX PREMIERS CARACTERES SERONT IMPRIMES.
*  E     FLAG      IGNORE.  CONSERVE POUR COMPATIBILITE ARRIERE
*  E     SUPP      LIGNES D'INFORMATION SUPPLEMENTAIRE A IMPRIMER
*  E     NSUP      NOMBRE DE LIGNE SUPPLEMENTAIRES A IMPRIMER
*
*MESSAGES
*        EXDB ET EXFIN ECRIVENT SUR L'UNITE FORTRAN 6. L'USAGER
*        EST RESPONSABLE D'OUVRIR  CETTE UNITE CORRECTEMENT.
*
*MODULES
*        DATE      POUR OBTENIR LA DATE DU SYSTEME  (RMNLIB5)
*        DATMGP2   POUR RECONSTITUER LE DATE TIME STAMP  (RMNLIB5)
*        TIME      POUR OBTENIR L'HEURE  (LIBRAIRIE FORTRAN)
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


      CHARACTER(len = 24) :: CDATIM
      CHARACTER(len = 105) :: VERSION, titre, tempstring
      INTEGER :: I
      REAL :: T1
*     utilise pour que EXFIN ait acces a T1
      common/exdb_t1/ T1

      titre = ' '
      titre(1:min(len(in_titre),90))=in_titre(1:min(len(in_titre),90))
      CALL FDATE(CDATIM)
* Obtenir la version de rmnlib utilisee
      CALL RMNLIB_version(VERSION, .false.)
      WRITE(6,500) TITRE, REVIS, VERSION, CDATIM
      DO I = 1, NSUP
          tempstring = SUPP(I)
          WRITE(6,460) tempstring
      ENDDO
      WRITE(6,800) 'BEGIN  EXECUTION     '
 450  format(3X,'*',107X,'*',/3x,'*',8x,a8,t91,'*')
 460  format(3x,'*',107X,'*',/3x,'*',2x,a105,'*')
 500  FORMAT(1H1,
     %     /,3X,'*',107('*'),'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,A57,10X,A10,27X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',1X,A105,1X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,A24,46X,34X,'*')
 700  FORMAT(3X,'*',107X,'*',
     %     /,3X,'*',3X,7A4,I12,10X,'*')
 800  FORMAT(3X,'*',107X,'*',
     %     /,3X,'*',3X,A20,84X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',107('*'),'*')

      CALL CPU_TIME(T1)
      EXDBPLUS = 0

      RETURN
      END

***FONCTION EXDBPLUS    IMPRESSION DE BOITE DE FIN D'EXECUTION
      INTEGER FUNCTION EXFIN(in_TITRE,REVIS,FLAG)
      IMPLICIT NONE
      CHARACTER*(*) in_TITRE,REVIS,FLAG
*AUTEUR  M.VALIN RPN MARS 2022
*
*OBJET(EXFIN)
*        EXFIN IMPRIME UNE BOITE INDIQUANT LA FIN DE L'EXECUTION
*        D'UN PROGRAMME FORTRAN. LA VALEUR RENVOYEE EST ZERO.
*ARGUMENTS
*  E     TITRE     voir EXDB
*  E     REVIS     voir EXDB
*  E     FLAG      inutilise, garde par compatibilite arriere
*NOTES
*     autrefois un ENTRY dans EXDBPLUS, maintenant une fonction `a part
**
      external flush_stdout
      common/exdb_t1/ T1
      REAL T1, T2
      CHARACTER *105 titre
      CHARACTER *24 CDATIM
      call flush_stdout()
      titre = ' '
      titre(1:min(len(in_titre),90))=in_titre(1:min(len(in_titre),90))
      CALL FDATE(CDATIM)
      CALL CPU_TIME(T2)
      WRITE(6,501) TITRE,REVIS,CDATIM,'END EXECUTION       ',T2-T1
 501  FORMAT(
     %     /,3X,'*',107('*'),'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,A57,3X,A10,34X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,A24,46X,34X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,A20,84X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',3X,'CP SECS = ',F10.3,84X,'*',
     %     /,3X,'*',107X,'*',
     %     /,3X,'*',107('*'),'*')

      EXFIN = 0

      RETURN
      END
