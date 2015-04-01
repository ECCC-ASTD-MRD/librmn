	PROGRAM TEST
	CHARACTER * 8 CLE(7)
	character * 60 DEF(7), VAL(7)
        integer ipos
	DATA CLE /'CLE1','CLE2_','CLE2_','CLE2_','CLE3:','CLE4.','-.'/
	DATA DEF /'DEF1','DEF2','DEF2','DEF2','DEF3','DEF4','DEF5'/
	DATA VAL /'NUL1','NUL2','NUL2','NUL2','NUL3','NUL4','NUL5'/
*
	print *,'AVANT CCARD'
        do i = 1,7 
	  write(6,77) cle(i),val(i)
	enddo
        
	ipos = 0
	CALL CCARD(CLE,DEF,VAL,7,ipos)
        
	PRINT *,'APRES CCARD'
	do i = 1,7 
	  write(6,77) cle(i),val(i)
	enddo
 77	FORMAT(t10,a8,'= ',a60)
	STOP
	END
