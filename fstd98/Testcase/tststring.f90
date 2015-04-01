Program tststring

character *12 etik
character *12 etik_list(3)
data etik /'Label01'/
data etik_list /'Label001','Label002','Label008'/

call fs_to_cs(etik,.false.,1)
call fs_to_cs(etik,.true.,1)
call fs_to_cs(etik_list,.true.,3)
call cs_to_fs(etik,12)
write(6,*) '-->',etik,'<--'
stop
end
