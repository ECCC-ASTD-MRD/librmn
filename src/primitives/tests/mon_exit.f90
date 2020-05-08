program monexit
external exitsub

print *,'avant set exit'
call f_set_user_exit_handler(exitsub)
print *,'avant appel exit handler'
call f_user_exit_handler(12)
print *,'apres premier appel exit handler'
call f_user_exit_handler(33)
stop
end

subroutine exitsub(n)
integer n

print *,'Sortie exitsub=',n
return
end
