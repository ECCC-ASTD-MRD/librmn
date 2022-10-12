program test_large_jar
    use jar_module
    implicit none

#include "serializer.hf"

    logical   :: ok
    type(jar) :: medium_jar, large_jar

    integer, dimension(:), allocatable :: large_array_in, large_array_out

    integer(kind=8) :: i

    print *, 'Testing very large jar. This might take a minute or two.'

    allocate(large_array_in(1000000000))
    allocate(large_array_out(1000000000))

    ok = medium_jar % new(2000000000)
    if (.not. ok) then
        print *, 'ERROR: Unable to create medium-sized jar'
        error stop 1
    end if

    ok = large_jar % new(20000000000_8)
    if (.not. ok) then
        print *, 'ERROR: Unable to create large jar'
        error stop 1
    end if

    large_array_out(:) = 0
    do i = 1, size(large_array_in)
        large_array_in(i) = i
    end do

    do i = 1, 20000000000_8 / size(large_array_in) * 2
        ok = JAR_PUT_ITEMS(large_jar, large_array_in)
        if (.not. ok) then
            print *, 'ERROR: Could not insert large array at iteration ', i
            error stop 1
        end if
    end do

    ok = JAR_PUT_ITEMS(large_jar, large_array_in)
    if (ok) then
        print *, 'ERROR: Should not have been able to insert large array at this point. The jar is full!'
        error stop 1
    else
        print *, 'DEBUG: As expected, jar is full...'
    end if

    ok = JAR_GET_ITEMS(large_jar, large_array_out)

    if (.not. ok) then
        print *, 'ERROR: Unable to retrieve large array from jar!'
        error stop 1
    end if

    if (any(large_array_in(:) .ne. large_array_out(:))) then
        print *, 'ERROR: Extracted large array does not match inserted one'
        error stop 1
    end if
    
    deallocate(large_array_in)
    deallocate(large_array_out)

end program test_large_jar
