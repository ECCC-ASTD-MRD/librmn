#! /usr/bin/env bash

for RI in I R ; do
    [[ $RI == I ]] && RANGE="1 2 4 8"
    [[ $RI == R ]] && RANGE="4 8"
    [[ $RI == I ]] && PARAM_DECLARE="integer, parameter  :: I = TKR_INTEGER"
    [[ $RI == R ]] && PARAM_DECLARE="integer, parameter  :: R = TKR_REAL"
    for L in $RANGE ; do
        for D in 3 2 1 ; do
            if [[ $RI == I ]] ; then
                TYPE=integer
                [[ $L == 1 ]] && KIND='C_INT8_T'
                [[ $L == 2 ]] && KIND='C_INT16_T'
                [[ $L == 4 ]] && KIND='C_INT32_T'
                [[ $L == 8 ]] && KIND='C_INT64_T'
                TYPE_CONDITION='is_type_integer(this % datyp) == 1'
                ACCEPTED_TYPES='FST_TYPE_SIGNED or FST_TYPE_UNSIGNED'
            else
                TYPE=real
                [[ $L == 4 ]] && KIND='C_FLOAT'
                [[ $L == 8 ]] && KIND='C_DOUBLE'
                TYPE_CONDITION='is_type_real(this % datyp) == 1'
                ACCEPTED_TYPES='FST_TYPE_REAL'
            fi
            [[ $D == 3 ]] && DIMENSION=':,:,:'
            [[ $D == 2 ]] && DIMENSION=':,:'
            [[ $D == 1 ]] && DIMENSION=':'

            [[ $D == 3 ]] && SHAPE='this % ni, this % nj, this % nk'
            [[ $D == 2 ]] && SHAPE='this % ni, this % nj'
            [[ $D == 1 ]] && SHAPE='this % ni'
            cat << EOT

    subroutine fst24_record_get_data_array_${RI}${L}_${D}D(this, array)
        implicit none
        class(fst_record), intent(in) :: this
        $TYPE($KIND), dimension($DIMENSION), pointer, intent(out) :: array
        
        integer :: num_dims

        nullify(array)

        num_dims = 1
        if (this % nj > 1) num_dims = 2
        if (this % nk > 1) num_dims = 3

        if (${D}>1 .and. ${D} < num_dims) then
            write(app_msg, '(A, I2, A)') 'Record data has ', num_dims, ' dimensions, so your pointer (${D}-D) needs at least that to work'
            call lib_log(APP_LIBFST, APP_ERROR, app_msg)
            return
        end if

        if (.not. (${TYPE_CONDITION})) then
            write(app_msg, '(A, A, A, A)') 'Record has type ', trim(FST_TYPE_NAMES(base_fst_type(this % datyp))),       &
                    ' but this pointer can only take ', '${ACCEPTED_TYPES}'
            call lib_log(APP_LIBFST, APP_ERROR, app_msg)
            return
        end if

        if (storage_size(array) /= this % dasiz) then
            write(app_msg, '(A, I3, A, I3, A)') 'Record contains ', this % dasiz / 8,                                   &
                    '-byte elements, but you passed a pointer with ', ${L}, '-byte elements.'
            call lib_log(APP_LIBFST, APP_ERROR, app_msg)
            return
        end if

        call c_f_pointer(this % data, array, [$SHAPE])
    end subroutine fst24_record_get_data_array_${RI}${L}_${D}D 
EOT

        done
    done
done
