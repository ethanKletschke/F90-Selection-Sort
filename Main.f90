program SelectionSort
    implicit none

    ! The array to sort.
    integer :: to_sort(10)

    ! Initialise the array with the values listed.
    data to_sort /10, 1, 23, 90, 3, 45, 80, 109, 12, 19/

    ! Sort the array using the Selection Sort algorithm.
    call selection_sort(to_sort)
    ! Display the array.
    call display_array(to_sort)
contains
    ! A subroutine to swap the values of its arguments.
    subroutine swap_vals(x, y)
        ! The variables to swap the values of.
        integer, intent(inout) :: x, y

        ! A temporary swapper variable.
        integer :: temp

        ! Swap the values of the parameters.
        temp = x
        x = y
        y = temp
    end subroutine swap_vals

    ! An improved implementation of the Selection Sort algorithm.
    subroutine selection_sort(arr)
        ! The integer array to sort.
        integer, intent(inout) :: arr(:)
        
        ! Local variables.
        integer :: len, i, j, min_index

        ! Get the length of the array.
        len = size(arr)

        ! Outer loop controlling how many times the inner loop runs.
        loopI: do i = 1, len - 1
            ! Store the current index as the minimum index.
            min_index = i

            ! Loop through the array.
            loopJ: do j = i + 1, len
                ! If the value at index j is greater than at the min index:
                changeIndex: if (arr(j) < arr(min_index)) then
                    ! Change the minimum index to the value of j.
                    min_index = j
                end if changeIndex
            end do loopJ

            ! Swap the values of the array values at indexes i and the 
            ! min index, instead of shifting the values through popping,
            ! to improve the efficiency of the algorithm.
            call swap_vals(arr(i), arr(min_index))
        end do loopI
    end subroutine selection_sort

    ! A subroutine to display the array.
    subroutine display_array(arr)
        ! The array to display.
        integer, intent(in) :: arr(:)

        ! The do-loop variable.
        integer :: i

        ! From 1 to the size of the array:
        do i = 1, size(arr)
            ! Display the value at index i.
            print "(a7, i3, a2, i5)", 'Value #', i, ": ", arr(i)
        end do
    end subroutine display_array
end program SelectionSort
