C
C File:   main.f
C Author: jared-l
C
C Created on July 1, 2015, 6:33 PM
C      
C Description: This program attempts to follow and solve the problems shown and described in
C http://nucleartalent.github.io/Course2ManyBodyMethods/doc/pub/fci/pdf/fci-print.pdf
C
      program main
          integer rows, columns
          real eigen_value
          parameter (rows=7)
          parameter (columns=7)
          parameter (eigen_value=-1.0)      ! Define the eigen_value
          real hamiltonian(rows, columns)   ! Create the Hamiltonian array
          
          ! Initialize the Hamiltonian
          call initialize_hamiltonian(hamiltonian,
     +                                rows,
     +                                columns, 
     +                                eigen_value)
                                      
          ! Print to see that our results are correct
          call print_hamiltonian(hamiltonian, 
     +                           rows, 
     +                           columns)
      end
      
      
C     This subroutine initializes the Hamiltonian to match the configuration shown on page 5 of
C     http://nucleartalent.github.io/Course2ManyBodyMethods/doc/pub/fci/pdf/fci-print.pdf
      subroutine initialize_hamiltonian(hamiltonian, 
     +                                  rows, 
     +                                  columns, 
     +                                  eigenvalue)
                                        
          integer rows, columns, midpt, coeff1, coeff2
          real hamiltonian(rows, columns), eigenvalue
          
          midpt = (rows - 1)/2
          
          ! This works by looping over the 2-d array, and checking to see whether or
          ! not to set the array value to some particular eigen-value.
          do i=1,rows
              do j=1, columns
                  ! Fill these in with slater determinants
                  hamiltonian(i,j) = eigenvalue
                  coeff1 = i - j + midpt + 2
                  coeff2 = j - i + midpt + 2
                  
                  if (coeff1 .lt. midpt .or. coeff2 .lt. midpt)
     +            then
                      hamiltonian(i,j) = 0
                  else if (i .eq. 1 .and. j .eq. 2) then
                      hamiltonian(i,j) = 0
                  else if (i .eq. 2 .and. j .eq. 1) then
                      hamiltonian(i,j) = 0
                  endif
                  
              end do
          end do
      end subroutine
      
      
C     This subroutine prints the Hamiltonian (useful for debugging)
      subroutine print_hamiltonian(hamiltonian, rows, columns)
          integer rows, columns
          real hamiltonian(rows, columns)
          
          do, i=1, rows
              write(*,*) (hamiltonian(i,j), j=1, columns) ! N.B.: this uses an 'implicit' `do` loop!
          enddo
      end subroutine