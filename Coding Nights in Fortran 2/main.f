C
C File:   main.f
C Author: hclambda
C
C Created on July 1, 2015, 6:33 PM
C
      program main
          integer rows, columns
          parameter (rows=7)
          parameter (columns=7)
          
          ! Create the hamiltonian array
          real hamiltonian(rows, columns)
          
          call initialize_hamiltonian(hamiltonian, rows, columns)
          call print_hamiltonian(hamiltonian, rows, columns)
      end
      
      subroutine initialize_hamiltonian(hamiltonian, rows, columns)
          integer rows, columns, scoeff, coeff1, coeff2
          real hamiltonian(rows, columns)
          
          scoeff = (rows - 1)/2
          
          do i=1,rows
              do j=1, columns
                  ! Fill these in with slater determinants
                  hamiltonian(i,j) = -1
                  coeff1 = i - j + scoeff + 2
                  coeff2 = j - i + scoeff + 2
                  
                  if (coeff1 .lt. scoeff .or. coeff2 .lt. scoeff)
     +            then
                      hamiltonian(i,j) = 0
                  endif
                  
              end do
          end do
      end subroutine
      
      subroutine print_hamiltonian(hamiltonian, rows, columns)
          integer rows, columns
          real hamiltonian(rows, columns)
          
          do, i=1, rows
              write(*,*) (hamiltonian(i,j), j=1, columns)
          enddo
      end subroutine