module mod_pdt
  
  use iso_fortran_env, only: real32,real64
  implicit none
  private
  
  !1.) define an abstract derived type with kind parameter k and len parameters m & n
  type, abstract :: matT
  end type matT  
  !2.) define a type extension with an rank2-array component mat
  ! making use of the parameters of its parent
  type, extends(matT) :: rmatT
     real(k) :: mat(m,n)
  end type rmatT
  
  !3.) define assignment that can handle different
  ! kind-parameters for rmatT, e.g. real32 and real64
  interface assignment(=)
  end interface assignment(=)
  
  !4.) define a matrix multiplication that can handle different
  ! kind-parameters for rmatT, e.g. real32 and real64
  interface matmul
  end interface matmul

  !1.-4.) module default is private here
  !       explicitly make entities public as required
  public rmatT

contains
    
end module mod_pdt

program pdt
  
  use iso_fortran_env, only: real32,real64
  use mod_pdt
  implicit none
  !WARNINGS:
  ! intel-compiler as of 19.0: may need len-parameters to be defined as paramters to solve the exercise
  ! gnu-compiler as of 8.2 still cannot handle this exercise
  ! nag-compiler as of 6.2 is fine for this exercise
  
  !other declarations ...
  type(rmatT(real32,:,:)) , allocatable :: A,D
  type(rmatT(real64,:,:)) , allocatable :: B,C
  
  !use default constructor rmatT to allocate A and B with values
  !A = rmatT( ... )( ...)
  !B = rmatT( ... )( ...)
  !construct C from A by first allocate C and then assign from A
  !allocate(...::C)
  !C = A
  !
  !allocate D and assign h the matrix multiplication if A and B
  !allocate(...::D)
  !D = matmul(A,B)

  ! you may add some print statements to check program correctness:
  !
  !write(*,*)'A: values are:',(new_line(''),A%mat(i,:),i=1,A%m)
  !write(*,*)'A: storage_size is:',storage_size(A%mat),'bit/element'
  !write(*,*)'A: shape is: [',A%m,A%n,']'//new_line('')
  !write(*,*)'B: values are:',(new_line(''),B%mat(i,:),i=1,B%m)
  !write(*,*)'B: storage_size is:',storage_size(B%mat),'bit/element'
  !write(*,*)'B: shape is: [',B%m,B%n,']'//new_line('')
  !write(*,*)'C: values are:',(new_line(''),C%mat(i,:),i=1,C%m)
  !write(*,*)'C: storage_size is:',storage_size(C%mat),'bit/element'
  !write(*,*)'C: shape is: [',C%m,C%n,']'//new_line('')
  !write(*,*)'D: values are:',(new_line(''),D%mat(i,:),i=1,D%m)
  !write(*,*)'D: storage_size is:',storage_size(D%mat),'bit/element'
  !write(*,*)'D: shape is: [',D%m,D%n,']'//new_line('')

end program pdt
