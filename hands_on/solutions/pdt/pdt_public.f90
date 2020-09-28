module mod_pdt_1
  
  use iso_fortran_env, only: real32,real64
  implicit none
  private

  type, abstract :: matT(k,m,n)
     integer, kind :: k=real32
     integer, len  :: m=1, n=1
  end type matT  
  type, extends(matT) :: rmatT
!     private
     real(k) :: mat(m,n)
  end type rmatT
  
  interface assignment(=)
     module procedure copy_rmat3264
     module procedure copy_rmat6432
  end interface assignment(=)
  
  interface matmul
     module procedure matmul_rmat3232
     module procedure matmul_rmat3264
     module procedure matmul_rmat6432
     module procedure matmul_rmat6464
  end interface matmul

  public matT,rmatT,assignment(=),matmul !,get_mat
  
contains
    
  subroutine copy_rmat3264(lhs,rhs)
    type(rmatT(real32,*,*)),intent(in) :: rhs
    type(rmatT(real64,rhs%m,rhs%n)), intent(out) :: lhs
    write(*,*)'copy_rmat3264'
    lhs%mat=rhs%mat
  end subroutine copy_rmat3264
  subroutine copy_rmat6432(lhs,rhs)
    type(rmatT(real64,*,*)),intent(in) :: rhs
    type(rmatT(real32,rhs%m,rhs%n)), intent(out) :: lhs
    lhs%mat(:,:)=rhs%mat(:,:)
  end subroutine copy_rmat6432
  
  function matmul_rmat3232(A,B)result(C)
    type(rmatT(real32,*,*)),intent(in) :: A
    type(rmatT(real32,A%n,A%m)),intent(in) :: B
    type(rmatT(real32,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat3232
  function matmul_rmat3264(A,B)result(C)
    type(rmatT(real32,*,*)),intent(in) :: A
    type(rmatT(real64,A%n,A%m)),intent(in) :: B
    type(rmatT(real64,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat3264
  function matmul_rmat6432(A,B)result(C)
    type(rmatT(real64,*,*)),intent(in) :: A
    type(rmatT(real32,A%n,A%m)),intent(in) :: B
    type(rmatT(real64,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat6432
  function matmul_rmat6464(A,B)result(C)
    type(rmatT(real64,*,*)),intent(in) :: A
    type(rmatT(real64,A%n,A%m)),intent(in) :: B
    type(rmatT(real64,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat6464
  
end module mod_pdt_1

program pdt

  use iso_fortran_env, only: real32,real64
  use mod_pdt_1
  implicit none
  integer :: n1=4,n2=3
  integer :: i
  type(rmatT(real32,:,:)) , allocatable :: A,C
  type(rmatT(real64,:,:)) , allocatable :: B,D
  
  A = rmatT(real32,n1,n2)(reshape([(real(i,real32),i=1,n1*n2)],[n1,n2]))
  B = rmatT(real64,n2,n1)(reshape([(real(i,real64),i=1,n1*n2)],[n2,n1]))
  allocate(rmatT(C%k,B%m,B%n)::C)
  C = B
  allocate(rmatT(D%k,A%m,A%m)::D)
  D = matmul(A,B)
  
  write(*,*)'A: values are:',(new_line(''),A%mat(i,:),i=1,A%m)
  write(*,*)'A: storage_size is:',storage_size(A%mat),'bit/element'
  write(*,*)'A: shape is: [',A%m,A%n,']'//new_line('')
  write(*,*)'B: values are:',(new_line(''),B%mat(i,:),i=1,B%m)
  write(*,*)'B: storage_size is:',storage_size(B%mat),'bit/element'
  write(*,*)'B: shape is: [',B%m,B%n,']'//new_line('')
  write(*,*)'C: values are:',(new_line(''),C%mat(i,:),i=1,C%m)
  write(*,*)'C: storage_size is:',storage_size(C%mat),'bit/element'
  write(*,*)'C: shape is: [',C%m,C%n,']'//new_line('')
  write(*,*)'D: values are:',(new_line(''),D%mat(i,:),i=1,D%m)
  write(*,*)'D: storage_size is:',storage_size(D%mat),'bit/element'
  write(*,*)'D: shape is: [',D%m,D%n,']'//new_line('')

end program pdt
