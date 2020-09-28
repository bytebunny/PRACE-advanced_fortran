module mod_pdt_2
  
  use iso_fortran_env, only: real32,real64
  implicit none
  private
  
  type, abstract :: matT(k,m,n)
     integer, kind :: k=real32
     integer, len :: m=1
     integer, len :: n=1
  end type matT  
  type, extends(matT) :: rmatT
     private
     real(k) :: mat(m,n)
  end type rmatT

  interface get_mat
     module procedure get_mat32,get_mat64
  end interface get_mat
  
  interface assignment(=)
     module procedure copy_rmat3264
     module procedure copy_rmat6432
  end interface assignment(=)

  interface rmatT
     module procedure create_rmat32
     module procedure create_rmat64
  end interface rmatT
  
  interface matmul
     module procedure matmul_rmat3232
     module procedure matmul_rmat3264
     module procedure matmul_rmat6432
     module procedure matmul_rmat6464
  end interface matmul

  public matT,rmatT,matmul,get_mat,assignment(=)
  
contains

  function get_mat32(s)result(mat)
    class(rmatT(real32,*,*)) :: s
    real(real32)::mat(s%m,s%n)
    mat=s%mat
  end function get_mat32
  function get_mat64(s)result(mat)
    class(rmatT(real64,*,*)) :: s
    real(real64)::mat(s%m,s%n)
    mat=s%mat
  end function get_mat64
  
  function create_rmat32(m,n,mat)result(r)
    integer :: m,n
    real(real32) :: mat(:)
    type(rmatT(real32,m,n)) :: r
    r%mat = reshape(mat,[r%m,r%n])
  end function create_rmat32
  function create_rmat64(m,n,mat)result(r)
    integer :: m,n
    real(real64) :: mat(:)
    type(rmatT(real64,m,n)) :: r
    r%mat = reshape(mat,[r%m,r%n])
  end function create_rmat64
    
  subroutine copy_rmat3264(lhs,rhs)
    type(rmatT(real32,*,*)),intent(in) :: rhs
    type(rmatT(real64,rhs%m,rhs%n)),intent(out) :: lhs
    lhs%mat=rhs%mat
  end subroutine copy_rmat3264
  subroutine copy_rmat6432(lhs,rhs)
    type(rmatT(real64,*,*)),intent(in) :: rhs
    type(rmatT(real32,rhs%m,rhs%n)),intent(out) :: lhs
    lhs%mat=rhs%mat
  end subroutine copy_rmat6432

  function matmul_rmat3264(A,B)result(C)
   type(rmatT(real32,*,*)),intent(in) :: A
    type(rmatT(real64,A%n,A%m)),intent(in) :: B
    type(rmatT(real64,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat3264
  
  function matmul_rmat3232(A,B)result(C)
    type(rmatT(real32,*,*)),intent(in) :: A
    type(rmatT(real32,A%n,A%m)),intent(in) :: B
    type(rmatT(real32,A%m,A%m)) :: C
    C%mat=matmul(A%mat,B%mat)
  end function matmul_rmat3232
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
  
end module mod_pdt_2

program pdt

  use iso_fortran_env, only: real32,real64
  use mod_pdt_2
  implicit none
  integer :: n1=4,n2=3
  integer :: i
  type(rmatT(real32,:,:)) , allocatable :: A,C
  type(rmatT(real64,:,:)) , allocatable :: B,D

  A = rmatT(n1,n2,[(real(i,real32),i=1,n1*n2)])
  B = rmatT(n2,n1,[(real(i,real64),i=1,n1*n2)])
  allocate(rmatT(C%k,B%m,B%n)::C)
  C = B
  allocate(rmatT(D%k,A%m,A%m)::D)
  D = matmul(A,B)
  
  associate(Amat=>get_mat(A),Bmat=>get_mat(B),Cmat=>get_mat(C),Dmat=>get_mat(D))
    write(*,*)'A: values are:',(new_line(''),Amat(i,:),i=1,A%m)
    write(*,*)'A: storage_size is:',storage_size(get_mat(A)),'bit/element'
    write(*,*)'A: shape is: [',A%m,A%n,']'//new_line('')
    write(*,*)'B: values are:',(new_line(''),Bmat(i,:),i=1,B%m)
    write(*,*)'B: storage_size is:',storage_size(get_mat(B)),'bit/element'
    write(*,*)'B: shape is: [',B%m,B%n,']'//new_line('')
    write(*,*)'C: values are:',(new_line(''),Cmat(i,:),i=1,C%m)
    write(*,*)'C: storage_size is:',storage_size(get_mat(C)),'bit/element'
    write(*,*)'C: shape is: [',C%m,C%n,']'//new_line('')
    write(*,*)'D: values are:',(new_line(''),Dmat(i,:),i=1,D%m)
    write(*,*)'D: storage_size is:',storage_size(get_mat(D)),'bit/element'
    write(*,*)'D: shape is: [',D%m,D%n,']'//new_line('')
  end associate
  
end program pdt
