! { dg-do run }
!
! Test OpenMP 4.5 structure-element mapping

! TODO: character(kind=4,...) needs to be tested, but depends on
!       PR fortran/95837
! TODO: ...%str4 should be tested but that currently fails due to
!       PR fortran/95868 (see commented lined)
! TODO: Test also array-valued var, nested derived types,
!       type-extended types.

program main
  implicit none

  type t2
    integer :: a, b
    ! For complex, assume small integers are exactly representable
    complex(kind=8) :: c
    integer :: d(10)
    integer, pointer :: e => null(), f(:) => null()
    character(len=5) :: str1
    character(len=5) :: str2(4)
    character(len=:), pointer :: str3 => null()
    character(len=:), pointer :: str4(:) => null()
  end type t2

  integer :: i

  call one ()
  call two ()
  call three ()
  call four ()
  call five ()
  call six ()
  call seven ()
  call eight ()

contains
  ! Implicitly mapped – but no pointers are mapped
  subroutine one() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "one" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%e, source=99)
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str3, source="HelloWorld")
    allocate (var%str4, source=["Let's", "Go!!!"])

    !$omp target map(tofrom:var)
      if (var%a /= 1) stop 1
      if (var%b /= 2)  stop 2
      if (var%c%re /= -1.0_8 .or. var%c%im /= 2.0_8) stop 3
      if (any (var%d /= [(-3*i, i = 1, 10)])) stop 4
      if (var%str1 /= "abcde") stop 5
      if (any (var%str2 /= ["12345", "67890", "ABCDE", "FGHIJ"])) stop 6
    !$omp end target

    deallocate(var%e, var%f, var%str3, var%str4)
  end subroutine one

  ! Explicitly mapped – all and full arrays
  subroutine two() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "two" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%e, source=99)
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str3, source="HelloWorld")
    allocate (var%str4, source=["Let's", "Go!!!"])

    !$omp target map(tofrom: var%a, var%b, var%c, var%d, var%e, var%f, &
    !$omp&                   var%str1, var%str2, var%str3, var%str4)
      if (var%a /= 1) stop 1
      if (var%b /= 2)  stop 2
      if (var%c%re /= -1.0_8 .or. var%c%im /= 2.0_8) stop 3
      if (any (var%d /= [(-3*i, i = 1, 10)])) stop 4
      if (var%str1 /= "abcde") stop 5
      if (any (var%str2 /= ["12345", "67890", "ABCDE", "FGHIJ"])) stop 6

      if (.not. associated (var%e)) stop 7
      if (var%e /= 99) stop 8
      if (.not. associated (var%f)) stop 9
      if (size (var%f) /= 4) stop 10
      if (any (var%f /= [22, 33, 44, 55])) stop 11
      if (.not. associated (var%str3)) stop 12
      if (len (var%str3) /= len ("HelloWorld")) stop 13
      if (var%str3 /= "HelloWorld") stop 14
      if (.not. associated (var%str4)) stop 15
      if (len (var%str4) /= 5) stop 16
      if (size (var%str4) /= 2) stop 17
      if (any (var%str4 /= ["Let's", "Go!!!"])) stop 18
    !$omp end target

    deallocate(var%e, var%f, var%str3, var%str4)
  end subroutine two

  ! Explicitly mapped – one by one but full arrays
  subroutine three() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "three" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%e, source=99)
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str3, source="HelloWorld")
    allocate (var%str4, source=["Let's", "Go!!!"])

    !$omp target map(tofrom: var%a)
      if (var%a /= 1) stop 1
    !$omp end target
    !$omp target map(tofrom: var%b)
      if (var%b /= 2)  stop 2
    !$omp end target
    !$omp target map(tofrom: var%c)
      if (var%c%re /= -1.0_8 .or. var%c%im /= 2.0_8) stop 3
    !$omp end target
    !$omp target map(tofrom: var%d)
      if (any (var%d /= [(-3*i, i = 1, 10)])) stop 4
    !$omp end target
    !$omp target map(tofrom: var%str1)
      if (var%str1 /= "abcde") stop 5
    !$omp end target
    !$omp target map(tofrom: var%str2)
      if (any (var%str2 /= ["12345", "67890", "ABCDE", "FGHIJ"])) stop 6
    !$omp end target

    !$omp target map(tofrom: var%e)
      if (.not. associated (var%e)) stop 7
      if (var%e /= 99) stop 8
    !$omp end target
    !$omp target map(tofrom: var%f)
      if (.not. associated (var%f)) stop 9
      if (size (var%f) /= 4) stop 10
      if (any (var%f /= [22, 33, 44, 55])) stop 11
    !$omp end target
    !$omp target map(tofrom: var%str3)
      if (.not. associated (var%str3)) stop 12
      if (len (var%str3) /= len ("HelloWorld")) stop 13
      if (var%str3 /= "HelloWorld") stop 14
    !$omp end target
    !$omp target map(tofrom: var%str4)
      if (.not. associated (var%str4)) stop 15
      if (len (var%str4) /= 5) stop 16
      if (size (var%str4) /= 2) stop 17
      if (any (var%str4 /= ["Let's", "Go!!!"])) stop 18
    !$omp end target

    deallocate(var%e, var%f, var%str3, var%str4)
  end subroutine three

  ! Explicitly mapped – all but only subarrays
  subroutine four() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "four" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str4, source=["Let's", "Go!!!"])

!   !$omp target map(tofrom: var%d(4:7), var%f(2:3), var%str2(2:3), var%str4(2:2))
    !$omp target map(tofrom: var%d(4:7), var%f(2:3), var%str2(2:3))
      if (any (var%d(4:7) /= [(-3*i, i = 4, 7)])) stop 4
      if (any (var%str2(2:3) /= ["67890", "ABCDE"])) stop 6

      if (.not. associated (var%f)) stop 9
      if (size (var%f) /= 4) stop 10
      if (any (var%f(2:3) /= [33, 44])) stop 11
!     if (.not. associated (var%str4)) stop 15
!     if (len (var%str4) /= 5) stop 16
!     if (size (var%str4) /= 2) stop 17
!     if (var%str4(2) /= "Go!!!") stop 18
    !$omp end target

    deallocate(var%f, var%str4)
  end subroutine four

  ! Explicitly mapped – all but only subarrays and one by one
  subroutine five() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "five" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str4, source=["Let's", "Go!!!"])

    !$omp target map(tofrom: var%d(4:7))
      if (any (var%d(4:7) /= [(-3*i, i = 4, 7)])) stop 4
    !$omp end target
    !$omp target map(tofrom: var%str2(2:3))
      if (any (var%str2(2:3) /= ["67890", "ABCDE"])) stop 6
    !$omp end target

    !$omp target map(tofrom: var%f(2:3))
     if (.not. associated (var%f)) stop 9
     if (size (var%f) /= 4) stop 10
     if (any (var%f(2:3) /= [33, 44])) stop 11
    !$omp end target
!  !$omp target map(tofrom: var%str4(2:2))
!     if (.not. associated (var%str4)) stop 15
!     if (len (var%str4) /= 5) stop 16
!     if (size (var%str4) /= 2) stop 17
!     if (var%str4(2) /= "Go!!!") stop 18
!  !$omp end target

    deallocate(var%f, var%str4)
  end subroutine five

  ! Explicitly mapped – all but only array elements
  subroutine six() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "six" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str4, source=["Let's", "Go!!!"])

!   !$omp target map(tofrom: var%d(5), var%f(3), var%str2(3), var%str4(2))
    !$omp target map(tofrom: var%d(5), var%f(3), var%str2(3))
      if (var%d(5) /= -3*5) stop 4
      if (var%str2(3) /= "ABCDE") stop 6

     if (.not. associated (var%f)) stop 9
     if (size (var%f) /= 4) stop 10
     if (var%f(3) /= 44) stop 11
!     if (.not. associated (var%str4)) stop 15
!     if (len (var%str4) /= 5) stop 16
!     if (size (var%str4) /= 2) stop 17
!     if (var%str4(2) /= "Go!!!") stop 18
    !$omp end target

    deallocate(var%f, var%str4)
  end subroutine six

  ! Explicitly mapped – all but only array elements and one by one
  subroutine seven() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "seven" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])
    allocate (var%f, source=[22, 33, 44, 55])
    allocate (var%str4, source=["Let's", "Go!!!"])

    !$omp target map(tofrom: var%d(5))
      if (var%d(5) /= (-3*5)) stop 4
    !$omp end target
    !$omp target map(tofrom: var%str2(2:3))
      if (any (var%str2(2:3) /= ["67890", "ABCDE"])) stop 6
    !$omp end target

    !$omp target map(tofrom: var%f(2:3))
     if (.not. associated (var%f)) stop 9
     if (size (var%f) /= 4) stop 10
     if (any (var%f(2:3) /= [33, 44])) stop 11
    !$omp end target
!   !$omp target map(tofrom: var%str4(2:2))
!     if (.not. associated (var%str4)) stop 15
!     if (len (var%str4) /= 5) stop 16
!     if (size (var%str4) /= 2) stop 17
!     if (var%str4(2) /= "Go!!!") stop 18
!   !$omp end target

    deallocate(var%f, var%str4)
  end subroutine seven

  ! Check mapping of NULL pointers
  subroutine eight() 
    type(t2) :: var, var2(4)
    type(t2), pointer :: var3, var4(:)

    print '(g0)', '==== TESTCASE "eight" ===='

    var = t2(a = 1, &
             b = 2, c = cmplx(-1.0_8, 2.0_8,kind=8), &
             d = [(-3*i, i = 1, 10)], &
             str1 = "abcde", &
             str2 = ["12345", "67890", "ABCDE", "FGHIJ"])

!    !$omp target map(tofrom: var%e, var%f, var%str3, var%str4)
    !$omp target map(tofrom: var%e, var%str3)
      if (associated (var%e)) stop 1
!      if (associated (var%f)) stop 2
      if (associated (var%str3)) stop 3
!      if (associated (var%str4)) stop 4
    !$omp end target
  end subroutine eight

end program main
