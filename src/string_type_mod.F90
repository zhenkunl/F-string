module string_type_mod

  implicit none

  private

  type, public :: string
    private
    character(len=:), allocatable :: value
  contains
    private
    procedure, public, pass(self)  :: get_value
    procedure, public, pass(self)  :: len      => len_string
    procedure, public, pass(self)  :: len_trim => len_trim_string
    procedure, public, pass(self)  :: trim     => trim_string
    procedure, public, pass(self)  :: adjustl  => adjustl_string
    procedure, public, pass(self)  :: adjustr  => adjustr_string
    procedure, public, pass(self)  :: reverse  => resverse_string
    procedure, public, pass(self)  :: to_lower => to_lower_string
    procedure, public, pass(self)  :: to_upper => to_upper_string
    procedure, public, pass(self)  :: to_int   => string_to_int
    procedure, public, pass(self)  :: capitalize
    ! procedure, public, pass(self)  :: count
    ! procedure, public, pass(self)  :: find
    ! procedure, public, pass(self)  :: start_with
    ! procedure, public, pass(self)  :: end_with
    ! procedure, public, pass(self)  :: join
    ! procedure, public, pass(self)  :: split
    ! procedure, public, pass(self)  :: strip
    procedure, private, pass(lhs)  :: assign_char
    procedure, private, pass(lhs)  :: assign_string
    generic, public                :: assignment(=) => assign_char, assign_string
#ifdef __GNUC__
    procedure, public, pass(self)  :: delete => delete_string_polymorph
#endif
    final :: delete_string
  end type string

  interface string
    module procedure new_string_from_str
    module procedure new_string_from_int
  end interface string

contains

  function new_string_from_str(val) result(new)

    implicit none
    character(len=*), intent(in) :: val
    type(string)                 :: new

    new%value = val

  end function new_string_from_str

  function new_string_from_int(val) result(new)

    implicit none
    integer, intent(in)       :: val
    type(string)              :: new
    integer, parameter        :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer

    write(buffer, '(i0)') val
    new%value = trim(buffer)

  end function new_string_from_int

  subroutine delete_string(self)

    implicit none
    type(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string

#ifdef __GNUC__
  subroutine delete_string_polymorph(self)

    implicit none
    class(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string_polymorph
#endif

  function get_value(self) result(string_value)

    implicit none
    class(string), intent(in)     :: self
    character(len=:), allocatable :: string_value

    string_value = self%value

  end function get_value

  function len_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len(self%value)

  end function len_string

  function len_trim_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len_trim(self%value)

  end function len_trim_string

  function trim_string(self) result(trimmed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: trimmed_string

    trimmed_string = self
    trimmed_string%value = trim(self%value)

  end function trim_string

  function adjustl_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = self
    adjusted_string%value = adjustl(self%value)

  end function adjustl_string

  function adjustr_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = self
    adjusted_string%value = adjustr(self%value)

  end function adjustr_string

  function resverse_string(self) result(reversed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: reversed_string
    integer                   :: i, n

    reversed_string = self
    n = self%len()
    do i = 1, n
      reversed_string%value(n-i+1:n-i+1) = self%value(i:i)
    end do

  end function resverse_string

  function to_lower_string(self) result(lower_string)

    implicit none
    class(string), intent(inout) :: self
    type(string)                 :: lower_string
    character(len=*), parameter  :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter  :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                      :: i, j

    lower_string = self
    do i = 1, self%len()
      j = index(upper_alphabet, lower_string%value(i:i))
      if (j > 0) then
        lower_string%value(i:i) = lower_alphabet(j:j)
      end if
    end do

  end function to_lower_string

function to_upper_string(self) result(upper_string)

    implicit none
    class(string), intent(inout) :: self
    type(string)                 :: upper_string
    character(len=*), parameter  :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter  :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                      :: i, j

    upper_string = self
    do i = 1, self%len()
      j = index(lower_alphabet, upper_string%value(i:i))
      if (j > 0) then
        upper_string%value(i:i) = upper_alphabet(j:j)
      end if
    end do

  end function to_upper_string

  function capitalize(self) result(capitalized_string)

    implicit none
    class(string), intent(inout) :: self
    type(string)                 :: capitalized_string
    character(len=*), parameter  :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter  :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                      :: i

    capitalized_string = self%to_lower()
    i = index(lower_alphabet, capitalized_string%value(1:1))
    if (i > 0) then
      capitalized_string%value(1:1) = upper_alphabet(i:i)
    end if

  end function capitalize

  function string_to_int(self) result (int)

    implicit none
    class(string), intent(inout) :: self
    integer                      :: int

    read(self%value, *) int

  end function string_to_int

  subroutine assign_char(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    character(*), intent(in)     :: rhs

    lhs%value = rhs

  end subroutine assign_char

  subroutine assign_string(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    type(string), intent(in)     :: rhs

    lhs%value = rhs%value

  end subroutine assign_string

end module string_type_mod