module string_type_mod

  implicit none

  private

  public :: string

  type :: string
    private
    character(len=:), allocatable :: value
  contains
    procedure, public  :: get_value
    procedure, public  :: len      => len_string
    procedure, public  :: len_trim => len_trim_string
    procedure, public  :: trim     => trim_string
    procedure, public  :: adjustl  => adjustl_string
    procedure, public  :: adjustr  => adjustr_string
    procedure, public  :: reverse  => resverse_string
    procedure, public  :: to_lower => to_lower_string
    procedure, public  :: to_upper => to_upper_string
    procedure, public  :: to_int   => string_to_int
    procedure, private :: assign_char
    procedure, private :: assign_string
    generic, public    :: assignment(=) => assign_char, assign_string
#ifdef __GNUC__
    procedure, public  :: delete => delete_string_polymorph
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

  subroutine delete_string(this)

    implicit none
    type(string), intent(inout) :: this

    if (allocated(this%value)) then
      deallocate(this%value)
    end if

  end subroutine delete_string

#ifdef __GNUC__
  subroutine delete_string_polymorph(this)

    implicit none
    class(string), intent(inout) :: this

    if (allocated(this%value)) then
      deallocate(this%value)
    end if

  end subroutine delete_string_polymorph
#endif

  function get_value(this) result(string_value)

    implicit none
    class(string), intent(in)     :: this
    character(len=:), allocatable :: string_value

    string_value = this%value

  end function get_value

  function len_string(this) result(length)

    implicit none
    class(string), intent(in) :: this
    integer                   :: length

    length = len(this%value)

  end function len_string

  function len_trim_string(this) result(length)

    implicit none
    class(string), intent(in) :: this
    integer                   :: length

    length = len_trim(this%value)

  end function len_trim_string

  function trim_string(this) result(trimmed_string)

    implicit none
    class(string), intent(in)     :: this
    character(len=:), allocatable :: trimmed_string

    trimmed_string = trim(this%value)

  end function trim_string

  function adjustl_string(this) result(adjusted_string)

    implicit none
    class(string), intent(in)     :: this
    character(len=:), allocatable :: adjusted_string

    adjusted_string = adjustl(this%value)

  end function adjustl_string

  function adjustr_string(this) result(adjusted_string)

    implicit none
    class(string), intent(in)     :: this
    character(len=:), allocatable :: adjusted_string

    adjusted_string = adjustr(this%value)

  end function adjustr_string

  function resverse_string(this) result(reverse_string)

    implicit none
    class(string), intent(in)     :: this
    character(len=:), allocatable :: reverse_string
    integer                       :: i, n

    reverse_string = this%value
    n = this%len()
    do i = 1, n
      reverse_string(n-i+1:n-i+1) = this%value(i:i)
    end do

  end function resverse_string

  function to_lower_string(this) result(lower_string)

    implicit none
    class(string), intent(inout)  :: this
    character(len=:), allocatable :: lower_string
    character(len=*), parameter   :: lower = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter   :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                       :: i, j

    lower_string = this%value
    do i = 1, this%len()
      j = index(upper, lower_string(i:i))
      if (j > 0) then
        lower_string(i:i) = lower(j:j)
      end if
    end do
  end function to_lower_string

function to_upper_string(this) result(upper_string)

    implicit none
    class(string), intent(inout)  :: this
    character(len=:), allocatable :: upper_string
    character(len=*), parameter   :: lower = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter   :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                       :: i, j

    upper_string = this%value
    do i = 1, this%len()
      j = index(lower, upper_string(i:i))
      if (j > 0) then
        upper_string(i:i) = upper(j:j)
      end if
    end do
  end function to_upper_string

  function string_to_int(this) result (int)

    implicit none
    class(string), intent(inout) :: this
    integer                      :: int

    read(this%value, *) int

  end function string_to_int

  subroutine assign_char(this, str)

    implicit none
    class(string), intent(inout) :: this
    character(*), intent(in)     :: str

    this%value = str

  end subroutine assign_char

  subroutine assign_string(this, from)

    implicit none
    class(string), intent(inout) :: this
    type(string), intent(in)     :: from

    this%value = from%value

  end subroutine assign_string

end module string_type_mod